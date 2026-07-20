% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Birth Threshold Personhood Reading
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This constraint story models the birth_threshold_reading of the contested
 *   personhood_boundary kernel: the claim that moral standing begins
 *   unambiguously at birth and extends to all born humans without exception.
 *   Historically instantiated in legal codes from Roman law to modern
 *   homicide statutes, this reading functions as both a protective
 *   coordination mechanism for vulnerable infants and a mandatory obligation
 *   structure for caregivers. The source material designates born infants as
 *   the 'victim set' in the moral sense (those protected from homicide),
 *   which this story maps to the beneficiary layer while acknowledging the
 *   terminological tension in an omega. The constraint is actively enforced
 *   through infanticide prohibitions and child-welfare law, and it
 *   structurally forecloses fitness-contingent and potential-based exclusions
 *   of born humans. As a commitment system, it grounds authority in legal and
 *   moral lineage (fixed_text/lineage) with an active interpretive layer that
 *   has absorbed drift without formal revision â though contemporary
 *   bioethics now exerts substantial repudiation pressure.
 *
 * KEY AGENTS:
 *   - born_infants (powerless/trapped) â structural beneficiaries of the protection, unable to exit or contest
 *   - caregivers_parents (moderate/constrained) â bear mandatory obligations of care; the constraint's extraction falls here
 *   - legal_tradition_and_courts (institutional/constrained) â administers and is simultaneously bound by the threshold; dual-positioned as enforcer and authority-loser
 *   - society_at_large (organized/constrained) â benefits from bright-line moral coordination
 *   - alternative_bioethicists (moderate/mobile) â holders of competing readings, structurally excluded from this framework's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.48).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.58).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth Threshold Personhood Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '469b3851-d12c-4505-a7c7-bf324a33a768').
narrative_ontology:cs_kernel_codification('469b3851-d12c-4505-a7c7-bf324a33a768', fixed_text).
narrative_ontology:cs_authority_grounding('469b3851-d12c-4505-a7c7-bf324a33a768', lineage).
narrative_ontology:cs_interpretation_layer_present('469b3851-d12c-4505-a7c7-bf324a33a768').
narrative_ontology:cs_reading_relation('469b3851-d12c-4505-a7c7-bf324a33a768', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('469b3851-d12c-4505-a7c7-bf324a33a768', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('469b3851-d12c-4505-a7c7-bf324a33a768', foundational, moral_standing_begins_at_birth).
narrative_ontology:cs_axiom_status(moral_standing_begins_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('469b3851-d12c-4505-a7c7-bf324a33a768', moral_standing_begins_at_birth, deontological).
narrative_ontology:cs_axiom('469b3851-d12c-4505-a7c7-bf324a33a768', secondary, universal_protection_without_exclusion).
narrative_ontology:cs_axiom_status(universal_protection_without_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('469b3851-d12c-4505-a7c7-bf324a33a768', universal_protection_without_exclusion, deontological).
narrative_ontology:cs_reference_frame('469b3851-d12c-4505-a7c7-bf324a33a768', birth_as_absolute_moral_threshold).
narrative_ontology:cs_drift_state('469b3851-d12c-4505-a7c7-bf324a33a768', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('469b3851-d12c-4505-a7c7-bf324a33a768', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, society_at_large).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, caregivers_parents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, legal_tradition_and_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upon birth, they are categorically granted moral standing and legal protection against homicide. They cannot exit this classification, cannot contest it, and depend entirely on caregivers and legal systems for enforcement.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_infants, beneficiary,
    powerless, immediate, trapped, local).

% Are legally and morally obligated to care for the infant once born. The birth threshold removes discretion to expose, abandon, or selectively withhold care without legal penalty. They bear the material, temporal, and autonomy costs of this mandatory obligation.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, caregivers_parents, payer,
    moderate, biographical, constrained, local).

% Administers and enforces the personhood boundary through homicide and child-welfare law, interpreting marginal cases. Simultaneously, it is constrained by the rule â the tradition loses authority to declare any born human a non-person or to permit exclusion based on fitness or potential.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_tradition_and_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, legal_tradition_and_courts, payer).

% Benefits from a clear, administrable moral and legal boundary that prevents arbitrary killing and stabilizes expectations about human status, reducing social coordination costs around care and homicide.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Hold fitness-contingent or potential-based personhood readings that would exclude some born humans. Within this framework, their readings are rendered invalid and excluded from policy legitimacy, though they remain mobile across other frameworks.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, alternative_bioethicists, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an unambiguous, administrable criterion for moral standing that resolves coordination problems around homicide, care, and legal status without requiring case-by-case assessment of cognitive capacity or physical fitness at birth.
% TRANSFER_FUNCTION: Transfers obligation of care and legal protection to all born humans, while transferring the burden of that care from discretionary to mandatory for parents and caregivers. Simultaneously transfers authority over personhood determination away from state or medical discretion to the biological fact of birth.
% ABSENT_VOICES: Advocates of fitness-contingent and potential-based personhood readings, who would permit exclusion of some born infants, are structurally excluded from legitimacy within this framework. Late-term fetuses just prior to birth are also absent â the framework assigns them no standing, and they have no voice in the boundary's placement.
% DISAPPEARANCE_RATIONALE: If the birth threshold vanished, homicide laws would require a new criterion for personhood, parental obligations would lose their automatic trigger, and the legal system's inability to exclude any born human would disappear â medical and state discretion over personhood would return, forcing case-by-case adjudication and likely selective exclusion.
% FOUNDING_PROBLEM: Uncertainty and arbitrariness in the moral and legal status of newborn humans, leading to selective exposure, infanticide, and lack of clear social obligation to care for infants â especially those with disabilities or in conditions of scarcity.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and archaeological evidence of infant exposure across ancient societies corroborates the historical problem from outside the moral-legal tradition. Contemporary disability-rights advocates provide partial external corroboration that exclusionary personhood criteria remain a live threat; conversely, critical bioethicists and reproductive-rights scholars from outside the benefiting parties contest whether the birth threshold remains the appropriate solution, arguing the founding problem has mutated or was overstated.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores moderate extractiveness (0.48) because the protective function for infants is paired with a non-voluntary obligation on caregivers that functions as a real cost. Suppression (0.58) reflects the active exclusion of infanticide and alternative personhood criteria. Theater is low (0.22) because enforcement is largely functional rather than performative. Accessibility collapse is high (0.72): once the birth threshold is institutionalized, alternatives (case-by-case assessment) become cognitively and legally inaccessible. Resistance (0.42) captures ongoing bioethical contestation without systemic breakdown. The temporal series show gradual hardening of enforcement obligations (suppression rising) and slow extraction accumulation as welfare obligations expand, on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the infant's seat, the constraint is pure subsidy â they receive standing and protection without cost. From the caregiver's seat, it is a mandatory extraction of resources and autonomy. From the legal tradition's seat, it is simultaneously a source of authority (the power to enforce) and a constraint on authority (the inability to exclude). The engine will compute divergent per-seat classifications: the infant seat likely computes as rope or mountain-like protection, while the caregiver seat experiences tangled_rope obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (born_infants, society_at_large) receive low directionality â the constraint subsidizes their security and coordination. Victims (caregivers_parents) receive high directionality â they bear the mandatory cost. The legal_tradition_and_courts seat is complex: as agenda_setter it would derive low d, but its secondary payer role (authority constrained) suggests a higher effective burden. No override is declared because the structural ambiguity is honest and is captured by an omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination (infant protection) and extraction (caregiver obligation) to be present. A pure-protection reading (rope) would miss the non-voluntary cost on caregivers; a pure-extraction reading (snare) would miss the genuine protective coordination. The founding problem (infanticide/exposure uncertainty) is contested but not dead, preventing automatic piton classification despite the rule's antiquity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    born_infant_victim_framing_ambiguity,
    'The source material places all born infants in the victim set, yet the constraint structurally protects them. Are infants victims of the constraint''s enforcement failures, or is ''victim'' here used in the moral sense (would-be victims of homicide)?',
    'Clarify schema semantics against source terminology; if infants are treated as schema-victims, directionality inverts and classification collapses into incoherence. Retain the schema-semantics mapping (beneficiaries) and document the source tension.',
    'Resolves whether the constraint is protective (rope/tangled) or harmful (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(born_infant_victim_framing_ambiguity, conceptual, 'Terminological ambiguity between source ''victim set'' and schema victim semantics').

omega_variable(
    caregiver_obligation_extraction_boundary,
    'Is the mandatory care obligation imposed on caregivers by the birth threshold a genuine extraction, or merely the reciprocal cost of membership in a moral community that protects the vulnerable?',
    'Cross-cultural comparison of care burdens where birth thresholds are strong versus weak; measure subjective entrapment versus normative acceptance through ethnographic and survey data.',
    'If normatively accepted cost, extraction drops and the constraint moves toward rope; if entrapped extraction, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caregiver_obligation_extraction_boundary, empirical, 'Whether caregiver obligation is extraction or reciprocal coordination cost').

omega_variable(
    axiom_overriding_trajectory,
    'Will contemporary bioethical challenges (viability, neural criteria) successfully override the birth axiom, or merely create pressure without formal supersession?',
    'Track legislative and judicial drift over the next decade; watch for explicit rejection of the birth threshold in favor of neurological or viability criteria in any jurisdiction.',
    'If axioms shift to overridden status, foreclosure recomputation triggers and drift_state may shift toward codification_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_trajectory, empirical, 'Whether bioethical pressure formally overrides the birth-threshold axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_birth_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(personhood_birth_tr_t10, personhood_boundary__birth_threshold_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(personhood_birth_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(personhood_birth_tr_t30, personhood_boundary__birth_threshold_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(personhood_birth_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(personhood_birth_tr_t50, personhood_boundary__birth_threshold_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(personhood_birth_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(personhood_birth_be_t10, personhood_boundary__birth_threshold_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(personhood_birth_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(personhood_birth_be_t30, personhood_boundary__birth_threshold_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(personhood_birth_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(personhood_birth_be_t50, personhood_boundary__birth_threshold_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(personhood_birth_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(personhood_birth_su_t10, personhood_boundary__birth_threshold_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(personhood_birth_su_t20, personhood_boundary__birth_threshold_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(personhood_birth_su_t30, personhood_boundary__birth_threshold_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(personhood_birth_su_t40, personhood_boundary__birth_threshold_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(personhood_birth_su_t50, personhood_boundary__birth_threshold_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is the birth-threshold reading of the personhood_boundary kernel. Its siblings (fitness_contingent_reading, potential_based_reading) instantiate structurally distinct constraints from the same contested kernel. This reading's epsilon reflects the coordination-and-obligation structure of a bright-line birth rule; sibling readings have different victim/beneficiary structures and higher extraction profiles due to selective exclusion logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
