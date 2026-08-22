% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Begins at Birth â Birth Threshold Reading
 *   domain: moral philosophy/historical ethics/commitment systems
 *
 * SUMMARY:
 *   This constraint story instantiates the birth_threshold_reading of the
 *   personhood_boundary kernel. The reading treats birth as an absolute,
 *   non-contingent threshold for full moral standing. Under this reading, all
 *   born humans â including the most severely disabled neonates â possess
 *   standing that cannot be revoked by state or parental discretion. The
 *   structural modeling follows the expected delta: born infants are
 *   positioned as the victim set because the constraint extracts from them
 *   the burden of mandatory personhood (life-sustaining intervention, state
 *   registration, institutional management) regardless of individual
 *   condition or capacity. The state loses authority to exclude, while
 *   medical institutions gain jurisdiction. The constraint coordinates moral
 *   behavior around a bright line but asymmetrically imposes costs on the
 *   very subjects it claims to protect.
 *
 * KEY AGENTS:
 *   - born_infants: Primary target (powerless/trapped) â bear the extraction of imposed personhood.
 *   - state_actor: Agenda-setter (institutional/constrained) â defines and enforces the boundary, loses exclusion authority.
 *   - medical_establishment: Primary beneficiary (institutional/constrained) â gains jurisdiction and funding.
 *   - parents_and_guardians: Secondary target (moderate/constrained) â lose discretion over newborn care.
 *   - bioethicists_and_dissenters: Excluded voice (organized/mobile) â argue for contingent thresholds but are kept out of policy.
 *   - human_rights_observers: Analytical observer (analytical/analytical) â monitors compliance with the norm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.65).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.75).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Begins at Birth â Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral philosophy/historical ethics/commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '6b680391-4fa9-404c-8033-9c04e327eb2e').
narrative_ontology:cs_kernel_codification('6b680391-4fa9-404c-8033-9c04e327eb2e', fixed_text).
narrative_ontology:cs_authority_grounding('6b680391-4fa9-404c-8033-9c04e327eb2e', lineage).
narrative_ontology:cs_interpretation_layer_present('6b680391-4fa9-404c-8033-9c04e327eb2e').
narrative_ontology:cs_reading_relation('6b680391-4fa9-404c-8033-9c04e327eb2e', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('6b680391-4fa9-404c-8033-9c04e327eb2e', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('6b680391-4fa9-404c-8033-9c04e327eb2e', foundational, birth_constitutes_personhood_boundary).
narrative_ontology:cs_axiom_status(birth_constitutes_personhood_boundary, holdable).
narrative_ontology:cs_axiom_grounding('6b680391-4fa9-404c-8033-9c04e327eb2e', birth_constitutes_personhood_boundary, deontological).
narrative_ontology:cs_axiom('6b680391-4fa9-404c-8033-9c04e327eb2e', foundational, moral_standing_non_contingent_for_born).
narrative_ontology:cs_axiom_status(moral_standing_non_contingent_for_born, holdable).
narrative_ontology:cs_axiom_grounding('6b680391-4fa9-404c-8033-9c04e327eb2e', moral_standing_non_contingent_for_born, deontological).
narrative_ontology:cs_reference_frame('6b680391-4fa9-404c-8033-9c04e327eb2e', birth_as_absolute_moral_threshold).
narrative_ontology:cs_drift_state('6b680391-4fa9-404c-8033-9c04e327eb2e', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b680391-4fa9-404c-8033-9c04e327eb2e', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, medical_establishment).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, born_infants).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are automatically granted full moral and legal standing at birth, which subjects them to state registration, medical jurisdiction, and mandatory life-sustaining intervention regardless of gestational age, disability, or predicted quality of life; cannot decline the protections or burdens of the assigned status.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_infants, payer,
    powerless, biographical, trapped, universal).

% Defines and enforces the legal personhood boundary at birth through homicide statutes, birth registration, and child protective services; loses the authority to exclude born infants from the polity but gains prosecutorial power and population jurisdiction.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_actor, agenda_setter,
    institutional, generational, constrained, national).

% Holds authority to certify birth, adjudicate neonatal viability, and mandate treatment of born infants under the personhood regime; benefits from expanded jurisdiction and funding streams tied to mandatory protection of newborn life.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_establishment, beneficiary,
    institutional, generational, constrained, national).

% Bear legal and moral obligations toward born infants that begin immediately at birth; lose discretion to withhold care, euthanize, or abandon regardless of infant disability or family capacity; subject to state intervention if they violate the protective regime.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_guardians, payer,
    moderate, biographical, constrained, local).

% Argue for fitness-based or potential-based personhood thresholds that would exclude some born infants from standing; are structurally excluded from policy-setting in jurisdictions where birth is the settled legal threshold but continue to publish and advocate in academic and clinical contexts.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, bioethicists_and_dissenters, excluded,
    organized, generational, mobile, global).

% Monitor and report on state compliance with the birth-threshold personhood norm; treat the boundary as a human rights standard while occasionally noting tensions with disability rights and neonatal ethics.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, human_rights_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an unambiguous moral and legal boundary for personhood at the moment of birth, eliminating case-by-case adjudication of moral standing and coordinating collective behavior toward a uniformly protected class.
% TRANSFER_FUNCTION: Moves authority over infant life and death from parental and state discretion to a universal protective regime; moves the burden of personhood â mandatory life, institutional management, and state registration â onto born infants regardless of individual condition.
% ABSENT_VOICES: Unborn entities and advocates for fitness-based or potential-based thresholds are structurally excluded from the protected class; they would argue for discretionary or gradual personhood but are silenced by the bright-line rule in jurisdictions where birth is the settled threshold.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, infanticide would not be classified as homicide, state and medical authority over newborn life would dissolve, and the sibling readings â fitness-contingent and potential-based â would become operational. The moral and legal order would rearrange around contingent or gradual personhood criteria.
% FOUNDING_PROBLEM: The ancient problem of moral uncertainty at the margins of human life: who possesses standing, who may be killed or abandoned, and how societies avoid arbitrary violence toward the newly born.
% FOUNDING_PROBLEM_CORROBORATION: Moral philosophers and legal historians attest that infanticide and marginal standing have ancient roots. However, the specific birth-threshold solution is contested by functional and potential readings; corroboration from outside the benefiting institutions â state and medical authorities â is sparse. The problem remains live, but the birth-threshold reading is one of several competing responses rather than a settled answer.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the constraint imposes the full apparatus of personhood on infants irrespective of condition, forcing life and institutional management upon them. Suppression (0.75) is high because the birth threshold actively suppresses fitness-based and potential-based alternatives through homicide law and medical protocol. Theater_ratio (0.40) reflects moderate performative maintenance: the protective function is real, but a growing share of institutional activity enforces the boundary itself rather than infant flourishing. Accessibility_collapse (0.80) is high because once the birth threshold is accepted, alternative criteria become almost unthinkable within the regime. Resistance (0.45) is moderate: bioethical dissent persists but is marginalized in policy.
 *
 * PERSPECTIVAL GAP:
 *   The medical establishment and state experience the constraint as a legitimate coordination mechanism that prevents infanticide and stabilizes moral expectations. Born infants experience it as an externally imposed status that binds them to institutional existence without consent. Parents experience it as a duty imposition that overrides family discretion. The engine computes this divergence from the structural asymmetry in power and exit options across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Born infants are powerless and trapped within the category (d near 1.0, high effective extraction). Parents are moderate power and constrained (d ~0.7). The medical establishment is institutional with constrained exit but collects authority and funding (d ~0.2). The state is institutional and constrained by international human rights norms (d ~0.5). Bioethicists are organized and mobile, sitting outside the constraint's directional pull (observer/analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy, this constraint could be misread as a pure Rope (protective norm) or even a Mountain (natural moral fact). The genealogy reveals it was built to solve the problem of marginal standing and infanticide, a problem that remains contested. The presence of identifiable beneficiaries (medical establishment) and victims (born infants, parents) prevents misclassification as Mountain or pure Rope. The Tangled Rope classification captures both the genuine coordination function (bright-line protection) and the asymmetric extraction (institutional subjection of infants).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_victimhood_ambiguity,
    'Does assigning personhood at birth primarily protect infants from violence, or does it subject them to institutional extraction by forcing life-sustaining intervention and state management regardless of individual condition?',
    'Comparative neonatal outcome studies across jurisdictions with different personhood thresholds, plus first-person accounts from disabled adults and parents regarding imposed life-sustaining treatment.',
    'If protection is primary, the victim classification weakens and the constraint shifts toward Rope; if institutional subjection dominates, the victim classification holds and Tangled Rope or Snare is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_victimhood_ambiguity, conceptual, 'Ambiguity over whether imposed personhood protects or extracts from infants.').

omega_variable(
    birth_moment_definitional_vagueness,
    'What empirical event constitutes ''birth'' â conception, heartbeat, viability, complete emergence, or first breath? The constraint''s operation depends on resolving this boundary, yet it is conventionally underspecified.',
    'Cross-jurisdictional legal analysis of birth definitions and neonatal practice standards; physiological studies on the transition from fetal to infant status.',
    'If the boundary is arbitrary or variable, the constraint''s accessibility_collapse is lower than modeled and its Mountain-like appearance is undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(birth_moment_definitional_vagueness, empirical, 'Empirical ambiguity in the definition of the birth moment.').

omega_variable(
    suppression_of_alternative_readings,
    'Are fitness-based and potential-based readings suppressed by active enforcement of the birth threshold, or are they merely minority positions in a pluralistic discourse?',
    'Citation analysis and policy-adoption tracking for fitness-based and potential-based personhood arguments across jurisdictions and medical institutions.',
    'If actively suppressed, the suppression metric is validated; if merely minority, suppression should be revised downward and the constraint may be a Rope rather than Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Whether alternative personhood readings are structurally suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_birth_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(personhood_birth_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(personhood_birth_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(personhood_birth_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(personhood_birth_tr_t80, personhood_boundary__birth_threshold_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(personhood_birth_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(personhood_birth_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(personhood_birth_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(personhood_birth_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(personhood_birth_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(personhood_birth_be_t80, personhood_boundary__birth_threshold_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(personhood_birth_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(personhood_boundary__birth_threshold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one reading of the personhood_boundary kernel. See sibling constraints fitness_contingent_reading and potential_based_reading for alternative structurally distinct claims that share the same kernel but instantiate different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
