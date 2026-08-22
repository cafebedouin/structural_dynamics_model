% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual Catastrophe Memory Preservation â Survival Competence Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint is the survival_competence_reading of the contested
 *   kernel catastrophe_memory_preservation. The kernel concerns what ritual
 *   practice preserves across generations after collective catastrophe. This
 *   reading claims that ritual encodes and transfers operational
 *   threat-recognition competence, entangling genuine coordination
 *   (intergenerational survival) with asymmetric extraction
 *   (present-generation autonomy and resources). The sibling
 *   mourning_practice_reading treats the same ritual as symbolic continuity
 *   without operational transfer, while hybrid_atrophy_reading treats it as
 *   once-operational competence that has decayed under modernity. The
 *   claim/metric independence is maintained: the claimed type is
 *   tangled_rope, and the metrics describe high extraction (0.78),
 *   substantial suppression (0.72), and moderate theater (0.45) at interval
 *   end.
 *
 * KEY AGENTS:
 *   - future_generations: Structural beneficiary (powerless/civilizational/trapped) â receives preserved threat-recognition capacity without having paid ritual costs.
 *   - present_generation_community: Primary target (moderate/biographical/identity_locked) â bears autonomy and resource costs of compulsory ritual participation.
 *   - ritual_elite: Agenda setter (institutional/generational/constrained) â administers ritual and enforces intergenerational transmission norms.
 *   - secular_modernizers: Excluded voice (organized/generational/analytical) â advocates non-ritual alternatives but lacks standing within ritual communities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Catastrophe Memory Preservation â Survival Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '3e36acf8-7c51-4009-9fff-57e2c3b4d52f').
narrative_ontology:cs_kernel_codification('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', distributed).
narrative_ontology:cs_authority_grounding('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', lineage).
narrative_ontology:cs_interpretation_layer_present('3e36acf8-7c51-4009-9fff-57e2c3b4d52f').
narrative_ontology:cs_reading_relation('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', foundational, operational_competence_transfers).
narrative_ontology:cs_axiom_status(operational_competence_transfers, holdable).
narrative_ontology:cs_axiom_grounding('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', operational_competence_transfers, empirically_contingent).
narrative_ontology:cs_axiom('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', foundational, generational_duty_over_autonomy).
narrative_ontology:cs_axiom_status(generational_duty_over_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', generational_duty_over_autonomy, deontological).
narrative_ontology:cs_reference_frame('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', ancestral_survival_competence_frame).
narrative_ontology:cs_drift_state('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', modern_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e36acf8-7c51-4009-9fff-57e2c3b4d52f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit a community that retains (or claims to retain) operational threat-recognition capacity embedded in ritual practice. They receive the benefit of preserved competence without having paid its costs, but also cannot opt out of the historical and cultural frame they are born into.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, regional).

% Bear the recurring costs of ritual participationâtime, material resources, bodily discipline, and curtailed individual autonomyâunder the expectation that their compliance secures collective survival capacity for descendants. Exit means abandoning communal identity, protective social structure, and often material support networks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_community, payer,
    moderate, biographical, identity_locked, regional).

% Administer the ritual, adjudicate correct performance, transmit esoteric threat-recognition content, and enforce participation norms through social sanction and doctrinal authority. Their authority and social role derive from maintaining the intergenerational transfer mechanism.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_elite, agenda_setter,
    institutional, generational, constrained, regional).

% Advocate for institutional, technological, or educational alternatives to ritual transmission of catastrophe memory. Are excluded from normative discourse within ritual communities and lack standing to alter ritual performance, though their critiques circulate in adjacent academic and policy spheres.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, secular_modernizers, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the generational decay of catastrophe knowledge: individual memory dies with the eyewitness generation, and written records may not encode embodied, situation-specific response patterns. Ritual compresses and repetitively rehearses threat-recognition and response protocols so they survive the memory horizon.
% TRANSFER_FUNCTION: Moves time, bodily discipline, cognitive attention, and autonomy from present-generation participants to future-generation survival capacity by encoding operational knowledge in repeatable, costly ceremonial action.
% ABSENT_VOICES: Secular modernizers and individual autonomy advocates who would reject costly ritual obligation; also descendants who might prefer updated, non-ritual transmission methods but are not yet born to voice this.
% DISAPPEARANCE_RATIONALE: If the ritual constraint vanished, the embodied, repeated rehearsal of threat-recognition would cease. Within two to three generations, the specific operational competence would atrophy, leaving only textual or symbolic residue. The community would rearrange around either renewed vulnerability or alternative (untested) transmission mechanisms.
% FOUNDING_PROBLEM: Catastrophe amnesia: societies that survive extreme events systematically forget the operational specifics of that survival within 2-3 generations, leaving them vulnerable to recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropology and longue-durÃ©e history (outside the ritual beneficiary set) document generational forgetting curves; however, corroboration that ritual is the necessary or sufficient solution (as opposed to institutional, technological, or textual alternatives) is contested and primarily asserted by the ritual authority itself.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.52 to 0.78) because the original catastrophe recedes from living memory, increasing the cost-to-obvious-benefit ratio for present participants while the ritual demands remain fixed or intensify. Suppression is high (0.72) because persistence depends on active enforcement of participation norms against declining voluntary adherence. Theater is moderate (0.45): the survival-competence reading asserts genuine operational content persists, but the rising theater trajectory acknowledges that performative maintenance grows as direct empirical verification of the threat-recognition function becomes rarer. Accessibility collapse (0.68) reflects that once a community is organized around ritual transmission, non-ritual alternatives appear unthinkable or impious. Resistance (0.55) captures quiet noncompliance, intergenerational friction, and occasional defections.
 *
 * PERSPECTIVAL GAP:
 *   The future-generations seat and the present-generation seat compute divergently. From the future seat, the constraint is protective coordination that pays survival dividends. From the present seat, the same structure is costly extraction of autonomy and labor in service of a threat that may never materialize. The ritual_elite seat experiences administration of a necessary tradition; the excluded secular_modernizer seat sees unnecessary superstition. The engine computes these divergences from the same structural data without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary structural asymmetry is intergenerational. Future generations are declared beneficiaries (low d, near 0.0): they receive preserved competence without paying its costs. The present_generation_community are declared victims/payers (high d, near 1.0): they bear the autonomy and resource extraction directly, and their exit is identity_locked, amplifying effective extraction. The ritual_elite are agenda_setters rather than beneficiaries; they administer the constraint but are structurally constrained by it (their authority depends on its continuation), placing their derived d near the symmetric middle. Directionality is not overridden because the structural derivation from beneficiary/victim declarations plus exit options accurately captures the intergenerational extraction geometry.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling by requiring both a genuine coordination function (generational survival of catastrophe competence) and identifiable asymmetric extraction (present autonomy sacrificed). If the operational content were fully atrophied, the coordination function would be hollow and the constraint would trend toward snare. If the participation were fully voluntary and low-cost, it would trend toward rope. The temporal measurements show extraction accumulating but theater remaining below 0.5, consistent with a tangled rope where coordination and extraction are structurally entangled rather than one being cover for the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_operational_falsifiability,
    'Does the ritual actually encode operational threat-recognition competence that improves survival outcomes, or has its operational content atrophied into symbolic performance?',
    'Controlled observation of catastrophe-response performance in communities with and without the ritual tradition, or archaeological and epidemiological correlation of ritual maintenance with disaster-resilience outcomes.',
    'If operational, the extractiveness measure overstates exploitation and the constraint is more genuinely coordinating; if symbolic, the coordination function is hollow and the constraint trends toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_operational_falsifiability, empirical, 'Whether ritual encodes genuine survival competence or symbolic residue').

omega_variable(
    participation_suppression_mechanism,
    'Is present-generation compliance enforced through structural community sanctions (exclusion, material penalty) or through internalized sacred duty and identity fusion?',
    'Post-exit trajectory study: whether defectors continue to self-police and experience guilt or fear after leaving the community, indicating internalized suppression.',
    'If internalized, effective suppression exceeds structural measure and the payer''s directionality sits closer to full target; if purely structural, the constraint is more readily dismantled by removing enforcers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'Is the survival-competence reading the correct interpretation of the ritual kernel, or do the mourning-practice and hybrid-atrophy readings capture more of the ritual''s actual function?',
    'Comparative ethnography and functional analysis across ritual communities; assessment of whether ritual performance correlates with measurable survival-competence transfer versus symbolic identity maintenance.',
    'If the mourning-practice reading is more accurate, this constraint is misclassified as tangled_rope and should be re-evaluated as rope or piton; if hybrid-atrophy, the temporal measurements should show rising theater_ratio and extractiveness over the interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Indeterminacy between competing kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.43).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.73).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
