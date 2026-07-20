% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Potential-Based Personhood Boundary
 *   domain: moral_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the potential_based_reading of the contested
 *   personhood_boundary kernel. It holds that moral personhood is grounded in
 *   the potential for rational agency, which entails that severely disabled
 *   infants lacking such potential may be denied full moral standing. The
 *   framework vests decisional authority in neonatal clinicians and parents,
 *   excluding the affected infants and marginalizing disability-rights
 *   critiques. It is claimed as a principled philosophical coordination
 *   device but operates with high extractiveness by stripping standing from
 *   the most vulnerable humans.
 *
 * KEY AGENTS:
 *   - profoundly_disabled_newborns: Primary target (powerless/trapped) â bear the extraction of moral standing and protective rights.
 *   - neonatal_clinicians: Primary agenda-setter (institutional/constrained) â control diagnostic framing and threshold application.
 *   - parents: Beneficiary (moderate/constrained) â receive delegated authority over life-and-death decisions.
 *   - bioethicists: Beneficiary (institutional/mobile) â expand professional jurisdiction through framework construction.
 *   - disability_rights_advocates: Excluded voice (organized/mobile) â contest the framework but are structurally backgrounded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.82).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.68).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Potential-Based Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '528f099a-6d4c-42d5-9a6f-aaa139eede81').
narrative_ontology:cs_kernel_codification('528f099a-6d4c-42d5-9a6f-aaa139eede81', formalized).
narrative_ontology:cs_authority_grounding('528f099a-6d4c-42d5-9a6f-aaa139eede81', expertise).
narrative_ontology:cs_interpretation_layer_present('528f099a-6d4c-42d5-9a6f-aaa139eede81').
narrative_ontology:cs_reading_relation('528f099a-6d4c-42d5-9a6f-aaa139eede81', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('528f099a-6d4c-42d5-9a6f-aaa139eede81', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_axiom('528f099a-6d4c-42d5-9a6f-aaa139eede81', foundational, potentiality_grounds_personhood).
narrative_ontology:cs_axiom_status(potentiality_grounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('528f099a-6d4c-42d5-9a6f-aaa139eede81', potentiality_grounds_personhood, deontological).
narrative_ontology:cs_axiom('528f099a-6d4c-42d5-9a6f-aaa139eede81', foundational, absence_of_potential_voids_standing).
narrative_ontology:cs_axiom_status(absence_of_potential_voids_standing, holdable).
narrative_ontology:cs_axiom_grounding('528f099a-6d4c-42d5-9a6f-aaa139eede81', absence_of_potential_voids_standing, deontological).
narrative_ontology:cs_reference_frame('528f099a-6d4c-42d5-9a6f-aaa139eede81', rational_agency_potentiality_framework).
narrative_ontology:cs_drift_state('528f099a-6d4c-42d5-9a6f-aaa139eede81', contemporary_disability_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('528f099a-6d4c-42d5-9a6f-aaa139eede81', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, neonatal_clinicians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, bioethicists).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, profoundly_disabled_newborns).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classified as lacking the potential for rational agency and therefore denied full moral standing under this framework; subject to withdrawal of care or non-treatment decisions made by proxies, without capacity to consent or contest.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, profoundly_disabled_newborns, payer,
    powerless, immediate, trapped, local).

% Assess prognostic potential and apply the personhood framework in NICU settings; their diagnostic judgments determine which infants fall below the potentiality threshold and which receive full intervention.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, neonatal_clinicians, agenda_setter,
    institutional, biographical, constrained, local).

% Granted moral and legal authority to authorize withholding or withdrawing treatment from infants deemed to lack personhood-potential; the constraint resolves their moral dilemma but places them in a decision role they may not have sought.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents, beneficiary,
    moderate, biographical, constrained, local).

% Construct, refine, and defend the potentiality criterion in academic and policy discourse; the framework provides them with a principled boundary-tool that expands their advisory role in life-and-death decisions.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethicists, beneficiary,
    institutional, generational, mobile, global).

% Assert the equal moral standing of all humans regardless of cognitive capacity; their critiques of the potentiality criterion are systematically backgrounded in mainstream bioethics frameworks that adopt this reading.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, neonatal_clinicians).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the boundary problem of moral standing for infants and cognitively impaired humans by grounding personhood in the potential for rational agency rather than actual current capacity.
% TRANSFER_FUNCTION: Transfers moral standing away from newborns who lack potential for rational agency and transfers decisional authority over their care to neonatal clinicians and parents.
% ABSENT_VOICES: Disability rights advocates and communities who affirm unconditional moral standing for all born humans; their perspectives are systematically backgrounded in bioethical frameworks that operationalize the potentiality criterion.
% DISAPPEARANCE_RATIONALE: If the potentiality constraint vanished, profoundly disabled newborns would be treated as full moral persons; neonatal protocols would shift toward universal intervention and the authority of clinicians and parents to withhold care would be severely curtailed.
% FOUNDING_PROBLEM: How to justify the moral standing of infants who lack current rational agency without extending equivalent standing to all biological human life, including embryos and the permanently unconscious.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights advocates and critical disability scholars attest the problem is a constructed dilemma serving medical authority; mainstream bioethicists and neonatologists attest it is a genuine conceptual problem. The corroboration is split, with substantial critique from outside the beneficiary set.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint denies fundamental moral standing to a class of humans, authorizing non-treatment. Suppression (0.68) reflects that alternative frameworks (universal standing, sanctity of life) are kept at the margins of bioethical and clinical discourse. Theater ratio (0.58) captures the growing performative dimension: as disability-rights critique has intensified, defenses of the potentiality criterion have become more elaborate and less empirically grounded. Accessibility collapse (0.58) indicates that once the potentiality framework is accepted, alternatives become cognitively distant, though they remain alive in external discourse. Resistance (0.72) is substantial due to sustained critique from disability communities and theological ethics.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (clinicians, bioethicists, parents) experience the constraint as a difficult but necessary conceptual tool for resolving tragic choices. The payer seat (profoundly disabled newborns) experiences it as a fundamental exclusion from the moral community. The engine computes this divergence from the structural asymmetry in power, exit, and role; the claim does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Neonatal clinicians, parents, and bioethicists are declared beneficiaries: they gain authority, professional jurisdiction, or decisional resolution from the constraint, yielding low directionality. Profoundly disabled newborns are declared victims: they bear the cost of excluded standing, yielding high directionality. Disability rights advocates are excluded from the framework's operation and neither benefit nor pay directly within the constraint's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this arrangement as pure coordination (a rope) because it explicitly names victims and active enforcement. It also prevents mislabeling it as a pure snare because the coordination function â resolving a genuine boundary problem in moral status â is structurally real and not merely cover. The high theater ratio warns that the coordination story is partially performative, but the extraction is layered onto a functioning conceptual mechanism rather than invented from whole cloth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_kind_vs_constructed_exclusion,
    'Is the category of ''lacking potential for rational agency'' a discovered natural kind or a socially constructed mechanism for excluding disabled infants from moral protection?',
    'Genealogical analysis of the criterion''s emergence in bioethics alongside historical sociology of neonatal care; comparison with cross-cultural personhood attributions.',
    'If constructed, the constraint is better classified as a snare using philosophical language as extraction cover; if a natural kind, it remains a contested mountain or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_vs_constructed_exclusion, conceptual, 'Whether the potentiality criterion reflects natural moral fact or constructed exclusion.').

omega_variable(
    axiom_stability_under_empirical_challenge,
    'Can the deontological axiom that potentiality grounds personhood survive systematic empirical challenge from disability studies showing that prognosis is often wrong and relational care produces outcomes the framework renders invisible?',
    'Longitudinal outcome studies of NICU infants alongside critical disability ethnography.',
    'If the axiom collapses under empirical pressure, the constraint loses its coordination legitimacy and extraction dominates; if it holds, the tangled rope balance persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_stability_under_empirical_challenge, empirical, 'Stability of the potentiality axiom under empirical challenge.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative personhood frameworks structural (enforced by medical institutions and bioethics curricula) or internalized (accepted as philosophical self-evidence by practitioners)?',
    'Post-exit practitioner interviews and curriculum analysis; tracking whether clinicians who leave high-pressure NICU settings continue to affirm the potentiality framework.',
    'If internalized, effective suppression exceeds the structural measure and the constraint is more deeply embedded than institutional metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of alternative frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_potential_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(personhood_potential_tr_t10, personhood_boundary__potential_based_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(personhood_potential_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(personhood_potential_tr_t30, personhood_boundary__potential_based_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(personhood_potential_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(personhood_potential_tr_t50, personhood_boundary__potential_based_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(personhood_potential_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(personhood_potential_be_t10, personhood_boundary__potential_based_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(personhood_potential_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(personhood_potential_be_t30, personhood_boundary__potential_based_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(personhood_potential_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(personhood_potential_be_t50, personhood_boundary__potential_based_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(personhood_potential_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(personhood_potential_su_t10, personhood_boundary__potential_based_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(personhood_potential_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(personhood_potential_su_t30, personhood_boundary__potential_based_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(personhood_potential_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(personhood_potential_su_t50, personhood_boundary__potential_based_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three structurally distinct constraints because each reading assigns a different epsilon and a different victim set. The potential-based reading extracts by denying standing to the disabled; the birth-threshold reading extracts differently (if at all) by including all born humans; the fitness-contingent reading extracts by denying standing to all pre-fitness entities including typical infants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
