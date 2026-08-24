% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Personhood Begins at Birth; All Born Humans Possess Moral Standing
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the birth_threshold_reading of the
 *   personhood_boundary kernel: the claim that personhood attaches at birth
 *   and all born humans possess full moral standing. Historically, this
 *   reading emerged from natural law traditions, religious doctrines (e.g.,
 *   ensoulment at birth), and modern human rights frameworks (UDHR Article 1,
 *   CRC). It functions as a constraint on state power — prohibiting
 *   infanticide, mandating equal legal protection, and denying state
 *   authority to exclude any born human from personhood. The reading claims
 *   Mountain status (natural moral fact), but its historical trajectory shows
 *   rising enforcement costs (suppression_requirement increasing from 0.3 to
 *   0.45 over 250 time units) and moderate extraction from state exclusionary
 *   authority (extractiveness 0.35). The constraint requires active
 *   enforcement through criminal law, child protection systems, and
 *   international monitoring. Its beneficiaries are all born humans; its
 *   cost-bearers are state authorities that lose exclusionary discretion. Two
 *   sibling readings contest the kernel: fitness_contingent_reading
 *   (personhood requires demonstrated capacities) and potential_based_reading
 *   (personhood requires potential for rational agency). Both exclude subsets
 *   of born humans that this reading protects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.35).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.45).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Begins at Birth; All Born Humans Possess Moral Standing").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'b943425d-df16-4ae0-b22a-86491183839f').
narrative_ontology:cs_kernel_codification('b943425d-df16-4ae0-b22a-86491183839f', fixed_text).
narrative_ontology:cs_authority_grounding('b943425d-df16-4ae0-b22a-86491183839f', lineage).
narrative_ontology:cs_interpretation_layer_present('b943425d-df16-4ae0-b22a-86491183839f').
narrative_ontology:cs_reading_relation('b943425d-df16-4ae0-b22a-86491183839f', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('b943425d-df16-4ae0-b22a-86491183839f', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('b943425d-df16-4ae0-b22a-86491183839f', foundational, all_born_humans_possess_moral_standing).
narrative_ontology:cs_axiom_status(all_born_humans_possess_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('b943425d-df16-4ae0-b22a-86491183839f', all_born_humans_possess_moral_standing, deontological).
narrative_ontology:cs_axiom('b943425d-df16-4ae0-b22a-86491183839f', foundational, personhood_attaches_at_birth_not_capacity).
narrative_ontology:cs_axiom_status(personhood_attaches_at_birth_not_capacity, holdable).
narrative_ontology:cs_axiom_grounding('b943425d-df16-4ae0-b22a-86491183839f', personhood_attaches_at_birth_not_capacity, deontological).
narrative_ontology:cs_reference_frame('b943425d-df16-4ae0-b22a-86491183839f', universal_birth_personhood).
narrative_ontology:cs_drift_state('b943425d-df16-4ae0-b22a-86491183839f', contemporary_bioethics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b943425d-df16-4ae0-b22a-86491183839f', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, state_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, medical_professionals).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity_at_birth).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, prohibition_on_infanticide).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_protection_under_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All born humans are the protected class. They cannot exit personhood — it constitutes their moral and legal identity. The constraint guarantees them protection from killing, equal legal standing, and inclusion in the moral community. They do not administer the constraint; they receive its protection. Their situation is defined by vulnerability: without this constraint, they are exposed to infanticide, abandonment, and legal exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, beneficiary,
    powerless, biographical, identity_locked, universal).

% The state enacts and enforces laws implementing universal birth personhood (criminal prohibitions on infanticide, child protection statutes, equal protection clauses). It bears the cost of enforcement (courts, police, welfare systems) and loses the sovereign authority to define personhood boundaries (e.g., cannot exclude disabled infants, cannot permit exposure). The state cannot easily exit this constraint — abandoning it would trigger legitimacy collapse and international sanction — but can reform its implementation. The state also benefits from the coordination function: a stable, universally protected citizenry.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, state_authority, payer).

% Physicians and neonatologists are constrained in treatment decisions for severely disabled newborns. They cannot withhold life-sustaining treatment based on quality-of-life judgments that would constitute a personhood determination. Professional ethics and law bind them to preserve all born human life. Their exit options are constrained: they can advocate for policy change, but cannot individually opt out of the constraint without professional sanction. They bear moral distress costs in marginal cases.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_professionals, payer,
    organized, biographical, constrained, national).

% Philosophers, bioethicists, and policy advocates who hold that personhood requires demonstrated capacities (consciousness, rationality, autonomy). They argue that anencephalic infants, persistent vegetative state patients, and severely cognitively disabled humans lack moral standing. Their reading is excluded from legal frameworks adopting the birth threshold. They can publish, debate, and lobby — they are not silenced — but their view cannot be implemented as law without overturning the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_contingent_advocates, excluded,
    moderate, biographical, mobile, global).

% Advocates (often from disability ethics, religious traditions, or specific philosophical frameworks) who ground personhood in potential for rational agency. They argue that severely disabled infants who lack such potential may not possess full moral standing. Like fitness-contingent advocates, they are excluded from legal implementation but remain active in academic and policy discourse. Their reading specifically targets the birth threshold's inclusion of severely disabled infants.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, potential_based_advocates, excluded,
    moderate, biographical, mobile, global).

% Analytical observers who study the kernel contest, map the structural relationships between readings, and evaluate the constraint's classification. They do not collect rents from the constraint nor bear its enforcement costs. Their exit is analytical — they can adopt any reading as an intellectual position without material consequence.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, moral_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the 'who counts as a person' coordination problem by establishing a bright-line, non-negotiable threshold (birth) that is publicly verifiable and requires no capacity assessment. This prevents sliding exclusions, factional disputes over fitness criteria, and state arbitrariness in defining the protected class.
% TRANSFER_FUNCTION: Transfers exclusionary authority from state to protected class: the state loses the power to define personhood boundaries; born humans gain indefeasible protection. The transfer is not monetary but jurisdictional — the state's sovereign prerogative to exclude is extinguished for the born.
% ABSENT_VOICES: Pre-born humans (fetuses) are structurally absent — they are not in the conversation because the constraint defines them as outside the personhood boundary. Severely disabled infants who cannot self-advocate are present only through proxies (parents, guardians, state). Future generations who might prefer a different personhood boundary are absent by definition.
% DISAPPEARANCE_RATIONALE: If the birth threshold constraint vanished overnight, states would immediately regain authority to define personhood boundaries. Infanticide laws would be challenged or repealed. Disabled infants would lose equal protection guarantees. The fitness-contingent and potential-based readings would become legally implementable. The moral and legal landscape would reorganize around contested capacity assessments.
% FOUNDING_PROBLEM: Prevent arbitrary state exclusion of born humans from moral and legal protection; solve the coordination problem of 'who counts' with a bright-line rule that requires no capacity assessment and resists sliding exclusions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the drafting history of the UDHR and CRC (state delegates from multiple traditions), by the historical record of infanticide and exposure practices that the constraint was built to abolish, and by contemporary disability rights movements that cite the constraint as essential protection. No single beneficiary group controls the corroboration — states, NGOs, and independent scholars all attest the problem's reality, though they contest whether the birth threshold is the correct solution.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the constraint's transfer of exclusionary power from state to protected class — states lose the authority to define personhood boundaries. Suppression (0.45) captures the coercive machinery needed to prevent exclusionary practices (infanticide, selective neglect, discriminatory laws). Theater ratio (0.15) is low: enforcement machinery (courts, child welfare, international bodies) performs genuine protective work, though performative rhetoric exists. Accessibility collapse (0.65) is moderately high: alternative personhood boundaries (fitness, potential) remain conceptually available but are legally foreclosed in jurisdictions adopting this reading. Resistance (0.55) reflects persistent philosophical contestation and pockets of non-compliance (e.g., neonatal euthanasia debates, abortion-as-infanticide framings). The claimed_type is tangled_rope because the constraint combines genuine coordination (universal protection solving the 'who counts' problem) with asymmetric extraction (state loses exclusionary power) and requires active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the born human seat (identity_locked, universal scope), the constraint appears as Mountain — natural, unchosen, defining their moral existence. From the state authority seat (institutional, national scope, constrained exit), it appears as Tangled Rope — coordination function (stable citizenry) mixed with extraction (lost exclusionary power). From the excluded advocate seats, it appears as Snare — their coherent alternative readings are suppressed by the constraint's enforcement. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are structural beneficiaries (d ~ 0.15): the constraint subsidizes their survival and legal standing. State authority is the primary payer (d ~ 0.85): it bears enforcement costs and loses exclusionary discretion. Medical professionals are secondary payers (d ~ 0.7): constrained in treatment decisions for severely disabled newborns. Fitness-contingent and potential-based advocates are excluded (d not computed): their readings are foreclosed within frameworks adopting this constraint. Moral philosophers are observers (d = 0.5): analytical seat, neither collecting nor paying. The engine will derive directionality from these structural positions plus exit options: born humans are identity_locked (cannot exit personhood), state authority is constrained (can reform but not abolish without regime change), medical professionals are constrained (professional ethics bind them).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'prevent arbitrary exclusion of born humans from moral/legal protection' — remains live (contested status). The constraint has not atrophied into a Piton: enforcement is active, not theatrical; extraction is not diffuse but targeted at exclusionary authority. No concentrated beneficiary captures the extraction — protection is universal. Mandatrophy is not resolved: the constraint's function (universal protection) is still the live problem it was built to solve, though the fitness/potential readings contest whether the solution is over-inclusive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the birth threshold for personhood a genuine natural moral law (Mountain) or a constructed norm that benefits identifiable agents (Tangled Rope/Snare)?',
    'Cross-cultural historical analysis of personhood boundaries; convergence/divergence of independent moral traditions on birth as threshold; empirical test of whether the constraint persists without active enforcement.',
    'If natural law, classification should be Mountain with emerges_naturally=true and negligible extraction. If constructed, current Tangled Rope classification stands and extraction metrics reflect enforcement costs borne by state authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Whether the birth threshold is a mind-independent moral fact or a socially maintained commitment.').

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s structural relationship to the personhood_boundary kernel differ from its sibling readings?',
    'Comparative analysis of the three readings'' beneficiary/victim sets, enforcement requirements, and authority grounding. The engine computes per-reading classifications from structural data.',
    'If sibling readings produce different constraint types (e.g., fitness_contingent_reading as Snare extracting from disabled infants), the kernel''s internal structure explains contestation patterns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee-frame omega: this constraint is the birth_threshold_reading of kernel personhood_boundary; siblings are fitness_contingent_reading and potential_based_reading. Structural deltas: this reading includes all born infants in beneficiary set (protected class), forecloses state exclusion authority; siblings exclude subsets of born humans.').

omega_variable(
    enforcement_as_extraction_on_state,
    'Does the active enforcement of universal birth personhood constitute asymmetric extraction from state authority, or is the state''s compliance cost the price of legitimate coordination?',
    'Measure state capacity diverted to enforcing universal protection vs. counterfactual exclusionary regimes; assess whether states gain net legitimacy benefit exceeding enforcement cost.',
    'If net extraction from state, Tangled Rope classification strengthens. If net coordination benefit to state, Rope classification may apply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_as_extraction_on_state, empirical, 'Whether the constraint''s coordination function (universal protection) generates net benefit for the enforcing authority or net cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t50, personhood_boundary__birth_threshold_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t150, personhood_boundary__birth_threshold_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t200, personhood_boundary__birth_threshold_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_tr_t250, personhood_boundary__birth_threshold_reading, theater_ratio, 250, 0.15).

% Extraction over time
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t50, personhood_boundary__birth_threshold_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t150, personhood_boundary__birth_threshold_reading, base_extractiveness, 150, 0.32).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t200, personhood_boundary__birth_threshold_reading, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_be_t250, personhood_boundary__birth_threshold_reading, base_extractiveness, 250, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t50, personhood_boundary__birth_threshold_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t100, personhood_boundary__birth_threshold_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t150, personhood_boundary__birth_threshold_reading, suppression_requirement, 150, 0.42).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t200, personhood_boundary__birth_threshold_reading, suppression_requirement, 200, 0.44).
narrative_ontology:measurement(personhood_boundary__birth_threshold_reading_su_t250, personhood_boundary__birth_threshold_reading, suppression_requirement, 250, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__birth_threshold_reading, 0.08).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% Kernel personhood_boundary decomposes into three readings with distinct ε values and beneficiary/victim structures. birth_threshold_reading (this story): ε≈0.35, beneficiaries=born_humans, victims=state_authority, type=tangled_rope. fitness_contingent_reading: ε higher (excludes disabled infants), beneficiaries=fit_humans, victims=pre_fitness_infants, type=snare. potential_based_reading: ε moderate, beneficiaries=potential_agents, victims=severely_disabled_infants, type=tangled_rope. The upstream reading (birth_threshold) influences downstream readings by setting the dominant legal baseline that siblings must contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, institutional, 0.85).
constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
