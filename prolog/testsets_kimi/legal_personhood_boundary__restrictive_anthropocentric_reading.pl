% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Restrictive Anthropocentric Legal Personhood Boundary
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive_anthropocentric_reading of
 *   the legal_personhood_boundary kernel. It limits legal personhood to born
 *   humans possessing cognitive capacity, thereby excluding fetuses,
 *   non-human animals, ecosystems, and artificial intelligences from the set
 *   of rights-bearing entities. The reading maximizes pregnant-person
 *   autonomy and minimizes state intervention in reproduction and
 *   environmental law by establishing a hard anthropocentric threshold. It is
 *   structurally distinct from the developmental_potentiality_reading
 *   (personhood at conception) and the functional_capacity_reading
 *   (personhood regardless of species).
 *
 * KEY AGENTS:
 *   - Cognitively capable humans: Primary beneficiaries (organized/mobile) who receive full legal rights and standing.
 *   - Pregnant persons: Secondary beneficiaries (moderate/mobile) who gain reproductive autonomy from fetal exclusion.
 *   - Legal administrators: Agenda-setters (institutional/analytical) who enforce and interpret the boundary, benefiting from administrative simplicity.
 *   - Excluded fetuses: Excluded non-agents (powerless/trapped) denied standing and protection.
 *   - Non-human animals: Excluded non-agents (powerless/trapped) denied rights regardless of sentience.
 *   - Artificial intelligence systems: Excluded non-agents (powerless/trapped) denied personhood despite functional capacity.
 *   - Ecosystems: Excluded non-agents (powerless/trapped) denied standing in environmental law.
 *   - Profoundly disabled humans: Payers (powerless/constrained) who bear the risk of capacity-based exclusion despite being born human.
 *   - Animal rights advocates: Observers (organized/mobile) who challenge the boundary on behalf of excluded entities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.53).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Restrictive Anthropocentric Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal/philosophical").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'dfcf42f0-0d97-49bf-ba1f-6e8630da252c').
narrative_ontology:cs_kernel_codification('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', fixed_text).
narrative_ontology:cs_authority_grounding('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', lineage).
narrative_ontology:cs_interpretation_layer_present('dfcf42f0-0d97-49bf-ba1f-6e8630da252c').
narrative_ontology:cs_reading_relation('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', foundational, moral_status_intrinsic_to_human_species).
narrative_ontology:cs_axiom_status(moral_status_intrinsic_to_human_species, holdable).
narrative_ontology:cs_axiom_grounding('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', moral_status_intrinsic_to_human_species, deontological).
narrative_ontology:cs_axiom('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', foundational, birth_as_necessary_for_independent_rights).
narrative_ontology:cs_axiom_status(birth_as_necessary_for_independent_rights, holdable).
narrative_ontology:cs_axiom_grounding('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', birth_as_necessary_for_independent_rights, conventional).
narrative_ontology:cs_reference_frame('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', classical_liberal_personhood).
narrative_ontology:cs_drift_state('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', contemporary_bioethical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dfcf42f0-0d97-49bf-ba1f-6e8630da252c', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_capable_humans).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_administrators).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, excluded_fetuses).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_systems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, profoundly_disabled_humans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born humans with demonstrated cognitive capacity who receive full legal rights, court standing, and protection from harm under the restrictive personhood framework. Their status as rights-bearers is the default assumption of the legal system.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_capable_humans, beneficiary,
    organized, generational, mobile, national).

% Benefit from reproductive autonomy and reduced state intervention in pregnancy because fetuses are excluded from personhood status, removing competing legal claims from within their own bodies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, mobile, national).

% Courts, legislatures, and administrative bodies that define, interpret, and enforce the personhood boundary. They benefit from an administratively simple standard that avoids complex standing questions for non-human and pre-birth entities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Human organisms at pre-birth developmental stages who are categorically denied legal personhood, standing, and rights protection under the restrictive framework, making them subject to termination or experimentation without legal recourse.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, excluded_fetuses, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, excluded_fetuses).

% Sentient and non-sentient animals excluded from legal personhood regardless of cognitive capacity, permitting their use in research, agriculture, and entertainment without rights-based constraints.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals).

% Advanced computational systems with functional cognitive capacities analogous to humans but excluded from personhood due to non-biological status, leaving them without legal standing or protections.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_systems, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_systems).

% Complex ecological systems and natural collectivities excluded from personhood status, preventing them from holding rights or being represented as plaintiffs in environmental and property law.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems).

% Born humans who lack or have lost cognitive capacity due to congenital conditions, injury, or disease, who live under threat of diminished legal status or guardianship substitution because the capacity requirement questions their full personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, profoundly_disabled_humans, payer,
    powerless, biographical, constrained, national).

% Represent non-human animals in legal and political discourse, challenging the anthropocentric boundary through litigation and legislative advocacy, but facing structural disadvantage because their would-be clients lack standing ab initio.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable criterion for legal rights and standing, resolving who may bring claims, hold duties, and receive state protection without requiring infinite case-by-case adjudication of every entity's moral status.
% TRANSFER_FUNCTION: Moves legal protection, court standing, and autonomy from excluded entities toward cognitively capable born humans and legal institutions seeking administrable standards.
% ABSENT_VOICES: Fetuses, non-human animals, ecosystems, and AI systems are structurally excluded from legal speech; their would-be advocates face institutional barriers because the framework denies their clients standing at the threshold.
% DISAPPEARANCE_RATIONALE: If the restrictive personhood boundary vanished, fetuses would gain competing claims in reproductive law, non-human animals and ecosystems could become plaintiffs in environmental and cruelty proceedings, AI systems would be candidates for rights protections, and the legal system would face a standing and rights-allocation crisis requiring fundamental reconstruction.
% FOUNDING_PROBLEM: How to establish an administrable boundary for legal rights in a pluralistic society without collapsing the legal system under infinite competing demands for standing and recognition.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians acknowledge the administrative necessity of boundaries, but critical animal studies scholars, disability theorists, and some bioethicists attest from outside the benefiting legal-administrative class that technological and moral evolution has rendered the restrictive solution obsolete.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial cost of exclusion from legal personhood: entities without standing can be destroyed, experimented upon, or deprived of habitat without legal remedy. Suppression (0.53) captures the active legal enforcement required to maintain the boundary against mounting empirical challenges from animal cognition science and AI development. Theater ratio (0.38) registers the increasing proportion of legal and philosophical argument devoted to justifying the boundary as purely definitional rather than functional. Accessibility collapse (0.78) is high because, once the restrictive framework is accepted, alternative allocations of personhood become legally unimaginable without systemic overhaul. Resistance (0.55) reflects active but structurally disadvantaged opposition from animal rights, disability justice, and AI ethics movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as necessary coordinationâan administrable solution to infinite rights-claims. The excluded and payer seats experience it as categorical erasure. The engine computes this divergence from structural position: beneficiaries have mobile exit and organized power; excluded entities are trapped and powerless, amplifying their effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal administrators and capable humans sit near the beneficiary pole (low d) because the constraint subsidizes their legal status and administrative convenience. Pregnant persons also sit near the beneficiary pole because fetal exclusion directly expands their autonomy. Excluded fetuses, animals, AI, and ecosystems sit at the full-target end (high d) because the constraint's primary operation is to deny them standing. Profoundly disabled humans sit nearer the target end due to their constrained exit and powerlessness in the face of capacity-testing frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function: legal systems require standing rules to function, and a capacity threshold is not merely cover for extraction. However, the asymmetric distribution of costsâexclusion falls on entities with no voiceâreveals the extractive component. A snare classification would miss the real administrative problem the constraint solves; a rope classification would miss the structural violence against the excluded. The tangled_rope type captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_indeterminacy,
    'What precise cognitive capacity test distinguishes persons from non-persons, and does the restrictive reading collapse into functional_capacity when applied consistently?',
    'Comparative legal analysis of capacity tests across jurisdictions and empirical review of which humans would fail proposed animal-level capacity benchmarks.',
    'If capacity tests are applied consistently across species, the restrictive reading either abandons its anthropocentrism or excludes more humans, forcing reclassification toward either functional_capacity or a more extractive snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_indeterminacy, conceptual, 'Indeterminacy in capacity threshold application').

omega_variable(
    construct_natural_law_ambiguity,
    'Is the restrictive personhood boundary a constructed legal convenience or a discovery of natural moral kinds?',
    'Historical genealogy of personhood concepts revealing the contingency of birth and species criteria across legal traditions.',
    'If constructed, the constraint is a coordination mechanism chosen for administrative ease rather than moral truth, strengthening tangled_rope classification. If natural, it approaches mountain territory despite its beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construct_natural_law_ambiguity, conceptual, 'Whether personhood is constructed or natural').

omega_variable(
    disabled_human_exclusion_pressure,
    'Does the cognitive capacity requirement structurally threaten the personhood status of profoundly disabled humans, or is their status protected by a separate legal presumption?',
    'Tracking legal trends in guardianship, euthanasia, and disability rights to see if capacity tests increasingly erode protections for disabled humans.',
    'If capacity tests erode protections for disabled humans, the victim set expands within the human species and the constraint''s extractiveness increases. If protected by presumption, the anthropocentric boundary remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disabled_human_exclusion_pressure, empirical, 'Capacity requirement threat to disabled humans').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_ra_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lpb_ra_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(lpb_ra_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lpb_ra_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(lpb_ra_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(lpb_ra_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(lpb_ra_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lpb_ra_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(lpb_ra_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(lpb_ra_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(lpb_ra_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(lpb_ra_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lpb_ra_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lpb_ra_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(lpb_ra_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(lpb_ra_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(lpb_ra_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(lpb_ra_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legal_personhood_boundary kernel. The restrictive_anthropocentric_reading, developmental_potentiality_reading, and functional_capacity_reading are structurally distinct constraints with different epsilon values, victim sets, and beneficiary structures. They share a natural-language label but instantiate different constraints per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
