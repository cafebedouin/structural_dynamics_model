% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo Incoherent Bundle
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo, the historical syncretism of kami worship and Buddhism
 *   in Japan, is read here not as a coherent theological system but as an
 *   institutionally sustained bundle of contradictory commitments. The
 *   constraint persists through institutional inertia and the practical
 *   efficacy of its rituals, masking its theoretical incoherence with
 *   performative success. Attempts at ontological separation (notably the
 *   Meiji shinbutsu bunri) failed because the bundle's institutional
 *   embedding exceeded the reformers' capacity to disentangle it. This
 *   reading treats the natural-language concept 'shinbutsu-shugo' as a
 *   contested kernel: the honji_suijaku_monism and domain_partition readings
 *   instantiate structurally different constraints with different epsilon
 *   profiles.
 *
 * KEY AGENTS:
 *   - Religious institutions (agenda_setter, institutional/constrained): Administer the syncretic bundle and could theoretically rationalize it, but the cost of disentanglement exceeds the benefit.
 *   - Practitioners (beneficiary/payer, moderate/constrained): Receive practical ritual coordination but pay with ontological incoherence.
 *   - Ritual specialists (payer, moderate/identity_locked): Bear the cost of identity fusion with a contradictory performative system.
 *   - Theological reformers (payer, moderate/constrained): Bear the political and cognitive costs of failed separation attempts.
 *   - Comparative religion scholars (observer, analytical): External analytical seat documenting the structural persistence of contradictions.
 *   - Shinto orthodox revivalists (excluded, moderate/constrained): Marginalized voices demanding separation that the inertial bundle neutralizes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.55).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.74).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '6fe84231-3c78-425b-bdcc-b9ef42760400').
narrative_ontology:cs_kernel_codification('6fe84231-3c78-425b-bdcc-b9ef42760400', distributed).
narrative_ontology:cs_authority_grounding('6fe84231-3c78-425b-bdcc-b9ef42760400', practice).
narrative_ontology:cs_interpretation_layer_present('6fe84231-3c78-425b-bdcc-b9ef42760400').
narrative_ontology:cs_reading_relation('6fe84231-3c78-425b-bdcc-b9ef42760400', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('6fe84231-3c78-425b-bdcc-b9ef42760400', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('6fe84231-3c78-425b-bdcc-b9ef42760400', foundational, practical_efficacy_legitimizes_contradiction).
narrative_ontology:cs_axiom_status(practical_efficacy_legitimizes_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('6fe84231-3c78-425b-bdcc-b9ef42760400', practical_efficacy_legitimizes_contradiction, instrumental).
narrative_ontology:cs_axiom('6fe84231-3c78-425b-bdcc-b9ef42760400', foundational, institutional_continuity_over_ontological_clarity).
narrative_ontology:cs_axiom_status(institutional_continuity_over_ontological_clarity, holdable).
narrative_ontology:cs_axiom_grounding('6fe84231-3c78-425b-bdcc-b9ef42760400', institutional_continuity_over_ontological_clarity, conventional).
narrative_ontology:cs_reference_frame('6fe84231-3c78-425b-bdcc-b9ef42760400', medieval_syncretic_practice).
narrative_ontology:cs_drift_state('6fe84231-3c78-425b-bdcc-b9ef42760400', contemporary_academic_scrutiny, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6fe84231-3c78-425b-bdcc-b9ef42760400', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theological_reformers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the syncretic ritual complex, maintaining contradictory ontological commitments simultaneously. They inherit the bundle from history and sustain it through routine practice rather than active theological defense. Could theoretically rationalize the system but the cost of disentangling intertwined patronage networks, ritual calendars, and property arrangements exceeds the benefit.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Participate in rituals that function effectively for life-cycle needs but receive contradictory ontological guidance depending on whether the ritual frame is Shinto or Buddhist. Bear the cognitive cost of holding fusion and separation simultaneously without a coherent metanarrative.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, practitioners, payer).

% Perform rituals requiring simultaneous invocation of kami and buddhas using incompatible ontological frameworks. Their professional identity and training are constituted by the syncretic bundle; exiting would require abandoning their vocation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ritual_specialists, payer,
    moderate, biographical, identity_locked, regional).

% Attempt to impose ontological clarity and separation, such as the Meiji-era shinbutsu bunri, but encounter resistance from embedded institutional practice. Their separation attempts are absorbed or subverted by the inertial weight of the syncretic bundle.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theological_reformers, payer,
    moderate, generational, constrained, national).

% Observe the structural incoherence from outside the tradition, documenting the persistence of contradictory commitments and the failure of separation attempts across historical periods.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% Advocate for pure Shinto free of Buddhist contamination. Their voices are structurally marginalized by the syncretic institutional complex despite periodic state support; the inertial bundle absorbs or neutralizes their critiques without substantive reform.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_orthodox_revivalists, excluded,
    moderate, generational, constrained, national).

narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified ritual field where practitioners can address life-cycle needs without navigating inter-religious boundaries; the bundle coordinates practical religious life across traditions that would otherwise require separate institutional loyalties.
% TRANSFER_FUNCTION: Moves ontological ambiguity from institutions to practitioners and reformers: institutions avoid the cost of theological clarification while practitioners absorb contradictory commitments, and reformers expend political capital on failed separation attempts.
% ABSENT_VOICES: Shinto orthodox revivalists and Buddhist purists who would demand clean ontological separation are structurally marginalized; their exclusion is maintained by the institutional inertia of the syncretic complex, which can temporarily accommodate their critiques theatrically without substantive reform.
% DISAPPEARANCE_RATIONALE: If the incoherent bundle vanished and practitioners were forced to choose between coherent Shinto and coherent Buddhism, religious practice would reorganize around distinct institutional loyalties, patronage networks would split, and ritual specialists would face identity dissolution. The practical coordination function would be lost even as ontological clarity increased.
% FOUNDING_PROBLEM: How to integrate two distinct religious systems with incompatible ontologies into a single field of practice without forcing practitioners to choose exclusive loyalty.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of Japanese religion attest that the integration problem was structurally resolved by the early modern period. Contemporary practitioners interviewed about their theological commitments often reveal syncretic incoherence, corroborating that the original coordination problem is no longer the active reason for the arrangement's persistence.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.74) because the bundle's persistence depends overwhelmingly on ritual performativity masking theoretical incoherence; the rituals 'work' regardless of their contradictory ontological premises. Extractiveness is moderate (0.52) because the costs are diffuse (cognitive load of incoherence, blocked reform) rather than concentrated extraction. Suppression is moderate (0.55): separation attempts fail not through extreme coercion but through the accumulated weight of institutional intertwinement. Resistance is moderately high (0.62) because major separation attempts (Meiji shinbutsu bunri) represent significant historical resistance. Accessibility_collapse is moderate (0.50) because pure alternatives exist in theory but are practically inaccessible due to embedded practice. The measurement series share a single time grid to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (religious institutions), the bundle appears as inherited routine practice that would be prohibitively expensive to rationalize; from the payer seats (practitioners, ritual specialists, reformers), it appears as a block on ontological clarity and institutional reform. The engine computes this divergence from the structural asymmetry in exit options: institutions are constrained by structural entanglement, while ritual specialists are identity_locked into the bundle's contradictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners are declared beneficiaries because they receive the practical coordination of life-cycle ritual; they are simultaneously payers because they absorb the cognitive costs of contradictory commitments. Religious institutions are agenda_setters rather than beneficiaries to respect the piton structure: they administer the bundle out of inertia, not concentrated rent extraction. Theological reformers and ritual specialists are victims because they bear the asymmetric costs of the bundle's resistance to clarification. Ritual specialists' identity_locked exit amplifies their effective extraction because their professional self-concept is fused with the syncretic bundle.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as piton rather than tangled_rope prevents misidentifying the bundle's persistence as driven by active coordination or concentrated extraction. The founding problem (integrating two religious systems) is dead: the solution persists not because the problem is live but because disentangling the solution is prohibitively expensive. The dead founding_problem_status paired with world_rearranges disappearance_verdict flags the constraint as a zombie/piton: the world would rearrange if it vanished, but its mandate has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_inertia_vs_active_defense,
    'Is the bundle''s persistence driven by passive institutional inertia (no party profits enough to change it) or by active institutional defense (concentrated beneficiaries maintaining the arrangement)?',
    'Historical analysis of resource mobilization against the Meiji shinbutsu bunri: if temples and shrines actively diverted resources to resist separation, active defense; if non-compliance arose from routine and structural entanglement without coordinated mobilization, inertia.',
    'If active defense, reclassify toward tangled_rope or snare; if pure inertia, confirms piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_active_defense, empirical, 'Whether persistence is inertial or actively defended.').

omega_variable(
    ritual_efficacy_ontological_status,
    'Does the practical efficacy of shinbutsu-shugo rituals derive from a genuinely integrated (but complex) ontology, or from the bracketing of ontological questions in favor of performative success?',
    'Ethnographic study of practitioner beliefs and ritual outcomes; if efficacy persists even when practitioners are informed of ontological contradictions, the efficacy is performative rather than ontologically grounded.',
    'If performative, supports high theater_ratio and piton classification; if ontologically grounded despite apparent contradictions, the bundle may be a rope rather than a degraded piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_ontological_status, conceptual, 'Whether ritual efficacy is performative or ontologically grounded.').

omega_variable(
    committer_reading_boundary,
    'This constraint is the incoherent_bundle reading of kami_buddha_ontology; would classifying the same historical practices under the honji_suijaku_monism or domain_partition readings produce a different constraint type?',
    'Compare computed types of sibling readings once authored; divergence confirms the kernel decomposition was necessary.',
    'Validates the kernel decomposition; if all readings compute to the same type, the decomposition is unnecessary and the epsilon-invariance principle would require collapsing them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Committee omega documenting reading-relativity of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__incoherent_bundle, theater_ratio, 30, 0.55).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__incoherent_bundle, theater_ratio, 60, 0.62).
narrative_ontology:measurement(kami_tr_t90, kami_buddha_ontology__incoherent_bundle, theater_ratio, 90, 0.68).
narrative_ontology:measurement(kami_tr_t120, kami_buddha_ontology__incoherent_bundle, theater_ratio, 120, 0.71).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__incoherent_bundle, theater_ratio, 150, 0.74).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(kami_be_t90, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 90, 0.46).
narrative_ontology:measurement(kami_be_t120, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 120, 0.49).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 150, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(kami_su_t60, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(kami_su_t90, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 90, 0.48).
narrative_ontology:measurement(kami_su_t120, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 120, 0.42).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 150, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the natural-language concept 'shinbutsu-shugo' into three structurally distinct claims: coherent monistic identity (honji_suijaku), clean domain separation (domain_partition), and incoherent practical bundle (this reading). Their epsilon values differ because they describe different constraints with different stakeholder structures, extraction profiles, and persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
