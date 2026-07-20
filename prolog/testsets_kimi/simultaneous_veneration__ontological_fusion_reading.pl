% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Frame
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint story models the ontological_fusion_reading of the
 *   simultaneous_veneration kernel in medieval Japanese religion. Under this
 *   reading, honji-suijaku theory is taken as metaphysically true: kami and
 *   buddhas are ontologically identical beings viewed through different
 *   cultural lenses. The constraint enforces this unity through Buddhist
 *   institutional hierarchy's interpretive monopoly, subordinating indigenous
 *   kami cults to a universal Buddhist cosmological schema. The story treats
 *   the arrangement as a tangled rope â it delivered genuine coordination
 *   by enabling unified worship across traditions, while asymmetrically
 *   extracting doctrinal autonomy from local kami cults. This is one of three
 *   readings; the siblings are domain_partition_reading and
 *   pragmatic_incoherence_reading.
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: Agenda-setter and primary beneficiary (institutional/generational/mobile) â enforces ontological fusion and collects interpretive monopoly rents
 *   - indigenous_kami_cults: Primary payer (moderate/biographical/identity_locked) â lose autonomous theological standing under Buddhist hermeneutic supremacy
 *   - syncretic_lay_communities: Secondary beneficiary (moderate/biographical/constrained) â receive coordination benefit of unified ritual access
 *   - comparative_religionists: Analytical observer (analytical/biographical/analytical) â evaluates the structural operation of the fusion frame from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.72).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Frame").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '6ca01d83-2b40-48a0-8426-2d1fea22359d').
narrative_ontology:cs_kernel_codification('6ca01d83-2b40-48a0-8426-2d1fea22359d', formalized).
narrative_ontology:cs_authority_grounding('6ca01d83-2b40-48a0-8426-2d1fea22359d', extraction).
narrative_ontology:cs_interpretation_layer_present('6ca01d83-2b40-48a0-8426-2d1fea22359d').
narrative_ontology:cs_reading_relation('6ca01d83-2b40-48a0-8426-2d1fea22359d', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('6ca01d83-2b40-48a0-8426-2d1fea22359d', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('6ca01d83-2b40-48a0-8426-2d1fea22359d', foundational, ontological_identity_kami_buddha).
narrative_ontology:cs_axiom_status(ontological_identity_kami_buddha, holdable).
narrative_ontology:cs_axiom_grounding('6ca01d83-2b40-48a0-8426-2d1fea22359d', ontological_identity_kami_buddha, theological).
narrative_ontology:cs_axiom('6ca01d83-2b40-48a0-8426-2d1fea22359d', foundational, buddhist_hermeneutic_supremacy).
narrative_ontology:cs_axiom_status(buddhist_hermeneutic_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('6ca01d83-2b40-48a0-8426-2d1fea22359d', buddhist_hermeneutic_supremacy, conventional).
narrative_ontology:cs_reference_frame('6ca01d83-2b40-48a0-8426-2d1fea22359d', universal_buddhist_cosmology).
narrative_ontology:cs_drift_state('6ca01d83-2b40-48a0-8426-2d1fea22359d', meiji_separation_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('6ca01d83-2b40-48a0-8426-2d1fea22359d', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, syncretic_lay_communities).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers honji-suijaku theology and claims interpretive monopoly over local deities, reclassifying them as traces or manifestations of buddhas and bodhisattvas. Collects land, patronage, and ritual jurisdiction through shrine-temple multiplexes. Could reform the doctrinal frame but has institutional incentives to maintain the fusion hierarchy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Maintain shrines and rites for local kami. Under the ontological fusion frame, their deities are absorbed into Buddhist cosmology as provisional manifestations, stripping the cults of independent doctrinal authority. Their ritual identity is fused with place and ancestry, making exit from the interpretive frame equivalent to abandoning their social and spiritual role.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults, payer,
    moderate, biographical, identity_locked, local).

% Practice a blended worship that honors both buddhas and kami within a single ritual economy. Receive coordination benefit from unified worship calendars, shared pilgrimage networks, and simplified cosmological order. Their choices are bounded by the available temple-shrine infrastructure; fully independent kami-only worship is structurally marginalized.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, syncretic_lay_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Analyze the honji-suijaku system from outside the theological frame, comparing it with other cases of religious syncretism and domination. They do not participate in the ritual economy but document how ontological claims map onto institutional power asymmetries.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religionists, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates a universalist soteriological religion (Buddhism) with indigenous, place-bound deity worship (kami cults) into a single ritual and cosmological economy, preventing sectarian fragmentation and enabling cross-tradition patronage.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual patronage, and doctrinal legitimacy from local kami priesthoods to Buddhist monastic institutions, reclassifying indigenous deities as provisional manifestations of Buddhist cosmic principles.
% ABSENT_VOICES: Hard-line kami exclusivists and doctrinal purists who rejected Buddhist ontological supremacy were structurally excluded from theological discourse; their positions survive only in folk counter-practice and post-Meiji Shinto restorationist texts.
% DISAPPEARANCE_RATIONALE: Without the enforced ontological fusion, kami cults would reclaim independent doctrinal and institutional status, Buddhist monastic claims to universal interpretive authority would fragment, and the Japanese religious landscape would reorganize around competing centers rather than a single hierarchy.
% FOUNDING_PROBLEM: How to integrate a foreign, universalist salvific religion with indigenous place-bound deity worship without destabilizing the religious field or the political order that depended on ritual legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians of Japanese religion attest the integration problem was historically real. They also attest that the honji-suijaku solution outlived its political utility and was forcibly dismantled by the Meiji state, corroborating that the arrangement persisted beyond the problem it was built to solve.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically transfers interpretive authority from local kami cults to Buddhist institutions; the ontological identity claim is not reciprocally empowering but hierarchically absorbing. Suppression (0.72) reflects the active enforcement required to maintain that kami are provisional traces rather than independent deities â alternatives (autonomous Shinto ontology) were structurally marginalized. Theater_ratio (0.45) indicates that by the late medieval and early modern periods, a substantial portion of honji-suijaku discourse had become performative maintenance of institutional subordination rather than living theological argument. Accessibility_collapse (0.68) captures how thoroughly the Buddhist interpretive frame saturated the ritual landscape, making independent kami ontology difficult to articulate within official discourse; resistance (0.55) registers persistent folk practice and the eventual Meiji separation as long-run pushback.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist institutional seat experiences the constraint as legitimate cosmic order and necessary religious integration; the indigenous kami cult seat experiences it as doctrinal absorption and loss of autonomous standing. The syncretic lay seat sits nearer symmetric, receiving genuine coordination benefit (unified worship) while indirectly reinforcing the extractive hierarchy. The engine computes these divergences from the structural data â the agenda-setter's power and mobile exit versus the payer's identity-locked confinement.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy is declared as beneficiary and agenda-setter, deriving low directionality (subsidy side). Indigenous kami cults are declared victims/payers, deriving high directionality (target side). Syncretic lay communities are beneficiaries of coordination, deriving moderately low directionality. Comparative religionists are analytical observers with no stake in the flow. The engine will amplify effective extraction for the kami cults and damp it for the Buddhist hierarchy accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling: pure rope would ignore the asymmetric extraction of interpretive autonomy from kami cults; pure snare would ignore the genuine coordination function the frame served in enabling cross-tradition worship and preventing sectarian conflict. The founding problem â integrating foreign Buddhism with indigenous kami practice â was a real coordination problem. However, the specific solution ossified into an extractive monopoly. The R5 genealogy records the problem as dead (solved by Meiji separation), while the disappearance verdict is world_rearranges, flagging the persistent structural dependency the arrangement created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_suppression_mechanism,
    'Was the subordination of kami achieved primarily through institutional coercion (temple-shrine mergers, state backing) or through theological hegemony that local practitioners internalized as legitimate?',
    'Archaeological and textual evidence of resistance vs. compliance; post-Meiji separation surveys of priestly self-identification.',
    'If internalized, effective extraction and suppression are higher than structural measures suggest; if coerced, the constraint is more fragile and more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_suppression_mechanism, empirical, 'Structural vs internalized suppression in religious subordination').

omega_variable(
    metaphysical_truth_or_institutional_fiction,
    'Does honji-suijaku theory capture an independent metaphysical truth about kami and buddhas, or is it an institutional fiction constructed to legitimize Buddhist interpretive monopoly?',
    'Comparative theological analysis; evidence of independent convergence vs. historical invention tied to patronage patterns.',
    'If metaphysically true, the high extraction metric may measure necessary cost of cosmic order rather than rent; if institutional fiction, the constraint is a snare using theology as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_truth_or_institutional_fiction, conceptual, 'Whether ontological fusion is metaphysical truth or political theology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(simu_tr_t10, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(simu_tr_t25, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(simu_tr_t50, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(simu_tr_t75, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(simu_be_t10, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(simu_be_t25, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(simu_be_t50, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(simu_be_t75, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 75, 0.76).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(simu_su_t10, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(simu_su_t25, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(simu_su_t50, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(simu_su_t75, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
