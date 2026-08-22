% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Ontological Substrate: Incoherent Bundle Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   In pre-modern Japan, the state enforced the fusion of kami worship and
 *   Buddhism (shinbutsu shugo) through registration, ranking, and ritual
 *   integration. This reading (incoherent_bundle) treats the resulting
 *   syncretism not as a coherent theological kernelâwhether ontologically
 *   unified (syncretic_fusion) or functionally partitioned
 *   (domain_partition)âbut as an accumulation of institutional drift held
 *   together by state enforcement. The state apparatus benefits from the
 *   appearance of a unified religious field that generates legitimacy and
 *   compliance; practitioners and clergy bear the costs of contradictory
 *   beliefs and practices that lack theological resolution. The constraint is
 *   authored as a snare: the coordination story (harmonious religious unity)
 *   is cover for state extraction of religious legitimacy and administrative
 *   control.
 *
 * KEY AGENTS:
 *   - State religious administrators (institutional/constrained): agenda-setters who enforce fusion and collect legitimacy
 *   - Lay practitioners (powerless/constrained): targets who navigate incoherent ritual obligations
 *   - Shrine-temple clergy (moderate/identity_locked): targets whose professional identity is fused to the syncretic system
 *   - Purist religious movements (powerless/trapped): excluded voices suppressed by the enforced fusion
 *   - Scholars of religion (analytical/analytical): observers who document the structural incoherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.82).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Ontological Substrate: Incoherent Bundle Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'f46f2952-b0a8-493c-8cc3-be7dd9486054').
narrative_ontology:cs_kernel_codification('f46f2952-b0a8-493c-8cc3-be7dd9486054', implicit).
narrative_ontology:cs_authority_grounding('f46f2952-b0a8-493c-8cc3-be7dd9486054', extraction).
narrative_ontology:cs_interpretation_layer_present('f46f2952-b0a8-493c-8cc3-be7dd9486054').
narrative_ontology:cs_reading_relation('f46f2952-b0a8-493c-8cc3-be7dd9486054', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('f46f2952-b0a8-493c-8cc3-be7dd9486054', shinbutsu_ontological_substrate__domain_partition_reading, influences).
narrative_ontology:cs_axiom('f46f2952-b0a8-493c-8cc3-be7dd9486054', foundational, no_coherent_ontological_substrate).
narrative_ontology:cs_axiom_status(no_coherent_ontological_substrate, holdable).
narrative_ontology:cs_axiom_grounding('f46f2952-b0a8-493c-8cc3-be7dd9486054', no_coherent_ontological_substrate, empirically_contingent).
narrative_ontology:cs_axiom('f46f2952-b0a8-493c-8cc3-be7dd9486054', secondary, honji_suijaku_as_post_hoc_rationalization).
narrative_ontology:cs_axiom_status(honji_suijaku_as_post_hoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('f46f2952-b0a8-493c-8cc3-be7dd9486054', honji_suijaku_as_post_hoc_rationalization, empirically_contingent).
narrative_ontology:cs_reference_frame('f46f2952-b0a8-493c-8cc3-be7dd9486054', state_enforced_syncretism).
narrative_ontology:cs_drift_state('f46f2952-b0a8-493c-8cc3-be7dd9486054', edo_period_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f46f2952-b0a8-493c-8cc3-be7dd9486054', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_religious_administrators).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_clergy).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_religious_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imperial and shogunal offices that administer the registration, ranking, and ritual integration of shrines and temples. They enforce the fusion of kami and buddha cults under a single bureaucratic order, collecting political legitimacy and administrative compliance from religious institutions. Their authority depends on maintaining the appearance of a unified religious field.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_religious_administrators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_religious_administrators, beneficiary).

% Ordinary worshippers who participate in rituals at local shrines and temples where kami and buddhas are venerated together without theological clarification. They bear the cognitive and ritual costs of a system that presents unified worship but offers no coherent soteriological or cosmological framework, making navigation of religious obligations dependent on custom rather than doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, constrained, local).

% Priests, monks, and shrine attendants whose institutional identities and livelihoods are bound to the syncretic system. They perform rituals that fuse traditions their own scriptural canons might separate, trapped between state requirements and theological coherence. Professional identity is constituted through the maintenance of the state's syncretic arrangement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_clergy, payer,
    moderate, biographical, identity_locked, regional).

% Buddhist reform movements and Shinto restorationists who insist on doctrinal purity and institutional separation. They are structurally marginalized by the state's enforcement of fused practice and excluded from the official religious field.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, purist_religious_movements, excluded,
    powerless, biographical, trapped, national).

% Academic historians and religious studies scholars who analyze the structural incoherence of shinbutsu syncretism from outside the system. They document the gap between state rhetoric and theological reality, with some adopting this skeptical reading and others defending functional or ontological coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, scholars_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_religious_administrators).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed coordination: integrating shrine and temple practice under a unified imperial-religious order to prevent sectarian competition and ensure administrative clarity. Under this reading, no genuine coordination problem is solved; the enforced fusion serves state extraction of legitimacy and compliance, while the underlying traditions remain structurally unintegrated.
% TRANSFER_FUNCTION: Moves religious legitimacy, institutional compliance, and administrative control from shrines, temples, and lay practitioners to the state apparatus, under the cover of a unified cultic order.
% ABSENT_VOICES: Purist Buddhist reformers, Shinto restorationists, and sectarian movements who would demand theological clarity and institutional separation; they are suppressed or marginalized by the state's enforcement of fused practice.
% DISAPPEARANCE_RATIONALE: If the enforced fusion vanished overnight, shrines and temples would reorganize along distinct sectarian lines, state claims to sacred mediation would collapse, and practitioners would be forced to navigate clarified theological boundaries rather than an incoherent bundle.
% FOUNDING_PROBLEM: How to consolidate Buddhist and kami-worship institutions into a politically manageable, legitimacy-generating religious field under imperial and later shogunal state authority.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians outside the state beneficiary set (e.g., Kuroda Toshio, Mark Teeuwen, Fabio Rambelli) attest that the arrangement served state administrative and ideological goals rather than solving a genuine theological or popular coordination problem; no corroboration exists from independent religious actors that unified worship was necessary to prevent collective dysfunction.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the state gains substantial legitimacy and administrative control without offering genuine theological coherence in return. Suppression is higher (0.82) because the constraint depends on active state enforcementâregistration systems, legal compulsion, and the marginalization of purist movementsâto prevent separation. Theater ratio is moderately high (0.55): the system maintains elaborate rituals of unified worship that perform ontological coherence while masking underlying contradictions. Accessibility collapse is substantial (0.70) because alternatives (pure Shinto, sectarian Buddhism) are rendered invisible or illegitimate by the state's unified field. Resistance is moderate-low (0.35) because state enforcement is effective, though marginal purist movements persist.
 *
 * PERSPECTIVAL GAP:
 *   From the state administrative seat, the arrangement is a successful coordination of national religious practice that prevents sectarian conflict and ensures imperial legitimacy. From the practitioner and clergy seats, the same arrangement is an enforced incoherence that extracts compliance and obscures theological clarity. The engine computes this divergence from the structural data: agenda_setter with constrained exit but beneficiary role vs. payers with identity_locked or constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   State religious administrators are structural beneficiaries (low d): the constraint subsidizes their authority and legitimacy. Lay practitioners and shrine-temple clergy are structural targets (high d): the constraint extracts compliance, cognitive labor, and professional identity from them. The clergy's identity_locked exit amplifies their effective extraction relative to their moderate power. Scholars occupy an analytical seat with analytical exit, near-zero extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâconsolidating religious institutions under state authorityâwas solved by the Heian period and dead by the Edo period. The constraint persisted for centuries as inertial extraction: the state continued to enforce fusion because it continued to harvest legitimacy, not because unification remained a live coordination need. The Meiji Restoration's shinbutsu bunri (1868) abolished the fusion, confirming the mandatrophy: the state could separate the traditions cheaply once a new political model required distinct religious categories, proving that the enforced unity had outlived its administrative function and was being maintained for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    no_coherent_kernel_or_historiographical_projection,
    'Is the incoherence of shinbutsu syncretism a genuinely irreducible historical condition, or a modern historiographical projection that itself serves post-Meiji secular state interests?',
    'Comparative analysis of pre-modern practitioner texts and ritual manuals to determine whether actors experienced cognitive dissonance or navigated multiple frameworks fluently.',
    'If practitioners experienced no dissonance, the victim structure of this reading is overstated and the constraint may reclassify toward tangled_rope or rope; if dissonance is documented, the snare classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_coherent_kernel_or_historiographical_projection, conceptual, 'Whether incoherence is intrinsic or projected').

omega_variable(
    top_down_extraction_vs_bottom_up_syncretism,
    'Does the state apparatus actively extract religious legitimacy through enforced fusion, or does it merely administer an organic popular syncretism that predates and exceeds state formation?',
    'Archaeological and textual evidence of popular religious practice in the proto-historic and early historic periods, compared against the timing and content of state registration and ranking systems.',
    'If syncretism is primarily bottom-up, the state is a coordinator rather than an extractor and the constraint reclassifies toward rope; if top-down imposition is primary, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(top_down_extraction_vs_bottom_up_syncretism, empirical, 'State imposition versus organic popular practice').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement, registration systems, legal penalties) or internalized (clergy and practitioners naturalizing contradictory beliefs as normal or even piously desirable)?',
    'Post-Meiji exit trajectory: if theological clarification accelerated rapidly after the state''s enforcement apparatus was removed (shinbutsu bunri), suppression was primarily structural; if incoherent practice persisted despite legal separation, suppression was partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s extractive depth is greater than surface enforcement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(shin_tr_t70, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 70, 0.55).
narrative_ontology:measurement(shin_tr_t85, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 85, 0.58).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(shin_be_t70, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 70, 0.72).
narrative_ontology:measurement(shin_be_t85, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 85, 0.76).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shin_su_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(shin_su_t70, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 70, 0.8).
narrative_ontology:measurement(shin_su_t85, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 85, 0.83).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the shinbutsu_ontological_substrate kernel family. The epsilon-invariance principle requires separate stories for the ontological-unification reading, functional-partition reading, and incoherent-bundle reading because they assign different structural properties, beneficiary/victim structures, and epsilon values to the same historical label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
