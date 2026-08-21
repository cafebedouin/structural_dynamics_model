% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Kami-Buddha Ontological Identity (Honji Suijaku Monism)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'honji suijaku' (original ground and trace
 *   manifestation) doctrine, a central tenet of medieval Japanese syncretism,
 *   which posited that indigenous kami were phenomenal traces (suijaku) of
 *   original Buddhist entities (honji). From the perspective of its
 *   proponents, this doctrine represents an ontological truth about the
 *   nature of reality, providing a coherent framework for the relationship
 *   between Shinto and Buddhism. The claimed type is 'mountain' because it
 *   asserts a fixed, natural-law-like structure within its own theological
 *   framework, even though its historical persistence involved active
 *   intellectual and institutional enforcement. The metrics reflect the
 *   intellectual extraction of ontological independence from kami and the
 *   doctrinal suppression of alternative views.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.65).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.75).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.65).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, mountain).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Kami-Buddha Ontological Identity (Honji Suijaku Monism)").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).
domain_priors:emerges_naturally(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'c8e2181e-e600-4c47-9087-46dc630d2695').
narrative_ontology:cs_kernel_codification('c8e2181e-e600-4c47-9087-46dc630d2695', formalized).
narrative_ontology:cs_authority_grounding('c8e2181e-e600-4c47-9087-46dc630d2695', lineage).
narrative_ontology:cs_interpretation_layer_present('c8e2181e-e600-4c47-9087-46dc630d2695').
narrative_ontology:cs_reading_relation('c8e2181e-e600-4c47-9087-46dc630d2695', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('c8e2181e-e600-4c47-9087-46dc630d2695', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('c8e2181e-e600-4c47-9087-46dc630d2695', foundational, ultimate_reality_is_buddha_nature).
narrative_ontology:cs_axiom_status(ultimate_reality_is_buddha_nature, holdable).
narrative_ontology:cs_axiom_grounding('c8e2181e-e600-4c47-9087-46dc630d2695', ultimate_reality_is_buddha_nature, theological).
narrative_ontology:cs_axiom('c8e2181e-e600-4c47-9087-46dc630d2695', foundational, kami_are_provisional_manifestations).
narrative_ontology:cs_axiom_status(kami_are_provisional_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('c8e2181e-e600-4c47-9087-46dc630d2695', kami_are_provisional_manifestations, theological).
narrative_ontology:cs_reference_frame('c8e2181e-e600-4c47-9087-46dc630d2695', buddhist_ontological_supremacy).
narrative_ontology:cs_drift_state('c8e2181e-e600-4c47-9087-46dc630d2695', meiji_restoration_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c8e2181e-e600-4c47-9087-46dc630d2695', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, syncretic_scholars).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_purists).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kami_autonomy_advocates).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_philosophical_supremacy).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, syncretic_theological_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgated and defended the honji suijaku doctrine, which provided a theological framework for the integration of Shinto deities into Buddhist cosmology, thereby solidifying their own authority and influence over indigenous religious practices. They benefit from the intellectual coherence and expanded devotional base.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Developed and refined the honji suijaku theory, finding intellectual satisfaction and professional standing in its systematization. Their careers and intellectual identity are often tied to the coherence and acceptance of this syncretic framework.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, syncretic_scholars, beneficiary,
    organized, biographical, identity_locked, national).

% Resisted the subsumption of kami under Buddhist entities, advocating for the independent and supreme status of indigenous deities. They bore the cost of intellectual marginalization and, at times, institutional suppression of their views, particularly during periods of strong Buddhist influence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_purists, payer,
    moderate, generational, constrained, national).

% Local practitioners and thinkers who, while perhaps not forming organized 'purist' movements, felt their indigenous traditions and the unique spiritual power of kami were diminished or misrepresented by the monistic Buddhist framework. Their spiritual worldview was constrained by the dominant theological narrative.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_autonomy_advocates, payer,
    powerless, biographical, trapped, local).

% Analyze the historical development and impact of the honji suijaku doctrine, examining its role in Japanese religious and political history without necessarily endorsing its theological claims. They provide an external, critical perspective on its persistence and effects.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, secular_historians, observer,
    analytical, generational, analytical, global).

% Advocates for the view that Kami and Buddhas are distinct entities governing separate domains (e.g., Shinto for life, Buddhism for death). Their perspective is fundamentally incompatible with honji suijaku monism and was often marginalized or suppressed within the dominant syncretic discourse.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, domain_partition_proponents, excluded,
    organized, generational, identity_locked, national).

% Scholars and critics who argue that Shinbutsu-shugo (the fusion of kami and buddhas) was not a coherent system but an institutionally sustained bundle of contradictory commitments. Their view challenges the very premise of systematization that honji suijaku monism provides, and thus they are excluded from its internal logic.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, incoherent_bundle_proponents, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified theological framework that reconciled indigenous kami worship with imported Buddhist doctrines, allowing for a coherent religious identity and practice across diverse traditions within Japan.
% TRANSFER_FUNCTION: Transferred ontological priority and ultimate interpretive authority from indigenous Shinto traditions to Buddhist philosophical frameworks, effectively subsuming kami as manifestations of Buddhist entities.
% ABSENT_VOICES: Proponents of purely Shinto autonomy and those who rejected the possibility or desirability of a coherent syncretic system were structurally excluded from the dominant theological discourse, their views often dismissed as unsophisticated or heterodox.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine vanished overnight, the historical and theological landscape of Japanese religion would be fundamentally altered. The conceptual basis for centuries of syncretic practice would dissolve, forcing a radical re-evaluation of the relationship between Shinto and Buddhism, and potentially leading to a resurgence of purely Shinto or purely Buddhist theological frameworks.
% FOUNDING_PROBLEM: The core problem was the theological and practical reconciliation of indigenous Japanese kami worship with the powerful, philosophically sophisticated, and institutionally organized imported religion of Buddhism, aiming to create a unified religious worldview.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and contemporary academic religious studies corroborate that the problem of reconciling indigenous and imported religious traditions was central to medieval Japanese intellectual life. While Buddhist institutions were primary beneficiaries, the widespread adoption and intellectual defense of the doctrine by various scholarly lineages attest to its perceived problem-solving utility beyond narrow institutional gain.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, ExtMetricName, E),
    domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kami_buddha_ontology__honji_suijaku_monism),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.65) because the doctrine fundamentally redefines the status of kami, subsuming their independent ontological status under Buddhist cosmology. This is an intellectual and spiritual extraction of autonomy. Suppression is high (0.75) due to the active intellectual and institutional efforts to establish and maintain this doctrine as the dominant theological explanation, marginalizing alternative views. Theater ratio is low (0.1) as the doctrine was a serious theological and philosophical endeavor, not primarily performative. Accessibility collapse is high (0.8) because once this monistic view is accepted, the possibility of kami existing as entirely independent entities collapses. Resistance is moderate (0.5) reflecting historical opposition from Shinto purists and later, during the Meiji era, from state-sponsored Shinto.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions and syncretic scholars, this doctrine provided essential theological coherence and a path to spiritual salvation, appearing as a 'mountain' of truth. For Shinto purists and advocates for kami autonomy, it was a 'snare' or 'tangled rope' that extracted their traditions' independence and suppressed alternative interpretations, enforced by the dominant religious establishment. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and syncretic scholars are beneficiaries, gaining intellectual authority, expanded devotional bases, and a coherent worldview. Shinto purists and kami autonomy advocates are victims, as their traditions' independent ontological status is extracted and their views suppressed. Secular historians act as observers, analyzing the doctrine's impact without direct participation in its theological claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'mountain' (from the perspective of its proponents) with high extractiveness and suppression, and declared beneficiaries/victims, prevents mislabeling a constructed theological hierarchy as a benign natural law. The FSM signature will detect this 'false summit,' highlighting that what is claimed as an inherent truth about reality also serves identifiable interests and requires active maintenance against resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_institutional_construct,
    'Is the honji suijaku doctrine a genuine ontological truth about the nature of reality, or a constructed theological framework that primarily served the institutional and intellectual interests of Buddhist establishments?',
    'Comparative religious studies examining similar syncretic processes in other cultures, and further historical analysis of the political and social pressures influencing its development and promulgation.',
    'If primarily a construct, its ''mountain'' claim is a false summit, and its classification would shift towards a ''tangled_rope'' or ''snare'' from an external, critical perspective, highlighting the extraction of ontological autonomy from Shinto traditions. If a genuine truth, its ''mountain'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_institutional_construct, conceptual, 'Ambiguity between a claimed natural law and an institutionally beneficial construct.').

omega_variable(
    degree_of_kami_autonomy_loss,
    'To what extent did the honji suijaku doctrine truly diminish the perceived autonomy and spiritual power of kami for practitioners, versus merely providing a new interpretive lens?',
    'Detailed ethnographic studies of historical and contemporary Shinto practices, analysis of devotional literature, and examination of local cults'' responses to the doctrine.',
    'If the loss of autonomy was profound for practitioners, the extractiveness and suppression metrics are accurate or even understated. If it was largely an intellectual framework with little impact on local practice, the effective extraction from ''kami_autonomy_advocates'' would be lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degree_of_kami_autonomy_loss, empirical, 'The actual impact of the doctrine on the spiritual experience and perceived autonomy of kami.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative views structural (institutional power, doctrinal enforcement) or internalized (cognitive patterns of acceptance within the syncretic framework)?',
    'Analysis of post-Meiji Restoration religious landscape: if alternative Shinto views rapidly re-emerged and gained traction after institutional separation, suppression was primarily structural. If syncretic patterns persisted despite institutional changes, internalized suppression played a larger role.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as adherence persisted even after external enforcement mechanisms weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 1000, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(kami_tr_t1150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1150, 0.1).
narrative_ontology:measurement(kami_tr_t1300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(kami_tr_t1450, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(kami_tr_t1600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(kami_tr_t1750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(kami_tr_t1870, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1870, 0.1).

% Extraction over time
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(kami_be_t1150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1150, 0.6).
narrative_ontology:measurement(kami_be_t1300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1300, 0.65).
narrative_ontology:measurement(kami_be_t1450, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1450, 0.68).
narrative_ontology:measurement(kami_be_t1600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(kami_be_t1750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(kami_be_t1870, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1870, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t1000, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(kami_su_t1150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1150, 0.65).
narrative_ontology:measurement(kami_su_t1300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1300, 0.7).
narrative_ontology:measurement(kami_su_t1450, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1450, 0.75).
narrative_ontology:measurement(kami_su_t1600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1600, 0.78).
narrative_ontology:measurement(kami_su_t1750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1750, 0.77).
narrative_ontology:measurement(kami_su_t1870, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1870, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, meiji_shinbutsu_bunri_edict).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kami_buddha_ontology' kernel, focusing on the honji suijaku monistic interpretation. It is linked to sibling readings that offer alternative interpretations of the kami-buddha relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
