% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Incoherent Institutional Bundle
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint models Shinbutsu-shugo not as a coherent theological
 *   system, but as an institutionally sustained bundle of contradictory
 *   commitments—simultaneous fusion and separation, hierarchical and
 *   reciprocal, systematized and unsystematized. This reading argues that no
 *   single, consistent ontology underlies the relationship between kami and
 *   buddhas; rather, the bundle persists due to institutional inertia,
 *   practical efficacy, and ritual success, which mask its theoretical
 *   incoherence. Attempts at strict separation or coherent synthesis are
 *   resisted, as the ambiguity serves various institutional and cultural
 *   functions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.75).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Incoherent Institutional Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, 'e8d6d4a9-3113-4d96-9c93-d4eac128baf5').
narrative_ontology:cs_kernel_codification('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', implicit).
narrative_ontology:cs_authority_grounding('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', practice).
narrative_ontology:cs_interpretation_layer_present('e8d6d4a9-3113-4d96-9c93-d4eac128baf5').
narrative_ontology:cs_reading_relation('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', foundational, ontological_ambiguity_is_culturally_functional).
narrative_ontology:cs_axiom_status(ontological_ambiguity_is_culturally_functional, holdable).
narrative_ontology:cs_axiom_grounding('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', ontological_ambiguity_is_culturally_functional, conventional).
narrative_ontology:cs_reference_frame('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', pre_meiji_syncretic_practice).
narrative_ontology:cs_drift_state('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', post_meiji_separation_attempts, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e8d6d4a9-3113-4d96-9c93-d4eac128baf5', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shinto_shrines).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, lay_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, japanese_state_institutions).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theologians_philosophers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, religious_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the dual patronage and cultural embeddedness afforded by the Shinbutsu-shugo bundle. They actively maintain practices that embody the fusion and separation, often without explicit theological reconciliation, ensuring their continued relevance across diverse spiritual needs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_shrines, agenda_setter,
    institutional, generational, constrained, national).

% Similar to Shinto shrines, temples benefit from the broad appeal and institutional stability provided by the Shinbutsu-shugo framework. They perpetuate rituals and narratives that blend kami and buddhas, often prioritizing practical efficacy and tradition over strict ontological consistency.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).

% Bear the intellectual cost of the bundle's inherent contradictions. They attempt to analyze, rationalize, or critique the lack of a single coherent ontology, often finding their efforts resisted by the practical and institutional inertia of the religious landscape.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theologians_philosophers, payer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, theologians_philosophers, observer).

% Seek to clarify or separate the kami and buddha traditions, often advocating for a purer form of Shinto or Buddhism. Their efforts are typically marginalized or actively resisted by established institutions that benefit from the existing, ambiguous bundle.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_reformers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, religious_reformers, excluded).

% Benefit from a rich array of spiritual options for various life events (e.g., Shinto for birth/marriage, Buddhism for death). However, they may experience internal confusion or cognitive dissonance when confronted with the underlying ontological contradictions, which are often masked by ritual success and cultural habit.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, lay_practitioners, payer).

% Historically, the state has played a significant role in shaping and enforcing the relationship between Shinto and Buddhism, notably during the Meiji separation. While not directly involved in daily theological debates, state policies influence the institutional framework that sustains the bundle's contradictions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, japanese_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, diffuse).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse religious practices and beliefs (Shinto and Buddhist) to coexist and intermingle within a single cultural framework, providing spiritual options for various life events and fostering a sense of shared religious identity.
% TRANSFER_FUNCTION: Transfers legitimacy, patronage, and resources to religious institutions by allowing them to serve a broad spectrum of spiritual needs without strict ontological boundaries. It also transfers the burden of theoretical incoherence to scholars and those seeking clarity.
% ABSENT_VOICES: Early Meiji-era separationists who sought a pure Shinto or pure Buddhism, and contemporary individuals seeking clear, non-contradictory theological frameworks. Their attempts at separation were often suppressed or marginalized by institutional inertia and cultural embeddedness.
% DISAPPEARANCE_RATIONALE: If the Shinbutsu-shugo bundle and its institutional sustenance vanished overnight, the religious landscape, institutional structures, and many cultural practices in Japan would be profoundly altered. The deep intermingling of kami and buddhas in ritual, art, and belief means that removing the 'bundle' would necessitate a complete re-evaluation of religious identity and institutional roles.
% FOUNDING_PROBLEM: The initial problem was how to integrate newly introduced Buddhism with indigenous kami worship, and later, how to manage the coexistence of diverse spiritual practices and their institutional expressions within a unified cultural and political entity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion, anthropologists studying contemporary practices, and cultural commentators attest to the ongoing practical integration of these traditions, even while acknowledging the theoretical tensions. The practical problem of managing diverse spiritual practices remains, even if the theoretical problem of ontological coherence is deemed 'dead' by this reading.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` (0.68) is high because the maintenance of this incoherent bundle extracts intellectual and theological clarity from those seeking it, while benefiting institutions through dual patronage and broad cultural appeal. `Suppression` (0.75) is also high, reflecting the strong institutional inertia and cultural embeddedness that resist attempts at reform or clear ontological definition. The `theater_ratio` (0.55) is significant, as ritual success and practical functionality often mask the underlying theoretical contradictions, creating a performative coherence that belies the actual incoherence. The `claimed_type` is Tangled Rope because it genuinely coordinates diverse religious practices and beliefs (a coordination function) but does so through an asymmetric structure that extracts clarity and suppresses alternatives for institutional benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions, the Shinbutsu-shugo bundle is a functional, historically rich tradition that successfully integrates diverse spiritual needs. From the perspective of theologians and reformers, it represents a profound intellectual and theological challenge, a set of unresolved contradictions sustained by power and inertia. The engine's classification will highlight this divergence between the claimed coordination and the measured extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto shrines and Buddhist temples are primary beneficiaries and agenda-setters, as they directly profit from the dual patronage and cultural embeddedness of the bundle. Lay practitioners are also beneficiaries, gaining access to a rich spiritual landscape, but are also payers through the cognitive burden of incoherence. Theologians, philosophers, and religious reformers are primarily payers, bearing the cost of the bundle's contradictions and facing resistance to their efforts for clarity or separation. Japanese state institutions, historically, have also acted as agenda-setters, shaping the institutional context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_efficacy,
    'Is the lack of theoretical coherence in Shinbutsu-shugo a ''bug'' (a problem to be solved) or a ''feature'' (a source of practical efficacy and adaptability) for lay practitioners?',
    'Qualitative sociological studies of practitioner experiences, focusing on how individuals navigate or perceive the contradictions in their spiritual lives.',
    'If it''s primarily a feature, the ''extraction'' from lay practitioners is lower than measured, as they derive genuine benefit from the ambiguity. If a bug, the extraction is higher, reflecting cognitive dissonance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_efficacy, empirical, 'Whether ontological ambiguity is a functional aspect for practitioners.').

omega_variable(
    institutional_vs_theological_priority,
    'To what extent is the persistence of the incoherent bundle driven by institutional self-preservation and resource acquisition, versus a genuine theological or cultural conviction in its value?',
    'Historical analysis of institutional responses to reform movements, financial records of religious institutions, and comparative studies of religious syncretism under different political economies.',
    'If institutional priority is dominant, the constraint''s extractiveness is more clearly a function of rent-seeking. If theological/cultural conviction is primary, the extractiveness is more a byproduct of coordination costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_theological_priority, empirical, 'Drivers of the bundle''s persistence: institutional vs. theological.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative, coherent ontologies primarily structural (institutional power, historical precedent) or internalized (cultural habit, identity fusion among practitioners)?',
    'Analysis of post-Meiji era attempts at separation: if coherent alternatives failed to gain traction even after institutional separation, it suggests stronger internalized suppression. If they flourished, suppression was primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the ''target'' (those seeking coherence) carries the suppression with them. If structural, removing institutional barriers would more readily lead to alternative ontologies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ontological alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.4).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__incoherent_bundle, theater_ratio, 20, 0.45).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__incoherent_bundle, theater_ratio, 40, 0.5).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__incoherent_bundle, theater_ratio, 60, 0.52).
narrative_ontology:measurement(kami_tr_t80, kami_buddha_ontology__incoherent_bundle, theater_ratio, 80, 0.54).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__incoherent_bundle, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(kami_be_t80, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(kami_su_t40, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(kami_su_t60, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(kami_su_t80, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
