% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)
 *   domain: religious_studies/history/philosophy
 *
 * SUMMARY:
 *   This constraint story represents the 'incoherent bundle' reading of
 *   Shinbutsu-shugo, arguing that the historical coexistence of Kami and
 *   Buddhist deities in Japan was never a coherent theological or ontological
 *   fusion, but rather a pragmatic institutional arrangement maintained
 *   through deliberate ambiguity and state power. The Meiji government's
 *   forced separation (Shinbutsu Bunri) is seen not as creating a new
 *   division, but as revealing the underlying incoherence that the system had
 *   previously suppressed. This reading emphasizes the extractive and
 *   suppressive aspects of the system, particularly for those seeking
 *   theological clarity or alternative religious expressions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.65).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/history/philosophy").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'b161f703-bb02-4275-8e1d-b71491534d1a').
narrative_ontology:cs_kernel_codification('b161f703-bb02-4275-8e1d-b71491534d1a', distributed).
narrative_ontology:cs_authority_grounding('b161f703-bb02-4275-8e1d-b71491534d1a', extraction).
narrative_ontology:cs_interpretation_layer_present('b161f703-bb02-4275-8e1d-b71491534d1a').
narrative_ontology:cs_reading_relation('b161f703-bb02-4275-8e1d-b71491534d1a', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('b161f703-bb02-4275-8e1d-b71491534d1a', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('b161f703-bb02-4275-8e1d-b71491534d1a', foundational, ontological_ambiguity_as_structural_feature).
narrative_ontology:cs_axiom_status(ontological_ambiguity_as_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('b161f703-bb02-4275-8e1d-b71491534d1a', ontological_ambiguity_as_structural_feature, conventional).
narrative_ontology:cs_axiom('b161f703-bb02-4275-8e1d-b71491534d1a', foundational, institutional_power_sustains_incoherence).
narrative_ontology:cs_axiom_status(institutional_power_sustains_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('b161f703-bb02-4275-8e1d-b71491534d1a', institutional_power_sustains_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('b161f703-bb02-4275-8e1d-b71491534d1a', pre_meiji_institutional_ambiguity).
narrative_ontology:cs_drift_state('b161f703-bb02-4275-8e1d-b71491534d1a', meiji_restoration_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('b161f703-bb02-4275-8e1d-b71491534d1a', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authorities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, intellectual_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the combined religious sites, collected offerings, and maintained the ambiguous theological framework that allowed for the coexistence of Kami and Buddhist deities. Benefited from the institutional stability and resource flow of the combined system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).

% Coexisted with Buddhist temples, often sharing precincts and administration. Benefited from the institutional legitimacy and resource flow, despite the ontological ambiguity of their deities' relationship to Buddhist figures.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines, agenda_setter,
    institutional, generational, constrained, national).

% Used the ambiguous shinbutsu-shugo system to control religious institutions and populations, preventing the emergence of unified, ideologically coherent religious movements that could challenge state power. Benefited from the diffuse, unchallengeable nature of the religious framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Participated in local religious practices that blended Shinto and Buddhist elements, often without a clear understanding of the underlying theological inconsistencies. Paid offerings and labor to maintain the combined sites, bearing the costs of an arrangement they did not define.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Challenged the theological incoherence and institutional power of shinbutsu-shugo, advocating for a clear separation of Kami and Buddhist traditions. Faced suppression and marginalization for questioning the established order.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, intellectual_reformers, payer,
    moderate, biographical, constrained, national).

% Observed the shinbutsu-shugo system as a tool for political control and a source of institutional power. Its later policies (Shinbutsu Bunri) revealed the underlying incoherence by forcing a separation that the system could not withstand, rather than creating a new division.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed for the institutional coexistence and shared administration of Shinto shrines and Buddhist temples, providing a unified religious landscape for local communities and a mechanism for state control over religious practice.
% TRANSFER_FUNCTION: Transferred offerings, labor, and institutional legitimacy from local communities to the combined religious institutions and, indirectly, to the bakufu authorities, in exchange for religious services and social order.
% ABSENT_VOICES: Theological purists and early Shinto nationalists, who would have argued for a clear, ontologically consistent separation of Kami and Buddhist traditions, were marginalized or suppressed by the dominant ambiguous framework.
% DISAPPEARANCE_RATIONALE: If the deliberate ambiguity and institutional power maintaining shinbutsu-shugo had vanished, the religious landscape would have immediately fragmented into distinct Shinto and Buddhist institutions, forcing a clarification of theological positions and institutional allegiances that the system was designed to avoid. The Meiji Shinbutsu Bunri demonstrated this rearrangement.
% FOUNDING_PROBLEM: The need to integrate newly introduced Buddhism with indigenous Kami worship without creating irreconcilable theological conflicts or institutional rivalries, while also providing a framework for state control over religious life.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and the rapid collapse of the combined system under Meiji pressure corroborate that the 'problem' of managing theological incoherence was solved by institutional power and ambiguity, not genuine synthesis. The Meiji government's actions revealed the underlying incoherence, rather than creating it, indicating the founding problem was never truly resolved but merely managed through a fragile arrangement.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the institutional power wielded by temples and shrines, backed by the bakufu, which extracted resources and conformity from local communities without providing a coherent theological framework. Suppression (0.7) was high, as any attempts to clarify or challenge the ambiguous nature of shinbutsu-shugo were met with institutional resistance. The theater ratio (0.4) reflects the performative maintenance of a 'unified' religious system despite its internal contradictions, with a significant portion of institutional activity dedicated to preserving this ambiguity. Accessibility collapse (0.4) was moderate, as alternative interpretations or practices were difficult but not impossible to pursue, while resistance (0.75) was substantial from intellectual reformers and later, the Meiji state.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional beneficiaries (temples, shrines, bakufu), shinbutsu-shugo was a functional, if complex, system for managing religious life and maintaining social order. From the perspective of local communities and reformers, it was an opaque, extractive, and suppressive arrangement that lacked genuine theological grounding. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temples and Shinto shrines, as well as the bakufu authorities, were primary beneficiaries and agenda-setters, profiting from the institutional stability and control afforded by the ambiguous system. Local communities and intellectual reformers were the payers, bearing the costs of theological incoherence, resource extraction, and suppression of alternative views. The Meiji government, in this reading, acts as an observer whose policies ultimately expose the constraint's true nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_coherence_ambiguity,
    'Was there any underlying ontological coherence to Shinbutsu-shugo that was simply not articulated, or was it fundamentally an incoherent institutional construct?',
    'Discovery of previously unknown theological texts or archaeological evidence demonstrating a consistent, widespread philosophical synthesis, or further historical analysis confirming the dominance of pragmatic institutional arrangements over theological consistency.',
    'If coherence is found, the constraint might reclassify towards a more ''rope-like'' or ''tangled_rope'' type with a stronger coordination function, as the ambiguity would be a feature of a deeper synthesis rather than a cover for incoherence. If incoherence is confirmed, the ''snare'' or ''tangled_rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_coherence_ambiguity, conceptual, 'Ambiguity regarding the true ontological status of Shinbutsu-shugo.').

omega_variable(
    meiji_bunri_causality,
    'Did the Meiji Shinbutsu Bunri (separation of Kami and Buddhas) create the incoherence, or merely reveal a pre-existing, managed incoherence?',
    'Detailed historical analysis of pre-Meiji theological debates and institutional structures, focusing on internal pressures for separation or clarification prior to state intervention.',
    'If the Bunri created the incoherence, the ''incoherent bundle'' reading is weakened, suggesting the system had a functional coherence that was forcibly disrupted. If it revealed pre-existing incoherence, this reading is strongly corroborated, emphasizing the fragility and institutional nature of the prior arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_bunri_causality, empirical, 'Whether Meiji separation was a cause or a revelation of incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(shin_be_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(shin_be_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(shin_be_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(shin_be_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(shin_su_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(shin_su_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(shin_su_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(shin_su_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel, focusing on its nature as an incoherent bundle maintained by institutional power and ambiguity. It contrasts with readings that emphasize syncretic fusion or domain partition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
