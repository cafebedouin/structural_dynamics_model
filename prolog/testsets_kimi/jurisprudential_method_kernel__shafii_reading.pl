% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Four-Tier Jurisprudential Method with Hadith Authentication Arbiter
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the Shafi'i reading of the
 *   jurisprudential method kernel in classical Islamic law. Al-Shafi'i (d.
 *   204 AH / 820 CE) proposed a strict four-tier hierarchy of legal
 *   sourcesâQur'an, Hadith, Ijma, Qiyasâmaking the authentication of
 *   prophetic reports the methodological arbiter that resolves
 *   inconsistencies among earlier regional schools. The reading presents
 *   itself as pure coordination (eliminating arbitrariness by standardizing
 *   derivation), but structurally it concentrates epistemic authority in the
 *   hadith scholarly class while demoting independent customary and
 *   analogical sources. The expected structural delta is medium-high
 *   extractiveness: hadith scholars are the concentrated beneficiaries, while
 *   adherents of local custom and extensive analogy bear the costs of
 *   delegitimation.
 *
 * KEY AGENTS:
 *   - hadith_scholars: Primary agenda-setter and beneficiary (institutional/arbitrage) â control authentication and collect prestige.
 *   - local_customary_jurists: Primary payer (organized/constrained) â lose independent authority of regional custom.
 *   - independent_analogists: Primary payer (organized/constrained) â lose scope for rational extension and juristic preference.
 *   - rationalist_theologians: Excluded voice (organized/constrained) â structurally absent from the hierarchy's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.7).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Four-Tier Jurisprudential Method with Hadith Authentication Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, 'f8c8e378-c772-4a3f-a10d-3d35e45b5d0f').
narrative_ontology:cs_kernel_codification('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', formalized).
narrative_ontology:cs_authority_grounding('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', lineage).
narrative_ontology:cs_interpretation_layer_present('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f').
narrative_ontology:cs_reading_relation('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', jurisprudential_method_kernel__hanafi_reading, influences).
narrative_ontology:cs_reading_relation('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', foundational, transmission_arbiter_principle).
narrative_ontology:cs_axiom_status(transmission_arbiter_principle, holdable).
narrative_ontology:cs_axiom_grounding('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', transmission_arbiter_principle, conventional).
narrative_ontology:cs_axiom('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', foundational, analogy_subordinate_to_textual_sources).
narrative_ontology:cs_axiom_status(analogy_subordinate_to_textual_sources, holdable).
narrative_ontology:cs_axiom_grounding('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', analogy_subordinate_to_textual_sources, conventional).
narrative_ontology:cs_reference_frame('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', transmission_based_legal_hierarchy).
narrative_ontology:cs_drift_state('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', post_classical_madhhab_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8c8e378-c772-4a3f-a10d-3d35e45b5d0f', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, local_customary_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, independent_analogists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication of prophetic reports through isnad criticism and transmitter evaluation. Their certification determines whether a practice counts as binding Sunna. They occupy endowed teaching positions, judge the authenticity of narratives that control legal outcomes, and benefit from the epistemic centrality of their specialty.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary).

% Derive rulings from the lived customary practice of their region, above all the Medinan tradition. Under the four-tier hierarchy, their source is demoted to a subsidiary status; they must subordinate local consensus to authenticated hadith, losing independent methodological authority and institutional standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, local_customary_jurists, payer,
    organized, generational, constrained, regional).

% Extend divine intent to novel cases through extensive analogical reasoning and juristic preference. The strict hierarchy subordinates qiyas to hadith and consensus, curtailing their scope for independent rational extension and delegitimizing istihsan as arbitrary.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, independent_analogists, payer,
    organized, generational, constrained, global).

% Advocate for reason as an independent legal source and theological tool. Their methodological approach is structurally excluded from the hierarchy, which limits legitimate reasoning to analogical deduction after textual sources are exhausted, rendering their voice illegitimate inside the standard framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, rationalist_theologians, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving inconsistencies between regional legal schools by establishing a uniform, hierarchical methodology for deriving legal rulings from revealed sources, reducing arbitrary variation in jurisprudential outcomes.
% TRANSFER_FUNCTION: Transfers methodological authority and epistemic prestige from local customary practice and independent analogical reasoning to the hadith authentication enterprise; shifts juridical discretion from regional jurists to transmitter-critics.
% ABSENT_VOICES: Local customary jurists and rationalist analogists whose methodologies were delegitimized; their descendants in the Hanafi and Maliki schools who continue variant methods but are structurally subordinated in the hierarchy.
% DISAPPEARANCE_RATIONALE: If the four-tier hierarchy and hadith-arbiter principle vanished, legal methodology would fragment back toward regional eclecticism; hadith scholars would lose their gatekeeping role, and customary practice and analogical extension would regain independent legitimacy.
% FOUNDING_PROBLEM: Early Islamic jurisprudence suffered from severe methodological inconsistency across regional schools, with contradictory rulings arising from unmoored analogical reasoning, divergent local customs, and disputed prophetic traditions.
% FOUNDING_PROBLEM_CORROBORATION: Historical attestations from pre-Shafi'i jurists document fragmentation. Modern historians of Islamic law corroborate the inconsistency. However, Hanafi and Maliki scholars attest that the 'problem' was a false framing that delegitimized their living methodologies to elevate hadith scholarship; their testimony from outside the benefiting party supports the shifted-function reading.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) reflects the medium-high extraction expected from making hadith authentication the gatekeeper of legal validity: the hadith scholarly class captures epistemic rents. Suppression (0.55) is moderate because the enforcement is scholarly and discursive rather than physically coercive, yet alternative methodologies are systematically delegitimized. Theater ratio (0.25) is low because the methodological labor is genuine and substantive, though some ritualized citation practice develops later. Accessibility collapse (0.60) is significant: once the hierarchy is accepted, customary and analogical alternatives collapse as independently legitimate. Resistance (0.45) is moderate because the demoted schools continue to operate and contest the framing. The measurement series track the institutionalization of the method over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The hadith scholar seat experiences the constraint as necessary methodological reform that cleans up inconsistency; the customary jurist and analogist seats experience the same structure as a transfer of authority to a specialist class that extracts prestige and gatekeeping power. The engine computes this divergence from structural dataâbeneficiary/victim declarations and exit asymmetryâwithout requiring reconciled claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars are declared beneficiaries and agenda-setters with arbitrage-grade exit; their directionality sits near the beneficiary pole, yielding damped effective extraction (they are subsidized by the constraint). Local customary jurists and independent analogists are declared victims/payers with constrained exit; their directionality sits near the target pole, yielding amplified effective extraction. The excluded rationalist theologians have no voice and high directionality, but their exclusion is the enforcement mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is prevented by recognizing that the coordination function is genuineâstandardization did reduce inconsistencyâbut the extraction is equally genuine: a single group becomes the necessary arbiter. If we ignored the coordination, we would mislabel a rope as a snare; if we ignored the extraction, we would mislabel a tangled rope as a rope. The structural declarations (beneficiaries + victims + active enforcement) force the correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_sources_legitimacy,
    'Does the delegitimization of customary practice and extensive analogy reflect a necessary methodological correction or an asymmetric power transfer to the hadith scholarly class?',
    'Comparative legal history assessing whether the resulting standardization produced more consistent justice or merely transferred authority to a narrow specialist corps.',
    'If the former, the constraint leans toward rope/scaffold; if the latter, toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_sources_legitimacy, conceptual, 'Whether demotion of custom and analogy is correction or extraction.').

omega_variable(
    hadith_transmission_reliability,
    'Is the hadith corpus, as filtered through classical rijal criticism, sufficiently reliable to function as the exclusive second-tier source after the Qur''an?',
    'Historical and philological analysis of transmission chains; comparison with contemporary documentary evidence where available.',
    'If hadith transmission is shown to be systematically uncertain, the constraint''s base extractiveness rises because the arbiter role rests on a contested foundation; if reliable, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_transmission_reliability, empirical, 'Empirical reliability of the transmission filter.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the Shafi''i reading of the jurisprudential method kernel. How does its structural relationship to the Hanafi, Maliki, and Hanbali readings alter its classification?',
    'Cross-reading analysis: if the Shafi''i hierarchy forecloses the Maliki living tradition or Hanafi istihsan, the reading is more extractive; if it merely coexists, the extraction is competitive rather than suppressive.',
    'Determines whether the constraint is a tangled rope (coexistence with influence) or a snare (structural foreclosure of sibling readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship to sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t8, jurisprudential_method_kernel__shafii_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(juri_tr_t16, jurisprudential_method_kernel__shafii_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(juri_tr_t24, jurisprudential_method_kernel__shafii_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(juri_tr_t32, jurisprudential_method_kernel__shafii_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(juri_be_t8, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(juri_be_t16, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(juri_be_t24, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(juri_be_t32, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(juri_su_t8, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(juri_su_t16, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(juri_su_t24, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(juri_su_t32, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_reading).

% DUAL FORMULATION NOTE:
% This constraint is the Shafi'i reading of the jurisprudential_method_kernel, decomposed from the colloquial label 'Islamic legal methodology' which conflates four structurally distinct readings. Each reading has its own epsilon, stakeholders, and classification. The epsilon-invariance principle requires separate stories for each reading because measuring 'Islamic law' via hadith authentication yields a different extractiveness profile than measuring it via living tradition or analogical extension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
