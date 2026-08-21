% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 as Abrogating Universal Offensive Jihad
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the 'abrogating_universal' reading of Quran
 *   9:5, which claims that this verse abrogates all prior peaceful verses,
 *   establishing universal offensive jihad as a standing legal obligation
 *   until polytheist submission or conversion. This reading places all
 *   non-Muslims into a victim set as legitimate targets absent formal
 *   submission, authorizes first-strike violence, and benefits expansionist
 *   movements claiming divine mandate. It entails high suppression of
 *   coexistence frameworks. The claimed type is 'snare' because its
 *   coordination story (divine mandate for universal order) is cover for pure
 *   extraction and suppression, with identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.95).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.95).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 as Abrogating Universal Offensive Jihad").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'dff1049e-2a34-41d2-a5e7-1104ff3f7643').
narrative_ontology:cs_kernel_codification('dff1049e-2a34-41d2-a5e7-1104ff3f7643', fixed_text).
narrative_ontology:cs_authority_grounding('dff1049e-2a34-41d2-a5e7-1104ff3f7643', lineage).
narrative_ontology:cs_interpretation_layer_present('dff1049e-2a34-41d2-a5e7-1104ff3f7643').
narrative_ontology:cs_reading_relation('dff1049e-2a34-41d2-a5e7-1104ff3f7643', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('dff1049e-2a34-41d2-a5e7-1104ff3f7643', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('dff1049e-2a34-41d2-a5e7-1104ff3f7643', foundational, abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('dff1049e-2a34-41d2-a5e7-1104ff3f7643', abrogation_of_peaceful_verses, theological).
narrative_ontology:cs_axiom('dff1049e-2a34-41d2-a5e7-1104ff3f7643', foundational, universal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('dff1049e-2a34-41d2-a5e7-1104ff3f7643', universal_offensive_jihad_obligation, theological).
narrative_ontology:cs_reference_frame('dff1049e-2a34-41d2-a5e7-1104ff3f7643', classical_islamic_legal_theory_of_war).
narrative_ontology:cs_drift_state('dff1049e-2a34-41d2-a5e7-1104ff3f7643', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dff1049e-2a34-41d2-a5e7-1104ff3f7643', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, authoritarian_islamic_states).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_minorities_in_non_islamic_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups interpret 9:5 as a divine mandate for universal offensive jihad, justifying violence against non-Muslims until conversion or submission. They actively enforce this interpretation through military action and ideological indoctrination, benefiting from the recruitment and territorial expansion it enables.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter,
    organized, generational, identity_locked, global).

% Some states adopt this reading to legitimize their authority, suppress internal dissent, and justify aggressive foreign policy. They benefit from the theological cover it provides for consolidating power and maintaining social control, even if they do not fully implement its most extreme interpretations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, authoritarian_islamic_states, beneficiary,
    institutional, generational, constrained, national).

% These populations are directly targeted by groups adhering to this interpretation, facing demands for conversion, submission, or violence. Their options are limited to flight, resistance, or capitulation, bearing the full cost of this constraint.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, immediate, trapped, local).

% Scholars who advocate for contextual or progressive readings of 9:5 face severe pressure, threats, and accusations of apostasy from adherents of the abrogating_universal view. Their careers and personal safety are at risk for challenging this dominant interpretation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars, payer,
    moderate, biographical, identity_locked, global).

% These communities suffer from the negative perception and backlash generated by the actions of groups adhering to the abrogating_universal interpretation. They bear the cost of increased scrutiny, discrimination, and suspicion in their host countries, despite not endorsing this view.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_minorities_in_non_islamic_states, payer,
    powerless, biographical, constrained, national).

% These organizations document human rights abuses committed by groups and states acting on this interpretation. They advocate for international law and universal human rights, providing an external analytical perspective on the constraint's impact.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For its adherents, it coordinates a unified ideological and military front against perceived enemies, providing clear directives for action and a sense of divine purpose for expansionist goals.
% TRANSFER_FUNCTION: Transfers sovereignty, resources, and lives from non-Muslim populations to expansionist Islamic entities, and transfers legitimacy from peaceful coexistence to religiously sanctioned warfare.
% ABSENT_VOICES: The vast majority of non-Muslims, who are the primary targets, are excluded from any discourse on the legitimacy or application of this verse. Additionally, many Muslim voices advocating for peaceful, contextual, or ethical interpretations are suppressed or marginalized.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the ideological justification for many jihadist movements would collapse, leading to a significant reduction in religiously motivated violence and territorial conflict. International relations would shift, and internal debates within Islamic jurisprudence would reorient towards more peaceful frameworks.
% FOUNDING_PROBLEM: The problem this interpretation was built to solve, from its adherents' perspective, was the perceived threat and resistance from polytheist tribes in 7th-century Arabia, and the broader theological imperative to establish Islamic dominance.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading, particularly jihadist movements, assert that the founding problem of polytheist resistance and the imperative for Islamic dominance remains live and universal. However, the vast majority of non-adherents, including most Muslim scholars and international observers, attest that the specific 7th-century context is dead, and the universal application is a misinterpretation used for political ends.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because it demands conversion or submission, extracting sovereignty, resources, and lives. Suppression is also very high (0.9) as it actively seeks to eliminate or subjugate alternatives to its worldview through coercion and violence. Theater ratio is low (0.1) because its function is direct and overt; there is little performative maintenance masking a degraded function. Accessibility collapse is high (0.8) as it aims to eliminate all alternatives to its prescribed order. Resistance is high (0.7) due to the direct and violent opposition it generates from targeted populations and those who reject its interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of this reading perceive it as a divine command for justice and universal order, while its victims and most external observers experience it as extreme extraction and violent suppression. The engine's classification as 'snare' reflects the latter, based on the structural outcomes of its application.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist jihadist movements are the primary agenda-setters and beneficiaries, directly profiting from the ideological justification for their actions. Authoritarian Islamic states may also benefit by leveraging this interpretation for internal control and external policy. Non-Muslim populations are the direct victims, facing existential threats. Moderate Muslim scholars and Muslim minorities are also victims, suffering suppression and backlash for their dissent or association. International human rights organizations serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare because its coordination function (establishing a divinely mandated order) is inseparable from its extractive and suppressive mechanisms. It does not solve a genuine collective action problem for all participants; rather, it imposes a solution on unwilling parties through coercion. The persistence of this interpretation is due to active enforcement and suppression of alternatives, not mutual benefit. It is not a tangled rope because the coordination story is almost entirely cover for extraction, with no net benefit for the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_vs_universal_application,
    'Is Quran 9:5 a universal, timeless command, or is its application strictly limited to its specific 7th-century Medinan historical context?',
    'Consensus among leading Islamic jurisprudents and historians on the principles of abrogation (naskh) and contextual interpretation (asbab al-nuzul), corroborated by independent historical and linguistic analysis.',
    'If contextual, this reading''s claim of universal offensive jihad collapses, reclassifying it from a snare to a historical artifact or a misinterpretation with negligible extractiveness. If universal, its current classification as a snare is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_context_vs_universal_application, conceptual, 'Ambiguity regarding the historical specificity versus universal applicability of Quran 9:5.').

omega_variable(
    theological_legitimacy_of_abrogation,
    'Is the doctrine of abrogation (naskh), particularly the abrogation of peaceful verses by 9:5, a universally accepted and theologically sound principle within Islamic jurisprudence?',
    'Comprehensive survey of classical and contemporary Islamic legal schools and theological positions on naskh, identifying the extent of scholarly consensus or dissent regarding 9:5''s abrogating role.',
    'If abrogation by 9:5 is widely rejected or highly contested, the theological foundation of this reading weakens significantly, reducing its legitimacy and thus its effective extractiveness and suppression. If widely accepted, its snare classification is further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_legitimacy_of_abrogation, conceptual, 'Contestation over the theological validity of abrogation as applied to Quran 9:5.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations structural (e.g., state censorship, physical threats) or internalized (e.g., self-censorship due to fear, ideological indoctrination)?',
    'Post-exit suppression trajectory: if suppression of moderate scholars persists after the immediate threat is removed, reclassify as partially internalized. Analysis of discourse patterns in regions where physical threats are absent.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — dissenters carry the suppression with them after exit, making the constraint more resilient. If purely structural, removing external barriers would more readily lead to alternative interpretations gaining traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__abrogating_universal, theater_ratio, 10, 0.12).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.11).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__abrogating_universal, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__abrogating_universal, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__abrogating_universal, base_extractiveness, 30, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__abrogating_universal, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__abrogating_universal, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_law_of_war).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, dhimmi_status_constraint).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, freedom_of_religion_in_islamic_states).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
