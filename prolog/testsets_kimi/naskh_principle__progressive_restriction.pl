% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Quranic Progressive Restriction Reading of Naskh
 *   domain: islamic_jurisprudence/hermeneutics
 *
 * SUMMARY:
 *   The progressive restriction reading of the naskh (abrogation) kernel
 *   holds that Quranic revelation moved from permissive to restrictive
 *   rulings as a divine pedagogical strategy, not as textual invalidation.
 *   Earlier verses on matters such as wine, slavery, or gender relations are
 *   treated as transitional accommodations suited to the immediate revelatory
 *   community, while later restrictive verses express the final divine
 *   intent. This reading is advanced by evolutionary jurists and
 *   institutional hermeneutic authorities as a framework that preserves
 *   scriptural coherence without classical abrogation. However, it
 *   structurally marginalizes practitioners who cite earlier permissive texts
 *   as still-valid guides for contemporary life, delegitimizing their
 *   practice as historically naive. The constraint is claimed as coordination
 *   (reconciling chronology) but operates with substantial extraction against
 *   permissive seats.
 *
 * KEY AGENTS:
 *   - hermeneutic_authority: Primary agenda_setter (institutional/constrained) â administers the principle through seminary curricula, fatwa councils, and canonical manuals.
 *   - evolutionary_jurists: Primary beneficiary (organized/constrained) â accrues scholarly authority and career advancement from the pedagogical framework.
 *   - permissive_practitioners: Primary target (moderate/identity_locked) â bears extraction as their textual basis for practice is delegitimized.
 *   - classical_abrogation_scholars: Excluded voice (organized/constrained) â holds the competing abrogation reading, structurally sidelined in progressive restriction forums.
 *   - contextual_harmonization_scholars: Excluded voice (organized/constrained) â rejects chronological supersession entirely, absent from the institutional debate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.55).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Quranic Progressive Restriction Reading of Naskh").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '9a60ad76-efb3-4051-9bbb-e976572cd5de').
narrative_ontology:cs_kernel_codification('9a60ad76-efb3-4051-9bbb-e976572cd5de', fixed_text).
narrative_ontology:cs_authority_grounding('9a60ad76-efb3-4051-9bbb-e976572cd5de', lineage).
narrative_ontology:cs_interpretation_layer_present('9a60ad76-efb3-4051-9bbb-e976572cd5de').
narrative_ontology:cs_reading_relation('9a60ad76-efb3-4051-9bbb-e976572cd5de', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('9a60ad76-efb3-4051-9bbb-e976572cd5de', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('9a60ad76-efb3-4051-9bbb-e976572cd5de', foundational, earlier_verses_transitional_accommodation).
narrative_ontology:cs_axiom_status(earlier_verses_transitional_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('9a60ad76-efb3-4051-9bbb-e976572cd5de', earlier_verses_transitional_accommodation, theological).
narrative_ontology:cs_axiom('9a60ad76-efb3-4051-9bbb-e976572cd5de', foundational, later_restrictions_final_intent).
narrative_ontology:cs_axiom_status(later_restrictions_final_intent, holdable).
narrative_ontology:cs_axiom_grounding('9a60ad76-efb3-4051-9bbb-e976572cd5de', later_restrictions_final_intent, theological).
narrative_ontology:cs_reference_frame('9a60ad76-efb3-4051-9bbb-e976572cd5de', divine_pedagogical_progression).
narrative_ontology:cs_drift_state('9a60ad76-efb3-4051-9bbb-e976572cd5de', contemporary_fiqh_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a60ad76-efb3-4051-9bbb-e976572cd5de', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_jurists).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, permissive_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls seminary curricula, fatwa protocols, and canonical legal manuals. Determines which Quranic rulings are classified as transitional accommodations versus final divine legislation. Can shift classifications only within bounds that preserve juristic legitimacy and textual continuity; exiting the traditional interpretive framework would dissolve its authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, hermeneutic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Produce scholarship and legal opinions that frame later restrictive rulings as the always-intended final divine pedagogy. Their academic standing, publication access, and institutional appointments grow when this reading is endorsed by the hermeneutic authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_jurists, beneficiary,
    organized, biographical, constrained, global).

% Cite earlier permissive Quranic texts as guides for contemporary personal and communal practice. Under the progressive restriction reading, these citations are delegitimized as naive or historically conditioned. Their religious identity is often fused with the earlier practices, making scholarly rejection feel like existential invalidation rather than mere legal updating.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, permissive_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Hold the competing reading that later Quranic verses abrogate and invalidate earlier ones. In institutional forums where progressive restriction is presented as the only sophisticated alternative to literalism, their voices are not invited to adjudicate between the two frameworks.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_scholars, excluded,
    organized, generational, constrained, global).

% Hold that all verses remain valid within their specific revelatory contexts and reject chronological supersession of any kind. They are excluded from the binary debate that treats only progressive restriction and classical abrogation as live scholarly options.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, hermeneutic_authority).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic hermeneutic framework that reconciles chronologically variant Quranic rulings without requiring textual invalidation, preserving the coherence of the revelatory corpus as a unified pedagogical whole.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy from practitioners of earlier permissive rulings to scholars and institutions that enforce later restrictive rulings as the final divine intent.
% ABSENT_VOICES: Classical abrogation scholars reject the denial of textual invalidation; contextual harmonization scholars reject chronological supersession entirely; permissive practitioners themselves are rarely present in the scholarly venues where their lived practice is classified as historically transitional.
% DISAPPEARANCE_RATIONALE: If the progressive restriction reading vanished, permissive practitioners would regain textual legitimacy for their practices, classical abrogation would resurge as the primary chronological hermeneutic, and the authority of evolutionary jurists who built careers on the pedagogical narrative would weaken â the interpretive landscape would reorganize around either classical naskh or contextual harmonization.
% FOUNDING_PROBLEM: Apparent contradictions between earlier permissive and later restrictive Quranic rulings threatened the coherence of scripture and the legitimacy of the legal tradition; a mechanism was needed to reconcile chronology without rendering earlier verses falsely revealed or erroneously preserved.
% FOUNDING_PROBLEM_CORROBORATION: Evolutionary jurists attest the problem is still live, citing the need for non-literalist reconciliation of scripture. Classical abrogation scholars and historians of Islamic law attest that the problem is partly an artifact of later jurists' anachronistic systematization; they corroborate the founding problem's contested status from outside the progressive restriction beneficiary set.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the reading systematically strips earlier permissive rulings of contemporary legal force, transferring authority to later restrictive interpreters. Suppression (0.55) reflects the active marginalization of permissive practitioners and the scholarly gatekeeping that prevents classical abrogation or contextual harmonization from gaining equal institutional footing. Theater ratio (0.28) is low-moderate: the pedagogical narrative is often sincerely held, but a performative element exists when scholars invoke divine pedagogy to justify restrictions that align with their prior normative commitments. Accessibility collapse (0.45) is partial: alternative readings remain intellectually available in the broader tradition, but within institutions dominated by progressive restriction, alternatives become practically inaccessible. Resistance (0.50) is moderate: classical scholars resist, and permissive practitioners maintain subaltern persistence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats experience the constraint as a sophisticated retrieval of scriptural wisdom that preserves divine speech from the charge of error. The payer seat experiences the same structure as a closure of their textual and ritual options, enforced by scholarly authority that dismisses their lived practice as historically naive. The excluded sibling-reading seats see the constraint as an unwarranted foreclosure of alternative hermeneutic possibilities.
 *
 * DIRECTIONALITY LOGIC:
 *   The hermeneutic authority and evolutionary jurists are structural beneficiaries: the constraint subsidizes their interpretive monopoly and scholarly careers (low d). Permissive practitioners are structural targets: their identity-fusion with earlier texts and lack of scholarly standing amplify their directionality toward full target (high d). The classical_abrogation_scholars and contextual_harmonization_scholars are structurally excluded, sitting outside the directionality derivation because they are not governed by the constraintâthey are competing constraint instances.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare is warranted because the reading does perform a genuine coordination function: it resolves a real hermeneutic problem (chronological contradiction) without requiring textual invalidation, which many believers find theologically costly. However, the same structure that coordinates also extracts, because the pedagogical classification is applied selectively to verses where later restriction aligns with the jurists' preferences. If the coordination function were primary and extraction negligible, it would be a rope; if the coordination were entirely cover, it would be a snare. The presence of both, plus active enforcement, places it in the hybrid category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the progressive restriction reading represent a genuine retrieval of original revelatory pedagogy, or a retroactive rationalization constructed to justify later legal preferences?',
    'Historical-critical analysis of early Islamic legal literature to determine if the progressive restriction framework appears before or after the restrictive rulings became institutionally dominant.',
    'If constructed, the constraint''s extraction is higher than its coordination function suggests; if genuine retrieval, the coordination function is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the progressive restriction reading is a constructed rationalization or genuine retrieval.').

omega_variable(
    extraction_vs_coordination_boundary,
    'To what extent does the progressive restriction principle solve a real coordination problem (scriptural coherence) versus extract from permissive practitioners by foreclosing their textual basis?',
    'Comparative analysis of legal systems that resolve scriptural chronology through other means (classical abrogation, contextual harmonization) to measure the comparative extraction level.',
    'If the coordination function is separable from the extraction, the constraint is a tangled rope; if the coordination is merely cover, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Coordination function versus extraction in progressive restriction.').

omega_variable(
    verses_under_progressive_scope,
    'Which specific Quranic rulings are legitimately subject to progressive restriction, and who decides the boundary between transitional accommodation and permanent law?',
    'Corpus analysis of fiqh literature to map which verses have been claimed under this principle and whether the selection pattern tracks doctrinal utility for the claiming scholars.',
    'If the selection pattern concentrates on verses where later restriction aligns with the jurists'' preferred norms, the extraction profile is asymmetric and targeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verses_under_progressive_scope, empirical, 'Scope ambiguity of which verses are transitional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nask_tr_t8, naskh_principle__progressive_restriction, theater_ratio, 8, 0.2).
narrative_ontology:measurement(nask_tr_t16, naskh_principle__progressive_restriction, theater_ratio, 16, 0.24).
narrative_ontology:measurement(nask_tr_t24, naskh_principle__progressive_restriction, theater_ratio, 24, 0.26).
narrative_ontology:measurement(nask_tr_t32, naskh_principle__progressive_restriction, theater_ratio, 32, 0.27).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nask_be_t8, naskh_principle__progressive_restriction, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(nask_be_t16, naskh_principle__progressive_restriction, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(nask_be_t24, naskh_principle__progressive_restriction, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(nask_be_t32, naskh_principle__progressive_restriction, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(naskh_principle__progressive_restriction, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, contextual_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the naskh_principle kernel. The classical_abrogation and contextual_harmonization readings instantiate structurally distinct constraints from the same textual kernel, with different epsilon profiles and stakeholder arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
