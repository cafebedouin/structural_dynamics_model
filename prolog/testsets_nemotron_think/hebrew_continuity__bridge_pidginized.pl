% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Language (Bridge/Pidginized Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the bridge_pidginized reading of the
 *   Hebrew continuity kernel: Hebrew lives as a contact language for Jewish
 *   diaspora interaction, neither purely liturgical nor fully native. The
 *   arrangement is a pidginized variety used for cross-communal trade,
 *   correspondence, and governance, anchored by high-register written
 *   production that maintains a symbolic link to the textual tradition. The
 *   reading claims this instrumental utility is the primary engine of
 *   Hebrew's survival in diaspora. The two sibling readings —
 *   liturgical_preservation and native_generative — dismiss this variety as
 *   'not really Hebrew', creating a three-way contest over what counts as
 *   legitimate continuity. The constraint is claimed as a rope (genuine
 *   coordination with minimal extraction) because the contact language solves
 *   a collective-action problem (communication across the diaspora) without
 *   coercive enforcement and with participants as net beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.18).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.12).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Language (Bridge/Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'f58e1cd1-439a-4ac3-b1a9-8df015dee042').
narrative_ontology:cs_kernel_codification('f58e1cd1-439a-4ac3-b1a9-8df015dee042', distributed).
narrative_ontology:cs_authority_grounding('f58e1cd1-439a-4ac3-b1a9-8df015dee042', practice).
narrative_ontology:cs_interpretation_layer_present('f58e1cd1-439a-4ac3-b1a9-8df015dee042').
narrative_ontology:cs_reading_relation('f58e1cd1-439a-4ac3-b1a9-8df015dee042', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('f58e1cd1-439a-4ac3-b1a9-8df015dee042', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('f58e1cd1-439a-4ac3-b1a9-8df015dee042', foundational, hebrew_continuity_through_instrumental_contact).
narrative_ontology:cs_axiom_status(hebrew_continuity_through_instrumental_contact, holdable).
narrative_ontology:cs_axiom_grounding('f58e1cd1-439a-4ac3-b1a9-8df015dee042', hebrew_continuity_through_instrumental_contact, empirically_contingent).
narrative_ontology:cs_axiom('f58e1cd1-439a-4ac3-b1a9-8df015dee042', foundational, pidginized_hebrew_as_valid_hebrew).
narrative_ontology:cs_axiom_status(pidginized_hebrew_as_valid_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('f58e1cd1-439a-4ac3-b1a9-8df015dee042', pidginized_hebrew_as_valid_hebrew, conventional).
narrative_ontology:cs_reference_frame('f58e1cd1-439a-4ac3-b1a9-8df015dee042', diaspora_contact_ecology).
narrative_ontology:cs_drift_state('f58e1cd1-439a-4ac3-b1a9-8df015dee042', modern_national_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f58e1cd1-439a-4ac3-b1a9-8df015dee042', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jews).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, hebrew_as_lingua_franca).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_language_vitality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities dispersed across multiple linguistic territories who use Hebrew pidgin as a shared medium for trade, correspondence, and cross-comunal governance. They invest in acquiring the contact variety because it lowers transaction costs across the diaspora network; exit means reverting to local majority languages or other lingua francas, which is feasible but erodes the specifically Jewish communicative space.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jews, beneficiary,
    organized, generational, mobile, global).

% Rabbinic and scholarly elites who maintain Hebrew through ritual recitation, textual commentary, and legal adjudication. They define Hebrew continuity as preservation of the sacred register and dismiss the pidginized contact variety as a corruption. Their authority rests on control of the canonical textual tradition; they are structurally excluded from the contact language's functional domain but hold symbolic power over what counts as 'real Hebrew'.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_authorities, excluded,
    institutional, generational, identity_locked, global).

% The sparse native speakers of Hebrew (historically in Palestine, later in early Yishuv) whose daily generative use constitutes the native_generative reading. They are few, geographically concentrated, and their variety is treated by the contact language as a prestige reference rather than the living norm. They would object to the pidgin being equated with Hebrew continuity, but their demographic weight is minimal in the diaspora frame.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers, excluded,
    powerless, biographical, identity_locked, local).

% Sociolinguists, historical linguists, and Jewish studies scholars who document the contact language's structure, diffusion, and ideological contestation. They provide the empirical record that the bridge reading draws on, but they do not set the agenda for Hebrew's institutional status.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, language_academics, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared linguistic medium for Jewish diaspora communities to communicate across linguistic boundaries, enabling trade, cultural exchange, halakhic consultation, and collective identity maintenance without requiring fluency in a single vernacular.
% TRANSFER_FUNCTION: Moves communicative effort from learning multiple local languages to acquiring a reduced, shared Jewish lingua franca (Hebrew pidgin), with high-register written production serving as a stabilizing standard that anchors the contact variety to the textual tradition.
% ABSENT_VOICES: Liturgical authorities and native Hebrew speakers who define Hebrew continuity through ritual purity or native intuition would object to the validation of a pidginized contact language as 'real Hebrew'. They are excluded from the contact language's functional domain but hold institutional and symbolic power in defining Hebrew legitimacy.
% DISAPPEARANCE_RATIONALE: The contact language fills a structural need for a Jewish-specific communication channel; its loss would force diaspora Jews into majority languages or non-Jewish lingua francas, accelerating assimilation and eroding a distinct Jewish public sphere that has persisted for centuries.
% FOUNDING_PROBLEM: The problem of maintaining Jewish collective communication and identity across a multilingual diaspora without a shared vernacular, while preserving a link to the textual tradition.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociolinguistics (e.g., Joshua Fishman, Paul Wexler, Sarah Bunin Benor) documents the persistent use of Hebrew as a diaspora contact language from the medieval period through the Haskalah; contemporary Jewish studies scholars outside the liturgical establishment attest to its ongoing functional role in transnational Jewish networks.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the contact language imposes minimal learning costs (pidgin simplicity) and does not extract rents from users; suppression is low (0.12) because participation is voluntary and alternatives (local languages, other lingua francas) remain accessible; theater ratio is very low (0.08) because the arrangement is functionally driven, not performative. Accessibility collapse is moderate (0.35) because the contact language creates a dedicated Jewish communicative space that would be hard to replicate with non-Jewish languages, but alternatives do exist. Resistance (0.42) reflects the ideological contest from the two sibling readings, which actively delegitimize the contact variety but cannot suppress its use.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora user's perspective, the contact language is a practical solution that works. From the liturgical authority's perspective, it is a degradation that threatens the integrity of the sacred tongue. From the native speaker's perspective, it is an impoverished substitute for a living language. The engine computes the beneficiary seat's classification from the metrics; the excluded seats' perceptions are recorded in the absent_voices field and the omega variables, but they do not receive a χ value because they are not targets of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jews are the primary beneficiaries (d near 0.0): they gain a low-cost coordination tool that preserves collective distinctiveness. Liturgical authorities and native speakers are excluded seats (d not computed for them because they are not governed by the contact language; they are external contesters). The contact language does not extract from them; it merely fails to recognize their definition of Hebrew. The engine will compute per-seat types from the structural data: the beneficiary seat should see a rope, the excluded seats are not subject to the constraint's χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora communication without a shared vernacular) remains live. The arrangement has not outlived its function; it persists because the structural need persists. No mandatrophy is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contact_language_vs_degradation,
    'Is the pidginized contact variety a genuine coordination adaptation that keeps Hebrew alive in diaspora, or a degraded form that accelerates the loss of Hebrew''s structural integrity and paves the way for language shift?',
    'Longitudinal sociolinguistic analysis of diaspora communities that used the contact variety versus those that did not: compare rates of language shift, intergenerational transmission, and lexical/grammatical attrition over centuries.',
    'If the contact variety correlates with sustained Jewish linguistic distinctiveness and eventual revivification (as in the Yishuv), it supports the rope classification. If it correlates with rapid assimilation and loss of Hebrew competence, it suggests the arrangement is a snare (extraction of communal resources into a dead-end variety).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contact_language_vs_degradation, empirical, 'Whether the contact language functions as a vitality bridge or a vitality trap.').

omega_variable(
    legitimacy_contestation_structure,
    'Does the three-way contest among readings represent a stable pluralism (each reading serves a different communal function) or an unstable struggle where one reading must ultimately displace the others for Hebrew to have a coherent institutional status?',
    'Institutional history of Hebrew language planning: examine whether the Academy of the Hebrew Language, Israeli education system, and diaspora institutions treat the contact variety as a legitimate historical layer or as an error to be corrected.',
    'If the contest is resolved by institutional fiat (e.g., standardization on native generative norms), the bridge reading may be retrospectively reclassified as a scaffold (transitional). If pluralism persists, the bridge reading remains a live rope alongside the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_contestation_structure, conceptual, 'Structural stability of the kernel''s reading ecology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__bridge_pidginized, theater_ratio, 20, 0.06).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.07).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__bridge_pidginized, theater_ratio, 60, 0.07).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__bridge_pidginized, theater_ratio, 80, 0.08).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__bridge_pidginized, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__bridge_pidginized, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__bridge_pidginized, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__bridge_pidginized, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__bridge_pidginized, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__bridge_pidginized, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__bridge_pidginized, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__bridge_pidginized, suppression_requirement, 60, 0.11).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__bridge_pidginized, suppression_requirement, 80, 0.11).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__bridge_pidginized, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hebrew_continuity kernel. The liturgical_preservation reading treats Hebrew as a fixed textual tradition; the native_generative reading treats it as a living spoken language. The bridge_pidginized reading treats it as a contact language. The three readings have different ε values, different beneficiary/victim structures, and different claimed types. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
