% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sole Sufficient and Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint represents the 'sola scriptura' reading of biblical
 *   authority, a foundational principle of the Protestant Reformation. It
 *   asserts that Scripture alone is the sufficient and self-interpreting
 *   source of authority for Christian doctrine and practice, rejecting the
 *   need for an external magisterium or tradition to provide authoritative
 *   interpretation. This reading emphasizes individual access to the text and
 *   congregational autonomy, leading to low clerical extraction but also
 *   contributing to doctrinal fragmentation across communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.25).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.15).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sole Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '9f1df64b-bfba-4dfe-908e-446115453315').
narrative_ontology:cs_kernel_codification('9f1df64b-bfba-4dfe-908e-446115453315', fixed_text).
narrative_ontology:cs_authority_grounding('9f1df64b-bfba-4dfe-908e-446115453315', self_enforcing).
narrative_ontology:cs_reading_relation('9f1df64b-bfba-4dfe-908e-446115453315', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('9f1df64b-bfba-4dfe-908e-446115453315', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('9f1df64b-bfba-4dfe-908e-446115453315', foundational, scripture_is_perspicuous).
narrative_ontology:cs_axiom_status(scripture_is_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('9f1df64b-bfba-4dfe-908e-446115453315', scripture_is_perspicuous, theological).
narrative_ontology:cs_axiom('9f1df64b-bfba-4dfe-908e-446115453315', foundational, scripture_is_sufficient).
narrative_ontology:cs_axiom_status(scripture_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9f1df64b-bfba-4dfe-908e-446115453315', scripture_is_sufficient, theological).
narrative_ontology:cs_reference_frame('9f1df64b-bfba-4dfe-908e-446115453315', reformation_era_direct_access).
narrative_ontology:cs_drift_state('9f1df64b-bfba-4dfe-908e-446115453315', contemporary_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f1df64b-bfba-4dfe-908e-446115453315', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, denominational_leaders).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly access and interpret scripture for themselves, fostering personal spiritual autonomy and reducing reliance on clerical intermediaries. They benefit from the accessibility of the text but bear the responsibility of interpretation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Operate with significant autonomy in doctrine and practice, deriving their understanding directly from scripture without external hierarchical control. They coordinate around shared interpretations but face challenges in maintaining broader unity.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_congregations, beneficiary,
    organized, biographical, mobile, local).

% Provide interpretive tools, historical context, and systematic theology, influencing how scripture is understood. However, their authority is advisory, not ultimate, as the individual believer is the final arbiter.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theologians_scholars, agenda_setter,
    powerful, generational, constrained, global).

% Struggle to maintain doctrinal coherence and unity across diverse congregations and interpretive traditions. They bear the costs of fragmentation and the constant need for internal negotiation, as no single external authority can impose a definitive interpretation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, denominational_leaders, payer,
    organized, biographical, constrained, national).

% Their claims to broader interpretive authority or unifying doctrinal statements are largely rejected by this reading, which prioritizes local and individual interpretation. They are structurally excluded from exercising binding authority.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_bodies, excluded,
    organized, generational, trapped, global).

% Study the historical, theological, and sociological impacts of 'sola scriptura', analyzing its role in the development of Protestantism and its ongoing challenges, without being bound by its internal claims.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates belief and practice around a single, accessible textual source, fostering individual spiritual autonomy and local congregational self-governance, thereby solving the problem of external hierarchical control.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from clerical hierarchies and traditions to individual believers and local communities, enabling direct access to divine revelation.
% ABSENT_VOICES: Magisterial authorities and ecumenical councils are structurally excluded; they would argue for the necessity of an authoritative interpretive tradition or a living teaching office to maintain doctrinal unity and guard against heterodoxy.
% DISAPPEARANCE_RATIONALE: If 'sola scriptura' vanished overnight, the foundational principle of many Protestant denominations would collapse. This would necessitate a radical re-evaluation of authority structures, potentially leading to widespread fragmentation, a return to traditional or conciliar models, or the emergence of new, non-textual forms of authority, fundamentally reorganizing the landscape of Christian belief and practice.
% FOUNDING_PROBLEM: The perceived corruption, unbiblical practices, and theological errors of the medieval church, coupled with a desire for direct access to God's word without intermediaries and a rejection of human traditions as equal to divine revelation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation widely corroborate the historical context and the problems 'sola scriptura' sought to address. Independent theological scholars and proponents of other readings (e.g., Catholic or Orthodox theologians) acknowledge the historical grievances, even while disagreeing with the 'sola scriptura' solution or its ongoing status.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily functions as a coordination mechanism around a shared text, empowering individual believers and local congregations rather than extracting rents. Any extraction is diffuse, arising from the costs of maintaining interpretive communities. Suppression is low (0.15) as the principle actively resists external coercion in matters of faith and interpretation. Theater ratio is low (0.10) because the emphasis is on direct engagement with the text, minimizing performative rituals as a source of authority. Accessibility collapse is moderate (0.40) as while the text is accessible, its interpretation still requires effort, education, and community, but without a gatekeeping clergy. Resistance is low (0.10) from within the tradition, as it is a core tenet, though it faces external resistance from other Christian traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of lay believers, 'sola scriptura' is a liberating force, granting direct access to divine truth. From the perspective of denominational leaders, it presents a constant challenge to doctrinal coherence and unity, requiring continuous effort to manage interpretive diversity. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual congregations are the primary beneficiaries, gaining direct access to authority and autonomy (low directionality). Theologians and scholars act as agenda-setters, influencing interpretation without holding ultimate authority. Denominational leaders are payers, bearing the costs of maintaining unity amidst diverse interpretations (higher directionality). Ecumenical bodies are excluded, as their claims to authority are rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''sola_scriptura_reading'' of the ''biblical_authority'' kernel?',
    'Comparison with historical theological texts and contemporary denominational statements to confirm adherence to the core tenets of ''sola scriptura'' as distinct from other interpretive frameworks.',
    'If misidentified, the analysis of its structural relations to sibling readings and its internal axioms would be flawed, leading to incorrect classification and network effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the kernel context.').

omega_variable(
    doctrinal_fragmentation_cost,
    'Is the observed doctrinal fragmentation within Protestantism a necessary byproduct of ''sola scriptura''s emphasis on individual interpretation, or an avoidable failure of coordination?',
    'Comparative study of denominations with varying levels of internal interpretive authority and their respective rates of schism or doctrinal divergence. Analysis of whether robust, non-coercive coordination mechanisms could mitigate fragmentation.',
    'If fragmentation is a necessary byproduct, it is an inherent cost of the coordination function. If avoidable, it suggests a failure in the constraint''s design or implementation, potentially increasing its effective extractiveness from those seeking unity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_cost, empirical, 'Assesses whether doctrinal fragmentation is an inherent cost or an avoidable failure.').

omega_variable(
    self_interpretation_ambiguity,
    'To what extent is Scripture truly ''self-interpreting'' without the implicit or explicit aid of interpretive communities, historical context, or theological training?',
    'Empirical studies of lay interpretation across diverse contexts, comparing outcomes with and without access to interpretive aids. Conceptual analysis of the philosophical underpinnings of ''perspicuity'' and its practical limits.',
    'If ''self-interpretation'' is largely a theoretical ideal, the constraint''s actual accessibility collapse and suppression may be higher than stated, as unacknowledged interpretive gates (e.g., literacy, education, community norms) effectively limit access for some, shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_interpretation_ambiguity, conceptual, 'Examines the practical limits of Scripture''s self-interpreting nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__sola_scriptura_reading, theater_ratio, 1600, 0.07).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__sola_scriptura_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__sola_scriptura_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__sola_scriptura_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.1).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__sola_scriptura_reading, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement(bibl_su_t1750, biblical_authority__sola_scriptura_reading, suppression_requirement, 1750, 0.13).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.14).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, protestant_denominational_autonomy).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, individual_conscience_authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
