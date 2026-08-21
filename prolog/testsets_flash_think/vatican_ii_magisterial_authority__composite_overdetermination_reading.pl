% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority (Composite Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story analyzes Vatican II's magisterial authority through
 *   the 'composite overdetermination' reading. This perspective argues that
 *   the Council's texts are not a single, coherent reinterpretation, but
 *   rather a collection of ambiguous compromise formulations designed to
 *   achieve supermajority votes by encoding incompatible ecclesiological
 *   visions. This overdetermination means that hermeneutical control, rather
 *   than the texts' inherent clarity, becomes the primary locus of authority,
 *   leading to structural implementation divergence rather than a unified
 *   vision. The 10-12% rejection votes on key documents are seen as signals
 *   of unresolved theological incompatibility embedded within the final
 *   texts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '4c3e69ac-fe08-4901-b156-8fd9f69d0b49').
narrative_ontology:cs_kernel_codification('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', fixed_text).
narrative_ontology:cs_authority_grounding('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', lineage).
narrative_ontology:cs_interpretation_layer_present('4c3e69ac-fe08-4901-b156-8fd9f69d0b49').
narrative_ontology:cs_reading_relation('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', foundational, conciliar_texts_contain_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', conciliar_texts_contain_incompatible_visions, conventional).
narrative_ontology:cs_axiom('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', foundational, hermeneutical_control_is_locus_of_power).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_power, holdable).
narrative_ontology:cs_axiom_grounding('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', hermeneutical_control_is_locus_of_power, empirically_contingent).
narrative_ontology:cs_reference_frame('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', conciliar_compromise_texts).
narrative_ontology:cs_drift_state('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', contemporary_hermeneutical_struggle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c3e69ac-fe08-4901-b156-8fd9f69d0b49', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_interpreters).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, centrist_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, rank_and_file_catholics).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_authority).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecclesial_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those within the Church hierarchy tasked with interpreting and applying the Vatican II texts. They benefit from the ambiguity as it allows them to maintain control over the hermeneutical process, mediating between conflicting interpretations and asserting the 'authentic' meaning.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_interpreters, agenda_setter,
    institutional, generational, constrained, global).

% Theologians who thrive in the space of ambiguity, seeking synthesis and reconciliation between seemingly incompatible positions. They benefit from the overdetermined nature of the texts, which provides fertile ground for ongoing academic and pastoral work without forcing a definitive choice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, centrist_theologians, beneficiary,
    organized, biographical, mobile, global).

% Groups who advocate for a strict continuity with pre-conciliar tradition. They bear the cost of the texts' ambiguities, which they perceive as diluting clear doctrine and opening the door to rupture. Their attempts to assert a 'pure' continuity reading are often marginalized.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    organized, generational, identity_locked, global).

% Groups who advocate for a radical break with pre-conciliar tradition and a new ecclesiology. They bear the cost of the texts' ambiguities, which they perceive as hindering necessary reforms and maintaining outdated structures. Their attempts to assert a 'pure' rupture reading are often resisted.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_factions, payer,
    organized, generational, identity_locked, global).

% The general body of believers who experience the ongoing hermeneutical struggle as confusion, internal strife, or a lack of clear pastoral guidance. They pay the cost of the overdetermination through diminished clarity and perceived instability within the Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, rank_and_file_catholics, payer,
    powerless, biographical, constrained, global).

% Scholars who analyze the historical context, drafting process, and reception of the Vatican II texts. They observe the mechanisms of compromise and the resulting overdetermination, providing critical analysis without direct participation in the hermeneutical struggle.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, historical_theologians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_interpreters).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the appearance of ecclesial unity and conciliar authority by accommodating diverse theological viewpoints within a single set of documents, thereby achieving supermajority votes during the Council.
% TRANSFER_FUNCTION: Transfers hermeneutical control and institutional legitimacy to those who can navigate and enforce the ambiguous compromise formulations, from those seeking clear, unambiguous doctrinal statements (both traditionalist and progressive).
% ABSENT_VOICES: The 'pure' voices of both uncompromised traditionalism and uncompromised progressivism were structurally excluded from the final conciliar texts, as their definitive statements would have prevented the necessary supermajority consensus. They are present in the ongoing debates but not in the foundational texts themselves.
% DISAPPEARANCE_RATIONALE: If the composite, overdetermined nature of Vatican II's magisterial authority vanished overnight, the Church would be forced to definitively choose between a pure continuity or a pure rupture reading, leading to a major schism or a radical redefinition of its magisterial function and identity. The current institutional equilibrium depends on managing this ambiguity.
% FOUNDING_PROBLEM: To address the challenges of modernity and update the Church's self-understanding while preserving doctrinal continuity, and to achieve a supermajority consensus among bishops with diverse theological perspectives, many of whom held incompatible visions.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Council, independent theologians, and even some participants (e.g., periti) have documented the deliberate use of ambiguous language and compromise formulations to achieve consensus, corroborating the composite and overdetermined nature of the texts. This is attested in numerous scholarly works and memoirs from outside the immediate beneficiaries of the current hermeneutical control.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the ongoing struggle for interpretive control, fueled by textual ambiguity, allows certain factions (magisterial interpreters, centrist theologians) to accrue power and legitimacy by managing the tension, at the expense of those seeking clear doctrinal resolution. Suppression (0.55) is moderate because while outright rejection of the Council is suppressed, the ambiguity itself allows for a range of interpretations to coexist, albeit under constant pressure to conform to the 'official' hermeneutic. Theater ratio (0.40) reflects the significant effort expended on maintaining the *appearance* of unity and continuity, often through rhetorical reconciliation of fundamentally divergent concepts. Resistance (0.70) is high due to the active and ongoing hermeneutical battles waged by both traditionalist and progressive factions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of magisterial interpreters, the ambiguity is a feature, allowing for organic development and pastoral flexibility. From the perspective of traditionalist and progressive factions, it is a bug, preventing necessary clarity or reform. The engine's per-seat classification will reflect these divergent experiences, with beneficiaries seeing a 'rope' or 'tangled_rope' and victims experiencing a 'snare' or 'tangled_rope' due to the extraction of interpretive clarity.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial interpreters and centrist theologians are beneficiaries (low d) as they gain influence and legitimacy from their role in navigating and synthesizing the ambiguous texts. Traditionalist and progressive factions are targets (high d) as their clear, uncompromised visions are diluted and their attempts to assert a singular interpretation are resisted by the overdetermined nature of the texts. Rank-and-file Catholics are also targets, bearing the cost of confusion. The constraint subsidizes the ongoing interpretive industry.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled_rope' prevents mislabeling the constraint as a 'rope' (pure coordination) by highlighting the asymmetric extraction of interpretive control and the costs borne by factions seeking clear doctrinal statements. It also avoids mislabeling it as a 'snare' (pure extraction) by acknowledging the genuine coordination function of achieving supermajority consensus and maintaining a semblance of unity, however fragile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''composite_overdetermination_reading'' of the ''vatican_ii_magisterial_authority'' kernel, or is it better subsumed under a different reading?',
    'Further historical and theological analysis of the Council''s drafting process and the subsequent hermeneutical struggles, focusing on the explicit intent behind ambiguous formulations and the documented incompatibility of underlying theological positions.',
    'If this reading is confirmed, it reinforces the structural nature of the hermeneutical conflict. If subsumed, the analysis of extraction and suppression would shift to reflect the dominant reading''s framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the Vatican II kernel.').

omega_variable(
    hermeneutical_control_locus,
    'Is the primary locus of magisterial authority truly in the overdetermined texts themselves, or has it effectively shifted to the interpretive bodies and their capacity to enforce a particular hermeneutic?',
    'Empirical observation of how doctrinal disputes are resolved, who has the final say in interpreting ambiguous texts, and whether alternative interpretations are genuinely considered or systematically marginalized.',
    'If authority has shifted to interpretive bodies, the constraint''s extractiveness and suppression would be even higher, as the ''texts'' become a mere instrument for the agenda-setters. If the texts retain inherent authority, the constraint is more ''mountain-like'' in its resistance to arbitrary interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_control_locus, empirical, 'Determines where real authority resides: texts vs. interpreters.').

omega_variable(
    compromise_efficacy,
    'Was the textual overdetermination a successful strategy for long-term ecclesial unity, or has it become a persistent source of internal conflict and institutional instability?',
    'Longitudinal study of internal Church cohesion, rates of schism or dissent, and the perceived legitimacy of magisterial authority across different factions over several decades post-Council.',
    'If the compromise is found to be a source of instability, the ''tangled_rope'' classification is strongly reinforced, potentially leaning towards ''snare'' if the costs of disunity outweigh the benefits of managed ambiguity. If it is found to have fostered a robust, if complex, unity, the ''rope'' aspect would be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compromise_efficacy, empirical, 'Assesses the long-term impact of textual compromise on Church unity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.45).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_magisterial_authority' kernel. This 'composite_overdetermination_reading' focuses on the textual ambiguities and compromises that allow for both continuity and rupture interpretations to coexist in tension, leading to ongoing hermeneutical struggle. It differs from the 'continuity_reading' (which emphasizes organic development) and the 'rupture_reading' (which emphasizes a fundamental break) by asserting that the texts themselves structurally encode this tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
