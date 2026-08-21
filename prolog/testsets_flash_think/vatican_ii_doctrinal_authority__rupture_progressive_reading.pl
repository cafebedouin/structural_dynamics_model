% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture-Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture-progressive' reading of
 *   Vatican II's doctrinal authority. It posits that Vatican II represented a
 *   necessary break with pre-conciliar rigidity, and that the 'spirit of the
 *   Council' authorizes ongoing reform beyond the strict textual limits of
 *   the Council documents. This reading emphasizes adaptation to the modern
 *   world, ecumenism, and religious freedom, often interpreting textual
 *   ambiguities as intentional openings for further development.
 *   Post-conciliar implementation is treated as the authentic realization of
 *   conciliar intent, even when it appears to diverge from literal
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.65).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, scaffold).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:has_sunset_clause(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c42e6853-f305-435f-bc0a-6a083bcc5a80').
narrative_ontology:cs_kernel_codification('c42e6853-f305-435f-bc0a-6a083bcc5a80', fixed_text).
narrative_ontology:cs_authority_grounding('c42e6853-f305-435f-bc0a-6a083bcc5a80', lineage).
narrative_ontology:cs_interpretation_layer_present('c42e6853-f305-435f-bc0a-6a083bcc5a80').
narrative_ontology:cs_reading_relation('c42e6853-f305-435f-bc0a-6a083bcc5a80', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c42e6853-f305-435f-bc0a-6a083bcc5a80', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c42e6853-f305-435f-bc0a-6a083bcc5a80', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('c42e6853-f305-435f-bc0a-6a083bcc5a80', foundational, doctrinal_development_beyond_text).
narrative_ontology:cs_axiom_status(doctrinal_development_beyond_text, holdable).
narrative_ontology:cs_axiom_grounding('c42e6853-f305-435f-bc0a-6a083bcc5a80', doctrinal_development_beyond_text, conventional).
narrative_ontology:cs_axiom('c42e6853-f305-435f-bc0a-6a083bcc5a80', foundational, religious_freedom_as_intrinsic_right).
narrative_ontology:cs_axiom_status(religious_freedom_as_intrinsic_right, holdable).
narrative_ontology:cs_axiom_grounding('c42e6853-f305-435f-bc0a-6a083bcc5a80', religious_freedom_as_intrinsic_right, deontological).
narrative_ontology:cs_reference_frame('c42e6853-f305-435f-bc0a-6a083bcc5a80', post_conciliar_renewal).
narrative_ontology:cs_drift_state('c42e6853-f305-435f-bc0a-6a083bcc5a80', contemporary_synodal_process, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c42e6853-f305-435f-bc0a-6a083bcc5a80', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively interpret and promote the 'spirit of the Council' to justify ongoing doctrinal and pastoral reforms, gaining influence and shaping the Church's future direction.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, agenda_setter,
    institutional, generational, mobile, global).

% Embrace and benefit from the perceived liberalization and adaptation of the Church to modern society, finding their faith more relevant and inclusive.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_laity, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of doctrinal shifts, often marginalized or disciplined for resisting the 'spirit' and adhering to pre-conciliar interpretations, feeling a loss of their traditional identity and authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    institutional, generational, identity_locked, global).

% Feel alienated by changes, struggle with perceived loss of tradition and clarity, but remain within the Church due to deep-seated identity, family ties, or lack of perceived alternatives.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity, payer,
    moderate, biographical, constrained, global).

% The current teaching authority, which may align with or resist the progressive reading, but is ultimately responsible for implementing or reining in the 'spirit' through official pronouncements and appointments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium_of_the_day, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the internal dynamics of the Catholic Church from an external, academic, or journalistic perspective, often highlighting the tensions and shifts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, secular_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To adapt the Catholic Church to the modern world, fostering ecumenism, religious freedom, and a more collegial governance, moving beyond perceived rigidities of the past.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict textual adherence and historical precedent to an evolving 'spirit' or 'pastoral intent', from traditionalist interpretations to progressive ones.
% ABSENT_VOICES: Those who left the Church due to the perceived rupture, or those who are silenced within it for resisting the progressive interpretation; they would argue for a return to pre-conciliar norms or a more consistent traditionalism.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' as an authorizing principle for ongoing reform vanished, the current trajectory of the Catholic Church would lose its primary justification. This would lead to a re-entrenchment of textualism, a new crisis of authority, and fundamentally alter the Church's engagement with modernity, reorganizing its internal power dynamics and external relations.
% FOUNDING_PROBLEM: The perceived irrelevance and rigidity of the pre-conciliar Catholic Church in engaging with modern society, leading to a disconnect with contemporary thought, other Christian denominations, and a perceived lack of pastoral responsiveness.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many reform-minded laity attest to the problem's ongoing relevance, citing the need for continuous adaptation. Traditionalist critics and some secular historians might contest the framing of 'rigidity' or the necessity of the 'break', arguing the problem was misdiagnosed or exaggerated.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this reading demands significant shifts in doctrine and practice, imposing costs on those who adhere to prior interpretations or a more literal reading of the Council texts. Suppression (0.65) is substantial as traditionalist voices are often marginalized or disciplined for resisting the progressive interpretation. Resistance (0.75) is also high, reflecting ongoing internal conflicts. Theater ratio (0.2) is low, as the 'spirit' is actively pursued and implemented, not merely performed. The claimed type is 'scaffold' because this reading frames the Council as a transitional support for an ongoing, open-ended process of reform and adaptation, with the 'sunset' being the achievement of a fully adapted, yet perpetually evolving, Church.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive theologians, this reading is a necessary and beneficial adaptation, a 'rope' or 'scaffold' for the Church's vitality. From the perspective of traditionalist clergy and conservative laity, it is an extractive 'snare' that undermines tradition and imposes unwanted change, leading to a loss of identity and clarity.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform-minded laity are beneficiaries, gaining influence and finding their views affirmed. Traditionalist clergy and conservative laity are victims, bearing the costs of doctrinal shifts and feeling alienated. The Magisterium of the day acts as an agenda-setter, enforcing the prevailing interpretation, which can align with or resist the progressive reading depending on its composition.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as 'scaffold' with high extractiveness and suppression prevents mislabeling this as a pure 'rope' (simple coordination) or a 'mountain' (natural evolution). While it claims to be a necessary transition, its active enforcement and the costs it imposes on dissenting parties reveal its extractive dimension. The 'has_sunset_clause: true' reflects the idea that the justification is the transition, not a steady state, even if that transition is open-ended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_of_council_legitimacy,
    'Is the ''spirit of the Council'' a legitimate hermeneutical principle for doctrinal development, or does it represent an overreach beyond the Council''s actual textual and historical intent?',
    'Extensive historical-theological scholarship analyzing the Council Fathers'' intentions, the drafting process, and the reception history, alongside Magisterial clarifications on the limits of interpretation.',
    'If deemed an overreach, the justification for ongoing reforms beyond textual limits would weaken, potentially reclassifying the constraint as more extractive (snare) for those forced to comply. If affirmed, it strengthens the ''scaffold'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_of_council_legitimacy, conceptual, 'The hermeneutical validity of the ''spirit of the Council'' as an interpretive key.').

omega_variable(
    pre_conciliar_rigidity_accuracy,
    'Is the perceived ''rigidity'' of the pre-conciliar Church an accurate historical assessment, or a polemical framing used to justify radical change?',
    'Independent historical research into pre-conciliar theological movements, pastoral practices, and intellectual engagement with modernity, avoiding post-conciliar biases.',
    'If pre-conciliar rigidity is found to be exaggerated, the ''necessary break'' narrative weakens, potentially reclassifying the constraint as more extractive (snare) for those whose traditions were dismissed. If affirmed, it strengthens the ''scaffold'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_conciliar_rigidity_accuracy, empirical, 'Historical accuracy of the ''pre-conciliar rigidity'' narrative.').

omega_variable(
    doctrinal_change_reversal_status,
    'Does the Council''s teaching on religious freedom (Dignitatis Humanae) represent a genuine reversal of prior condemnations (e.g., Syllabus of Errors), or an organic development that avoids contradiction?',
    'Detailed theological analysis comparing the texts and their underlying philosophical assumptions, seeking to reconcile or identify irreconcilable differences, potentially with Magisterial intervention.',
    'If it is a genuine reversal, it strongly supports the ''rupture'' aspect of this reading, validating its high extractiveness on prior doctrine. If it''s an organic development, the ''rupture'' claim is weakened, and the constraint might be reclassified as less extractive on doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_change_reversal_status, conceptual, 'Whether key doctrinal shifts constitute reversal or development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.19).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'Vatican II doctrinal authority' kernel. Each reading presents a different structural interpretation of the Council's impact and legitimacy, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
