% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto: Endogenous Prophetic Reinterpretation
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto from the perspective of
 *   'endogenous prophetic reinterpretation.' In this reading, the Manifesto
 *   is understood as a legitimate act of divine revelation, where God
 *   temporarily suspended the practice of plural marriage to preserve the
 *   church's salvific mission and ensure its survival against federal
 *   persecution. The constraint coordinates the church's members around this
 *   new directive, maintaining institutional unity and legitimacy. While
 *   claimed as a 'Rope' (a coordination mechanism), the metrics reflect the
 *   significant extraction from those who resisted and the active enforcement
 *   required to maintain the new norm.
 *
 * KEY AGENTS:
 *   - church_institution: Primary beneficiary and agenda_setter (institutional/arbitrage)
 *   - prophetic_leadership: Agenda_setter (institutional/identity_locked)
 *   - mainstream_members: Beneficiary (organized/constrained)
 *   - fundamentalist_members: Payer (powerless/identity_locked)
 *   - federal_government: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.72).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.85).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto: Endogenous Prophetic Reinterpretation").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '6aa59ca4-3145-442c-897c-01e87ad16a17').
narrative_ontology:cs_kernel_codification('6aa59ca4-3145-442c-897c-01e87ad16a17', fixed_text).
narrative_ontology:cs_authority_grounding('6aa59ca4-3145-442c-897c-01e87ad16a17', lineage).
narrative_ontology:cs_interpretation_layer_present('6aa59ca4-3145-442c-897c-01e87ad16a17').
narrative_ontology:cs_reading_relation('6aa59ca4-3145-442c-897c-01e87ad16a17', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aa59ca4-3145-442c-897c-01e87ad16a17', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('6aa59ca4-3145-442c-897c-01e87ad16a17', foundational, prophetic_revelation_is_binding).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('6aa59ca4-3145-442c-897c-01e87ad16a17', prophetic_revelation_is_binding, theological).
narrative_ontology:cs_axiom('6aa59ca4-3145-442c-897c-01e87ad16a17', foundational, salvific_mission_supremacy).
narrative_ontology:cs_axiom_status(salvific_mission_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('6aa59ca4-3145-442c-897c-01e87ad16a17', salvific_mission_supremacy, deontological).
narrative_ontology:cs_reference_frame('6aa59ca4-3145-442c-897c-01e87ad16a17', divine_mandate_through_prophet).
narrative_ontology:cs_drift_state('6aa59ca4-3145-442c-897c-01e87ad16a17', contemporary_church_doctrine, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6aa59ca4-3145-442c-897c-01e87ad16a17', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central governing body of the church. It benefits from the reinterpretation by preserving its legal status, property, and salvific mission, ensuring temple access and missionary work can continue. It enforces the new directive through ecclesiastical courts and excommunication.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, generational, arbitrage, global).

% The individual(s) believed to receive and transmit divine revelation. They delivered the 1890 Manifesto as a legitimate reinterpretation of God's will, guiding the church through a crisis and preserving its mission. Their authority is reinforced by the acceptance of the reinterpretation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_leadership, agenda_setter,
    institutional, biographical, identity_locked, global).

% Members who accepted the reinterpretation and ceased practicing plural marriage. They benefit from continued temple access, participation in missionary work, and social acceptance within the church and broader society. Their compliance is essential for the church's unity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members, beneficiary,
    organized, biographical, constrained, global).

% Members who believed plural marriage was an eternal divine commandment and refused to abandon it, viewing the Manifesto as a capitulation or a false revelation. They bore the cost of excommunication, loss of community, and often formed separate, marginalized groups. Their identity is deeply tied to the original practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members, payer,
    powerless, generational, identity_locked, local).

% The external political entity whose anti-polygamy laws created the crisis. While not directly a party to the internal religious reinterpretation, its coercive power was the catalyst. From this reading's perspective, the government's role was to create the conditions under which God's new will was revealed.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the church's practices and membership around a new prophetic directive, ensuring its legal survival in the United States and preserving its salvific mission (temple ordinances, missionary work).
% TRANSFER_FUNCTION: Transfers the authority to determine the practice of plural marriage from individual members to the prophetic leadership, and transfers social and political legitimacy to the church by aligning its practices with federal law.
% ABSENT_VOICES: Early plural marriage adherents who died before the Manifesto, or those who left the church rather than comply with the new directive. Their perspectives on the legitimacy of the reinterpretation are not part of the mainstream narrative.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its reinterpretation vanished, the church's legal status in the US would be jeopardized, its internal unity would fracture, and its salvific mission (dependent on temple work and missionary efforts) would be fundamentally disrupted. The entire institutional structure would reorganize or collapse.
% FOUNDING_PROBLEM: The existential threat to the church's legal status, property, and the imprisonment of its leaders due to federal anti-polygamy laws, which jeopardized the continuation of its core religious practices and mission.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and contemporary news reports from non-church sources confirm the severe legal and political pressure on the church. The church's own narrative attests to the problem's severity and the divine solution. Fundamentalist groups, however, contest the legitimacy of the solution.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.72, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.72) because fundamentalist members bore the severe cost of excommunication and loss of community for adhering to the prior divine command. `suppression` is very high (0.85) due to the institutional power of excommunication and the social pressure to conform. `theater_ratio` is low (0.18) because, from this reading's perspective, the reinterpretation is a genuine act of revelation, not a performance masking a pragmatic decision. `accessibility_collapse` is high for mainstream members, as continuing plural marriage within the church became impossible. `resistance` is also high, reflecting the enduring opposition from fundamentalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the church institution and mainstream members, the Manifesto was a necessary, divinely guided act of coordination that saved the church. For fundamentalist members, it was a deeply extractive act that forced them to choose between their core beliefs and their community. The engine's classification will highlight this divergence, likely computing a more extractive type for the payer seats than the claimed 'Rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   The `church_institution` and `prophetic_leadership` are clear beneficiaries and agenda-setters, as the reinterpretation secured the church's survival and reinforced their authority. `mainstream_members` are beneficiaries, gaining continued access to temple ordinances and social cohesion. `fundamentalist_members` are the primary targets/victims, facing excommunication and marginalization for non-compliance. The `federal_government` is an external observer whose actions catalyzed the reinterpretation, but is not directly subject to or benefiting from the internal religious constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the reinterpretation as a pure 'Rope' by acknowledging the high extraction and suppression experienced by fundamentalist members. While the church frames it as a necessary coordination for its mission, the structural costs borne by a segment of its members are significant. The 'contested' status of the founding problem further highlights the ongoing debate about the constraint's true function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_causation,
    'Was the 1890 Manifesto primarily an endogenous prophetic reinterpretation, or was it an exogenous override forced by federal coercion?',
    'Analysis of internal church records, prophetic statements, and federal government communications to determine the primary causal driver. This would involve assessing the degree of internal theological justification versus external political pressure.',
    'If primarily exogenous, the constraint''s `suppression` and `extractiveness` would be re-attributed more directly to external state power, and the `claimed_type` might shift from ''Rope'' to ''Snare'' or ''Tangled Rope'' for the church institution itself, as it would be seen as an agent of external coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causation, conceptual, 'Ambiguity regarding the primary cause of the Manifesto: internal revelation vs. external pressure.').

omega_variable(
    salvific_mission_compromise,
    'Did the reinterpretation genuinely preserve the church''s salvific mission, or was the mission itself compromised by the abandonment of a prior divine command?',
    'Theological and historical analysis of the long-term doctrinal coherence and spiritual efficacy of the church''s mission post-Manifesto, particularly from the perspective of those who maintained the original practice.',
    'If the mission was compromised, the ''beneficiary'' status of the church institution would be challenged, and the constraint''s overall justification would weaken, potentially reclassifying it as a ''Snare'' or ''Piton'' if the original purpose was lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salvific_mission_compromise, preference, 'Whether the church''s core mission was truly preserved or fundamentally altered by the reinterpretation.').

omega_variable(
    internalized_suppression_mechanism,
    'Beyond structural excommunication, did the reinterpretation lead to internalized suppression among mainstream members, where dissent became unthinkable due to identity fusion with the church?',
    'Sociological and psychological studies of former and current members, analyzing post-exit trajectories and self-reported experiences of cognitive dissonance or identity conflict. If suppression persists after structural barriers are removed, it indicates internalization.',
    'If internalized suppression is significant, the effective `suppression` for mainstream members is higher than the structural measure suggests, making their ''constrained'' exit options more akin to ''identity_locked'' and amplifying the constraint''s effective extraction from them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for mainstream members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.15).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement(plur_tr_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.16).
narrative_ontology:measurement(plur_tr_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement(plur_tr_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.65).
narrative_ontology:measurement(plur_be_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(plur_be_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(plur_be_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 2020, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.78).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(plur_su_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.82).
narrative_ontology:measurement(plur_su_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1980, 0.83).
narrative_ontology:measurement(plur_su_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'plural_marriage_mandate' kernel. Each reading presents a distinct structural claim about the 1890 Manifesto and its implications for the church's authority and practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
