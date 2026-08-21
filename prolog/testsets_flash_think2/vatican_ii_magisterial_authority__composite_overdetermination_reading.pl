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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority (Composite Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story analyzes Vatican II not as a singular
 *   reinterpretation, but as an overdetermined composite encoding
 *   incompatible ecclesiological visions through ambiguous compromise
 *   formulations. The conciliar texts were designed to achieve supermajority
 *   votes by simultaneously supporting both continuity and rupture readings.
 *   This structural ambiguity means that hermeneutical control becomes the
 *   real locus of authority, and implementation divergence is a structural
 *   feature, not a bug. The 10-12% rejection votes on key documents signal
 *   unresolved theological incompatibility embedded in the final texts. The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function (maintaining institutional unity) but does so
 *   through asymmetric extraction of interpretive freedom and intellectual
 *   honesty from various factions.
 *
 * KEY AGENTS:
 *   - Magisterium: Primary agenda-setter (institutional/constrained) — benefits from interpretive control.
 *   - Theologians: Primary payer (moderate/identity_locked) — bear costs of navigating ambiguity and potential suppression.
 *   - Dissenting Clergy: Payer (moderate/identity_locked) — face career repercussions for non-conforming interpretations.
 *   - Laity Seeking Clarity: Payer (powerless/constrained) — bear costs of doctrinal confusion.
 *   - Institutional Church: Beneficiary (institutional/constrained) — benefits from perceived unity and continued authority.
 *   - Traditionalist Factions: Excluded (organized/constrained) — marginalized for rejecting aspects of the Council.
 *   - Progressive Factions: Excluded (organized/constrained) — marginalized for advocating radical interpretations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.78).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.85).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '21944ebe-7a8c-493d-8c9c-7b9d91203d2d').
narrative_ontology:cs_kernel_codification('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', fixed_text).
narrative_ontology:cs_authority_grounding('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', extraction).
narrative_ontology:cs_interpretation_layer_present('21944ebe-7a8c-493d-8c9c-7b9d91203d2d').
narrative_ontology:cs_reading_relation('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', secondary, hermeneutical_control_is_locus_of_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', hermeneutical_control_is_locus_of_authority, conventional).
narrative_ontology:cs_reference_frame('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', inherent_conciliar_ambiguity).
narrative_ontology:cs_drift_state('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', post_conciliar_hermeneutical_struggle, gap(stable, minor, false)).
narrative_ontology:cs_created_at('21944ebe-7a8c-493d-8c9c-7b9d91203d2d', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_church).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, dissenting_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity_seeking_clarity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which interprets and enforces the meaning of Vatican II texts. Benefits from the ambiguity by selectively emphasizing interpretations to maintain control and perceived unity, extracting compliance from various factions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).

% The broader institutional structure of the Catholic Church. Benefits from the continued functioning of the magisterial authority and the avoidance of overt schism, even if this comes at the cost of clarity and intellectual honesty.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_church, beneficiary,
    institutional, generational, constrained, global).

% Scholars dedicated to the study of religious doctrine. Bear the cost of navigating ambiguous texts, often facing pressure to conform their interpretations to official lines, risking professional marginalization or censure for dissent.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theologians, payer,
    moderate, biographical, identity_locked, global).

% Clergy members who hold interpretations of Vatican II that diverge from the official magisterial line. Face career repercussions, loss of assignments, or public reprimand for challenging established hermeneutics.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, dissenting_clergy, payer,
    moderate, biographical, identity_locked, global).

% Lay members of the Church who desire clear, unambiguous doctrinal and pastoral guidance. Bear the cost of doctrinal confusion, internal inconsistency, and the ongoing hermeneutical struggle, which can lead to disengagement or disillusionment.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity_seeking_clarity, payer,
    powerless, biographical, constrained, global).

% Groups within the Church who reject aspects of Vatican II or its post-conciliar implementation, often viewing it as a rupture with tradition. Their interpretations are marginalized or suppressed, and they are often excluded from mainstream discourse.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions, excluded,
    organized, generational, constrained, global).

% Groups within the Church who advocate for more radical interpretations of Vatican II, seeking greater change and adaptation. Their interpretations are often deemed heterodox or premature, leading to their marginalization from official influence.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_factions, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintained a fragile institutional unity within the Catholic Church after a period of intense theological debate, allowing for supermajority votes on conciliar texts by encoding diverse, sometimes incompatible, theological visions.
% TRANSFER_FUNCTION: Transfers interpretive authority and intellectual labor from theologians and dissenting factions to the central magisterium, in exchange for a semblance of institutional unity and the avoidance of overt schism.
% ABSENT_VOICES: Those who sought clear, unambiguous doctrinal statements, either traditionalists or progressives, whose positions were diluted or obscured in the compromise texts. Their voices are present in ongoing debates but lack official recognition or influence.
% DISAPPEARANCE_RATIONALE: If the overdetermined nature of Vatican II texts were universally acknowledged and the magisterium's interpretive control mechanism vanished, the institutional Church would face immediate and profound schism. Incompatible ecclesiological visions, currently held in tension by ambiguous formulations, would erupt into open conflict, forcing a definitive (and likely divisive) resolution.
% FOUNDING_PROBLEM: To modernize the Catholic Church and address contemporary challenges while maintaining doctrinal continuity, requiring broad consensus among diverse theological factions at the Council.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Council, independent theologians, and sociological studies of Catholic identity corroborate the ongoing tension and the strategic ambiguity of the texts. The problem of reconciling tradition with modernity, and maintaining unity amidst diverse theological views, remains central to the Church's ongoing life.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is high (0.78) because the magisterium leverages the textual ambiguity to demand intellectual conformity and suppress dissenting views, effectively extracting interpretive labor and freedom. Suppression is very high (0.85) due to the institutional power to censure, marginalize, and control theological discourse, making genuine alternatives to official hermeneutics difficult to sustain within the Church. Theater ratio is moderate-high (0.60) as much of the 'unity' and 'organic development' narrative is performative, masking deep and unresolved internal tensions that are structurally embedded in the conciliar texts. The measurement series track the intensification of this dynamic over the post-conciliar period.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's perspective, the constraint is a necessary mechanism for maintaining unity and continuity, a legitimate exercise of authority. From the perspective of theologians and dissenting clergy, it is an extractive mechanism that stifles intellectual inquiry and forces conformity to an often inconsistent interpretive line. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium and the institutional Church are beneficiaries, as they maintain authority and institutional stability through interpretive control. Theologians, dissenting clergy, and laity seeking clarity are targets, as they bear the costs of ambiguity, suppression, and intellectual compromise. Traditionalist and progressive factions are excluded, as their interpretations are actively marginalized by the dominant hermeneutic.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the post-conciliar interpretive struggle as pure coordination. While the constraint did coordinate a fragile unity, its persistence relies heavily on active enforcement of a particular hermeneutic, suppressing genuine alternatives and extracting intellectual conformity. The high extractiveness and suppression, coupled with the contested status of the founding problem, indicate that the coordination function is deeply intertwined with, and perhaps overshadowed by, an extractive dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_vatican_ii_reading,
    'Is this constraint a true representation of the ''composite overdetermination'' reading of Vatican II magisterial authority, or is it better captured by a ''continuity'' or ''rupture'' reading?',
    'Further historical and theological analysis of conciliar drafting processes, post-conciliar reception, and magisterial documents, specifically focusing on evidence of intentional ambiguity and internal contradictions.',
    'If a ''continuity'' reading were adopted, extractiveness and suppression would likely be lower, and the constraint might classify as a Rope or Scaffold. If a ''rupture'' reading were adopted, the constraint might be seen as a Snare, with the magisterium enforcing a new, incompatible doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_vatican_ii_reading, conceptual, 'This constraint is one reading of the ''vatican_ii_magisterial_authority'' kernel. Sibling readings (''continuity_reading'', ''rupture_reading'') would structurally alter the constraint''s perceived function and impact.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dissenting interpretations structural (institutional power, career paths) or internalized (theological obedience, self-censorship)?',
    'Sociological studies of theologians and clergy post-Vatican II, examining the persistence of self-censorship even in contexts with reduced direct institutional pressure, or the psychological impact of identity-locked positions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the extractive nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in theological discourse.').

omega_variable(
    ambiguity_as_feature_or_bug,
    'Is the inherent ambiguity of the Vatican II texts a structural feature (designed to allow for diverse reception and gradual integration) or a bug (leading to perpetual hermeneutical conflict and institutional paralysis)?',
    'Longitudinal studies of the Church''s adaptability and internal coherence in response to external challenges, comparing outcomes in areas where ambiguity was embraced versus areas where clarity was enforced.',
    'If ambiguity is a feature, the coordination function is more robust, and the extractiveness might be seen as a necessary cost of managing diversity. If it''s a bug, the extractiveness is purely parasitic, and the coordination function is failing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, conceptual, 'Whether conciliar ambiguity is a functional design choice or a source of dysfunction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1965, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.5).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.58).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.59).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.6).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.71).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1980, 0.74).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.77).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.83).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.84).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_magisterial_authority' kernel. The 'continuity_reading' and 'rupture_reading' are sibling constraints, each with distinct ε values and structural properties, linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
