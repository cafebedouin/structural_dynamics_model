% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models the 'rupture reading' of Vatican II, which asserts
 *   a fundamental break with pre-conciliar Catholic teaching and practice.
 *   The conciliar texts are interpreted as authorizing radical
 *   implementation, superseding prior positions (e.g., 'error has no
 *   rights'), legitimizing liturgical experimentation, and acknowledging
 *   religious freedom (Dignitatis Humanae) as a doctrinal progress that
 *   contradicts prior teaching. This reading is actively promoted by
 *   progressive theologians and implemented by liberal clergy, while
 *   traditionalist and conservative elements bear the costs of perceived
 *   doctrinal discontinuity and loss of tradition. The constraint is claimed
 *   as a 'rope' by its proponents, framing it as necessary adaptation, but
 *   its metrics reflect significant extraction and suppression for those who
 *   resist the rupture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '5d9a7115-7023-494c-a6ef-08a0ab2343b1').
narrative_ontology:cs_kernel_codification('5d9a7115-7023-494c-a6ef-08a0ab2343b1', fixed_text).
narrative_ontology:cs_authority_grounding('5d9a7115-7023-494c-a6ef-08a0ab2343b1', lineage).
narrative_ontology:cs_interpretation_layer_present('5d9a7115-7023-494c-a6ef-08a0ab2343b1').
narrative_ontology:cs_reading_relation('5d9a7115-7023-494c-a6ef-08a0ab2343b1', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5d9a7115-7023-494c-a6ef-08a0ab2343b1', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('5d9a7115-7023-494c-a6ef-08a0ab2343b1', foundational, doctrinal_progress_through_contradiction).
narrative_ontology:cs_axiom_status(doctrinal_progress_through_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('5d9a7115-7023-494c-a6ef-08a0ab2343b1', doctrinal_progress_through_contradiction, deontological).
narrative_ontology:cs_axiom('5d9a7115-7023-494c-a6ef-08a0ab2343b1', foundational, conciliar_texts_authorize_radical_implementation).
narrative_ontology:cs_axiom_status(conciliar_texts_authorize_radical_implementation, holdable).
narrative_ontology:cs_axiom_grounding('5d9a7115-7023-494c-a6ef-08a0ab2343b1', conciliar_texts_authorize_radical_implementation, conventional).
narrative_ontology:cs_reference_frame('5d9a7115-7023-494c-a6ef-08a0ab2343b1', post_conciliar_progressive_theology).
narrative_ontology:cs_drift_state('5d9a7115-7023-494c-a6ef-08a0ab2343b1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5d9a7115-7023-494c-a6ef-08a0ab2343b1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liberal_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, conservative_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the rupture reading as it legitimizes their theological innovations and pastoral practices, allowing them to move beyond what they perceive as rigid pre-conciliar doctrines. They actively promote this interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, global).

% Implement the rupture reading through liturgical changes, catechetical reforms, and pastoral initiatives. They see it as a necessary adaptation to the modern world, but face resistance from traditionalist elements within their dioceses.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liberal_clergy, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of the rupture reading, experiencing a loss of familiar liturgical forms and doctrinal clarity. They feel alienated from the mainstream Church, but their deep identity as Catholics makes formal exit unthinkable, leading to internal resistance or seeking out traditionalist enclaves.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics, payer,
    powerless, generational, identity_locked, local).

% Are forced to navigate a Church shaped by the rupture reading, often feeling their authority undermined or their theological positions marginalized. They may attempt to subtly resist or mitigate the effects of the rupture, but direct opposition carries career risks.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conservative_clergy, payer,
    moderate, biographical, constrained, national).

% The formal teaching authority that promulgates and interprets the texts of Vatican II. While officially promoting a 'hermeneutic of reform in continuity,' the rupture reading asserts that the practical implementation and doctrinal shifts authorized by the Council represent a de facto break, which the Magisterium must then manage or implicitly endorse through inaction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, magisterium_of_the_church, agenda_setter,
    institutional, civilizational, constrained, universal).

% Analyze the internal dynamics of the Catholic Church, often interpreting Vatican II as a modernization effort that brought the Church into conflict with its own past. They are not subject to the constraint but provide external commentary on its effects.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, secular_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's adaptation to the modern world, allowing for theological development, liturgical reform, and engagement with contemporary society, thereby preventing irrelevance or schism from external pressures.
% TRANSFER_FUNCTION: Transfers doctrinal authority and interpretive flexibility from a rigid, pre-conciliar framework to a more dynamic, progressive one, empowering certain theological schools and disempowering others, while also shifting the burden of adaptation onto traditionalist adherents.
% ABSENT_VOICES: Pre-conciliar theologians and their adherents, whose positions are deemed superseded by the rupture reading, are effectively silenced or marginalized within official discourse. Their arguments for the immutability of certain doctrines are dismissed as incompatible with the Council's spirit.
% DISAPPEARANCE_RATIONALE: If the rupture reading of Vatican II vanished, the theological and pastoral landscape of the Catholic Church would fundamentally reorganize. Progressive movements would lose their primary legitimizing framework, traditionalist positions would gain significant ground, and the internal conflicts over the Council's legacy would intensify, potentially leading to new schisms or a re-evaluation of the Council's authority itself.
% FOUNDING_PROBLEM: The Catholic Church faced increasing irrelevance and alienation from the modern world, perceived as anachronistic and resistant to necessary social and intellectual developments, leading to a crisis of engagement and declining influence.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many secular historians corroborate the problem of pre-conciliar irrelevance. Traditionalist critics, however, argue the 'problem' was a misdiagnosis and the 'solution' created new, deeper crises within the Church, making the status of the founding problem contested.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the rupture reading imposes significant costs on those who adhere to pre-conciliar traditions, forcing them to accept changes they view as illegitimate or to marginalize themselves. Suppression (0.70) is also high, as dissent from the rupture reading is often met with institutional pressure, marginalization, or accusations of disloyalty. Theater ratio (0.20) is moderate; while there is genuine theological work, a portion of the effort goes into maintaining the narrative of 'progress' against internal resistance. Accessibility collapse (0.40) is moderate, as alternatives (e.g., traditionalist groups) exist but are constrained. Resistance (0.75) is high, reflecting ongoing, often intense, opposition from traditionalist and conservative factions.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of the rupture reading (progressive theologians, liberal clergy) experience it as a liberating 'rope' that allows the Church to breathe and engage with the modern world. For traditionalist Catholics and conservative clergy, the same constraint operates as a 'snare' or 'tangled rope,' extracting their spiritual and cultural heritage and suppressing their preferred forms of worship and belief. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and liberal clergy are beneficiaries and agenda-setters, as the rupture reading legitimizes their work and empowers their initiatives. Traditionalist Catholics and conservative clergy are payers, bearing the costs of doctrinal and liturgical shifts. The Magisterium, while officially promoting continuity, is implicitly an agenda-setter for the rupture reading by allowing its implementation and not decisively condemning its more radical interpretations. Secular observers are analytical, not directly affected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_contradiction_status,
    'Is the contradiction between Dignitatis Humanae (religious freedom) and prior papal teaching (e.g., Syllabus of Errors) a genuine doctrinal rupture or a development that can be reconciled?',
    'Further magisterial clarification explicitly addressing the alleged contradiction, or a widely accepted theological synthesis that demonstrates continuity without equivocation.',
    'If a genuine rupture, it strengthens the rupture reading''s claim of a fundamental break. If reconcilable, it weakens the rupture reading and lends support to the continuity reading, potentially reclassifying this constraint as a ''tangled rope'' with less extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_contradiction_status, conceptual, 'Ambiguity regarding doctrinal continuity on religious freedom.').

omega_variable(
    liturgical_reform_legitimacy,
    'To what extent was the post-conciliar liturgical reform (Novus Ordo) a legitimate development authorized by Sacrosanctum Concilium, versus an unauthorized rupture with traditional liturgical forms?',
    'Historical-liturgical scholarship demonstrating clear lineage or clear break, and/or future magisterial pronouncements on the binding nature and interpretation of liturgical norms.',
    'If largely unauthorized, it would increase the ''theater_ratio'' and ''extractiveness'' of the rupture reading, as its implementation would be seen as more performative than functional. If fully authorized, it would reduce perceived extraction for those who accept the reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_reform_legitimacy, empirical, 'Legitimacy of post-conciliar liturgical changes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditionalist views structural (institutional policies, career paths) or internalized (social pressure, fear of being labeled disloyal)?',
    'Post-exit suppression trajectory: if traditionalist views persist and flourish after individuals leave mainstream institutional structures, reclassify as partially internalized. If suppression only lifts with institutional change, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — traditionalists carry the suppression with them after exit. If purely structural, institutional reforms could more easily alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Vatican II magisterial authority kernel. The rupture reading asserts a fundamental break with pre-conciliar teaching, in contrast to the continuity reading and the composite overdetermination reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
