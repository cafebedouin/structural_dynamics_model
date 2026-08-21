% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture (Traditionalist Reading)
 *   domain: Ecclesiology/Institutional History/Hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture_traditionalist_reading'
 *   of Vatican II doctrinal authority. From this perspective, the Second
 *   Vatican Council (1962-1965) represents a fundamental break with perennial
 *   Catholic tradition. Its documents are seen as containing ambiguities and
 *   errors that enabled a heterodox implementation in the post-conciliar era,
 *   leading to a loss of doctrinal clarity, liturgical reverence, and
 *   missionary zeal. The constraint's high extractiveness reflects the
 *   perceived loss of traditional Catholic identity and practice for those
 *   who adhere to it.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: Agenda-setter (institutional/constrained) — enforces the new direction
 *   - traditional_catholics: Payer (powerless/identity_locked) — bear the costs of perceived rupture
 *   - traditional_liturgy: Payer (non-agent/trapped) — suppressed and marginalized
 *   - doctrinal_clarity: Payer (non-agent/trapped) — eroded by ambiguities
 *   - missionary_zeal: Payer (non-agent/trapped) — weakened by new approaches
 *   - progressive_theologians: Beneficiary (powerful/mobile) — benefit from new interpretive freedom
 *   - conciliar_documents: Observer (non-agent/analytical) — the kernel itself, subject to interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority: Rupture (Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Ecclesiology/Institutional History/Hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '7377f92b-ac6f-48c1-bc95-f26669c1eaa7').
narrative_ontology:cs_kernel_codification('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', fixed_text).
narrative_ontology:cs_authority_grounding('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', lineage).
narrative_ontology:cs_interpretation_layer_present('7377f92b-ac6f-48c1-bc95-f26669c1eaa7').
narrative_ontology:cs_reading_relation('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', foundational, doctrinal_immutability_of_tradition).
narrative_ontology:cs_axiom_status(doctrinal_immutability_of_tradition, holdable).
narrative_ontology:cs_axiom_grounding('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', doctrinal_immutability_of_tradition, deontological).
narrative_ontology:cs_axiom('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', foundational, conciliar_documents_contain_ambiguities_and_errors).
narrative_ontology:cs_axiom_status(conciliar_documents_contain_ambiguities_and_errors, holdable).
narrative_ontology:cs_axiom_grounding('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', conciliar_documents_contain_ambiguities_and_errors, empirically_contingent).
narrative_ontology:cs_reference_frame('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', pre_conciliar_doctrinal_clarity).
narrative_ontology:cs_drift_state('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', contemporary_post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7377f92b-ac6f-48c1-bc95-f26669c1eaa7', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the conciliar documents and their subsequent interpretations, which are seen by traditionalists as a deviation from perennial tradition. They benefit from the expanded interpretive authority and the ability to steer the Church in new directions.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Bear the cost of perceived doctrinal ambiguity, liturgical changes, and the suppression of traditional practices. Their identity is deeply tied to pre-conciliar Catholicism, making exit from the Church unthinkable, but they feel alienated and marginalized within it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics, payer,
    powerless, biographical, identity_locked, global).

% Represents the forms of worship and ritual that traditionalists see as suppressed or replaced by new, less sacred forms. Its 'costs' are its marginalization and the loss of its central place in Catholic life.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).

% Represents the perceived erosion of clear, unambiguous theological statements due to the ambiguities and perceived errors in the conciliar documents and their subsequent interpretations. Its 'costs' are the confusion and relativism that traditionalists attribute to this shift.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).

% Represents the perceived decline in the Church's evangelistic fervor, attributed by traditionalists to ecumenical approaches that dilute the unique truth claims of Catholicism. Its 'costs' are the lost souls and the weakening of the Church's external mission.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

% Benefit from the perceived opening and flexibility of the Council, which they interpret as authorizing new theological explorations and adaptations to modernity. They gain influence and academic freedom from this shift.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians, beneficiary,
    powerful, biographical, mobile, global).

% The texts themselves, which traditionalists interpret as containing ambiguities and errors that enabled the post-conciliar rupture. They are the kernel around which the entire interpretive contest revolves.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_documents, observer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_documents).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Council's stated intent, it aimed to update the Church's self-understanding and engagement with the modern world, fostering Christian unity and renewing its mission. Traditionalists, however, view this as a flawed coordination that led to rupture.
% TRANSFER_FUNCTION: Transfers interpretive authority from the clear, consistent tradition of the past to a new, ambiguous magisterial interpretation. It also shifts liturgical practice from traditional forms to new ones, and theological emphasis from clear dogma to pastoral flexibility.
% ABSENT_VOICES: Pre-conciliar theologians, saints, and the 'sensus fidelium' (sense of the faithful) of previous eras, whose clear doctrinal positions and liturgical preferences are seen as implicitly rejected or sidelined by the Council's direction.
% DISAPPEARANCE_RATIONALE: If the Vatican II documents and their post-conciliar interpretation vanished overnight, the Catholic Church would revert to pre-conciliar forms and doctrines. The current theological landscape, liturgical practices, and ecumenical efforts would be fundamentally altered, leading to a complete reorganization of Catholic life and thought.
% FOUNDING_PROBLEM: To address the challenges of modernity, foster Christian unity, and renew the Church's mission in the mid-20th century.
% FOUNDING_PROBLEM_CORROBORATION: The post-conciliar magisterium and progressive theologians attest that the founding problems were live and the Council successfully addressed them. Traditionalists, however, argue that the Council either misidentified the problems or introduced new, more severe ones, with corroboration from historical analysis of pre- and post-conciliar trends, and the ongoing decline in certain metrics of Catholic practice.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the profound sense of loss and alienation experienced by traditionalists due to the perceived rupture. Suppression (0.70) is high because traditionalist expressions and criticisms are actively discouraged or restricted by the institutional Church. The moderate theater ratio (0.40) indicates that while some traditional forms or language may be maintained, their underlying substance is seen as having been eroded or reinterpreted in a way that is not genuinely continuous with the past. The rising trends in extractiveness, suppression, and theater over the interval reflect the increasing perceived deviation from tradition and the hardening of institutional enforcement against traditionalist dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist perspective, the Council and its aftermath are a snare, trapping the faithful in heterodoxy. The post-conciliar magisterium, however, officially maintains a 'hermeneutic of continuity,' viewing the Council as an organic development and a rope for renewal. The engine's computation of per-seat classifications will highlight this divergence, showing the same constraint as extractive for traditionalists and as coordination for the magisterium.
 *
 * DIRECTIONALITY LOGIC:
 *   The post_conciliar_magisterium and progressive_theologians are beneficiaries (low d) as they gain interpretive authority and freedom from the Council's direction. Traditional_catholics, traditional_liturgy, doctrinal_clarity, and missionary_zeal are targets (high d) as they bear the costs of perceived rupture and suppression. Traditional Catholics are identity_locked, as their faith prevents them from leaving the Church despite their profound disagreement with its current direction.
 *
 * MANDATROPHY ANALYSIS:
 *   From this traditionalist reading, the Council's original mandate for renewal and engagement with the modern world has been subverted. Instead of genuine renewal, the outcome is perceived as a rupture that extracts from tradition and suppresses those who uphold it. The constraint persists not because its original coordination function is still genuinely served for all parties, but because the agenda-setters benefit from the new interpretive authority and actively enforce the new direction, while victims are identity_locked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_ambiguity_vs_clarity,
    'Are the ambiguities in the Vatican II documents intentional pastoral flexibility, or are they errors that enable heterodox interpretations?',
    'Further magisterial clarifications that explicitly affirm or reject specific traditionalist concerns, or a future Council that definitively re-evaluates Vatican II''s texts.',
    'If deemed intentional flexibility, the extractiveness for doctrinal clarity might decrease. If deemed errors, the traditionalist reading of rupture would be strengthened, potentially increasing perceived extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_vs_clarity, conceptual, 'Whether conciliar ambiguities are pastoral or erroneous.').

omega_variable(
    legitimacy_of_post_conciliar_magisterium,
    'Is the post-conciliar magisterium a legitimate interpreter of tradition, or has it departed from it in a way that invalidates its authority?',
    'A future, universally recognized ecumenical council that either reaffirms the post-conciliar magisterium''s continuity or formally corrects its perceived deviations.',
    'If its legitimacy is affirmed by traditionalists, the perceived suppression and extractiveness would decrease. If its departure is widely recognized, the constraint would be seen as a more severe snare, with its authority resting purely on coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_post_conciliar_magisterium, conceptual, 'The legitimacy of the post-conciliar magisterium''s interpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist views primarily structural (official decrees, institutional marginalization) or internalized (social pressure, fear of ostracization)?',
    'Analysis of traditionalist communities in contexts where official restrictions are lifted: if traditionalist views still struggle to gain traction, internalized suppression is more significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measures suggest, as traditionalists carry the suppression with them even in less restrictive environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. It represents the traditionalist perspective that the Council constitutes a rupture with prior tradition, with its documents containing ambiguities and errors that enabled heterodox implementation. Its ε value differs significantly from the 'continuity_reading' and 'rupture_progressive_reading' due to its negative valuation of the changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
