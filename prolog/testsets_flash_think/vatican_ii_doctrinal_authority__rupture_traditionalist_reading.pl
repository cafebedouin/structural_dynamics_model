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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Rupture (Traditionalist Reading)
 *   domain: Ecclesiology / Institutional History / Hermeneutics
 *
 * SUMMARY:
 *   This constraint story represents the 'rupture traditionalist' reading of
 *   Vatican II, where the Council's documents are seen as containing
 *   ambiguities and errors that enabled a heterodox implementation, leading
 *   to a rupture with prior Catholic tradition. From this perspective, the
 *   post-conciliar period has been characterized by a substantial extraction
 *   of traditional liturgical and doctrinal forms, enforced by institutional
 *   actors who benefit from the new theological landscape. The claimed type
 *   is 'snare' because of the high extraction, identifiable victims, and
 *   active suppression of alternatives, despite the institutional Church's
 *   official narrative of 'continuity'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Rupture (Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Ecclesiology / Institutional History / Hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'a357786a-5c80-4c5e-86bd-e0d532b6625e').
narrative_ontology:cs_kernel_codification('a357786a-5c80-4c5e-86bd-e0d532b6625e', fixed_text).
narrative_ontology:cs_authority_grounding('a357786a-5c80-4c5e-86bd-e0d532b6625e', lineage).
narrative_ontology:cs_interpretation_layer_present('a357786a-5c80-4c5e-86bd-e0d532b6625e').
narrative_ontology:cs_reading_relation('a357786a-5c80-4c5e-86bd-e0d532b6625e', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a357786a-5c80-4c5e-86bd-e0d532b6625e', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('a357786a-5c80-4c5e-86bd-e0d532b6625e', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('a357786a-5c80-4c5e-86bd-e0d532b6625e', foundational, doctrinal_immutability_paramount).
narrative_ontology:cs_axiom_status(doctrinal_immutability_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a357786a-5c80-4c5e-86bd-e0d532b6625e', doctrinal_immutability_paramount, deontological).
narrative_ontology:cs_axiom('a357786a-5c80-4c5e-86bd-e0d532b6625e', foundational, ambiguity_in_conciliar_texts_is_defect).
narrative_ontology:cs_axiom_status(ambiguity_in_conciliar_texts_is_defect, holdable).
narrative_ontology:cs_axiom_grounding('a357786a-5c80-4c5e-86bd-e0d532b6625e', ambiguity_in_conciliar_texts_is_defect, conventional).
narrative_ontology:cs_reference_frame('a357786a-5c80-4c5e-86bd-e0d532b6625e', pre_conciliar_tradition).
narrative_ontology:cs_drift_state('a357786a-5c80-4c5e-86bd-e0d532b6625e', post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('a357786a-5c80-4c5e-86bd-e0d532b6625e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_clergy).
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

% Adhere to pre-conciliar doctrines and liturgical practices, viewing Vatican II as a rupture with tradition. They experience the post-conciliar changes as an extraction of their spiritual heritage and face institutional pressure to conform to new norms. Their identity is deeply tied to the traditional faith, making exit unthinkable.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics, payer,
    moderate, biographical, identity_locked, global).

% Interpret Vatican II as a necessary and positive break with past rigidity, enabling new theological developments. They benefit from the intellectual space and institutional support for their interpretations, which are seen as vindicated by the Council's spirit.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians, beneficiary,
    powerful, generational, mobile, global).

% Implement the post-conciliar reforms, often interpreting the Council's ambiguities in a way that aligns with progressive views. They benefit from the institutional power to shape the Church's direction and often suppress traditionalist dissent, viewing it as disloyal or backward.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_clergy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_clergy, beneficiary).

% The central administrative body of the Catholic Church, responsible for interpreting and enforcing conciliar documents. From this reading's perspective, the Curia often enables or fails to correct heterodox implementations, thereby perpetuating the rupture, while officially maintaining a narrative of continuity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_curia, agenda_setter,
    institutional, generational, constrained, global).

% The pre-conciliar forms of worship and sacraments, which traditionalists see as having been suppressed or marginalized by post-conciliar reforms. Its continued existence is often subject to severe restrictions and institutional disfavor.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).

% The unambiguous and consistent teaching of Catholic doctrine, which traditionalists believe was eroded by the Council's ambiguities and subsequent interpretations. Its perceived loss is a core victim of the rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).

% The fervent desire and effort to convert non-Catholics, which traditionalists argue declined significantly in the post-conciliar era due to new ecumenical approaches and a perceived loss of confidence in the Church's unique truth claims.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

% Scholars who study the historical context, documents, and implementation of Vatican II, often from a secular or non-partisan perspective. They can identify patterns of change and contestation without necessarily endorsing any particular theological reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, analytical_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the Council aimed to coordinate the Church's relationship with the modern world and foster ecumenical dialogue, but in doing so, it failed to coordinate internal doctrinal fidelity and traditional practice, leading to disunity and confusion.
% TRANSFER_FUNCTION: Transfers authority and legitimacy from established traditional doctrines and practices to novel interpretations and liturgical forms. It also transfers spiritual and material resources away from traditionalist communities towards those aligned with the post-conciliar direction.
% ABSENT_VOICES: The voices of pre-conciliar popes, theologians, and saints whose teachings are seen as implicitly contradicted or marginalized by the Council's documents and subsequent implementation. Their traditional understanding of the faith is effectively excluded from the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the documents and implementation of Vatican II vanished overnight, the Catholic Church would undergo a profound reorganization. Traditional doctrines and liturgical practices would likely reassert themselves, or a new, explicitly traditionalist council would emerge to restore what is perceived as lost, fundamentally altering the Church's current trajectory.
% FOUNDING_PROBLEM: To address the Church's relationship with the modern world, promote Christian unity (ecumenism), and renew liturgical practices, in response to perceived stagnation and external challenges.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and institutional authorities attest that the founding problems were live and the Council provided necessary solutions. Traditionalist scholars and lay movements, however, attest that the Council misdiagnosed the problems or introduced new, more severe ones, and that the original problems could have been addressed without rupture. This contestation is evident in ongoing theological debates and institutional tensions.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the perceived loss of traditional forms and doctrinal clarity. Suppression (0.78) is high due to active institutional measures to marginalize traditionalist practices and voices. The theater ratio is low (0.15) because, from this reading, the changes are genuinely substantive and harmful, not merely performative. Resistance is high (0.80) as traditionalist movements actively oppose the perceived rupture. The increasing extractiveness and suppression over time reflect the deepening of the perceived rupture and the intensification of efforts to enforce the post-conciliar norms.
 *
 * PERSPECTIVAL GAP:
 *   The 'rupture traditionalist' reading fundamentally diverges from the 'continuity' and 'rupture progressive' readings. While the institutional Church officially maintains a 'continuity' narrative, traditionalists experience the constraint as a snare, extracting from their spiritual heritage. Progressive actors, conversely, experience it as a liberating 'rope' or 'scaffold'. The engine's classification will highlight this divergence by computing a snare from the traditionalist seat, even if the claimed type is different in other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional Catholics, traditional liturgy, doctrinal clarity, and missionary zeal are the primary targets/victims (high d), bearing the costs of the perceived rupture. Progressive theologians and modernist clergy are beneficiaries/agenda-setters (low d), gaining influence and institutional power from the new theological and liturgical landscape. The Vatican Curia, while officially upholding continuity, is seen by this reading as an agenda-setter whose actions (or inactions) enable the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_of_conciliar_texts,
    'Are the ambiguities in Vatican II documents genuinely unintentional errors or intentional pastoral openness to diverse interpretations?',
    'Detailed textual analysis comparing conciliar drafts and debates, alongside a study of the theological method employed by the Council Fathers, to discern authorial intent regarding specific ambiguous passages.',
    'If unintentional errors, it strengthens the ''rupture'' claim by highlighting flaws in the documents themselves. If intentional openness, it shifts the blame for heterodoxy to post-conciliar interpreters, but still validates the ''rupture'' from a traditionalist perspective that values clarity over openness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_of_conciliar_texts, conceptual, 'Nature of ambiguity in Vatican II documents.').

omega_variable(
    causal_link_documents_implementation,
    'To what extent did the ambiguities and perceived errors in the Vatican II documents directly cause heterodox post-conciliar implementation, versus being merely coincidental with pre-existing theological currents?',
    'Historical-theological studies tracing specific heterodox developments back to particular conciliar texts, alongside counterfactual analysis exploring how different textual formulations might have altered outcomes.',
    'Stronger causal link reinforces the ''rupture'' claim as inherent to the Council itself. Weaker link suggests the Council was hijacked by external forces, potentially mitigating the direct culpability of the documents but not necessarily the fact of rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_documents_implementation, empirical, 'Causality between conciliar texts and post-conciliar implementation.').

omega_variable(
    suppression_mechanism_traditionalists,
    'Is the suppression of traditionalist views primarily structural (institutional policies, liturgical restrictions) or internalized (social pressure, fear of ostracization within Catholic communities)?',
    'Sociological studies of traditionalist communities'' experiences, analysis of institutional directives, and interviews with individuals who have navigated both traditional and post-conciliar environments. Post-exit trajectory of traditionalist communities.',
    'If primarily structural, the constraint''s effective suppression is directly tied to institutional power. If significantly internalized, the suppression is more pervasive and harder to address, as it persists even in the absence of explicit external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_traditionalists, empirical, 'Structural vs. internalized suppression of traditionalist views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1988, 0.14).
narrative_ontology:measurement(vati_tr_t2001, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(vati_tr_t2014, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1988, 0.78).
narrative_ontology:measurement(vati_be_t2001, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(vati_be_t2014, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1988, 0.72).
narrative_ontology:measurement(vati_su_t2001, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(vati_su_t2014, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2014, 0.77).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catholic_liturgical_norms).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catholic_moral_theology_development).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel, each representing a distinct structural claim about the Council's nature and impact. This 'rupture traditionalist' reading focuses on the perceived break with tradition and the negative consequences of conciliar ambiguities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
