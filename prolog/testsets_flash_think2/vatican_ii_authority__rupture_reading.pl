% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II as Rupture: Doctrinal Errors and Crisis
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture reading' of Vatican II,
 *   which posits that the Council represents a substantive break with prior
 *   Catholic tradition, introducing doctrinal errors or irreconcilable
 *   contradictions. From this perspective, the post-conciliar Church is in a
 *   state of crisis, and the changes primarily benefit a 'modernist faction'
 *   at the expense of traditional Catholic identity and doctrinal stability.
 *   This reading is often associated with traditionalist groups like the
 *   SSPX.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: Agenda-setter (institutional/constrained) — enforces the new interpretation
 *   - modernist_faction: Beneficiary (powerful/mobile) — benefits from the new interpretive framework
 *   - traditionalist_clergy_laity: Payer (powerless/identity_locked) — bears the cost of doctrinal instability and marginalization
 *   - sspx_members: Payer/Excluded (organized/identity_locked) — formally excluded but maintains traditional identity
 *   - academic_historians_theologians: Observer (analytical/analytical) — analyzes the dynamics without direct allegiance
 *   - continuity_theologians: Excluded (powerful/constrained) — their arguments for continuity are seen as papering over contradictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.85).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Rupture: Doctrinal Errors and Crisis").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'fde9905c-c473-4fd9-8a92-40f5dd8f36ed').
narrative_ontology:cs_kernel_codification('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', fixed_text).
narrative_ontology:cs_authority_grounding('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', extraction).
narrative_ontology:cs_interpretation_layer_present('fde9905c-c473-4fd9-8a92-40f5dd8f36ed').
narrative_ontology:cs_reading_relation('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', foundational, vatican_ii_doctrinal_error).
narrative_ontology:cs_axiom_status(vatican_ii_doctrinal_error, holdable).
narrative_ontology:cs_axiom_grounding('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', vatican_ii_doctrinal_error, deontological).
narrative_ontology:cs_axiom('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', secondary, post_conciliar_church_in_crisis).
narrative_ontology:cs_axiom_status(post_conciliar_church_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', post_conciliar_church_in_crisis, empirically_contingent).
narrative_ontology:cs_reference_frame('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', pre_vatican_ii_tradition).
narrative_ontology:cs_drift_state('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', post_conciliar_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fde9905c-c473-4fd9-8a92-40f5dd8f36ed', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, sspx_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The official teaching authority of the Catholic Church post-Vatican II, which interprets and enforces the Council's documents. From the rupture reading's perspective, this body actively promotes and defends the 'erroneous' interpretations, marginalizing traditionalist dissent.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Theological and clerical groups who actively embrace and promote the post-conciliar changes, seeing them as legitimate and necessary adaptations. They benefit from the new interpretive framework and the marginalization of traditionalist views.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    powerful, biographical, mobile, global).

% Catholics (clergy and lay) who adhere strictly to pre-Vatican II traditions and doctrines. They experience the Council's changes as a betrayal and a source of spiritual and doctrinal instability, often facing marginalization or censure for their views. Their identity is deeply tied to the pre-conciliar Church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy_laity, payer,
    powerless, generational, identity_locked, global).

% Members of the Society of Saint Pius X and similar groups who formally reject the legitimacy or interpretation of Vatican II, often operating in a state of irregular canonical status. They bear the cost of formal exclusion from mainstream Church life but maintain their traditional identity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_members, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, sspx_members, excluded).

% Scholars who analyze Vatican II and its aftermath from historical and theological perspectives, often without direct allegiance to a particular faction. They can observe the structural dynamics and doctrinal shifts without being directly subject to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, academic_historians_theologians, observer,
    analytical, generational, analytical, universal).

% Theologians who argue for an interpretation of Vatican II that emphasizes its continuity with prior tradition. While they are part of the mainstream, their arguments are often seen by the rupture reading as attempts to paper over fundamental contradictions, effectively excluding them from the 'true' understanding of the Council's impact.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, continuity_theologians, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the Council coordinated a departure from traditional Catholic doctrine and practice, ostensibly to 'update' the Church for the modern world, but effectively creating a new, erroneous identity.
% TRANSFER_FUNCTION: Transfers doctrinal authority and interpretive power from the established, immutable tradition to a more fluid, 'modernist' interpretation, resulting in a loss of doctrinal stability and traditional Catholic identity for many adherents.
% ABSENT_VOICES: The voices of pre-conciliar theologians and saints, whose teachings are seen as directly contradicted by the Council, are effectively silenced or reinterpreted to fit the new narrative. Contemporary traditionalist voices are marginalized within official Church discourse.
% DISAPPEARANCE_RATIONALE: If the authority of Vatican II as a rupture vanished overnight, the entire post-conciliar Church structure, its doctrines, and its legitimacy would be called into question, leading to a profound reorganization of Catholic identity, authority, and practice, potentially restoring pre-conciliar forms.
% FOUNDING_PROBLEM: The perceived need to 'open the windows' of the Church, to engage with and adapt to the modern world, and to address perceived rigidities or irrelevance of traditional forms.
% FOUNDING_PROBLEM_CORROBORATION: The 'modernist faction' and the post-conciliar magisterium attest that the founding problem of Church relevance and adaptation is still live. Traditionalist clergy and laity, however, argue that the 'solution' created new, graver problems, and that the original problem was misdiagnosed or exaggerated; their corroboration comes from historical analysis of pre-conciliar vitality and post-conciliar decline in certain metrics.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the 'rupture' fundamentally alters the spiritual and doctrinal landscape, demanding a reorientation of identity and belief from traditionalists. Suppression is high (0.75) due to the active marginalization, censure, and canonical penalties faced by those who resist the post-conciliar interpretation. Theater ratio is low (0.10) because this reading views the constraint as a direct, substantive break, not a performative maintenance of an atrophied function. Resistance is high (0.80) as this reading itself represents a significant, ongoing resistance movement within Catholicism. Accessibility collapse is moderate (0.60) because while mainstream alternatives are largely closed off, traditionalist communities and rites persist outside or on the fringes of official recognition.
 *
 * PERSPECTIVAL GAP:
 *   The 'rupture reading' fundamentally diverges from the 'continuity reading' and the 'composite overdetermination reading'. From the perspective of the 'modernist faction' and the 'post_conciliar_magisterium', the Council represents legitimate development or necessary adaptation. From the 'traditionalist_clergy_laity' and 'sspx_members' seats, the same event is experienced as a profound extraction from their spiritual heritage and identity, enforced by institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'modernist_faction' and 'post_conciliar_magisterium' are beneficiaries, gaining interpretive control and shaping the Church's direction. 'Traditionalist_clergy_laity' and 'sspx_members' are victims, bearing the costs of doctrinal instability, marginalization, and identity-lock. 'Academic_historians_theologians' are observers. 'Continuity_theologians' are excluded from the 'rupture' narrative's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the post-conciliar changes as mere coordination or legitimate development, as the 'rupture reading' asserts that the coordination story (aggiornamento) is a cover for a fundamental extraction from tradition. The high extractiveness and suppression, coupled with identifiable victims, point to a coercive structure rather than a benign one, from this specific interpretive seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_reading,
    'Is this constraint a genuine rupture with tradition, or an organic development (continuity_reading), or an overdetermined composite (composite_overdetermination_reading)?',
    'Comprehensive historical-theological analysis of primary sources, including pre-conciliar and conciliar documents, and their reception, assessed against established criteria for doctrinal development vs. rupture. This is a conceptual and empirical question.',
    'If resolved as continuity, the constraint''s extractiveness and suppression would be significantly lower, reclassifying it towards a Rope or even Mountain (for core doctrines). If resolved as composite, the constraint''s structure would be far more ambiguous, potentially leading to a Tangled Rope with highly contested directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vatican_ii_kernel_reading, conceptual, 'This constraint is one reading of the ''vatican_ii_authority'' kernel; its classification depends on which reading is structurally true.').

omega_variable(
    doctrinal_error_objectivity,
    'Are the alleged doctrinal errors or contradictions objectively present in the Vatican II documents, or are they a matter of interpretation and emphasis?',
    'Formal theological adjudication by an ecumenical council or a universally recognized authority, or a consensus among independent theological experts using agreed-upon hermeneutical principles. This is a conceptual and empirical question.',
    'If objective errors are confirmed, the ''rupture reading'' is strongly validated, reinforcing its Snare classification. If they are found to be interpretive differences, the extractiveness would decrease, as the ''victimization'' is less about objective error and more about a shift in emphasis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_error_objectivity, conceptual, 'The objectivity of doctrinal errors in Vatican II documents.').

omega_variable(
    modernist_faction_intent,
    'To what extent did the ''modernist faction'' intentionally seek to break with tradition for their own benefit, versus genuinely seeking pastoral adaptation?',
    'Historical analysis of private correspondence, memoirs, and internal documents of key figures, combined with a sociological study of the faction''s influence and outcomes. This is an empirical question.',
    'If intentional extraction is confirmed, the Snare classification is strengthened. If genuine pastoral intent is dominant, the extractiveness might be re-evaluated as a byproduct of a (misguided) coordination effort, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_faction_intent, empirical, 'Intent of the ''modernist faction'' regarding Vatican II''s impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.05).
narrative_ontology:measurement(vati_tr_t1972, vatican_ii_authority__rupture_reading, theater_ratio, 1972, 0.08).
narrative_ontology:measurement(vati_tr_t1982, vatican_ii_authority__rupture_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(vati_tr_t1992, vatican_ii_authority__rupture_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(vati_tr_t2002, vatican_ii_authority__rupture_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_authority__rupture_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__rupture_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1972, vatican_ii_authority__rupture_reading, base_extractiveness, 1972, 0.7).
narrative_ontology:measurement(vati_be_t1982, vatican_ii_authority__rupture_reading, base_extractiveness, 1982, 0.78).
narrative_ontology:measurement(vati_be_t1992, vatican_ii_authority__rupture_reading, base_extractiveness, 1992, 0.82).
narrative_ontology:measurement(vati_be_t2002, vatican_ii_authority__rupture_reading, base_extractiveness, 2002, 0.84).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_authority__rupture_reading, base_extractiveness, 2012, 0.85).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__rupture_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.5).
narrative_ontology:measurement(vati_su_t1972, vatican_ii_authority__rupture_reading, suppression_requirement, 1972, 0.6).
narrative_ontology:measurement(vati_su_t1982, vatican_ii_authority__rupture_reading, suppression_requirement, 1982, 0.68).
narrative_ontology:measurement(vati_su_t1992, vatican_ii_authority__rupture_reading, suppression_requirement, 1992, 0.72).
narrative_ontology:measurement(vati_su_t2002, vatican_ii_authority__rupture_reading, suppression_requirement, 2002, 0.74).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_authority__rupture_reading, suppression_requirement, 2012, 0.75).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__rupture_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'rupture reading' of the 'vatican_ii_authority' kernel, which claims substantive breaks and doctrinal errors. It is structurally distinct from the 'continuity_reading' (organic development) and the 'composite_overdetermination_reading' (unresolvable ambiguity), each with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
