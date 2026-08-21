% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Continuity
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'continuity reading' of Vatican
 *   II's authority, which asserts that the Council's reforms represent an
 *   organic doctrinal development in unbroken continuity with prior Catholic
 *   tradition. From this perspective, the reforms are legitimate expressions
 *   of an unchanging deposit of faith, and any ambiguities in the Council's
 *   documents are resolvable through traditional hermeneutics. This reading
 *   serves to legitimize post-conciliar changes and maintain doctrinal unity,
 *   benefiting progressive reformers and the Magisterium.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.15).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.4).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Doctrinal Continuity").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'a4fe2116-fa64-41a0-9e69-a3978c1d35f8').
narrative_ontology:cs_kernel_codification('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', fixed_text).
narrative_ontology:cs_authority_grounding('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', lineage).
narrative_ontology:cs_interpretation_layer_present('a4fe2116-fa64-41a0-9e69-a3978c1d35f8').
narrative_ontology:cs_reading_relation('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', foundational, doctrinal_development_is_organic).
narrative_ontology:cs_axiom_status(doctrinal_development_is_organic, holdable).
narrative_ontology:cs_axiom_grounding('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', doctrinal_development_is_organic, deontological).
narrative_ontology:cs_axiom('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', foundational, magisterial_interpretive_authority_is_supreme).
narrative_ontology:cs_axiom_status(magisterial_interpretive_authority_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', magisterial_interpretive_authority_is_supreme, conventional).
narrative_ontology:cs_reference_frame('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', pre_vatican_ii_doctrinal_unity).
narrative_ontology:cs_drift_state('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a4fe2116-fa64-41a0-9e69-a3978c1d35f8', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, theologians_supporting_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, faithful_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which officially promulgates and enforces this interpretation of Vatican II, ensuring doctrinal unity and continuity with tradition. It benefits from maintaining its authority and the coherence of the faith.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Theologians, clergy, and lay movements who advocate for and implement post-conciliar reforms, finding legitimacy and theological grounding for their work within this continuity framework. Their initiatives are validated and promoted.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, biographical, mobile, global).

% Academic and pastoral theologians whose work articulates and defends the organic development and continuity of Vatican II's teachings with prior tradition. Their scholarship is affirmed and integrated into official Church discourse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, theologians_supporting_continuity, beneficiary,
    organized, biographical, mobile, global).

% Factions within the Church who perceive Vatican II as a rupture with tradition and reject many of its reforms. From the perspective of the continuity reading, their views are outside the legitimate interpretive framework and are therefore excluded from official discourse, though they remain within the Church due to identity lock.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_critics, excluded,
    powerless, generational, identity_locked, global).

% The general body of Catholic believers who receive and internalize the official teaching, benefiting from a coherent and evolving understanding of their faith that integrates modern concerns while affirming tradition. They are coordinated into a unified understanding.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, faithful_laity, beneficiary,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified and authoritative interpretive framework for the Second Vatican Council, ensuring doctrinal coherence and preventing fragmentation within the Catholic Church by integrating reforms into a narrative of organic development.
% TRANSFER_FUNCTION: Transfers interpretive authority and doctrinal legitimacy from potentially conflicting theological perspectives to the Magisterium's continuity framework, thereby channeling theological discourse and pastoral practice along approved lines.
% ABSENT_VOICES: Traditionalist critics and those who perceive a substantive rupture in Vatican II's teachings are structurally excluded from the official discourse of continuity. They would argue that this reading suppresses legitimate concerns about doctrinal change and historical discontinuity.
% DISAPPEARANCE_RATIONALE: If this official reading vanished, the Catholic Church would face severe internal fragmentation and potential schism. Without a unifying interpretive framework, different factions would assert irreconcilable understandings of Vatican II, destabilizing the Church's global structure and authority.
% FOUNDING_PROBLEM: The challenge of integrating the teachings and reforms of the Second Vatican Council (1962-1965) with the existing body of Catholic tradition, avoiding the perception of a break and maintaining doctrinal unity in a rapidly changing world.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and theologians supporting continuity consistently attest to the ongoing necessity of this framework. Historians of religion and ecumenical observers, while potentially disagreeing on its success or implications, generally acknowledge the historical and theological challenge of reconciling the Council's teachings and the need for an interpretive strategy to maintain unity.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.15) because, by its own lights, this reading frames reforms as 'cost-free development' with no identifiable victims. Suppression is moderate (0.40) as it requires adherence to a specific interpretive framework, actively marginalizing alternative readings. Theater ratio is low (0.10) as the claim of genuine doctrinal development is central to its function, not merely performative. Accessibility collapse is moderate (0.50) as alternative interpretations are suppressed but not entirely eliminated, and resistance is low (0.30) because the official nature of this reading means direct, organized resistance against its core premise is limited within the Church.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and continuity-minded reformers, this constraint is a necessary coordination mechanism for doctrinal unity. However, traditionalist critics would experience it as a form of intellectual and spiritual suppression, forcing conformity to an interpretation they believe deviates from authentic tradition. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium acts as the agenda-setter, defining and enforcing this reading, thus benefiting from the maintenance of its authority. Progressive reformers and theologians supporting continuity are beneficiaries, as their work is legitimized and promoted. Traditionalist critics are excluded, as their views are deemed outside the legitimate interpretive framework, but they are not considered 'victims' by this reading's own account of 'cost-free development'.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_ambiguity,
    'Is the claimed organic doctrinal continuity a genuine theological development or a rhetorical construct designed to legitimize post-conciliar changes?',
    'Comprehensive historical-theological analysis comparing pre- and post-conciliar teachings on specific doctrines, assessed by a panel of independent scholars from diverse theological traditions.',
    'If found to be a rhetorical construct, the extractiveness of this reading would increase significantly, as it would be seen as suppressing genuine doctrinal discontinuity. If confirmed as genuine, its rope-like classification would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_ambiguity, conceptual, 'Ambiguity regarding the authenticity of claimed doctrinal continuity.').

omega_variable(
    cost_of_development_ambiguity,
    'Is the claim of ''cost-free development'' accurate, or are there hidden costs (e.g., marginalization, intellectual suppression) borne by those whose theological perspectives are deemed incompatible with this reading?',
    'Sociological and historical studies documenting the careers, publications, and institutional standing of theologians and clergy who dissent from the continuity reading, assessing patterns of promotion, censure, or exclusion.',
    'If significant hidden costs are identified, the extractiveness of this reading would increase, and its classification might shift towards a Tangled Rope, as it would reveal an asymmetric cost burden despite the ''no victim'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_development_ambiguity, empirical, 'Whether the ''cost-free development'' claim masks unacknowledged burdens on dissenters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__continuity_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.13).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__continuity_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__continuity_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__continuity_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__continuity_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__continuity_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__continuity_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__continuity_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
