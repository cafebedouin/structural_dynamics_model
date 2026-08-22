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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II as Rupture with Tradition (Rupture Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the 'rupture reading' of the Second Vatican
 *   Council, which asserts that the Council's documents contain doctrinal
 *   errors or irreconcilable contradictions with prior Catholic teaching,
 *   leading to a substantive break with tradition. From this perspective, the
 *   post-conciliar Church is in a state of crisis, and traditional Catholic
 *   identity and doctrinal stability are victims of this rupture. The SSPX
 *   (Society of St. Pius X) position is a prominent instantiation of this
 *   reading. The constraint is claimed as a snare because, from this
 *   perspective, the Council's authority is used to extract adherence to a
 *   flawed new paradigm, suppressing traditional alternatives.
 *
 * KEY AGENTS:
 *   - modernist_faction: Primary beneficiary (institutional/mobile) — gains influence and legitimacy.
 *   - liberal_theologians: Secondary beneficiary (organized/mobile) — find their views validated.
 *   - traditional_catholic_identity: Primary victim (powerless/identity_locked) — challenged and undermined.
 *   - doctrinal_stability: Secondary victim (powerless/trapped) — coherence of doctrine is eroded.
 *   - traditionalist_clergy_laity: Payer (moderate/constrained) — face marginalization for adherence to tradition.
 *   - roman_magisterium: Agenda setter (institutional/constrained) — enforces the post-conciliar paradigm.
 *   - society_of_st_pius_x: Observer (organized/identity_locked) — institutional voice for the rupture reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.85).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Rupture with Tradition (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'ec23babe-a3c1-486f-aaf9-95b6370b9ddc').
narrative_ontology:cs_kernel_codification('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', formalized).
narrative_ontology:cs_authority_grounding('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', extraction).
narrative_ontology:cs_interpretation_layer_present('ec23babe-a3c1-486f-aaf9-95b6370b9ddc').
narrative_ontology:cs_reading_relation('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', foundational, doctrinal_infallibility_of_prior_magisterium).
narrative_ontology:cs_axiom_status(doctrinal_infallibility_of_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', doctrinal_infallibility_of_prior_magisterium, deontological).
narrative_ontology:cs_axiom('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', foundational, vatican_ii_documents_contain_errors).
narrative_ontology:cs_axiom_status(vatican_ii_documents_contain_errors, holdable).
narrative_ontology:cs_axiom_grounding('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', vatican_ii_documents_contain_errors, empirically_contingent).
narrative_ontology:cs_reference_frame('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', contemporary_post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ec23babe-a3c1-486f-aaf9-95b6370b9ddc', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, liberal_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perceived rupture, which allows for reinterpretation of doctrine and practice in line with contemporary thought. They gain influence and legitimacy for their theological positions.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    institutional, generational, mobile, global).

% Find their theological approaches validated and promoted by the perceived break with prior tradition. They gain academic and ecclesiastical standing.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, liberal_theologians, beneficiary,
    organized, biographical, mobile, global).

% Suffers from the perceived invalidation of pre-conciliar forms of worship, doctrine, and piety. Their identity is challenged by the claim of rupture, leading to a sense of loss and alienation within the Church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_identity, payer,
    powerless, generational, identity_locked, global).

% The coherence and immutability of Catholic doctrine are seen as undermined by the alleged contradictions and errors in the Council's documents, leading to theological confusion and relativism.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_stability, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, doctrinal_stability).

% Bear the direct costs of adhering to traditional practices and beliefs, often facing marginalization, censure, or exclusion from mainstream Church life. Their options are to conform, resist, or leave the visible Church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy_laity, payer,
    moderate, biographical, constrained, global).

% The official teaching authority of the Catholic Church, which promulgates and interprets the Council's documents. From the rupture reading's perspective, the Magisterium is either complicit in the rupture or unable to effectively suppress it, thereby enforcing the new, flawed paradigm.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, roman_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% A traditionalist Catholic priestly society that explicitly rejects the legitimacy of Vatican II's reforms and maintains pre-conciliar liturgical and doctrinal practices. They serve as a primary institutional voice for the rupture reading.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, society_of_st_pius_x, observer,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, from this reading's perspective, fails to coordinate the faithful around a consistent, unchanging deposit of faith, instead coordinating a departure from it.
% TRANSFER_FUNCTION: Transfers doctrinal authority and legitimacy from prior tradition to the post-conciliar Magisterium and its favored theological interpretations, at the cost of traditional Catholic identity and doctrinal clarity.
% ABSENT_VOICES: The voices of pre-conciliar saints, theologians, and popes, whose teachings are seen as contradicted by Vatican II, are effectively silenced or reinterpreted to fit the new paradigm. Their absence is structural, as their authority is implicitly or explicitly superseded.
% DISAPPEARANCE_RATIONALE: If the rupture reading of Vatican II vanished, the entire post-conciliar Catholic Church would be forced to re-evaluate its legitimacy and doctrinal foundations. Traditionalist groups would either reintegrate or find their raison d'être dissolved, and the modernist project would lose its foundational justification, leading to a profound reorganization of Catholic theology and practice.
% FOUNDING_PROBLEM: The rupture reading asserts that Vatican II was not built to solve a problem but rather created one: a departure from the perennial teaching and practice of the Church, leading to a crisis of faith and identity.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist theologians and historians, as well as many disillusioned faithful, attest that the crisis of faith and identity is ongoing and directly attributable to the Council. This is corroborated by declining Mass attendance, vocations, and belief in core doctrines in many parts of the world, which are seen as symptoms of the rupture.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because the rupture reading views the post-conciliar Church as demanding adherence to a new, flawed doctrine, effectively extracting traditional identity and belief. Suppression is also high (0.75) due to the perceived marginalization and suppression of traditionalist voices and practices within the Church. Theater ratio is moderate (0.4) as efforts to present the Council as continuous with tradition are seen as performative, masking a deeper break. The metrics reflect the severity of the perceived crisis from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   The Roman Magisterium, from its own perspective, operates a 'continuity reading' (a different constraint) where Vatican II is an organic development. However, from the 'rupture reading' perspective, the Magisterium's actions (e.g., suppressing traditional rites or censuring traditionalist theologians) are seen as actively enforcing the rupture, making it an agenda-setter for a snare. This divergence is central to the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'modernist faction' and 'liberal theologians' are beneficiaries as their theological positions are validated and promoted by the perceived rupture. 'Traditional Catholic identity' and 'doctrinal stability' are victims, as their foundational principles are undermined. 'Traditionalist clergy and laity' are payers, bearing the social and ecclesiastical costs of resisting the new paradigm. The 'Roman Magisterium' acts as the agenda-setter, enforcing the post-conciliar order that, from this reading, constitutes the rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   From the rupture reading, the mandate of Vatican II (if it had one beyond rupture) has been entirely subverted. The constraint is not a degraded coordination mechanism but an active snare, extracting adherence to a new, flawed paradigm. The classification prevents mislabeling this as a mere 'piton' or 'tangled rope' by emphasizing the active extraction and suppression of traditional alternatives, rather than just inertial decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_contradiction_objectivity,
    'Are the alleged doctrinal errors or contradictions in Vatican II''s documents objectively irreconcilable with prior teaching, or are they matters of theological interpretation?',
    'A definitive, universally accepted theological analysis that either demonstrates irrefutable contradiction or provides a coherent synthesis accepted by all parties. This is unlikely to be resolved empirically.',
    'If objectively irreconcilable, the rupture reading is validated, and the constraint''s extractiveness is confirmed as illegitimate. If reconcilable through interpretation, the rupture reading''s claims of error are weakened, potentially reclassifying the constraint as a ''tangled rope'' or even ''rope'' from a different perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_contradiction_objectivity, conceptual, 'The objective status of alleged doctrinal contradictions.').

omega_variable(
    magisterial_intent_vs_effect,
    'Did the Roman Magisterium intend a rupture with tradition, or was the rupture an unintended consequence of reforms intended to be continuous?',
    'Historical analysis of internal Church documents and statements, combined with theological hermeneutics, to discern the primary intent. This is a matter of ongoing scholarly debate.',
    'If rupture was intended, the Magisterium''s role as an agenda-setter for a snare is strengthened. If unintended, the constraint might be reclassified as a ''tangled rope'' where a coordination effort (reform) had unforeseen extractive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_intent_vs_effect, empirical, 'The intent behind Vatican II''s reforms versus their perceived effect.').

omega_variable(
    legitimacy_of_resistance,
    'Is resistance to the post-conciliar Magisterium (as exemplified by traditionalist groups) a legitimate defense of tradition or an act of schism?',
    'A future, universally recognized ecclesiastical judgment on the canonical status of traditionalist resistance. This is a matter of ongoing theological and canonical dispute.',
    'If legitimate, the ''payer'' status of traditionalists is validated, and the constraint''s suppression is seen as unjust. If schismatic, their ''payer'' status is self-imposed, and the constraint''s suppression is seen as legitimate enforcement of unity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_resistance, preference, 'The theological and canonical legitimacy of traditionalist resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__rupture_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_authority__rupture_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__rupture_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__rupture_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__rupture_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_authority__rupture_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__rupture_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__rupture_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__rupture_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_authority__rupture_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__rupture_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__rupture_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_authority' kernel. This 'rupture_reading' posits a substantive break with tradition, contrasting with the 'continuity_reading' and the 'composite_overdetermination_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
