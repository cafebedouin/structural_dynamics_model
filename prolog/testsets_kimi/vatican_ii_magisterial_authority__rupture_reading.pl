% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The Second Vatican Council (1962-1965) produced texts that have been
 *   interpreted through competing hermeneutics. This constraint story
 *   instantiates the rupture reading: the claim that the Council's texts
 *   encode a new ecclesiology fundamentally incompatible with pre-conciliar
 *   teaching, authorizing radical implementation, liturgical experimentation,
 *   and the supersession of doctrines such as 'error has no rights' by
 *   Dignitatis Humanae. The constraint operates as an authority structure
 *   that coordinates progressive reform constituencies while extracting
 *   magisterial legitimacy from traditional Catholic communities who
 *   experience the reading as the delegitimization of their identity and
 *   practice. The constraint is actively enforced through liturgical
 *   restrictions, doctrinal discipline, and the institutional marginalization
 *   of continuity arguments.
 *
 * KEY AGENTS:
 *   - postconciliar_reform_constituency (beneficiary/organized): gains institutional authorization for liturgical and doctrinal innovation
 *   - traditional_catholic_communities (payer/moderate): bears the cost of delegitimization and restriction of pre-conciliar practice
 *   - postconciliar_magisterium (agenda_setter/institutional): administers the rupture hermeneutic and enforces supersession
 *   - continuity_reading_advocates (excluded/organized): structurally marginalized from magisterial interpretation
 *   - ecclesial_historians (observer/analytical): external analytical seat tracking reception history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'dab7d1cc-a78a-4dd2-9781-6d36fa556151').
narrative_ontology:cs_kernel_codification('dab7d1cc-a78a-4dd2-9781-6d36fa556151', formalized).
narrative_ontology:cs_authority_grounding('dab7d1cc-a78a-4dd2-9781-6d36fa556151', lineage).
narrative_ontology:cs_interpretation_layer_present('dab7d1cc-a78a-4dd2-9781-6d36fa556151').
narrative_ontology:cs_reading_relation('dab7d1cc-a78a-4dd2-9781-6d36fa556151', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dab7d1cc-a78a-4dd2-9781-6d36fa556151', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('dab7d1cc-a78a-4dd2-9781-6d36fa556151', foundational, conciliar_texts_encode_rupture_ecclesiology).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_rupture_ecclesiology, holdable).
narrative_ontology:cs_axiom_grounding('dab7d1cc-a78a-4dd2-9781-6d36fa556151', conciliar_texts_encode_rupture_ecclesiology, theological).
narrative_ontology:cs_axiom('dab7d1cc-a78a-4dd2-9781-6d36fa556151', foundational, doctrinal_progress_via_acknowledged_contradiction).
narrative_ontology:cs_axiom_status(doctrinal_progress_via_acknowledged_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('dab7d1cc-a78a-4dd2-9781-6d36fa556151', doctrinal_progress_via_acknowledged_contradiction, deontological).
narrative_ontology:cs_reference_frame('dab7d1cc-a78a-4dd2-9781-6d36fa556151', conciliar_rupture_as_authoritative_norm).
narrative_ontology:cs_drift_state('dab7d1cc-a78a-4dd2-9781-6d36fa556151', contemporary_postconciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dab7d1cc-a78a-4dd2-9781-6d36fa556151', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, postconciliar_reform_constituency).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditional_catholic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, liturgists, and activists who interpret Vatican II as authorizing radical reform. They benefit from the delegitimization of pre-conciliar constraints and the institutional opening for liturgical experimentation, doctrinal development, and expanded religious liberty. Their position is validated by the rupture reading's ascendancy in key theological faculties and diocesan offices.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, postconciliar_reform_constituency, beneficiary,
    organized, generational, mobile, global).

% Communities attached to the pre-conciliar liturgy, theology, and magisterial discipline. They experience the rupture reading as delegitimizing their identity: traditional liturgical forms are restricted, pre-conciliar doctrinal formulations are labeled obsolete, and their objections are treated as disobedience to the Council. Their exit is blocked by religious identity fusion and sacramental commitments.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditional_catholic_communities, payer,
    moderate, generational, identity_locked, global).

% Bishops, curial officials, and theological commissions who administer the Church through the rupture hermeneutic. They enforce the supersession of pre-conciliar positions, approve liturgical experimentation, and adjudicate doctrinal disputes by appeal to the Council's new ecclesiology. Their authority depends on maintaining the rupture reading as the operative framework.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, postconciliar_magisterium, agenda_setter,
    institutional, civilizational, constrained, global).

% Theologians and communities who argue for the hermeneutic of continuity. They are structurally excluded from magisterial interpretation under the rupture reading's dominance: their appeals to pre-conciliar tradition are treated as nostalgia or disobedience rather than legitimate theological arguments.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, continuity_reading_advocates, excluded,
    organized, generational, constrained, global).

% Academic historians and theologians who study the Council's documentary history and reception. They analyze whether the texts themselves support rupture, continuity, or composite ambiguity, without being bound to enforce any particular reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecclesial_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, postconciliar_reform_constituency).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the post-conciliar ecclesiological crisis by providing a hermeneutic framework that authorizes doctrinal and liturgical reform, unifying progressive constituencies around a shared narrative of the Council as a revolutionary event.
% TRANSFER_FUNCTION: Moves magisterial legitimacy and liturgical/doctrinal authorization from pre-conciliar traditional communities to post-conciliar reform constituencies, via the interpretation of conciliar texts as superseding prior teaching.
% ABSENT_VOICES: Traditional Catholic communities whose identity is bound to pre-conciliar forms are present in the Church but excluded from the interpretive conversation; their objections are treated as disobedience rather than theological argument. Continuity-reading theologians are similarly marginalized in institutions dominated by the rupture hermeneutic.
% DISAPPEARANCE_RATIONALE: If the rupture hermeneutic disappeared overnight, the authorization for radical liturgical experimentation and the supersession of pre-conciliar doctrine would collapse. Progressive reform constituencies would lose their primary magisterial warrant, traditional communities would no longer be classified as disobedient for maintaining pre-conciliar practice, and the Church's institutional balance would shift toward continuity or composite readings.
% FOUNDING_PROBLEM: The mid-20th-century Catholic Church faced a crisis of modernity: an embattled relationship with liberal states, a liturgy disconnected from contemporary cultures, and a centralized ecclesiology that many theologians judged insufficiently responsive to the laity and ecumenical realities.
% FOUNDING_PROBLEM_CORROBORATION: Reform constituencies attest the problem is still live, citing ongoing secularization. Traditional Catholic communities and some historians attest the founding problem was resolved by pre-conciliar means or was exacerbated by rupture; they corroborate that the arrangement now persists beyond its founding justification. External sociologists of religion note that secularization accelerated post-conciliarly, corroborating neither side's self-assessment cleanly.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the rupture reading transfers magisterial legitimacy away from traditional communities and toward reformers; suppression (0.68) is high because the constraint's persistence requires active enforcementârestricting traditional liturgy, disciplining dissent, and suppressing continuity arguments in seminaries and faculties. Theater ratio (0.45) reflects significant performative maintenance: the 'spirit of Vatican II' is frequently invoked to justify practices that go beyond the conciliar texts themselves. Accessibility collapse (0.70) is high because once the rupture framework is adopted, pre-conciliar alternatives become cognitively and institutionally inaccessibleâread as disobedience rather than tradition. Resistance (0.58) captures the sustained pushback from traditional communities and some episcopal quarters. Measurements show a cyclical pattern: extraction and enforcement peaked in the 1970s, receded under the 'reform of the reform' (1990sâ2000s), and ratcheted upward again in the 2010sâ2020s.
 *
 * PERSPECTIVAL GAP:
 *   The postconciliar reform constituency experiences the constraint as liberation and necessary modernization; the traditional Catholic community experiences the identical structure as erasure and delegitimization. The postconciliar magisterium experiences it as legitimate authority; the continuity advocate experiences it as ideological capture of the interpretive office. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The postconciliar reform constituency is the structural beneficiary (low d): the constraint subsidizes their theological and liturgical projects with magisterial cover. Traditional Catholic communities are the structural targets (high d): their identity, liturgy, and doctrinal commitments are actively extracted and suppressed. The postconciliar magisterium sits near symmetric but agenda-setter (moderate d): it both benefits from the authority the reading confers and bears the institutional cost of enforcing it. Continuity advocates are excluded, receiving no directional benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the coordination function (unifying reformers around a shared ecclesiology, resolving post-conciliar institutional chaos) as pure extraction, while also preventing the progressive self-narrative from masking the asymmetric extraction borne by traditional communities. If the coordination function atrophied entirely and only enforcement remained, it would degrade toward snare; if enforcement lapsed and the coordination dissolved into mere memory, it would degrade toward piton. Neither has occurred: the coordination remains real for beneficiaries and the extraction remains real for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_unity_ambiguity,
    'Do the conciliar texts genuinely encode a single rupture ecclesiology, or do they remain an ambiguous composite that the rupture reading resolves by selective emphasis?',
    'Close documentary analysis of the conciliar schemas, periti interventions, and final vote tallies across all sixteen documents to measure internal consistency with a rupture hermeneutic versus internal tension.',
    'If the texts are genuinely composite, the rupture reading''s epsilon is inflated by interpretive violence rather than textual fidelity, moving its classification toward snare; if the texts coherently encode rupture, the high extraction is the price of doctrinal fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_unity_ambiguity, conceptual, 'Whether the conciliar texts support a single rupture reading or are inherently ambiguous').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditional communities structural (canonical and liturgical restrictions) or internalized (identity fusion that makes exit psychologically impossible even where structurally permitted)?',
    'Post-exit trajectory analysis: if traditional communities maintain doctrinal resistance after structural penalties are removed, suppression is partially internalized; if resistance collapses when restrictions lift, suppression was primarily structural.',
    'If internalized, effective extraction exceeds the structural measure; the constraint operates partly through cognitive capture rather than institutional coercion alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for traditional communities').

omega_variable(
    mandate_obsolescence,
    'Has the founding problemâthe Church''s mid-century crisis of modernityâbeen resolved, rendering the rupture reading''s mandate obsolete?',
    'Comparative sociological analysis of Catholic practice, vocations, and institutional vitality across jurisdictions with stronger vs weaker rupture implementation.',
    'If the problem is dead and the constraint persists, it strengthens the piton/theater path; if the problem remains live, the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the rupture reading''s founding problem is still live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(vati_tr_t58, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 58, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(vati_be_t58, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 58, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(vati_su_t58, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 58, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vatican_ii_magisterial_authority kernel. The rupture, continuity, and composite readings produce structurally distinct beneficiary/victim configurations and epsilon profiles, requiring decomposition per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
