% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity as Magisterial Constraint
 *   domain: ecclesiological/institutional/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the Vatican II
 *   magisterial authority kernel. It asserts that the Second Vatican Council
 *   represents organic development within unbroken apostolic tradition, with
 *   no rupture from prior magisterium. Operatively, this reading constrains
 *   conciliar implementation to preserve pre-conciliar doctrine, rules
 *   'spirit of Vatican II' claims unauthorized, treats Sacrosanctum Concilium
 *   Â§36 as a binding Latin preservation mandate, and reconciles Dignitatis
 *   Humanae with the Syllabus of Errors through distinctions between thesis
 *   and hypothesis or development of doctrine. The Roman Curia and papal
 *   magisterium enforce this reading through doctrinal assessments,
 *   liturgical restrictions, and curial oversight of national bishops'
 *   conferences.
 *
 * KEY AGENTS:
 *   - papal_magisterium: Agenda-setter (institutional/identity_locked/global) â enforces the continuity reading through curial documents, doctrinal mandates, and liturgical restrictions
 *   - traditionalist_catholic_communities: Beneficiary (organized/constrained/global) â receive magisterial validation and expanded traditional liturgical permissions
 *   - progressive_catholic_theologians: Primary payer (moderate/constrained/global) â lose interpretive autonomy and face sanctions for rupture-oriented theology
 *   - national_bishops_conferences: Payer (institutional/constrained/global) â lose autonomous synodal and pastoral scope to Roman oversight
 *   - liturgical_reform_movements: Payer (moderate/constrained/national) â blocked from further vernacularization and inculturation by the Latin preservation mandate
 *   - historical_critical_scholars: Excluded (moderate/constrained/global) â findings on textual drafting history excluded from seminary formation and official catechesis
 *   - sociology_of_religion_observers: Observer (analytical/analytical/global) â track institutional power shifts without theological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.75).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.8).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Hermeneutic of Continuity as Magisterial Constraint").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiological/institutional/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '42b50c3d-7810-4537-aec2-9d701e9b8c65').
narrative_ontology:cs_kernel_codification('42b50c3d-7810-4537-aec2-9d701e9b8c65', fixed_text).
narrative_ontology:cs_authority_grounding('42b50c3d-7810-4537-aec2-9d701e9b8c65', lineage).
narrative_ontology:cs_interpretation_layer_present('42b50c3d-7810-4537-aec2-9d701e9b8c65').
narrative_ontology:cs_reading_relation('42b50c3d-7810-4537-aec2-9d701e9b8c65', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('42b50c3d-7810-4537-aec2-9d701e9b8c65', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('42b50c3d-7810-4537-aec2-9d701e9b8c65', foundational, doctrine_develops_organically_without_rupture).
narrative_ontology:cs_axiom_status(doctrine_develops_organically_without_rupture, holdable).
narrative_ontology:cs_axiom_grounding('42b50c3d-7810-4537-aec2-9d701e9b8c65', doctrine_develops_organically_without_rupture, theological).
narrative_ontology:cs_axiom('42b50c3d-7810-4537-aec2-9d701e9b8c65', foundational, prior_magisterium_controls_conciliar_interpretation).
narrative_ontology:cs_axiom_status(prior_magisterium_controls_conciliar_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('42b50c3d-7810-4537-aec2-9d701e9b8c65', prior_magisterium_controls_conciliar_interpretation, theological).
narrative_ontology:cs_reference_frame('42b50c3d-7810-4537-aec2-9d701e9b8c65', unbroken_apostolic_tradition).
narrative_ontology:cs_drift_state('42b50c3d-7810-4537-aec2-9d701e9b8c65', contemporary_post_conciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42b50c3d-7810-4537-aec2-9d701e9b8c65', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_catholic_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_catholic_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, liturgical_reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims the authority to adjudicate which interpretations of Vatican II are continuous with tradition; issues documents, doctrinal assessments, and liturgical instructions that bind the universal Church. Its institutional legitimacy is fused with the mission to guard unbroken apostolic tradition, making exit from the continuity frame tantamount to abandoning the Petrine office's self-understanding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, papal_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive magisterial validation, expanded permissions for the Latin Mass, and doctrinal preference under the continuity reading. Their theological and liturgical commitments are treated as normative expressions of tradition rather than exceptional accommodations.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_catholic_communities, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of having interpretations judged rupture-oriented or unauthorized. Face doctrinal sanctions, loss of teaching mandates, exclusion from seminary appointments, and retraction requirements when their framing of the council's 'spirit' is ruled incompatible with prior magisterium.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_catholic_theologians, payer,
    moderate, biographical, constrained, global).

% Lose autonomous interpretive and pastoral scope as Roman congregations assert direct review over doctrinal and liturgical implementation. Local synodal initiatives and pastoral adaptations are subjected to Roman examination for continuity with pre-conciliar norms, constraining collegial governance.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, national_bishops_conferences, payer,
    institutional, generational, constrained, global).

% Seek further vernacularization, inculturation, and adaptive liturgical forms. The continuity reading's binding interpretation of Sacrosanctum Concilium Â§36 as a Latin preservation mandate, together with restrictions on liturgical reform, blocks their agenda and delegitimizes their prior achievements.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, liturgical_reform_movements, payer,
    moderate, biographical, constrained, national).

% Academic historians who document the drafting compromises, ambiguous formulations, and overdetermined textual genesis of conciliar documents. Their findings are treated as irrelevant or pastorally dangerous to magisterial adjudication and are largely excluded from seminary formation, official catechesis, and curial consultation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, historical_critical_scholars, excluded,
    moderate, biographical, constrained, global).

% Academic observers who track how the continuity reading reshapes Catholic institutional identity, power distribution, and factional alignment without being bound by its theological claims or subject to its sanctions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, sociology_of_religion_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, papal_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional unity across a global church by asserting that its highest council is consonant with all prior teaching, preventing formal schism and providing a single interpretive key for diverse local churches.
% TRANSFER_FUNCTION: Moves interpretive authority from local bishops, theologians, and reform movements to the Roman magisterium, and transfers legitimacy and liturgical space from progressive factions to traditionalist communities.
% ABSENT_VOICES: Historical-critical scholars documenting conciliar drafting history; progressive theologians sanctioned or excluded from official teaching roles; ecumenical partners whose agreements depend on a non-retrograde reading of religious freedom; national synods whose local pastoral judgments are overridden.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, progressive theologians would regain institutional voice, Latin mandates would loosen, national synods would claim broader doctrinal and pastoral autonomy, and traditionalist communities would lose their magisterially-validated standing. The Catholic Church's internal power map would reorganize around competing hermeneutic claims within a generation.
% FOUNDING_PROBLEM: The post-conciliar crisis of authority and identity: how to maintain the Catholic Church's claim to unbroken doctrinal identity while acknowledging substantial changes in liturgy, ecclesiology, and interfaith relations after Vatican II.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist factions and the papal magisterium attest the problem is still live, citing doctrinal confusion and liturgical abuse. Progressive theologians and some national hierarchies attest the problem was resolved by the council itself and that the continuity reading is a retrograde construction. Sociologists of religion and independent conciliar historians outside the benefiting parties document the crisis as substantially manufactured by the continuity reading's own restrictive frame.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) reflects substantial taking of interpretive autonomy from theologians and local churches. Suppression (0.80) reflects active enforcement via doctrinal sanctions, withdrawal of teaching mandates, liturgical restrictions such as Traditionis Custodes, and curial review of national synodal initiatives. Theater ratio (0.45) indicates that nearly half of the magisterial activity devoted to continuity is performative maintenance that papers over real textual and historical tensions, particularly around religious freedom and collegiality. Accessibility collapse (0.78) is high because, within the institutional Church, alternative readings rapidly become classified as heretical, disobedient, or pastorally dangerous once the continuity frame is accepted. Resistance (0.60) reflects organized pushback from progressive theologians, some national hierarchies, and reform movements. The measurement series show a steady intensification from 1965 to 2025 as the continuity reading matured from a minority corrective into the dominant magisterial hermeneutic.
 *
 * PERSPECTIVAL GAP:
 *   From the papal magisterium seat, the constraint is experienced as necessary guardianship of divine revelation against fragmentation; the agenda-setter sees low directionality because the arrangement subsidizes its authority. From progressive theologians and liturgical reformers, the identical structure is experienced as suppression of legitimate development and forced retrenchment; these seats see high directionality because the constraint extracts their voice and vocational security. Traditionalist communities see subsidy and validation (low directionality). National bishops conferences experience a loss of collegial autonomy that varies by regional strength, producing moderate-to-high directionality. The engine computes this divergence from the structural data rather than adjudicating the theological truth of the continuity claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to traditionalist communities, who collect magisterial legitimacy and liturgical space without administering the constraint, placing them near the full-beneficiary end. The papal magisterium is agenda-setter and also the seat where extracted interpretive authority accrues, giving it a beneficiary-leaning directionality despite its administrative burden. Victim declarations map to progressive theologians, liturgical reformers, and national bishops conferences, who bear the costs of restricted speech, restricted liturgical practice, and restricted governance, placing them near the full-target end. Historical-critical scholars are excluded rather than coordinated; their exclusion is a structural requirement for maintaining the continuity narrative's coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading was founded to solve the post-conciliar crisis of authority and identity. For traditionalists and the Roman Curia, this problem remains liveâciting doctrinal confusion and liturgical abuseâjustifying the constraint's persistence as tangled rope. For progressives and independent historians, the problem was resolved by the council itself, making the continuity reading a zombie constraint that extracts obedience long after its founding crisis passed. The contested founding_problem_status blocks clean mandatrophy resolution. The rising theater ratio and extraction accumulation over the measurement interval suggest that even if the coordination function was once dominant, the constraint has layered substantial extractive overhead onto its genuine unifying role. It does not qualify as piton because concentrated beneficiaries (traditionalist communities and the curial apparatus) actively profit from its maintenance and would resist its removal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_genesis_vs_continuity_claim,
    'Does the documented historical genesis of Vatican II textsâas intentionally ambiguous compromises among competing theological factionsâstructurally undermine the continuity reading''s claim of unbroken organic development?',
    'Independent archival history and rhetorical analysis of conciliar drafting records; assessment of whether textual ambiguity was instrumental to compromise or merely apparent.',
    'If the texts are fundamentally overdetermined, the continuity reading functions as an imposed interpretive key rather than a discovered feature of tradition, raising effective extractiveness and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_genesis_vs_continuity_claim, conceptual, 'Whether conciliar drafting history contradicts organic continuity').

omega_variable(
    professional_identity_compliance,
    'Is adherence to the continuity reading among clergy and theologians driven by genuine theological conviction or by professional identity lock, where ordination status, teaching mandates, and career paths depend on not adopting rupture or composite readings?',
    'Post-exit trajectory studies of theologians who leave institutional positions; comparison of stated views before and after exit; analysis of sanction and non-renewal patterns.',
    'If identity-locked, effective suppression is higher than structural measures suggest, and the constraint''s persistence is partly inertial rather than conviction-sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_identity_compliance, empirical, 'Structural versus internalized suppression of alternative readings').

omega_variable(
    liturgical_mandate_binding_status,
    'Does Sacrosanctum Concilium Â§36 impose a binding Latin preservation mandate under the continuity reading, or is this mandate a selectively enforced performative marker of doctrinal obedience?',
    'Canonical analysis of SC Â§36 and its implementing legislation; comparative study of enforcement patterns across dioceses, rites, and jurisdictions.',
    'If selectively enforced as an obedience marker, theater_ratio rises and the liturgical aspect functions as extractive performance rather than genuine coordination of worship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_mandate_binding_status, empirical, 'Whether the Latin mandate is a genuine tradition or performative enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 60, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the continuity reading of the Vatican II magisterial authority kernel. Its siblings (rupture and composite readings) instantiate structurally distinct constraints from the same conciliar event. Each reading carries a distinct epsilon, stakeholder set, and classification. They are linked as a constraint family via network edges, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
