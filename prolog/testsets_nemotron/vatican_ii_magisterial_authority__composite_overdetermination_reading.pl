% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority — Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) produced sixteen documents that simultaneously
 *   encode two incompatible ecclesiological visions: a communio-ecclesiology
 *   of collegiality, subsidiarity, and lay participation (Lumen Gentium,
 *   Gaudium et Spes) and a juridical-ecclesiology of papal primacy, curial
 *   centralization, and clerical hierarchy (the same documents read through
 *   the 1983 Code, the 1985 Extraordinary Synod, and the CDF's hermeneutical
 *   directives). The composite_overdetermination_reading holds that this
 *   overdetermination was not accidental but structurally necessary to
 *   achieve the 90%+ supermajority votes — the texts are compromise
 *   formulations that preserve both visions in productive tension. The real
 *   locus of magisterial authority is not the texts themselves but the
 *   hermeneutical control that determines which vision governs
 *   implementation. Since 1965, extractiveness has risen as curial and
 *   academic offices monetize interpretive primacy (appointments, publishing,
 *   funding), theater has risen as synodal processes perform participation
 *   while outcomes are pre-determined, and suppression has stabilized at
 *   moderate levels through canonical penalties and internalized theological
 *   formation. The 10-12% rejection votes on key texts (Dignitatis Humanae,
 *   Gaudium et Spes, Nostra Aetate) are not dissent noise but the structural
 *   signature of unresolved incompatibility embedded in the final
 *   formulations.
 *
 * KEY AGENTS:
 *   - curial_hermeneutical_office: Primary agenda setter (institutional/arbitrage) — controls interpretive keys, appointments, censorship
 *   - bishops_conference_leadership: Secondary agenda setter / beneficiary (institutional/constrained) — implements synodal flexibility within hermeneutical boundaries
 *   - academic_theologians_establishment: Beneficiary (organized/constrained) — receives funding, positions, publication access for maintaining hermeneutical orthodoxy
 *   - traditionalist_laity: Primary victim (organized/trapped) — bears suppression for continuity reading; exit blocked by identity fusion with pre-conciliar Church
 *   - progressive_laity: Primary victim (organized/trapped) — bears suppression for rupture reading; exit blocked by identity fusion with conciliar promise
 *   - parish_pastors_caught_between: Secondary victim (moderate/constrained) — implements contradictory directives from both visions; bears coordination costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.48).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority — Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'd2f8043f-c113-4c3c-b2c7-dc6dc9a56e10').
narrative_ontology:cs_kernel_codification('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', fixed_text).
narrative_ontology:cs_authority_grounding('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10').
narrative_ontology:cs_reading_relation('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', foundational, conciliar_texts_are_overdetermined_composites).
narrative_ontology:cs_axiom_status(conciliar_texts_are_overdetermined_composites, holdable).
narrative_ontology:cs_axiom_grounding('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', conciliar_texts_are_overdetermined_composites, empirically_contingent).
narrative_ontology:cs_axiom('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', foundational, hermeneutical_control_is_locus_of_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', hermeneutical_control_is_locus_of_authority, conventional).
narrative_ontology:cs_reference_frame('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', conciliar_textual_corpus).
narrative_ontology:cs_drift_state('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', post_synodal_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d2f8043f-c113-4c3c-b2c7-dc6dc9a56e10', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_office).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, bishops_conference_leadership).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, parish_pastors_caught_between).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_hermeneutical_primacy).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_interpretive_monopoly).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, synodal_implementation_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretive keys for all conciliar texts through CDF notifications, papal audiences, appointment of bishops, and censorship (nihil obstat/imprimatur). Collects interpretive rents: curial promotions, pontifical university rectorships, consultancy fees, publishing contracts. Exit is arbitrage-grade: can rotate to diplomatic posts, academic chairs, or retirement with full pensions. The office sets the hermeneutical agenda that all other seats must navigate.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_office, agenda_setter,
    institutional, generational, arbitrage, universal).

% Implements synodal processes and pastoral plans within the hermeneutical frame set by Rome. Gains implementation flexibility (inculturation, liturgical adaptation, pastoral priorities) but must submit final documents for recognitio. Receives funding and institutional legitimacy from maintaining communion. Exit is constrained: leaving communion means losing diocesan infrastructure, priestly faculties, and canonical standing. Caught between curial directives and local pressures.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, bishops_conference_leadership, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, bishops_conference_leadership, agenda_setter).

% Holds chairs at pontifical universities, directs theological journals, sits on CDF consultative bodies, receives research grants from ecclesiastical foundations. Career advancement depends on demonstrating fidelity to the current hermeneutical synthesis. Exit is constrained: moving to secular universities loses ecclesiastical credential; independent scholarship loses institutional access. Theological formation internalizes the hermeneutical frame as epistemic virtue.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_establishment, beneficiary,
    organized, biographical, constrained, global).

% Identifies with pre-conciliar ecclesiology (Tridentine liturgy, papal monarchy, doctrinal immutability). Bears suppression: marginalized in parishes, denied traditional liturgy (until limited 2007/2021 permissions), labeled schismatic for resisting conciliar implementation. Exit is identity-locked: their self-concept is constituted through fidelity to the pre-conciliar Church; leaving feels like apostasy. Sedevacantism and SSPX are internal exit options that reproduce the same identity structure. The 10-12% rejection votes on Dignitatis Humanae and Gaudium et Spes reflect their structural position.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_laity, payer,
    organized, generational, identity_locked, global).

% Identifies with conciliar promise (collegiality, religious liberty, lay participation, ecumenism). Bears suppression: pastoral initiatives blocked by curial review, synodal outcomes pre-determined, women's ordination and married priesthood declared non-negotiable. Exit is identity-locked: their self-concept is constituted through the conciliar vision of a reformed Church; leaving feels like betraying the Council. Independent Catholic movements and Protestantism are external exits that reproduce the same hope structure. The same 10-12% rejection votes reflect their position from the opposite pole.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_laity, payer,
    organized, generational, identity_locked, global).

% Implements contradictory directives daily: synodal listening sessions whose outcomes are overridden; liturgical adaptations that require recognitio they cannot get; pastoral care for divorced/remarried and LGBTQ+ parishioners under doctrinal formulations that exclude them. Bears coordination costs (time, emotional labor, credibility loss) without capturing any interpretive rent. Exit is constrained: priestly identity is vocationally fused; laicization carries stigma and loss of ministry. They are the operational layer where the overdetermination becomes lived contradiction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, parish_pastors_caught_between, payer,
    moderate, biographical, constrained, local).

% Historians, sociologists, and comparative theologians who study the conciliar reception without ecclesial stake. They see the full structural pattern: the overdetermined texts, the hermeneutical control, the divergent implementations, the identity-locked victims on both poles. Their analysis is the analytical seat from which the constraint's composite structure is visible.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_office).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains ecclesial unity across 1.4 billion Catholics by providing a shared textual corpus and hermeneutical authority that can absorb divergent implementations without formal schism. The overdetermined texts function as a coordination substrate: both continuity and rupture readings can claim conciliar legitimacy, preventing either from declaring the other outside the Church.
% TRANSFER_FUNCTION: Moves interpretive authority, curial appointments, academic positions, publishing access, and NGO funding from the laity and parish clergy (who bear the implementation costs) to the curial hermeneutical office and its allied theologians (who control the hermeneutical keys). The 10-12% rejection votes represent the tax paid by both poles to maintain the synthesis.
% ABSENT_VOICES: The Global South episcopate (outside the Roman curial orbit) and the non-ordained baptized (especially women) are structurally excluded from the hermeneutical decision-making. They would object to the European-centric synthesis and the clerical monopoly on interpretation, but the synodal machinery admits them only as listeners, not as authors of the hermeneutical frame.
% DISAPPEARANCE_RATIONALE: If the magisterial hermeneutical monopoly vanished overnight, the conciliar texts would be read directly by local churches without curial mediation. Traditionalist and progressive implementations would diverge openly rather than through coded ambiguity. Schism would likely formalize within a decade. The curial offices would lose their primary source of authority and resource control. The coordination substrate would collapse into explicit pluralism.
% FOUNDING_PROBLEM: The Church faced a crisis of modernity: loss of temporal power, secularization of Europe, Protestant fragmentation, colonial collapse, and the need to articulate a coherent Catholic identity in a pluralistic world. The Council was convened to achieve aggiornamento (updating) while preserving doctrinal integrity.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalists attest the problem was solved by continuity (the Church already had the answer). Progressives attest the problem was betrayed (the Council promised rupture that was retrieved). The curial synthesis attests the problem is ongoing (synodality is the new aggiornamento). No party outside the benefiting curial/academic structure corroborates a single status; the contestation is the structural fact.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the resource capture by hermeneutical offices: curial appointments, pontifical university chairs, publishing contracts, and NGO funding all flow to those who maintain the official synthesis. The theater ratio (0.55) captures the synodal machinery — listening processes, continental assemblies, final documents — that performs participation while the hermeneutical frame is fixed in advance. Suppression (0.48) is moderate because formal penalties (censure, removal) are rare but internalized formation makes dissent structurally unthinkable for most clergy. Accessibility collapse (0.38) is low: alternatives exist (sedevacantism, independent Catholicism, Orthodoxy, Protestantism) but carry prohibitive identity costs. Resistance (0.57) is significant: traditionalist and progressive laity both resist the synthesis from opposite directions, and parish clergy resist the implementation burden. The rising extractiveness and theater over the interval show mandatrophic drift: the coordination function (ecclesial unity through conciliar reception) has atrophied while the extraction function (hermeneutical rent) has grown.
 *
 * PERSPECTIVAL GAP:
 *   From the curial seat, the constraint is a rope: it coordinates global ecclesial unity through a shared hermeneutic. From the traditionalist seat, it is a snare: the continuity reading is the cover for a rupture that has already occurred. From the progressive seat, it is a snare: the rupture reading is the promise betrayed by continuity retrieval. From the parish pastor seat, it is a tangled rope: they must coordinate both visions simultaneously in daily ministry. The engine computes this divergence; the claimed_type=tangled_rope reflects the structural hybridity visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The curial hermeneutical office is the primary beneficiary (d ~ 0.15): it collects interpretive rents, controls the agenda, and faces arbitrage-grade exit (can move to academia, diplomacy, retirement). Bishops conferences are secondary beneficiaries (d ~ 0.35): they gain implementation flexibility but remain constrained by curial review. Academic theologians are beneficiaries (d ~ 0.30) but constrained by appointment dependence. Traditionalist laity are primary targets (d ~ 0.85): identity-locked to pre-conciliar ecclesiology, suppressed by the rupture-reading implementation. Progressive laity are primary targets (d ~ 0.80): identity-locked to conciliar promise, suppressed by the continuity-reading retrieval. Parish pastors are secondary targets (d ~ 0.65): they bear the coordination costs of implementing contradictory directives with constrained exit (vocational identity lock). The engine will compute per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was ecclesial renewal and engagement with modernity (aggiornamento). That problem is contested: traditionalists say it was solved by continuity; progressives say it was betrayed; the curial synthesis says it is ongoing. The constraint persists because the hermeneutical offices extract enough to maintain it, but no party benefits enough to fix the underlying incompatibility. The theater ratio rising above 0.5 signals that synodal performance has replaced substantive reception — the coordination function is now the cover for the extraction function. This is mandatrophy: the mandate (conciliar renewal) has outlived its function, but the constraint (magisterial hermeneutical monopoly) persists through institutional inertia and theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the vatican_ii_magisterial_authority kernel, and what structural elements distinguish it from sibling readings?',
    'Structural comparison of beneficiary/victim sets, extractiveness profiles, and cs_structure axioms across the three declared readings. The composite_overdetermination_reading claims the constraint IS the overdetermined text itself; continuity and rupture readings claim the constraint is the hermeneutical key that resolves it.',
    'If the readings are structurally distinct constraints (different ε, different parties, different type), the kernel decomposition is validated. If they collapse to one constraint, the kernel frame is misapplied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings instantiate three distinct constraints or one constraint with observational variance.').

omega_variable(
    hermeneutical_control_as_extraction,
    'Does control of conciliar interpretation function as extraction (rent collection by curial/academic offices) or as genuine coordination (preserving ecclesial unity)?',
    'Empirical analysis of resource flows: do interpretive offices receive funding, appointments, publication access, or career advancement contingent on maintaining hermeneutical primacy? Does unity actually hold, or does fragmentation increase despite interpretive control?',
    'If extraction, the constraint is tangled_rope or snare. If coordination, it is rope or scaffold. The claimed_type=tangled_rope assumes a hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_control_as_extraction, empirical, 'Whether hermeneutical monopoly is a coordination service or an extractive position.').

omega_variable(
    implementation_divergence_intentionality,
    'Is the post-conciliar implementation divergence a structural feature (designed flexibility) or a bug (failure of reception)?',
    'Archival research on conciliar voting records, periti interventions, and post-conciliar curial directives. The 10-12% rejection votes on key texts (e.g. Gaudium et Spes 67-72, Dignitatis Humanae 70-75) signal intentional ambiguity; but whether that ambiguity was strategic compromise or theological incapacity is unresolved.',
    'If intentional, the constraint is a designed tangled_rope (coordination via ambiguity). If unintentional, it is a degraded rope becoming piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_divergence_intentionality, conceptual, 'Whether textual ambiguity was a feature or failure of the conciliar process.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (canonical penalties, appointment control, publication censorship) or internalized (theological formation that makes dissent unthinkable)?',
    'Post-exit trajectory study: track theologians and clergy who publicly dissent from official hermeneutics. If suppression persists after formal penalties are removed (self-censorship, career avoidance, identity fusion with magisterial fidelity), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint carries its own enforcement into the agent''s cognitive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the magisterial authority constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_tr_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_be_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(vatican_ii_magisterial_authority__composite_overdetermination_reading_su_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, synodal_process_2021_2024).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, clerical_formation_magisterial_fidelity).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the vatican_ii_magisterial_authority constraint family. The composite_overdetermination_reading is the upstream structural claim: the texts themselves are overdetermined. The continuity and rupture readings are downstream hermeneutical keys that each claim to resolve the overdetermination but structurally depend on it. All three share the same referent (the conciliar corpus) but author different ε, different beneficiary/victim structures, and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
