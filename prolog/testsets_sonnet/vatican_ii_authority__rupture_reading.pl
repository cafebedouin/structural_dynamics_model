% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Rupture Reading of Vatican II Authority (Sedevacantist/Traditionalist Break Claim)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Since the mid-1970s a network of clergy and laity, most visibly organized
 *   under the Society of St. Pius X after Archbishop Lefebvre's 1976
 *   suspension and the 1988 episcopal consecrations performed without papal
 *   mandate, has held that Vatican II's teachings on religious liberty,
 *   ecumenism, and collegiality cannot be reconciled with prior papal
 *   condemnations (e.g. Pius IX's Quanta Cura, Leo XIII and Pius XI on the
 *   confessional state) and that the Council therefore represents a doctrinal
 *   rupture rather than a development. This reading functions as a
 *   coordinating identity for traditionalist institutions but also imposes
 *   real sacramental and psychological costs on laity and disciplined clergy
 *   who cannot independently adjudicate the underlying theological dispute.
 *
 * KEY AGENTS:
 *   - sspx_leadership: administers the rupture-based parallel structure
 *   - traditionalist_clergy_networks: benefit institutionally from the crisis framing
 *   - traditional_catholic_laity_seeking_communion: bear sacramental uncertainty as payers
 *   - priests_disciplined_for_rejecting_council: bear career cost as payers
 *   - vatican_curial_authorities: excluded object of the rupture critique
 *   - theological_historians: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.58).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Rupture Reading of Vatican II Authority (Sedevacantist/Traditionalist Break Claim)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '2fd110e5-154d-4d5d-8e49-21c1b621da73').
narrative_ontology:cs_kernel_codification('2fd110e5-154d-4d5d-8e49-21c1b621da73', fixed_text).
narrative_ontology:cs_authority_grounding('2fd110e5-154d-4d5d-8e49-21c1b621da73', lineage).
narrative_ontology:cs_interpretation_layer_present('2fd110e5-154d-4d5d-8e49-21c1b621da73').
narrative_ontology:cs_reading_relation('2fd110e5-154d-4d5d-8e49-21c1b621da73', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2fd110e5-154d-4d5d-8e49-21c1b621da73', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('2fd110e5-154d-4d5d-8e49-21c1b621da73', foundational, conciliar_teaching_reconcilable_with_prior_anathemas_is_false).
narrative_ontology:cs_axiom_status(conciliar_teaching_reconcilable_with_prior_anathemas_is_false, holdable).
narrative_ontology:cs_axiom_grounding('2fd110e5-154d-4d5d-8e49-21c1b621da73', conciliar_teaching_reconcilable_with_prior_anathemas_is_false, deontological).
narrative_ontology:cs_axiom('2fd110e5-154d-4d5d-8e49-21c1b621da73', foundational, papal_authority_conditioned_on_doctrinal_continuity).
narrative_ontology:cs_axiom_status(papal_authority_conditioned_on_doctrinal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('2fd110e5-154d-4d5d-8e49-21c1b621da73', papal_authority_conditioned_on_doctrinal_continuity, conventional).
narrative_ontology:cs_reference_frame('2fd110e5-154d-4d5d-8e49-21c1b621da73', pre_conciliar_magisterial_settlement).
narrative_ontology:cs_drift_state('2fd110e5-154d-4d5d-8e49-21c1b621da73', post_1988_consecrations_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2fd110e5-154d-4d5d-8e49-21c1b621da73', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, traditionalist_clergy_networks).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, sspx_leadership).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_laity_seeking_communion).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, priests_disciplined_for_rejecting_council).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, extra_ecclesiam_nulla_salus_strict_reading).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, immutability_of_conciliar_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers a parallel clerical and sacramental structure premised on the claim that Vatican II's documents (especially on religious liberty, ecumenism, and collegiality) are gravely defective or erroneous. Ordains and assigns priests, runs seminaries, and adjudicates who counts as authentically Catholic under this reading. Benefits organizationally from members who accept the rupture framing as the price of doctrinal certainty; can shift positions (regularize, negotiate with Rome, or harden) with relative institutional freedom.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_leadership, agenda_setter,
    organized, generational, arbitrage, global).

% Priests and communities who derive their institutional identity, vocation, and community cohesion from asserting that the post-conciliar Church broke with prior teaching. Their authority, publishing platforms, and donor base depend on maintaining the rupture claim as unresolved crisis rather than settled question.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy_networks, beneficiary,
    organized, generational, constrained, continental).

% Lay Catholics attracted to pre-conciliar liturgy and doctrine who are told that accepting the Council's validity means accepting error, and that rejecting it means uncertain sacramental status (irregular marriages, disputed absolutions, contested confirmations) within the rupture framework. They bear the psychological and sacramental cost of a schism-adjacent identity without the resources to independently adjudicate the theological dispute.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_laity_seeking_communion, payer,
    moderate, biographical, trapped, national).

% Clergy who adopt the rupture position and face suspension, laicization, or loss of faculties from diocesan authorities. Their careers and clerical status are destroyed by the same claim that gives traditionalist networks their identity; they carry the material cost of a doctrinal position whose institutional benefits accrue mainly to network leadership.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, priests_disciplined_for_rejecting_council, payer,
    powerless, biographical, trapped, national).

% Hold that the Council is a valid ecumenical council in doctrinal continuity with tradition and treat the rupture claim as a canonical and doctrinal error to be corrected through dialogue (the 2009 Ecclesia Dei processes, the 1988 excommunications and their partial lifting). Their continuity position is the one this reading is defined against; they are structurally excluded from the internal rupture-reading discourse except as an object of critique.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_curial_authorities, excluded,
    institutional, civilizational, analytical, global).

% Scholars who examine the conciliar texts, drafting history, and reception against prior magisterial teaching. Some corroborate specific rupture claims on particular texts (e.g. religious liberty vs. prior condemnations of indifferentism) while rejecting the totalizing crisis narrative; their work is cited selectively by all three kernel readings.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, theological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, traditionalist_clergy_networks).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinally coherent identity and sacramental community for Catholics who experience post-conciliar liturgical and doctrinal changes as a genuine rupture, coordinating dispersed traditionalist resistance into stable parishes, seminaries, and publishing networks.
% TRANSFER_FUNCTION: Moves donor resources, vocations, and lay allegiance away from diocesan structures toward traditionalist institutions; moves psychological certainty and sacramental confidence away from laity uncertain of their status and toward network leadership who administer the crisis narrative.
% ABSENT_VOICES: Conciliar-era periti and bishops who drafted the documents and could testify to continuity intent are largely deceased; contemporary curial theologians offering continuity readings are excluded from the internal traditionalist discourse as compromised parties rather than engaged as interlocutors.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished, SSPX-aligned structures would need to either regularize with Rome or reconstitute their identity around a different grievance; traditionalist clergy networks argue the underlying doctrinal problems (they claim) would persist unaddressed even if the reading itself disappeared, while curial authorities and continuity-reading Catholics hold that ordinary communion would simply resume without loss.
% FOUNDING_PROBLEM: Rapid liturgical reform (the Novus Ordo Mass), the Declaration on Religious Liberty (Dignitatis Humanae), and ecumenical/collegial reorientation appeared to a segment of clergy and laity in the late 1960s and 1970s to contradict prior anathemas and papal condemnations of religious liberty and indifferentism, producing an acute crisis of authority for those who held the prior magisterium as unchangeable.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist theologians (e.g. within SSPX scholarship) attest the doctrinal contradiction is real and unresolved. Independent historians of Vatican II (including scholars not aligned with either traditionalist or progressive factions) corroborate that specific textual tensions exist between Dignitatis Humanae and prior 19th-century papal condemnations, while disputing that this rises to formal doctrinal error; the Vatican's own Ecclesia Dei and later Ecclesia Dei Commission processes implicitly corroborate that a real pastoral and doctrinal dispute exists by having negotiated with SSPX for decades rather than treating the matter as closed.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the rupture reading, once adopted, generates ongoing donor and vocational flows to traditionalist institutions that depend on the crisis remaining unresolved rather than settled — a genuine coordination function (stable traditionalist worship communities) exists alongside asymmetric extraction (laity and junior clergy bear sacramental and career risk that network leadership does not). Suppression is authored at 0.62, falling somewhat after the 1988 consecrations as parallel structures matured and needed less active enforcement of internal discipline, then stabilizing as the SSPX canonical-status question settled into a durable stalemate rather than resolving. Theater ratio rises modestly (0.2 to 0.4) as the movement's institutional apparatus (publishing houses, seminaries, canonical negotiations) grows relative to the underlying theological dispute's live content.
 *
 * DIRECTIONALITY LOGIC:
 *   SSPX leadership and the traditionalist clergy networks sit near the beneficiary end: they administer the reading, derive institutional identity and resources from its persistence, and have arbitrage-grade or organized exit options (they can negotiate regularization or maintain separation as suits institutional interest). Laity seeking communion and disciplined priests sit near the target end: trapped exit options, their sacramental status and clerical careers are put at risk by a doctrinal claim whose institutional benefits flow mainly to network leadership. Curial authorities are excluded from the internal discourse of this reading by construction — the reading is defined in opposition to their continuity position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (perceived doctrinal contradiction between conciliar texts and prior condemnations) was live and specific in 1965-1976. Whether it remains live is itself the contested six_questions answer: SSPX and traditionalist networks assert it remains live and unaddressed; curial authorities and continuity-reading theologians hold that decades of clarification (including the 2000 Dominus Iesus and ongoing doctrinal dialogue with SSPX) have substantially addressed the specific textual tensions, leaving the rupture reading's institutional apparatus persisting past the point its founding grievance was live in its original form — a mandatrophy signature, though contested rather than settled, since the underlying theological dispute (religious liberty vs. prior condemnations) has not received a resolution accepted by all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_reading_kernel_position,
    'Is the rupture reading a correct diagnosis of genuine doctrinal contradiction, or is it a constructed crisis narrative that sustains a parallel institutional economy? This is one of three declared readings of the vatican_ii_authority kernel (continuity_reading, rupture_reading, composite_overdetermination_reading); each is authored as a separate ε-invariant constraint.',
    'Would require either (a) a magisterial act universally recognized across all three reading-communities as authoritative on the specific contested texts (unlikely given that authority itself is the disputed kernel), or (b) independent historical-theological consensus on whether Dignitatis Humanae''s teaching is formally reconcilable with Quanta Cura and prior confessional-state doctrine — a question theological historians disagree on even outside the traditionalist/progressive divide.',
    'If the contradiction is real and formal, the rupture reading''s beneficiary/victim structure inverts: traditionalist networks become genuine doctrinal defenders rather than extractive institutions, and curial authorities become the ones vindicating error. If the contradiction is not formal (development-compatible), the current classification (tangled_rope: real coordination function, asymmetric extraction riding on it) stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_reading_kernel_position, conceptual, 'Whether the rupture reading''s core doctrinal claim is true is unresolvable within this story and is routed to the kernel-level contest across the three sibling readings.').

omega_variable(
    sspx_canonical_status_trajectory,
    'Does the ongoing (as of composition) partial rapprochement between Rome and SSPX (lifted excommunications 2009, structural dialogue) represent the rupture reading''s gradual dissolution into the continuity reading, or a durable stable equilibrium in which the rupture reading persists indefinitely alongside partial canonical recognition?',
    'Track SSPX canonical status and internal doctrine over subsequent decades; convergence toward full regularization without doctrinal concession would support dissolution, while stable semi-recognized status would support persistence.',
    'Affects whether the constraint''s classification should shift toward scaffold (transitional, with an implicit sunset as rapprochement completes) versus its current tangled_rope reading (stable extractive coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sspx_canonical_status_trajectory, empirical, 'Whether current Rome-SSPX dialogue trends toward resolution or stable schism-adjacent equilibrium.').

omega_variable(
    internalized_vs_structural_suppression_laity,
    'For laity trapped in sacramental uncertainty under the rupture reading, is the suppression they experience structural (genuine canonical ambiguity about sacramental validity) or internalized (psychological fusion of Catholic identity with the traditionalist crisis narrative, persisting even where canonical remedies exist)?',
    'Compare laity who formally regularize (e.g. via Ecclesia Dei societies in full communion) and observe whether their sense of doctrinal crisis persists post-regularization; persistence would indicate internalization.',
    'If substantially internalized, the effective suppression experienced by trapped laity exceeds the structural canonical facts, meaning the constraint''s true suppressive force is understated by the authored 0.62 figure for this population specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression_laity, empirical, 'Structural vs internalized suppression mechanism for laity within the rupture reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__rupture_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__rupture_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__rupture_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__rupture_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__rupture_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__rupture_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__rupture_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__rupture_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__rupture_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__rupture_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__rupture_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__rupture_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__rupture_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__rupture_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__rupture_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'Vatican II authority' per the ε-invariance principle: continuity_reading (Mountain/Rope-leaning: organic development, negligible contradiction), rupture_reading (this story: Tangled Rope — real coordinating identity function plus asymmetric extraction from trapped laity and disciplined clergy), and composite_overdetermination_reading (structurally ambiguous composite that resists either resolution). Each has its own ε, beneficiary/victim structure, and claimed_type; they are linked here rather than merged because measuring 'the same constraint' three different ways produced three different epsilon values — the ε-invariance test requires decomposition, not a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
