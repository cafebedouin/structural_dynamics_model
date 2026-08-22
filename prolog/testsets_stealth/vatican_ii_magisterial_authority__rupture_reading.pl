% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Rupture Reading: Conciliar Texts as Superseding Magisterial Baseline
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the Vatican II
 *   magisterial-authority kernel: the conciliar texts encode an ecclesiology
 *   incompatible with prior magisterial teaching, pre-conciliar positions
 *   (the error-has-no-rights framework for church-state relations,
 *   pre-conciliar liturgical exclusivity) are superseded, and the
 *   contradiction is acknowledged as doctrinal progress rather than
 *   concealed. The standing arrangement under contest, the post-conciliar
 *   magisterial regime that binds a global communion to the conciliar
 *   baseline, is assessed by this reading's own lights: the texts genuinely
 *   authorize the renewal they delivered, so the arrangement's coordination
 *   function is real (vernacular worship, doctrinal unity, ecumenical
 *   engagement), while its costs fall asymmetrically on the minority attached
 *   to the pre-conciliar forms, maintained by active canonical enforcement
 *   whose intensity has oscillated with successive pontificates. Claimed type
 *   and metrics are authored independently: the claim is tangled_rope (real
 *   coordination plus asymmetric extraction under enforcement); the metrics
 *   describe moderately extractive, actively enforced, only lightly
 *   theatrical operation. Epsilon's referent is the standing post-conciliar
 *   arrangement as this reading assesses it, not the pre-conciliar order this
 *   reading rejects and not an idealized alternative. KEY AGENTS (by
 *   structural relationship): holy_see_and_roman_curia: agenda setter
 *   (institutional/constrained), administers interpretation and enforcement,
 *   collects jurisdictional gains; diocesan_bishops_conferences: secondary
 *   beneficiary (institutional/constrained), gained collegial authority, bore
 *   implementation; ordinary_catholic_laity: primary beneficiary class
 *   (moderate/identity_locked), receives vernacular worship and liberty
 *   protections, bears diffuse transition costs; progressive_theologians:
 *   beneficiary (moderate/mobile), field reshaped around their project;
 *   non_catholic_christian_communities and
 *   non_christian_religious_communities: external beneficiaries
 *   (organized/mobile), recognized partners not governed by the arrangement;
 *   traditionalist_clergy_networks: primary target
 *   (organized/identity_locked), absorbs canonical penalties and
 *   displacement; traditionalist_laity: target (powerless/identity_locked),
 *   restricted access to inherited liturgy; sedevacantist_communities:
 *   excluded voice (powerless/identity_locked), severed communion, absent
 *   from deliberation; council_history_scholars: analytical observer
 *   (analytical/analytical), reconstructs drafting histories cited by all
 *   parties.
 *
 * KEY AGENTS:
 *   - holy_see_and_roman_curia: agenda setter (institutional/constrained) — administers interpretation and enforcement, collects jurisdictional gains
 *   - diocesan_bishops_conferences: secondary beneficiary (institutional/constrained) — gained collegial authority, bore implementation costs
 *   - ordinary_catholic_laity: primary beneficiary class (moderate/identity_locked) — receives vernacular worship and liberty protections, bears diffuse transition costs
 *   - progressive_theologians: beneficiary (moderate/mobile) — discipline reshaped around their project
 *   - non_catholic_christian_communities: external beneficiary (organized/mobile) — recognized partner, not governed
 *   - non_christian_religious_communities: external beneficiary (organized/mobile) — acknowledged, not governed
 *   - traditionalist_clergy_networks: primary target (organized/identity_locked) — absorbs canonical penalties and displacement
 *   - traditionalist_laity: target (powerless/identity_locked) — restricted access to inherited liturgy
 *   - sedevacantist_communities: excluded voice (powerless/identity_locked) — severed communion, absent from deliberation
 *   - council_history_scholars: analytical observer (analytical/analytical) — reconstructs drafting histories cited by all parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.52).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.66).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Rupture Reading: Conciliar Texts as Superseding Magisterial Baseline").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '41ffb26f-96a2-4c27-b74a-0efbd3bb8073').
narrative_ontology:cs_kernel_codification('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', fixed_text).
narrative_ontology:cs_authority_grounding('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', lineage).
narrative_ontology:cs_interpretation_layer_present('41ffb26f-96a2-4c27-b74a-0efbd3bb8073').
narrative_ontology:cs_reading_relation('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', foundational, dh_contradicts_prior_teaching).
narrative_ontology:cs_axiom_status(dh_contradicts_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', dh_contradicts_prior_teaching, empirically_contingent).
narrative_ontology:cs_axiom('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', foundational, supersession_is_doctrinal_progress).
narrative_ontology:cs_axiom_status(supersession_is_doctrinal_progress, holdable).
narrative_ontology:cs_axiom_grounding('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', supersession_is_doctrinal_progress, instrumental).
narrative_ontology:cs_axiom('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', secondary, liturgical_experimentation_legitimate).
narrative_ontology:cs_axiom_status(liturgical_experimentation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', liturgical_experimentation_legitimate, conventional).
narrative_ontology:cs_reference_frame('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', conciliar_superseding_baseline).
narrative_ontology:cs_drift_state('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', contemporary_post_traditionis_custodes, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('41ffb26f-96a2-4c27-b74a-0efbd3bb8073', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, holy_see_and_roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, diocesan_bishops_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ordinary_catholic_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, non_catholic_christian_communities).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, non_christian_religious_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_networks).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, diocesan_bishops_conferences).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, hermeneutic_of_discontinuity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, acknowledged_doctrinal_contradiction_as_progress).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, aggiornamento_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgated the conciliar texts and administers their interpretation and enforcement: the doctrinal congregation adjudicates disputed teaching, the liturgical dicastery approves texts and restricts or permits the older missal, and the secretariat of state manages relations with groups that resist the settlement. Collects jurisdictional gains: a uniform ordinary liturgy, definitional authority over what the texts mean, and disciplinary leverage over clergy. Its room to maneuver is bounded by its own claims, since it cannot repudiate the Council without discrediting the office, nor fully suppress traditionalist networks without risking visible schism.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, holy_see_and_roman_curia, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, holy_see_and_roman_curia, beneficiary).

% Gained recognized collegiality, authority over liturgical translation and pastoral adaptation, and a larger share of governance relative to Rome. Also carried implementation: rebuilding catechesis, managing the liturgical transition in parishes, and handling clergy and laity attached to the previous forms. Their discretion expands or contracts with each Roman liturgical decree.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, diocesan_bishops_conferences, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, diocesan_bishops_conferences, payer).

% Receive worship in vernacular languages, expanded lay ministries, lectionary-based scripture exposure, and a magisterium that affirms religious liberty and conscience protections. They bear diffuse costs: several decades of liturgical and catechetical flux and parish-level turbulence during implementation. Leaving the communion would mean losing sacramental life, community, and inherited identity, so most remain regardless of satisfaction with particular changes.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ordinary_catholic_laity, beneficiary,
    moderate, biographical, identity_locked, global).

% Work in a discipline whose post-conciliar shape vindicates their project: doctrinal development, historical-critical method applied to tradition, ecumenical and interreligious engagement. Career paths, journals, and faculty posts opened around conciliar implementation. Individual disciplines occurred at moments, but the settlement's direction matched their agenda.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    moderate, biographical, mobile, global).

% Were reclassified from adversaries to separated brethren with whom the Church seeks restored unity; they gained bilateral dialogues, joint declarations, and observer invitations to Catholic synods. They are not governed by the magisterium and participate voluntarily; their own doctrines remain untouched.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, non_catholic_christian_communities, beneficiary,
    organized, generational, mobile, global).

% Gained explicit acknowledgment that the Church rejects nothing of what is true and holy in their traditions, plus structured interreligious dialogue channels. Like the Christian partners, they sit outside the enforcement perimeter and incur no obligation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, non_christian_religious_communities, beneficiary,
    organized, generational, mobile, global).

% Priests and seminaries organized around the pre-conciliar liturgy and doctrinal corpus, the Society of St. Pius X most prominently, plus institutes erected to operate the older forms with canonical approval. They have absorbed suspensions, excommunication decrees later lifted for the Society's bishops, prohibition of ordinations, and repeated renegotiation of their legal status. Their vocation is constituted by attachment to the pre-conciliar form; abandoning it would dissolve the identity that drew them to priesthood.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_networks, payer,
    organized, generational, identity_locked, global).

% Faithful attached to the Latin liturgical tradition, concentrated in particular dioceses and countries. After 2021 they face restricted access to the older mass in their own parishes, dependence on dispensations, and in some cases travel to distant approved sites. Their religious identity is fused with the older form; adopting the ordinary liturgy feels to many like losing the religion itself.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity, payer,
    powerless, biographical, identity_locked, regional).

% Small groups that concluded the post-conciliar popes are not valid popes and severed communion entirely. They publish critiques and maintain chapels, and they are structurally absent from every magisterial conversation: no synod, congregation, or dialogue solicits their position. Their absence is total rather than negotiated.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, sedevacantist_communities, excluded,
    powerless, generational, identity_locked, global).

% Historians and theologians, including the Bologna school's multi-volume History of Vatican II, who reconstructed the council's drafting histories and documented the hermeneutic dispute between continuity and discontinuity readings. They publish analyses that all parties cite, hold no stake in enforcement, and can adopt whichever reading the evidence supports.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, council_history_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, holy_see_and_roman_curia).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single doctrinal, liturgical, and disciplinary baseline across a global communion: one ordinary rite in vernacular languages, shared catechetical norms, unified teaching authority, and common protocols for engaging other Christians and religions, which a body of 1.4 billion cannot dispense with.
% TRANSFER_FUNCTION: Moves liturgical form and doctrinal allegiance from the pre-conciliar corpus to the conciliar corpus; moves ministerial legitimacy and canonical security to clergy who conform and irregularity or displacement to those who do not; moves liturgical decision-making outward from Rome to episcopal conferences while keeping definitional authority at the center.
% ABSENT_VOICES: Sedevacantist communities and unreconciled traditionalists are structurally absent: no synodal or congregational process solicits their position. The pre-conciliar magisterium itself cannot answer, since the drafters of Quanta Cura and the Syllabus framework are not present to concede or contest that their teaching was superseded. Lay traditionalists lacked formal representation in the synodal processes that shaped implementation.
% DISAPPEARANCE_RATIONALE: If the conciliar settlement vanished overnight, the Church would revert to a pre-conciliar baseline: Latin-only liturgy, the pre-1962 doctrinal posture toward other Christians and religions, collapse of fifty years of ecumenical dialogues and joint declarations, and immediate canonical crisis for every cleric ordained under the revised forms. Every diocese, seminary, and parish would rearrange.
% FOUNDING_PROBLEM: John XXIII convened the Council against a cluster of problems: the Church's fortified defensive posture toward modern societies, liturgy experienced as distant from participants, the unfinished scandal of Christian division, the obsolescence of the confessional-state framework for church-state relations, and stagnant catechesis.
% FOUNDING_PROBLEM_CORROBORATION: Non-party sources attest the problems were and remain real: secular historians of the period document the pre-conciliar impasse; Orthodox and Protestant observers at the council attested the division problem; sociologists of religion document continuing secularization and liturgical-participation deficits. Even the sharpest critics of the conciliar solution, the traditionalist networks, attest the founding problems existed, disputing the remedy rather than the diagnosis. No corroborating source claims the problems were already solved before the Council.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.52: the arrangement delivers real goods across the broad body of the governed (accessible vernacular worship, religious-liberty protections, ecumenical peace) while concentrating heavy costs on a small identity-locked minority (canonical irregularity, restricted access to the inherited liturgy) and light diffuse costs on everyone else (six decades of liturgical and catechetical flux, unconditional doctrinal assent to the new baseline). Suppression 0.66: persistence depends on active canonical machinery, including mandated liturgical conformity, suspension and excommunication decrees, and permission regimes for the older missal, rather than on voluntary consensus; dissent is tolerated at the margins but penalized at the center. Theater 0.26: the core functions (worship, teaching, dialogue) are performed for real; the performative share is anniversary ritualization and rhetorical invocations of the council that float free of the texts. Accessibility_collapse 0.45: alternatives persist but at identity cost, including approved traditionalist institutes, the Society of St. Pius X's irregular parallel structures, Eastern Catholic rites, and outright exit. Resistance 0.55: decades of organized traditionalist resistance, the 1988 consecration crisis, and the academic hermeneutic dispute that produced the continuity counter-reading. All three temporal series run on one shared grid (T=0 to 60, mapping 1962 to 2022); the suppression series traces one full enforcement cycle (implementation tightening, indult accommodation after Ecclesia Dei, Summorum Pontificum liberalization, Traditionis Custodes re-tightening) driven by alternating administrations' hermeneutic commitments. The oscillation is political contingency of the Roman seat rather than engineered intermittent reinforcement, though its effect on the identity-locked minority functions like intermittent reinforcement in practice, since each reversal resets the minority's costs unpredictably. Fixing the arrangement in either direction (full rupture implementation or pre-conciliar restoration) would fracture the communion, hence prohibitive fixing cost; the gains of enforcement (liturgical uniformity, definitional control, disciplinary leverage) demonstrably accrue to the Apostolic See, hence the named receipt seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. The Holy See experiences the arrangement as its own legitimate teaching action: it promulgated the texts, defines their meaning, and collects the jurisdictional gains, so its computed extraction sits near the subsidy end despite bearing administrative burden. The identity-locked payer seats compute the steepest extraction: traditionalist clergy and laity cannot exit without dissolving the vocation or religious identity that constitutes them, which pushes their directionality toward the full-target end and amplifies their effective extraction well above the aggregate epsilon. The external beneficiary seats (non-Catholic Christians, non-Christian religions) compute near-zero cost: they receive recognition and dialogue without being governed, and their mobile exit makes the arrangement a pure subsidy from their side. Same-power divergence: organized traditionalist clergy networks and organized non-Catholic communions hold the identical power atom yet sit at opposite ends of the computation, differentiated entirely by role (payer versus beneficiary) and exit (identity_locked versus mobile). The bishops straddle: beneficiaries of collegial authority and payers of implementation, computing mid-range.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the Holy See (agenda-setter and collector), the episcopal conferences (collegial authority), the laity (vernacular worship, liberty protections), progressive theologians (a field reshaped around their project), and the external dialogue partners all sit toward the beneficiary end, the externals lowest because their exit is effectively unconstrained. Victim declarations map to high directionality: traditionalist clergy networks and traditionalist laity bear the enforcement's costs, and their identity-locked exit places them near the full-target end, since trapped or identity-fused targets amplify effective extraction relative to mobile ones. No directionality overrides are authored: the derivation chain (role declarations plus exit options) already separates every seat correctly, including the two organized actors whose identical power atoms resolve oppositely through role and exit rather than power. Spatial scope is global for most seats, which scales effective extraction modestly upward through verification difficulty; suppression is authored as a raw structural property and is deliberately left unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems (engagement with modernity, Christian division, liturgical participation, church-state obsolescence) remain live, so no resolved-mandatrophy condition is declared and the R5 mismatch flag (dead status paired with a world-rearranges verdict) is avoided honestly. The classification guards against mislabeling in both directions: a pure-coordination reading fails because the enforcement asymmetry is real and concentrated, meaning someone is coordinated and someone pays through the same structure and it takes active enforcement to hold; a pure-extraction reading fails because the coordination function is real and broadly received, delivering unity, accessibility, and ecumenical peace that the majority would not surrender. The arrangement is not a piton: maintenance is active and politically contested rather than inertial, and the theater ratio is modest. The oscillating enforcement trajectory is the main lifecycle risk: each reversal (Summorum Pontificum, then Traditionis Custodes) shows the settlement's terms are resettable by a single administration, which keeps extraction on the minority seat volatile rather than decaying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_or_development_hermeneutic,
    'Do the conciliar texts (Dignitatis Humanae, Nostra Aetate, Unitatis Redintegratio) contradict the prior magisterium on religious liberty, the interreligious stance, and ecumenism, or legitimately develop it?',
    'Comparative textual and drafting-history analysis: the multi-volume History of Vatican II reconstruction, side-by-side doctrinal comparison of Dignitatis Humanae with Quanta Cura, Mirari Vos, and the Syllabus-era framework, and assessment of the doctrinal weight (ordinary versus solemn) of the superseded formulations.',
    'Confirmed contradiction validates this reading''s foundational axiom and leaves the arrangement''s lineage-grounded authority carrying an acknowledged internal break; demonstrated continuity collapses this reading into the continuity sibling and rewrites epsilon downward over the same referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_or_development_hermeneutic, conceptual, 'Whether the acknowledged rupture is real contradiction or development in disguise.').

omega_variable(
    kernel_reading_position_delta,
    'This story instantiates the rupture_reading of kernel vatican_ii_magisterial_authority; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Corpus-layer cross-reading comparison: continuity_reading authors low epsilon over the same standing arrangement and a coordination-forward claim; composite_overdetermination_reading decomposes the text corpus into multiple sub-constraints with divergent epsilon values. The disagreement is located in the relation between the conciliar corpus and prior magisterial teaching (contradiction versus development versus managed ambiguity) and in whether the texts authorize or merely permit radical implementation.',
    'Merging the readings into one constraint would average epsilon across incommensurable assessments and violate epsilon-invariance; keeping them as linked separate stories preserves per-reading and per-seat classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_delta, conceptual, 'Committer-frame bookkeeping: one kernel, three readings, this file is the rupture reading.').

omega_variable(
    enforcement_oscillation_future_path,
    'Will the Traditionis Custodes permission regime persist, reverse (as Summorum Pontificum reversed the indult regime), or stabilize into a permanent dual-form settlement?',
    'Observe subsequent pontificates'' liturgical legislation and dicasterial permission practice; track diocesan-level implementation variance over the next decade.',
    'Reversal drops minority-seat extraction sharply (the 2007-2019 window shows the floor); persistence locks the elevated trajectory; stabilization splits the difference and flattens the cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_oscillation_future_path, empirical, 'Future path of the oscillating enforcement regime.').

omega_variable(
    minority_cost_concentration,
    'What share of the arrangement''s total imposed costs falls on the traditionalist minority relative to its share of the governed population?',
    'Demographic surveys of attachment to the older liturgical form, counts of canonical penalties and restricted-parish cases, and comparison against majority-seat cost incidence.',
    'High concentration means the aggregate epsilon understates the payer seats'' effective extraction; the engine''s per-seat computation already amplifies this through identity-locked directionality, but the empirical base rate calibrates the magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_cost_concentration, empirical, 'Cost-incidence concentration on the identity-locked minority.').

omega_variable(
    reception_basis_of_authority,
    'Does the settlement''s binding force rest on papal promulgation and conciliar authority alone, or on ecclesial reception by the whole body?',
    'Canonical-theological analysis of the texts'' promulgation mode combined with observed compliance patterns across decades and constituencies.',
    'If reception-based, the traditionalist minority''s durable non-reception erodes the arrangement''s legitimacy from within and predicts further enforcement cycles; if promulgation-based, enforcement alone sustains it and the oscillation is mere policy variance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reception_basis_of_authority, conceptual, 'Promulgation versus reception as the ground of binding force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.36).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (vatican_ii_magisterial_authority), three readings emitted as three linked stories. The continuity reading is the officially favored upstream frame, since its adoption by the authority structure shapes the enforcement conditions this reading operates under; the composite-overdetermination reading mediates between them by splitting the text corpus into ambiguous compromise formulations. Each story carries its own epsilon over the same standing arrangement (the post-conciliar magisterial regime); the values differ because epsilon is reading-indexed, not topic-indexed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
