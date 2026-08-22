% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Post-Conciliar Doctrinal Authority Settlement (Traditionalist Reading: Rupture with Enabling Ambiguities)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   vatican_ii_doctrinal_authority: the rupture_traditionalist_reading, which
 *   holds that the Second Vatican Council broke with the received magisterium
 *   and that its documents contain ambiguities functioning as defects that
 *   enabled heterodox implementation. Per the epsilon-referent ruling, the
 *   authored epsilon describes the STANDING ARRANGEMENT UNDER CONTEST - the
 *   operative post-conciliar settlement by which conciliar texts, interpreted
 *   by the Roman apparatus, govern doctrine, liturgy, and discipline -
 *   assessed by THIS reading's own lights. It is not the
 *   traditionalist-preferred restoration that is measured; that alternative
 *   is not this constraint. On the reading's lights the settlement carries
 *   heavy burden: traditional liturgical forms were suppressed then rationed
 *   by indult and decree, clergy formed under the previous regime faced
 *   obedience-or-marginalization choices, missionary momentum declined, and
 *   disciplinary machinery repeatedly targeted those who kept the older
 *   forms. The claim/metric independence rule applies deliberately: the
 *   claimed type is tangled_rope because a genuine coordination layer exists
 *   even under this hostile reading (shared vernacular rites, collegial
 *   consultation, ecumenical protocols coordinating a global communion),
 *   while the metrics describe heavily burdened, actively enforced operation.
 *   Where the computed per-seat types diverge from the claim, that divergence
 *   is the corpus's measurement. Sibling readings are separate files linked
 *   through network.affects_constraints; their epsilon values differ
 *   (near-zero on doctrinal change for continuity; high but positively framed
 *   for rupture-progressive; partitioned across sub-shifts for the composite
 *   reading).
 *
 * KEY AGENTS:
 *   - - traditional_latin_mass_adherents: primary target (moderate/identity_locked) — bears the settlement's liturgical and disciplinary costs; identity fused to the inherited rite
 *   - - pre_conciliar_formed_clergy: primary target (moderate/trapped) — ordained into vows that bind them to jurisdictions administering the new books
 *   - - irregular_status_traditionalist_fraternities: organized target (organized/trapped) — keep the older forms without regular canonical standing, cycling between concession and censure
 *   - - mission_territory_churches: secondary target (moderate/constrained) — absorbed the decline in missionary momentum after priorities shifted
 *   - - curial_reform_administration: primary beneficiary/agenda_setter (institutional/constrained) — administers the settlement, appoints, disciplines, and receives the deference and discretion it generates
 *   - - national_bishops_conferences: beneficiary (powerful/constrained) — collected governance weight against Rome under collegial structures
 *   - - conciliar_theology_profession: beneficiary (organized/constrained) — careers, journals, and advisory roles built on interpreting the texts
 *   - - liturgical_reform_industry: beneficiary (organized/mobile) — consultants, publishers, and commissions whose work exists because implementation continues
 *   - - ordinary_parish_laity: dual-positioned beneficiary/payer (moderate/mobile) — gained vernacular participation, absorbed liturgical churn and retention losses
 *   - - sedevacantist_networks: excluded voice (organized/trapped) — rejected the settlement entirely and sit outside every conversation about it
 *   - - council_history_scholars: analytical observer (analytical/analytical) — documentary access to the textual genesis and implementation record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.75).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Post-Conciliar Doctrinal Authority Settlement (Traditionalist Reading: Rupture with Enabling Ambiguities)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '8fcc94d6-f821-4a8d-ada7-2aeba0055f00').
narrative_ontology:cs_kernel_codification('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', fixed_text).
narrative_ontology:cs_authority_grounding('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', lineage).
narrative_ontology:cs_interpretation_layer_present('8fcc94d6-f821-4a8d-ada7-2aeba0055f00').
narrative_ontology:cs_reading_relation('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_axiom('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', foundational, textual_ambiguity_is_constitutive_error).
narrative_ontology:cs_axiom_status(textual_ambiguity_is_constitutive_error, holdable).
narrative_ontology:cs_axiom_grounding('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', textual_ambiguity_is_constitutive_error, empirically_contingent).
narrative_ontology:cs_axiom('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', foundational, preconciliar_magisterium_is_judging_standard).
narrative_ontology:cs_axiom_status(preconciliar_magisterium_is_judging_standard, holdable).
narrative_ontology:cs_axiom_grounding('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', preconciliar_magisterium_is_judging_standard, deontological).
narrative_ontology:cs_axiom('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', secondary, implementation_follows_from_textual_defect).
narrative_ontology:cs_axiom_status(implementation_follows_from_textual_defect, holdable).
narrative_ontology:cs_axiom_grounding('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', implementation_follows_from_textual_defect, empirically_contingent).
narrative_ontology:cs_reference_frame('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', preconciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', contemporary_postconciliar_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fcc94d6-f821-4a8d-ada7-2aeba0055f00', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, curial_reform_administration).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, national_bishops_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_theology_profession).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reform_industry).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_formed_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, irregular_status_traditionalist_fraternities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, mission_territory_churches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the interpretive and disciplinary documents through which the conciliar corpus governs, appoints bishops aligned with the settlement, reviews and restricts requests to use the older liturgical books, and issues censures against groups operating outside regular canonical channels. Deference, appointment leverage, and administrative discretion flow toward this seat from the settlement's day-to-day operation. Leaving would mean resigning the offices through which the seat acts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, curial_reform_administration, agenda_setter,
    institutional, generational, constrained, global).

% Gained standing governance weight relative to Rome under the collegial structures the settlement established. They translate universal norms into regional policy, control local liturgical permissions, and mediate between the center and their clergy. Their autonomy is real but bounded by the same apparatus that granted it; exit would mean schism-level rupture with the center.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, national_bishops_conferences, beneficiary,
    powerful, generational, constrained, global).

% University chairs, journals, editorial boards, and advisory consultancies are built on interpreting the conciliar texts and their aftermath. Publication, promotion, and invitation flow follow fluency in the post-conciliar framework. Abandoning that framework mid-career would strand accumulated expertise and sever professional networks.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_theology_profession, beneficiary,
    organized, biographical, constrained, continental).

% Liturgical consultants, translation committees, music publishers, and commission staff whose ongoing employment exists because implementation and re-translation continue. Demand for their services recurs with each revised edition and each jurisdictional adoption. Individual members can move between dioceses and projects, though the customer base itself is the settlement.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reform_industry, beneficiary,
    organized, biographical, mobile, global).

% Attend vernacular liturgies, donate, volunteer, and staff parish programs. Many welcomed the vernacular and expanded participation; many others experienced decades of liturgical revision, catechetical inconsistency, and declining institutional belonging, and their children's retention rates shifted measurably after the transition. Their practical exit is quiet drift to other denominations or to none, which large numbers exercised.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity, payer).

% Attached to the liturgical forms in force before 1970. After the wholesale replacement of those forms, access was rationed by indult, then widened by permission in 2007, then narrowed again by decree in 2021. Adherents fund their own chapels, travel past nearer parishes, and organize across dioceses to preserve access. Their attachment is constitutive of family and religious identity across generations; giving up the inherited forms means assimilating to practices they read as rupture, or leaving the communion entirely, both of which they experience as self-loss.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_adherents, payer,
    moderate, generational, identity_locked, global).

% Ordained before or during the transition under formation shaped by the previous liturgical and theological regime. Their ministerial lives were rerouted by the new books; celebrating the older forms required permission that varied by bishop and decade. Vowed obedience binds them to jurisdictions administering the new arrangements, so refusal carried suspension and conformity carried conscience-cost, with no third door short of laicization.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_formed_clergy, payer,
    moderate, biographical, trapped, continental).

% Priestly fraternities that maintained the older liturgical forms without regular canonical standing after disputes with Rome escalated in the 1970s-80s. They operate seminaries, chapels, and schools under suspended or irregular faculties, cycling between negotiated concessions and formal censure. Their sacramental validity is disputed by adversaries and defended by allies; normalization offers exist periodically but require accepting the settlement they exist to survive.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, irregular_status_traditionalist_fraternities, payer,
    organized, biographical, trapped, global).

% Missionary orders and young churches in conversion territories whose recruitment, vocations, and expansion slowed as institutional priorities shifted toward internal renewal, dialogue, and administrative consolidation after the council. They adapted to vernacular inculturation directives with uneven success and had limited voice in the liturgical decisions that reshaped their evangelistic toolkit.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, mission_territory_churches, payer,
    moderate, generational, constrained, continental).

% Communities that concluded the post-conciliar papacy itself is invalid and severed communion entirely. They maintain parallel sacramental life through their own clergy and broadcast extensive criticism of every faction inside the conversation. No organ of the settlement consults them; they are the loudest objection to it and are structurally outside the room.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sedevacantist_networks, excluded,
    organized, generational, trapped, global).

% Historians and archivists publishing documentary accounts of the council's drafting, the minority reports, the vote margins, and the implementation correspondence. Their access to diaries, Acta, and correspondence lets them trace which ambiguities were deliberate compromises and which were oversights. They take materials from every faction and owe allegiance to none.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, council_history_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, curial_reform_administration).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global communion on shared vernacular rites, collegial consultation structures linking episcopal conferences to Rome, and common protocols for ecumenical and interreligious engagement - solved once, centrally, for roughly 1.4 billion members rather than per-diocese.
% TRANSFER_FUNCTION: Moves liturgical patrimony, doctrinal terminological certainty, and missionary momentum away from traditionalist clergy, laity, and mission territories; moves interpretive authority, appointment leverage, publishing demand, and administrative discretion toward the curial apparatus, bishops' conferences, the theology profession, and the liturgical services sector.
% ABSENT_VOICES: Sedevacantist networks and the large population of quietly departed laity would object most forcefully and are wholly outside the conversation. The pre-conciliar magisterial corpus 'speaks' but is quoted selectively by every faction rather than seated as a party. Eastern Catholic liturgists were minimally consulted during the Latin liturgical reform and would contest assumptions embedded in the new books.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the liturgical calendar would revert or fork regionally, the theology profession's interpretive infrastructure would lose its warrant, bishops' conferences would renegotiate their relation to Rome from scratch, ecumenical accords built on the settlement's language would need renegotiation, and the traditionalist constituencies' grievance structure - the organizing principle of their communal life for two generations - would dissolve into a different argument. Nearly every named seat's arrangements depend on the settlement existing.
% FOUNDING_PROBLEM: The council was convened to solve a cluster of pastoral problems: how a global church engages modern plural societies, how lay participation in worship could deepen beyond silent assistance, how governance could adapt to a vastly larger and more geographically distributed membership, and how to relate to other Christian bodies and religions after centuries of polemic.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the documentary council-scholarship community attests the founding problems were sincerely framed and remain unresolved, citing the Acta and participant diaries; Protestant and Orthodox observer delegates' published assessments from the council floor corroborate the engagement-with-modernity framing independently of Catholic administrators; sociologists of religion outside the church document that the participation and retention outcomes the renewal sought moved adversely, supporting the 'problem persists' side of the contest. No source outside the beneficiary set attests that the problems are settled.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.75 at interval end) because, on this reading, the burden falls on parties with the least procedural recourse: liturgical inheritance was withdrawn wholesale in 1969-70, later rationed rather than returned, and disciplinary decrees track the visibility of traditionalist practice rather than any doctrinal offense the texts sanction. Suppression (0.68) is a raw unscaled structural property: the settlement's persistence against this constituency has depended on repeated acts of enforcement (censures in 1974-1988, restriction decrees in 2021 onward), not on voluntary convergence. Theater_ratio (0.46) is moderate-high: consultation, dialogue, and 'spirit of the Council' invocations increasingly function as performance overlying decisions made administratively, though the underlying governance and liturgical production remain real. Accessibility_collapse (0.55) is mid-range because alternatives never fully collapsed: the older forms survived in institutes of pontifical right, in irregular fraternities, and in the Eastern churches, so exit exists but at identity price. Resistance (0.70) is high and organized: pilgrimages, scholarly counter-literature, canonical-status negotiations, and coalition-building among dispersed traditionalist constituencies. The temporal series run on one shared nine-point grid (every tracked metric authored at every examined year) spanning the council through 2025. The suppression_requirement series is deliberately non-monotonic because enforcement history is the story's dynamic: escalation to the 1988 censures, relaxation through the 2007 liberalization window, renewed tightening from 2021. The oscillation tracks successive papal administrations' policies rather than intermittent reinforcement as a deliberate mechanism, though each liberalization raised traditionalist expectations that subsequent re-tightening then punished - a partial-reinforcement effect worth watching. Base scalars sample the 2025 endpoint, the re-tightened phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that arithmetic. From the agenda_setter seat, the settlement is a governance achievement it built, staffed, and defends: coordination it performs daily, burden experienced as the ordinary cost of unity. From the identity_locked payer seat (laity attached to the older rite), the same structure operates as dispossession administered by decree. From the trapped clergy seat it is obedience under duress. From the beneficiary professions it is a funding and authority environment. The analytical seat sees a contested hermeneutic fight in which every faction quotes the same corpus selectively. Nothing in the authored claim adjudicates among these; the structural data - roles, exits, directionalities - generate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the curial administration receives discretion, appointment leverage, and deference; bishops' conferences received governance weight; the theology profession receives careers; the liturgical industry receives continuing demand; parish laity receive vernacular accessibility (their indirect payment - churn, weaker catechesis - justifies their dual positioning). Victim declarations map to the burden-bearers: adherents of the older forms paid in liturgical inheritance and travel; pre-conciliar clergy paid in constrained ministry; irregular fraternities paid in canonical standing; mission territories paid in momentum. Trapped and identity_locked exits push the payer seats toward the full-target end of directionality; the curial seat sits nearest the beneficiary end despite administering rather than passively collecting; the laity's mobility damps their derived burden below their nominal dual-role exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pastoral renewal, liturgical participation, governance adaptation, engagement with modernity) is authored as CONTESTED, not dead: traditionalists hold it was misconceived or mishandled, progressives hold it unfinished, administrators hold it ongoing. Because status is contested rather than dead alongside a world_rearranges disappearance verdict, the mismatch consumer should not fire the zombie flag here - the settlement's persistence is not obviously theatrical maintenance of an obsolete mandate. The classification guards against two symmetrical errors: reading the settlement as pure coordination (which would erase the documented disciplinary record and the displaced constituencies) and reading it as pure extraction (which would erase the real vernacular, collegial, and ecumenical functions that coordinate a global communion daily). The tangled_rope claim holds both truths apart; if enforcement decayed entirely while extraction persisted, the computed type should migrate toward pure extraction, and if the burden equalized across seats it should migrate back toward coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the rupture_traditionalist_reading of kernel vatican_ii_doctrinal_authority; how do the sibling readings alter the structural classification?',
    'Compile the sibling stories (continuity_reading, rupture_progressive_reading, composite_overdetermination_reading) and compare per-seat classifications across the family.',
    'The continuity_reading drives epsilon toward negligible on doctrinal change and dissolves most of this victim set; rupture_progressive_reading shares the high epsilon but inverts its valuation; composite_overdetermination_reading splits the arrangement into several sub-constraints with distinct epsilon values, redistributing extraction across liturgical, ecumenical, ecclesiological, and political axes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: this classification holds for one reading of a contested kernel.').

omega_variable(
    rupture_vs_organic_development,
    'Is the documented post-conciliar change best described as rupture with the prior magisterium or as organic development of implicitly held teaching?',
    'Side-by-side doctrinal adjudication by disinterested historians of doctrine: Dignitatis Humanae against nineteenth-century religious-liberty condemnations, the reformed liturgical books against their pre-conciliar predecessors, collegiality provisions against prior primacy formulations.',
    'An organic-development verdict collapses this reading''s epsilon toward the continuity reading''s value and empties the victim set; a rupture verdict sustains the high authored epsilon and the declared victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_organic_development, conceptual, 'The factual hinge on which this entire reading stands or falls.').

omega_variable(
    textual_defect_vs_reception_failure,
    'Are heterodox implementations caused by defects residing in the conciliar texts themselves, or by failures in subsequent interpretation and reception?',
    'Philological tracing from draft schemas through final texts into implementation instruments (liturgical editio typica revisions, catechetical norms, seminary curricula), comparing implementations across jurisdictions that received identical texts.',
    'If reception-side, the constraint''s locus shifts from the texts to the interpretive apparatus: the texts'' own burden falls and the apparatus''s rises, changing which seat captures the measured burden. If authorial-defect, the burden stays concentrated on the texts and this reading''s account is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_defect_vs_reception_failure, empirical, 'Whether ''errors enabling heterodox implementation'' is a property of the texts or of their readers.').

omega_variable(
    enforcement_vs_assent_persistence,
    'Does the post-conciliar settlement persist by genuine reception among the faithful or by disciplinary enforcement against dissenters?',
    'Attendance and vocation statistics cross-referenced against the enforcement-event timeline; the 2007-2021 liberalization window serves as a natural experiment: did traditional practice flourish when coercion was relaxed?',
    'Enforcement-dependent persistence raises effective suppression for targeted seats and pushes per-seat computations toward pure extraction; demonstrated voluntary assent supports the coordination component and holds the classification at hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_assent_persistence, empirical, 'Persistence basis of the settlement: assent or enforcement.').

omega_variable(
    founding_problem_valence,
    'Was the council''s pastoral-renewal mandate a genuine problem that required the settlement, or a misdiagnosis that manufactured the need it then answered?',
    'Historiographic comparison of pre-conciliar growth, ordination, and practice indicators against post-conciliar trajectories, controlling for secularization trends affecting all Western denominations.',
    'A misdiagnosis verdict removes coordination credit and pushes the computed type toward pure extraction; a genuine-problem verdict preserves the hybrid classification even under high measured burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_valence, preference, 'Whether renewal itself was needed, or was the cover story for the settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2_rupture_trad_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(v2_rupture_trad_tr_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(v2_rupture_trad_tr_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(v2_rupture_trad_tr_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1988, 0.38).
narrative_ontology:measurement(v2_rupture_trad_tr_t1998, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(v2_rupture_trad_tr_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2007, 0.36).
narrative_ontology:measurement(v2_rupture_trad_tr_t2019, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2019, 0.42).
narrative_ontology:measurement(v2_rupture_trad_tr_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2021, 0.45).
narrative_ontology:measurement(v2_rupture_trad_tr_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2025, 0.46).

% Extraction over time
narrative_ontology:measurement(v2_rupture_trad_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(v2_rupture_trad_be_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(v2_rupture_trad_be_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(v2_rupture_trad_be_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1988, 0.68).
narrative_ontology:measurement(v2_rupture_trad_be_t1998, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1998, 0.66).
narrative_ontology:measurement(v2_rupture_trad_be_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(v2_rupture_trad_be_t2019, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(v2_rupture_trad_be_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement(v2_rupture_trad_be_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(v2_rupture_trad_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement(v2_rupture_trad_su_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(v2_rupture_trad_su_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(v2_rupture_trad_su_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1988, 0.7).
narrative_ontology:measurement(v2_rupture_trad_su_t1998, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(v2_rupture_trad_su_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2007, 0.4).
narrative_ontology:measurement(v2_rupture_trad_su_t2019, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(v2_rupture_trad_su_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(v2_rupture_trad_su_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what did Vatican II mean' decomposes, per the epsilon-invariance principle, into four readings each instantiating a distinct constraint with its own epsilon, victim set, and classification. This file authors the rupture_traditionalist_reading. The upstream members (continuity, composite) supply the textual and historical baseline that this reading cites as evidence of rupture; the progressive sibling shares the factual premise of change but inverts its valuation, so the pair's epsilon magnitudes may converge while their signified harm runs in opposite directions. All four stories link mutually through affects_constraints; contamination analysis should treat a textual-discovery event (e.g., draft-schema revelations) as propagating to every member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
