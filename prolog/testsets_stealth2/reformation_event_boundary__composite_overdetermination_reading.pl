% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Composite-Overdetermination Regime of Reformation Narration
 *   domain: historical epistemology/religious history/commitment system analysis
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination_reading of the
 *   reformation_event_boundary kernel: the Reformation enters history as an
 *   irreducibly composite event — theological innovation, institutional
 *   collapse, political realignment, and denominational proliferation as
 *   parallel strands operating simultaneously rather than in sequence — and
 *   the constraint under story is the historiographical regime that enforces
 *   that narration. Consolidated after the social-historical turn of the
 *   1960s-70s, the regime requires multi-causal treatment, penalizes complete
 *   single-driver accounts as reductionist, and keeps periodization
 *   permanently contestable, because different readings track different
 *   completion points. The regime coordinates four strand-communities under
 *   one event label (the Reformation's multiple commitment patterns run in
 *   parallel inside it rather than in sequence), and it extracts
 *   asymmetrically: publication space, citation flows, and the absorptive
 *   'overdetermined' shield accrue to the gatekeeping apparatus, while
 *   complete-account rivals, doctoral entrants, and popular narrators pay.
 *   Beneficiary and victim sets vary with which sub-event is foregrounded —
 *   confessional historians hold the theology and institutional lanes, social
 *   historians hold the communal and print lane, and each lane's protection
 *   is the same structure that marginalizes the sibling readings. Per the
 *   epsilon-invariance principle, theological_climb_reading and
 *   political_swap_reading are separate constraints with their own epsilon,
 *   beneficiary/victim structures, and types, linked via
 *   network.affects_constraints; they are not folded into this story. Epsilon
 *   referent: the standing arrangement under contest is the discipline's
 *   composite-overdetermination regime itself, assessed by this reading's own
 *   lights — the reading endorses the frame as descriptively accurate and
 *   prices its costs as the honest price of adequacy, hence moderate
 *   extraction rather than none.
 *
 * KEY AGENTS:
 *   - reformation_studies_establishment: agenda-setter (institutional/arbitrage) — administers the norm via peer review and hiring; collects publication space and gatekeeping rents
 *   - confessional_historians: beneficiary (organized/constrained) — protected lane for each confessional sub-narrative
 *   - reformation_social_historians: beneficiary (organized/constrained) — the communal/print lane the frame consolidated as co-equal strand
 *   - revisionist_periodization_scholars: dual beneficiary/payer (moderate/constrained) — collects contestability rents, pays in permanent non-settlement
 *   - single_cause_theorists: primary target (moderate/constrained) — bears marginalization as reductionist
 *   - graduate_students_in_reformation_studies: target (powerless/trapped) — bears the four-literature entry tax; secondary beneficiary via topic and job space
 *   - popular_history_narrators: target (moderate/mobile) — bears the compression penalty from both reviewer and audience sides
 *   - confessional_apologists: excluded (organized/identity_locked) — would object that the frame dissolves confessional ownership; outside the peer-review circuit
 *   - historiography_of_history_analysts: analytical observer (analytical/analytical) — sees the full structure; collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.55).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Composite-Overdetermination Regime of Reformation Narration").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical epistemology/religious history/commitment system analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '8176090f-7921-4ceb-af87-500a0363fce7').
narrative_ontology:cs_kernel_codification('8176090f-7921-4ceb-af87-500a0363fce7', distributed).
narrative_ontology:cs_authority_grounding('8176090f-7921-4ceb-af87-500a0363fce7', expertise).
narrative_ontology:cs_interpretation_layer_present('8176090f-7921-4ceb-af87-500a0363fce7').
narrative_ontology:cs_reading_relation('8176090f-7921-4ceb-af87-500a0363fce7', reformation_event_boundary__theological_climb_reading, influences).
narrative_ontology:cs_reading_relation('8176090f-7921-4ceb-af87-500a0363fce7', reformation_event_boundary__political_swap_reading, influences).
narrative_ontology:cs_axiom('8176090f-7921-4ceb-af87-500a0363fce7', foundational, no_single_causal_driver_suffices).
narrative_ontology:cs_axiom_status(no_single_causal_driver_suffices, holdable).
narrative_ontology:cs_axiom_grounding('8176090f-7921-4ceb-af87-500a0363fce7', no_single_causal_driver_suffices, empirically_contingent).
narrative_ontology:cs_axiom('8176090f-7921-4ceb-af87-500a0363fce7', foundational, periodization_is_reading_relative).
narrative_ontology:cs_axiom_status(periodization_is_reading_relative, holdable).
narrative_ontology:cs_axiom_grounding('8176090f-7921-4ceb-af87-500a0363fce7', periodization_is_reading_relative, empirically_contingent).
narrative_ontology:cs_reference_frame('8176090f-7921-4ceb-af87-500a0363fce7', composite_event_parallel_irreducible_strands).
narrative_ontology:cs_drift_state('8176090f-7921-4ceb-af87-500a0363fce7', global_history_decentering_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8176090f-7921-4ceb-af87-500a0363fce7', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformation_studies_establishment).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, confessional_historians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformation_social_historians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, revisionist_periodization_scholars).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, single_cause_theorists).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, graduate_students_in_reformation_studies).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, popular_history_narrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, graduate_students_in_reformation_studies).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, revisionist_periodization_scholars).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historiographical_overdetermination_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, methodological_pluralism_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journal editors, series editors, chair holders, and the conference circuit that constitute Reformation studies as a field. They set what counts as an admissible causal claim about the Reformation through peer review, hiring, and curriculum gatekeeping, and they collect the field's publication space, citation flows, and gatekeeping authority. If the composite frame lost its grip they could rebrand as early modern religious history and keep their positions; their expertise transfers, their gatekeeping rents do not.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformation_studies_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Ecclesiastical historians working from Protestant and Catholic institutional bases — seminaries, church archives, confessional faculties. The composite frame gives each tradition a protected lane: Protestants foreground doctrinal development, Catholics foreground institutional reform and continuity, and no lane has to concede primacy or absorb the others. Their exit is constrained because their positions, archives, and audiences are anchored in confessional institutions that need the Reformation narrated at all.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_historians, beneficiary,
    organized, generational, constrained, continental).

% Urban, communal, and print-culture historians whose work from the 1960s onward demonstrated dynamics operating independently of, and before, Luther's theology. The composite frame consolidated their findings as a co-equal strand rather than a refutation of the theological account, giving their subfield permanent co-equal status, dedicated journals, and job lines. Their exit is constrained: their archival competence and career investment are specific to the period and to the subfield structure the frame organizes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformation_social_historians, beneficiary,
    organized, generational, constrained, continental).

% Scholars proposing competing periodization schemes — the long Reformation, confessionalization from 1555 to 1648, radical-Reformation framings, global-history decenterings. The permanently contestable boundary sustains their publication program; the same contestability means no scheme ever settles, and each new proposal is absorbed as one more strand rather than adopted. They collect contestability rents and pay in permanent non-settlement.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, revisionist_periodization_scholars, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, revisionist_periodization_scholars, payer).

% Scholars advancing parsimonious complete accounts — theological-primacy theses, political-capture theses, media-determinist or economic single-driver accounts. Reviewers label complete monocausal accounts reductionist, and the venues open to them shrink to confessional presses, popular history, and periodic revisionist manifestos. Exit means leaving the subfield's conversation or rebranding the account as a partial strand, which concedes the point at issue.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, single_cause_theorists, payer,
    moderate, biographical, constrained, national).

% Doctoral students must master four literatures — theological, political, social, denominational — before contributing, and dissertation committees enforce the multi-causal review format. The burden functions as an entry tax whose screening value accrues to the gatekeepers. They also benefit secondarily: the sprawling composite sustains more dissertation topics and job lines than a settled single-narrative field would. Sunk costs, advisor lock-in, and a thin academic market leave them little exit.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, graduate_students_in_reformation_studies, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, graduate_students_in_reformation_studies, beneficiary).

% Trade-press authors, documentary makers, and textbook writers who must compress the Reformation for audiences. The composite frame penalizes them from both directions: academic reviewers fault single-narrative compression as distortion, while publishers and audiences reject four-strand complexity as unsellable. Their exit is real — they can write on other periods — but the Reformation is a durable commercial subject, so the penalty recurs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, popular_history_narrators, payer,
    moderate, biographical, mobile, global).

% Denominational heritage writers, seminary polemicists, and confessional educators on both Protestant and Catholic sides who hold that the Reformation was about something — truth or error, liberation or schism — and that its narration should serve confessional identity. The composite frame's refusal of a single driver and a single verdict dissolves their ownership claim. They sit outside the peer-review circuit: their objections register in heritage media and seminary curricula, not in the journals that administer the frame. Their stake is confessional identity rather than career, so exit is unthinkable rather than merely costly.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_apologists, excluded,
    organized, generational, identity_locked, continental).

% Historians of historiography and philosophers of history who study how the discipline narrates the Reformation. They see the full structure — the frame's coordination function, its gatekeeping, its absorptive treatment of rivals — and collect nothing from its operation. Their publications influence the frame's self-understanding but not its enforcement.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiography_of_history_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, reformation_studies_establishment).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in a field divided by confession and discipline: the composite frame lets theological, political, social, and denominational historians work on the same event without any subfield conceding primacy, and lets confessional traditions coexist under one event label without adjudication. It also guards against premature closure — each generation's new single-driver proposal (media determinism, epidemiology, economics) can be tested against the other strands' findings instead of displacing them.
% TRANSFER_FUNCTION: Moves explanatory authority and publication space from complete single-driver accounts to multi-causal composite accounts; moves the cost of entry (four-literature mastery) from the establishment onto doctoral entrants; moves legitimacy from confessional ownership narratives to credentialed multi-causal narration. Net direction: from single_cause_theorists, confessional_apologists, and graduate_students_in_reformation_studies toward reformation_studies_establishment and the subfield apparatus.
% ABSENT_VOICES: Confessional apologists on both sides would object that the frame dissolves the Reformation's meaning into mechanism; they are outside the peer-review circuit, in seminaries and heritage media. Popular narrators are present only as penalized applicants, not as participants in setting the standard. The general reading public, for whom a usable causal story is the entire product, has no seat at all.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination norm vanished overnight, the subfields would decouple — theological, political, and social history would stop sharing an event label — complete single-driver accounts would resurface and compete openly, confessional narratives would reclaim public narration, and the four-literature entry structure would collapse. The field's journals, curricula, and conference circuit are organized around the composite frame and would reorganize around whichever reading won the reopened contest.
% FOUNDING_PROBLEM: The frame was built to solve two linked problems: the confessional historiography wars (Protestant grand narrative versus Catholic counter-narrative, each claiming the event's meaning) and the empirical failure of single-driver accounts once social historians showed urban, communal, and print dynamics operating independently of — and before — Luther's theology. The composite frame was the peace treaty: an event large enough to hold every strand without forcing a verdict among them.
% FOUNDING_PROBLEM_CORROBORATION: The causal-adequacy half of the founding problem is attested live from outside the beneficiary set: single_cause_theorists themselves, when their accounts collide with the other strands' findings, concede the residue — their recurring failure is the strongest corroborating evidence for the frame's founding problem. The confessional-peace half is attested dead: the secularization of the academy removed the adjudication the frame once brokered, and no confessional institution any longer contests the journals' territory. Revisionist periodization scholars attest from outside the beneficiary core that the contestability function persists. No single outside source attests both halves; the dispute over the founding problem's status is itself the contested finding.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Interval mapping: T0-T60 corresponds to approximately 1965-2025. Extraction 0.55: the regime's rents are real — publication space and citation flows accrue to the gatekeeping apparatus, the unfalsifiability shield absorbs rival accounts without adjudication, and the four-literature entry tax converts entrant labor into establishment screening — but they are bounded by the frame's genuine descriptive output. Suppression 0.58: enforcement is active (the reductionist label in review, shrinking venues for complete monocausal accounts, committee-enforced multi-causal formats) but partial — the sibling readings persist at confessional institutions, in popular media, and as periodic revisionist manifestos, so exits are narrowed, not closed. Theater 0.30: a minority of the frame's operation is performative — ritual complexity and overdetermination citations in literature reviews that do no analytic work — while the subfield research the frame organizes is real. Accessibility_collapse 0.40: alternatives do not collapse on contact with the frame; the sibling readings remain live and reachable, which is itself evidence against mountain-like status. Resistance 0.50: recurring revisionist manifestos, the religious turn's re-centering of doctrine, and global-history decentering sustain permanent low-grade pressure that never wins. Claim/metric independence: tangled_rope is claimed from structure — genuine coordination function, asymmetric extraction through the same structure, active enforcement — while the metrics are authored from descriptive operation; the engine computes per-seat types from the structural data, and any divergence is the measurement. Temporal note: all three tracked metrics share one grid (T0, 10, 20, 30, 40, 50, 60). suppression_requirement is tracked because enforcement capacity genuinely moved over the interval — rising as social history consolidated its gatekeeping (T0 to T30), then partially relaxing as the religious turn re-opened doctrinal venues within composite terms — not merely because extraction shifted. theater_ratio rises monotonically as ritual multi-causalism accumulates; extractiveness peaks near the confessionalization-thesis era and plateaus. Coordination-type genuineness check: the identity_coordination declaration is not a cover story — the frame genuinely maintains the boundary of who may narrate the Reformation and manages confessional coexistence, and the coexistence function would survive venue-opening for complete accounts, so the extraction is separable from the coordination. Committer-axis note: the authored drift against this reading's reference frame is practice_drift/minor/unacknowledged — global and connected-history practice increasingly narrates plural Reformations decentered from Europe, straining the single-composite-event unit the frame presumes, while the frame's core multi-strand premise is intact and the establishment treats the frame as settled method rather than registering the boundary erosion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From reformation_studies_establishment, the frame is the discipline's hard-won descriptive adequacy: coordination it built, maintains, and benefits from, with rivals fairly labeled. From single_cause_theorists, the same structure is gatekeeping that converts parsimony into professional risk and reserves the complete-account category for the frame itself — which is never asked to be complete. From graduate_students_in_reformation_studies, it is an entry tax collected in years of surplus literature mastery. From confessional_apologists — a seat outside the conversation entirely — the frame is a dissolvent of meaning. Same-level divergence: single_cause_theorists and revisionist_periodization_scholars hold the same nominal power (moderate) and similar constrained exits, yet sit on opposite sides of the frame's admissibility line — the differentiator is not power but whether the account claims completeness; complete accounts pay, contestable ones collect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the establishment collects publication space and gatekeeping rents (directionality near the beneficiary end); confessional_historians and reformation_social_historians collect protected lanes (low d); revisionist_periodization_scholars collect contestability rents but pay in permanent non-settlement — their dual position (beneficiary with secondary payer role) places them nearer symmetric than a derivation from their beneficiary listing alone would suggest. No directionality override is authored: the override axis is the power atom, and revisionist_periodization_scholars share the moderate atom with single_cause_theorists, who need high d; a single per-atom override cannot serve both, so the dual-role declaration carries the correction instead. Victim declarations: single_cause_theorists bear marginalization with constrained exit (high d); graduate_students bear the entry tax with trapped exit (highest effective d among payers — trapped targets sit nearer the full-target end); popular_history_narrators bear the compression penalty but with mobile exit, damping their effective extraction. Scope: the regime operates at global academic scope, which amplifies verification difficulty — the overdetermined shield is hardest to audit precisely where the frame is most authoritative. Suppression is authored as a raw structural property of the regime's enforcement machinery and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification guards both failure modes. Calling the frame a snare would erase its genuine coordination output — the confessional peace and the protection against premature causal closure are real achievements, not cover. Calling it a rope would erase the documented extraction: the entry tax, the absorptive unfalsifiability shield, and the venue narrowing that falls asymmetrically on complete-account rivals. Tangled rope holds both. Mandatrophy status: the frame's founding mandate is half-obsolete — the confessional-peace function is dead, since the secularized academy no longer needs the treaty, while the causal-adequacy function renews with each generation's new monocausal proposal. The frame is not yet a piton: enforcement is functional rather than theatrical (theater_ratio 0.30, below the 0.5 substitution threshold), its coordination output is real, and no seat could cheaply replace it — fixing_cost is prohibitive because any settlement would falsify one strand's findings, force re-adjudication of a century of scholarship, and collapse the confessional peace function with no referee available. Degradation watch: if overdetermination becomes pure ritual citation (theater_ratio crossing 0.5) while the subfields quietly decouple, the frame degrades toward inertial maintenance — administered by an establishment that could replace it but bears less of its cost than its entrants do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — composite_overdetermination_reading — of the reformation_event_boundary kernel; what would the sibling readings (theological_climb_reading, political_swap_reading) change structurally, and where exactly is the disagreement located?',
    'No dataset resolves a kernel-reading commitment; resolution would require the historiographical community to converge on one reading of the event boundary, which this reading itself predicts will not occur. The sibling readings are separate constraint stories carrying their own epsilon values, beneficiary/victim structures, and types.',
    'Under the theological_climb_reading, the beneficiary/victim structure collapses to a single driver (doctrine as cause, institutions as effect) and epsilon re-indexes to a confessional adjudication arrangement; under the political_swap_reading, secular rulers become agenda-setters and the church the victim set, with theology demoted to post-hoc rationalization. The disagreement is located in exactly two structural elements: causal primacy (whether any single driver suffices) and completion point (whether periodization has a determinate boundary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates one reading of the reformation_event_boundary kernel; the siblings are separate constraints, not parts of this one.').

omega_variable(
    periodization_completion_point,
    'Do the composite event''s strands genuinely complete at different dates (theological settlement by the Formula of Concord 1577, political settlement by Augsburg 1555, denominational systems by Westphalia 1648, proliferation continuing into the seventeenth century), or is periodization contestation a symptom of the frame''s unfalsifiability?',
    'Strand-by-strand completion dating against archival consensus: if each strand''s institutional record shows a distinct stabilization point, reading-relative periodization is vindicated; if one date fits all strands, a unified periodization exists and the frame overclaims irreducibility.',
    'Genuinely divergent completion points strengthen this reading''s foundational periodization axiom; a single fitting date would dissolve the composite reading toward whichever driver''s completion point it was, and the frame''s periodization function would recompute as extraction riding on coordination rather than coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_completion_point, empirical, 'Whether the strands'' completion points genuinely diverge or periodization contestation masks unfalsifiability.').

omega_variable(
    overdetermination_falsifiability,
    'Is ''overdetermination'' a substantive, falsifiable causal claim about the Reformation, or an absorptive shield that no observation could defeat?',
    'Adversarial test: a sustained research program attempting total absorption of the four strands'' findings under one driver. The composite reading survives only if every such program terminates in irreducible residue; the frame is a shield if its holders treat each failed absorption as confirmation rather than as a live threat to the irreducibility claim.',
    'If falsifiable and surviving, the frame''s coordination claim strengthens and extraction estimates fall; if unfalsifiable, theater_ratio rises toward the substitution threshold and the regime drifts toward inertial, performative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_falsifiability, empirical, 'Falsifiability status of the overdetermination claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of single-driver accounts structural (venue access, review gatekeeping, committee formats) or internalized (junior scholars self-censor toward multi-causal formats as professional virtue)?',
    'Post-exit suppression trajectory: track scholars who leave the subfield for general-audience or confessional venues and write complete single-driver accounts there. If self-restriction toward composite formats persists after venue constraints are removed, the suppression is partly internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure of 0.58 because targets carry the gatekeeping with them after exit; if purely structural, venue reform would release suppressed complete accounts quickly and the regime''s coercive overhead is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of monocausal accounts.').

omega_variable(
    cs_framing_underdetermination,
    'Is the declared commitment-system framing — a distributed kernel adjudicated by credentialed expertise — the only defensible one, or does the alternative framing (implicit kernel: the regime''s standards are whatever peer review in fact does; authority grounded in practice) yield a different commitment-system classification?',
    'Compare the two framings against enforcement practice: if gatekeeping decisions cite articulable standards for admissible causal claims, the distributed-kernel/expertise framing holds; if decisions track precedent and reviewer habit with no articulable standard, the implicit-kernel/practice framing holds.',
    'Under the implicit-kernel/practice framing the constraint reads as a pure enforcement mechanism with no interpretive buffer, its coordination claim weakens, and per-seat classifications shift toward the payer seats; the tangled-rope classification is stable under both framings, but the framing choice changes the interpretive-layer verdict and the extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Framing under-determination: distributed-kernel/expertise versus implicit-kernel/practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t10, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(refo_tr_t10, observed).
narrative_ontology:measurement(refo_tr_t20, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(refo_tr_t20, observed).
narrative_ontology:measurement(refo_tr_t30, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(refo_tr_t30, observed).
narrative_ontology:measurement(refo_tr_t40, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(refo_tr_t40, observed).
narrative_ontology:measurement(refo_tr_t50, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(refo_tr_t50, observed).
narrative_ontology:measurement(refo_tr_t60, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(refo_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t10, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(refo_be_t10, observed).
narrative_ontology:measurement(refo_be_t20, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(refo_be_t20, observed).
narrative_ontology:measurement(refo_be_t30, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(refo_be_t30, observed).
narrative_ontology:measurement(refo_be_t40, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(refo_be_t40, observed).
narrative_ontology:measurement(refo_be_t50, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(refo_be_t50, observed).
narrative_ontology:measurement(refo_be_t60, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(refo_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(refo_su_t0, observed).
narrative_ontology:measurement(refo_su_t10, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(refo_su_t10, observed).
narrative_ontology:measurement(refo_su_t20, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(refo_su_t20, observed).
narrative_ontology:measurement(refo_su_t30, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(refo_su_t30, observed).
narrative_ontology:measurement(refo_su_t40, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(refo_su_t40, observed).
narrative_ontology:measurement(refo_su_t50, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(refo_su_t50, observed).
narrative_ontology:measurement(refo_su_t60, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(refo_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into three reading-constraints of the reformation_event_boundary kernel. Their epsilon values differ because their referents differ: this story's epsilon indexes the composite-overdetermination historiographical regime itself; theological_climb_reading's epsilon would index a confessional-adjudication arrangement; political_swap_reading's epsilon would index a political-capture arrangement. This story is the upstream member (highest current empirical confidence; the composite frame is the disciplinary default in the secular academy) and it influences the downstream siblings: each monocausal account must now position itself against the composite default and is absorbed as 'one strand' rather than refuted — a legitimacy-condition change that stops short of foreclosure, since the siblings remain live at confessional institutions and in popular narration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
