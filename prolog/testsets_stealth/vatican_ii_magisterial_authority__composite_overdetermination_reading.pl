% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority as Overdetermined Composite Settlement
 *   domain: religious/institutional/hermeneutical
 *
 * SUMMARY:
 *   Under the composite_overdetermination_reading, the standing arrangement
 *   under assessment is the post-conciliar magisterial settlement: a fixed
 *   corpus of conciliar texts whose operative force runs through an
 *   interpretive office. On this reading the texts were assembled to secure
 *   supermajority votes by encoding incompatible ecclesiological visions in
 *   deliberately balanced formulas — hence the recurring 10-12% negative and
 *   abstention votes on the contested schemas, and hence the durable
 *   divergence of implementation across regions. The arrangement delivers
 *   real coordination goods (visible unity, a common liturgy, an ecumenical
 *   opening) while transferring the cost of unresolved disagreement onto
 *   those bound by whichever interpretation currently prevails. Claim and
 *   metrics are independent: the claimed type is authored from the structural
 *   analysis below; the metric values describe the arrangement's observed
 *   operation and are not tuned to the claim.
 *
 * KEY AGENTS:
 *   - conciliar_majority_drafters: historical authors of the settlement (powerful/arbitrage) — traded precision for vote margins; primary beneficiaries of the supermajority record
 *   - papal_magisterial_interpreter: agenda-setter (institutional/constrained) — rules on the texts' meaning; cannot repudiate the council without harming its own office
 *   - dissenting_minority_fathers: absorbed minority (moderate/trapped) — objections archived, not answered; faced submission or schism
 *   - post_conciliar_theologians: mixed seat (organized/constrained) — funded by textual openness, exposed by interpretive discipline
 *   - traditionalist_communities: resisting payers (moderate/identity_locked) — fidelity constituted by resistance to the prevailing reading
 *   - progressive_reform_factions: disappointed payers (organized/identity_locked) — hold uncashed conciliar promises
 *   - diocesan_implementers: delegated beneficiaries (institutional/mobile) — collect textual latitude as local discretion
 *   - ordinary_faithful: diffuse payers (powerless/trapped) — receive shifting doctrine with no channel into the contest
 *   - ecumenical_dialogue_partners: excluded (organized/constrained) — staked bilateral work on readings they cannot defend
 *   - ecclesiological_historians: analytical observer (analytical/analytical) — sees the drafting trades, the votes, and the divergence whole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.6).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority as Overdetermined Composite Settlement").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "religious/institutional/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'ca441318-783e-4740-98d0-ed4b954368d4').
narrative_ontology:cs_kernel_codification('ca441318-783e-4740-98d0-ed4b954368d4', fixed_text).
narrative_ontology:cs_authority_grounding('ca441318-783e-4740-98d0-ed4b954368d4', lineage).
narrative_ontology:cs_interpretation_layer_present('ca441318-783e-4740-98d0-ed4b954368d4').
narrative_ontology:cs_reading_relation('ca441318-783e-4740-98d0-ed4b954368d4', vatican_ii_magisterial_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('ca441318-783e-4740-98d0-ed4b954368d4', vatican_ii_magisterial_authority__rupture_reading, influences).
narrative_ontology:cs_axiom('ca441318-783e-4740-98d0-ed4b954368d4', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('ca441318-783e-4740-98d0-ed4b954368d4', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('ca441318-783e-4740-98d0-ed4b954368d4', foundational, interpretive_office_is_operative_authority).
narrative_ontology:cs_axiom_status(interpretive_office_is_operative_authority, holdable).
narrative_ontology:cs_axiom_grounding('ca441318-783e-4740-98d0-ed4b954368d4', interpretive_office_is_operative_authority, conventional).
narrative_ontology:cs_reference_frame('ca441318-783e-4740-98d0-ed4b954368d4', negotiated_compromise_settlement).
narrative_ontology:cs_drift_state('ca441318-783e-4740-98d0-ed4b954368d4', contemporary_hermeneutical_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca441318-783e-4740-98d0-ed4b954368d4', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_majority_drafters).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterial_interpreter).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, diocesan_implementers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, dissenting_minority_fathers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theologians).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, supermajority_vote_legitimacy).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, managed_doctrinal_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Council fathers and expert advisers who assembled the final schemas. They traded precise formulations for vote margins, accepting language each major faction could sign without agreeing on what it meant. Their reward was a near-unanimous record and a body of texts bearing their names as the council's settled teaching. As historical actors their remaining stake is reputational: how posterity reads what they wrote.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_majority_drafters, beneficiary,
    powerful, biographical, arbitrage, global).

% Successive popes and curial bodies decide which reading of the conciliar texts governs: issuing interpretive documents, appointing interpreters, disciplining deviant readings, and answering clarification requests from around the world. They cannot repudiate the council without dissolving their own office's recent authority, so they manage the texts rather than resolve them.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterial_interpreter, agenda_setter,
    institutional, generational, constrained, global).

% The roughly tenth of the council who voted against or withheld consent from key schemas. Their written objections entered the archives but not the operative canon. When the council closed they faced a choice between public submission to texts they had judged defective and schism; nearly all submitted, and their reservations survive mainly in private diaries and unpublished commentaries.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, dissenting_minority_fathers, payer,
    moderate, biographical, trapped, global).

% Academic theologians working the conciliar corpus. The open texture of the texts funds a generation of creative scholarship — careers built on exploring what the formulas might mean. The same openness exposes them: when Rome rules an interpretation out of bounds, books are withdrawn, mandates lost, licenses to teach revoked. Their professional fate tracks whichever reading currently holds.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theologians, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theologians, beneficiary).

% Clergy and laity attached to pre-conciliar liturgy and doctrine. They read the reforms as loss and the ambiguous texts as camouflage for that loss. Full acceptance of the prevailing interpretation would dissolve the identity that binds them; open rejection costs them canonical standing. They persist in communities that measure fidelity by resistance to the prevailing reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, global).

% Reform-minded clergy, religious, and laity who read the texts as promissory notes for structural change — collegiality realized, laity empowered, pastoral practice liberalized. Decades of incremental implementation have left most of the promised architecture unbuilt, but abandoning the conciliar promise would abandon their life's work. They stay, pressing each pontificate to cash the notes.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_factions, payer,
    organized, biographical, identity_locked, global).

% Bishops and diocesan offices translating the conciliar decrees into local practice. The texts' latitude delegates real discretion: what liturgy looks like, how councils of priests operate, how ecumenism proceeds varies widely by region. They collect that discretion and the flexibility it buys, while absorbing the whiplash when Rome shifts the prevailing reading and local arrangements must be redone.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, diocesan_implementers, beneficiary,
    institutional, generational, mobile, regional).

% Parishioners who received a changed liturgy, changed catechesis, and successive official accounts of what the council 'really' taught. They chose none of the interpretive contests that produced these shifts and have no formal channel into them. Their stake is continuity of worship and doctrine; their option is attendance elsewhere at the price of leaving communion.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_faithful, payer,
    powerless, generational, trapped, global).

% Other churches and ecclesial communities that built dialogue on particular readings of the conciliar decrees on unity, religious liberty, and the non-Christian religions. They have no seat in the intra-Catholic contest over what those decrees mean; a shift in the prevailing reading can devalue decades of bilateral work without their consent or notice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecumenical_dialogue_partners, excluded,
    organized, generational, constrained, global).

% Scholars of the council working from the published acta, drafting histories, voting records, and participant diaries. They can reconstruct which formulations were contested, what was traded for which vote, and how implementation diverged across regions — the full arc that participants each saw only from inside their own faction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecclesiological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterial_interpreter).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a globally dispersed episcopal college inside one visible communion while updating its liturgy, its posture toward other religions and states, and its internal governance — by formulas each major faction could sign without agreeing on what they meant.
% TRANSFER_FUNCTION: Moves doctrinal assent and interpretive deference upward — from bishops, theologians, and faithful to whichever seat currently rules on the texts' meaning — and moves legitimacy downward from that seat's rulings across the whole church; historically it moved supermajority prestige to the conciliar project itself.
% ABSENT_VOICES: The minority fathers' written objections sit in archives outside the operative canon; ecumenical partners who staked bilateral work on particular readings have no seat in intra-Catholic interpretation; the lay faithful have no formal channel into hermeneutical contests that reshape their worship and catechesis.
% DISAPPEARANCE_RATIONALE: If the conciliar settlement and its interpretive machinery vanished overnight, the factions currently held in suspension by the ambiguous formulas would separate into their native positions — a continuity church and a rupture church, or a negotiated split — because nothing else currently holds them in one communion. Liturgical practice, seminary curricula, ecumenical agreements, and curial authority would all reorganize around whichever settlement replaced it.
% FOUNDING_PROBLEM: How a global church could engage the modern world — liturgical participation, religious liberty, ecumenism, collegial governance — without shattering visible unity between theological schools that disagreed about whether that engagement betrayed or fulfilled the tradition.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the published voting records and the minority fathers' preserved diaries and commentaries attest that the founding disagreement was never resolved, only voted past; council historians working from the acta document the deliberate trading of precision for margins; and the sequence of mutually corrective official interpretations across successive pontificates — each adjusting the last — attests from the institutional record itself that no reading has stabilized. No beneficiary-party attestation is relied on.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.65: the settlement delivers genuine goods — a single communion, a workable liturgy, an ecumenical door — but the cost of its constitutive device (signable ambiguity) lands on seats that never agreed to it: minorities absorbed rather than answered, theologians disciplined by whichever reading holds, faithful receiving doctrine that shifts with interpretive control. Substantial, short of pure-extraction territory because the coordination goods are real and broadly distributed. Suppression 0.60: enforcement is episodic rather than total — withdrawn books, revoked teaching mandates, irregularized communities on one flank, silenced reformers on the other — but it is bidirectional and recurs whenever a reading hardens. Theater_ratio 0.50: roughly half the arrangement's observable activity is formula-management — anniversary rhetoric, spirit-versus-letter invocations, celebrations of a consensus that was purchased rather than reached — layered over a thinner core of genuine clarification. Accessibility_collapse 0.60: inside the framework, the alternative of a plain reading collapses — every use of the texts becomes hermeneutical labor policed by the office — while exit remains possible at the price of schism, keeping collapse incomplete. Resistance 0.55: sustained two-flank resistance that neither extinguishes nor succeeds. The measurement series share one decadal grid (points 0-60; 0 = October 1962, council opening): extraction and suppression climb as the texts bind and enforcement matures, theater climbs as rhetorical maintenance thickens and then plateaus.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the interpreter's chair the arrangement is legitimate development requiring guidance — authority exercised, not taken. From the absorbed minority's chair it is a permanent motion to table: objections enter archives, never answers. From the theologians' chairs the same openness is livelihood and hazard in one instrument. From the faithful's chairs it is weather — doctrine that changes overhead without consultation. The engine derives these per-seat classifications from the declared roles, powers, and exits; the divergence between them is the finding, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the drafters collected the supermajority record outright; the interpreter collects deference and agenda control while bearing management costs that keep it off the pure-beneficiary pole; implementers collect delegated discretion offset by rework when readings shift. Victim declarations drive high directionality: minority fathers sit nearest the full-target pole (trapped, absorbed); traditionalists and progressives sit near it with identity lock amplifying their effective burden; the faithful are diffuse targets — individually small costs, collectively the largest borne. Theologians are the genuinely mixed seat: victim-declared, but the same openness that exposes them funds their work, so their effective extraction sits below the trapped-payer band. No directionality overrides were needed: the power atoms separate the seats cleanly enough for the structural derivation, and the one mixed case is carried by the secondary-role declaration rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabels. Calling the settlement pure coordination would erase the asymmetry — the coordination was purchased with buried disagreement, and the bill is paid by identifiable seats. Calling it pure extraction would erase the goods — unity, liturgy, ecumenism — that even the paying seats largely value and that would not survive open factional war. The hybrid category holds both facts. On obsolescence: the founding problem (modernize without schism) is arguably still live, so no mandatrophy is declared; the arrangement is not performing a dead mandate, it is performing an unresolved one. The decay risk runs the other way: if the interpretive office ever stopped enforcing and the factions stopped hoping, the formulas would persist as pure ceremony — theatrical maintenance of a settlement nobody believes resolves anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates one reading (composite_overdetermination_reading) of the kernel vatican_ii_magisterial_authority; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No empirical resolution — the readings partition the same textual corpus. Resolution would require the contending parties to agree on a criterion of textual determinacy, which is itself the disputed ground.',
    'If continuity_reading were adopted, epsilon drops toward negligible (organic development, no burden beyond coordination cost) and the victim set empties; if rupture_reading were adopted, the victim set expands to the whole pre-conciliar church and epsilon rises sharply. The disagreement is located in whether the conciliar texts possess a determinate meaning at all: both siblings assume they do; this reading denies it and relocates authority in interpretive control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings alter epsilon and victim sets over the same arrangement.').

omega_variable(
    ambiguity_deliberateness,
    'Were the ambiguous compromise formulations a deliberate strategy to assemble supermajorities, or the emergent residue of genuine theological pluralism that no drafting committee could resolve?',
    'Drafting-stage archives: the relatio syntheses, the modi submissions, roll-call votes before and after specific amendments, and periti memoirs correlating wording changes with vote swings.',
    'Deliberate ambiguity weights the engineered-extraction side of the ledger (coordination purchased with buried disagreement); emergent ambiguity supports a good-faith-coordination account in which later hardening, not design, produced the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_deliberateness, empirical, 'Whether textual overdetermination was designed or accumulated.').

omega_variable(
    minority_vote_baseline,
    'Do the 10-12% negative and abstention votes on the contested schemas signal unresolved theological incompatibility embedded in the final texts, or fall within the normal dissent band of any large deliberative assembly?',
    'Compare dissent rates on the same schemas across drafting stages, against uncontested schemas at the same council, and against comparable supermajority assemblies; persistent above-baseline dissent clustered on the same doctrines indicates embedded incompatibility.',
    'Above-baseline clustered dissent confirms the composite structure as load-bearing rather than cosmetic; baseline-level dissent would recast the minority as routine losers of ordinary procedure and lower measured extraction from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_vote_baseline, empirical, 'Whether rejection votes carry structural signal or procedural noise.').

omega_variable(
    hermeneutical_authority_locus,
    'Does magisterial authority after the council reside in the texts themselves or in the office that adjudicates their meaning?',
    'Conceptual: examine cases where the office''s ruling contradicts the most natural textual reading and observably prevails without legitimacy cost. Systematic displacement of natural readings by office rulings locates authority in the office.',
    'If authority is textual, ambiguity dilutes authority and the arrangement drifts toward inertial persistence; if authority is interpretive-office-based, the texts function as instruments and the burden concentrates on whoever loses each interpretive round.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_authority_locus, conceptual, 'Locus-of-authority question underlying the hermeneutical-control claim.').

omega_variable(
    implementation_divergence_cause,
    'Is the wide divergence in post-conciliar implementation across regions a structural feature the texts license, or contingent failure of execution that a firmer hand would have prevented?',
    'Cross-regional comparison holding the texts constant: if divergence tracks local political and cultural variables beyond what textual latitude permits, contingency dominates; if regions with similar contexts still diverge along the texts'' fault lines, structure dominates.',
    'Structural divergence confirms this reading''s central prediction; contingent divergence would shift responsibility from text-design to governance and lower the arrangement''s measured extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_divergence_cause, empirical, 'Whether implementation divergence is licensed by the texts or imposed on them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_composite_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t0, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t10, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t20, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t30, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t40, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t50, observed).
narrative_ontology:measurement(vatican_ii_composite_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(vatican_ii_composite_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vatican_ii_composite_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t0, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t10, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t20, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t30, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t40, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t50, observed).
narrative_ontology:measurement(vatican_ii_composite_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement_basis(vatican_ii_composite_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_composite_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t0, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t10, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t20, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t30, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t40, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t50, observed).
narrative_ontology:measurement(vatican_ii_composite_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(vatican_ii_composite_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'Vatican II' covers three structurally distinct claims. This story authors epsilon for the standing post-conciliar arrangement AS the composite reading sees it (overdetermined texts, interpretive enforcement, absorbed minorities). The continuity reading authors epsilon for the same arrangement as organic development (near-negligible extraction); the rupture reading authors a different victim set (the pre-conciliar church) and higher extraction. Downstream/upstream structure: the continuity reading supplies the legitimating vocabulary the arrangement's administrators cite, so this reading exerts pressure on it by undermining its plainness claim; the rupture reading competes for the same ambiguity this reading says the texts contain. Each file keeps one stable epsilon; the family links preserve the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
