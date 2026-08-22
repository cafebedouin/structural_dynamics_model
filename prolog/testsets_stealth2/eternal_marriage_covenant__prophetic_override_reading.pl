% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override of the Eternal Marriage Covenant (1890 Manifesto Reading)
 *   domain: religious/political_theology/commitment_system
 *
 * SUMMARY:
 *   The kernel is the eternal marriage covenant of D&C 132, canonized in 1876
 *   as binding scripture. Three readings instantiate three different
 *   constraints from that kernel. This file generates the
 *   prophetic_override_reading: the living prophet's
 *   circumstantially-required revelation supersedes prior binding revelation,
 *   as exercised in the 1890 Manifesto ending plural marriage under
 *   existential federal pressure. The standing arrangement under contest -
 *   and therefore the epsilon referent - is the override-governed
 *   covenant-obligation system itself, assessed by this reading's own lights:
 *   even granting that the override was legitimate, the reversal's costs
 *   landed on the parties with the least voice (stranded plural households,
 *   disciplined dissenters) while survival, retained property, and statehood
 *   accrued to the institution, and no member-side check constrains future
 *   exercises of the same channel. The sibling readings are separate
 *   constraints with their own epsilon, victims, and types:
 *   eternal_marriage_covenant__immutable_commandment_reading (D&C 132 as
 *   immutable law required for exaltation, held today chiefly by
 *   fundamentalist descendants) and
 *   eternal_marriage_covenant__temporal_accommodation_reading (practice
 *   suspended, doctrine intact, law-of-the-land priority). The claim/metrics
 *   split is deliberate: claimed_type is tangled_rope from this reading's
 *   seat - a genuine coordination channel carrying asymmetric extraction -
 *   while the metrics are authored descriptively of the arrangement's actual
 *   operation; the engine computes per-seat classifications and measures any
 *   divergence.
 *
 * KEY AGENTS:
 *   - church_presidency: Agenda-setter (institutional/arbitrage) - controls when the override channel fires; personal liberty and corporate assets rode on the timing
 *   - institutional_church: Primary beneficiary (institutional/constrained) - retains temples and property, keeps authority-continuity claim intact
 *   - utah_latter_day_community: Collective beneficiary (organized/constrained) - amnesty, restored franchise, statehood, end of the raids
 *   - plural_marriage_households: Primary target (powerless/trapped) - bear the reversal's household-level costs without consultation or restitution
 *   - post_manifesto_dissenters: Secondary target (organized/identity_locked) - apostles and elders disciplined out for treating the earlier mandate as still binding
 *   - rank_and_file_membership: Dual-positioned (moderate/constrained) - peace and legality gained, epistemic cost of the unacknowledged reversal paid
 *   - federal_authorities: External beneficiary (institutional/mobile) - compliance obtained, enforcement wound down
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.56).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.52).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of the Eternal Marriage Covenant (1890 Manifesto Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '00d46eeb-69fe-44e3-804c-906611fc0559').
narrative_ontology:cs_kernel_codification('00d46eeb-69fe-44e3-804c-906611fc0559', fixed_text).
narrative_ontology:cs_authority_grounding('00d46eeb-69fe-44e3-804c-906611fc0559', lineage).
narrative_ontology:cs_interpretation_layer_present('00d46eeb-69fe-44e3-804c-906611fc0559').
narrative_ontology:cs_reading_relation('00d46eeb-69fe-44e3-804c-906611fc0559', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('00d46eeb-69fe-44e3-804c-906611fc0559', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('00d46eeb-69fe-44e3-804c-906611fc0559', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('00d46eeb-69fe-44e3-804c-906611fc0559', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('00d46eeb-69fe-44e3-804c-906611fc0559', secondary, covenant_obligations_track_current_oracle).
narrative_ontology:cs_axiom_status(covenant_obligations_track_current_oracle, holdable).
narrative_ontology:cs_axiom_grounding('00d46eeb-69fe-44e3-804c-906611fc0559', covenant_obligations_track_current_oracle, theological).
narrative_ontology:cs_reference_frame('00d46eeb-69fe-44e3-804c-906611fc0559', living_oracle_administered_covenant).
narrative_ontology:cs_drift_state('00d46eeb-69fe-44e3-804c-906611fc0559', contemporary_official_discourse, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('00d46eeb-69fe-44e3-804c-906611fc0559', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, institutional_church).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, utah_latter_day_community).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, plural_marriage_households).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, post_manifesto_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_presidency).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, federal_authorities).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, prophetic_succession_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wilford Woodruff and his counselors hold sole recognized authority to receive and declare binding revelation for the community. Through 1887-1890 they weigh continued practice against confiscation of temples, corporate dissolution, and the imprisonment of the leadership itself, then issue the 1890 declaration ending the practice. They decide when the override channel fires, face no internal appeal, and their own liberty and the corporation's assets ride on the timing.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_presidency, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, church_presidency, beneficiary).

% The corporate church retains the temples, meetinghouses, and tithing income that escheatment proceedings threatened to seize, and keeps its claim to continuous divine authority intact by routing the reversal through the living oracle rather than conceding that any prior teaching was wrong. Its alternative to adaptation is dissolution or schism, so it absorbs the reversal into its own authority structure and continues operating.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, institutional_church, beneficiary,
    institutional, generational, constrained, continental).

% Settler communities in the territory gain amnesty, restoration of franchise prospects, and statehood in 1896; the marshal raids, arrests, and the underground hiding system end. They pay indirectly through a fractured collective memory of the practice decades and through families among them who absorbed the reversal personally.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, utah_latter_day_community, beneficiary,
    organized, generational, constrained, regional).

% Families sealed under the 1852 mandate - wives who had left first marriages for covenant plural unions, children counted into covenant lines - watch the practice become criminalized and then rescinded without ceremony or restitution. Support obligations, inheritance expectations, and the promised exaltation framework dissolve around them. Leaving the community means losing salvation as taught; staying means living inside a reversal no one will name. No council consulted them before the decision.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, plural_marriage_households, payer,
    powerless, generational, trapped, regional).

% Apostles and elders - John W. Taylor and Matthias F. Cowley among them - who performed or entered new sealings after 1890 treat the earlier commandment as still binding on them. They resign or are dropped from their quorums, and the most persistent are eventually excommunicated. Their self-concept as covenant-keepers makes compliance with the reversal feel like apostasy; the only exit that preserves the practice is flight to the colonies in Mexico and Canada, and later to the schismatic settlements.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, post_manifesto_dissenters, payer,
    organized, biographical, identity_locked, continental).

% Ordinary members gain peace, legal safety, and social respectability as the raids stop and the territory moves toward statehood. They also absorb the epistemic cost of having been taught for four decades that the practice was required for exaltation and then watching it end without doctrinal accounting. Voicing doubt about the reversal marks a member as weak in faith; leaving costs kin, community, and salvation as taught.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership, payer).

% Congress, the courts, and territorial officials obtain compliance with federal marriage law without pursuing confiscation to the bitter end. They accept the 1890 declaration at face value, wind down enforcement, and close the file - a forbearance that is itself part of what lets the internal settlement hold.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_authorities, beneficiary,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, institutional_church).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authorized channel for revising binding covenant obligations when the community's survival is threatened: instead of each member deciding case-by-case whether to keep a criminalized practice, the community routes the decision through one recognized oracle, preserving unity, coordinated legal compliance, and the continuity of institutional authority across a reversal.
% TRANSFER_FUNCTION: During the practice era, the covenant program moved marital and household arrangements, and the personal liberty of practitioners who went underground or to prison, from member families into the church's covenant scheme. Through the 1890 override, the costs of reversal - dissolved sealing expectations, disrupted plural households, excommunication of dissenters - moved onto practitioner families and dissenters, while survival, retained property, and statehood accrued to the corporate institution.
% ABSENT_VOICES: Sealed plural wives - particularly younger women whose marriages the reversal stranded - had no seat in the councils that decided it; their petitions survive at the margins of the record. Dissenting apostles were removed rather than heard. The fundamentalist descendants who inherit the reversal's costs stand wholly outside the tradition's account of the episode.
% DISAPPEARANCE_RATIONALE: If the override channel had not fired in 1890, the church faces disincorporation, temple confiscation, and a leadership imprisoned past the point of orderly succession; the community fragments between compliance and exile. If the mechanism were absent from the tradition permanently, later adaptations - the 1978 priesthood extension follows the same pattern - lose their channel. The modern shape of mainstream Latter-day Saint practice, and the fundamentalist schisms that define themselves against it, both depend on this arrangement.
% FOUNDING_PROBLEM: How a covenant community that holds its obligations to arrive through continuing revelation can retire a practice it had declared eternally mandated, under existential legal coercion, without either shattering its claim to continuous divine authority or forcing every member into private conscience decisions that would fracture the community.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the federal-pressure sequence (Morrill Act 1862, Reynolds v. United States 1879, Edmunds Act 1882, Edmunds-Tucker Act 1887) is documented in congressional records and case law independent of the church, and the Reed Smoot hearings transcript records leadership testimony under oath about how the ending was decided. The revelatory character of the override is attested only by the tradition's own offices - no external source can confirm or deny it - while the survival calculus is visible in Woodruff's own public statement that he saw beforehand exactly what would take place if the practice continued.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56: the arrangement delivered real goods - unity preserved through a crisis that otherwise meant dissolution, a lawful exit from a criminalized mandate, and a reusable adaptation channel (exercised again in 1978) - but the reversal's costs were neither consulted nor restituted, falling on sealed wives, plural households, and dissenters, and the channel itself leaves members with no recourse against future reversals of what they are told is eternal. Suppression 0.52 (raw, unscaled - the engine applies directionality and scope scaling to extractiveness only): enforcement did not disappear after 1890, it changed hands - from federal marshals and escheatment courts to internal discipline (the 1904 Second Manifesto, forced resignations, excommunications), plus steady discouragement of the doctrinal question itself. Theater 0.48: the decade 1890-1904 shows a widening gap between the public compliance narrative and continued private sealings in the colonies, the Manifesto's careful framing as advice rather than renunciation, and apostolic testimony at the Smoot hearings that aged badly under oath; after 1904 the practice genuinely ends and theater declines, though the continuity narrative ('the principle remains, only the practice paused') persists. Accessibility_collapse 0.5: exits existed - the colonies, schism, simple departure - but each carried severe spiritual and social cost, so alternatives narrowed without vanishing. Resistance 0.48: apostolic resignations, fundamentalist persistence, and member questioning were real and were met with discipline, not mere neglect. The measurement series run on one shared eight-point grid (1852-1910) so every tracked metric is authored at every examined time point; the trajectories show a rise-crest-fall-rise shape driven by the locus of enforcement shifting from federal to internal, not by intermittent reinforcement - the oscillation is a handoff, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural inputs. From the presidency's position the arrangement is the tradition working as designed: the oracle weighed catastrophe, received direction, and saved the community - a coordination triumph. From a stranded plural wife's position in 1892 the same event is the unilateral voiding of her covenant expectations by the very office that declared them eternal; from a dissenting apostle's position it is betrayal enforced by excommunication. Rank-and-file members sit between: beneficiaries of the peace, payers of the epistemic cost. The engine computes this divergence per seat from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency derives near the beneficiary end (agenda-setter plus survival collector, arbitrage-grade control of the channel) despite bearing real personal legal risk before 1890. The institutional church is a full beneficiary: property retained, authority continuity preserved. The Utah community benefits collectively with modest indirect cost. Federal authorities benefit from compliance with mobile exit - near the beneficiary end. Plural marriage households derive near the full-target end: victims with trapped exit and no decision power. Post-manifesto dissenters sit at the extreme target end: victims whose identity lock makes exit equivalent to self-damnation in their own framework. One explicit override is authored: rank_and_file_membership (moderate power atom) to d=0.45. The structural derivation would read their beneficiary role and place them near 0.15-0.2, but their actual position is near-symmetric - they gained communal peace and legal safety while paying real subordination-of-conscience costs and absorbing the reversal's unacknowledged losses through their own sealed kin. The override corrects the derivation where the dual role would mislead; no other seat needs correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the arrangement as pure rope - the tradition's preferred frame - erases the stranded households and the excommunicated dissenters, converting an asymmetric settlement into innocent coordination. Reading it as pure snare - the critical outsider's frame - erases the genuine coordination delivered: unity preserved through an existential crisis, a lawful collective exit, and a reusable adaptation channel the tradition has relied on since. Tangled rope holds both facts in one structure: the same channel that saved the corporation transferred the reversal's costs downward onto those with no seat. On mandatrophy proper: the founding problem - retiring an eternal mandate without shattering the authority claim - remains live, not dead; the mechanism was reused in 1978 and remains doctrine, so there is no dead-mandate drift to resolve. The residue to watch is the unacknowledged-cost remainder: if the tradition never accounts for the reversal's casualties, the historical-memory function of the arrangement decays into performance while the mechanism itself stays live - a partial piton formation inside a live tangled rope, detectable as rising theater in the later measurement record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the eternal_marriage_covenant kernel describes the operative constraint - prophetic override (this file), immutable commandment, or temporal accommodation?',
    'Adoption-pattern analysis: which reading actually governs practice rulings, discipline decisions, and sealings across the tradition''s administrative bodies, as distinct from official prose.',
    'Each reading yields a different epsilon, a different victim set, and a different type; the numbers in this file hold only under the override reading. Under the immutable reading the arrangement is a standing demand on members; under the accommodation reading the doctrine is untouched and only practice paused.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one of three rival instantiations of the D&C 132 kernel; classification is reading-indexed.').

omega_variable(
    revelation_vs_survival_causation,
    'Was the 1890 override a genuine revelatory event received by the prophet, or a survival-driven policy reversal clothed in revelatory form?',
    'Council minutes, Woodruff''s diaries, and contemporaneous private correspondence compared against the public framing and against the legal deadline structure (escheatment proceedings, pending legislation); comparative sequencing of the decision against the coercive timeline.',
    'If the reversal was purely political, the override channel functions as authorization-laundering and the arrangement drifts toward the snare side; if revelatory, the coordination function is stronger than the metrics suggest and the rope component firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_survival_causation, empirical, 'Authenticity of the revelatory character of the override versus survival calculus as the driver.').

omega_variable(
    reversal_cost_distribution,
    'Who concretely bore the reversal''s costs - how many plural households lost support or dissolved, what became of sealed wives'' status and children''s covenant-line expectations, and how many dissenters were disciplined or excommunicated between 1890 and 1920?',
    'Ward and Relief Society records, disciplinary council minutes, fundamentalist migration counts to the Mexican and Canadian colonies, and demographic reconstruction of plural households across the settlement period.',
    'Concentrated household-level harm raises effective extraction on the payer seats and supports the tangled-rope reading over rope; if the harm proves diffuse and largely voluntary, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_cost_distribution, empirical, 'Distribution and magnitude of the costs the reversal imposed on practitioner families and dissenters.').

omega_variable(
    member_compliance_mechanism,
    'Is rank-and-file acceptance of the reversal driven by conviction in the override''s legitimacy, or by identity and social lock-in that would persist regardless of belief?',
    'Post-1890 testimony patterns, dissent and exit rates, and the fundamentalist schism as a natural experiment: the minority unconvinced by the override visibly exited and formed schismatic communities, permitting comparison of stated belief against revealed preference.',
    'If lock-in dominates, part of the measured suppression is internalized and travels with members after any exit, raising effective suppression above the structural measure; if conviction dominates, the arrangement''s coordination function is more genuine than the suppression figure implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_compliance_mechanism, empirical, 'Conviction versus identity lock-in as the source of member compliance with the reversal.').

omega_variable(
    supersession_language_retreat,
    'Has the tradition''s later official discourse abandoned the supersession claim distinctive to this reading - that new revelation replaced the old mandate - in favor of suspension language closer to the temporal-accommodation sibling?',
    'Content analysis of official discourse from the Second Manifesto (1904) through the present for supersession framing versus suspension-and-continuity framing of the 1890 episode.',
    'If supersession language has been retired, this reading survives only as a mechanism rather than a doctrine, and the temporal_accommodation_reading better describes the standing arrangement - shifting epsilon and the victim set toward that sibling''s file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_language_retreat, conceptual, 'Whether the reading''s distinguishing claim is still held or has been quietly absorbed into accommodation language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1852, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1852, 0.15).
narrative_ontology:measurement_basis(eter_tr_t1852, observed).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1862, 0.18).
narrative_ontology:measurement_basis(eter_tr_t1862, observed).
narrative_ontology:measurement(eter_tr_t1879, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1879, 0.22).
narrative_ontology:measurement_basis(eter_tr_t1879, observed).
narrative_ontology:measurement(eter_tr_t1887, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1887, 0.25).
narrative_ontology:measurement_basis(eter_tr_t1887, observed).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement_basis(eter_tr_t1890, observed).
narrative_ontology:measurement(eter_tr_t1896, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1896, 0.5).
narrative_ontology:measurement_basis(eter_tr_t1896, observed).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.55).
narrative_ontology:measurement_basis(eter_tr_t1904, observed).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1910, 0.48).
narrative_ontology:measurement_basis(eter_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1852, 0.38).
narrative_ontology:measurement_basis(eter_be_t1852, observed).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1862, 0.44).
narrative_ontology:measurement_basis(eter_be_t1862, observed).
narrative_ontology:measurement(eter_be_t1879, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1879, 0.55).
narrative_ontology:measurement_basis(eter_be_t1879, observed).
narrative_ontology:measurement(eter_be_t1887, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1887, 0.7).
narrative_ontology:measurement_basis(eter_be_t1887, observed).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement_basis(eter_be_t1890, observed).
narrative_ontology:measurement(eter_be_t1896, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1896, 0.54).
narrative_ontology:measurement_basis(eter_be_t1896, observed).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement_basis(eter_be_t1904, observed).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1910, 0.56).
narrative_ontology:measurement_basis(eter_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1852, 0.2).
narrative_ontology:measurement_basis(eter_su_t1852, observed).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1862, 0.25).
narrative_ontology:measurement_basis(eter_su_t1862, observed).
narrative_ontology:measurement(eter_su_t1879, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1879, 0.4).
narrative_ontology:measurement_basis(eter_su_t1879, observed).
narrative_ontology:measurement(eter_su_t1887, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1887, 0.75).
narrative_ontology:measurement_basis(eter_su_t1887, observed).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement_basis(eter_su_t1890, observed).
narrative_ontology:measurement(eter_su_t1896, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1896, 0.45).
narrative_ontology:measurement_basis(eter_su_t1896, observed).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.6).
narrative_ontology:measurement_basis(eter_su_t1904, observed).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1910, 0.52).
narrative_ontology:measurement_basis(eter_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto and the eternal marriage covenant' covers three structurally distinct claims that the epsilon-invariance principle requires separating. The immutable_commandment_reading treats D&C 132 as fixed eternal law (high extraction on all members, no override possible); the prophetic_override_reading (this file) treats the covenant as administered through a living oracle whose new direction replaces old mandates (moderate extraction, asymmetrically distributed); the temporal_accommodation_reading treats the Manifesto as suspension without doctrinal touch (low doctrinal extraction, high continuity claim). The immutable reading is upstream in dissenters' argumentation - fundamentalists cite its permanence as evidence the override was illegitimate - while the accommodation reading functions as the retrospective hedge that lets the institution hold continuity and compliance simultaneously. Each story links the other two via affects_constraints; each carries its own epsilon, victim set, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
