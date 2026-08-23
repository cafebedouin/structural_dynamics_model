% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Order (Coordination Reading of the Feud Kernel)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the feud_obligation_kernel: the
 *   stateless_coordination_reading, in which blood-feud obligation operates
 *   as a self-enforcing coordination mechanism supplying justice and
 *   deterrence where no central enforcer exists. The paradigmatic case is the
 *   Icelandic Commonwealth — interval 0-332 indexes roughly 930 CE (the
 *   Althing settlement order) to 1262 CE (the Old Covenant) — read
 *   comparatively against segmentary-lineage feud systems documented
 *   elsewhere in the stateless-world record. Structurally, a killing opens a
 *   legally cognizable claim held by the slain person's close kin,
 *   prosecutable by vengeance or convertible to scheduled compensation
 *   (wergild); every lineage's standing pledge of retaliation prices violence
 *   for all its neighbors at once. The reading's declared structure places
 *   feud-participating lineages in the beneficiary set (they receive
 *   deterrence and a recognized justice procedure) and feud-defectors in the
 *   victim set (honor loss, withdrawal of protection, expulsion), with
 *   alternative dispute mechanisms — compensation settlement, neutral
 *   arbitration, emigration — remaining open, hence low suppression of
 *   alternatives. Per the kernel-reading rules, epsilon is authored for the
 *   standing feud arrangement as this reading assesses it, never for the
 *   royal-justice successor order it regards as external; the claimed type
 *   and the metrics are independent authored facts, and per-seat computations
 *   are left free to diverge. The sibling readings — extraction_cycle_reading
 *   and christianized_pacification_reading — are separate files with their
 *   own epsilon and beneficiary/victim structures, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - feud_participant_kin_groups: Primary beneficiary (organized/generational, identity_locked) — receives deterrence and a recognized justice procedure
 *   - - homicide_victims_next_of_kin: Right-holder and risk-bearer (moderate/biographical, identity_locked) — holds the claim, bears execution exposure
 *   - - feud_defectors: Sanctioned cost-bearer (powerless/biographical, trapped) — honor loss and kinship expulsion
 *   - - kin_group_elders_and_settlement_brokers: Agenda-setter (organized/generational, constrained) — declares claims, authorizes raids, fixes wergild terms
 *   - - opposing_kin_groups_in_live_feud: Counterparty participant (organized/generational, constrained) — ex ante co-beneficiary of mutual deterrence, ex post co-payer in blood and wealth
 *   - - wergild_settlement_recipients: Settlement-track beneficiary (moderate/biographical, constrained) — converts the claim to compensation
 *   - - ecclesiastical_vengeance_opponents: Excluded objector (institutional/civilizational, mobile) — holds the sibling christianized-pacification position
 *   - - legal_anthropology_observers: Analytical observer (analytical/civilizational, analytical) — sees the full cross-cultural structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.55).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.31).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Order (Coordination Reading of the Feud Kernel)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'd2e49c30-4ae3-462c-b8bc-b84d8d834c63').
narrative_ontology:cs_kernel_codification('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', formalized).
narrative_ontology:cs_authority_grounding('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', practice).
narrative_ontology:cs_interpretation_layer_present('d2e49c30-4ae3-462c-b8bc-b84d8d834c63').
narrative_ontology:cs_reading_relation('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', foundational, vengeance_reciprocity_constitutes_justice).
narrative_ontology:cs_axiom_status(vengeance_reciprocity_constitutes_justice, holdable).
narrative_ontology:cs_axiom_grounding('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', vengeance_reciprocity_constitutes_justice, instrumental).
narrative_ontology:cs_axiom('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', secondary, private_enforcement_legitimate_absent_public_authority).
narrative_ontology:cs_axiom_status(private_enforcement_legitimate_absent_public_authority, holdable).
narrative_ontology:cs_axiom_grounding('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', private_enforcement_legitimate_absent_public_authority, conventional).
narrative_ontology:cs_reference_frame('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', stateless_kin_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', late_commonwealth_sturlung_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2e49c30-4ae3-462c-b8bc-b84d8d834c63', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participant_kin_groups).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, homicide_victims_next_of_kin).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_settlement_recipients).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, opposing_kin_groups_in_live_feud).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, homicide_victims_next_of_kin).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, opposing_kin_groups_in_live_feud).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, decentralized_deterrence_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extended lineages that hold feud rights and owe feud duties: a killing of one of their members opens a claim they may prosecute by vengeance or convert to compensation, and their standing pledge of retaliation is what makes neighboring groups hesitate. Membership is by birth; a lineage that abandoned the obligation would forfeit the protection the system extends to every member, so the group's identity and its security are the same thing.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participant_kin_groups, beneficiary,
    organized, generational, identity_locked, regional).

% The close relatives of a slain person hold the legally recognized claim: they may take vengeance, accept wergild, or broker a settlement, and the arrangement guarantees their claim will be treated as valid. The same position exposes them personally — executing vengeance invites counter-vengeance against them and theirs, and refusing to act shames the lineage they belong to.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, homicide_victims_next_of_kin, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, homicide_victims_next_of_kin, payer).

% Men who refuse a vengeance duty or abandon a feud mid-course: they lose honor standing, their lineage withdraws protection and may expel them outright, and no neighboring group shelters a man marked as having deserted his dead. Once marked, they cannot buy back standing except through renewed service to the very duty they tried to leave.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, local).

% The lineage on the other side of an active claim: before a feud opens, both sides live under the same pledged-retaliation umbrella that keeps either from casual violence, and both count as holders of the system's protections. Once a feud is live, it kills their members, burns their farm buildings, and forces them to fund wergild or raiding parties. Their exit runs through settlement negotiation or victory, not departure.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, opposing_kin_groups_in_live_feud, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, opposing_kin_groups_in_live_feud, payer).

% Senior kinsmen, chieftains, and respected neutral figures who decide when a killing ripens into a prosecutable claim, authorize vengeance parties, declare truces, and fix compensation terms. Their authority rests on being seen to administer the custom fairly; they gain standing from successful settlements but cannot exempt their own lineages from the duties they administer.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_group_elders_and_settlement_brokers, agenda_setter,
    organized, generational, constrained, regional).

% Claimants who convert a vengeance right into a compensation payment: they receive livestock, silver, or land from the slayer's kin according to the injured person's rank, and the exchange ends. Taking compensation is lawful and common, though it trades the standing won by vengeance for wealth and peace.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_settlement_recipients, beneficiary,
    moderate, biographical, constrained, local).

% Church authorities and their envoys, who hold that vengeance belongs to God and to delegated spiritual or royal offices, not to kin obligation. Before conversion they are simply absent from the northern councils; afterward they preach against feud, offer penitential routes out of vengeance duties, and eventually help assemble the royal peace that replaces the custom — while remaining outside the kin-council conversation that defines legitimacy for most of the interval.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, ecclesiastical_vengeance_opponents, excluded,
    institutional, civilizational, mobile, continental).

% Comparative legal historians and ethnographers who study feud systems across stateless societies — saga-era Iceland, segmentary lineage peoples, medieval borderlands — recording settlement rates, casualty patterns, and enforcement costs, and weighing rival interpretations of what the obligation accomplishes.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, legal_anthropology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In regions lacking central coercive authority, the feud obligation deters inter-kin predation: every killing creates a legally cognizable claim enforceable by the slain person's kin, making homicide predictably costly, and channels the response through recognized procedures — declared vengeance, truce, negotiated compensation — rather than unbounded violence.
% TRANSFER_FUNCTION: Delivers deterrence security symmetrically to all participating kin groups; upon settlement, moves compensation wealth (wergild) from the slayer's lineage to the slain person's lineage; places the blood-debt obligation on the offender's kin as a group; concentrates the physical risk of executing claims on the young men who carry them out.
% ABSENT_VOICES: Ecclesiastical authorities holding the divine-law prohibition on vengeance stand outside the kin councils that define legitimate process, pressing their objection from the margins until conversion shifts the balance. Women, who broker the marriage alliances that make and unmake feuds and frequently transmit claims to their sons, deliberate informally but hold no council seat. The feud dead do not speak. The future royal administrators are absent because no such office yet exists — their later arrival is what ends the arrangement.
% DISAPPEARANCE_RATIONALE: Overnight removal in a stateless region eliminates the only credible homicide deterrent: stronger lineages resume preying on weaker ones until some substitute order — assembly law, chieftain protection, or royal justice — emerges; the wergild schedules and settlement networks lose their enforcement backdrop; and kin groups reorganize around whatever protection they can purchase, migrate toward, or mount themselves.
% FOUNDING_PROBLEM: Armed autonomous kin groups with no common enforcer: killings and takings among them would otherwise go unanswered, inviting aggression by whichever lineage could mobilize more men, and leaving the injured with no recognized recourse at all.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: ecclesiastical chroniclers concede the enforcement vacuum even while condemning the remedy, penitential sources distinguishing the necessity of order from the sin of vengeance; royal commissioners documenting the enforcement vacuum in their own words when taking over the Commonwealth at the Old Covenant; and modern comparative legal anthropology finding the same vacuum generating feud-forms across unrelated stateless societies. No corroboration rests solely on the feud-participating lineages' own testimony.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state epsilon of 0.55 reflects an arrangement that aged past its coordination prime: early in the interval (epsilon around 0.28) the deterrence dividend exceeded feud costs — homicide was priced, claims were honored, and most exchanges ended in compensated settlement. The late rise tracks mandate aging: claims prosecuted for lineage standing rather than security, chieftains converting feud-following into political capital, and the escalated conflicts of the final decades whose casualty profile exceeded any deterrence return. Suppression is authored at 0.31 and unscaled — the engine owns any scope or directionality scaling — reflecting the reading's declared low-suppression structure: wergild, arbitration, emigration, and eventually royal and penitential exit routes remained open throughout, and the gently falling suppression_requirement series records widening exits rather than intensifying enforcement. Theater_ratio rises from 0.12 to 0.42: honor displays began as costly signals doing real deterrent work, but by interval end formal vengeance gestures and assembly rhetoric often substituted for substantive prosecution — performance outliving function. Accessibility_collapse (0.48) and resistance (0.52) are authored honestly for a working coordination order with visible alternatives and routine internal peace-seeking. All three series run on one shared seven-point grid so every metric is authored at every examined time point; the suppression series is included because enforcement-capacity attrition is a traced dynamic of this story, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same arrangement. The lineage seat — identity-locked, generational horizon, born into the protection-and-duty bundle — computes the feud as order itself. The next-of-kin seat blends a benefit (a guaranteed-valid claim) with payer exposure (counter-vengeance risk falls on the claim-holder's household). The defector seat computes near-full-target treatment: sanction, expulsion, and no voice anywhere in the process. The elder seat administers the custom and collects standing from successful brokerage while remaining unable to exempt its own line. The opposing-lineage seat oscillates between ex ante co-beneficiary of mutual deterrence and ex post co-payer in a hot feud. The ecclesiastical seat, holding the sibling christianized reading, computes the same events as sin awaiting displacement. The engine derives these per-seat classifications from the authored structural positions; nothing here adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-declared lineages sit near the subsidy end of directionality: deterrence and procedure are delivered to them. Feud_defectors, the declared victim set, sit near the full-target end: they bear the sanction structure without collecting the protection. Dual-role seats (homicide_victims_next_of_kin as beneficiary/payer; opposing_kin_groups_in_live_feud as beneficiary/payer) derive blended directionalities between the poles. The excluded ecclesiastical seat has no beneficiary or victim anchor in this reading's transfer loop, so the derivation leaves it mid-structure rather than forcing an override — no directionality_overrides are authored because the beneficiary/victim and exit data already yield the correct relationships. Regional spatial scope modestly amplifies verification difficulty in the engine's computation, which is appropriate: feud compliance was verified neighbor-to-neighbor, not centrally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an enforcement vacuum among armed autonomous lineages — was live for nearly the whole interval, so no resolved-mandatrophy flag is authored; and the arrangement did not linger past its function as empty performance: it dissolved with the Commonwealth itself in 1262 rather than persisting theatrically under the new royal order. The classification work here is preventive in both directions. The rope claim stops the defector-sanction structure from being misread as asymmetric rent collection — sanctioning non-contribution is cooperation maintenance, the same move any cooperative norm makes against free-riding — while the recorded rise in theater and extraction marks exactly the transition zone where the sibling extraction_cycle_reading becomes historically plausible. The corpus takes that claim-versus-metric divergence as measurement, not as an error to reconcile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the blood-feud obligation best instantiated as this stateless-coordination constraint (a net-beneficial deterrence order), or as one of the sibling instantiations — the extraction cycle (net-destructive depletion that blocks territorial consolidation) or the christianized-pacification reading (illegitimate violence awaiting displacement by delegated divine and royal authority)?',
    'Comparative corpus analysis across documented feud societies: settlement-completion rates, feud mortality as a share of total violent death, enforcement cost per claim honored, and whether the deterrence dividend exceeds feud casualties; adjudicate between readings by whichever beneficiary/victim structure the aggregate record supports.',
    'If extraction-cycle evidence dominates, this reading''s beneficiary set is mis-specified, epsilon rises toward the sibling''s authored value, and the computed classification shifts toward tangled_rope or snare; if the coordination evidence holds, the sibling readings remain minority-seat instantiations computed from their own structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which instantiation of the feud_obligation_kernel the historical arrangement actually supports.').

omega_variable(
    settlement_track_openness,
    'How open was the wergild and settlement alternative in practice, given that honor pressure could compel continued feud even where compensation was legally available?',
    'Count settlement outcomes versus vengeance outcomes across documented feud cases; measure the standing-cost premium paid by parties who settled against kin expectation.',
    'If the settlement track was effectively closed by honor coercion, suppression is materially understated here, the low-suppression structural delta fails, and the reading drifts toward tangled_rope; if settlement was genuinely routine, the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_track_openness, empirical, 'Whether the coexisting compensation alternative was substantively usable or honor-blocked.').

omega_variable(
    honor_internalization_ambiguity,
    'Is the measured suppression of feud-defection structural (expulsion, outlawry, loss of kin protection) or internalized (shame constituting the self such that refusal is unthinkable before any sanction lands)?',
    'Trace post-exit trajectories of documented defectors: those who left kin protection under church or royal guarantee and report durable relief indicate internalization was minor; those who sought reinstatement or died in disgrace indicate internalized suppression carried the load.',
    'If internalization dominates, effective suppression exceeds the structural measure and persists beyond the arrangement''s dissolution — explaining why feud norms resurfaced in frontier zones long after state arrival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism behind defection sanctions.').

omega_variable(
    latent_hierarchy_dependency,
    'Is the feud obligation genuinely self-enforcing among equals, or does it depend on latent hierarchy — chieftains and big men whose prestige interest sustains enforcement — such that the self-enforcing label understates a hidden administrative seat?',
    'Trace enforcement episodes: who mobilizes vengeance parties, who brokers truces, whether settlement terms track kin-council judgment or chieftain advantage; compare enforcement durability in lineages lacking prominent leaders.',
    'If latent hierarchy carries enforcement, the arrangement harbors a concentrated beneficiary seat the current stakeholder set underspecifies, gains concentrate accordingly, and the reading slides toward tangled_rope with chieftains as the receipt seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_hierarchy_dependency, conceptual, 'Whether enforcement is horizontally self-enforcing or vertically dependent on emergent big-men authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 332).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_coord_reading_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t0, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t55, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 55, 0.14).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t55, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t111, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 111, 0.18).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t111, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t166, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 166, 0.23).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t166, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t221, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 221, 0.29).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t221, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t277, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 277, 0.35).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t277, observed).
narrative_ontology:measurement(feud_coord_reading_tr_t332, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 332, 0.42).
narrative_ontology:measurement_basis(feud_coord_reading_tr_t332, observed).

% Extraction over time
narrative_ontology:measurement(feud_coord_reading_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(feud_coord_reading_be_t0, observed).
narrative_ontology:measurement(feud_coord_reading_be_t55, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 55, 0.3).
narrative_ontology:measurement_basis(feud_coord_reading_be_t55, observed).
narrative_ontology:measurement(feud_coord_reading_be_t111, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 111, 0.34).
narrative_ontology:measurement_basis(feud_coord_reading_be_t111, observed).
narrative_ontology:measurement(feud_coord_reading_be_t166, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 166, 0.39).
narrative_ontology:measurement_basis(feud_coord_reading_be_t166, observed).
narrative_ontology:measurement(feud_coord_reading_be_t221, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 221, 0.45).
narrative_ontology:measurement_basis(feud_coord_reading_be_t221, observed).
narrative_ontology:measurement(feud_coord_reading_be_t277, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 277, 0.5).
narrative_ontology:measurement_basis(feud_coord_reading_be_t277, observed).
narrative_ontology:measurement(feud_coord_reading_be_t332, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 332, 0.55).
narrative_ontology:measurement_basis(feud_coord_reading_be_t332, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_coord_reading_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(feud_coord_reading_su_t0, observed).
narrative_ontology:measurement(feud_coord_reading_su_t55, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 55, 0.43).
narrative_ontology:measurement_basis(feud_coord_reading_su_t55, observed).
narrative_ontology:measurement(feud_coord_reading_su_t111, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 111, 0.41).
narrative_ontology:measurement_basis(feud_coord_reading_su_t111, observed).
narrative_ontology:measurement(feud_coord_reading_su_t166, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 166, 0.38).
narrative_ontology:measurement_basis(feud_coord_reading_su_t166, observed).
narrative_ontology:measurement(feud_coord_reading_su_t221, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 221, 0.36).
narrative_ontology:measurement_basis(feud_coord_reading_su_t221, observed).
narrative_ontology:measurement(feud_coord_reading_su_t277, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 277, 0.34).
narrative_ontology:measurement_basis(feud_coord_reading_su_t277, observed).
narrative_ontology:measurement(feud_coord_reading_su_t332, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 332, 0.31).
narrative_ontology:measurement_basis(feud_coord_reading_su_t332, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'blood feud' decomposes into three structurally distinct instantiations of feud_obligation_kernel, authored as separate files per the epsilon-invariance principle: this stateless_coordination_reading (justice-and-deterrence order; beneficiaries = feud participants; end-state epsilon 0.55 by its own functionalist lights), the extraction_cycle_reading (destructive depletion cycle; substantially higher epsilon, victims = productive capacity and consolidating polities), and the christianized_pacification_reading (illegitimate violence; victim set = all feud participants under divine law, beneficiaries = delegated spiritual and royal offices). Upstream/downstream structure: this reading supplies the functional baseline that the extraction reading disputes and the christianized reading supersedes; each file links the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
