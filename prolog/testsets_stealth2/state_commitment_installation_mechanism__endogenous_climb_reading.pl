% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Fringe-Climb Legitimacy Channel (Endogenous Reading)
 *   domain: historical sociology/state formation/cultural authority
 *
 * SUMMARY:
 *   Within the historical sociology of state formation, one persistent puzzle
 *   is how a polity's authoritative commitments - administrative methods,
 *   legal doctrines, religious settlements - get replaced when the center is
 *   invested in the old ones. This story instantiates the
 *   endogenous_climb_reading of the state_commitment_installation_mechanism
 *   kernel: new commitments gain legitimacy by climbing from institutional
 *   fringes through demonstrated superiority, not by decree. Peripheral
 *   advocates demonstrate, peripheral jurisdictions adopt first, certifying
 *   bodies convert results into portable legitimacy, and the center resists
 *   until displacement is complete. The claim/metric gap is deliberate: the
 *   reading CLAIMS a meritocratic coordination channel while the authored
 *   metrics describe a hybrid structure that also charges real costs -
 *   proof-labor to advocates, uncompensated displacement to the apex, and
 *   years of avoidable harm to populations living through the adoption lag.
 *   The engine measures that divergence; the claim is not reconciled to the
 *   metrics. Sibling readings (exogenous_imposition_reading,
 *   hybrid_cascade_reading) are separate constraints with their own epsilon
 *   values and beneficiary structures, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   fringe_advocates: dual-positioned climber (moderate/identity_locked) -
 *   pays proof-labor, collects legitimacy returns -
 *   early_adopter_peripheries: first-mover beneficiary (organized/arbitrage)
 *   - converts marginality into adoption advantage -
 *   validated_practice_communities: scaling beneficiary
 *   (organized/constrained) - inherits the growing stream of posts and
 *   deference - legitimacy_certifying_bodies: agenda_setter
 *   (institutional/constrained) - owns the demonstration standard, collects
 *   gatekeeping standing - apex_authority_holders: primary target
 *   (institutional/trapped) - loses agenda monopoly without compensation -
 *   superseded_commitment_incumbents: target (organized/identity_locked) -
 *   bears the write-down of the displaced commitment -
 *   adoption_lag_populations: pure-cost bearer (powerless/trapped) - lives
 *   the adoption lag as avoidable harm - unproven_systemic_critics: excluded
 *   voice (powerless/trapped) - filtered out by the demonstration standard
 *   itself - comparative_historians: analytical observer
 *   (analytical/analytical) - sees the full climb structure across cases
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.48).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.3).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Fringe-Climb Legitimacy Channel (Endogenous Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical sociology/state formation/cultural authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '71a6f040-3927-475d-ac3e-659280496d48').
narrative_ontology:cs_kernel_codification('71a6f040-3927-475d-ac3e-659280496d48', distributed).
narrative_ontology:cs_authority_grounding('71a6f040-3927-475d-ac3e-659280496d48', expertise).
narrative_ontology:cs_interpretation_layer_present('71a6f040-3927-475d-ac3e-659280496d48').
narrative_ontology:cs_reading_relation('71a6f040-3927-475d-ac3e-659280496d48', state_commitment_installation_mechanism__exogenous_imposition_reading, forecloses).
narrative_ontology:cs_reading_relation('71a6f040-3927-475d-ac3e-659280496d48', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('71a6f040-3927-475d-ac3e-659280496d48', foundational, legitimacy_constituted_by_demonstrated_performance).
narrative_ontology:cs_axiom_status(legitimacy_constituted_by_demonstrated_performance, holdable).
narrative_ontology:cs_axiom_grounding('71a6f040-3927-475d-ac3e-659280496d48', legitimacy_constituted_by_demonstrated_performance, empirically_contingent).
narrative_ontology:cs_axiom('71a6f040-3927-475d-ac3e-659280496d48', secondary, authority_must_be_earned_not_decreed).
narrative_ontology:cs_axiom_status(authority_must_be_earned_not_decreed, holdable).
narrative_ontology:cs_axiom_grounding('71a6f040-3927-475d-ac3e-659280496d48', authority_must_be_earned_not_decreed, deontological).
narrative_ontology:cs_reference_frame('71a6f040-3927-475d-ac3e-659280496d48', performance_legitimated_diffusion_order).
narrative_ontology:cs_drift_state('71a6f040-3927-475d-ac3e-659280496d48', contemporary_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71a6f040-3927-475d-ac3e-659280496d48', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_peripheries).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, validated_practice_communities).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, legitimacy_certifying_bodies).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_authority_holders).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, superseded_commitment_incumbents).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, adoption_lag_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Peripheral reformers - provincial administrators, dissenting clergy, marginal professionals - develop a commitment the center ignores and spend their own careers demonstrating that it works: pilot reforms run at their own risk, results published at their own expense, reputations staked on each comparison. A failed demonstration costs them standing; a successful one begins their climb. Leaving the commitment would mean abandoning the work their lives and self-conceptions are built on, so they persist through repeated refusals.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates, payer).

% Frontier jurisdictions, minor courts, and border polities adopt the new practice first because they carry little legacy investment in the old one. Adoption is cheap for them and yields visible administrative or commercial advantage while the center hesitates; their recorded results become the evidence later advocates cite. If a practice disappoints, they drop it quietly and little is lost.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_peripheries, beneficiary,
    organized, generational, arbitrage, regional).

% Practitioner networks - professional associations, reform congregations, trained administrative cadres - operationalize the commitment once it gains traction. Posts, fees, and deference flow to them as adoption spreads, and their internal standards come to define competent practice. Their position depends on the practice continuing to spread, so they invest in training and certification.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, validated_practice_communities, beneficiary,
    organized, generational, constrained, national).

% Learned societies, professional colleges, chancery offices, and review organs decide which demonstrations count as proof that a new commitment outperforms the old. They set the evidentiary bar, convene the comparisons, and issue the judgments that turn local results into portable legitimacy. Every climb passes through their gate; they collect standing, correspondence, and gatekeeping authority from that position without running the risks the demonstrators run.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, legitimacy_certifying_bodies, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, legitimacy_certifying_bodies, beneficiary).

% Court, crown, central ministry, established church - the center whose word currently settles what counts as legitimate. Each successful fringe demonstration narrows what they can refuse without argument. They respond with delay, co-optation of promising advocates, and counter-demonstrations of their own, but they cannot leave their position at the center, and when displacement comes it arrives without compensation for surrendered prerogatives.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_authority_holders, payer,
    institutional, generational, trapped, national).

% Holders of the commitment being displaced - guild masters, scholastic faculties, hereditary officeholders - whose training, status, and self-understanding are bound to the old arrangement. As the new practice climbs, their skills write down and their honors thin. Taking up the new commitment means starting again at the bottom in unfamiliar terms; most defend the old one instead.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, superseded_commitment_incumbents, payer,
    organized, generational, identity_locked, national).

% Subjects, litigants, tenants, and clients who live under the old arrangement while the superior one climbs - years spent under inconsistent jurisdiction, customary exaction, or unreformed practice that the new commitment would end. The gradual pace protects institutional stability, but the lag is borne by them, and they hold no seat in any venue where the pace is decided.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, adoption_lag_populations, payer,
    powerless, immediate, trapped, regional).

% Movements whose claim cannot be shown piecemeal - demands for wholesale constitutional redesign, abolition of entrenched categories, remedies for harms that mature over generations. Nothing short of the whole change would demonstrate their superiority, so the demonstration standard offers them no pilot form. They stand outside every certification venue, arguing that the channel itself is rigged against exactly the commitments that matter most.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, unproven_systemic_critics, excluded,
    powerless, generational, trapped, continental).

% Scholars reconstructing climb sequences across polities and centuries: coding adoption curves, identifying sponsors, reading certification records against outcomes. They watch the whole structure - where the channel opened, whom it filtered, what the certifiers rewarded - and can compare cases no participant ever saw together.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, legitimacy_certifying_bodies).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives polities and institutions a decentralized filter for choosing among rival commitments: performance demonstrated at the margins replaces armed struggle or pure fiat as the arbiter of which arrangement enters the authoritative core, letting change proceed while the center's consent remains withheld.
% TRANSFER_FUNCTION: Moves legitimacy - and the posts, deference, and jurisdiction it commands - from incumbent apex holders to peripheral coalitions that can demonstrate superiority; simultaneously moves proof-labor, ruin-risk, and delay costs onto the advocates who climb and onto the populations living under the superseded arrangement during the climb.
% ABSENT_VOICES: Commitments whose superiority cannot be shown piecemeal - systemic critiques and slow-harm claims - have no demonstration venue and therefore no seat; apex traditionalists who reject peripheral demonstration as a validity source at all appear only as obstruction. Both stand outside the certification circuit: the critics because the standard excludes them, the traditionalists because they refuse its terms.
% DISAPPEARANCE_RATIONALE: If the climb channel vanished overnight, normative change would route entirely through apex fiat or open rupture: peripheries would lose their only legitimacy path, adoption curves would collapse into step functions set by succession crises, and the certifying bodies' gatekeeping would lose its object. Every seat's strategy - advocacy, early adoption, certification, resistance - is organized around the channel.
% FOUNDING_PROBLEM: How can an institution replace a commitment its center is invested in, when the center controls appointment, purse, and doctrine - without schism, coup, or stagnation?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative-historical scholarship on institutional change and diffusion documents recurring climb sequences and apex resistance across unrelated polities; apex archives corroborate the problem indirectly, since centers recorded fringe innovations as threats requiring management - attesting that the renewal problem was live for the very actors who resisted the channel.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) because the channel's costs are real but partly intrinsic to its selection function: proof must be paid for by someone, and the climb deliberately prices advocacy in risk rather than in fiat. Suppression is lower (0.30) than in mandated-installation arrangements because the channel filters and delays rather than coerces - though it does suppress one class of alternatives outright: commitments that cannot demonstrate piecemeal. Theater (0.32) concentrates late in the cycle, when success breeds inevitability narratives and imitative demonstrations staged for certification rather than for use. Accessibility collapse is low (0.30): apex fiat, purchase, and rupture remain available as rival routes, so understanding the climb mechanism does not close the option space. Resistance is high (0.70) - apex counter-mobilization is this reading's signature. The measurement series share one grid (t=0..60 by tens) tracing a canonical climb cycle: extraction peaks mid-climb (t=30) when apex counter-mobilization, maximal proof burden, and lag harm coincide, then declines as installation routinizes; suppression_requirement tracks defense of the demonstration venues themselves, rising while the channel is contested and falling once the outcome settles; theater_ratio rises monotonically as victory converts advocacy into myth-making. Values are stylized cycle-level judgments, not measurements of one dated case.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit four different arrangements under the same structure. The apex seat experiences a threat machine: it pays displacement, cannot exit, and rationally reads every demonstration as siegecraft. The advocate seat experiences the only honest path available to actors without apex access - while also paying the proof-labor that makes the path expensive (hence the directionality override). The certifier seat experiences stewardship: it administers everyone else's costs and collects standing from the gate. The lag-population seat experiences pure delay: it receives none of the coordination return and all of the waiting. The engine computes these divergences from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (fringe_advocates, early_adopter_peripheries, validated_practice_communities, legitimacy_certifying_bodies) pull those seats toward the beneficiary end; victim declarations (apex_authority_holders, superseded_commitment_incumbents, adoption_lag_populations) push them toward the target end, amplified by trapped and identity_locked exits. One override is authored: the derivation would read fringe_advocates' beneficiary declaration plus identity_locked exit as a strongly subsidized seat (d near 0.2), understating the proof-labor and ruin-risk the climb charges them; the override sets d=0.38 for the moderate power atom - which in this story maps only to the advocates - reflecting their genuinely dual payer/beneficiary position. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Two temptations frame the classification problem. The reading's own endorsement tempts a pure-coordination verdict: everyone eventually benefits from better commitments, so the channel looks like frictionless selection. The apex's hostility tempts a pure-extraction verdict: the channel looks like a usurpation machine that strips centers without consent. Both erase half the structure - the first the uncompensated displacement and lag harm, the second the genuine selection function that lets change proceed without rupture. The hybrid classification holds both facts. On lifecycle: the founding problem (renewal against invested centers) is live wherever centers remain invested, so no resolved-mandatrophy flag is authored; the forward risk runs toward atrophy instead - if certifying bodies entrench, certification can become performance staged for the gate rather than demonstration aimed at use, a drift the theater_ratio series and the channel-recurrence omega are positioned to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the endogenous_climb_reading of kernel state_commitment_installation_mechanism; would instantiating the exogenous_imposition_reading or hybrid_cascade_reading instead change the structural classification?',
    'Author the sibling readings as separate stories and compare computed types: if the exogenous reading computes as pure extraction (coerced installation, identifiable victims) while this reading computes as hybrid, the kernel''s classification is reading-indexed and the disagreement is located in the legitimacy-source premise.',
    'Under the exogenous reading, fringe actors become targets rather than beneficiaries, apex actors become agenda-setters rather than payers, and the epsilon referent shifts to coerced installation arrangements; under the hybrid reading, both seat structures appear and the enforcement requirement compounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the state commitment installation kernel.').

omega_variable(
    superiority_standard_authorship,
    'Who authors the standard of demonstrated superiority - and does the standard measure superiority for the institution, for the certifiers, or for the populations the commitment governs?',
    'Trace certification decisions to their criteria: compare outcomes for governed populations under climbed versus refused commitments against the certifiers'' stated standards.',
    'If the standard tracks certifier or apex interests, the channel''s coordination function is thinner than authored and effective extraction rises across payer seats; if it tracks governed-population outcomes, the hybrid reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superiority_standard_authorship, conceptual, 'Whether the demonstration standard is neutral instrumentation or a gatekeeper interest.').

omega_variable(
    selection_vs_demonstrability_bias,
    'Does the climb select for genuinely superior commitments, or for commitments whose superiority is cheapest to demonstrate - favoring visible, quick-yielding changes over slow-manifesting ones?',
    'Compare the distribution of climbed versus refused commitments on long-horizon outcome measures, controlling for initial advocacy resources and sponsor visibility.',
    'Strong demonstrability bias would mean the channel systematically misprices slow-harm commitments, raising the costs borne by adoption_lag_populations beyond the authored level and hardening the excluded position of unproven_systemic_critics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_vs_demonstrability_bias, empirical, 'Whether the channel filters on merit or on demonstrability.').

omega_variable(
    channel_recurrence_after_installation,
    'Once a climbed commitment installs, does the channel stay open for the next climb, or does the new incumbent coalition close it - making each climb a ladder pulled up behind its climbers?',
    'Code successive climb intervals within single polities: measure inter-climb spacing and certification-bar movement after each installation.',
    'If the channel closes after installation, the mechanism is a transitional support consumed by its first success and mature polities should drift toward inertial theatrical certification; if it stays open, the hybrid classification is stable across cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(channel_recurrence_after_installation, empirical, 'Lifecycle question: whether the climb mechanism recurs or is consumed by its own winners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 60, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, resource_allocation).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'how do new commitments gain legitimacy?' covers three structurally distinct mechanisms modeled as one kernel family: endogenous climb (this file - fringe beneficiaries, apex payers, gradual curves), exogenous imposition (separate file - apex agenda-setter, coerced targets, steep suppression), and hybrid cascade (separate file - both seats, compounded enforcement). The epsilon values differ because the referent arrangements differ: a climb channel charges proof-labor and displacement; an imposition regime charges compliance under mandate; a cascade charges both in sequence. Each story carries its own stable epsilon; the family links record that the sibling claims are cited as rivals in the same historiographic disputes, not that the constraints share structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
