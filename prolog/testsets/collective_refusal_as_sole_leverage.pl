% ============================================================================
% CONSTRAINT STORY: collective_refusal_as_sole_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_refusal_as_sole_leverage, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collective_refusal_as_sole_leverage
 *   human_readable: Collective Refusal as the Sole Leverage Against Unilateral Standing-Seizure
 *   domain: political_economy/institutional_authority
 *
 * SUMMARY:
 *   An authority structure — administrative, professional, or institutional —
 *   holds unilateral power to seize or suspend any single actor's standing.
 *   That power is structurally immune to appeal, evidence, or grievance
 *   offered by the aggrieved actor alone: no individual channel exists that
 *   binds the authority to reconsider. The authority's own legitimating
 *   ritual (the recitation), however, contains a built-in procedural
 *   requirement — a quorum of independent peer participation — that, when
 *   collectively withheld, is the one mechanism the authority must respond
 *   to. This constraint sits downstream of two related constraints:
 *   voice_without_leverage (which establishes that individual appeal carries
 *   no binding force) and disciplined_self_distrust_as_inherited_suppression
 *   (which explains why individual peers often fail to organize even when
 *   coordination would serve their collective interest — internalized doubt
 *   about their own standing to object). Together they explain why the quorum
 *   mechanism, though genuinely a coordination device, functions in practice
 *   as a high-bar gate that concentrates real leverage in organized peer
 *   collectives while leaving isolated petitioners with no functioning
 *   remedy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_refusal_as_sole_leverage, 0.68).
domain_priors:suppression_score(collective_refusal_as_sole_leverage, 0.79).
domain_priors:theater_ratio(collective_refusal_as_sole_leverage, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_refusal_as_sole_leverage, extractiveness, 0.68).
narrative_ontology:constraint_metric(collective_refusal_as_sole_leverage, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(collective_refusal_as_sole_leverage, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collective_refusal_as_sole_leverage, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(collective_refusal_as_sole_leverage, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_refusal_as_sole_leverage, tangled_rope).
narrative_ontology:human_readable(collective_refusal_as_sole_leverage, "Collective Refusal as the Sole Leverage Against Unilateral Standing-Seizure").
narrative_ontology:topic_domain(collective_refusal_as_sole_leverage, "political_economy/institutional_authority").

domain_priors:requires_active_enforcement(collective_refusal_as_sole_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_refusal_as_sole_leverage, peer_collective_when_unified).
narrative_ontology:constraint_beneficiary(collective_refusal_as_sole_leverage, authority_administrators).
narrative_ontology:constraint_victim(collective_refusal_as_sole_leverage, isolated_individual_petitioner).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(collective_refusal_as_sole_leverage, individual_non_participating_peer).
narrative_ontology:constraint_victim(collective_refusal_as_sole_leverage, coordinating_organizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the recitation — the standing-legitimating ritual — and can seize or suspend any single actor's status unilaterally. Treats individual appeal, evidence, and grievance as non-binding inputs; only alters position when the ritual's built-in participation quorum among peers is collectively withheld. Benefits from the arrangement because it means no single petitioner can force change, keeping the administrative burden of responsiveness low and authority concentrated.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, authority_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Has standing seized or suspended by unilateral authority action. Can present appeal, evidence, or grievance, none of which the authority is structurally required to weigh. Has no path to compel reconsideration alone — the only lever that exists (collective withholding of ritual participation) is not theirs to pull individually. Exit means abandoning the standing altogether; there is no independent channel to contest the seizure.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, isolated_individual_petitioner, payer,
    powerless, biographical, trapped, local).

% Individually holds no more formal power than the isolated petitioner, but the recitation ritual requires multiple peers' participation to proceed. When peers act in coordination and withhold that participation, they gain leverage the authority must respond to — the only structural veto point in the system. This makes the collective, when unified, the sole effective beneficiary of the system's one compellable mechanism, even though no single peer benefits alone.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, peer_collective_when_unified, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(collective_refusal_as_sole_leverage, peer_collective_when_unified, agenda_setter).

% A peer who declines to withhold participation, whether from fear of individual retaliation, disbelief in coordination succeeding, or simple non-organization. Their individual defection from the withholding effort is invisible in isolation but collectively determines whether the quorum threshold is met. Bears the same structural powerlessness as the petitioner but is rarely named as a party to the dispute — treated as background noise in the ritual's headcount rather than as an agent with a stake.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, individual_non_participating_peer, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(collective_refusal_as_sole_leverage, individual_non_participating_peer, payer).

% Those among the peers who do the work of assembling the collective withholding — communicating, monitoring compliance, absorbing retaliation risk disproportionately. They administer the only mechanism that can move the authority, but bear personal exposure (professional, reputational, sometimes legal) that individual non-organizing peers do not, in service of a collective good they cannot capture alone.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, coordinating_organizers, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(collective_refusal_as_sole_leverage, coordinating_organizers, payer).

% Reviews whether the authority's seizure/suspension power and its ritual quorum requirement constitute a fair process or a structurally rigged one. Can commission studies of how often isolated appeals succeed versus collective withholdings, and could in principle mandate an individual-grievance channel — but has not yet done so.
narrative_ontology:constraint_stakeholder(collective_refusal_as_sole_leverage, institutional_oversight_body, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(collective_refusal_as_sole_leverage, authority_administrators).
narrative_ontology:fixing_cost_class(collective_refusal_as_sole_leverage, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The recitation's participation quorum genuinely solves a coordination problem: it prevents the authority from being whipsawed by single disgruntled actors while still requiring it to answer to a demonstrated, non-trivial consensus among peers before altering settled standing determinations.
% TRANSFER_FUNCTION: Moves the practical capacity to compel institutional reconsideration away from any single aggrieved individual and concentrates it in whichever peer group can achieve coordinated withholding — a transfer of effective voice from the individually powerless to the collectively organized, and correspondingly a transfer of unaccountability toward the authority when no such coordination materializes.
% ABSENT_VOICES: The isolated petitioner is present but structurally unheard — their appeal is procedurally routed to nowhere. Individual non-participating peers are almost never named as parties to the dispute at all; their choices are aggregated into a headcount without their perspective on why they didn't organize being solicited.
% DISAPPEARANCE_RATIONALE: If the quorum-veto mechanism vanished, either the authority would become fully unaccountable to anyone (if no substitute check emerged) or individual grievance channels would have to be built from scratch to replace collective withholding as the sole lever — either way the relationship between petitioners, peers, and the authority reorganizes substantially.
% FOUNDING_PROBLEM: The recitation ritual's participation quorum was built to prevent the authority from either (a) being captured or destabilized by any single self-interested actor's complaint, or (b) becoming a rubber stamp with zero external check at all — collective withholding was designed as the minimum viable brake.
% FOUNDING_PROBLEM_CORROBORATION: Authority administrators attest the quorum requirement still serves its original anti-capture function. Coordinating organizers and institutional oversight body analysis suggest the mechanism has drifted: it now functions less as a check on authority overreach and more as a high-cost, high-friction gate that almost never gets triggered, leaving isolated petitioners with no functioning remedy in the interim — a reading corroborated by independent case-outcome studies showing individual appeals succeed at a negligible rate compared to instances of organized withholding.
narrative_ontology:disappearance_verdict(collective_refusal_as_sole_leverage, world_rearranges).
narrative_ontology:founding_problem_status(collective_refusal_as_sole_leverage, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(collective_refusal_as_sole_leverage, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(collective_refusal_as_sole_leverage, 'none', 1).
narrative_ontology:epsilon_provenance(collective_refusal_as_sole_leverage, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_refusal_as_sole_leverage_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_refusal_as_sole_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collective_refusal_as_sole_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at a substantial-but-not-extreme 0.68 because the mechanism does periodically work — collective withholding does compel the authority when it materializes — but the overwhelming majority of individual grievances never reach that threshold, so the practical extraction from isolated petitioners is high while the theoretical coordination function remains real. Suppression is high (0.79) because the authority's structural indifference to individual appeal is itself an active design feature, not an incidental gap — the isolated petitioner is suppressed by the very existence of a channel that only responds to a form of leverage they cannot individually wield. Theater ratio is moderate (0.42) and rising: a growing share of the recitation's ceremonial trappings (public grievance windows, appeal boards) perform responsiveness without providing it, since the real veto lies entirely in the quorum mechanism the ceremony obscures.
 *
 * PERSPECTIVAL GAP:
 *   From the authority's seat, the arrangement looks like a stable, legitimate check-and-balance: peer quorum requirements prevent capture by lone bad actors, and periodic collective withholding demonstrates the mechanism functions when truly warranted. From the isolated petitioner's seat, the same structure is a wall: their appeal, however well-founded, has no channel. From the individual non-organizing peer's seat, the mechanism is invisible — they may not even register that their non-participation in a collective effort is itself a structurally significant act. The engine should compute these seats to genuinely diverge: agenda_setter/beneficiary seats trend toward coordination-reading, payer/trapped seats trend toward extraction-reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The isolated individual petitioner is the clearest victim: trapped exit, powerless atom, and a structural position where no available channel binds the authority. The peer collective, when unified, is the beneficiary — not because any single peer profits, but because coordinated withholding is the only leverage that functions, and it accrues to the organized group as a group. Authority administrators are declared as a second beneficiary because low individual-grievance responsiveness reduces their administrative burden and consolidates their discretion; this reflects genuine asymmetric extraction alongside genuine coordination function, which is exactly the tangled_rope signature — the recitation quorum solves an actual anti-capture coordination problem (a single disgruntled actor cannot destabilize legitimate authority action) AND enables asymmetric extraction from anyone who cannot muster peer coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing capture by lone bad-faith actors while still requiring the authority answer to genuine peer consensus) may still be partly live — authority structures probably do need protection from single-actor destabilization. But the corroborated case-outcome data suggesting individual appeals succeed at negligible rates versus organized withholding indicates the mechanism has drifted from balanced check toward a near-total gate against individual accountability. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (quorum-based anti-capture) while still registering the asymmetric extraction from isolated petitioners — a pure snare classification would erase the real anti-capture logic the recitation was built on; a pure rope classification would erase the victim class entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quorum_threshold_calibration,
    'Is the specific number of peers required to withhold participation calibrated to a genuine anti-capture function, or has it been set (or drifted) to a level that is practically unreachable for most aggrieved groups?',
    'Historical analysis of successful versus attempted collective withholdings: track the actual peer counts achieved in failed attempts against the formal quorum requirement, and compare to the requirement''s stated original rationale.',
    'If the threshold is set near the practical ceiling of achievable coordination, the coordination function is largely nominal and the classification should weight further toward snare; if it sits well below what determined peer groups can achieve, the tangled_rope reading (genuine but unevenly accessible coordination) is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quorum_threshold_calibration, empirical, 'Whether the quorum number is a genuine anti-capture calibration or a de facto extraction-preserving barrier.').

omega_variable(
    organizer_exposure_asymmetry,
    'Do coordinating organizers bear retaliation risk disproportionate to individual non-organizing peers, and does this asymmetry suppress the frequency of successful collective withholding below what peer sentiment alone would predict?',
    'Compare career/reputational outcomes for identified organizers of successful and unsuccessful withholding efforts against outcomes for non-participating peers in the same cohort.',
    'If organizer exposure is high and disproportionate, the collective mechanism''s rarity is partly an artifact of suppressed individual willingness to organize rather than genuine peer disagreement — strengthening the extraction reading and linking directly to the upstream disciplined_self_distrust_as_inherited_suppression constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizer_exposure_asymmetry, empirical, 'Whether organizer-specific retaliation risk suppresses collective action frequency below latent peer support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_refusal_as_sole_leverage, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, collective_refusal_as_sole_leverage, theater_ratio, 0, 0.25).
narrative_ontology:measurement(coll_tr_t4, collective_refusal_as_sole_leverage, theater_ratio, 4, 0.29).
narrative_ontology:measurement(coll_tr_t8, collective_refusal_as_sole_leverage, theater_ratio, 8, 0.33).
narrative_ontology:measurement(coll_tr_t12, collective_refusal_as_sole_leverage, theater_ratio, 12, 0.36).
narrative_ontology:measurement(coll_tr_t16, collective_refusal_as_sole_leverage, theater_ratio, 16, 0.39).
narrative_ontology:measurement(coll_tr_t20, collective_refusal_as_sole_leverage, theater_ratio, 20, 0.41).
narrative_ontology:measurement(coll_tr_t24, collective_refusal_as_sole_leverage, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, collective_refusal_as_sole_leverage, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(coll_be_t4, collective_refusal_as_sole_leverage, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(coll_be_t8, collective_refusal_as_sole_leverage, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(coll_be_t12, collective_refusal_as_sole_leverage, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(coll_be_t16, collective_refusal_as_sole_leverage, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(coll_be_t20, collective_refusal_as_sole_leverage, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(coll_be_t24, collective_refusal_as_sole_leverage, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coll_su_t0, collective_refusal_as_sole_leverage, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(coll_su_t4, collective_refusal_as_sole_leverage, suppression_requirement, 4, 0.67).
narrative_ontology:measurement(coll_su_t8, collective_refusal_as_sole_leverage, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(coll_su_t12, collective_refusal_as_sole_leverage, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(coll_su_t16, collective_refusal_as_sole_leverage, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(coll_su_t20, collective_refusal_as_sole_leverage, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(coll_su_t24, collective_refusal_as_sole_leverage, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_refusal_as_sole_leverage, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(collective_refusal_as_sole_leverage, 0.12).
narrative_ontology:affects_constraint(collective_refusal_as_sole_leverage, voice_without_leverage).
narrative_ontology:affects_constraint(collective_refusal_as_sole_leverage, disciplined_self_distrust_as_inherited_suppression).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two upstream constraints in the same family. voice_without_leverage establishes that individual appeal to the authority carries no binding force — the structural precondition that makes collective withholding the ONLY leverage available. disciplined_self_distrust_as_inherited_suppression explains why individual peers, even when their collective coordination would serve shared interest, frequently fail to organize — internalized doubt about their standing to object suppresses the peer-collective mechanism's activation frequency below what bare structural opportunity would predict. Together the three form a causal chain: individual appeal is powerless (upstream) -> internalized suppression reduces the odds any given peer will organize (upstream) -> the resulting rarity of successful collective withholding is what this constraint measures as the sole compellable leverage point, and its asymmetric extraction from isolated petitioners.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
