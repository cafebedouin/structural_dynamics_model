% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism (Decline Reading): the Dueling Norm's Weakening to Fringe Status
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism — the norm that an insult to a
 *   gentleman's honor must be answered by a duel under codified rules —
 *   operated for over a century as the dispute-resolution and status-ordering
 *   machinery of the European gentleman estate. This story instantiates the
 *   decline reading of that kernel: the mechanism persisted as a single
 *   continuous practice, weakening under rising enforcement (dueling
 *   statutes, army regulations, prosecutions) and rising social cost
 *   (bourgeois moral disapproval), until it survived only at the fringe —
 *   dueling fraternities, professional seconds, arranged first-blood affairs.
 *   The claim/metric split is deliberate: the constraint is CLAIMED as
 *   tangled_rope (a genuine coordination function for the estate fused with
 *   asymmetric burdens on its coerced juniors and casualties, held up by
 *   active enforcement), while the authored metrics describe the interval's
 *   end state — extraction and suppression decayed, theatricality risen,
 *   alternatives widely available, resistance triumphant. The engine measures
 *   the divergence and computes per-seat types; this file does not reconcile
 *   them. Per the kernel discipline, only this reading is authored here: the
 *   contest with the contraction and composite readings is routed to the
 *   omega variables, and the sibling constraint files are linked in
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   gentleman_class_honor_estate: agenda-setter and primary beneficiary
 *   (powerful/identity_locked) — administers the honor code; its status order
 *   collects the code's rents - coerced_junior_officers: primary target
 *   (powerless/trapped) — bears compulsory risk under threat of ruin -
 *   duel_casualties_and_families: primary target (powerless/trapped) — bears
 *   the mortal costs; no standing anywhere in the code - honor_refusers:
 *   secondary target (moderate/constrained) — bears the enforcement penalty
 *   for exit - anti_dueling_societies: excluded external opposition
 *   (organized/mobile) — the coalition that priced the practice out -
 *   state_legal_authorities: excluded external enforcer
 *   (institutional/mobile) — the 'enforcement' half of this reading's delta -
 *   fringe_dueling_masters: residual beneficiary (moderate/mobile) —
 *   late-period tradesmen who keep the mechanism alive - historians_of_honor:
 *   analytical observer (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.3).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.35).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism (Decline Reading): the Dueling Norm's Weakening to Fringe Status").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical sociology/legal history/normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '588b2ff6-1f40-4880-89cc-d1caf3b99b55').
narrative_ontology:cs_kernel_codification('588b2ff6-1f40-4880-89cc-d1caf3b99b55', formalized).
narrative_ontology:cs_authority_grounding('588b2ff6-1f40-4880-89cc-d1caf3b99b55', practice).
narrative_ontology:cs_interpretation_layer_present('588b2ff6-1f40-4880-89cc-d1caf3b99b55').
narrative_ontology:cs_reading_relation('588b2ff6-1f40-4880-89cc-d1caf3b99b55', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('588b2ff6-1f40-4880-89cc-d1caf3b99b55', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('588b2ff6-1f40-4880-89cc-d1caf3b99b55', foundational, honor_mechanism_remained_live_option).
narrative_ontology:cs_axiom_status(honor_mechanism_remained_live_option, holdable).
narrative_ontology:cs_axiom_grounding('588b2ff6-1f40-4880-89cc-d1caf3b99b55', honor_mechanism_remained_live_option, empirically_contingent).
narrative_ontology:cs_axiom('588b2ff6-1f40-4880-89cc-d1caf3b99b55', foundational, decline_operated_through_cost_escalation).
narrative_ontology:cs_axiom_status(decline_operated_through_cost_escalation, holdable).
narrative_ontology:cs_axiom_grounding('588b2ff6-1f40-4880-89cc-d1caf3b99b55', decline_operated_through_cost_escalation, empirically_contingent).
narrative_ontology:cs_reference_frame('588b2ff6-1f40-4880-89cc-d1caf3b99b55', operative_code_duello_regime).
narrative_ontology:cs_drift_state('588b2ff6-1f40-4880-89cc-d1caf3b99b55', pre_great_war_fringe_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('588b2ff6-1f40-4880-89cc-d1caf3b99b55', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentleman_class_honor_estate).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, fringe_dueling_masters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, coerced_junior_officers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duel_casualties_and_families).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, honor_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, fringe_dueling_masters).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, code_duello_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, honor_status_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The hereditary officer and gentry estate that administered the honor code: its courts of honor defined what counted as an insult and what satisfaction required, its seconds staged the meetings, and its regimental and club opinion applied the penalties. The code maintained the estate's internal hierarchy of courage and marked its boundary against tradesmen and commoners, who were excluded from satisfaction altogether. As statutes, army regulations, and bourgeois disapproval raised the price of the practice, administering it grew costlier and its younger members treated it as increasingly optional; but abandoning the code would have meant repudiating the estate's own standing order, a cost no practical arrangement could offset.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentleman_class_honor_estate, agenda_setter,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, gentleman_class_honor_estate, beneficiary).

% Junior officers and young gentlemen of modest means who faced a challenge, or the expectation that they would issue one. Commanding officers and peers applied the pressure directly: refusal meant resignation from the regiment, loss of livelihood, and exclusion from the society that furnished marriage and advancement. Acceptance meant standing on the dueling ground. Leaving the regiment meant losing career and community together, so most paid the price the code set.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, coerced_junior_officers, payer,
    powerless, biographical, trapped, national).

% Men killed or disabled on the ground and the widows, orphans, and dependent parents left behind. They bore the practice's mortal costs with no standing in any court of honor; pensions were informal, compensation rare, and the code's records treated their losses as the ordinary expense of satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_casualties_and_families, payer,
    powerless, immediate, trapped, local).

% Men who declined a challenge or refused to issue one on religious, moral, or prudential grounds. The code's penalty fell on them directly: broken engagements, resigned commissions, cold shoulders in regimental and club society, and in some armies formal cashiering. Some left the gentlemanly world entirely for bourgeois professions where the code did not reach; that exit path widened as the interval progressed and bourgeois society grew more self-sufficient.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_refusers, payer,
    moderate, biographical, constrained, national).

% Religious bodies, humanitarian leagues, and legal reformers who campaigned against the practice: pamphlet wars, test prosecutions, lobbying for statutes and army regulations, public shaming of prominent duelists. They held no seat in any court of honor and their objections were answered with contempt inside the code's world; their leverage came entirely from outside it, through the press, the churches, and the legislature.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, anti_dueling_societies, excluded,
    organized, generational, mobile, national).

% Sovereigns, legislatures, and war offices that banned the duel by statute and military regulation and pursued duelists under homicide and dueling laws, cashiered officers, and arrested seconds. Enforcement was selective and class-skewed for most of the interval — juries reluctant to convict gentlemen — but the accumulating legal risk was a standing tax on the practice. The authorities stood entirely outside the honor code; its adherents treated their writ as one more cost to manage rather than a command.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authorities, excluded,
    institutional, generational, mobile, national).

% Fencing masters, professional seconds, and habitual duelists of the late period who kept the practice alive after its social base eroded: the university dueling fraternities with their goggled bouts, the French dueling scene where affairs were arranged to end at first blood. They drew livelihood and notoriety from the residual demand and absorbed the wounds themselves. For them the code was a trade rather than an inheritance, and when demand fell they converted to pure fencing instruction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, fringe_dueling_masters, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, fringe_dueling_masters, payer).

% Scholars reconstructing the practice's course from court records, regimental archives, pamphlet wars, and casualty registers. They see the whole structure at once — the dispute-resolution function the code performed for the estate, the burdens it placed on juniors, refusers, and casualties, and the accumulation of legal and moral cost that thinned the practice to a fringe.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, historians_of_honor, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, gentleman_class_honor_estate).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized the resolution of honor disputes within the gentleman estate: a shared procedure (challenge, seconds, codified terms) that substituted for private feud, street violence, and dishonoring lawsuit, while marking the estate's boundary — willingness to give satisfaction distinguished the gentleman from those who sued or brawled.
% TRANSFER_FUNCTION: Moved risk and deference from junior members of the estate to its status order: young gentlemen transferred exposure to death, maiming, and reputational subordination upward in exchange for standing inside the honor community; refusers transferred careers and social standing to the enforcement machinery itself.
% ABSENT_VOICES: The dead and their dependents had no seat in any court of honor; widows and mothers bore the costs with no standing; servants and tradesmen insulted by gentlemen had no satisfaction mechanism at all — the code was explicitly class-bounded, so its coordination never covered those below the estate. Anti-dueling moralists were heard only as external noise to be contemptuously declined.
% DISAPPEARANCE_RATIONALE: At the interval's start the estate's entire dispute order depended on the mechanism — removing it overnight would have forced gentlemen back onto feud, brawl, or dishonoring lawsuit, and the officer corps would have lost a core discipline instrument. By the interval's end the dependency had shrunk to the fringe: dueling fraternities would lose a core ritual, the fencing and seconding trades would lose their market, and a few regimental codes would lose an instrument — a real but small rearrangement. The shrinking of that rearrangement footprint across the interval is the decline reading's content.
% FOUNDING_PROBLEM: Elite dispute resolution in an era before reliable state courts for gentlemen: when a gentleman was insulted, the available alternatives were grinding private feud, street violence, or a lawsuit that itself dishonored him. The code duello was built as a bounded, consented, rule-governed procedure that contained the violence and preserved both parties' standing.
% FOUNDING_PROBLEM_CORROBORATION: State court and police archives, church condemnations, and anti-dueling society records — all sources outside the benefiting estate — attest that elite dispute resolution had passed to public law and bourgeois civil society by the interval's end; regimental and university archives show the residual practice persisting as ritual rather than as dispute resolution. No source outside the estate's remnants attests the founding problem as still live.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state metrics describe the fringe era: extractiveness 0.30 and suppression 0.35 — the code's coercive machinery (courts of honor, regimental discipline, ostracism) decayed as bourgeois exit widened and state prohibition accumulated; theater_ratio 0.55 — by the interval's close a majority of residual activity was ritual maintenance (goggled Mensur bouts, arranged first-blood affairs) rather than grievance-driven satisfaction; accessibility_collapse 0.25 — the mechanism never collapsed alternatives, and by the end courts, press, and public apology were fully available substitutes; resistance 0.70 — the anti-dueling coalition (churches, reformers, legislatures) effectively won. The measurement series run on one shared grid (t=0..130, one unit approximating one year, 1777–1907) with all three metrics authored at all seven points; trajectories are monotonic — no cycle — with base_extractiveness falling 0.72→0.30 (this reading's delta: epsilon drops via enforcement and social cost), suppression_requirement falling 0.78→0.35 (enforcement decay of the code's own machinery), and theater_ratio rising 0.15→0.55 (ritualization of the residue). The claimed type is tangled_rope, stated independently of the metrics: the mechanism genuinely coordinated the estate's honor disputes (a real collective-action solution substituting for feud, brawl, and dishonoring lawsuit) while asymmetrically burdening its coerced juniors, refusers, and casualties, and it required active enforcement throughout — all three structural facts the type demands. The rising theater_ratio is authored honestly as a drift signal, not reconciled into the claim; whether the residue is function or performance is carried by the late_period_functionality_ambiguity omega. Coupling note: the mechanism's identity-coordination function concentrated its burdens on powerless juniors at national scope while its benefits accrued to a powerful estate — the Power × Scope signature the identity-coordination floor exists to flag, authored here rather than smoothed away. Suppression was both structural (career destruction, ostracism, regimental discipline) and internalized (honor identity); the split is carried by its omega, and the late-interval widening of bourgeois exit is the natural experiment that would resolve it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. The estate seat experiences the code as an order it built and administers: its members' standing, its boundary against commoners, and its internal hierarchy all ride on the mechanism — from that seat the arrangement is coordination. The coerced junior officers and the casualties experience the same rules as a price levied on them: death risk and compulsory participation with no exit and no seat in the adjudication — from those seats the arrangement is burden. The refuser seat splits the difference: it bears the enforcement penalty precisely by refusing the burden, so it experiences the code as coercion applied to conscience. The fringe masters' seat is a trade: gains and wounds both flow to them, and they experience the code as a market. The same estate seat also computes differently across time — at t0 the code is backed by unanimous peer enforcement and cheap to run; at tn it is propped against statute and ridicule — so per-seat classification should drift with the interval, which the shared measurement grid registers. Same-level divergence: coerced juniors and refusers hold comparable nominal standing, but independent income, religious community, and bourgeois career paths gave refusers an exit the juniors lacked — exit options, not power, differentiate their seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: gentleman_class_honor_estate (administers the code and collects its status rents — deference, boundary, hierarchy) and fringe_dueling_masters (late-period livelihood and notoriety). Victim declarations: coerced_junior_officers (compulsory risk, no exit), duel_casualties_and_families (mortal costs, no standing), honor_refusers (enforcement penalty for exit). The derivation should place the estate near the beneficiary end — its identity_locked exit reflects fusion of the estate's self-concept with the code, which damps rather than raises its burden since exit was never contemplated — and the three victim groups near the target end, with the trapped juniors nearest the full-target pole and the refusers somewhat back (their constrained exit meant they could leave by paying). The fringe masters sit mid-range: declared beneficiaries who also absorbed the wounds through their secondary payer role. The state authorities and the anti-dueling societies sit outside the benefit/burden relation entirely — excluded seats whose pressure is this reading's 'enforcement and social cost' delta, not participants in the extraction relation. vindicated_propositions (code_duello_doctrine, honor_status_order_doctrine) collect nothing and are listed as propositions, not beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings are live here and the classification guards both. First, the degraded-residue temptation: the rising theater_ratio and the dead founding problem invite reading the end state as pure theatrical residue. But concentrated residual beneficiaries exist (the dueling trades), the residue still drew real blood, and the practice persisted by choice rather than pure inertia — the honest course is to author the theater rise and let the engine's cost-asymmetry test decide, which is what the late_period_functionality_ambiguity omega feeds. Second, the pure-extraction temptation: because the code killed and coerced, it is tempting to flatten it to a snare; but the coordination function was genuine while the mechanism operated — it really did solve the estate's dispute problem, and the burden rode on that solution, which is the tangled-rope structure, not a snare's. The R5 record is authored without smoothing: the founding problem is dead (state courts absorbed elite dispute resolution) while fringe arrangements still depend on the mechanism — the mismatch flag should fire, and it is the honest signal of a mandate outliving its function, not an anomaly to argue away. The genealogy also explains the decline's shape: the mechanism did not fall to a single blow but to a century of accumulating cost, which is why the epsilon trajectory is a slope and not a cliff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_contraction,
    'This constraint is the decline_reading of the kernel honor_satisfaction_mechanism: the mechanism persisted at declining frequency as a live, deliberated option until fringe status. The sibling contraction_reading instead holds the mechanism became cognitively unthinkable — a category-level impossibility with residual performances. Which account describes the end state: an available-but-costly choice (this reading) or an unthinkable category with category-error remnants (contraction)?',
    'Agent-level deliberation evidence from the late period — diaries, correspondence, courts-of-honor minutes, court testimony of refusers and duelists: if late-period agents weigh, defer, and decline the duel as a live option, this reading holds; if the duel no longer appears as a thinkable move at all, the contraction reading holds.',
    'If the contraction reading is right, this story''s end-state epsilon and availability claim are overstated — the residual practice is inertial performance and the constraint is degraded at the close; if this reading is right, the mechanism remained a genuine hybrid coordination/burden structure through the fringe and the decline is cost-driven weakening of a live arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_contraction, empirical, 'Decline-vs-contraction: whether the mechanism remained a live deliberated option (this reading) or became cognitively unthinkable (sibling reading).').

omega_variable(
    reading_contestation_composite,
    'This reading attributes the weakening to enforcement and social cost acting on one continuous mechanism. The sibling composite_reading attributes the same record to plural distinct mechanisms — state monopoly on dispute resolution, bourgeois norms, casualty insurance, category shift — displacing the duel. Is the observed epsilon decline this constraint''s own weakening, or the net effect of successor constraints absorbing its functions?',
    'Decomposition of the decline record: separate the fall attributable to the duel mechanism''s own enforcement and social-cost escalation (prosecutions, ostracism risk, anti-dueling pressure) from the fall attributable to successor mechanisms taking over its functions (courts hearing insult cases, the press handling reputation, widows'' funds absorbing casualty risk).',
    'If the composite reading is right, this story''s epsilon trajectory conflates one mechanism''s weakening with its displacement, and successor-constraint stories should carry the displaced share; this reading then survives only for the residual core that declined on its own cost structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_composite, conceptual, 'Decline-vs-composite: a single mechanism weakening versus plural successor mechanisms displacing it.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the mechanism''s suppression structural (career destruction, ostracism, regimental discipline applied from outside) or internalized (the gentleman''s honor identity making refusal unthinkable from inside)?',
    'Refuser trajectories across the interval: men who refused and retained standing versus men who refused and were ruined, and the late-interval widening of exit into bourgeois society — if reported compulsion fell as exit widened, suppression was structural; if refusers with safe exits still reported compulsion, it was internalized.',
    'If internalized, suppression outlives external enforcement and the end-state suppression is understated — this reading''s cost-driven account undercounts the identity mechanism, and the target carries the compulsion past every removed barrier; if structural, widening exit fully explains the decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the honor compulsion.').

omega_variable(
    late_period_functionality_ambiguity,
    'At the interval''s end, was residual dueling functionally operative — real satisfaction under real risk — or theatrical performance: arranged first-blood affairs, goggled Mensur bouts, choreographed exchanges maintained for the sake of the ritual itself?',
    'Per-episode casualty and arrangement records by period and country: seconds'' correspondence, medical reports, and press accounts distinguishing affairs with genuine risk and genuine grievance from choreographed ones.',
    'If theatrical, the end state is inertial performance and the authored theater_ratio is understated; if functional, the tangled_rope claim holds through the fringe and the theater_ratio overstates the decay of the mechanism''s real function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_period_functionality_ambiguity, empirical, 'Whether late-period dueling remained functional or became theatrical maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_decline_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(honor_decline_tr_t0, observed).
narrative_ontology:measurement(honor_decline_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(honor_decline_tr_t20, observed).
narrative_ontology:measurement(honor_decline_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(honor_decline_tr_t40, observed).
narrative_ontology:measurement(honor_decline_tr_t60, honor_satisfaction_mechanism__decline_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(honor_decline_tr_t60, observed).
narrative_ontology:measurement(honor_decline_tr_t80, honor_satisfaction_mechanism__decline_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement_basis(honor_decline_tr_t80, observed).
narrative_ontology:measurement(honor_decline_tr_t100, honor_satisfaction_mechanism__decline_reading, theater_ratio, 100, 0.44).
narrative_ontology:measurement_basis(honor_decline_tr_t100, observed).
narrative_ontology:measurement(honor_decline_tr_t130, honor_satisfaction_mechanism__decline_reading, theater_ratio, 130, 0.55).
narrative_ontology:measurement_basis(honor_decline_tr_t130, observed).

% Extraction over time
narrative_ontology:measurement(honor_decline_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(honor_decline_be_t0, observed).
narrative_ontology:measurement(honor_decline_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(honor_decline_be_t20, observed).
narrative_ontology:measurement(honor_decline_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(honor_decline_be_t40, observed).
narrative_ontology:measurement(honor_decline_be_t60, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(honor_decline_be_t60, observed).
narrative_ontology:measurement(honor_decline_be_t80, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement_basis(honor_decline_be_t80, observed).
narrative_ontology:measurement(honor_decline_be_t100, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(honor_decline_be_t100, observed).
narrative_ontology:measurement(honor_decline_be_t130, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 130, 0.3).
narrative_ontology:measurement_basis(honor_decline_be_t130, observed).

% Suppression requirement over time
narrative_ontology:measurement(honor_decline_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(honor_decline_su_t0, observed).
narrative_ontology:measurement(honor_decline_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(honor_decline_su_t20, observed).
narrative_ontology:measurement(honor_decline_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(honor_decline_su_t40, observed).
narrative_ontology:measurement(honor_decline_su_t60, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(honor_decline_su_t60, observed).
narrative_ontology:measurement(honor_decline_su_t80, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement_basis(honor_decline_su_t80, observed).
narrative_ontology:measurement(honor_decline_su_t100, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 100, 0.43).
narrative_ontology:measurement_basis(honor_decline_su_t100, observed).
narrative_ontology:measurement(honor_decline_su_t130, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 130, 0.35).
narrative_ontology:measurement_basis(honor_decline_su_t130, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' decomposes into three structurally distinct constraints over one kernel (honor_satisfaction_mechanism): this decline_reading (continuous practice, cost-driven weakening, fringe persistence), contraction_reading (category-level collapse), and composite_reading (displacement by plural successor mechanisms). Each reading carries its own epsilon, beneficiary/victim structure, and claimed type; this file authors only the decline reading. Family links run through network.affects_constraints in all three files. Neither sibling is foreclosed by this reading's premises — a layered account (decline in frequency, then mainstream category collapse; weakening plus displacement) is coherent, so the relations are coexistence, not foreclosure. The decline reading's empirical record (prosecution counts, casualty registers, frequency series) is the shared evidentiary substrate the other two readings reinterpret, which is why this file links to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
