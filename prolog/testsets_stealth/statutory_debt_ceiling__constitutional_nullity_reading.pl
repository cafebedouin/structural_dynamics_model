% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling — Constitutional Nullity Reading
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the constitutional_nullity_reading of the
 *   statutory_debt_ceiling kernel: on this reading, Section 4 of the
 *   Fourteenth Amendment ('the validity of the public debt... shall not be
 *   questioned') renders statutory limits on borrowing for already-enacted
 *   obligations void, Treasury issues debt as appropriations require, and the
 *   ceiling's periodic votes are ceremony with no legal force. The eps
 *   referent is the standing arrangement under contest — the statutory
 *   ceiling as actually administered — assessed by this reading's own lights;
 *   the near-zero eps is a property of this reading, not of the colloquial
 *   label. KEY AGENTS (by structural relationship): united_states_congress:
 *   agenda setter (institutional/mobile) — enacts and ritually adjusts a
 *   ceiling it could repeal at any time; us_treasury: payer and co-executor
 *   (institutional/constrained) — bears the pretense's administrative burden
 *   while its borrowing practice renders the ceiling inoperative;
 *   recorded_vote_seeking_legislators: beneficiary (moderate/mobile) —
 *   harvests symbolic value from the recurring ceremony;
 *   brinkmanship_coverage_media: beneficiary (organized/arbitrage) —
 *   monetizes the recurring drama; bond_market_participants: payer
 *   (powerful/arbitrage) — prices episodic tail risk the void ceiling never
 *   legitimately posed; appropriation_funded_obligees: payer
 *   (powerless/trapped) — depend on timely federal payments the ceremony
 *   episodically unsettles; constitutional_nullity_advocates: observer
 *   (moderate/analytical) — scholarship and executive deliberation advancing
 *   the Section 4 supersession claim. The claim/metrics split is deliberate:
 *   the ceiling is CLAIMED as piton (an inertial shell whose remaining
 *   substance is performance) while the metrics independently describe
 *   near-zero extraction, residual suppression, and very high theater; the
 *   engine computes per-seat classifications from the structural data, and
 *   divergence between claim and computed type is measurement, not error.
 *
 * KEY AGENTS:
 *   - united_states_congress: agenda setter (institutional/mobile) — enacts and ritually adjusts a ceiling it could repeal at any time
 *   - us_treasury: payer and co-executor (institutional/constrained) — bears the pretense's administrative burden while its borrowing practice renders the ceiling inoperative
 *   - recorded_vote_seeking_legislators: beneficiary (moderate/mobile) — harvests symbolic value from the recurring ceremony
 *   - brinkmanship_coverage_media: beneficiary (organized/arbitrage) — monetizes the recurring drama
 *   - bond_market_participants: payer (powerful/arbitrage) — prices episodic tail risk the void ceiling never legitimately posed
 *   - appropriation_funded_obligees: payer (powerless/trapped) — depend on timely federal payments the ceremony episodically unsettles
 *   - constitutional_nullity_advocates: observer (moderate/analytical) — scholarship and executive deliberation advancing the Section 4 supersession claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.04).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.08).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling — Constitutional Nullity Reading").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'c0e3837f-6ae1-4aa5-a380-82288a9f4517').
narrative_ontology:cs_kernel_codification('c0e3837f-6ae1-4aa5-a380-82288a9f4517', formalized).
narrative_ontology:cs_authority_grounding('c0e3837f-6ae1-4aa5-a380-82288a9f4517', practice).
narrative_ontology:cs_interpretation_layer_present('c0e3837f-6ae1-4aa5-a380-82288a9f4517').
narrative_ontology:cs_reading_relation('c0e3837f-6ae1-4aa5-a380-82288a9f4517', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('c0e3837f-6ae1-4aa5-a380-82288a9f4517', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('c0e3837f-6ae1-4aa5-a380-82288a9f4517', foundational, section4_renders_statutory_debt_limits_void).
narrative_ontology:cs_axiom_status(section4_renders_statutory_debt_limits_void, holdable).
narrative_ontology:cs_axiom_grounding('c0e3837f-6ae1-4aa5-a380-82288a9f4517', section4_renders_statutory_debt_limits_void, conventional).
narrative_ontology:cs_axiom('c0e3837f-6ae1-4aa5-a380-82288a9f4517', foundational, borrowing_authority_inheres_in_enacted_appropriations).
narrative_ontology:cs_axiom_status(borrowing_authority_inheres_in_enacted_appropriations, holdable).
narrative_ontology:cs_axiom_grounding('c0e3837f-6ae1-4aa5-a380-82288a9f4517', borrowing_authority_inheres_in_enacted_appropriations, conventional).
narrative_ontology:cs_reference_frame('c0e3837f-6ae1-4aa5-a380-82288a9f4517', section4_debt_validity_baseline).
narrative_ontology:cs_drift_state('c0e3837f-6ae1-4aa5-a380-82288a9f4517', contemporary_post_2023_mainstreaming, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c0e3837f-6ae1-4aa5-a380-82288a9f4517', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, recorded_vote_seeking_legislators).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, brinkmanship_coverage_media).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, us_treasury).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, bond_market_participants).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, appropriation_funded_obligees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes the ceiling statute and periodically suspends or raises it. The votes change no payment outcome — borrowing follows enacted appropriations regardless — yet repeal never happens, because removing the formality would spend political capital while changing nothing, and each cycle supplies members with recorded votes to campaign on. A simple majority could erase the ceiling by ordinary legislation at any time.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, united_states_congress, agenda_setter,
    institutional, generational, mobile, national).

% Issues debt to meet every obligation Congress has already enacted, and maintains contingency playbooks for seasons when the ceiling's calendar collides with payment schedules. Every administration has proceeded on the working assumption that lawful debts must and will be paid; the playbooks cover a collision that the Fourteenth Amendment's debt-validity guarantee arguably makes impossible. Treasury cannot walk away from its statutory mission and absorbs the staffing, planning, and rehearsal burden of the recurring cycle.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_treasury, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, us_treasury, agenda_setter).

% Individual members who use each ceiling cycle to cast a symbolically valuable vote — for paying the nation's bills, or for fiscal restraint — at no fiscal consequence, since the borrowing outcome is fixed by appropriations either way. They can skip the ritual whenever it stops serving them; nothing binds them to it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, recorded_vote_seeking_legislators, beneficiary,
    moderate, immediate, mobile, national).

% News organizations that monetize each cycle's deadline drama — countdown coverage, negotiation-stakes framing, market-reaction segments. Their attention investment is fully portable: when a cycle ends, coverage rotates to the next spectacle at no cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, brinkmanship_coverage_media, beneficiary,
    organized, immediate, arbitrage, national).

% Holders and traders of Treasury securities who price episodic tail risk around each deadline — wider spreads, hedging flows, cash-management shifts — even though every completed cycle has ended with every debt obligation honored in full and on time. They can reprice, shorten duration, or step aside within hours, which blunts whatever cost the ritual attempts to impose on them.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bond_market_participants, payer,
    powerful, biographical, arbitrage, global).

% Social security beneficiaries, federal contractors, military families, and grant recipients whose payments run through Treasury's systems. They hold no seat in ceiling negotiations and no way to hedge; each cycle's uncertainty lands on households and vendors who did nothing to create it, even though the payments themselves have never been interrupted through a completed cycle.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, appropriation_funded_obligees, payer,
    powerless, immediate, trapped, national).

% Constitutional scholars, former officials, and executive-branch lawyers who argue that the Fourteenth Amendment's debt-validity guarantee strips the ceiling of force, and who pressed successive administrations to say so openly. Their seat is analytical: they bear none of the cycle's costs and collect none of its benefits, and their influence runs through memos, op-eds, and occasional presidential deliberation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_nullity_advocates, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None operative. Matching borrowing to enacted obligations is accomplished by the appropriations process together with Treasury's execution authority; the ceiling adds no coordinating step that appropriations do not already complete. Its votes arrive after the obligations they purport to gate have already been incurred.
% TRANSFER_FUNCTION: Nothing moves. No money, work, goods, or status transfers through the ceiling's operation on this account: borrowing proceeds as appropriations require, and the periodic votes alter no payment, no program, and no obligation. The only circulating quantities are attention and recorded-vote credit, which the ceremony recycles rather than transfers.
% ABSENT_VOICES: Future-cohort taxpayers who will service the refinanced debt have no seat in any ceiling proceeding, nor do state and municipal issuers whose borrowing costs track federal fiscal signals. Both would object to the ritual's continuation as pure avoidable noise; neither is represented by any current participant, and their absence is what lets the ceremony persist unopposed by the people whose ledger it decorates.
% DISAPPEARANCE_RATIONALE: Overnight repeal would change no payment, no issuance, and no market outcome: Treasury would continue funding enacted obligations exactly as it does now, because on this account it always has and always must. The only casualties would be the ceremony itself — the countdown coverage, the recorded votes, the contingency-playbook drills — which is to say, the ceiling's entire remaining activity.
% FOUNDING_PROBLEM: In 1917 and 1939, Congress faced a delegation problem: authorizing debt issue-by-issue had become unworkable in wartime and depression finance, so it delegated issuance discretion to Treasury while retaining a nominal aggregate checkpoint. The ceiling was built to solve the problem of stopping micromanagement of bond issuance without surrendering the form of congressional control over debt.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Research Service histories and Treasury's institutional records attest the per-issue authorization bottleneck the 1917 act removed; neither depends on this reading's adoption. That the founding problem is long solved is corroborated from outside the benefiting seats by the fact that no completed ceiling cycle has ever ended in a defaulted obligation, and by CRS documentation that the ceiling now operates as a duplicate checkpoint appended after appropriations have already bound the Treasury. No participant outside the ceremony's beneficiaries attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.04, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.04: on this reading the ceiling cannot validly bind, so it extracts nothing operative; the residual is the dead-weight of the ceremony itself (staff time, playbook drills, episodic market noise), not extraction allocated to any seat. Suppression is 0.08: the coercive pretense (default-threat rhetoric, prioritization planning) has decayed to near-vestigial as Section 4 arguments mainstreamed; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater ratio is 0.90: nearly all ceiling activity is performance — the votes ratify outcomes appropriations already mandated, and every completed cycle has ended with obligations paid in full. Accessibility collapse is 0.12: understanding this reading reveals there is no constraint to comply with; alternatives (fund as appropriated) remain fully open. Resistance is 0.30: resistance is doctrinal rather than behavioral — Section 4 advocacy, executive deliberation, market pricing-through — because there is nothing operative to resist at behavioral cost. The temporal series run on one shared eight-point grid (every tracked metric authored at every point). The theater series rises monotonically as the gap between the ceiling's form and its null function widens. The extractiveness series bumps at 2011 (the brinkmanship peak, when the pretense briefly imposed real friction) and decays thereafter as voidness mainstreamed. The suppression_requirement series is authored deliberately as an enforcement-capacity arc — buildup through the 1995-2011 weaponization era, peak coercion attempt at 2011-2013, decay afterward as threat credibility eroded — because the dynamic this story traces is the construction and erosion of enforcement machinery, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp even at near-zero extraction. From the agenda-setter seat (congress), the ceiling is a retained prerogative form: repealing it spends capital on nothing, so inertia is individually rational for every coalition. From the treasury seat, the same form is an operational hazard to be planned around — an institutional actor with constrained exit and a biographical horizon that has fused its self-conception as reliable payer to the practice of paying regardless. From the beneficiary seats, it is a free symbolic resource renewed each cycle. From the trapped obligee seat, it is pure unpriced exposure. Inter-institutional and same-level dynamics coincide here: congress and treasury hold the same nominal institutional power but opposite exit options (mobile versus constrained), which is exactly the constraint-specific factor that differentiates their computed seats — congress can end the arrangement costlessly and does not; treasury cannot exit its mission and plans around the form anyway.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (recorded_vote_seeking_legislators, brinkmanship_coverage_media) sit near the beneficiary end of directionality — subsidized by a ceremony they do not maintain, with mobile or arbitrage exit. Payers differentiate by exit: appropriation_funded_obligees (trapped, powerless) sit nearest the full-target end; bond_market_participants (arbitrage) are strongly damped — they reprice and rotate exposure within hours, so effective extraction on them approaches the subsidy end despite bearing nominal costs; us_treasury (constrained exit, institutional) bears administrative costs but also exercises the execution freedom the voidness creates, netting near-symmetric. No directionality overrides are authored: the derivation chain from beneficiary declarations, payer roles, and exit options reproduces these relationships without correction, and with base extractiveness at 0.04 the scaled effective extraction is negligible at every seat regardless. National and global scopes amplify verification difficulty modestly in the engine's arithmetic, but there is almost nothing to amplify.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetric mislabels. Mislabeling the ceiling as pure extraction would import the sibling snare account's leverage findings into a reading where the lever has no valid force — and the empirical record this reading relies on (every cycle ends with obligations honored) is evidence the leverage never consummates. Mislabeling it as coordination would credit the ceiling with work the appropriations process and Treasury's execution authority already perform. Piton captures the residue: a form maintained by inertia and ritual, whose founding problem (per-issue authorization delegation, solved 1917-1939) died decades ago — mandatrophy is resolved, and the coherent R5 pairing holds: founding_problem_status dead with disappearance_verdict world_unchanged, because nothing depends on the arrangement and nothing would rearrange. The receipt surface records the same structure affirmatively: gain_flow is diffuse (each named seat was checked; the near-nil extraction accrues to no seat — legislators and media collect symbolic value from the ceremony, which is benefit-without-receipt), and fixing_cost is prohibitive relative to benefit: repeal is mechanically cheap, but the benefit of removing a void formality is approximately zero while the political-symbolic cost (unilateral disarmament of a recurring messaging ritual both coalitions harvest) is real, so no agenda-setter spends the capital — the cost-asymmetry that keeps the shell on the books.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the statutory_debt_ceiling kernel; the same statutory instrument supports a coordination_scaffold_reading and an extraction_snare_reading. Where exactly is the disagreement among the readings located?',
    'Not resolvable by data alone: the fork turns on whether Section 4 of the Fourteenth Amendment strips the ceiling of legal force for obligations already enacted. Judicial adjudication, a definitive Justice Department opinion adopted across administrations, or settled cross-administration execution practice would collapse the fork.',
    'If the nullity premise fails, this story''s near-zero extractiveness, its world_unchanged disappearance verdict, and its piton claim all invert into the siblings'' structures — a validly binding ceiling is either scaffolding or a lever, not a nullity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity: epsilon and type are properties of this reading of the kernel, not of the colloquial label.').

omega_variable(
    section4_adjudication_absence,
    'No court has ever ruled on whether Section 4 supersedes the ceiling; does the absence of adjudication leave the ceiling''s force genuinely undetermined, or has executive practice already settled it?',
    'An obligee or bondholder suit reaching judgment; a definitive executive-branch legal opinion embraced by successive administrations of both parties; or a completed cycle in which Treasury publicly and durably disregards the ceiling without judicial objection.',
    'Adjudication for nullity converts the ceremony into confirmed dead letter and accelerates repeal; adjudication against nullity falsifies this story and restores force to the sibling readings'' accounts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(section4_adjudication_absence, empirical, 'The legal-status question underneath the reading remains formally open.').

omega_variable(
    market_pricing_of_voidness,
    'Do Treasury markets price ceiling deadlines as void (no tail risk) or as live (episodic tail risk)?',
    'Event studies comparing bill yields, sovereign CDS spreads, and repo rates across ceiling windows against matched non-window periods, controlling for issuance-calendar effects.',
    'Live-risk pricing would mean the ceremony imposes real carrying costs on the fisc and on obligees even under this reading, raising the residual extractiveness above the authored 0.04; priced-through voidness corroborates the world_unchanged verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_pricing_of_voidness, empirical, 'Whether the pretense is free or carries a market premium.').

omega_variable(
    treasury_contingency_habit,
    'Why does Treasury still build and rehearse contingency playbooks for a collision this reading holds to be legally impossible — institutional habit, reflexive legal caution, or considered judgment that the ceiling might bind?',
    'Comparative review of contingency-planning documents and testimony across administrations, distinguishing habit-driven continuity from reasoned legal positions.',
    'Habit confirms the inertial-maintenance account and the high theater ratio; considered legal judgment that the ceiling might bind would weaken the nullity premise and pull this story toward the siblings'' territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treasury_contingency_habit, empirical, 'Whether the executive branch''s own practice believes the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1979, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1979, 0.55).
narrative_ontology:measurement(stat_tr_t1985, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1985, 0.58).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.63).
narrative_ontology:measurement(stat_tr_t2005, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2005, 0.66).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.78).
narrative_ontology:measurement(stat_tr_t2015, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2015, 0.81).
narrative_ontology:measurement(stat_tr_t2021, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2021, 0.84).
narrative_ontology:measurement(stat_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.9).

% Extraction over time
narrative_ontology:measurement(stat_be_t1979, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1979, 0.03).
narrative_ontology:measurement(stat_be_t1985, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1985, 0.03).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement(stat_be_t2005, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.09).
narrative_ontology:measurement(stat_be_t2015, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2015, 0.07).
narrative_ontology:measurement(stat_be_t2021, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2021, 0.06).
narrative_ontology:measurement(stat_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.04).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1979, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1979, 0.05).
narrative_ontology:measurement(stat_su_t1985, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1985, 0.06).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(stat_su_t2005, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2011, 0.3).
narrative_ontology:measurement(stat_su_t2015, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement(stat_su_t2021, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2021, 0.15).
narrative_ontology:measurement(stat_su_t2025, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the debt ceiling' decomposes into three structurally distinct constraints sharing a statutory referent but differing on the premise of legal force. This file instantiates the constitutional_nullity_reading (ceiling void under Fourteenth Amendment Section 4; epsilon near zero; world unchanged if repealed). The coordination_scaffold_reading treats the same statute as valid procedural coordination; the extraction_snare_reading treats it as a valid weaponized boundary. The constitutional text (ratified 1868, predating the 1917 and 1939 statutes) is upstream of all three readings and is cited by each as support. Each sibling file should link back here via network.affects_constraints. Epsilon differs across the family because binding force — hence who can be made to pay what — differs by reading, not because the referent is observer-relative within any one reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
