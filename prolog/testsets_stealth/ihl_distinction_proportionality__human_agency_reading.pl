% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction/Proportionality — Irreducible Human Judgment Requirement (Human Agency Reading)
 *   domain: legal/ethical/technological
 *
 * SUMMARY:
 *   This story instantiates the human_agency_reading of the kernel
 *   ihl_distinction_proportionality: the claim that IHL's distinction and
 *   proportionality obligations require irreducible human moral judgment at
 *   the moment of lethal force application, and that Martens Clause
 *   principles of humanity forbid delegating life/death decisions to
 *   machines. The constraint under classification is the human-judgment
 *   requirement as an operative norm pressing against the current automation
 *   trajectory — embedded partially in Article 36 review practice, national
 *   directives on autonomy in weapon systems, and the CCW GGE debate. EPSILON
 *   REFERENT: the standing arrangement under contest is this requirement as
 *   it actually operates on military practice, assessed by this reading's own
 *   lights; the reading's endorsed ideal (a fully human-centered targeting
 *   order) is NOT the referent, and the outcomes-based alternative is a
 *   different constraint authored in a sibling file. Per the
 *   epsilon-invariance principle, the colloquial label 'IHL governs
 *   autonomous weapons' decomposes into a three-story constraint family
 *   (human_agency, outcomes_based, categorical_prohibition), each with its
 *   own epsilon, beneficiary/victim structure, and classification; this file
 *   links both siblings via network.affects_constraints. Claim and metrics
 *   are authored independently: the claimed type reflects the structure I
 *   believe true (a genuine accountability/dignity coordination core carrying
 *   asymmetric costs and interpretive rents); the metrics describe the
 *   constraint's operation as I assess it descriptively.
 *
 * KEY AGENTS:
 *   - - icrc_interpretive_authorities: Primary beneficiary and interpretive enforcer (institutional/identity_locked) — maintains doctrinal centrality through the human-judgment framing
 *   - - advanced_military_powers: Primary target (institutional/arbitrage) — pays in foregone machine-speed engagement; hedges around the rule
 *   - - defense_ai_industry: Secondary target (powerful/mobile) — its core product class is foreclosed; pivots to supervised and commercial lines
 *   - - field_commanders and frontline_human_operators: Burdened intermediaries (trapped) — absorb tempo losses, moral weight, and accountability-sink exposure while holding the formally reserved decision authority
 *   - - technologically_lagging_states and humanitarian_disarmament_coalitions: Secondary beneficiaries (organized/constrained) — strategic freeze and campaign relevance respectively
 *   - - civilian_populations_in_conflict_zones: Intended protected party (powerless/trapped) — decisions about their deaths routed through answerable humans
 *   - - prospective_victims_of_human_error: Excluded voice (powerless/trapped) — their counterfactual objection to mandated human slowness is structurally unvoiced
 *   - - ihl_academic_scholars: Analytical observer (analytical/analytical) — maps the interpretive contest without bearing its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.7).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.76).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction/Proportionality — Irreducible Human Judgment Requirement (Human Agency Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "legal/ethical/technological").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '80d9359a-50df-4b88-906f-2a739560e00a').
narrative_ontology:cs_kernel_codification('80d9359a-50df-4b88-906f-2a739560e00a', fixed_text).
narrative_ontology:cs_authority_grounding('80d9359a-50df-4b88-906f-2a739560e00a', lineage).
narrative_ontology:cs_interpretation_layer_present('80d9359a-50df-4b88-906f-2a739560e00a').
narrative_ontology:cs_reading_relation('80d9359a-50df-4b88-906f-2a739560e00a', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_reading_relation('80d9359a-50df-4b88-906f-2a739560e00a', ihl_distinction_proportionality__outcomes_based_reading, coexists_with).
narrative_ontology:cs_axiom('80d9359a-50df-4b88-906f-2a739560e00a', foundational, lethal_force_requires_irreducible_human_judgment).
narrative_ontology:cs_axiom_status(lethal_force_requires_irreducible_human_judgment, holdable).
narrative_ontology:cs_axiom_grounding('80d9359a-50df-4b88-906f-2a739560e00a', lethal_force_requires_irreducible_human_judgment, deontological).
narrative_ontology:cs_axiom('80d9359a-50df-4b88-906f-2a739560e00a', secondary, martens_clause_forbids_machine_life_death_delegation).
narrative_ontology:cs_axiom_status(martens_clause_forbids_machine_life_death_delegation, holdable).
narrative_ontology:cs_axiom_grounding('80d9359a-50df-4b88-906f-2a739560e00a', martens_clause_forbids_machine_life_death_delegation, deontological).
narrative_ontology:cs_reference_frame('80d9359a-50df-4b88-906f-2a739560e00a', irreducible_human_judgment_baseline).
narrative_ontology:cs_drift_state('80d9359a-50df-4b88-906f-2a739560e00a', contemporary_ccw_gge_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('80d9359a-50df-4b88-906f-2a739560e00a', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_disarmament_coalitions).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, technologically_lagging_states).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, advanced_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, defense_ai_industry).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, field_commanders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, frontline_human_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, field_commanders).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, frontline_human_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodian and leading interpreter of international humanitarian law. Publishes the position that distinction and proportionality can only be discharged by a human being at the point of attack, lobbies states through the CCW process, and trains military lawyers worldwide. Its budget, access, and standing depend on remaining the authoritative voice on lawful killing; stepping back from that role would mean abandoning its constitutive mission, so leaving the position is not a real option.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities, agenda_setter).

% An umbrella of NGOs campaigning for a new treaty instrument on autonomous weapons. Organizes state pledges, parliamentary initiatives, and public campaigns. Staff careers, donor relationships, and organizational missions are built around this campaign; disengaging would dissolve the coalition's reason for existence, though individual organizations could redirect to adjacent causes at real cost.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, humanitarian_disarmament_coalitions, beneficiary,
    organized, generational, constrained, global).

% States without frontier military-AI industries that support binding limits on autonomous targeting. A freeze on machine-speed engagement keeps the battlefield at a tempo where their forces can compete. They gain strategically from the limits while contributing little enforcement effort; their flexibility is bounded by alliance politics and the desire to remain inside a future treaty perimeter.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, technologically_lagging_states, beneficiary,
    organized, generational, constrained, global).

% People living where these systems operate. The rule routes decisions about their deaths through identifiable, answerable humans rather than opaque automated pipelines. They are exposed to whichever decision-making arrangement prevails and have no seat in the diplomatic processes that set it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% States running the largest autonomy research programs. They pay in foregone operational speed: engagement cycles that machines could run in milliseconds must pause for human decision. They sustain parallel work under ambiguous labels — defensive autonomy, human-on-the-loop loitering munitions — and hedge through allied frameworks and definitional boundaries, so they can shift costs around the rule but not ignore the reputational and alliance consequences of open defiance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, advanced_military_powers, payer,
    institutional, generational, arbitrage, global).

% Contractors whose most ambitious product line — systems that select and engage targets without a human decision — sits on the prohibited side of the line. Current revenue comes from decision-support, targeting aids, and supervised platforms; the fully autonomous segment is deferred indefinitely. Firms can pivot engineers and capital into commercial AI or dual-use markets, but the core program they were built to deliver is the one the rule closes off.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, defense_ai_industry, payer,
    powerful, biographical, mobile, global).

% Officers who must plan operations around mandatory human decision points, accepting slower tempo, and who personally answer when a strike they approved goes wrong. They also hold the formal authority the rule reserves: the decision to fire is legally theirs, which shields them from any order to delegate it. Career structures and command responsibility mean they cannot walk away from either side of that position.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, field_commanders, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, field_commanders, beneficiary).

% The designated person in the engagement chain — often working from screens, with seconds to approve or reject a machine-proposed target. They carry the moral weight of each decision and serve as the named responsible party when systems err, while receiving the formal protection of being the one who decided. Orders, unit membership, and career consequences leave no practical exit from the seat.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, frontline_human_operators, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, frontline_human_operators, beneficiary).

% People who would die from tired, biased, or slow human decisions that a well-performing automated system might have avoided. Their counterfactual objection — that insisting on a human finger on the trigger costs lives — is structurally unvoiced: they are hypothetical casualties, and the debate counts machine-error victims far more loudly than human-error ones.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, prospective_victims_of_human_error, excluded,
    powerless, immediate, trapped, global).

% Academic specialists in IHL and military ethics who map the interpretive dispute, test each position against treaty text and custom, and publish the analyses both camps cite. They take no side in the negotiation and bear none of its costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_academic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, diffuse).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for where human responsibility must sit in lethal-force decisions: states, commanders, and operators align on the locus of accountability, rules of engagement interoperate around common human decision points, and review bodies have a determinate object to assess.
% TRANSFER_FUNCTION: Moves decision authority and its costs: concentrates the moral and legal burden of each killing onto designated human operators and commanders; transfers operational speed from advanced militaries to nobody (largely destroyed as deadweight); transfers interpretive authority and doctrinal centrality toward the humanitarian-law custodial institutions; transfers strategic relief to states that could not win an autonomy race.
% ABSENT_VOICES: Prospective victims of retained human error have no seat: those who would die from slower, fatigued, or biased human decisions that optimized automation might have prevented cannot object, and the debate's accounting asymmetry (loud for machine-error victims, silent for human-error ones) follows from their absence. Conflict-zone civilians are spoken for but not present. Soldiers who might be spared by faster autonomous defense are similarly unrepresented.
% DISAPPEARANCE_RATIONALE: If the human-judgment requirement vanished overnight, targeting authority would migrate rapidly toward machine-speed pipelines wherever the technology permitted: national directives, Article 36 review practice, and the 'meaningful human control' compliance architecture would dissolve, alliance rules of engagement would fragment along technological lines, the ICRC's interpretive program on the question would lose its object, and a competitive dynamic among advanced powers would reorganize procurement around full autonomy. The accountability chains built on named human deciders would need wholesale reconstruction.
% FOUNDING_PROBLEM: Ensuring that killing in war remains attributable to answerable human agents: preserving distinction and proportionality as duties someone can actually discharge, and preventing an accountability gap when target selection and engagement decisions are made by systems no one can blame, court-martial, or deter. The genealogy runs through the Martens Clause (1899) and the post-Nuremberg individual-responsibility settlement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the judge advocate general and military legal communities of the advanced powers themselves attest the accountability-gap problem — national directives on autonomy in weapon systems explicitly cite the need for human judgment and traceable responsibility, and defense-academy literature treats the gap as a serious engineering-law problem. UN Secretary-General reports and CCW GGE chair summaries record the concern across blocs. What these sources corroborate is the founding problem's liveness, not this reading's particular solution to it; the outcomes-based camp disputes the solution while conceding the problem.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) because the requirement's costs scale with the maturity of the technology it forecloses: every advance in machine perception and targeting widens the gap between what supervised pipelines can do and what the rule permits, and the foregone efficiency is largely deadweight — destroyed rather than transferred. Suppression (0.76) is a raw structural property, unscaled by power or scope: the rule categorically forecloses an entire system class regardless of demonstrated performance, which is precisely what distinguishes this reading from the outcomes-based sibling. Accessibility_collapse (0.66) is substantial but incomplete: within compliant jurisdictions the fully autonomous alternative collapses almost entirely, but arbitrage channels (definitional boundaries, allied frameworks, deferred stockpiles) keep it partly available. Resistance (0.58) is real and organized: advanced powers block consensus instruments while continuing development. Theater_ratio (0.44) is the story's sharpest diagnostic: as 'meaningful human control' became the compliance formula, implementation drifted toward nominal approval workflows — humans positioned to sign rather than to judge — a textbook Goodhart drift on the proxy 'a human is in the loop.' The temporal series run on one shared eight-point grid (2012–2026, all three metrics at every point). Base_extractiveness rises monotonically with technological opportunity cost; theater_ratio rises with compliance-formula drift; suppression_requirement rises because enforcement machinery was deliberately built up over the interval (GGE sessions from 2014, UNGA resolutions from 2022, national review regimes) — this is an enforcement-intensification trajectory, which is why suppression_requirement is authored rather than left static. Note the deliberate divergence between base_properties.suppression (0.76) and the suppression_requirement series endpoint (0.62): total suppression exceeds visible enforcement capacity because much of the foreclosure operates through self-censorship, funding structures, and career incentives rather than active enforcement. Receipt surface: gain_flow is authored as 'diffuse' affirmatively — the largest single flow is deadweight loss received by no one, and the institutional gains that do exist (interpretive centrality, strategic freeze, campaign relevance) scatter across at least three seats with no concentrated capturer. Fixing_cost is 'prohibitive': removal would collapse the accountability architecture and trigger race dynamics whose costs exceed the efficiency benefit for any feasible fixing agent.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical text. From the ICRC seat, the arrangement is guardianship: the interpretive line it maintains is the thin barrier between lawful force and industrialized anonymous killing, and its own centrality is inseparable from the barrier's maintenance. From the advanced-power seat, the same arrangement is a competitive handicap imposed by actors who bear none of its operational costs, evaded where possible and endured where alliance politics compel. From the operator seat, it is a double bind: the rule both burdens them (seconds-scale approval windows, moral injury, named liability when systems err) and protects them (the decision is legally theirs; no order can take it away). From the lagging-state seat, it is a leveling device that keeps the battlefield at human tempo. The engine computes these per-seat classifications from the structural data — power, exit, and directional position — not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: ICRC, coalitions, lagging states, and conflict-zone civilians sit near the beneficiary pole (low d); advanced powers, the defense industry, commanders, and operators sit near the target pole (high d), with the trapped exit of commanders and operators amplifying their effective position and the arbitrage of advanced powers damping theirs. One override is authored: power_atom 'powerful' (held uniquely by defense_ai_industry) at d=0.70. The derivation from victim-status plus mobile exit would damp d toward symmetry, on the theory that pivot capacity escapes the extraction; but the foreclosure strikes the industry's central product class directly regardless of pivot capacity — the fully autonomous engagement system is the program the firms exist to build, and commercial diversion is consolation, not escape. The true structural relationship sits much nearer the full-target end than mobility alone predicts. No override is needed elsewhere: the institutional atom correctly splits by declaration (ICRC beneficiary vs advanced powers victim), and the powerless atom's three holders are separated by their declared roles rather than by any single corrected value.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. Against the rope reading: the extraction is real and asymmetric — the efficiency sacrificed is deadweight, the interpretive rents accrue to identifiable custodians, and the categorical foreclosure suppresses alternatives on deontological rather than performance grounds. Against the snare reading: the coordination core is genuine and externally corroborated (the accountability-gap problem is attested by military legal communities themselves, outside the beneficiary set), the victims retain arbitrage and political recourse rather than captivity, and no victim class is held in place by the constraint for its benefit. Mandatrophy is NOT resolved: the founding problem (locating responsibility for killing in an answerable agent) is live and intensifying, so the arrangement has not outlived its function. The forward risk is the theater trajectory: if theater_ratio crosses 0.5 — nominal human approval everywhere, judgment nowhere — the constraint decays toward piton, retaining the liability-allocation shell while the substantive function migrates into software. That crossover, not any treaty event, is the lifecycle signal to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel ihl_distinction_proportionality (reading: human_agency_reading). How would the sibling readings change the structure if adopted?',
    'Adopting the outcomes_based_reading would collapse the victim set (military efficiency costs vanish where performance parity is demonstrated) and drive epsilon toward the coordination floor; adopting the categorical_prohibition_reading would widen victims to include supervised autonomy and saturate suppression. The disagreement is located at one structural element: whether IHL obligations are agent-relative (dischargeable only by a moral agent) or outcome-relative (dischargeable by any sufficiently reliable mechanism).',
    'Classification is reading-indexed: the same colloquial label (''IHL governs autonomous weapons'') yields three structurally distinct constraints with different epsilon, beneficiaries, and victims. Cross-reading comparison is valid only between separately authored files linked through the network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three live readings of the IHL distinction/proportionality kernel; sibling readings instantiate different constraints.').

omega_variable(
    meaningful_control_theater_question,
    'Is the declared human control in supervised systems functional moral judgment or nominal approval — a signature on a machine-made decision?',
    'Decision-latency audits of engagement chains: if human approval windows are shorter than the sensor-fusion-to-option-generation processing minimum, approval is provably nominal. Corroborate with operator testimony on rejection rates and workload.',
    'If approval is largely nominal, theater_ratio crosses 0.5 and the constraint drifts toward piton: the residual function reduces to liability allocation while the substantive judgment function is performed by software. If approval is substantive, the coordination core is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_control_theater_question, empirical, 'Whether ''human in the loop'' compliance is judgment or rubber-stamping.').

omega_variable(
    human_vs_machine_performance_bracket,
    'The reading brackets the outcomes question by construction: does mandated human judgment actually reduce unlawful harm relative to mature automation, or does it impose costs without protective payoff?',
    'Comparative error and casualty studies across matched engagement classes where human-supervised and automated decision paths can be contrasted, controlling for target environment and threat density.',
    'Demonstrated machine superiority would raise the constraint''s net cost (suppression without protective return) and strengthen the outcomes_based sibling; persistent human superiority would vindicate the coordination core and justify the efficiency sacrifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_vs_machine_performance_bracket, empirical, 'Empirical uncertainty the reading deliberately walls off from its own justification.').

omega_variable(
    codification_trajectory,
    'Will the human-judgment requirement harden into binding treaty law, or remain a soft norm sustained by advocacy and national practice?',
    'Track CCW/UNGA treaty-track outcomes, national implementing legislation, and alliance-level directives over the coming decade.',
    'Binding codification raises suppression further and entrenches beneficiary positions; continued soft-norm status leaves the arrangement vulnerable to great-power defection and potentially rapid decay, since enforcement currently rests on reputational and alliance mechanisms rather than adjudicated obligation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(codification_trajectory, empirical, 'Whether the constraint consolidates or erodes institutionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2012, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(ihl__tr_t2014, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(ihl__tr_t2016, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2018, 0.34).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ihl__tr_t2022, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2022, 0.41).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2024, 0.43).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2012, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2012, 0.54).
narrative_ontology:measurement(ihl__be_t2014, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(ihl__be_t2016, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2018, 0.63).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(ihl__be_t2022, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2024, 0.69).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2012, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2012, 0.32).
narrative_ontology:measurement(ihl__su_t2014, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2014, 0.38).
narrative_ontology:measurement(ihl__su_t2016, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2016, 0.44).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(ihl__su_t2022, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2024, 0.61).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distiction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL obligations on autonomous weapons' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (human_agency_reading, tangled_rope: agent-relative obligations, high epsilon, ICRC-centrality beneficiaries, efficiency-cost victims) links to its two siblings. The upstream/downstream structure runs from this reading to the categorical_prohibition_reading (whose ban campaign depends on this reading's Martens-Clause and human-judgment premises), while the outcomes_based_reading coexists as a live competing position. Sibling constraint_ids follow the {kernel}__{reading} pattern; if the sibling files use different ids, update these edges at compile time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
