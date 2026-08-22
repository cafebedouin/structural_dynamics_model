% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border-Control Legitimacy under the Jurisdictional-Sovereignty Reading
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   Destination states operate a border-control apparatus — visa regimes,
 *   asylum procedures, detention, removal, and externalized enforcement —
 *   that this reading evaluates through the jurisdictional-sovereignty lens:
 *   sovereignty confers authority to regulate rights and obligations within
 *   territory, that authority extends to conditioning entry, but it is not an
 *   absolute closure power and its exercise is legitimate only when it
 *   balances protection obligations, labor needs, and public consent. The
 *   epsilon referent is the standing enforcement-heavy arrangement as this
 *   reading sees it: substantially costly to those it excludes and detains,
 *   and burdenshifting onto residents whose consent is invoked more than
 *   consulted, while still performing an allocation function no available
 *   alternative provides. Assumptions stated: the interval maps T=0 to circa
 *   1990 (post-Cold War globalization of migration, early externalization
 *   experiments) and T=35 to circa 2025; 'displaced citizens' denotes
 *   residents of admission-concentrated districts who bear adaptation costs
 *   without setting admission policy. This story is one member of a
 *   three-story constraint family decomposing the border_control_legitimacy
 *   kernel; the siblings author different epsilon over the same referent. KEY
 *   AGENTS (by structural relationship): - receiving_state_executives:
 *   agenda-setter (institutional/arbitrage) — writes the rules, negotiates
 *   externalization, captures discretion and credit -
 *   immigration_bureaucracies: beneficiary (institutional/constrained) —
 *   budgets and powers scale with enforcement volume - licensed_employers:
 *   beneficiary (powerful/constrained) — purchase rationed,
 *   sponsorship-dependent labor - resident_publics: beneficiary and payer
 *   (organized/constrained) — receive ordered admission, fund enforcement,
 *   absorb unconsented change - excluded_migrants: primary target
 *   (powerless/trapped) — bear denial, limbo, and route risk -
 *   detained_asylum_seekers: primary target (powerless/trapped) — confinement
 *   as the price of claiming - displaced_citizens: secondary target
 *   (moderate/constrained) — adaptation costs without policy voice -
 *   human_rights_courts: analytical observer (institutional/analytical) —
 *   proportionality review supplies the internal limit -
 *   refugee_protection_agencies: excluded voice (institutional/constrained) —
 *   locked out of the bilateral bargains - smuggling_intermediaries:
 *   parasitic beneficiary (organized/arbitrage) — revenue scales with
 *   restriction
 *
 * KEY AGENTS:
 *   - receiving_state_executives: agenda-setter (institutional/arbitrage) — sets admission rules, directs enforcement, captures discretion and electoral credit, can shift burdens to other jurisdictions
 *   - immigration_bureaucracies: beneficiary (institutional/constrained) — headcount, budgets, and statutory powers grow with control activity
 *   - licensed_employers: beneficiary (powerful/constrained) — access a rationed workforce whose status depends on continued sponsorship
 *   - resident_publics: beneficiary with payer secondary role (organized/constrained) — receive sequenced admission and planning horizons, pay fiscally and morally, absorb change at uneven speeds
 *   - excluded_migrants: primary target (powerless/trapped) — denial of entry, family separation, procedural limbo, irregular-route risk
 *   - detained_asylum_seekers: primary target (powerless/trapped) — administrative confinement as the condition of pursuing a claim
 *   - displaced_citizens: secondary target (moderate/constrained) — bear localized adaptation costs with no seat in admission policy
 *   - human_rights_courts: analytical observer (institutional/analytical) — enforce proportionality, necessity, and non-refoulement limits
 *   - refugee_protection_agencies: excluded (institutional/constrained) — supervisory mandate without a seat in the operative bilateral deals
 *   - smuggling_intermediaries: parasitic beneficiary (organized/arbitrage) — sell passage around legal channels; revenue scales with restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.66).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.66).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border-Control Legitimacy under the Jurisdictional-Sovereignty Reading").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '8a8745a1-b053-4756-ac1d-04f21fdfbed4').
narrative_ontology:cs_kernel_codification('8a8745a1-b053-4756-ac1d-04f21fdfbed4', formalized).
narrative_ontology:cs_authority_grounding('8a8745a1-b053-4756-ac1d-04f21fdfbed4', lineage).
narrative_ontology:cs_interpretation_layer_present('8a8745a1-b053-4756-ac1d-04f21fdfbed4').
narrative_ontology:cs_reading_relation('8a8745a1-b053-4756-ac1d-04f21fdfbed4', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('8a8745a1-b053-4756-ac1d-04f21fdfbed4', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('8a8745a1-b053-4756-ac1d-04f21fdfbed4', foundational, border_authority_is_jurisdictional_not_absolute).
narrative_ontology:cs_axiom_status(border_authority_is_jurisdictional_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8a8745a1-b053-4756-ac1d-04f21fdfbed4', border_authority_is_jurisdictional_not_absolute, conventional).
narrative_ontology:cs_axiom('8a8745a1-b053-4756-ac1d-04f21fdfbed4', foundational, legitimacy_requires_balanced_exercise).
narrative_ontology:cs_axiom_status(legitimacy_requires_balanced_exercise, holdable).
narrative_ontology:cs_axiom_grounding('8a8745a1-b053-4756-ac1d-04f21fdfbed4', legitimacy_requires_balanced_exercise, instrumental).
narrative_ontology:cs_axiom('8a8745a1-b053-4756-ac1d-04f21fdfbed4', secondary, non_refoulement_is_hard_limit).
narrative_ontology:cs_axiom_status(non_refoulement_is_hard_limit, holdable).
narrative_ontology:cs_axiom_grounding('8a8745a1-b053-4756-ac1d-04f21fdfbed4', non_refoulement_is_hard_limit, deontological).
narrative_ontology:cs_reference_frame('8a8745a1-b053-4756-ac1d-04f21fdfbed4', balanced_jurisdictional_admission).
narrative_ontology:cs_drift_state('8a8745a1-b053-4756-ac1d-04f21fdfbed4', contemporary_externalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a8745a1-b053-4756-ac1d-04f21fdfbed4', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_executives).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, immigration_bureaucracies).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, licensed_employers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, resident_publics).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, smuggling_intermediaries).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, detained_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, resident_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets admission rules through legislation and executive action, negotiates externalized-enforcement agreements with transit and origin states, and directs the agencies running visas, asylum processing, detention, and removal. Gains discretionary control over who enters and on what terms, electoral credit from visible control, and bargaining leverage in visa diplomacy; can route enforcement burdens onto other jurisdictions when domestic costs rise.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_executives, agenda_setter,
    institutional, generational, arbitrage, national).

% Staffs and expands on the control mandate: caseworkers, border guards, detention operators, and asylum adjudicators whose budgets, headcount, and statutory powers grow with the volume of control activity. Careers and institutional identity are built on the mandate; a shrinking enforcement task means reassignment or redundancy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, immigration_bureaucracies, beneficiary,
    institutional, biographical, constrained, national).

% Accesses a rationed labor supply through sponsored work visas and seasonal schemes calibrated by the state. Gains a workforce whose legal status depends on continued sponsorship, which disciplines turnover and wage demands; lobbies sector by sector to widen or narrow quotas as conditions shift.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, licensed_employers, beneficiary,
    powerful, biographical, constrained, global).

% Receives sequenced admission, public-goods planning horizons, and nominal protection of wage floors. Also funds detention and removal through taxation, absorbs neighborhood and service-level change at uneven speeds, and finds that major admission shifts are negotiated over its head — consent is invoked rhetorically and consulted episodically at elections, then presented as settled between contests.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, resident_publics, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, resident_publics, payer).

% Bears denial of entry, family separation, years in procedural limbo, and the physical risks of irregular routes. Legal status inside the territory is conditional or absent. Leaving the situation means abandoning migration plans that survival, persecution, or family unity may make non-optional; staying means living outside the law's protection.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Held in administrative detention while claims are processed, sometimes for months or years, under expedited procedures and geographic restrictions. Cannot leave detention without forfeiting the claim and cannot pursue the claim without remaining confined; the choice set is the confinement itself.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, detained_asylum_seekers, payer,
    powerless, immediate, trapped, regional).

% Lives in the districts where admission concentrates: school places, housing stock, and entry-level labor markets adjust faster than public services do. Bears adaptation costs without any seat in admission policy; objections surface politically as consent shocks after decisions have already been implemented.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, regional).

% Reviews enforcement against proportionality and necessity standards, non-refoulement obligations, and family-life protections. Invalidates individual removals and occasionally entire schemes, supplying the arrangement's internal limit without administering any part of it; depends on complainants reaching the courtroom.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_courts, observer,
    institutional, generational, analytical, continental).

% Holds a supervisory mandate over protection obligations but is locked out of the bilateral enforcement deals that increasingly determine who ever reaches territory. Documents violations, negotiates marginal access, and publishes counts that the operative decision-makers cite selectively.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, refugee_protection_agencies, excluded,
    institutional, generational, constrained, global).

% Sells passage around the legal channels. Every tightening of lawful routes raises the price of its service, so revenue scales with restriction itself. Operates outside the arrangement's rules, shifts all enforcement risk onto clients, and relocates along routes faster than enforcement can close them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, smuggling_intermediaries, beneficiary,
    organized, immediate, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_executives).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences and conditions entry so that protection duties are honored, labor demand is matched without collapsing wage floors, and integration proceeds at a rate public institutions can absorb — a collective-action problem spanning origin, transit, and destination jurisdictions that no unilateral actor solves alone.
% TRANSFER_FUNCTION: Moves legal status and physical access from a global pool of would-be entrants to those the state selects; moves enforcement costs onto detainees, removees, and taxpayers; moves a rationed, sponsorship-dependent workforce to licensed employers; moves political credit to the executives who administer the gate.
% ABSENT_VOICES: Excluded migrants hold no vote in the polities deciding their cases. Refugee protection agencies and origin-state governments sit outside the bilateral enforcement bargains that now set the operative rules. The residents of tomorrow's admission-shifted districts are not yet identifiable participants. Each would object from a different direction: the first at the severity of exclusion, the second at externalization's accountability gap, the third at decisions taken before affected communities existed as constituencies.
% DISAPPEARANCE_RATIONALE: If the admission-control arrangement vanished overnight, labor supply chains built on sponsored migration would rupture within seasons, asylum systems and the litigation practice around them would lose their object, citizenship and integration pipelines would reorganize, origin-state remittance economies would contract sharply, and smuggling networks would lose their price floor — the surrounding institutional world is arranged around the gate and would rearrange itself around its absence.
% FOUNDING_PROBLEM: After the World Wars collapsed the pre-war free-movement order, states had to rebuild the terms on which non-members enter: mass displacement met acute labor shortage, and territorial jurisdictions needed admission rules that honored protection duties without outrunning the consent of the publics funding and absorbing the results.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR global displacement reporting and ILO labor-migration reviews attest both halves of the founding problem — protection pressure and labor demand — from outside the benefiting parties; comparative migration-law scholarship corroborates the consent-strain half. No corroborating source attests that the balance is currently struck; the standing disagreement between protection bodies, courts, and executives is itself the signal that the problem remains live.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: the arrangement imposes heavy, concentrated costs on excluded and detained migrants and pushes adaptation and fiscal burdens onto residents without their meaningful consent, offset partially by a real allocation function. Suppression at 0.62 reflects detention, carrier sanctions, externalized pushback arrangements, and status-dependence — coercive machinery that is real but judicially bounded. Theater at 0.41 captures the widening gap between humanitarian framing ('safe and orderly routes') and deterrence practice, plus security rhetoric wrapped around labor-rationing functions. Accessibility_collapse at 0.45: alternatives persist (asylum claims, judicial review, regular pathways, origin-side stay options) but have been progressively narrowed, so the structure does not behave like an inevitability. Resistance at 0.58: sustained litigation, advocacy campaigns, irregular-route persistence, and episodic local consent shocks. Claimed type is tangled_rope on structural grounds independent of these scores: a genuine coordination problem (sequencing entry against protection duties, labor demand, and absorptive capacity) is solved through the same machinery that asymmetrically extracts from the excluded and the unconsented, and the arrangement survives only through continuous active enforcement. The three measurement series share one time grid (points 0, 7, 14, 21, 28, 35) so every metric is authored at every examined time point; all series rise over the interval, tracking externalization deals, detention expansion, and the growing rhetorical-practical gap.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the executive seat the arrangement is a legitimate balancing act it performs under constraint; from the excluded-migrant and detained-asylum-seeker seats the same structure presents as a barrier with no workable exit; from the displaced-citizen seat it presents as a burden imposed without consultation; from the court seat it presents as a rule-bound process with correctable excesses; from the smuggling seat it presents as a price floor. Same structure, four incompatible experiences — the engine derives this divergence from the declared roles, power atoms, and exit options rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the executive, bureaucratic, employer, and smuggling seats: the first three collect discretion, budgets, and labor access; the smuggler collects counter-intuitively, since every lawful-route tightening raises its service price and its arbitrage-grade exit insulates it from the costs it helps inflate. Payer declarations drive high directionality for excluded migrants and detained asylum seekers, whose trapped exit puts them near the full-target end. Resident publics carry both declarations (beneficiary primary, payer secondary): genuine coordination benefits pull them toward symmetry while fiscal, moral, and consent costs pull the other way, landing them mid-range. Displaced citizens, declared payers with constrained exit, sit target-ward but below the trapped seats since their losses are diffuse and partially compensable. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate every seat, including the institutional trio (executives arbitrage, bureaucracies constrained, courts analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling territorial jurisdiction with mass displacement and labor demand without outrunning public consent — is live, so this is not a mandate outliving its function. The classification discipline cuts both ways here. Reading the whole apparatus as pure extraction erases the allocation function that no surveyed alternative provides (unilateral open admission dissolves consent; unilateral closure breaches protection duties and starves labor markets); reading it as pure coordination erases the documented dual-victim asymmetry the balance is supposed to prevent. The tangled_rope claim keeps both facts in view. Drift watches: if proportionality review hollows out while enforcement intensifies, the tangle slides toward pure extraction; if enforcement decays into ritual screening behind a maintained facade, it slides toward inertial performance. The rising theater_ratio series is the early indicator for the second slide; the rising suppression_requirement series is the early indicator for the first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the jurisdictional_sovereignty reading of the border_control_legitimacy kernel; how would the sibling readings restructure the same arrangement?',
    'Generate the sibling stories (sovereignty_primary, freedom_of_movement_primary) and compare epsilon, victim sets, and enforcement structure across the family.',
    'Under sovereignty_primary the victim set collapses to migrants alone and the proportionality limits vanish, raising effective extraction; under freedom_of_movement_primary the allocation function itself becomes suspect and epsilon rises further while the beneficiary set shrinks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file is one reading of a three-reading kernel; sibling files instantiate the others.').

omega_variable(
    dual_victim_weighting,
    'How should the two acknowledged victim sets — excluded migrants and consent-bypassed resident districts — be weighted against each other inside the legitimacy balance?',
    'Distributional incidence analysis of enforcement and admission effects combined with deliberative polling in admission-concentrated districts.',
    'The weighting decides whether the binding failure reads as under-admission (migrant-side remedy dominates) or over-admission-without-consent (citizen-side remedy dominates), flipping which seat drives reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_victim_weighting, preference, 'The reading''s own balancing criterion leaves the inter-victim tradeoff unresolved.').

omega_variable(
    proportionality_review_efficacy,
    'Do proportionality and necessity tests actually constrain enforcement practice, or do they legitimate it while practice drifts past the reference frame?',
    'Compare removal and detention rates before and after landmark rulings; track compliance lag and scheme-reintroduction-after-invalidation patterns.',
    'If review is ineffective, the reading''s constraining delta is largely performative and effective extraction climbs toward the sovereignty_primary profile; if effective, the tangle stays balanced enough to hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_review_efficacy, empirical, 'Whether the judicial constraint layer binds or decorates.').

omega_variable(
    consent_operationalization,
    'What counts as public consent for legitimacy — electoral majorities, deliberative microcosms, or mere absence of organized revolt?',
    'Comparative analysis of which operationalization predicts policy stability and compliance across destination jurisdictions.',
    'An electoral operationalization licenses majoritarian closure and lowers measured illegitimacy; a deliberative operationalization raises the consent bar and increases the measured illegitimacy of current practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_operationalization, conceptual, 'The consent term of the balance is underspecified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.24).
narrative_ontology:measurement(bord_tr_t7, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 7, 0.27).
narrative_ontology:measurement(bord_tr_t14, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 14, 0.31).
narrative_ontology:measurement(bord_tr_t21, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 21, 0.34).
narrative_ontology:measurement(bord_tr_t28, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 28, 0.38).
narrative_ontology:measurement(bord_tr_t35, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(bord_be_t7, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(bord_be_t14, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 14, 0.56).
narrative_ontology:measurement(bord_be_t21, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(bord_be_t28, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 28, 0.63).
narrative_ontology:measurement(bord_be_t35, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 35, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(bord_su_t7, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 7, 0.49).
narrative_ontology:measurement(bord_su_t14, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 14, 0.53).
narrative_ontology:measurement(bord_su_t21, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 21, 0.57).
narrative_ontology:measurement(bord_su_t28, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(bord_su_t35, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the border_control_legitimacy kernel per the epsilon-invariance principle: the colloquial label 'border control legitimacy' conflates three structurally distinct claims about what sovereignty entails at the edge. This story authors the jurisdictional_sovereignty reading (bounded jurisdictional authority, legitimacy as balance, dual victim sets, proportionality-constrained enforcement). The sovereignty_primary sibling authors absolute exclusion discretion (single victim set, unconstrained enforcement, higher epsilon); the freedom_of_movement_primary sibling authors movement-as-fundamental-right (the allocation function itself contested, highest epsilon, smallest beneficiary set). Each member links the others through affects_constraints; upstream/downstream structure runs from the jurisdictional reading outward, since its proportionality concessions are cited as evidence by both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
