% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty: Unconditional Domestic Authority and Categorical Non-Intervention
 *   domain: political/legal/international-governance
 *
 * SUMMARY:
 *   The absolute reading of sovereign equality, codified in the UN Charter's
 *   domestic-jurisdiction clause and in customary non-intervention law: every
 *   state holds unconditional authority over its domestic affairs, and
 *   external interference is categorically illegitimate. The arrangement
 *   solves a real collective problem, removing the historical drivers of
 *   interstate war by guaranteeing each government an untouchable domestic
 *   sphere, and it simultaneously grants every executive, above all
 *   executives engaged in repression, a shield that removes their treatment
 *   of their own population from external scrutiny or remedy. The people
 *   inside repressive states are the arrangement's unnamed cost-bearers:
 *   international law addresses states, not peoples, so those who pay were
 *   never parties to the bargain. KEY AGENTS (by structural relationship): -
 *   great_power_governments: Agenda setter (institutional/arbitrage) —
 *   administers the regime and is selectively exempt from it; -
 *   authoritarian_regime_leadership: Primary beneficiary
 *   (powerful/identity_locked) — collects the domestic-impunity shield; -
 *   small_state_governments: Coordination beneficiary
 *   (moderate/identity_locked) — protected sphere against larger neighbors; -
 *   populations_under_repressive_regimes: Primary target (powerless/trapped)
 *   — bears the arrangement's costs; - persecuted_minority_groups: Target
 *   (powerless/trapped) — protection placed beyond external reach; -
 *   human_rights_movement: Excluded voice (organized/constrained) — its core
 *   claim is what the arrangement excludes; -
 *   international_courts_and_tribunals: Analytical observer
 *   (analytical/analytical) — shapes the rule's edges, cannot reach its core.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.65).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty: Unconditional Domestic Authority and Categorical Non-Intervention").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "political/legal/international-governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'c020caed-ee64-4a42-9875-4991a219b739').
narrative_ontology:cs_kernel_codification('c020caed-ee64-4a42-9875-4991a219b739', formalized).
narrative_ontology:cs_authority_grounding('c020caed-ee64-4a42-9875-4991a219b739', distributed).
narrative_ontology:cs_reading_relation('c020caed-ee64-4a42-9875-4991a219b739', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('c020caed-ee64-4a42-9875-4991a219b739', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('c020caed-ee64-4a42-9875-4991a219b739', foundational, domestic_jurisdiction_exclusive_and_unconditional).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive_and_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('c020caed-ee64-4a42-9875-4991a219b739', domestic_jurisdiction_exclusive_and_unconditional, conventional).
narrative_ontology:cs_axiom('c020caed-ee64-4a42-9875-4991a219b739', secondary, external_interference_per_se_illegitimate).
narrative_ontology:cs_axiom_status(external_interference_per_se_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c020caed-ee64-4a42-9875-4991a219b739', external_interference_per_se_illegitimate, conventional).
narrative_ontology:cs_reference_frame('c020caed-ee64-4a42-9875-4991a219b739', absolute_nonintervention_order).
narrative_ontology:cs_drift_state('c020caed-ee64-4a42-9875-4991a219b739', contemporary_post_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c020caed-ee64-4a42-9875-4991a219b739', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leadership).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, great_power_governments).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, small_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, persecuted_minority_groups).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, domestic_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, sovereign_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent veto-wielding seats in the Security Council and shape customary international law through state practice. They wrote the domestic-jurisdiction clause and decide case by case when intervention proceeds anyway: their own cross-border operations advance when retaliation is unlikely, while they invoke non-intervention defensively against rivals. Their power lets them breach the rule at low cost in ways smaller states cannot, so they arbitrage between invoking the guarantee and exempting themselves from it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, great_power_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Govern through domestic coercion and depend on the guarantee that no external actor may lawfully challenge how they treat their population. They invoke non-intervention in every international forum, block enforcement action through alliances and vetoes where available, and frame external criticism as aggression. Their standing as legitimate sovereigns is constituted by the rule they defend; abandoning it would expose them to the remedies it forecloses, so exit is unthinkable from where they stand.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leadership, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leadership, agenda_setter).

% Receive the arrangement's core protection: a guaranteed sphere no larger neighbor may lawfully enter. For states without military depth, the non-intervention guarantee is the principal asset the international order provides, and sovereign equality is the identity they trade on diplomatically. They comply scrupulously because reciprocity is their only enforcement tool, and they resist dilution of the guarantee even when distressed peers are the ones asking for it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, small_state_governments, beneficiary,
    moderate, generational, identity_locked, national).

% Live under governments whose treatment of them is legally insulated from outside challenge. They cannot appeal to foreign courts, international bodies lack jurisdiction over their government's domestic conduct without state consent, and emigration is often restricted precisely to prevent departure. Their available remedies are internal, which means the same institutions their government controls.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes, payer,
    powerless, biographical, trapped, national).

% Face state-directed persecution that the non-intervention guarantee places beyond external reach. Protection historically arrived through intervention or imposed minority treaties, both of which the current arrangement renders illegitimate absent Security Council consensus their persecutors can veto. Group survival strategies reduce to flight, concealment, or internal accommodation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, persecuted_minority_groups, payer,
    powerless, generational, trapped, regional).

% Documents abuses, petitions treaty bodies, and argues for external remedy channels, but holds no vote in the forums where the non-intervention rule is maintained. Its core claim, that systematic abuse forfeits the shield, is precisely the proposition the arrangement excludes from legal recognition, so its participation is confined to documentation, naming, and marginal procedural openings.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_movement, excluded,
    organized, generational, constrained, global).

% Adjudicate sovereignty disputes only when states consent to jurisdiction, articulate the content of non-intervention and its exceptions, and occasionally prosecute individuals for atrocities under complementary statutes. They cannot reach inside a state without cooperation, so their doctrinal output shapes the rule's edges while its core remains politically maintained.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_courts_and_tribunals, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leadership).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts interstate anarchy into stable mutual restraint: each state receives a guaranteed sphere of exclusive domestic authority, removing the historical drivers of interstate war (dynastic, religious, ideological, and imperial intervention into other states' internal constitutions) and making diplomacy, trade, and treaty relations possible among radically unequal states.
% TRANSFER_FUNCTION: Moves immunity from external challenge to state executives: every government receives a protected sphere over its territory and population. The corresponding cost falls on the people inside states, whose treatment is thereby withdrawn from external scrutiny, remedy, or appeal. Security and status flow upward to governments; accountability for domestic conduct is taken from populations.
% ABSENT_VOICES: Populations under repressive rule hold no seat: international law addresses states, not peoples, so those bearing the arrangement's costs were never parties to its codification. Stateless nations, occupied populations, and dissident movements stand outside the state-centric frame entirely; human rights organizations attend as observers without vote.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen intervention as a routine instrument: small states would immediately seek great-power patrons or independent deterrents, regional hegemons would test neighbors, minority-protection and refugee channels would flood, and interstate bargaining would reorganize around explicit spheres of influence. Something functionally equivalent would likely be rebuilt quickly because the demand it meets is real, but the transition itself would be violent.
% FOUNDING_PROBLEM: Ending the European wars fought over the internal religious and constitutional order of other states, and, in the UN Charter recodification, ending the great-power intervention competition of the imperial era by guaranteeing each state an untouchable domestic sphere.
% FOUNDING_PROBLEM_CORROBORATION: Interstate-war historians and international-relations scholars (including the organized-hypocrisy literature) attest that the interstate-stability problem remains live and that the norm manages it; small-state diplomatic practice attests the dependence daily. On the domestic-shielding function, corroboration runs the other way: human rights organizations and universal-jurisdiction prosecutors, sources outside the beneficiary set, attest that the shield now does more work for abusive executives than for interstate peace. No source outside state executives attests that the domestic-shield function serves anyone but the executives.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.55) reflects a split structure: the interstate-restraint half delivers real, still-load-bearing coordination, while the domestic-shield half concentrates impunity on executives. The referent is the standing absolute-non-interference arrangement itself, not any alternative arrangement. Suppression (0.65) is authored as a raw structural property and is not scaled by power or scope: the closure of external recourse (no standing, no forum, no remedy without state consent) is what the number measures; the engine scales only extractiveness. Theater (0.30): sovereign-equality ritual (one-state-one-vote assemblies, formal protest exchanges) masks power asymmetry, but the war-prevention work is genuine, so the performative share stays below half. Accessibility collapse (0.40): understanding the arrangement does not collapse alternatives, since humanitarian channels, universal jurisdiction, sanctions, and rival doctrinal moves remain visible and partly operable; it prices them as illegitimate, which is a partial rather than total collapse. Resistance (0.60): sustained doctrinal challenge, ad hoc tribunals, and intervention coalitions meet the norm continuously. Seat divergence: the small-state beneficiary seat computes toward coordination (its protection is real and its exit is identity-locked), the authoritarian-executive seat computes toward subsidized extraction (full shield, negligible cost side), and the trapped population seats compute toward the coercive end (full-cost bearing, zero exit). Cyclical pattern: the shared-grid series traces one full geopolitical cycle: Cold War entrenchment (suppression peaking at t30), post-Cold War interventionist opening (erosion through t60), and the Libya-backlash restoration (re-hardening to t80). The oscillation is enforcement politics rather than noise, and the restoration phase functions as intermittent reinforcement: each re-hardening re-legitimates the impunity accumulated during the opening. Coalition note: the powerless victim seats hold latent coalition potential (diaspora litigation, transnational advocacy), but the arrangement's exclusion of non-state standing is maintained against exactly that coalition route.
 *
 * PERSPECTIVAL GAP:
 *   Three seats inhabit three different arrangements under one name. From the small-state seat this is life insurance: the single asset the international order guarantees a state with no military depth. From the authoritarian-executive seat it is an entitlement and an instrument: a rule to invoke in every forum and a wall behind which domestic conduct is nobody else's business. From the population-under-repression seat it is enforced isolation: the legal form of having no one to call. The engine computes these divergences from power and exit data; the authored claim does not adjudicate them. Inter-institutionally, the great-power seat experiences the same rule as a discretionary instrument, binding when convenient and exempt when not, which is why its directionality is overridden away from the pure-beneficiary reading its declaration would otherwise produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Authoritarian regime leadership: declared beneficiary with identity-locked exit, deriving a d near the beneficiary end; the shield subsidizes them directly. Small-state governments: declared beneficiary, likewise identity-locked, deriving near-full subsidy; their protection is the arrangement's genuine coordination dividend. Great-power governments: declared beneficiary, but the derivation from the declaration alone would understate their cost side (reciprocity binds their cross-border operations, and selective exemption carries prestige and precedent costs), so an explicit override sets d to 0.25, nearer symmetric, reflecting administered privilege rather than pure receipt. Populations under repressive regimes and persecuted minority groups: declared victims, powerless, trapped, deriving d near the full-target end; their trapped exit sits them at the extreme of the target range. The norm's global spatial scope raises verification difficulty, which the engine folds into modestly amplified effective extraction on the target seats. Suppression enters the computation unscaled, as a structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is declared: the founding problem (interstate war driven by intervention into domestic orders) remains live, and the arrangement still performs that function. The classification work here is preventive in both directions. Reading the arrangement as pure coordination erases the trapped population seats, the people who pay for the executives' shield, and would certify a protection racket as a public good. Reading it as pure extraction erases the genuine war-prevention service that small states, whose only enforcement tool is reciprocity, depend on daily. The tangled-rope structure keeps both halves on the books and makes the drift question tractable: if the coordination half (collective security, dispute settlement) stays deadlocked while the shield half hardens, the arrangement slides toward pure extraction, and the founding-problem-status-by-disappearance mismatch together with the theater path should catch the slide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the absolute_sovereignty reading of the westphalian_sovereignty kernel: what structural changes would the sibling readings (conditional_sovereignty, graduated_sovereignty) introduce if adopted?',
    'Compile the sibling stories and diff the structural surfaces: victim sets, exit options for the population seats, beneficiary coverage, and enforcement gates. Divergence localizes the disagreement to the conditionality axis of the sovereign grant.',
    'Under the conditional reading, populations gain an external remedy channel (exit shifts from trapped toward constrained) and abusive executives lose shield coverage upon trigger; under the graduated reading, the shield becomes capacity-contingent and low-capacity abusive executives drop out of the beneficiary set. Either adoption lowers measured extraction and reshapes the beneficiary declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-of-kernel identity and sibling structural deltas.').

omega_variable(
    organized_hypocrisy_vs_sincere_restraint,
    'Is observed compliance with non-intervention driven by sincere mutual restraint or by instrumental invocation tracking power asymmetry (the organized-hypothesis reading)?',
    'Code intervention and non-intervention episodes against power differentials: if compliance rates track the intervening state''s exposure to retaliation rather than norm acceptance, the hypocrisy reading dominates.',
    'The hypocrisy reading raises the theater ratio attributable to the agenda-setter seat and relocates effective extraction from the norm itself to its selective administration; the sincere-restraint reading keeps extraction located in the norm''s domestic-shield effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_hypocrisy_vs_sincere_restraint, empirical, 'Whether the norm''s operation is sincere coordination or power-tracking instrumentalism.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the closed-off external recourse of populations under repressive rule structural (no legal standing, no accessible forum) or partly internalized (populations treat non-interference as the natural order and do not petition outward)?',
    'Observe claim generation where channels open: jurisdictions that acquired universal-jurisdiction reach, and diaspora-led litigation, reveal whether suppressed demand exists behind the closed channels.',
    'If substantially internalized, removing the structural barrier would not immediately release demand for external remedy, and the measured suppression overstates the barrier''s current binding force; if structural, channel-opening would produce rapid claim surges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of the suppression measure between legal-structural closure and internalized acceptance.').

omega_variable(
    founding_function_attribution,
    'Was the arrangement built primarily to prevent interstate war (coordination design) or to protect rulers'' domestic control (extraction design)? The Westphalian settlement protected princes'' confessional authority; the UN Charter clause targeted great-power intervention.',
    'Historiographic comparison of the two codifications'' texts, negotiating records, and ratification debates, tracing which function each draft''s sponsors prioritized.',
    'If extraction-design dominates, the domestic shield is original purpose rather than parasitic drift, and the tangled-rope reading overstates the coordination half; if coordination-design dominates, the shield is accreted rent and the drift trajectory points toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_function_attribution, conceptual, 'Genealogy: whether the domestic-shield effect is design or drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.24).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.28).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.3).
narrative_ontology:measurement(west_tr_t40, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 40, 0.32).
narrative_ontology:measurement(west_tr_t50, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 50, 0.28).
narrative_ontology:measurement(west_tr_t60, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 60, 0.29).
narrative_ontology:measurement(west_tr_t70, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 70, 0.31).
narrative_ontology:measurement(west_tr_t80, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(west_be_t40, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(west_be_t50, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(west_be_t60, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(west_be_t70, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 70, 0.54).
narrative_ontology:measurement(west_be_t80, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 80, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(west_su_t40, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(west_su_t50, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(west_su_t60, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(west_su_t70, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 70, 0.6).
narrative_ontology:measurement(west_su_t80, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 80, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, graduated_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'Westphalian sovereignty' decomposes into three structurally distinct claims along one axis: the conditionality of the sovereign grant. This file instantiates the absolute reading (unconditional grant, categorical non-interference); conditional_sovereignty and graduated_sovereignty instantiate the sibling claims with different victim sets, exit structures, and beneficiary coverage, hence different epsilon values. Family members link via affects_constraints. Citation runs upstream from this reading (Charter text, customary law) toward the siblings: R2P doctrine and capacity-based governance frameworks cite the absolute baseline they modify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
