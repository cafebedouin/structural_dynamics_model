% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Unratified Custom Enforced by Naval Presence
 *   domain: international law / maritime governance / geopolitical strategy
 *
 * SUMMARY:
 *   The arrangement under contest: naval powers — principally the United
 *   States, which has never ratified UNCLOS — assert that freedom of
 *   navigation is customary international law binding all states regardless
 *   of treaty accession, and maintain it by scheduled warship transits
 *   (FONOPs) through waters whose jurisdiction coastal states claim
 *   exclusively. The regime solves a real commons problem (open lanes for
 *   global trade) while embedding a structural asymmetry: the enforcing
 *   powers enjoy the regime's rights without its obligations, define which
 *   claims count as excessive through their own limits briefs, and stand
 *   outside the adjudicative institutions that review everyone else. KEY
 *   AGENTS (by structural relationship): blue_water_naval_powers —
 *   agenda-setter and primary beneficiary (institutional/arbitrage);
 *   global_shipping_interests — beneficiary (organized/mobile);
 *   eez_exclusivity_claimant_states — primary target (powerful/constrained);
 *   chokepoint_littoral_states — target (moderate/trapped);
 *   maritime_dispute_tribunals — observer (institutional/analytical);
 *   small_coastal_states_without_enforcement_capacity — excluded
 *   (powerless/trapped). CONSTRAINT FAMILY NOTE (epsilon decomposition): the
 *   colloquial label 'law of the sea / maritime sovereignty dispute'
 *   conflates three structurally distinct claims with different epsilon and
 *   different victim sets — strict_eez_reading (treaty-text exclusivity; the
 *   transiting user is the extracting seat), historical_rights_reading
 *   (pre-treaty usage overrides zones; victim sets overlap both siblings),
 *   and this reading (asserted custom enforced by presence; victims are
 *   exclusivity claimants and chokepoint littorals, beneficiaries are naval
 *   powers and shipping). This story authors epsilon only for the standing
 *   arrangement as this reading assesses it — the unratified-enforcement
 *   regime as practiced — never for the sibling readings' arrangements. The
 *   claim (tangled_rope) and the metrics are authored independently: the
 *   metrics describe the operation as it runs, including its substantial
 *   extraction and hardening enforcement.
 *
 * KEY AGENTS:
 *   - blue_water_naval_powers: Agenda-setter and primary beneficiary (institutional/arbitrage) — publishes the limits briefs defining 'excessive' claims, schedules and executes enforcement patrols, and declines the treaty whose rules it enforces
 *   - global_shipping_interests: Beneficiary (organized/mobile) — carries the trade the open-lane regime secures and pays nothing into the enforcement that secures it
 *   - eez_exclusivity_claimant_states: Primary target (powerful/constrained) — asserts expansive jurisdiction, absorbs the sovereignty cost of challenged claims, cannot exit geography or escalate without general war
 *   - chokepoint_littoral_states: Target (moderate/trapped) — hosts passages whose transit rules were settled over their objection; cannot move the strait
 *   - maritime_dispute_tribunals: Observer (institutional/analytical) — adjudicates claims under the convention while the principal enforcer stands outside their jurisdiction
 *   - small_coastal_states_without_enforcement_capacity: Excluded (powerless/trapped) — hold identical entitlement kinds with no capacity to police them; their practice never shapes the customary record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.7).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Unratified Custom Enforced by Naval Presence").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international law / maritime governance / geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '38d30495-2491-4ed9-ac52-a3b7af8e41d0').
narrative_ontology:cs_kernel_codification('38d30495-2491-4ed9-ac52-a3b7af8e41d0', fixed_text).
narrative_ontology:cs_authority_grounding('38d30495-2491-4ed9-ac52-a3b7af8e41d0', practice).
narrative_ontology:cs_interpretation_layer_present('38d30495-2491-4ed9-ac52-a3b7af8e41d0').
narrative_ontology:cs_reading_relation('38d30495-2491-4ed9-ac52-a3b7af8e41d0', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('38d30495-2491-4ed9-ac52-a3b7af8e41d0', unclos_sovereignty_boundary__historical_rights_reading, influences).
narrative_ontology:cs_axiom('38d30495-2491-4ed9-ac52-a3b7af8e41d0', foundational, navigation_liberty_binds_independent_of_ratification).
narrative_ontology:cs_axiom_status(navigation_liberty_binds_independent_of_ratification, holdable).
narrative_ontology:cs_axiom_grounding('38d30495-2491-4ed9-ac52-a3b7af8e41d0', navigation_liberty_binds_independent_of_ratification, conventional).
narrative_ontology:cs_axiom('38d30495-2491-4ed9-ac52-a3b7af8e41d0', foundational, naval_presence_constitutes_lawful_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_constitutes_lawful_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('38d30495-2491-4ed9-ac52-a3b7af8e41d0', naval_presence_constitutes_lawful_enforcement, instrumental).
narrative_ontology:cs_reference_frame('38d30495-2491-4ed9-ac52-a3b7af8e41d0', customary_navigation_liberty_baseline).
narrative_ontology:cs_drift_state('38d30495-2491-4ed9-ac52-a3b7af8e41d0', contemporary_great_power_competition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38d30495-2491-4ed9-ac52-a3b7af8e41d0', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_interests).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, eez_exclusivity_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, chokepoint_littoral_states).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, mare_liberum_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_law_binding_nonparties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes annual limits briefs designating which coastal claims count as excessive, schedules and executes freedom-of-navigation patrols against them, and declines ratification of the convention whose rules it enforces — retaining discretion over which obligations bind it. Collects unrestricted transit, intelligence-collection access inside foreign zones, and strategic mobility; carries the fiscal cost of patrol tempo and the escalation risk of each challenge.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers, beneficiary).

% Moves the overwhelming share of world traded tonnage by sea and depends on uniform transit rules and open chokepoints. Pays nothing into the enforcement regime that secures its lanes beyond ordinary freight and insurance markets; can reroute around a closed zone only at substantial fuel, time, and premium cost.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_interests, beneficiary,
    organized, immediate, mobile, global).

% Asserts exclusive resource and regulatory jurisdiction to 200 nautical miles and beyond via straight baselines, historic-bay closures, or overlay lines. Finds its claims entered into enforcement briefs as excessive and challenged by transiting warships. Cannot abandon the claims without domestic political collapse and cannot expel the challenging navies without general war; absorbs the sovereignty cost while its fishing and energy zones remain open to foreign military activity.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, eez_exclusivity_claimant_states, payer,
    powerful, generational, constrained, regional).

% Sits astride straits and archipelagic passages whose transit regimes were negotiated over its objection or before it had capacity to contest them. Hosts the enforcement patrols it did not invite; cannot move its coastline and cannot close the passage without triggering the enforcement machinery. Receives transit-safety benefits but subordinates its environmental, security, and toll-setting preferences to passage freedom.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, chokepoint_littoral_states, payer,
    moderate, biographical, trapped, regional).

% Adjudicates maritime claims under the convention's compulsory procedures, most prominently the 2016 arbitral award rejecting historic-rights overlay claims in the South China Sea. The principal enforcing power stands outside its jurisdiction by virtue of non-ratification, so its rulings bind claimant respondents while leaving the enforcer's own practice unreviewable; it publishes findings that neither fleet obeys nor ignores outright.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_dispute_tribunals, observer,
    institutional, generational, analytical, global).

% Holds maritime entitlements identical in kind to the great-power claimants' but lacks any capacity to police or defend them. Its state practice never enters the record from which customary content is compiled, and its objections to unilateral enforcement carry no fleet behind them; it inherits whichever transit rules the enforcing powers and great-power claimants settle between themselves.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_coastal_states_without_enforcement_capacity, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains uniform, predictable access to the global commons of sea lanes and chokepoints — solving the collective-action problem in which any single coastal state's closure, toll, or exclusion zone imposes costs on every trading nation, and in which no merchant fleet can secure passage rights alone.
% TRANSFER_FUNCTION: Moves jurisdictional control over adjacent waters from claimant coastal states to transiting naval and commercial traffic; moves the cost of maintaining passage from the trading public onto enforcing navies' defense budgets; moves the compliance burden onto claimant states, who must litigate or militarize to make their claims effective.
% ABSENT_VOICES: Small coastal states without enforcement capacity — their practice never shapes what counts as customary, yet the rules compiled from others' practice bind their waters. The tribunals' authority is declined by the principal enforcer, so the adjudicative voice is structurally half-present. Claimant-state publics, for whom exclusive-zone resources are development assets, have no seat in the limits-brief process that designates their claims excessive.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, sea-lane access would revert to bilateral bargain and zone-by-zone closure: hull insurance would reprice chokepoint risk within weeks, claimant states would consolidate exclusive control over fisheries and hydrocarbons, naval powers would negotiate access treaty-by-treaty or contest closure by force, and the uniform transit rules that roughly ninety percent of traded tonnage relies on would fragment into dozens of jurisdictional patches.
% FOUNDING_PROBLEM: Post-war creeping jurisdiction: the Truman Proclamation of 1945 and successive 200-mile and cannonball decrees threatened to partition the oceans into exclusive national zones; a universal twelve-mile territorial sea would have closed straits narrower than twenty-four miles; Cold War submarine and carrier mobility required guaranteed transit. The 1979 formalization of the freedom-of-navigation program was built to stop excessive claims from closing the commons.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the beneficiary set: the 2016 South China Sea arbitral award — obtained at the claimant state's own initiative — found specific historic-rights claims incompatible with the convention, corroborating that excessive-claim disputes are real; shipping-industry chokepoint analyses corroborate the access problem's economic weight. Not corroborated: the enforcers' designation of particular claims as excessive rests on their own limits briefs; claimant states categorically deny the characterization, and no body the enforcing powers accept as authoritative attests their selection criteria.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the regime transfers real jurisdictional value from claimant states to transiting traffic, and the enforcer's non-ratification exempts it from the reciprocal obligations (dispute settlement, seabed regime, deep-pocket liability) that make the transfer symmetrical for everyone else — but the transfer also underwrites a coordination function claimant states themselves depend on for their exports, so epsilon sits well below snare range. Suppression 0.70: the enforcement instrument is warship presence; the alternative open to a challenged claimant is escalation against a superior navy, which is prohibitive for all but one claimant — alternatives persist (litigation, coalition diplomacy, fait accompli construction) but at heavy cost, hence accessibility_collapse 0.52 rather than mountain-range collapse. Resistance 0.68: island-building, coast-guard shadowing, historic-lines declarations, and allied ratification pressure are active, organized resistance, not acquiescence. Theater_ratio 0.38: a growing share of enforcement activity is signaling — single-destroyer transits past features whose operational effect is nil but whose declaratory value feeds a rivalry audience — while the underlying lane-keeping function remains real. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity intensification (patrol tempo rose from a handful of annual operations to dozens; the coercive backdrop shifted from policing weak claimants to confronting a nuclear peer), which is exactly the dynamic the scalar base_properties.suppression cannot carry alone. All three tracked series run on one shared six-point grid (t=0..45, mapping 1979-2024) so every metric is authored at every examined time point; end-state values correspond to the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as the subsidized maintenance of a lawful commons — it pays the patrol bills and reads its own limits briefs as neutral legal judgment. The payer seats experience the same structure as jurisdictional dispossession delivered by warship. Among payers the experience diverges further: the great-power claimant can answer each challenge with dredged runways and missile batteries, converting humiliation into capability, while the trapped chokepoint littoral experiences the same transit as permanent subordination with no answer available at any price. The excluded seat experiences no constraint at all — only its results, rules compiled from practice it never contributed to. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed. blue_water_naval_powers combine agenda-setting with beneficiary position and arbitrage-grade exit (they wrote themselves out of the treaty while keeping its benefits), placing them nearest the beneficiary pole. global_shipping_interests are pure beneficiaries with mobile exit — rerouting is costly but real — sitting close behind. eez_exclusivity_claimant_states are declared victims with constrained exit (geography is fixed, escalation is war), placing them near the target pole; chokepoint_littoral_states, declared victims with trapped exit, sit nearest the full-target end. The tribunals take the analytical seat. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation, which is why the global-scope enforcement amplifies effective extraction for the trapped and constrained payer seats most of all.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Collapsing the structure to rope would erase the non-ratifier asymmetry that is this reading's defining feature — rights without obligations, enforcement without submission to adjudication — and launder a coercive transfer as mere coordination. Collapsing it to snare would erase the genuine chokepoint-coordination function that claimant states' own export economies consume daily, and would misread commercially indispensable lane-keeping as pure cover. The founding problem remains live (each decade produces new excessive-claim forms: straight baselines, air-defense identification zones, artificial-island arcs), so no scaffold sunset is available and no piton inertia claim applies — the arrangement persists because enforcement is actively purchased every year, not because anyone forgot why it exists. Mandatrophy is therefore unresolved by design, and the R5 status x verdict pairing (live x world_rearranges) correctly flags no zombie condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel unclos_sovereignty_boundary (reading: non_ratifier_enforcement_reading). Which sibling reading''s entitlement structure ultimately governs — asserted custom enforceable by presence (this reading), exclusive treaty zones (strict_eez_reading), or pre-treaty historical rights (historical_rights_reading)?',
    'Institutional settlement signals: universal ratification with the former non-parties inside a compulsory-adjudication regime resolves toward strict_eez; sustained effective occupation plus general acquiescence resolves toward historical_rights; continued unratified enforcement with broad commercial reliance resolves toward this reading.',
    'If strict_eez_reading prevails institutionally, this reading''s enforcement loses its legal warrant and recomputes as coercion with the naval powers recast as targets; if historical_rights_reading prevails, the victim set inverts; if this reading persists, the current tangled_rope structure with its present beneficiary/victim split holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading''s entitlement structure governs the sovereignty-boundary kernel.').

omega_variable(
    custom_by_assertion_status,
    'Is freedom of navigation established custom (widespread state practice plus opinio juris) or custom manufactured by the enforcing powers'' own fleet movements — asserted as law and protested, rather than accepted, by much of the coastal world?',
    'Systematic coding of state practice and diplomatic protest archives: acquiescence ratios by region and by naval capability, separating the practice of states that could enforce the norm from states that merely endure it.',
    'If the custom is largely self-manufactured, the reading''s warrant collapses and the constraint drifts toward snare (enforcement without accepted norm); if acquiescence is broad and independent, the coordination framing strengthens and the measured extraction reads increasingly as enforcement cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_by_assertion_status, empirical, 'Whether the customary-law foundation is genuine consensus or enforced assertion.').

omega_variable(
    challenge_selection_neutrality,
    'Are enforcement target selections driven by objective excessiveness criteria or by strategic rivalry — is the same claim challenged when held by an adversary and tolerated when held by an ally?',
    'Comparative dataset matching claim types against enforcement schedules over the full program history, controlling for alliance status and claimant power.',
    'Demonstrated selection bias converts the coordination function into selective coercion: the extraction component grows, per-seat divergence widens, and the rope-half of the tangled structure thins; demonstrated neutrality supports the coordination reading and stabilizes the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_selection_neutrality, empirical, 'Whether enforcement selection tracks principle or rivalry.').

omega_variable(
    free_rider_reciprocity_pressure,
    'Does the enforcer''s non-ratification exemption persist indefinitely, or does accumulating allied and institutional pressure eventually force reciprocity — accession or an equivalent binding commitment?',
    'Track ratification-pressure episodes, allied diplomatic conditioning, and any reciprocal claim-submission or adjudication-consent by the enforcing powers over the coming decade.',
    'Persistent exemption hardens the rights-without-obligations asymmetry and drives drift toward snare; emergent reciprocity rebalances the structure toward rope and lowers effective extraction for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_reciprocity_pressure, empirical, 'Whether the asymmetry between enforcing and enforced is stable or self-correcting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uncl_tr_t9, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(uncl_tr_t18, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(uncl_tr_t27, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 27, 0.25).
narrative_ontology:measurement(uncl_tr_t36, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(uncl_tr_t45, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 45, 0.38).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t9, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement(uncl_be_t18, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(uncl_be_t27, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 27, 0.54).
narrative_ontology:measurement(uncl_be_t36, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement(uncl_be_t45, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 45, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uncl_su_t9, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(uncl_su_t18, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(uncl_su_t27, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 27, 0.6).
narrative_ontology:measurement(uncl_su_t36, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 36, 0.66).
narrative_ontology:measurement(uncl_su_t45, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 45, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'law of the sea / maritime sovereignty dispute,' per the epsilon-invariance principle. The label conflates three structurally distinct claims: strict_eez_reading (treaty-text exclusivity; extracting seat is the transiting/naval user; epsilon lowest where treaty institutions function), historical_rights_reading (pre-treaty usage overrides zones; victim set overlaps both siblings; epsilon indexed to occupation facts), and this reading (asserted custom enforced by presence; victims are exclusivity claimants and chokepoint littorals; beneficiaries are naval powers and shipping). Citation runs in both directions across the family: this reading cites the treaty order's universality gap to justify enforcement, while strict_eez_reading cites this reading's unilateralism as evidence the treaty order needs stronger institutional teeth. Each member is authored as a separate story with its own epsilon, stakeholders, and classification; all are linked here per family discipline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
