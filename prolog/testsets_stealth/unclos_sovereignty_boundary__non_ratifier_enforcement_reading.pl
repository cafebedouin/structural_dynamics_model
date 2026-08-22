% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Customary Freedom of Navigation Enforced by Non-Ratifier Naval Presence
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the UNCLOS sovereignty-boundary
 *   kernel: the claim that freedom-of-navigation principles constitute
 *   customary international law binding even on states that never ratified
 *   the convention, enforceable through recurring naval presence operations.
 *   Under this reading, the enforcing naval power enters the beneficiary set
 *   (it administers and collects from a rule it never accepted as treaty
 *   text), coastal states pursuing exclusive-zone control enter the victim
 *   set, and the constraint's authority decouples from ratification entirely
 *   — resting on asserted custom plus demonstrated capability. The
 *   claim/metric gap is deliberate and load-bearing: the reading CLAIMS
 *   legitimate rule-maintenance (its own self-understanding), while the
 *   authored metrics describe a substantially extractive, actively enforced
 *   arrangement whose asymmetry has grown over four decades. The engine
 *   measures that divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - forward_deployed_naval_powers: Primary beneficiary and agenda-setter (institutional/arbitrage) — selects and executes challenges, collects access and precedent, bears no treaty reciprocity
 *   - expansive_maritime_claimants: Primary target (powerful/identity_locked) — absorbs enforcement pressure, cannot retreat without sovereign-narrative rupture
 *   - smaller_coastal_claimants: Secondary target (moderate/constrained) — pays diplomatic friction, collects the same commerce the challenges protect
 *   - global_merchant_shipping: Free-rider beneficiary (organized/mobile) — receives open lanes, contributes nothing to enforcement
 *   - unclos_state_parties: Obligation-bearing beneficiaries (organized/constrained) — carry treaty burdens the enforcer avoids while sharing the navigation good
 *   - annex_vii_tribunals: Excluded adjudicator (institutional/analytical) — would settle these disputes; the enforcer accepts no forum
 *   - maritime_legal_academy: Analytical observer — supplies doctrine to every seat, holds no stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom of Navigation Enforced by Non-Ratifier Naval Presence").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '2646d939-857a-4bd7-a51d-41e61a40e1bc').
narrative_ontology:cs_kernel_codification('2646d939-857a-4bd7-a51d-41e61a40e1bc', fixed_text).
narrative_ontology:cs_authority_grounding('2646d939-857a-4bd7-a51d-41e61a40e1bc', practice).
narrative_ontology:cs_interpretation_layer_present('2646d939-857a-4bd7-a51d-41e61a40e1bc').
narrative_ontology:cs_reading_relation('2646d939-857a-4bd7-a51d-41e61a40e1bc', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('2646d939-857a-4bd7-a51d-41e61a40e1bc', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_axiom('2646d939-857a-4bd7-a51d-41e61a40e1bc', foundational, customary_fon_binds_non_parties).
narrative_ontology:cs_axiom_status(customary_fon_binds_non_parties, holdable).
narrative_ontology:cs_axiom_grounding('2646d939-857a-4bd7-a51d-41e61a40e1bc', customary_fon_binds_non_parties, conventional).
narrative_ontology:cs_axiom('2646d939-857a-4bd7-a51d-41e61a40e1bc', foundational, naval_presence_lawful_custom_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_lawful_custom_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('2646d939-857a-4bd7-a51d-41e61a40e1bc', naval_presence_lawful_custom_enforcement, instrumental).
narrative_ontology:cs_reference_frame('2646d939-857a-4bd7-a51d-41e61a40e1bc', customary_navigation_freedom_baseline).
narrative_ontology:cs_drift_state('2646d939-857a-4bd7-a51d-41e61a40e1bc', contemporary_multipolar_maritime_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2646d939-857a-4bd7-a51d-41e61a40e1bc', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, forward_deployed_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_merchant_shipping).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, expansive_maritime_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, smaller_coastal_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, smaller_coastal_claimants).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_state_parties).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_state_parties).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, mare_liberum_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the freedom-of-navigation program: publishes the limits-claims digest, selects which maritime claims to challenge each year, schedules and executes the transits, and issues the accompanying diplomatic statements. Collects operational access, precedent value, and alliance reassurance from every completed challenge. Has not ratified the underlying convention, so sits outside its compulsory dispute settlement and seabed-cost machinery while asserting the convention's navigation core as binding on everyone.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, forward_deployed_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, forward_deployed_naval_powers, beneficiary).

% Coastal great power asserting historic-rights and expansive-zone control over contested waters. Responds to each challenge transit with shadowing vessels, radio warnings, diplomatic protests, and physical construction that hardens the physical claim. Backing down would require renouncing claims that are fused with the national sovereignty narrative and domestic legitimacy, so retreat is politically unavailable even where the legal position is weak.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, expansive_maritime_claimants, payer,
    powerful, generational, identity_locked, regional).

% Mid-sized coastal states whose straight-baseline systems, security zones, or prior-notification requirements appear on the challenge list. They lack the naval capacity to contest presence operations, so they file protests and absorb the diplomatic friction. Simultaneously their economies run on the same open sea lanes the challenges keep open, so they collect the commerce benefit while paying the sovereignty cost.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, smaller_coastal_claimants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, smaller_coastal_claimants, beneficiary).

% Container lines, bulk carriers, and tankers moving the large majority of seaborne trade through straits, archipelagic passages, and exclusive economic zones under the asserted freedoms. They contribute nothing to the enforcement effort, reroute at their own cost when closure risk spikes, and see war-risk insurance premiums track the level of great-power friction in contested waters.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_merchant_shipping, beneficiary,
    organized, immediate, mobile, global).

% States that ratified the convention and carry its reciprocal burdens: seabed-regime contributions, dispute-settlement exposure, reporting duties. They receive the same navigation freedoms the non-party enforcer asserts, and cannot withhold those freedoms from the non-ratifier without undermining the very custom they rely upon for their own commerce.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_state_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_state_parties, beneficiary).

% Arbitral panels and tribunals constituted under the convention would adjudicate exactly the claims the program challenges. Coastal states have brought cases before them, but the enforcing power accepts neither ratification nor compulsory jurisdiction, so the tribunals hear challenges to others' conduct while the challenger answers to no forum. They are kept outside the conversation about the enforcement itself.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, annex_vii_tribunals, excluded,
    institutional, civilizational, analytical, global).

% Scholars of the law of the sea who track the customary-status debate, publish the source-hierarchy analyses and practice surveys that both sides cite, and annotate each new challenge digest. They hold no stake in enforcement outcomes and supply the doctrinal ammunition for every seat.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_legal_academy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, forward_deployed_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the world's sea lanes open and predictable — strait transit, archipelagic sea-lane passage, navigation and overflight through exclusive economic zones — solving the collective-action problem in which any single coastal state's closure, toll, or prior-consent regime would impose costs on every other trading nation at once.
% TRANSFER_FUNCTION: Moves effective maritime access from coastal claimant states to naval powers: each sustained challenge converts a contested zone back into operational commons for the challenging navy and its allies. It simultaneously moves compliance pressure, response-deployment costs, and diplomatic capital onto the challenged state, and accumulates precedent value for the enforcing fleet.
% ABSENT_VOICES: Coastal fishing communities whose grounds sit inside militarized response zones; smaller littoral publics with no navy whose governments absorb the friction silently; and the enforcing power's own domestic public, which was never asked to ratify the treaty obligations its navy enforces on others. None of these seats is inside the program's annual challenge-selection loop.
% DISAPPEARANCE_RATIONALE: If the enforcement practice vanished overnight, claim settlement would shift entirely to bilateral negotiation weighted by local power: strait states would test tolls and notification regimes, expansive claimants would consolidate their zones physically, insurers would reprice entire route corridors, and allied navies would either build a replacement coalition or accept restricted charts. The shipping economy would visibly reorganize.
% FOUNDING_PROBLEM: In the late 1970s, coastal-state jurisdictional claims were proliferating rapidly — straight baselines, 200-mile zones, strait-state prior-notification regimes — threatening to fragment the oceans into closed national segments at precisely the moment superpower naval mobility and the global tanker trade made uninterrupted passage existential.
% FOUNDING_PROBLEM_CORROBORATION: Strait-dependent states such as Singapore and international shipping associations corroborate, from outside the naval beneficiary set, that open-passage problems remain live. Coastal-state coalitions in the G77 attest the opposite about the enforcement mode: that the founding problem of strait closure never warranted unilateral policing of military activities inside economic zones. No source outside the beneficiary set attests that the program's current scope matches its founding problem; the corroborating parties endorse the navigation principle, not the present enforcement footprint.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58: the coordination core is real (open lanes benefit even the challenged states' own trade), but the arrangement's defining asymmetry — a non-party enforcing treaty-derived norms without treaty obligations, increasingly into the treaty-silent zone of EEZ military activities — has accumulated steadily since 1979. Suppression is 0.62: persistence depends on coercive presence against states that cannot legally answer it anywhere, since the enforcer declines compulsory jurisdiction; the alternative channel (arbitration) exists but collapses against a non-consenting great power, hence accessibility_collapse at 0.45 rather than near-zero. Theater_ratio 0.42 reflects the growing share of operations that are demonstrative single-ship transits accompanied by press messaging rather than passages serving operational need. Resistance 0.65 captures active shadowing, formal protests, and physical claim-hardening. All three temporal series run on one shared eight-point grid (1979–2026) so no metric's row borrows another's end-state; trajectories are monotonic, not cyclical — enforcement ratchets up as challenged claimants grow capable enough to require larger response packages.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is rule-maintenance it performs at its own expense for a global good — a rope-like experience. From the identity-locked claimant seat the identical operations are coerced concessions extracted under gunsight — a snare-flavored experience. From the state-parties seat the salient fact is obligation asymmetry: they fund the seabed regime and expose themselves to tribunals while the enforcer does neither. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Forward-deployed naval powers sit nearest the beneficiary pole: they administer the arrangement and are subsidized by it (d low, amplified by arbitrage-grade exit — they choose which claims to challenge and which to overlook). Global merchant shipping is the purest free-rider (d lowest of all: full benefit, zero contribution, mobile exit). Expansive maritime claimants sit near the full-target pole (d high), with identity-lock pushing them further toward trapped: their claims are fused with national sovereignty narratives, so exit means narrative rupture. Smaller coastal claimants occupy a genuinely mixed seat — declared victims of the challenges yet simultaneous beneficiaries of the openness — which the dual role records. UNCLOS state parties derive mid-range with a payer tilt from their secondary role. Excluded tribunals and the analytical academy feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Calling this a rope would erase the extraction: the non-party enforcement asymmetry and the drift into the EEZ military-activities gap are real transfers from claimant states to the enforcing navy, not coordination overhead. Calling it a snare would erase the coordination good: the sea lanes stay open for everyone, including the victim seats' own exporters, and the founding problem (strait closure, baseline abuse) was genuine. On the genealogy interview, the founding problem is contested rather than dead — strait-closure risk persists — but the mandate has visibly expanded beyond it into policing military activities in economic zones, where the founding warrant is weakest. Contested status paired with a world_rearranges verdict produces no zombie flag, but the rising base_extractiveness series marks accumulating extraction layered onto the coordination core, which is the signature the corpus watches for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the unclos_sovereignty_boundary kernel; how would the classification change under the sibling readings?',
    'Compare against the sibling stories: under strict_eez_reading the naval powers move from beneficiary set to violating set and the coastal claimants become rights-holders; under historical_rights_reading the victim and beneficiary sets invert entirely and enforcement becomes the violation. The disagreement is located in the source-hierarchy premise — whether custom, treaty text, or history is supreme.',
    'Every directional assignment in this story is reading-indexed; a different reading yields a different constraint with different beneficiaries, victims, and epsilon over the same waters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story instantiates one of three live readings of a single contested kernel.').

omega_variable(
    customary_status_authenticity,
    'Is the navigation-freedom norm genuinely customary (widespread state practice plus opinio juris) or a projection of treaty norms onto non-parties dressed as custom?',
    'Systematic survey of state practice and protest behavior: count which states acquiesce to transit regimes, which formally protest, and whether the protesting set is stable or growing; weigh official statements against actual toleration of passage.',
    'If the custom is authentic, the reading''s foundational axiom holds and the arrangement retains its coordination core; if it is projection, the constraint loses its legal warrant and reads as unilateral imposition backed by capability — pushing the computed type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_authenticity, empirical, 'Whether the customary-law foundation of the reading is real or asserted.').

omega_variable(
    selective_enforcement_symmetry,
    'Is challenge selection applied symmetrically across allies and rivals, or does operational tempo track geopolitical rivalry rather than the objective excessiveness of claims?',
    'Cross-tabulate the published claims digest against executed operations: identify allied excessive claims (straight-baseline systems, security-zone practices) that appear in the digest but draw few or no operational challenges, compared with rival claims of similar legal character.',
    'Demonstrated selectivity would raise effective extraction sharply, convert the enforcement practice from rule-maintenance into instrumented rivalry, and support reclassification pressure toward snare; uniform enforcement would strengthen the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_symmetry, empirical, 'Whether enforcement tracks the rule or the rival.').

omega_variable(
    eez_military_activities_regulatory_gap,
    'Does the convention actually regulate military activities inside exclusive economic zones, or is the zone of contest a genuine silence in the text that this reading fills by enforcement?',
    'Travaux préparatoires analysis combined with the pattern of state submissions and the 2016 arbitral award''s treatment of the question; observe whether drafters considered and rejected military-activity restrictions or never addressed them.',
    'If the text is genuinely silent, this reading occupies an ungoverned space and its extraction there lacks even arguable textual warrant — the asymmetry is pure capability; if the text implicitly permits, the reading is filling a gap the community left open, which is closer to ordinary law-development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eez_military_activities_regulatory_gap, conceptual, 'Whether the contested zone is treaty silence or treaty permission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1979, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1979, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(uncl_tr_t1986, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(uncl_tr_t1993, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(uncl_tr_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(uncl_tr_t2007, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement(uncl_tr_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement(uncl_tr_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement(uncl_tr_t2026, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1979, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1979, 0.34).
narrative_ontology:measurement(uncl_be_t1986, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1986, 0.38).
narrative_ontology:measurement(uncl_be_t1993, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1993, 0.41).
narrative_ontology:measurement(uncl_be_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(uncl_be_t2007, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2007, 0.48).
narrative_ontology:measurement(uncl_be_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement(uncl_be_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(uncl_be_t2026, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1979, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1979, 0.4).
narrative_ontology:measurement(uncl_su_t1986, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1986, 0.43).
narrative_ontology:measurement(uncl_su_t1993, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1993, 0.46).
narrative_ontology:measurement(uncl_su_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2000, 0.49).
narrative_ontology:measurement(uncl_su_t2007, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement(uncl_su_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2014, 0.56).
narrative_ontology:measurement(uncl_su_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2019, 0.59).
narrative_ontology:measurement(uncl_su_t2026, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the UNCLOS sovereignty boundary' decomposes into three structurally distinct readings of one fixed-text kernel, per the epsilon-invariance principle. This story (non_ratifier_enforcement_reading) authors epsilon for the standing arrangement in which naval presence enforces asserted customary navigation freedoms against zone-exclusivity claims; strict_eez_reading authors epsilon for the arrangement in which Article 57 exclusivity is enforced against all overlays; historical_rights_reading authors epsilon for the arrangement in which historical usage overrides zone provisions. The upstream member is the ratified-treaty framework itself (highest empirical confidence, widest assent); this reading is downstream of it — it cites the treaty's navigation articles as evidence of custom while declining the treaty's obligations — and therefore links to both siblings. Each file's network block links the other two; no orphan members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
