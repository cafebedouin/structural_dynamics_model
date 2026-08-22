% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Normative-Legitimacy Gate on Statehood (Hybrid Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   Since the early 1990s, the practice of recognition and international
 *   standing has been progressively conditioned not only on the four
 *   objective criteria (permanent population, defined territory, effective
 *   government, capacity for external relations) but on normative
 *   performance: democratic governance, human-rights observance, and
 *   non-aggression. Under this arrangement, polities that satisfy the
 *   objective tests but fail the normative ones are refused recognition,
 *   institutional admission, and financing access; established
 *   liberal-democratic states acquire a principled vocabulary for gatekeeping
 *   the international system's membership; and sovereignty becomes
 *   conduct-conditional, widening the space in which humanitarian-framed
 *   intervention and pressure for internal reform proceed. A genuine
 *   coordination function (shared membership standards replacing case-by-case
 *   bargaining) and asymmetric extraction (exclusion of the normatively
 *   failing, conditional sovereignty for the non-liberal) operate through the
 *   same structure, actively maintained by the gatekeeping coalition. KEY
 *   AGENTS (by structural relationship): liberal_democratic_states — agenda
 *   setter (institutional/arbitrage), drafts and grades the benchmarks,
 *   collects gatekeeping discretion; non_liberal_secessionists — primary
 *   target (powerless/trapped), meets objective criteria, denied standing;
 *   unrecognized_de_facto_state_populations — collateral target
 *   (powerless/trapped), bears the isolation costs of denied status;
 *   non_liberal_great_powers — contested target (institutional/mobile),
 *   sovereignty made conditional while harvesting the doctrine's
 *   inconsistencies; international_financial_institutions — secondary
 *   beneficiary (institutional/arbitrage), conditionality mandate;
 *   humanitarian_intervention_coalitions — secondary beneficiary
 *   (powerful/mobile), intervention cover; small_non_aligned_states —
 *   excluded voice (organized/constrained); international_law_scholars —
 *   analytical observer.
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: Agenda setter (institutional/arbitrage) — drafts the normative benchmarks, coordinates admission decisions, waives criteria for partners, applies them strictly to rivals
 *   - non_liberal_secessionists: Primary target (powerless/trapped) — satisfies the objective criteria, denied recognition pending internal transformation to the graders' specification
 *   - unrecognized_de_facto_state_populations: Collateral target (powerless/trapped) — live under functioning authorities whose documents, money, and borders the outside world does not honor
 *   - non_liberal_great_powers: Contested target (institutional/mobile) — sovereignty rendered conditional and intervention-exposed; builds parallel institutions and prosecutes the doctrine's double standards
 *   - international_financial_institutions: Secondary beneficiary (institutional/arbitrage) — governance conditionality operationalizes the benchmarks as lending terms
 *   - humanitarian_intervention_coalitions: Secondary beneficiary (powerful/mobile) — conduct-based sovereignty widens legally claimable space for coercive action
 *   - small_non_aligned_states: Excluded voice (organized/constrained) — defends strict sovereign equality without agenda power in recognition-coordinating forums
 *   - international_law_scholars: Analytical observer (analytical/analytical) — documents the doctrine's genealogy and its selective application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.61).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Normative-Legitimacy Gate on Statehood (Hybrid Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '08b985bc-156f-47ec-9f3e-6c94ff18bb9d').
narrative_ontology:cs_kernel_codification('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', fixed_text).
narrative_ontology:cs_authority_grounding('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', lineage).
narrative_ontology:cs_interpretation_layer_present('08b985bc-156f-47ec-9f3e-6c94ff18bb9d').
narrative_ontology:cs_reading_relation('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', montevideo_statehood_criteria__constitutive_reading, influences).
narrative_ontology:cs_axiom('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', foundational, normative_legitimacy_completes_statehood).
narrative_ontology:cs_axiom_status(normative_legitimacy_completes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', normative_legitimacy_completes_statehood, deontological).
narrative_ontology:cs_axiom('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', secondary, sovereignty_conditional_on_conduct).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_conduct, holdable).
narrative_ontology:cs_axiom_grounding('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', sovereignty_conditional_on_conduct, instrumental).
narrative_ontology:cs_reference_frame('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', criteria_plus_normative_gate).
narrative_ontology:cs_drift_state('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('08b985bc-156f-47ec-9f3e-6c94ff18bb9d', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_coalitions).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, unrecognized_de_facto_state_populations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, non_liberal_great_powers).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, normative_statehood_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and revise the recognition practice: they formulate the governance, rights, and non-aggression benchmarks, coordinate admission decisions in forums they dominate, and decide which applicants clear the bar. They waive the criteria for strategic partners and apply them strictly to rivals. Leaving the arrangement would mean abandoning a gate they control; reframing the criteria is always available to them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Control territory and population and sustain external relations, satisfying the objective criteria, but fail the governance or rights benchmark and are refused recognition and institutional admission. Their path to standing runs through remaking their internal order to their opponents' specification, often without the resources or security that recognition itself would provide. Patron recognition from rival blocs is available but carries dependency costs and deepens their isolation from the main institutions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    powerless, biographical, trapped, regional).

% Live under authorities that issue documents, run schools, and collect taxes that the outside world does not honor. Travel, banking, investment, and disaster response route around them; their status is argued over in forums they cannot address. Exit means leaving the territory or waiting for a settlement negotiated above their heads.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, unrecognized_de_facto_state_populations, payer,
    powerless, biographical, trapped, regional).

% Hold permanent institutional positions and decisive military or economic weight; the doctrine renders their sovereignty conditional and exposes them to coercive action framed as humanitarian enforcement. They respond by building parallel institutions, extending their own recognition to isolated polities, and prosecuting the doctrine's inconsistencies as evidence of bad faith. They also invoke the same normative language when it serves their clients, collecting whatever legitimacy the vocabulary carries.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_great_powers, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, non_liberal_great_powers, beneficiary).

% Operate lending and adjustment programs whose governance conditionality translates the normative benchmarks into loan terms; the doctrine supplies the mandate that lets them attach political conditions to money. They design programs whose costs fall on populations without a seat in their governance.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, international_financial_institutions, agenda_setter).

% Assemble ad hoc military or sanctioning coalitions under humanitarian framing; the conduct-based view of sovereignty widens the space in which such coalitions can claim legality. They select their own targets, write their own mandates, and dissolve when the operation ends.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_coalitions, beneficiary,
    powerful, immediate, mobile, continental).

% Members of the general assembly without veto or agenda-setting power; they defend strict sovereign equality and object that the benchmarks are graded by the strong, but their objections register only as votes in bodies that do not control recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, small_non_aligned_states, excluded,
    organized, generational, constrained, global).

% Trace the doctrine's genealogy, document selective application, and publish the analyses that recognition-coordinating states cite or ignore. They hold no enforcement power and depend on the very practice they critique for their subject matter.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the membership boundary of the international system: gives recognition-coordinating states a shared standard for admitting new polities, aligning aid conditionality, institutional admission, and diplomatic posture around common criteria instead of case-by-case bargaining over each new authority's status.
% TRANSFER_FUNCTION: Moves international legal standing, financing access, and sovereign immunity away from polities that satisfy the objective criteria but fail the normative test, toward the gatekeeping coalition that administers the test; moves intervention discretion toward coalitions able to claim humanitarian purpose.
% ABSENT_VOICES: The secessionist polities and unrecognized-state populations whose status is decided have no seat where the criteria are drafted or applied; small non-aligned states object to conditionality but lack agenda power in recognition-coordinating forums; the criteria were authored and are graded by the states that benefit from grading them.
% DISAPPEARANCE_RATIONALE: If the normative gate vanished overnight, recognition decisions would reorganize around either pure objective criteria or open great-power preference; several dozen de facto authorities would immediately seek recognition; aid conditionality, accession screening, and humanitarian-framed intervention authorization would lose their doctrinal anchor and require renegotiation from scratch.
% FOUNDING_PROBLEM: After the Cold War, mass atrocities and aggressive wars by recognized governments exposed the gap between effective territorial control and legitimate authority: the arrangement was built to answer how the community of states can withhold recognition and standing from polities that control territory but massacre or aggress — without abandoning the objective criteria that keep recognition from collapsing into pure great-power whim.
% FOUNDING_PROBLEM_CORROBORATION: UN commissions of inquiry, ICJ proceedings, and human-rights organizations outside the benefiting states attest that atrocity and aggression by territorial authorities remain live problems. The same sources, however, document that the arrangement's application tracks geopolitical alignment as often as normative performance — corroboration of the founding problem's liveness is considerably stronger than corroboration of the arrangement's even-handedness.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the arrangement denies international standing to polities that meet every objective test, converts sovereignty from a shield into a conditional grant, and does so through machinery the governed cannot appeal to — but the normative criteria themselves are sincerely endorsed by much of the coordinating coalition and do structure real aid and accession decisions, so the extraction is substantial without being total. Suppression is authored at 0.61 as a raw structural property (unscaled by power or scope; only extractiveness is scaled, by the engine): refusal of recognition is enforced through coordinated admission vetoes, sanctions architecture, and conditionality treaties, and no self-help path to standing exists. Theater is 0.46: roughly half of normative invocation is post hoc rationalization of decisions taken on alignment grounds — the share grew as documentation of ally-waiver accumulated — while conditionality and accession screening retain real function. Accessibility collapse is 0.50: understanding the gate does not close all exits, since patron recognition from rival blocs, observer status, and durable de facto statehood remain available at a price. Resistance is 0.60: sustained doctrinal contestation from non-liberal powers, the non-aligned caucus, and critical scholarship, intensifying after the post-2011 intervention backlash. The temporal series run on one shared eight-point grid (1990–2025, five-year steps) with all three metrics authored at every point: base_extractiveness rises as conditionality institutionalized and rent layered onto coordination; theater_ratio rises as documented selectivity shifted invocation toward rationalization; suppression_requirement rises as the enforcement machinery (veto coordination, sanctions regimes, treaty-based conditionality) matured and hardened — an enforcement-buildup trajectory, which is why suppression_requirement is tracked at all alongside the static scalar. The trajectories are monotonic; no cyclical dynamics are claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the agenda-setter seat (liberal_democratic_states), the arrangement is principled standard-setting it authored and controls: low personal cost, high legitimacy return, and full freedom to reframe the criteria — the gate reads as coordination. From the trapped payer seats (non_liberal_secessionists, unrecognized_de_facto_state_populations), the same structure operates as enforced exclusion with no appeal: the gate reads as extraction backed by isolation. The financial-institution seat experiences it as mandate and leverage; the intervention-coalition seat as enlarged discretion. The great-power payer seat is structurally unique: it bears the doctrine's sovereignty-conditional bite while simultaneously exploiting its inconsistencies as rhetorical ammunition — bearing and harvesting at once. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: liberal_democratic_states, international_financial_institutions, and humanitarian_intervention_coalitions sit near the beneficiary end (damped or inverted effective extraction); the two trapped victim groups sit near the full-target end, amplified by their powerlessness and regional confinement; non_liberal_great_powers are declared victims but their mobile exit options and counter-institutional resources place them short of full-target. No directionality_overrides are authored: the override surface keys on power atoms, and the story contains several differently-positioned institutional agents (the gatekeeping coalition, the financial institutions, the great powers) sharing that atom — an override would misfire across seats the structural data already distinguishes. Scope amplification applies modestly: the gate operates globally, making even-handed verification hard and favoring the graders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live: authorities that control territory while committing mass atrocity or aggression still seek and sometimes obtain standing, and the objective criteria alone do not screen for conduct. Mandatrophy is therefore not resolved — the arrangement's mandate has not outlived its function, though its function has drifted toward gatekeeping maintenance. The classification guards against two mislabels: reading the arrangement as pure extraction (snare) would erase the real coordination it performs — shared membership standards, aligned conditionality, predictable admission criteria that replace case-by-case great-power bargaining; reading it as pure coordination (rope) would erase the identifiable victims — polities meeting every objective test yet denied standing, and populations isolated as a consequence. The tangled-rope reading holds both: coordination and extraction through one actively enforced structure. The piton signature is checked and rejected: the administrator plainly benefits enough to maintain the arrangement, so the cost-asymmetry test for inertial persistence fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which completion of the statehood condition does this story encode, and what would the sibling readings change structurally?',
    'Not resolvable by data within this story: it resolves by the classification system treating montevideo_statehood_criteria__declaratory_reading and montevideo_statehood_criteria__constitutive_reading as separate files with their own epsilon, beneficiary sets, and victim sets; cross-file comparison locates the disagreement.',
    'Under the declaratory sibling the normative victim set empties and this story''s extraction collapses toward coordination cost; under the constitutive sibling the victims become polities lacking patrons rather than polities lacking virtue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this is one reading of the statehood-criteria kernel; sibling readings instantiate structurally different victim sets.').

omega_variable(
    selectivity_constitutive_or_incidental,
    'Is the documented asymmetry in applying the normative benchmarks (strictness toward rivals, waiver for strategic partners) incidental hypocrisy correctable within the doctrine, or constitutive of how the gate actually operates?',
    'Comparative audit of recognition, conditionality, and intervention decisions controlling for objective-criteria compliance and alliance position; if asymmetry persists after controls, selectivity is structural rather than episodic.',
    'If constitutive, the arrangement''s coordination function is largely cover and the classification trends toward pure extraction; if incidental, the hybrid coordination-plus-extraction reading stands with reform headroom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_constitutive_or_incidental, empirical, 'Whether selective application is a bug or the operating principle of the normative gate.').

omega_variable(
    secessionist_victim_attribution,
    'Do non-liberal secessionists count as victims of this arrangement specifically, or of the underlying secession conflicts and great-power patronage politics that the normative gate overlays?',
    'Counterfactual enumeration against the declaratory baseline: list polities that would qualify for recognition under objective criteria alone but are denied under the added normative test; the delta isolates this arrangement''s independent contribution to their exclusion.',
    'If most exclusion traces to patronage politics rather than the normative gate, the victim set shrinks and effective extraction falls; if the gate does independent exclusionary work, the victim set stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secessionist_victim_attribution, conceptual, 'Attribution of secessionist exclusion to the normative gate versus confounding conflict dynamics.').

omega_variable(
    intervention_cover_causality,
    'Does conduct-based sovereignty actually cause additional humanitarian-framed interventions, or does it merely supply post hoc vocabulary for coalitions decided on other grounds?',
    'Process-tracing of intervention decisions before and after the doctrine''s crystallization: did authorization arguments invoke normative-performance criteria before the operative decision, or only afterward as justification?',
    'If vocabulary-only, the arrangement enables less intervention than the structural delta suggests and the intervention-coalition beneficiary weight falls; if causal, that weight rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_cover_causality, empirical, 'Causal weight of the doctrine in enabling humanitarian-framed intervention.').

omega_variable(
    protection_extraction_tradeoff,
    'From this reading''s own normative lights, does the gate protect vulnerable populations on balance (withholding legitimacy from abusive authorities) or harm them (isolating the populations of unrecognized polities from finance, travel, and aid)?',
    'Not resolvable by evidence alone: it turns on how the reading weights the moral currency of legitimacy-signaling against the welfare costs of isolation; resolution requires making the reading''s own value ordering explicit.',
    'If protection dominates, part of the measured extraction is re-read as the price of the normative good and effective extraction falls; if isolation harms dominate, the reading''s endorsement of the gate weakens and the arrangement trends toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_extraction_tradeoff, preference, 'The reading''s internal tension between its protective ambition and the isolation costs it imposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement_basis(mont_tr_t1990, observed).
narrative_ontology:measurement(mont_tr_t1995, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement_basis(mont_tr_t1995, observed).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement_basis(mont_tr_t2000, observed).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(mont_tr_t2005, observed).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement_basis(mont_tr_t2010, observed).
narrative_ontology:measurement(mont_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement_basis(mont_tr_t2015, observed).
narrative_ontology:measurement(mont_tr_t2020, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(mont_tr_t2020, observed).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.46).
narrative_ontology:measurement_basis(mont_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(mont_be_t1990, observed).
narrative_ontology:measurement(mont_be_t1995, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement_basis(mont_be_t1995, observed).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement_basis(mont_be_t2000, observed).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement_basis(mont_be_t2005, observed).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement_basis(mont_be_t2010, observed).
narrative_ontology:measurement(mont_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(mont_be_t2015, observed).
narrative_ontology:measurement(mont_be_t2020, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(mont_be_t2020, observed).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(mont_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement_basis(mont_su_t1990, observed).
narrative_ontology:measurement(mont_su_t1995, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement_basis(mont_su_t1995, observed).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement_basis(mont_su_t2000, observed).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement_basis(mont_su_t2005, observed).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement_basis(mont_su_t2010, observed).
narrative_ontology:measurement(mont_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement_basis(mont_su_t2015, observed).
narrative_ontology:measurement(mont_su_t2020, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(mont_su_t2020, observed).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement_basis(mont_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, r2p_humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'criteria for statehood': the label conflates three structurally distinct claims about what completes the statehood condition, with materially different epsilon values, beneficiary sets, and victim sets. This file instantiates the hybrid reading only. It sits downstream of the declaratory text (it presupposes the objective criteria as necessary) and exerts structural pressure on the constitutive reading (its normative gate changes which acts of recognition count as legitimate, forcing the constitutive position to filter through norms). The third edge anticipates the intervention-doctrine file that this reading's conduct-based sovereignty feeds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
