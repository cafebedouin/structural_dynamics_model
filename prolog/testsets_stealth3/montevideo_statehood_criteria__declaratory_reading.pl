% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Declaratory Reading: Objective Criteria Establish Statehood as Legal Fact
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This story instantiates the declaratory reading of the Montevideo
 *   statehood kernel: the claim that a permanent population, defined
 *   territory, government, and capacity for foreign relations establish
 *   statehood as a legal fact, with recognition merely acknowledging what
 *   already exists. Epsilon's referent is the standing arrangement under
 *   contest — the recognition-mediated membership regime that actually
 *   operates, formally draped in declaratory language but constitutive in
 *   operation — assessed by this reading's own lights, under which
 *   withholding legal personality from criteria-meeting polities is
 *   indefensible gatekeeping. KEY AGENTS (by structural relationship): -
 *   unrecognized_de_facto_authorities: primary target (moderate/trapped) —
 *   meets the criteria, denied legal personality -
 *   leveraged_unrecognized_entities: differentiated target
 *   (moderate/constrained) — criteria-meeting, partially mitigates exclusion
 *   through economic indispensability - secessionist_region_populations: deep
 *   target (powerless/trapped) — bears exclusion costs daily - parent_states:
 *   beneficiary bearing upkeep costs (powerful/constrained) — collects
 *   territorial-integrity protection - great_power_patrons: agenda-setter and
 *   principal collector (institutional/arbitrage) — converts admission
 *   decisions into geopolitical currency - incumbent_recognized_states:
 *   collective beneficiary (institutional/arbitrage) — operates the
 *   membership gate - would_be_secessionists_in_stable_states: excluded voice
 *   (moderate/constrained) - international_law_community: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.7).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.62).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Declaratory Reading: Objective Criteria Establish Statehood as Legal Fact").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'da56d72b-0f5d-4265-8d75-a009252eeaa2').
narrative_ontology:cs_kernel_codification('da56d72b-0f5d-4265-8d75-a009252eeaa2', fixed_text).
narrative_ontology:cs_authority_grounding('da56d72b-0f5d-4265-8d75-a009252eeaa2', practice).
narrative_ontology:cs_interpretation_layer_present('da56d72b-0f5d-4265-8d75-a009252eeaa2').
narrative_ontology:cs_reading_relation('da56d72b-0f5d-4265-8d75-a009252eeaa2', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('da56d72b-0f5d-4265-8d75-a009252eeaa2', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('da56d72b-0f5d-4265-8d75-a009252eeaa2', foundational, objective_criteria_constitute_statehood).
narrative_ontology:cs_axiom_status(objective_criteria_constitute_statehood, holdable).
narrative_ontology:cs_axiom_grounding('da56d72b-0f5d-4265-8d75-a009252eeaa2', objective_criteria_constitute_statehood, conventional).
narrative_ontology:cs_axiom('da56d72b-0f5d-4265-8d75-a009252eeaa2', secondary, recognition_merely_acknowledges_pre_existing_fact).
narrative_ontology:cs_axiom_status(recognition_merely_acknowledges_pre_existing_fact, holdable).
narrative_ontology:cs_axiom_grounding('da56d72b-0f5d-4265-8d75-a009252eeaa2', recognition_merely_acknowledges_pre_existing_fact, conventional).
narrative_ontology:cs_reference_frame('da56d72b-0f5d-4265-8d75-a009252eeaa2', self_executing_objective_statehood).
narrative_ontology:cs_drift_state('da56d72b-0f5d-4265-8d75-a009252eeaa2', contemporary_recognition_politics, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('da56d72b-0f5d-4265-8d75-a009252eeaa2', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, great_power_patrons).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, incumbent_recognized_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, unrecognized_de_facto_authorities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, leveraged_unrecognized_entities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, secessionist_region_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, declaratory_theory_of_statehood).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, effectiveness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern territory with permanent population, defined borders, functioning administration, and foreign-relations bureaus — Somaliland, the Turkish Republic of Northern Cyprus, Abkhazia, Transnistria — yet hold no General Assembly seat, cannot accede to most treaties, issue documents many states refuse to honor, and borrow at premiums reflecting legal limbo. The statehood bid outlives individual leaderships; abandoning it means dissolving the polity itself.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, unrecognized_de_facto_authorities, payer,
    moderate, generational, trapped, regional).

% Meet the objective criteria and sustain deep functional integration — Taiwan supplies a decisive share of advanced semiconductor capacity, maintains unofficial representative offices in dozens of capitals, and joins technical bodies episodically — while barred from formal admission to most international organizations. Economic indispensability buys working relationships short of recognition; formal legal personality stays out of reach regardless of criteria performance.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, leveraged_unrecognized_entities, payer,
    moderate, generational, constrained, global).

% Live inside the unrecognized polities: passports go unhonored for visa-free travel, pandemic-era vaccine procurement routed around them, investors price their risk higher, pensions and property documents lose validity abroad. Individual exit means emigration and losing home; staying means carrying the legal invisibility daily.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, secessionist_region_populations, payer,
    powerless, biographical, trapped, regional).

% Serbia, Georgia, China, Somalia, Cyprus — hold de jure title over the contested territories and rely on non-recognition by the broader community to keep their sovereignty claims intact. They spend diplomatic capital lobbying other governments against recognition and administer the frozen conflicts, embargo lines, and transit restrictions that non-recognition sustains.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, parent_states, payer).

% Decide which new polities are admitted and which stay outside: Washington recognized Kosovo while leading non-recognition of annexed Ukrainian territory; Moscow recognized Abkhazia and South Ossetia days after the 2008 war; Beijing imposes trade retaliation on governments that upgrade ties with Taipei. Admission and exclusion decisions cascade through alliances and lending institutions, converting them into alliance discipline, basing rights, and market access.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, great_power_patrons, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, great_power_patrons, beneficiary).

% The roughly 190 recognized members operate the admission votes, credentials committees, and specialized-agency membership gates. Club membership preserves relative standing, protects borders against unilateral revision, and keeps club size — and with it each member's share of attention and voting weight — under collective control.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, incumbent_recognized_states, beneficiary,
    institutional, generational, arbitrage, global).

% Catalan, Scottish, Québécois, and comparable movements read every recognition decision for precedent effects. Governments fearing domestic precedent — Spain over Kosovo — work to keep the admission question closed, so these movements hold no seat in the bodies that write recognition practice even though each ruling redraws their option set.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, would_be_secessionists_in_stable_states, excluded,
    moderate, biographical, constrained, regional).

% Treaty lawyers, the International Court of Justice, and scholars document where criteria satisfaction and recognition outcomes diverge: the Court's 2010 Kosovo advisory opinion treated the independence declaration as not prohibited by international law while leaving statehood to recognition practice, and the monograph literature tracks entities governing effectively for decades without admission.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, great_power_patrons).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The four criteria give every actor a shared factual checklist for deciding whether a polity counts as a state, so bilateral recognition, treaty-making, and organization admission need not be renegotiated case-by-case from raw power politics; membership waves (decolonization, post-Soviet succession) were processed at scale once the checklist did the sorting.
% TRANSFER_FUNCTION: Moves legal personality, treaty capacity, and access to international goods — development finance, health governance, aviation registration, diplomatic immunity — granting them to criteria-passing, consensus-backed polities and denying them to criteria-passing, politically contested ones; the denial side transfers bargaining leverage to incumbents and patrons.
% ABSENT_VOICES: Unrecognized authorities and their populations are the loudest absent voices: they carry the regime's costs directly but sit outside every body that writes recognition policy — General Assembly credentials debates occur solely among recognized members. Would-be secessionists in stable states are kept out deliberately, to avoid precedent contagion.
% DISAPPEARANCE_RATIONALE: Without a criteria-based determination layer, every admission reverts to open great-power bargaining: the decolonization and post-Soviet admission waves would have required case-by-case patron sponsorship, treaty networks would lack stable counterparties, and the roughly twenty currently contested polities would shift from legal limbo into outright nonexistence — while incumbent states would gain an even freer hand to recognize or ignore at pleasure.
% FOUNDING_PROBLEM: Interwar recognition was discretionary great-power favor: a polity's legal existence hung on patrons, admission was unpredictable, and denial was a routine instrument of pressure (the Stimson non-recognition era). Montevideo's drafters sought a factual definition — permanent population, defined territory, government, capacity for foreign relations — fixing statehood independent of anyone's consent, to stabilize expectations for new and old states alike.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic public-law scholarship (Crawford's The Creation of States and the declaratory-school literature generally) documents systematic divergence between criteria satisfaction and recognition outcomes; the International Court of Justice's 2010 Kosovo proceedings placed the criteria-versus-consensus dispute on the judicial record; and the administrative record of criteria-meeting entities denied admission for decades (Somaliland's uninterrupted governance since 1991 alongside recognized members passing through collapsed-administration periods) attests the founding problem's persistence. No attesting seat sits within the patron or club beneficiary set, whose interest lies in preserving exactly the discretion the declaratory reading would remove.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70) because exclusion costs for trapped criteria-meeting polities approach the existential — no treaty capacity, no multilateral finance, invalid documents — and because denial tracks patron preference rather than criteria performance (South Sudan cleared admission within weeks of its 2011 referendum; Somaliland has waited thirty years with stronger administrative continuity than many admitted members). Extraction is not 0.9 because consensus-forming still admits newcomers when patrons align. Suppression (0.62) reflects active enforcement machinery: bloc-disciplined non-recognition, punish-the-recognizer trade retaliation (Lithuania 2021), and credentials blocks — targeted rather than systemic, hence below the platform-commission style ceiling. Theater (0.40) rises across the interval as admission politics acquire procedural and juridical legitimation layers (Badinter-style criteria supplements, advisory-opinion litigation) that stage universality after the substantive decision was made in capitals. The temporal series is U-shaped, not monotonic: extraction fell as decolonization made admission near-automatic (trough 0.38 at t=45, the universal-membership plateau), then climbed as post-1991 conditional and patron-selective recognition rebuilt discretionary gating under legality vocabulary. All three series run on one shared seven-point grid (t=0 anchors 1933, t=90 anchors 2023; fifteen-year steps). The claim (tangled_rope) is authored from structure — a real coordination checklist plus asymmetric selective denial plus active enforcement — independently of these metric values; the engine computes per-seat types from the data, and any divergence from the claim is the datum.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergently from identical structure. The trapped payer seats (unrecognized authorities, their populations) experience near-total closure and should compute toward the snare side; the patron and club seats administer a working classification system they built and staff, and should compute rope-side; parent states straddle — they collect integrity protection (rope-side benefit) while paying frozen-conflict upkeep (payer-side cost); the leveraged entity computes intermediate, its constrained exit damping effective extraction relative to its trapped peers. Same nominal regime, four experienced types; the engine derives this from power, exit, and role data rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Club members, patrons, and parent states derive low directionality (subsidized seats): the arrangement preserves their standing, hands patrons a convertible leverage instrument, and shields parent-state title. Unrecognized authorities and region populations derive near-full-target directionality, amplified by trapped exit — there is no legal route out of the category they occupy. The leveraged entity sits slightly off the full-target pole: constrained (not arbitrage) exit means mitigation without escape. The excluded secessionist movements and the analytical community contribute no flow. Coalition potential among the powerless exists in principle but is structurally discouraged — the polities are dispersed, several sit in opposing regional dyads, and patron isolation strategies treat each bid separately, which is itself part of the enforcement design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary, patron-controlled recognition — was declared substantially solved at the universal-membership plateau (~1978), when admission approached automaticity and the checklist did the classifying. Post-1991 practice (legitimacy-supplemented conditional recognition, patron-selective admission) reopened the original arbitrariness under new vocabulary, so the mandate's status is contested, not dead, and no sunset applies. The declaratory reading guards against misclassification in both directions: read without its coordination-function data, the regime collapses into gatekeeping-for-its-own-sake (pure snare misread); read without its victim structure, the criteria checklist passes as innocent bookkeeping (pure rope misread). Tangled rope — genuine coordination plus asymmetric extraction under active enforcement — is the classification that keeps both halves visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_delta_routing,
    'This constraint is the declaratory reading of the montevideo_statehood_criteria kernel; what structural changes would the constitutive or hybrid sibling readings produce if they displaced it?',
    'Comparative classification across the three sibling stories in the family: trace how victim-set membership, beneficiary leverage, and measured extraction shift under each reading''s displacement scenario.',
    'Under the constitutive sibling, unrecognized de facto authorities exit the victim set and parent-state leverage is restored as legitimate discretion; under the hybrid sibling, the victim set narrows to criteria-passing polities failing legitimacy supplements. Victim-set membership — hence measured extraction — is reading-relative, not arrangement-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_delta_routing, conceptual, 'Reading-relative victim structure across the Montevideo kernel''s sibling readings.').

omega_variable(
    criteria_performance_vs_power_predicting_admission,
    'Across statehood bids since 1960, do the four objective criteria predict admission outcomes better than patron alignment does?',
    'Code all statehood bids on criteria satisfaction, recognition success, and patron alignment; regress admission on both with interaction terms and era fixed effects.',
    'If patron alignment dominates, the criteria layer functions as cover and the standing arrangement sits nearer pure gatekeeping; if criteria dominate, the coordination function is load-bearing and excess extraction is bounded to the selectively contested cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_performance_vs_power_predicting_admission, empirical, 'Whether criteria performance or patron alignment drives admission outcomes.').

omega_variable(
    leveraged_entity_mitigation_durability,
    'Will the leveraged unrecognized entity''s economic-indispensability mitigation survive semiconductor supply-chain diversification through the 2030s?',
    'Track fabrication-capacity dispersion and the entity''s share of critical production nodes; observe whether functional participation contracts as substitutability rises.',
    'Loss of leverage converts the constrained seat toward the trapped seat''s profile, raising its effective extraction toward full-target and widening the measured spread within the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leveraged_entity_mitigation_durability, empirical, 'Durability of economic mitigation for the leveraged unrecognized seat.').

omega_variable(
    self_execution_degree_in_current_law,
    'How far does contemporary international law already run self-executing on criteria satisfaction — customary treatment, judicial practice, automatic-effect doctrines — rather than remaining consensus-dependent?',
    'Systematic review of post-1945 judicial and arbitral treatment of statehood questions where recognition was withheld; identify cases where legal effect attached despite non-recognition.',
    'Greater existing self-execution shrinks the delta between the standing arrangement and this reading''s endorsement, lowering the extraction attributable specifically to the recognition layer; lesser self-execution confirms the constitutive operation this reading contests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_execution_degree_in_current_law, conceptual, 'Degree of self-execution already present in operative international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mont_tr_t0, observed).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(mont_tr_t15, observed).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(mont_tr_t30, observed).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(mont_tr_t45, observed).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(mont_tr_t60, observed).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 75, 0.37).
narrative_ontology:measurement_basis(mont_tr_t75, observed).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement_basis(mont_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(mont_be_t0, observed).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(mont_be_t15, observed).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(mont_be_t30, observed).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement_basis(mont_be_t45, observed).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(mont_be_t60, observed).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement_basis(mont_be_t75, observed).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 90, 0.7).
narrative_ontology:measurement_basis(mont_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(mont_su_t0, observed).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(mont_su_t15, observed).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(mont_su_t30, observed).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement_basis(mont_su_t45, observed).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(mont_su_t60, observed).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement_basis(mont_su_t75, observed).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 90, 0.62).
narrative_ontology:measurement_basis(mont_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, information_standard).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Montevideo statehood criteria' decomposes into three structurally distinct readings — constitutive, declaratory, hybrid — with different victim sets, different beneficiary leverage, and therefore different epsilon values over the same standing arrangement. The declaratory reading is upstream in rhetorical standing (its language dominates treaty texts) while the constitutive operation dominates practice; erosion of the declaratory reading's credibility feeds the constitutive sibling's leverage, and the hybrid sibling's legitimacy supplements import gate conditions that change who can clear the criteria layer at all. Linked via affects_constraints so purity degradation propagates across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
