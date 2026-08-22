% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Absolute Non-Intervention Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The Westphalian absolute non-intervention doctrine asserts that state
 *   sovereignty is categorical and territorial—external interference in
 *   domestic affairs is per se illegitimate regardless of internal conduct.
 *   This reading treats sovereignty as an inviolable property, not a
 *   conditional status tied to state performance. Under this reading, mass
 *   atrocity, ethnic cleansing, systematic torture, and repression remain
 *   internal matters; external intervention is categorically illegitimate
 *   whether or not the state protects its population. The doctrine shields
 *   state elites and authoritarian regimes from external accountability while
 *   rendering populations under atrocity ineligible for external protection.
 *   The structural claim is that this reading FORECLOSES the
 *   conditional-responsibility reading (which ties intervention legitimacy to
 *   state failure to protect) within any single framework: one cannot
 *   simultaneously hold that sovereignty is categorical-and-inviolable AND
 *   that sovereignty is conditional-on-protection. The graded-sovereignty
 *   reading COEXISTS as a rival reading held by different international
 *   actors but not foreclosed by the categorical reading itself.
 *
 * KEY AGENTS:
 *   - state_elites: institutional power, agenda_setter role — formulate and enforce the doctrine
 *   - authoritarian_regimes: institutional power, beneficiary role — shield internal conduct from intervention
 *   - populations_under_atrocity: powerless, payer role — trapped without external protection
 *   - weaker_states: moderate power, constrained exit — invoke sovereignty rhetorically but lack enforcement capacity
 *   - enforcement_coalition: institutional power, agenda_setter role — maintain doctrine through Security Council and treaty law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.82).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.77).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.82).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, snare).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Absolute Non-Intervention Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '59263e94-a8bb-4346-bbbb-6c71232601dd').
narrative_ontology:cs_kernel_codification('59263e94-a8bb-4346-bbbb-6c71232601dd', formalized).
narrative_ontology:cs_authority_grounding('59263e94-a8bb-4346-bbbb-6c71232601dd', extraction).
narrative_ontology:cs_interpretation_layer_present('59263e94-a8bb-4346-bbbb-6c71232601dd').
narrative_ontology:cs_reading_relation('59263e94-a8bb-4346-bbbb-6c71232601dd', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('59263e94-a8bb-4346-bbbb-6c71232601dd', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('59263e94-a8bb-4346-bbbb-6c71232601dd', foundational, sovereignty_categorically_inviolable).
narrative_ontology:cs_axiom_status(sovereignty_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('59263e94-a8bb-4346-bbbb-6c71232601dd', sovereignty_categorically_inviolable, conventional).
narrative_ontology:cs_axiom('59263e94-a8bb-4346-bbbb-6c71232601dd', foundational, territorial_authority_immune_from_external_judgment).
narrative_ontology:cs_axiom_status(territorial_authority_immune_from_external_judgment, holdable).
narrative_ontology:cs_axiom_grounding('59263e94-a8bb-4346-bbbb-6c71232601dd', territorial_authority_immune_from_external_judgment, deontological).
narrative_ontology:cs_reference_frame('59263e94-a8bb-4346-bbbb-6c71232601dd', westphalia_territorial_immunity).
narrative_ontology:cs_drift_state('59263e94-a8bb-4346-bbbb-6c71232601dd', contemporary_mass_atrocity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59263e94-a8bb-4346-bbbb-6c71232601dd', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, stateless_and_displaced_persons).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, internal_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, powerful_western_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, powerful_western_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_humanitarian_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, weaker_states_and_developing_nations).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, state_monopoly_on_legitimate_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulate and enforce the non-intervention principle through diplomatic channels, treaty law, and UN Security Council procedure. Benefit from a rule that shields domestic conduct from external scrutiny—from labor practices to human rights violations. Can cite sovereignty doctrine to block humanitarian investigation and external pressure. Maintain the norm through diplomatic pressure, threat of retaliation (sanctions, trade closure), and narrative control in international forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites, agenda_setter,
    institutional, generational, arbitrage, global).

% Shield internal conduct—mass detention, ethnic cleansing, systematic torture, restriction of civil liberties—from external intervention by invoking absolute sovereignty. The doctrine provides legal and rhetorical immunity for mass atrocity and repression. They benefit from the rule's categorical character: there is no legitimacy gradient, no intervention threshold tied to severity, no external authority to adjudicate. The constraint protects them specifically because they violate humanitarian norms most severely.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    powerful, generational, arbitrage, global).

% Formally endorse the non-intervention principle in multilateral forums but retain unilateral capacity to intervene when strategic interests align (NATO intervention in Kosovo, Iraq, Libya). They benefit from the norm's protection when they wish to conduct operations without external interference; they pay when rivals use the same sovereignty shield to protect strategic competitors. Their exit option is selective enforcement—they can invoke the norm or override it depending on geopolitical calculation.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, powerful_western_states, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, powerful_western_states, payer).

% Endure mass atrocity, ethnic cleansing, systematic violence, and repression within a state that claims territorial immunity from external intervention. They cannot exit; they cannot appeal to external protection; the constraint renders their suffering an internal matter. They bear the extraction through direct violence and suffering—the constraint's persistence depends on suppressing their voice, their ability to seek refuge, and their access to external humanitarian protection.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_atrocity, payer,
    powerless, immediate, trapped, local).

% Flee atrocity and seek asylum across borders, but find that the sovereignty doctrine treats them as internal matters of the origin state—asylum claims are rejected on the grounds that external states cannot legitimately question the origin state's conduct. They are trapped between origin-state violence and destination-state borders; the constraint denies them the only exit that exists (asylum) by rendering atrocity an internal jurisdictional question.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, stateless_and_displaced_persons, payer,
    powerless, immediate, trapped, regional).

% Provide assistance in conflict zones and atrocity settings but are barred by sovereignty doctrine from direct intervention or reporting that would invite external state action. They bear the cost of neutrality—they see atrocity but cannot call for intervention without risking expulsion, loss of access, and accusations of violating sovereignty. They are confined to the role of observer and service-provider, not advocate for intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_humanitarian_organizations, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, international_humanitarian_organizations, observer).

% Document mass atrocity, produce reports with names and timelines, establish liability—but lack enforcement authority and are structurally barred from calling for intervention by the same sovereignty doctrine. They would advocate for external accountability and protective intervention if empowered; the constraint excludes them from the legitimate intervention decision by defining intervention itself as illegitimate.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_human_rights_bodies, excluded,
    organized, biographical, constrained, global).

% Invoke sovereignty as protection but hold minimal capacity to enforce it; their sovereignty is nominal. They benefit rhetorically from the principle but cannot operationalize its protection when powerful states choose to intervene. They pay by exposing themselves to intervention while powerful states retain the exit option of selective enforcement. The constraint applies categorically in law but selectively in practice, asymmetrically harming those too weak to maintain arbitrage.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, weaker_states_and_developing_nations, payer,
    moderate, biographical, constrained, regional).

% Maintains the sovereignty principle through UN Security Council veto mechanics, treaty law, and diplomatic norms. Primary maintainers are permanent Security Council members (Russia, China) who use sovereignty doctrine to shield allies from humanitarian intervention scrutiny. They enforce through retaliation threats, diplomatic isolation of intervention advocates, and narrative control in international forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, enforcement_coalition, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system of territorial jurisdiction immune from external interference, enabling states to conduct internal affairs without external override. The stated coordination problem: prevent the strong from dominating the weak through constant intervention; enable stable state systems to function without external powers imposing regime change.
% TRANSFER_FUNCTION: Transfers immunity from external accountability and intervention to state elites and authoritarian regimes at the cost of leaving populations under atrocity without external protective recourse. Moves decision-authority over mass violence entirely to the state claiming territorial sovereignty, away from international bodies or humanitarian actors who would intervene.
% ABSENT_VOICES: Populations under atrocity are structurally excluded from the decision calculus—they would contest the norm entirely if empowered to speak. So would human rights bodies, international humanitarian organizations, and asylum seekers. These actors would argue for a threshold-based intervention doctrine tied to atrocity severity; their exclusion is what the non-intervention principle enforces.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention doctrine vanished overnight, humanitarian intervention would become legally legitimate when atrocity severity crosses a threshold (replacing categorical prohibition with graduated legitimacy). Asylum and refugee law would expand; accountability mechanisms would gain enforcement capacity; humanitarian protection would supersede state sovereignty in specified extreme cases. The state system would reorganize around qualified, threshold-bounded sovereignty rather than categorical territorial immunity.
% FOUNDING_PROBLEM: In the 17th century, constant religious and dynastic intervention by major powers destabilized the European state system; the Peace of Westphalia codified territorial sovereignty as a way to stabilize interstate relations by establishing non-interference as the foundational rule.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international law scholars (including those outside the benefiting state-elite camp) attest that the founding problem—constant regime-change intervention destabilizing the interstate system—was substantially solved by Westphalia. Contemporary international relations scholars and human rights organizations outside the enforcement coalition attest that the founding problem is solved and the constraint now functions primarily as a shield for atrocity, not as stabilizer of interstate relations. Systematic evidence: states that violate humanitarian norms most severely invoke sovereignty doctrine most aggressively; humanitarian intervention has become more selective and geopolitically calculated rather than categorically prohibited.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically transfers immunity for mass violence to state elites at the cost of leaving atrocity populations without external recourse. The measurement series tracks the constraint's evolution: at Westphalia (1648, t=0) extractiveness was lower (0.45) because the founding problem (destabilizing intervention cycles) was acute and the doctrine genuinely solved it by stabilizing interstate relations. Over 376 years, extractiveness rose as the founding problem became solved and the doctrine's function shifted from stabilization to protection-of-atrocity. By 1945, after the Holocaust, extractiveness jumped to 0.68 as the doctrine was reaffirmed even as its original justification (interstate stability) became manifest. By 2024, extractiveness reaches 0.82 as the constraint's extractive function is nearly complete—it shields atrocity, not interstate conflict. Suppression tracks enforcement intensity: t0=0.40 (early period, emerging doctrine), rising to 0.77 by 2024 (active suppression of humanitarian intervention norms and asylum access). Theater_ratio rises sharply after 1945 (0.35 → 0.48 → 0.61): increasingly, state invocations of sovereignty are performative—powerful states override the doctrine when strategic interest aligns (NATO Kosovo, US Iraq, French Mali interventions) while authoritarian states invoke it categorically. The performativity is highest at t=2024 (0.61): the doctrine's stated function (preventing destabilizing intervention) is overtaken by its actual function (shielding atrocity from accountability). All measurements authored on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The state-elite and authoritarian-regime seats experience the constraint as categorical protection—sovereignty is inviolable, the constraint is natural law grounded in interstate order. The population-under-atrocity seat experiences it as extractive snare: the same doctrine that protects state elites operates as forced exposure to violence without exit or external recourse. Weaker-state seats experience a paradoxical asymmetry: they invoke sovereignty as protection but lack the arbitrage capacity (selective enforcement) that powerful states retain. The engine should compute this constraint as snare from the atrocity-population seat (high d, extraction-dependent) and as rope or even mountain from the state-elite seat (low d, coordination-framed). The divergence is the measurement itself—the claimed type (snare) reflects the atrocity-population perspective, which the structural data supports. Powerful-state seats should compute as beneficiaries with partial symmetry (they pay when rivals invoke the same shield), reflecting their mobile exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is stratified by power and exit options. Authoritarian regimes and state elites have d near 0.0 (full beneficiaries): they collect immunity, control the rule's interpretation, and can exit selectively (arbitrage). Populations under atrocity have d near 1.0 (full targets): they pay in direct violence, have no exit (trapped), and have no agency in rule interpretation. Weaker states are intermediate (d ~0.6): they nominally benefit but lack enforcement capacity; their sovereignty is categorical in law but selective in practice. Powerful Western states are interesting: they have low d despite being beneficiaries because they retain the exit option (selective enforcement, arbitrage capacity). The structural derivation from beneficiary/victim + exit should produce this pattern: beneficiaries with arbitrage → low d; victims with trapped exit → high d; organized excluded actors → intermediate (they would benefit if included but are structurally prevented from participating). No directionality override is needed; the structural data is sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (destabilizing intervention cycles) was live at Westphalia (1648) and remained live through the Congress of Vienna (1815) and Versailles (1919). By 1945, after the Holocaust, the problem was demonstrably solved: interstate intervention had been replaced with alliance-based warfare; the constant regime-change destabilization of the 17th-18th centuries had been superseded by bloc competition. The constraint should have evolved into either a rope (pure coordination, no extraction) or a scaffold (marked for sunset as the problem solved). Instead, it calcified and its function inverted: it became a shield for mass atrocity. The measuring evidence is the divergence between founding_problem_status (dead) and base_extractiveness (0.82 at t=end): a constraint whose founding problem is dead but whose extractiveness is rising is a textbook mandatrophy case. The theater_ratio rise (0.15 → 0.61) confirms the inversion: performative invocation has replaced functional stabilization. The remedy would require either reformulating sovereignty as conditional (the conditional_responsibility reading) or creating threshold-based intervention doctrine (the graded_sovereignty reading). This constraint, as authored, is the lock that these alternative readings would open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the Westphalian founding problem (destabilizing intervention cycles) actually solved, or does it persist in updated form?',
    'Historical analysis of intervention patterns: if major-power intervention remains constant or increasing (NATO expansion, US interventions, Chinese regional operations), the problem is solved; if intervention frequency tracks inversely with non-intervention doctrine invocation, the founding problem is dead and the doctrine has inverted function.',
    'If the founding problem is empirically dead, the constraint is mandatrophy (protecting a solved problem → extractive stasis). If the problem is still live (destabilizing cycles remain), the constraint performs its original function. This determines whether reform (conditional/graded readings) is warranted or whether categorical non-intervention remains functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem the constraint was built to solve is actually solved').

omega_variable(
    categorical_vs_conditional_foreclosure,
    'Does the categorical non-intervention reading logically foreclose the conditional-responsibility reading, or can a framework accommodate both (conditional authority that defaults to categorical immunity)?',
    'Formal logic test: can ''sovereignty is inviolable UNLESS the state fails to protect'' be held coherently, or does adding the conditional clause contradict the categorical claim? Legal analysis of hybrid frameworks that attempt to incorporate both (e.g., Responsibility to Protect doctrine and sovereignty coexisting in the same text).',
    'If foreclose is correct, the two readings are genuinely incompatible and the constraint choice is binary (choose one). If conditional can be grafted onto categorical (sovereignty with escape clauses), the readings might coexist in a single framework and classification ambiguity remains. A foreclosure finding supports the claim that these are distinct constraints (separate constraint files); a coexistence finding suggests they might be seat-indexed framings of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_conditional_foreclosure, conceptual, 'Whether absolute non-intervention and conditional responsibility are logically incompatible or can coexist').

omega_variable(
    atrocity_population_exclusion_mechanism,
    'Is the exclusion of atrocity populations from intervention-decision participation structural (they are outside the state system) or performative (they are silenced by state gatekeeping)?',
    'If excluded populations were given voice (e.g., through international referendum on intervention, parallel asylum court, international-criminal-accountability proceedings they initiate), would they block or support interventions? Historical evidence from cases where populations were consulted (e.g., Afghanistan, Syria intervention debates on social media).',
    'If structural, the exclusion is inherent to state-system architecture. If performative, the exclusion is a choice by enforcement coalition actors. A performative finding supports mandatory consultation mechanisms (stakeholder-surface reform); a structural finding suggests consent-of-populations cannot be operationalized without reforming state-system sovereignty itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_population_exclusion_mechanism, empirical, 'Whether populations under atrocity are excluded from intervention decisions by systemic structure or by enforcement coalition choice').

omega_variable(
    kernel_reading_asymmetry_in_power,
    'Do powerful states and weaker states hold different readings of the Westphalia kernel because the constraint operates asymmetrically at different power levels, or do they hold different readings despite symmetric structure?',
    'Comparative analysis of how powerful states (US, China, Russia) invoke non-intervention doctrine when their interests are protected vs. when rivals use it. If powerful states selectively invoke/override the doctrine based on geopolitical alignment, they are reading it conditionally in practice (even if asserting categorically); if they are consistent, their reading is genuinely categorical.',
    'If asymmetric operation, the constraint is actually graded-sovereignty in practice (different-rule-by-power level) even though it is absolute-non-intervention in formal law. This would support the graded-sovereignty reading as more accurate. If consistent operation, powerful states are simply free-riders on the same rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_asymmetry_in_power, empirical, 'Whether the non-intervention doctrine operates asymmetrically depending on actor power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.15).
narrative_ontology:measurement_basis(west_tr_t1648, projected).
narrative_ontology:measurement(west_tr_t1815, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1815, 0.22).
narrative_ontology:measurement_basis(west_tr_t1815, projected).
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.35).
narrative_ontology:measurement_basis(west_tr_t1945, observed).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.48).
narrative_ontology:measurement_basis(west_tr_t1990, observed).
narrative_ontology:measurement(west_tr_t2010, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2010, 0.56).
narrative_ontology:measurement_basis(west_tr_t2010, observed).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2024, 0.61).
narrative_ontology:measurement_basis(west_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.45).
narrative_ontology:measurement_basis(west_be_t1648, projected).
narrative_ontology:measurement(west_be_t1815, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1815, 0.52).
narrative_ontology:measurement_basis(west_be_t1815, projected).
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.68).
narrative_ontology:measurement_basis(west_be_t1945, observed).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement_basis(west_be_t1990, observed).
narrative_ontology:measurement(west_be_t2010, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement_basis(west_be_t2010, observed).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(west_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement_basis(west_su_t1648, projected).
narrative_ontology:measurement(west_su_t1815, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1815, 0.48).
narrative_ontology:measurement_basis(west_su_t1815, projected).
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.61).
narrative_ontology:measurement_basis(west_su_t1945, observed).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(west_su_t1990, observed).
narrative_ontology:measurement(west_su_t2010, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement_basis(west_su_t2010, observed).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2024, 0.77).
narrative_ontology:measurement_basis(west_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, asylum_and_refuge_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, international_criminal_accountability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Westphalia kernel (westphalia_sovereignty). The conditional_responsibility and graded_sovereignty readings are sibling constraints; each instantiates the same kernel but produces different ε values and stakeholder structures. The absolute_non_intervention reading (this file) FORECLOSES conditional_responsibility within a single coherent framework: one cannot simultaneously hold that sovereignty is absolutely inviolable and that sovereignty is conditional on state protection performance. The three constraints form a constraint family; all members are linked via network.affects_constraints to enable contamination-propagation analysis. When one reading's purity degrades (e.g., selective enforcement of absolute non-intervention undermines the categorical claim), downstream readings' classifications shift (e.g., conditional_responsibility becomes more credible). The family is upstream of humanitarian_intervention_doctrine, asylum_and_refuge_doctrine, and international_criminal_accountability, all of which operate in the space this kernel permits or forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
