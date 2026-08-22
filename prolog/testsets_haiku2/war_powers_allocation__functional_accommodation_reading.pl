% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation by Operational Context (Functional Accommodation Reading)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   Under the functional accommodation reading, war powers are allocated
 *   based on operational context: the president holds unilateral authority to
 *   respond to imminent threats without prior congressional authorization;
 *   prolonged military campaigns require explicit congressional
 *   authorization. This reading claims to split the difference between
 *   congressional primacy and executive inherence by acknowledging legitimate
 *   constraints on both branches — congress cannot rapidly mobilize for
 *   genuine emergencies, but the president cannot sustain major wars
 *   indefinitely without legislative backing. The constraint coordinates a
 *   real tension: the president's operational authority within
 *   imminent-threat bounds; congress's legislative check on sustained
 *   commitment. However, the reading creates an ambiguity zone between
 *   imminent and prolonged where both branches claim authority and neither
 *   has clear priority, which enables each to invoke the reading to justify
 *   actions the other opposes. The claimed type is tangled_rope because the
 *   constraint genuinely coordinates executive-legislative interaction (both
 *   branches benefit from the flexibility it provides) AND it creates
 *   asymmetric extraction (the president extracts interpretive discretion in
 *   the gray zone; congress's nominal authority is undercut by operational
 *   facts on the ground). The functional accommodation reading vindicates the
 *   doctrines of constitutional flexibility and contextual emergency
 *   authority, which serve as legitimating propositions for situations where
 *   neither pure primacy reading applies.
 *
 * KEY AGENTS:
 *   - presidency: holds unilateral imminent-threat authority and interpretive discretion over when authorization is required
 *   - congress: retains formal authorization authority for prolonged campaigns but faces structural disadvantage in the gray zone
 *   - military_subordinates: execute orders but face legal ambiguity about which authorization standard applies
 *   - affected_foreign_populations: bear costs of military action with no formal voice in authorization process
 *   - courts: observe the constraint but abstain through political question doctrine, reinforcing the ambiguity zone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.62).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.71).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation by Operational Context (Functional Accommodation Reading)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '94f13792-ba4c-45e4-ab07-7442a9f7188b').
narrative_ontology:cs_kernel_codification('94f13792-ba4c-45e4-ab07-7442a9f7188b', fixed_text).
narrative_ontology:cs_authority_grounding('94f13792-ba4c-45e4-ab07-7442a9f7188b', distributed).
narrative_ontology:cs_reading_relation('94f13792-ba4c-45e4-ab07-7442a9f7188b', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('94f13792-ba4c-45e4-ab07-7442a9f7188b', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('94f13792-ba4c-45e4-ab07-7442a9f7188b', foundational, contextual_flexibility_constitutionally_grounded).
narrative_ontology:cs_axiom_status(contextual_flexibility_constitutionally_grounded, holdable).
narrative_ontology:cs_axiom_grounding('94f13792-ba4c-45e4-ab07-7442a9f7188b', contextual_flexibility_constitutionally_grounded, instrumental).
narrative_ontology:cs_axiom('94f13792-ba4c-45e4-ab07-7442a9f7188b', foundational, executive_discretion_constrained_by_legislative_veto).
narrative_ontology:cs_axiom_status(executive_discretion_constrained_by_legislative_veto, holdable).
narrative_ontology:cs_axiom_grounding('94f13792-ba4c-45e4-ab07-7442a9f7188b', executive_discretion_constrained_by_legislative_veto, deontological).
narrative_ontology:cs_reference_frame('94f13792-ba4c-45e4-ab07-7442a9f7188b', constitutional_accommodation_of_executive_emergency_and_legislative_check).
narrative_ontology:cs_drift_state('94f13792-ba4c-45e4-ab07-7442a9f7188b', contemporary_extended_military_operations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94f13792-ba4c-45e4-ab07-7442a9f7188b', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, military_subordinates).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, affected_foreign_populations).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, constitutional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, contextual_emergency_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the president retains unilateral authority to respond to imminent threats and deploy forces for short-duration operations without prior congressional approval. For prolonged campaigns, the president must secure congressional authorization but retains interpretive discretion over what constitutes 'prolonged' and what triggers the authorization requirement. Benefits from operational speed in immediate crises; constrained by eventual authorization requirement for sustained operations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, presidency, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, presidency, beneficiary).

% Retains formal authority to authorize prolonged military campaigns and declare war, and can theoretically curtail executive action through defunding or legislation. However, faces structural disadvantage in the ambiguity zone: by the time a 'short' operation proves 'prolonged,' military facts on the ground constrain the congressional choice set. The reading benefits congress by preserving its constitutional prerogative while undermining its practical capacity to exercise it in the gray area.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress, beneficiary).

% Serve at the operational direction of both the commander-in-chief and the legislative body, but the constraint creates structural ambiguity about which authority legitimates their actions. In the imminent-threat zone, they follow executive orders. In the prolonged-campaign zone, they require (or should require) congressional mandate. In the gray area between, they face unresolved legitimacy questions and potential legal exposure if the authorization dispute is later resolved against the executive.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_subordinates, payer,
    moderate, biographical, trapped, global).

% Bear the costs of military action (injury, displacement, infrastructure destruction) with no formal voice in either executive or congressional deliberation, and no clarity on what legitimation standard applies to the force directed at them. In the ambiguity zone, they face force that may or may not carry the full constitutional legitimacy of either branch.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, affected_foreign_populations, payer,
    powerless, immediate, trapped, global).

% Formally adjudicate separation-of-powers disputes but have developed doctrine holding most war-powers questions non-justiciable (political question doctrine). They observe the constraint's operation but abstain from policing the executive-congressional boundary in the ambiguity zone, which reinforces the gray area's persistence.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, courts, observer,
    institutional, generational, constrained, national).

% Would have legitimate standing to demand clarity about the legitimacy of military action directed in their name, but is structurally excluded from the authorization process. Public debate about war powers occurs after-the-fact, constrained by operational necessity and military success narratives.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_opinion, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, presidency).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates military authority between branches to accommodate genuine operational constraints: executive speed for imminent threats; legislative authorization for sustained commitment. Both branches benefit from this allocation because it prevents deadlock (congress cannot rapidly mobilize for emergencies) while preserving legislative check (the president cannot wage indefinite war without authorization).
% TRANSFER_FUNCTION: Transfers interpretive discretion about what constitutes imminent threat from congress to the executive. The transfer is formal and reversible (congress can legislate categorical rules), but in practice the president's determination of imminence is decisive because military facts on the ground constrain congressional options.
% ABSENT_VOICES: Affected foreign populations, whose legal status is not recognized in either branch's authorization process. Domestic public opinion, which is excluded from initial authorization decisions. Military subordinates, who must obey orders but face legal exposure if the authorization is later disputed. Congressional minorities and allied-nation governments, whose interests are affected but whose voices are advisory at best.
% DISAPPEARANCE_RATIONALE: If the functional accommodation reading disappeared, the president would lose unilateral authority to respond to imminent threats, or congress would lose its formal authorization power. Either outcome would trigger constitutional recalibration and would alter the allocation of military authority fundamentally.
% FOUNDING_PROBLEM: The Constitution allocates war authority ambiguously: the president is commander-in-chief; congress holds power to declare war. Genuine emergencies require executive speed; major wars require legislative legitimacy. The founding problem is that pure congressional primacy is infeasible for imminent threats, and pure executive authority leaves congress with no lever.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars across the spectrum (e.g., David Barron, Marty Lederman, Jack Goldsmith) attest that genuine emergencies require faster decision-making than legislative process permits. However, critics (e.g., scholars arguing for congressional primacy such as Jules Lobel and many progressive constitutional scholars) attest that the gap is exaggerated and that congress CAN mobilize faster than commonly assumed, and that the emergency frame has been stretched to justify non-emergency operations. The founding problem remains contested because the empirical conditions under which congress actually CAN or CANNOT mobilize are not clearly established in the record.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the functional accommodation reading enables executive extraction in the gray zone: by controlling what counts as imminent vs. prolonged, the president can sustain significant military action without returning to congress for authorization. The extraction is not pure (congress retains nominal authority and can eventually force an authorization dispute) but it is real (the operational window for executive action is wide). Suppression is high (0.71) because the gray-zone ambiguity must be actively maintained: if the rules became categorical (imminent ONLY up to 30 days, for example), both branches would know what behavior was permitted, and the president's discretion would collapse. Theater ratio is moderate (0.48) because the functional accommodation language genuinely describes real constitutional constraints (emergencies do require speed; congress does provide essential legitimacy for major wars), but it also functions as cover for executive action that congress would dispute if forced to vote. The measurement series shows extractiveness rising sharply in the early period (0.48 to 0.61, t0 to t25) then plateauing, consistent with a pattern where gray-zone extraction accumulates as more military actions are framed as imminent-response situations until the constitutional accommodation doctrine stabilizes at a new equilibrium. Theater ratio follows the same trajectory, suggesting that as executives become more comfortable with the reading's flexibility, the performative dimension of citing emergency necessity becomes more routine. Suppression requirement (the active work needed to maintain the ambiguity zone) remains high and flat, indicating sustained institutional effort to prevent categorical rules from crystallizing. The coercion grid shows structural-level suppression (0.72 at t0, 0.74 at t40) higher than individual-level (0.50 at t0, 0.52 at t40): the constraint is maintained at the system level through institutional practice and judicial abstention, not through pressure on individual actors who might resist. Organizational resistance is high (0.78 at t0) and slightly declining, indicating congress and allied institutional actors continue to contest the gray zone even as they accommodate the basic framework.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat, the functional accommodation reading is genuine coordination: it provides the flexibility necessary to handle true emergencies while respecting congress's constitutional role. From congress's seat, the same reading is extraction: it transfers the president's interpretive discretion over the imminent/prolonged boundary while nominally preserving legislative authority that becomes harder to exercise once military operations are underway. Military subordinates experience unresolved legitimacy: they receive orders under one reading from the president and are asked to explain them under the other reading if congress later disputes authorization. Affected populations experience the constraint as pure coercion: they are subject to military force that may or may not carry full constitutional legitimacy. Courts experience the constraint as non-justiciable: they abstain from policing the boundary, which allows both branches to claim the reading supports their authority. The engine computes these divergences from the structural data: the president and congress are both agenda-setters with institutional power but different time horizons and exit options; military subordinates face trapped exit; affected populations are powerless with immediate time horizons; courts are observers. The reading's extractiveness is not constant across seats — it is higher from the executive seat (who benefits from discretion) and lower from the congress seat (nominal authority with diminished practical capacity), and highest from the military and foreign population seats (who experience coercion without resolution).
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency benefits from the functional accommodation reading (it secures unilateral authority in the imminent zone and interpretive control over the imminent/prolonged boundary) while congress benefits from the reading's existence (it preserves the constitutional principle that prolonged wars require authorization, even if that principle becomes harder to enforce in practice). Both branches are nominal beneficiaries but with inverted incentives: the president wants to maximize the imminent zone and minimize congressional oversight; congress wants to minimize the gray zone and establish clear authorization requirements. Military subordinates are victims in the structural sense: they execute orders but bear legal exposure if the authorization dispute is later resolved against the executive. Affected foreign populations are victims: they are subject to military action but have no formal voice in authorization decisions and no clarity on what constitutional legitimacy standard applies. The reading's persistence depends on neither branch being harmed enough to force a categorical resolution. If congress were harmed enough (repeated major wars without authorization), it would push for the congressional primacy reading. If the presidency were harmed enough (forced to withdraw operations or prosecute officers for unauthorized command), it would push for the inherent executive reading. The functional accommodation reading's extraction is distributed across a powerless or trapped class (military and foreign populations) and a moderately powerful organizing class (congress, which has institutional resources but faces collective-action problems in mobilizing its authority during crises).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: does the functional accommodation reading serve its founding purpose (accommodating genuine operational constraints while preserving legislative check), or has the founding problem been solved while the accommodation persists? The reading avoids misclassification as pure rope (coordination without extraction) because the gray zone enables executive extraction; it avoids misclassification as pure snare (extraction without coordination function) because the authorization requirement for prolonged campaigns genuinely constrains executive authority and congress does eventually mobilize its power. The tangled_rope classification is structurally correct: both coordination (the allocation of authorities to accommodate real constraints) and extraction (the president's discretionary control over the imminent/prolonged boundary) are present. However, mandatrophy risk is real: if operational conditions changed such that genuine imminent threats became rare and most military action became prolonged-campaign situations, the coordination function would atrophy while the extraction persisted, and the constraint would degrade toward pure snare. The empirical trigger for mandatrophy: do imminent-threat operations actually remain proportionally rare (supporting the coordination function), or have they become the dominant frame for military action (suggesting the accommodation is mostly extraction cover)? This is resolvable but unresolved empirically — congressional scholars and executive officers disagree on whether recent operations (military interventions in Libya, Syria, Iraq follow-on operations) fit the imminent-threat accommodation or abuse it. The measurement series at t25-t30 (the inflection point where extractiveness plateaus) may represent the point at which the accommodating function stabilized and the constraint transitioned from growing extraction to stable gray-zone operation — a piton-like state where both branches accept the reading as functional myth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_vs_prolonged_boundary,
    'Is the boundary between imminent threat and prolonged campaign a principled constitutional distinction, or a contextual frame that shifts to accommodate whatever military action the executive wants to take?',
    'Comparative analysis of how imminent and prolonged have been applied across cases: if the classification is stable and predictable (e.g., actions over 30 days consistently treated as prolonged), the distinction is principled; if the classification varies to accommodate executive preferences (e.g., the same duration treated as imminent in one case and prolonged in another), the frame is opportunistic.',
    'If the boundary is principled, the functional accommodation reading genuinely coordinates executive and legislative authority. If the boundary is opportunistic, the reading is mostly extraction cover, and the constraint should be reclassified as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminent_vs_prolonged_boundary, empirical, 'Whether the imminent/prolonged distinction is constitutionally grounded or operationally manipulated.').

omega_variable(
    gray_zone_beneficiary_asymmetry,
    'In the gray zone (operations long enough that congress would dispute the imminent-threat claim but short enough that the president can complete them without authorization), who extracts benefit: the president (through discretionary action), congress (through preserved nominal authority), or both?',
    'Case-study analysis of major military actions in the gray zone: for each action, did the executive gain a military advantage by moving faster than the congressional authorization process would have permitted, and did congress later attempt to reassert authority (suggesting it viewed itself as harmed)?',
    'If the president consistently extracts speed advantages and congress consistently attempts to reassert authority, the reading enables asymmetric extraction and the snare-risk omega becomes high-confidence. If both branches benefit (executive speed for real time-sensitive operations, congress''s ultimate authority for major policy), the coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_beneficiary_asymmetry, empirical, 'Whether gray-zone operations produce net extraction to the executive or genuine coordination.').

omega_variable(
    constitutional_codification_vs_practice_drift,
    'Is the functional accommodation reading a principled constitutional doctrine that could be codified into clear rules (e.g., executive authority for operations under 60 days, congressional authorization required beyond), or does it rest on interpretive flexibility that resists codification?',
    'Attempt to translate the reading''s logic into statutory war-powers authorization: if a clear rule can be derived, the reading is codifiable and stable; if attempts to codify produce pushback from both branches, the reading''s flexibility is functionally necessary and codification would disrupt the accommodating balance.',
    'If codifiable, the reading could be made precise and would resist gaming. If uncodifiable, the reading''s flexibility is its essence, and the constraint will remain ambiguous and contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_codification_vs_practice_drift, conceptual, 'Whether the functional accommodation is a stable doctrine or depends on interpretive flexibility to persist.').

omega_variable(
    structural_suppression_of_categorical_rules,
    'What prevents congress from simply legislating a clear war-powers rule (e.g., all force beyond 30 days requires authorization) if it genuinely wants to constrain executive action? Is the barrier constitutional (congress cannot impose such limits without amending the Constitution), institutional (congress lacks the coordination to pass such legislation), or procedural (the president can veto such legislation)?',
    'Track congressional attempts to legislate war-powers constraints and the executive responses. If congress passes legislation and the executive ignores it, the barrier is constitutional or political. If congress fails to pass legislation, the barrier is internal to congress.',
    'If the barrier is constitutional, the functional accommodation reading is grounded in structural necessity. If the barrier is political (congress lacks the discipline), the suppression of categorical rules is active coercion, not accommodation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_suppression_of_categorical_rules, empirical, 'What mechanisms prevent categorical war-powers rules from crystallizing.').

omega_variable(
    kernel_reading_contention,
    'Do the three sibling readings (congressional_primacy, functional_accommodation, inherent_executive) represent genuinely distinct constitutional interpretations, or are they post-hoc rationalizations of executive action and congressional reaction?',
    'Historical analysis: trace when each reading was explicitly articulated as a constitutional claim (vs. merely as a justification for action after-the-fact). If all three readings emerged after major military actions were already underway, they are rationalizations. If readings were articulated in advance (e.g., in legal scholarship or official opinions before cases arose), they are genuine constitutional theories.',
    'If the readings are genuine constitutional theories, the kernel is a contested constitutional question where the three readings legitimately compete. If the readings are rationalizations, the kernel is an exercise of raw power dressed in constitutional language, and all three constraint stories should be reclassified as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether the three war-powers readings are genuine constitutional theories or post-hoc rationalizations of power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t5, war_powers_allocation__functional_accommodation_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(war__tr_t5, observed).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t15, war_powers_allocation__functional_accommodation_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(war__tr_t15, observed).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t25, war_powers_allocation__functional_accommodation_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement_basis(war__tr_t25, observed).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(war__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t5, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(war__be_t5, observed).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t15, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(war__be_t15, observed).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t25, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(war__be_t25, observed).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(war__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t5, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(war__su_t5, observed).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t15, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(war__su_t15, observed).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t25, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(war__su_t25, observed).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(war__su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(war__grid_01, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(war__grid_02, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(class), 40, 0.58).
narrative_ontology:measurement(war__grid_03, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(individual), 0, 0.5).
narrative_ontology:measurement(war__grid_04, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(individual), 40, 0.52).
narrative_ontology:measurement(war__grid_05, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(war__grid_06, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(organizational), 40, 0.52).
narrative_ontology:measurement(war__grid_07, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(war__grid_08, war_powers_allocation__functional_accommodation_reading, accessibility_collapse(structural), 40, 0.65).
narrative_ontology:measurement(war__grid_09, war_powers_allocation__functional_accommodation_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(war__grid_10, war_powers_allocation__functional_accommodation_reading, resistance(class), 40, 0.73).
narrative_ontology:measurement(war__grid_11, war_powers_allocation__functional_accommodation_reading, resistance(individual), 0, 0.6).
narrative_ontology:measurement(war__grid_12, war_powers_allocation__functional_accommodation_reading, resistance(individual), 40, 0.58).
narrative_ontology:measurement(war__grid_13, war_powers_allocation__functional_accommodation_reading, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(war__grid_14, war_powers_allocation__functional_accommodation_reading, resistance(organizational), 40, 0.76).
narrative_ontology:measurement(war__grid_15, war_powers_allocation__functional_accommodation_reading, resistance(structural), 0, 0.71).
narrative_ontology:measurement(war__grid_16, war_powers_allocation__functional_accommodation_reading, resistance(structural), 40, 0.73).
narrative_ontology:measurement(war__grid_17, war_powers_allocation__functional_accommodation_reading, stakes_inflation(class), 0, 0.6).
narrative_ontology:measurement(war__grid_18, war_powers_allocation__functional_accommodation_reading, stakes_inflation(class), 40, 0.63).
narrative_ontology:measurement(war__grid_19, war_powers_allocation__functional_accommodation_reading, stakes_inflation(individual), 0, 0.55).
narrative_ontology:measurement(war__grid_20, war_powers_allocation__functional_accommodation_reading, stakes_inflation(individual), 40, 0.58).
narrative_ontology:measurement(war__grid_21, war_powers_allocation__functional_accommodation_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(war__grid_22, war_powers_allocation__functional_accommodation_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(war__grid_23, war_powers_allocation__functional_accommodation_reading, stakes_inflation(structural), 0, 0.73).
narrative_ontology:measurement(war__grid_24, war_powers_allocation__functional_accommodation_reading, stakes_inflation(structural), 40, 0.75).
narrative_ontology:measurement(war__grid_25, war_powers_allocation__functional_accommodation_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(war__grid_26, war_powers_allocation__functional_accommodation_reading, suppression(class), 40, 0.7).
narrative_ontology:measurement(war__grid_27, war_powers_allocation__functional_accommodation_reading, suppression(individual), 0, 0.5).
narrative_ontology:measurement(war__grid_28, war_powers_allocation__functional_accommodation_reading, suppression(individual), 40, 0.52).
narrative_ontology:measurement(war__grid_29, war_powers_allocation__functional_accommodation_reading, suppression(organizational), 0, 0.6).
narrative_ontology:measurement(war__grid_30, war_powers_allocation__functional_accommodation_reading, suppression(organizational), 40, 0.62).
narrative_ontology:measurement(war__grid_31, war_powers_allocation__functional_accommodation_reading, suppression(structural), 0, 0.72).
narrative_ontology:measurement(war__grid_32, war_powers_allocation__functional_accommodation_reading, suppression(structural), 40, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.22).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three constraint stories, one per sibling reading. The functional_accommodation_reading is the moderate position between congressional_primacy (congress-centric) and inherent_executive (president-centric). The three stories are linked because each reading's validity depends on refuting the others' core premises. The functional_accommodation_reading influences both siblings by establishing the imminent/prolonged distinction as constitutionally grounded; siblings must either accept the distinction or argue it lacks constitutional status. Each story has a distinct epsilon: congressional_primacy_reading emphasizes extracted executive discretion (higher epsilon); inherent_executive_reading emphasizes congressional frustration of legitimate presidential authority (lower epsilon from the president's seat); functional_accommodation_reading splits the difference (moderate epsilon). The omegas in each story document the irreducible uncertainties about which reading the Constitution actually endorses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
