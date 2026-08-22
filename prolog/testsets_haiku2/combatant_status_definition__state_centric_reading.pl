% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Criterion (Geneva Conventions Article 4)
 *   domain: international humanitarian law / law of armed conflict
 *
 * SUMMARY:
 *   The state-centric reading of combatant status asserts that only formal
 *   state militaries meeting Article 4 Geneva Convention criteria (fixed
 *   distinctive emblem, responsible command structure, carrying arms openly,
 *   conducting operations in compliance with laws of war) qualify as lawful
 *   combatants entitled to prisoner-of-war protections. Non-state armed
 *   groups are categorically excluded, regardless of their organizational
 *   discipline, adherence to humanitarian law, or political legitimacy of
 *   their cause. This reading codifies into law the proposition that
 *   combatant immunity and POW status are privileges of state actors, not
 *   rights extended to organized non-state fighters. The constraint's
 *   structural operation is tangled: it coordinates a bright-line rule
 *   (reducing discretion in conflict classification) while simultaneously
 *   extracting legal immunity from non-state fighters and transferring
 *   prosecutorial authority to states. The measurement trajectory shows
 *   extractiveness rising over the interval as insurgencies and non-state
 *   armed groups proliferated (asymmetric warfare becoming the modal conflict
 *   form), making the categorical exclusion increasingly extractive in
 *   practice even as the formal rule remained constant.
 *
 * KEY AGENTS:
 *   - State militaries — institutional beneficiaries; receive combatant immunity and POW protections under the constraint
 *   - Non-state armed groups — moderate-power payers; face categorical exclusion from combatant status regardless of organization or behavior
 *   - Detained non-state fighters — powerless victims; lose access to POW protections, face criminal prosecution, subject to extended detention and interrogation
 *   - State prosecution authorities — institutional agenda-setters; maintain prosecutorial jurisdiction over non-state fighters as the constraint's enforcement mechanism
 *   - International humanitarian law interpreters (ICRC, ICJ, scholars) — analytical observers; interpret the constraint's application and adjudicate edge cases
 *   - Occupying or colonial states — structurally excluded; would be constrained by sibling readings (AP I Article 1(4)) but are not addressed by this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.82).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.76).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Criterion (Geneva Conventions Article 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international humanitarian law / law of armed conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '5cfa3d8f-9564-4883-bb0d-7deaf138e85d').
narrative_ontology:cs_kernel_codification('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', fixed_text).
narrative_ontology:cs_authority_grounding('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', extraction).
narrative_ontology:cs_interpretation_layer_present('5cfa3d8f-9564-4883-bb0d-7deaf138e85d').
narrative_ontology:cs_reading_relation('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', foundational, only_state_militaries_qualify_combatant_status).
narrative_ontology:cs_axiom_status(only_state_militaries_qualify_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', only_state_militaries_qualify_combatant_status, conventional).
narrative_ontology:cs_axiom('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', foundational, non_state_actors_excluded_regardless_of_functional_equivalence).
narrative_ontology:cs_axiom_status(non_state_actors_excluded_regardless_of_functional_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', non_state_actors_excluded_regardless_of_functional_equivalence, deontological).
narrative_ontology:cs_reference_frame('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', state_sovereignty_combatant_immunity_doctrine).
narrative_ontology:cs_drift_state('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', contemporary_asymmetric_warfare_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5cfa3d8f-9564-4883-bb0d-7deaf138e85d', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_prosecution_authorities).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, detained_non_state_fighters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed forces of recognized states that meet Article 4 criteria (uniform, fixed emblem, command responsible for subordinates' conduct, carrying arms openly, conducting operations in compliance with laws of war) are entitled to combatant immunity and prisoner-of-war status upon capture. Their members are protected from prosecution for lawful acts of war, have right to humane treatment, medical care, and repatriation. The constraint codifies their privileged legal status globally.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations such as liberation movements, insurgencies, and armed groups are categorically excluded from combatant status under the state-centric reading regardless of their organizational structure, command discipline, or adherence to laws of war. Members who are captured face classification as criminals or war criminals under domestic or international law. The group's political legitimacy — whether they fight occupation, for independence, or for other causes — does not alter the legal consequence: non-state status means no combatant immunity.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, identity_locked, regional).

% Upon capture, members of non-state armed groups lose access to combatant immunity and prisoner-of-war status. They are interrogated as criminal suspects, may be denied access to legal counsel, are subject to domestic criminal prosecution under the capturing state's laws, and are incarcerated without the enhanced protections afforded to state military POWs (protection from enhanced interrogation, right to medical care, right to fair and speedy trial). They can only exit through state pardon, ceasefire agreements, or regime change.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detained_non_state_fighters, payer,
    powerless, immediate, trapped, local).

% Law enforcement and military justice officials of states maintain prosecutorial jurisdiction over captured non-state fighters as ordinary criminals under domestic law. The constraint (categorical exclusion of non-state actors from combatant status) is what creates and sustains their legal authority to prosecute. They can demand enhanced interrogation, deny due process protections, extend detention beyond what international law permits for POWs, and apply domestic criminal penalties.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_prosecution_authorities, agenda_setter,
    institutional, generational, analytical, national).

% The International Committee of the Red Cross, international courts, and legal academic communities interpret and apply the constraint. They assess whether specific non-state groups meet Article 4 criteria (a technical question even if the answer is always negative under the state-centric reading), monitor state compliance with the rule, and occasionally advocate for functional equivalence to override formal exclusion. Their influence is advisory; enforcement remains with states.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_humanitarian_law_institutions, observer,
    institutional, generational, analytical, global).

% States engaged in occupation or colonialism are the primary actors affected by AP I Article 1(4), which would extend combatant status to non-state groups fighting for self-determination or against occupation. The state-centric reading does not directly address them, but its enforcement (maintaining categorical exclusion) constrains their ability to grant combatant status even to non-state groups that meet behavioral criteria. They are excluded from this reading's framework because the reading forecloses their ability to recognize non-state combatants.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, occupying_or_colonial_states, excluded,
    institutional, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_prosecution_authorities).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, objective, global standard for determining combatant status that minimizes discretion: a combatant is someone who belongs to a formal state military, wears a distinctive emblem, is part of a responsible command structure, carries arms openly, and conducts operations in compliance with laws of war. This objective standard is meant to prevent arbitrary classification and provide certainty about who qualifies for combatant protections versus who is subject to criminal prosecution.
% TRANSFER_FUNCTION: Transfers combatant immunity (protection from prosecution for lawful acts of war) and prisoner-of-war status (rights to humane treatment, medical care, repatriation) from all non-state armed actors to state militaries exclusively. Non-state fighters lose access to these protections and combatant immunity, and are subjected to criminal prosecution under domestic law. Prosecution authorities gain legal authority to charge and try non-state fighters as criminals.
% ABSENT_VOICES: Non-state armed groups and liberation movements that meet functional combatant criteria but lack state status are structurally excluded from this reading's framework. They would argue for functional equivalence to override formal state requirement, citing the behavioral criteria they meet. Detained non-state fighters themselves have no voice in the constraint's application; their legal status is determined by states without consultation or representation. International humanitarian law advocates supporting recognition of non-state combatant status (particularly in liberation struggles) are also absent from the state-centric reading's legal structure.
% DISAPPEARANCE_RATIONALE: If the state-centric constraint disappeared, non-state armed groups would become eligible for combatant status if they met functional organizational criteria. The legal landscape would shift dramatically: detained fighters from non-state groups would access prisoner-of-war status, combatant immunity, and Geneva protections; prosecution authorities would lose categorical criminal jurisdiction; and legal determinations of combatant status would shift from rule-based (state membership) to factual (organization, command, laws of war compliance). The entire ecosystem of domestic prosecutions for non-state fighters would require legal reconfiguration. The world absolutely rearranges.
% FOUNDING_PROBLEM: After World War II and the 1949 Geneva Convention, states needed an objective criterion for combatant status to prevent arbitrary detention and legal uncertainty in armed conflict. Ambiguous classifications of who qualifies as a lawful combatant created risk of summary execution, indefinite detention without clear legal status, and conflicts over prisoners' rights. A formal, organizational criterion — membership in a state military — was designed to provide a bright-line rule that any observer could apply without requiring political judgment about the legitimacy of different armed actors.
% FOUNDING_PROBLEM_CORROBORATION: State military and prosecution authorities attest that the founding problem remains live: formal state requirement prevents ambiguity and provides objective clarity about who qualifies for protections. They argue functional criteria would invite disputes and gaming. International humanitarian law scholars and the ICRC attest the founding problem is substantially solved: the behavioral criteria (organization, command, laws of war compliance) are well-established and applied in practice; the requirement for state membership adds no clarity, only categorical exclusion of functionally equivalent non-state groups. No voice outside state prosecution authorities supports the categorical formal requirement as necessary to solve ambiguity; independent assessments suggest it serves extraction rather than coordination.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint systematically denies a class of actors (non-state groups) a status (combatant immunity) that another class (states) enjoys, regardless of functional equivalence in organization or behavior. The extraction is asymmetric: states retain their protections; non-state fighters lose access to the same protections even if they organize, command-control, and comply with laws of war in identical ways. The rising trajectory reflects the increasing practical cost of the categorical exclusion as non-state conflicts became statistically dominant: each new insurgency, liberation movement, or organized armed group that meets functional combatant criteria faces the same legal exclusion, making the extracted value (legal immunity differentials) accumulate. Suppression is high (0.76) because the constraint's persistence depends on active enforcement — states must maintain the rule categorically, courts must apply it to deny POW status to non-state fighters, and domestic prosecutors must sustain jurisdiction over non-state detainees. Theater ratio is moderate-low (0.28) because the formal criterion (state military meeting Article 4 requirements) has a genuine organizational basis, but an increasing fraction of enforcement effort goes to maintaining the categorical exclusion despite mounting functional equivalence with non-state organized groups. Accessibility of alternatives is moderately high (0.71) because non-state actors could theoretically submit to state authority or seek diplomatic recognition, but identity-locking (commitment to armed struggle, political cause) makes exit nearly impossible for the payer seats.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (state militaries and prosecution authorities) experiences this constraint as legitimate coordination — a clear rule that prevents ambiguity about who counts as a combatant and who faces criminal prosecution. The rule is necessary for state sovereignty and rule of law. From the payer seats (non-state fighters), the constraint is extractive: identical organization and behavior produces opposite legal consequences based solely on state status, and the categorical exclusion forecloses recourse to humanitarian protections. The engine should compute substantially different type classifications across these seats: state militaries should compute toward rope (benefiting from a coordination rule that protects them), while non-state fighters should compute toward snare (trapped in a rule that excludes them categorically and sustains their prosecution).
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries have d near 0.0 (full beneficiary): they collect combatant immunity, POW status, and protection from criminal prosecution as a structural benefit of the rule. They have exit options (arbitrage: they can conduct warfare at sea, use private contractors, or appeal to international law) that further reduce d. State prosecution authorities have d near 0.1 (beneficiary with enforcement burden): they gain prosecutorial jurisdiction but must maintain the rule's enforcement. Non-state armed groups have d near 0.95 (nearly full target): they are systematically denied a status that structurally equivalent state actors retain, and their exit options are severely constrained by identity-locking (they cannot cease being members of their political cause or armed group without dissolving their identity). Detained non-state fighters have d = 1.0 (full target): they are trapped (immediate horizon, no exit), powerless, and subject to criminal prosecution with no immunity or status recognition. The functional equivalence between state and non-state organized combatants is not reflected in directionality — the constraint's formal structure (state requirement) creates the asymmetry that the directionality derivation captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows clear mandatrophy: the founding problem (ambiguity about combatant status, need for a bright-line rule to prevent discretionary detention) is substantially solved by the time-series midpoint. State prosecutors and militaries have clear rules; they know who qualifies. But the constraint persists, and extractiveness rises, because the rule now serves as a mechanism for denying legal protections to non-state fighters rather than as a solution to ambiguity. The measured rise in theater_ratio (from 0.12 to 0.28) and suppression_requirement (from 0.58 to 0.76) while functionality (disambiguation of combatant status) plateaus is diagnostic of mandatrophy: the constraint's nominal purpose is achieved, but active enforcement machinery is maintained and expanded to sustain a secondary benefit (extraction of prosecutorial authority over non-state fighters). The rising disappearance_verdict (world_rearranges) paired with dead or contested founding_problem_status would trigger mandatrophy reclassification per OQ-83 R5 logic, as the founding problem is no longer the driver.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_equivalence_vs_formal_status,
    'If a non-state armed group meets all the behavioral criteria of Article 4 (fixed emblem, command structure, carrying arms openly, compliance with laws of war) functionally, should the formal requirement of state sponsorship override that functional equivalence, or does functional equivalence establish de facto combatant status regardless of state structure?',
    'Examination of state practice in conflicts involving highly organized non-state groups (Hamas, PKK, YPG, etc.) to determine whether states extend POW-equivalent protections despite denying formal combatant status, or whether the categorical exclusion holds regardless of functional equivalence. Analysis of ICRC guidance and case law on Common Article 3 extension to non-state fighters.',
    'If functional equivalence is treated as sufficient by states in practice, the constraint''s extractiveness would be reclassified downward (the rule is not enforced as stated). If the categorical exclusion is strictly maintained despite functional equivalence, extractiveness remains high and the mandatrophy risk increases (the rule persists despite its original rationale being achieved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_equivalence_vs_formal_status, empirical, 'Whether combatant status is determined by formal state structure or by functional organizational equivalence to state militaries.').

omega_variable(
    article_1_4_applicability_domestically,
    'Does Additional Protocol I Article 1(4), which extends combatant recognition to non-state groups fighting colonial/occupation/racist regimes, apply as a matter of binding international law, or is it subject to state reservation/non-ratification, effectively allowing states to maintain the categorical exclusion?',
    'Survey of state ratification and reservation status of AP I; examination of international court rulings (ICJ, ICC) on whether AP I Article 1(4) applies domestically within states that have ratified, and whether customary international law has adopted the Article 1(4) standard independent of treaty.',
    'If AP I 1(4) is binding customary law, the state-centric reading is foreclosed in principle and non-state groups fighting occupation do qualify for combatant status; the constraint should be reclassified to the national_liberation_reading. If AP I 1(4) is optional or non-binding domestically, states can maintain categorical exclusion under the state-centric reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_1_4_applicability_domestically, empirical, 'Whether AP I Article 1(4) combatant extension is binding law or subject to state opt-out.').

omega_variable(
    extraction_vs_coordination_separation,
    'Can the coordinate function (clear rule preventing discretionary combatant status) be achieved through a functional criteria test (organization, command, laws of war compliance) rather than through formal state requirement, or does the state requirement provide necessary precision that a functional test cannot?',
    'Analysis of conflicts where functional criteria are applied to non-state groups and tracked for discretionary abuse, false positives, litigation costs, and ambiguity relative to conflicts under state-only criteria. Assessment of whether functional tests produce workable bright-line rules in practice.',
    'If functional criteria provide sufficient clarity without state requirement, the extraction component (denial of combatant status to non-state groups) can be cleanly separated from the coordination component (clear rule for determining status). This would support the functional_protection_reading and reclassify the state-centric reading as pure extraction rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_separation, empirical, 'Whether the state-requirement component is necessary to achieve the coordination function or whether it serves extractive purposes independent of coordination.').

omega_variable(
    identity_locking_mechanism_interpersonal,
    'For non-state fighters in the payer seat, what portion of the measured suppression (0.76) is structural (legal barriers, lack of state recognition) versus internalized (fighters'' commitment to the cause making exit psychologically or politically unthinkable even if legal barriers were removed)?',
    'Post-conflict case analysis: when combatants from non-state groups transition after conflict ceases or when they defect, do they face structural prosecution/imprisonment, or does commitment to the group/cause persist even in the absence of external enforcement? Assessment of reintegration barriers.',
    'If suppression is primarily structural, removing the categorical exclusion would lower the payer seat''s effective extraction significantly. If suppression is substantially internalized (identity fusion with the armed group), removing the formal rule would not fully restore the payer''s exit options; suppression would persist post-exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_mechanism_interpersonal, empirical, 'Whether suppression of non-state combatants is structural legal exclusion or identity-fused commitment.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Between the state-centric reading and the national_liberation reading, is there genuine logical foreclosure (one reading''s core premise rules out the other''s in a single framework), or do they coexist as competing interpretations of AP I held by different state parties and non-state actors?',
    'Textual analysis of Article 4 and Article 1(4) of the Geneva Conventions and AP I to determine whether they are logically contradictory (foreclosure) or can coexist with different scope conditions (coexistence). Survey of state positions and declarations on whether AP I 1(4) modifies or overrides Article 4 for the categories it covers.',
    'If foreclosure: the state-centric reading''s axiom (only formal state militaries qualify) is in direct logical contradiction with the national_liberation reading''s axiom (organized non-state groups fighting liberation wars qualify). This would require one reading to be formally rejected. If coexistence: both readings are live, applied by different parties in different contexts (states that reject AP I maintain state-centric; states ratifying AP I apply 1(4) to non-state groups in their context).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether the state-centric and national_liberation readings are logically contradictory or can coexist with different scope conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comb_tr_t0, observed).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__state_centric_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(comb_tr_t10, observed).
narrative_ontology:measurement(comb_tr_t25, combatant_status_definition__state_centric_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement_basis(comb_tr_t25, observed).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__state_centric_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(comb_tr_t40, observed).
narrative_ontology:measurement(comb_tr_t55, combatant_status_definition__state_centric_reading, theater_ratio, 55, 0.27).
narrative_ontology:measurement_basis(comb_tr_t55, observed).
narrative_ontology:measurement(comb_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement_basis(comb_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(comb_be_t0, observed).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__state_centric_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(comb_be_t10, observed).
narrative_ontology:measurement(comb_be_t25, combatant_status_definition__state_centric_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(comb_be_t25, observed).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__state_centric_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement_basis(comb_be_t40, observed).
narrative_ontology:measurement(comb_be_t55, combatant_status_definition__state_centric_reading, base_extractiveness, 55, 0.81).
narrative_ontology:measurement_basis(comb_be_t55, observed).
narrative_ontology:measurement(comb_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.82).
narrative_ontology:measurement_basis(comb_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comb_su_t0, observed).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__state_centric_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(comb_su_t10, observed).
narrative_ontology:measurement(comb_su_t25, combatant_status_definition__state_centric_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(comb_su_t25, observed).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__state_centric_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comb_su_t40, observed).
narrative_ontology:measurement(comb_su_t55, combatant_status_definition__state_centric_reading, suppression_requirement, 55, 0.75).
narrative_ontology:measurement_basis(comb_su_t55, observed).
narrative_ontology:measurement(comb_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.76).
narrative_ontology:measurement_basis(comb_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel decomposes into three reading-specific constraints: (1) state_centric_reading (this story) asserts only state militaries qualify for combatant status; (2) national_liberation_reading extends status to non-state groups meeting AP I 1(4) criteria in liberation wars; (3) functional_protection_reading bypasses combatant status by providing Common Article 3 protections to all detainees. Each reading has different ε values because the standing arrangement under contest (what counts as lawful combatant status) is assessed differently by each reading's lights. The state-centric reading authors high ε for non-state fighters (no immunity, criminal prosecution) and low ε for state militaries (full immunity). The national_liberation reading would author lower ε for qualifying non-state groups. The functional reading would author low ε across all detainees (everyone gets minimum protections). The three readings coexist as live positions held by different state and non-state parties; they are linked here to model the kernel-level contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
