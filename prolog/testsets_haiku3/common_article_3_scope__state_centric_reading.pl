% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: CA3 Threshold Gatekeeping (State-Centric Reading)
 *   domain: international humanitarian law / armed conflict
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of Common Article
 *   3's scope. CA3 of the Geneva Conventions purports to establish
 *   humanitarian protections for victims of armed conflict not of an
 *   international character. The state-centric reading interprets 'armed
 *   conflict' narrowly: conflict only triggers CA3 when it crosses intensity
 *   thresholds (sustained armed operations, battle-related death toll) AND
 *   organization thresholds (command structure, uniforms, combatant
 *   discipline). Below these thresholds, the state-centric reading holds,
 *   conflict remains classified as law enforcement or criminal violence, and
 *   CA3 does not apply. This gatekeeping retains for governments maximum
 *   discretion over when humanitarian law obligations activate. Irregular
 *   combatants in low-intensity operations are excluded from CA3's victim
 *   set, receiving neither prisoner-of-war status nor humanitarian
 *   protection. The constraint's persistence depends on active state
 *   enforcement of the threshold interpretation against alternative readings
 *   (the expansive human-rights reading, which applies CA3 as a humanitarian
 *   floor regardless of intensity; the customary-law reading, which derives
 *   CA3 scope from state practice).
 *
 * KEY AGENTS:
 *   - state_militaries: Sets and administers the threshold interpretation; retains operational discretion below the official threshold line.
 *   - government_law_enforcement: Benefits from below-threshold classification (can use law-enforcement tools without CA3 constraints).
 *   - irregular_combatants_below_threshold: Excluded from CA3 victim set; no prisoner-of-war status, no protected status as wounded; trapped between criminal law and humanitarian law.
 *   - low_intensity_conflict_populations: Civilians in below-threshold conflicts; receive no humanitarian protections because state has gatekept them out of 'armed conflict' classification.
 *   - icrc_and_humanitarian_advocates: Would expand CA3 scope; actively suppressed by state-centric threshold enforcement.
 *   - international_courts_and_tribunals: Interpret CA3 in specific cases but structurally dependent on state cooperation; gradual jurisprudential drift toward human-rights reading, but limited enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.78).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.89).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, snare).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "CA3 Threshold Gatekeeping (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international humanitarian law / armed conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'b819ac12-d3c0-4d78-a086-df28d9c34d4e').
narrative_ontology:cs_kernel_codification('b819ac12-d3c0-4d78-a086-df28d9c34d4e', fixed_text).
narrative_ontology:cs_authority_grounding('b819ac12-d3c0-4d78-a086-df28d9c34d4e', extraction).
narrative_ontology:cs_interpretation_layer_present('b819ac12-d3c0-4d78-a086-df28d9c34d4e').
narrative_ontology:cs_reading_relation('b819ac12-d3c0-4d78-a086-df28d9c34d4e', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b819ac12-d3c0-4d78-a086-df28d9c34d4e', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('b819ac12-d3c0-4d78-a086-df28d9c34d4e', foundational, intensity_organization_thresholds_legitimate_gatekeeping).
narrative_ontology:cs_axiom_status(intensity_organization_thresholds_legitimate_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('b819ac12-d3c0-4d78-a086-df28d9c34d4e', intensity_organization_thresholds_legitimate_gatekeeping, conventional).
narrative_ontology:cs_axiom('b819ac12-d3c0-4d78-a086-df28d9c34d4e', foundational, state_discretion_in_threshold_assessment).
narrative_ontology:cs_axiom_status(state_discretion_in_threshold_assessment, holdable).
narrative_ontology:cs_axiom_grounding('b819ac12-d3c0-4d78-a086-df28d9c34d4e', state_discretion_in_threshold_assessment, instrumental).
narrative_ontology:cs_reference_frame('b819ac12-d3c0-4d78-a086-df28d9c34d4e', state_monopoly_on_conflict_classification).
narrative_ontology:cs_drift_state('b819ac12-d3c0-4d78-a086-df28d9c34d4e', contemporary_asymmetric_warfare_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('b819ac12-d3c0-4d78-a086-df28d9c34d4e', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, government_law_enforcement).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, low_intensity_conflict_populations).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, domestic_armed_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, domestic_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, intensity_and_organization_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed forces of recognized states. Interpret and apply CA3 thresholds to their own operations. Retain maximum operational discretion by keeping conflicts below the officially-declared intensity/organization floor. Control the assessment mechanism that determines whether conflict has crossed the threshold and CA3 obligations have activated. Benefit from below-threshold classification by avoiding prisoner-of-war status and medical immunity obligations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_militaries, agenda_setter,
    institutional, generational, analytical, national).

% Police, internal security forces, border guards operating under state authority. Operate under domestic law only when conflict remains below CA3 thresholds. Retain the option to use law-enforcement tools (arrest, detention, interrogation without humanitarian protections) rather than applying prisoner-of-war and wounded combatant treatment. Below-threshold classification allows law-enforcement response to armed groups without triggering humanitarian law constraints.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, government_law_enforcement, beneficiary,
    institutional, biographical, analytical, national).

% Armed individuals or groups whose operations do not cross state-defined intensity thresholds (battle-related death toll, sustained combat operations) or organization thresholds (command structure, uniforms, combatant discipline). Excluded from CA3's victim set: they receive no prisoner-of-war status, no medical immunity, no protection against summary execution or torture. Trapped between criminal law (which treats armed resistance as crime) and humanitarian law (which does not apply to them). No legitimate exit path: laying down arms leaves them exposed to criminal prosecution for past armed activity.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, biographical, trapped, local).

% Civilians living in areas where the state classifies operations as below-threshold: counterinsurgency, border skirmishes, law-enforcement actions against armed groups. Receive no humanitarian law protections (medical immunity, shelter, food) because the state has gatekept them out of 'armed conflict' classification. Receive no domestic criminal law protections because military/security forces operate in combat mode outside normal civilian jurisdiction. Trapped between two legal systems, protected by neither.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, low_intensity_conflict_populations, payer,
    powerless, immediate, trapped, local).

% Non-state armed organizations (insurgencies, militias, liberation movements, armed wings of political parties). Must demonstrate both intensity AND organization to trigger CA3 application and combatant protections. Their political/ideological identity is fused with armed resistance: exit means abandoning both the armed struggle and the identity that defines group membership. Even when operations cross thresholds, their combatants often receive weaker protections than state soldiers in state-centric interpretations. Benefit marginally when thresholds are crossed (combatant status granted); pay heavily when below-threshold classification applies (no protections).
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, domestic_armed_groups, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, domestic_armed_groups, beneficiary).

% International Committee of the Red Cross, human rights organizations, humanitarian law scholars, advocacy bodies. Advocate for CA3 application as a humanitarian floor regardless of intensity/organization thresholds. Excluded from threshold-setting decisions: states set the thresholds and assess compliance. Their input is solicited in academic and institutional forums, but gatekeeping authority remains with states. Their alternative reading (expansive human-rights reading) is actively suppressed by state-centric enforcement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_and_humanitarian_advocates, excluded,
    organized, generational, constrained, global).

% International Criminal Court, ad-hoc tribunals (ICTY, ICTR, SCSL), treaty monitoring bodies, regional human rights courts. Interpret and apply CA3 in specific cases and aggregate jurisprudence. Structurally dependent on state cooperation for enforcement and jurisdiction: limited ability to override state threshold assessments. Their jurisprudence has drifted gradually toward broader CA3 application (the customary-law reading), but cannot compel states to apply CA3 below official thresholds in their own operations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable rule for when humanitarian law obligations activate: intensity and organization thresholds reduce transaction costs for states and parties to conflict by clarifying scope ex ante. Provides a joint signal: when both thresholds are crossed, all parties know CA3 applies and humanitarian protections are mandatory. Avoids the transaction cost of case-by-case threshold litigation.
% TRANSFER_FUNCTION: Moves immunity from humanitarian law obligations from states and their militaries to irregular combatants and low-intensity conflict populations. States transfer to themselves: they retain maximum discretion by controlling threshold assessment. Irregular fighters transfer FROM themselves: they are excluded from CA3 victim protections. Civilians in below-threshold areas transfer FROM themselves: they lose humanitarian law protection without gaining criminal law protection.
% ABSENT_VOICES: Irregular combatants and civilian populations in low-intensity conflicts. They would argue that thresholds are tools of state power used to suppress humanitarian protections and exclude resistance from legitimate combatant status. They would advocate for CA3 application as a humanitarian floor to all armed violence. The ICRC and human rights advocates make similar arguments but are not fully excluded — they participate in academic and institutional forums while states retain final gatekeeping authority. The irregular combatants themselves are systematically excluded from threshold-setting discussions and lack representation in legal or diplomatic venues where CA3 scope is negotiated.
% DISAPPEARANCE_RATIONALE: If CA3 thresholds vanished — if humanitarian law applied to all armed violence regardless of intensity or organization — state capacity for asymmetric warfare would be fundamentally constrained. States would face binding obligations (prisoner-of-war status for combatants, medical immunity for wounded, proportionality rules, restraint on civilian targeting) even for police-like operations against small armed groups or borderline law-enforcement actions. The legal immunity states now hold below thresholds would collapse. Irregular armed groups would gain combatant status and protections immediately upon taking up arms, regardless of operational scale. Civilian populations in low-intensity conflicts would gain humanitarian law protections. The constraint's disappearance would constitute a complete reorganization of state operational discretion in asymmetric conflict.
% FOUNDING_PROBLEM: Early humanitarian law was built for conventional inter-state war with massed armies, clear battle lines, and uniformed combatants. The founding problem CA3 thresholds were intended to solve: distinguishing genuine 'armed conflict' (subject to humanitarian law) from mere 'law enforcement' (subject to domestic law) or 'criminal violence' (subject to criminal law). In the context of mid-twentieth-century interstate conflict, intensity and organization thresholds provided meaningful gatekeeping: only sustained, organized conflicts between armed forces triggered humanitarian law. Low-level banditry, police actions, and criminal violence remained outside humanitarian law scope.
% FOUNDING_PROBLEM_CORROBORATION: State military and legal establishments continue to assert that thresholds are essential for operational clarity and for distinguishing war from law enforcement. Humanitarian law scholars, the ICRC, and international courts document that the founding distinction is no longer coherent: modern asymmetric conflicts blur every boundary between war and law enforcement. Non-state actors use combat methods in populated areas; state militaries conduct population-control operations; hybrid threats (state-sponsored proxies, cyber-enabled insurgencies, terrorism) violate the organizational and intensity assumptions. Customary international law has drifted toward broader CA3 application (the ICRC's customary-law reading). The founding problem — 'Is this war or law enforcement?' — cannot be answered cleanly for 95% of modern armed conflicts. The International Criminal Court's jurisprudence increasingly applies CA3 to conflicts that fall below traditional state thresholds, signaling that the founding distinction has lost authority. No corroboration source outside the state-military establishment supports the claim that thresholds solve the founding problem; their corroboration comes exclusively from the states that benefit from threshold gatekeeping.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) reflects the constraint's core function: thresholds transfer immunity from humanitarian law from below-threshold irregular combatants to states. The measurement series shows a monotonic rise in extractiveness over 40 years (0.62 to 0.78), tracking the proliferation of asymmetric conflicts where non-state actors operate below the intensity/organization thresholds, allowing states to exploit the threshold gatekeeping to avoid humanitarian law obligations across growing classes of conflict. Suppression is higher (0.89) because the constraint's persistence depends on active state enforcement: alternative readings (human-rights floor, customary-law evolution) would dissolve the thresholds if not suppressed. The ICRC and humanitarian law scholars continuously challenge the state-centric interpretation; states must continuously re-assert it through legal argument, domestic military doctrine, and international coordination. Theater ratio (0.42 at interval end) reflects that the enforcement machinery increasingly performs threshold maintenance rather than genuine humanitarian gatekeeping — as conflicts grow asymmetric, states spend increasing effort litigating whether thresholds are crossed rather than complying with humanitarian law once crossed. The foundational problem (distinguishing war from law enforcement) is DEAD: modern asymmetric conflicts make the distinction incoherent, yet the state-centric reading persists as pure institutional inertia and state interest, not as functional problem-solving. This mandatrophy signal tracks the rising theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The state-military and government-law-enforcement seats experience this constraint as necessary gatekeeping that preserves operational flexibility. From the state perspective, the constraint is a rational coordination rule: it provides clear signals for when humanitarian obligations activate, and allows legitimate law-enforcement response to criminal violence and small-scale insurgencies without triggering wartime rules. From the irregular-combatant and low-intensity-conflict-population seats, the same structure operates as a suppression mechanism that excludes them from protection while denying them the combatant status they claim. The engine computes this divergence from power (institutional vs. powerless), exit options (analytical vs. trapped/identity-locked), and directionality (0.0 for state agenda-setters who benefit, 1.0 for irregular fighters excluded). The state-centric reading produces low-beneficiary and high-victim directionality by construction; the human-rights reading would produce the opposite (uniform protection as the beneficiary, state discretion as the victim).
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and government law enforcement are beneficiaries: they retain discretion over threshold assessment, escape CA3 obligations for below-threshold operations, and control the mechanism that determines scope. Directionality for these agents is near 0.0 (full beneficiary). Irregular combatants below thresholds and low-intensity-conflict populations are victims: they are excluded from CA3's victim set, denied prisoner-of-war protections, and trapped in legal voids where they receive neither combatant status nor civilian protection. Directionality for these agents is near 1.0 (full target). Domestic armed groups sit between: they may cross thresholds through sustained operations, but their combatants often receive weaker protections than state soldiers even when thresholds are met. Their identity is locked into armed resistance (ideological commitment); exit means abandoning both the armed struggle and the political identity. The ICRC and humanitarian advocates have constrained exit (they can argue the alternative reading, but states retain gatekeeping authority) and moderate power (organized but not institutional). The international courts have analytical exit and institutional power but are structurally dependent on state cooperation. The directionality computation captures these asymmetries; the authored metrics describe what the state-centric reading instantiates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: the state-centric reading was built to distinguish 'war' (high-intensity, organized interstate conflict) from 'law enforcement' (domestic violence, criminal activity). Modern asymmetric conflicts — counterinsurgency, terrorism, hybrid warfare — violate this distinction systematically. Conflicts involve non-state actors, sustained armed operations, and organized command structures, yet fall below state-defined intensity thresholds through state discretion. The thresholds no longer gatekeep a meaningful category (war vs. law enforcement); they gatekeep state operational discretion. The rising theater ratio (0.28 to 0.42 over the interval) and rising suppression (0.79 to 0.89) track this: states increasingly spend effort maintaining the threshold fiction rather than solving the founding problem. The constraint persists through pure institutional inertia and state interest in preserving immunity, not because the problem it was built to solve still exists. The six-questions verdict confirms: founding_problem_status = dead (the founding distinction is incoherent); disappearance_verdict = world_rearranges (if thresholds vanished, state capacity for below-threshold operations would collapse, forcing states to accept CA3 obligations for asymmetric warfare). This is a mandatrophy candidate: the constraint's function has atrophied, but its institutional machinery remains because states benefit from its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contested_threshold_definition,
    'Is this constraint one reading of CA3''s scope, or is it CA3''s actual scope under international humanitarian law?',
    'This is the kernel contest itself: the state-centric reading claims CA3 scope is defined by intensity/organization thresholds; the expansive-human-rights reading claims CA3 applies as a humanitarian floor regardless of thresholds; the customary-law reading claims scope emerges from state practice. The contest is unresolvable within a single framework — different authority structures (state sovereignty vs. human rights vs. customary law evolution) adjudicate scope differently.',
    'If this reading''s core premise is correct, CA3 protections are truly limited and low-intensity conflicts are legitimate state discretion. If either sibling reading is correct, this constraint is a false framing of CA3 — suppressing protections that should apply. The entire classification hinges on which reading is endorsed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contested_threshold_definition, conceptual, 'Which reading of CA3 scope is authoritative? State-centric vs. human-rights vs. customary-law authority.').

omega_variable(
    threshold_manipulation_incentive,
    'Do state assessments of whether thresholds are crossed track the actual facts of conflict, or do they track state interest in escaping CA3 obligations?',
    'Comparative case study: when is a conflict state-classified as below-threshold despite evidence of intensity/organization, and when is it classified as above-threshold despite lower evidence? If classifications cluster around state interest in constraint avoidance, threshold assessment is captured. If classifications appear independent of state preference, thresholds function as neutral gatekeeping.',
    'If captured, the suppression value understates the extraction — thresholds become not a bright-line rule but a state-controlled instrument for suppressing CA3 application case-by-case. If neutral, the suppression value reflects genuine gatekeeping by predictable rule, and the constraint is better described as a legitimate coordination device with side effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_manipulation_incentive, empirical, 'Whether threshold assessment is objective rule or state-captured discretion.').

omega_variable(
    irregular_combatant_identity_lock,
    'For domestic armed groups classified below thresholds, is the exit option ''identity_locked'' because their political/ideological identity is inseparable from armed resistance, or because legal traps make exit practically impossible?',
    'Post-exit trajectory: does a domestic armed group that lays down arms and returns to civilian/political life retain the label and legal exposure of ''combatant,'' or does the label and exposure end with the armed activity? If labels and exposure persist (criminal prosecution for past acts, continued targeting), exit is trapped rather than identity-locked. If both are contingent on maintained activity, exit is available to those willing to abandon the identity.',
    'If identity-locked, the constraint suppresses resistance to an identity-constituted commitment; the extraction is ideological. If trapped, the constraint suppresses resistance through legal consequence and physical danger; the extraction is structural. Identity-lock changes the omega classification (preference vs. empirical) and the interpretation of suppression (internalized vs. external).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irregular_combatant_identity_lock, empirical, 'Whether irregular combatants'' exclusion is identity-constituted or legally-trapped.').

omega_variable(
    humanitarian_floor_vs_state_discretion_reading_contest,
    'This story instantiates the state-centric reading. The sibling reading ''expansive_human_rights_reading'' asserts CA3 applies as a minimum humanitarian floor to all organized armed violence. Do these readings coexist (both live, different parties hold them) or does one foreclose the other?',
    'The test: can a state adopt the human-rights reading and still be a state in good standing within the international system? If yes, they coexist (different parties, competing interpretations). If states adopting the human-rights reading face institutional pressure to abandon it (sanctions, exclusion from bodies, treaty termination), then the state-centric reading functionally forecloses it.',
    'If coexist_with: both readings remain live, the constraint is a choice point where states cluster around different interpretations. If forecloses: the human-rights reading is suppressed by institutional pressure, and the state-centric reading is the structural gate. The reading_relations declaration follows from this assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_floor_vs_state_discretion_reading_contest, empirical, 'Whether state-centric and human-rights readings coexist or one forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t5, common_article_3_scope__state_centric_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(comm_tr_t5, observed).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__state_centric_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__state_centric_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t25, common_article_3_scope__state_centric_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(comm_tr_t25, observed).
narrative_ontology:measurement(comm_tr_t40, common_article_3_scope__state_centric_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t5, common_article_3_scope__state_centric_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(comm_be_t5, observed).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__state_centric_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__state_centric_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t25, common_article_3_scope__state_centric_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(comm_be_t25, observed).
narrative_ontology:measurement(comm_be_t40, common_article_3_scope__state_centric_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.79).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t5, common_article_3_scope__state_centric_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement_basis(comm_su_t5, observed).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__state_centric_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__state_centric_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t25, common_article_3_scope__state_centric_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(comm_su_t25, observed).
narrative_ontology:measurement(comm_su_t40, common_article_3_scope__state_centric_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__state_centric_reading, 0.18).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of CA3's scope, forming a constraint family. The state-centric reading interprets scope narrowly (intensity/organization thresholds); the expansive human-rights reading interprets scope broadly (humanitarian floor regardless of intensity); the customary-law reading interprets scope as emergent from state practice. Each reading instantiates a different ε value, beneficiary/victim structure, and persistence mechanism. All three readings compete within the international humanitarian law system; their contest is unresolvable within a single framework because they invoke different authority structures (state sovereignty, human rights, customary international law). The readings are linked by network.affects_constraints: the state-centric reading influences both alternatives by setting the baseline against which they argue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
