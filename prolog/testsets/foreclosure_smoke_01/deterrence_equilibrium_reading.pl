% ============================================================================
% CONSTRAINT STORY: deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deterrence_equilibrium_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium: Total War Remains Reachable but Deterred by Mutual Vulnerability
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   The deterrence equilibrium reading frames total war as remaining
 *   strategically reachable within the planning space of nuclear-armed
 *   states, but deterred through the mechanism of mutual vulnerability to
 *   catastrophic retaliation. This reading instantiates one interpretation of
 *   a contested kernel: the 'total war possibility space.' Unlike sibling
 *   readings (the space_contraction_reading, which argues war has become
 *   physically/institutionally impossible; the nuclear_taboo_reading, which
 *   argues war has become normatively foreclosed), the
 *   deterrence_equilibrium_reading holds that war remains logically reachable
 *   and deterrence works through cost-benefit calculation with extremely high
 *   costs. This reading predicts institutional continuity: doctrine
 *   development persists, counterforce targeting remains theorized,
 *   escalation ladder planning continues, and war-fighting capability
 *   investment is justified as necessary deterrent signal. The constraint
 *   exhibits extraction because the deterrence equilibrium generates
 *   continuous institutional investment in war-fighting capacity, nuclear
 *   modernization, and strategic doctrine development whose primary
 *   beneficiaries are the military-industrial complex and strategic doctrine
 *   establishments, while primary victims include civilian populations under
 *   existential threat, the global economic system vulnerable to escalation
 *   disruption, and arms control frameworks systematically undermined by the
 *   deterrence logic's demand for opacity. The theater ratio rises over the
 *   interval (0.42 to 0.63) as Cold War doctrine rhetoric becomes
 *   increasingly detached from actual strategic planning, yet persists
 *   through institutional inertia.
 *
 * KEY AGENTS:
 *   - Nuclear-Armed Strategic Powers: Primary beneficiaries (institutional/arbitrage) — capture security prestige, deterrent credibility, and weapons development justification through continuous doctrine development
 *   - Military-Industrial Complex and Strategic Doctrine Establishments: Secondary beneficiaries (institutional/arbitrage) — sustain institutional funding and intellectual authority through claims of deterrence necessity
 *   - Civilian Populations (Global): Primary victims (powerless/trapped) — exposed to existential escalation risk with no exit option or democratic voice in escalation decision-making
 *   - Arms Control Verification Infrastructure: Institutional victim (institutional/constrained) — institutional capture through the verification paradox (testing detection requires demonstrating undetected capability)
 *   - Non-Armed or Smaller States: Secondary victims (moderate/constrained) — dependent on nuclear umbrella security while bearing asymmetric costs and lacking independent deterrent capacity
 *   - Anti-Militarism and Disarmament Movements: Organized opposition (organized/mobile) — possess exit capacity and alternative frameworks but are suppressed through institutional marginalization of alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing deterrence equilibrium as inherent to mutual vulnerability rather than recognizing contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deterrence_equilibrium_reading, 0.68).
domain_priors:suppression_score(deterrence_equilibrium_reading, 0.72).
domain_priors:theater_ratio(deterrence_equilibrium_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deterrence_equilibrium_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deterrence_equilibrium_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deterrence_equilibrium_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deterrence_equilibrium_reading, snare).
narrative_ontology:human_readable(deterrence_equilibrium_reading, "Deterrence Equilibrium: Total War Remains Reachable but Deterred by Mutual Vulnerability").
narrative_ontology:topic_domain(deterrence_equilibrium_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deterrence_equilibrium_reading, 'efdd0fc5-ea53-45f9-b653-63ab4899e8d4').
narrative_ontology:cs_created_at('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', '').
narrative_ontology:cs_kernel_codification('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', formalized).
narrative_ontology:cs_authority_grounding('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', extraction).
narrative_ontology:cs_interpretation_layer_present('efdd0fc5-ea53-45f9-b653-63ab4899e8d4').
narrative_ontology:cs_kernel_id(deterrence_equilibrium_reading, total_war_possibility_space).
narrative_ontology:cs_reading_relation('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', foundational, total_war_remains_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_reachable, holdable).
narrative_ontology:cs_axiom('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', foundational, mutual_vulnerability_is_symmetric).
narrative_ontology:cs_axiom_status(mutual_vulnerability_is_symmetric, holdable).
narrative_ontology:cs_axiom('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', secondary, deterrence_requires_continuous_doctrine_development).
narrative_ontology:cs_axiom_status(deterrence_requires_continuous_doctrine_development, holdable).
narrative_ontology:cs_reference_frame('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', cold_war_mutual_assured_destruction).
narrative_ontology:cs_drift_state('efdd0fc5-ea53-45f9-b653-63ab4899e8d4', contemporary_multipolar_nuclear_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deterrence_equilibrium_reading, military_industrial_complex).
narrative_ontology:constraint_beneficiary(deterrence_equilibrium_reading, strategic_doctrine_establishments).
narrative_ontology:constraint_victim(deterrence_equilibrium_reading, civilian_populations).
narrative_ontology:constraint_victim(deterrence_equilibrium_reading, global_economic_system).
narrative_ontology:constraint_victim(deterrence_equilibrium_reading, arms_control_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped in a system where escalation remains structurally possible and continuous war-fighting preparation is framed as necessary safety. No exit from exposure to nuclear/total war risk; cannot opt out of deterrence logic. Maximum suppression through existential threat framing.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED OR SMALLER STATES (TANGLED ROPE) — Constrained by security architecture they did not design; benefit from some stability provided by major-power deterrence while bearing asymmetric costs (nuclear umbrella dependency, lack of independent deterrent capacity). Mixed extraction and coordination.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR-ARMED STRATEGIC POWERS (ROPE) — Experience deterrence as coordination mechanism solving mutual vulnerability problem. Continuous capacity development is framed as necessary communication of resolve. Exit option (arbitrage) enables unilateral escalation or de-escalation per strategic calculation. Net beneficiary.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARMS CONTROL VERIFICATION INFRASTRUCTURE (TANGLED ROPE) — Institutionally constrained by deterrence logic's demand for opacity (verification paradox: testing detection capability requires demonstration of undetected weapons capability). Provides genuine coordination (treaty monitoring) while being systematically undermined by the very logic it serves. Moderate extraction through institutional capture.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANTI-MILITARISM AND DISARMAMENT MOVEMENTS (SNARE) — Organized coalition with genuine mobility (can exit engagement with deterrence regime intellectually/politically) but experiences extraction through institutional suppression of alternatives. War-fighting doctrine persists; alternatives (non-nuclear defense, nuclear prohibition) are marginalized despite having mobilized advocates. High suppression despite organized exit capacity indicates extraction mechanism.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, snare,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC DOCTRINE INSTITUTIONAL RESIDUE (PITON) — Theater ratio (0.58) reflects that much deterrence rhetoric and doctrine persists as institutional performance. Actual strategic planning has shifted away from mutual assured destruction scenarios, yet the canonical doctrine, escalation ladder theorizing, and counterforce targeting doctrines remain formally operative. Degraded function (actual deterrence relies more on economic interdependence and uncertainty than on doctrine) maintained through institutional inertia.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, mutual vulnerability is an irreducible structural fact: if two parties possess existential weapons, the logical possibility of catastrophic escalation is inherent to the situation. No institutional arrangement can fully escape this constraint. However, the structural data reveals beneficiaries and institutional capture, triggering false summit detection — the 'naturalization' of deterrence equilibrium obscures the contingent institutional arrangements that sustain continuous war-fighting investment.
constraint_indexing:constraint_classification(deterrence_equilibrium_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deterrence_equilibrium_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deterrence_equilibrium_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deterrence_equilibrium_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deterrence_equilibrium_reading, TR),
    TR >= 0.70.

:- end_tests(deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The deterrence equilibrium generates continuous investment in war-fighting capability, strategic doctrine, and nuclear modernization. While this investment is justified as necessary deterrence signal, the beneficiaries (military-industrial complex, strategic establishments) capture substantial institutional and economic value from the constraint's continuation. The extractiveness increased over the interval from 0.48 to 0.68, driven by accumulation of doctrine without proportional disarmament (base_extractiveness rises even as the actual probability of nuclear war may have declined, indicating Goodhart drift — the proxy goal of deterrent signal substitutes for the original goal of war prevention). Suppression (0.72): High. The constraint works through existential threat framing (if you try to exit deterrence regime, you face undefended vulnerability) and institutional marginalization of alternatives (disarmament frameworks are presented as naïve or unrealistic). The suppression is both material (weapons exist; escalation is physically possible) and narrative (alternatives are epistemically delegitimized). Theater ratio (0.58): Moderate. Cold War doctrine (mutual assured destruction, escalation ladders, flexible response) remains formally operative in strategic planning, yet actual deterrence increasingly relies on economic interdependence, uncertainty, and mutual interest in avoiding catastrophe rather than on doctrine itself. The rise in theater ratio (0.42 to 0.63) indicates that the performative content of deterrence doctrine has increased — the doctrine persists as institutional ritual while its actual strategic relevance has declined.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival divergence possible within a single structural situation. Nuclear-armed powers see deterrence as coordination (Rope) — solving mutual vulnerability through continuous demonstration of resolve. They experience the constraint as beneficial. Civilian populations see deterrence as extraction (Snare) — they bear existential risk while having no voice in escalation decisions or weapons deployment. Smaller states see mixed coordination and extraction (Tangled Rope) — they benefit from security architecture but pay asymmetric costs. Disarmament movements see extraction despite their organizational capacity (Snare) — they have exit capacity (can intellectually reject deterrence) but are suppressed through institutional marginalization. Arms control infrastructure sees institutional capture (Tangled Rope) — their coordination function (treaty monitoring) is systematically undermined by the very deterrence logic they serve. Cold War doctrine residue is degraded (Piton) — the theater has increased while actual strategic relevance declined. The civilizational analytical observer risks false summit (naturalizing deterrence as inherent to mutual vulnerability rather than as contingent institutional arrangement). The perspectival span (from rope to snare to piton to mountain) indicates that the constraint's type is fundamentally observer-relative, not independent of position.
 *
 * DIRECTIONALITY LOGIC:
 *   The deterrence equilibrium reading structures directionality asymmetrically: nuclear-armed strategic powers occupy the beneficiary position (arbitrage exit — they can escalate or de-escalate per calculation, giving them maximum structural freedom). Their derived d is low (~0.15–0.25), producing negative or minimal χ. Civilian populations occupy the victim position (trapped exit — no exit from exposure to escalation risk), deriving d near 1.0, producing maximum χ. Smaller non-armed states are victims with constrained exit (dependent on nuclear umbrella, but theoretically mobile if they pursued independent deterrence), deriving d around 0.75–0.85. Disarmament movements are organized (higher power than powerless) with mobile exit (they can exit engagement with deterrence regime), but experience high suppression through institutional marginalization, deriving d around 0.65–0.75 (victim with exit capacity but suppressed). The perspectival gap is substantial: beneficiaries experience rope (coordination); victims experience snare (extraction); organized opposition experiences snare despite mobility (indicating suppression mechanism); analytical observer risks naturalizing as mountain (false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that deterrence equilibrium is a partial coordination mechanism with substantial asymmetric extraction. It is NOT pure Rope (despite beneficiary perspectives that frame it as coordination) because civilian victims bear existential costs with no exit and no benefit. It is NOT pure Snare (despite victim perspectives that experience it as extraction) because nuclear-armed powers genuinely solve mutual vulnerability through the mechanism of deterrence — some coordination function is real. The Tangled Rope classification captures this hybrid: genuine coordination (mutual vulnerability resolution) plus asymmetric extraction (beneficiaries are strategic powers; victims are civilian populations). The mandatrophy resolves by acknowledging that both the coordination claim (deterrence works by making escalation irrational through vulnerability) and the extraction claim (continuous weapons development benefits military-industrial beneficiaries while civilians bear risk) are simultaneously true. The constraint is Tangled Rope from the analytical perspective because it possesses both a genuine coordination function (preventing accidental/miscalculated escalation through transparency and mutual understanding of vulnerability) AND sustained asymmetric extraction (military-industrial benefits, civilian risk exposure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_probability,
    'Is the constraint that total war remains ''strategically reachable'' in the logical/planning sense, or that it remains probabilistically likely? These have radically different institutional implications.',
    'Historical analysis of deterrence doctrine: if deterrence theorizing emphasizes planning reachability (escalation ladders, counterforce doctrine), then the constraint is about institutional preservation of war-fighting capacity. If emphasis shifts to probabilistic deterrence (mutual destruction guarantees making escalation irrational), then the constraint is about belief systems rather than structural reachability.',
    'If reachability interpretation: extractiveness remains high (continuous institutional investment in war-fighting capability is justified). If probability interpretation: extractiveness may decline (arms control becomes coherent as complementary to deterrence). This reading instantiates the reachability interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_vs_probability, conceptual, 'Reachability vs. probability gap in deterrence doctrine').

omega_variable(
    vulnerability_asymmetry_dynamics,
    'Does ''mutual vulnerability'' remain actually mutual, or have technological asymmetries (missile defense, first-strike capacity, nuclear infrastructure hardening) created asymmetric vulnerability profiles that the deterrence fiction glosses over?',
    'Technical analysis of offensive-to-defensive capability ratios; historical tracking of vulnerability assessments in strategic doctrine; post-hoc correlation between doctrine stability and actual vulnerability metrics.',
    'If truly mutual: deterrence equilibrium is stable (snare classification from powerless perspective is justified). If asymmetric: deterrence claims mask vulnerability transitions, and some parties experience snare while others experience tangled rope or rope. This reading assumes mutual vulnerability holds; if empirical asymmetry is demonstrated, reclassify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_asymmetry_dynamics, empirical, 'Whether mutual vulnerability remains symmetric or has drifted to asymmetry').

omega_variable(
    kernel_contest_framing,
    'This constraint is one reading of the ''total war possibility space'' kernel. The sibling readings (space_contraction_reading, nuclear_taboo_reading) are alternative ways the same kernel is interpreted. Does the deterrence_equilibrium_reading framework allow the sibling readings to coexist as live positions, or does adopting this reading''s core premise (total war is strategically reachable; vulnerability is mutual) logically foreclose the siblings?',
    'Structural analysis: the deterrence_equilibrium_reading assumes reachability and mutual vulnerability as foundational. The space_contraction_reading assumes the possibility space has actually contracted (nuclear war has become physically/institutionally impossible). Do these coexist (different parties'' beliefs) or foreclose each other (logically incompatible within a single framework)? The answer determines whether the relations are coexists_with or forecloses.',
    'If coexists_with: the kernel admits multiple live readings; policy can move between them. If forecloses: adopting this reading commits you against the siblings; policy choice is actually a metaphysical claim about what is possible. This omega routes the committer-axis uncertainty into DR''s structured uncertainty framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether deterrence equilibrium reading coexists with or forecloses sibling readings of the total war kernel').

omega_variable(
    doctrine_as_extraction_mechanism,
    'Does continuous war-fighting doctrine development (counterforce targeting, escalation ladders, force modernization) exist because deterrence actually requires it, or because military-industrial institutions have captured the deterrence narrative to justify weapons development that serves institutional rather than security interests?',
    'Comparative institutional analysis: periods of doctrine stability vs. innovation; correlation between strategic threat assessment and doctrine change (should be tight if doctrine serves deterrence, loose if institutional capture); analysis of what would be sacrificed if doctrine were allowed to degrade without weapons system modernization.',
    'If doctrine serves deterrence: extractiveness is lower (continuous investment is legitimate security requirement). If doctrine serves institutional capture: extractiveness is higher (beneficiaries are military-industrial complex, not genuinely the deterred populations). This reading assumes some blend — beneficiaries include both security establishment and military-industrial complex, suggesting partial capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_as_extraction_mechanism, empirical, 'Whether doctrine development serves deterrence or institutional capture').

omega_variable(
    exit_pathways_foreclosure,
    'Why have disarmament movements and anti-militarism remained organizationally marginalized despite the demonstrable risks of the deterrence regime and the existence of alternative strategic frameworks (non-nuclear defense, nuclear prohibition treaties)?',
    'Historical analysis of suppression mechanisms: legal prohibition (most disarmament movements are not illegal), economic exclusion (activists are not collectively impoverished), or narrative suppression (alternatives are framed as naïve, unrealistic, or dangerous by institutional doctrine keepers). If narrative suppression dominates: this is the constraint mechanism, indicating that organized agents remain mobile but are suppressed by epistemic capture rather than material barriers.',
    'If narrative suppression is primary: reclassify anti-militarism perspective from snare to different type (organized agents with epistemic identity_locked exit options, rather than trapped exit). This would indicate the constraint''s extraction works through identity framing rather than material suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_pathways_foreclosure, empirical, 'Why disarmament movements remain marginalized despite theoretical alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deterrence_equilibrium_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deters_tr_t0, deterrence_equilibrium_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(deters_tr_t15, deterrence_equilibrium_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(deters_tr_t35, deterrence_equilibrium_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement(deters_tr_t50, deterrence_equilibrium_reading, theater_ratio, 50, 0.63).

% Extraction over time
narrative_ontology:measurement(deters_be_t0, deterrence_equilibrium_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(deters_be_t15, deterrence_equilibrium_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(deters_be_t35, deterrence_equilibrium_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(deters_be_t50, deterrence_equilibrium_reading, base_extractiveness, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(deterrence_equilibrium_reading, space_contraction_reading).
narrative_ontology:affects_constraint(deterrence_equilibrium_reading, nuclear_taboo_reading).
narrative_ontology:affects_constraint(deterrence_equilibrium_reading, arms_control_verification_paradox).
narrative_ontology:affects_constraint(deterrence_equilibrium_reading, military_industrial_doctrine_capture).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel decomposes into three constraint stories: deterrence_equilibrium_reading (this file), space_contraction_reading (war has become impossible), and nuclear_taboo_reading (war has become normatively prohibited). These are not three perspectives on a single constraint — they are three fundamentally different constraints instantiated by different readings of the same kernel. Each has its own ε value reflecting the empirical status of the reading. Deterrence_equilibrium_reading (ε=0.68, Snare) assumes reachability and mutual vulnerability. Space_contraction_reading (ε unknown) would assume irreversibility or physical impossibility. Nuclear_taboo_reading (ε unknown) would assume normative binding. The stories are linked by kernel identity and by causal influence: if deterrence_equilibrium_reading is correct, the sibling readings are incorrect, and this affects their credibility. The network_affects_constraints edges connect to related constraints that depend on deterrence logic: arms_control_verification_paradox (the constraint that verifying disarmament requires demonstrating undetected capability), military_industrial_doctrine_capture (the constraint that military-industrial institutions capture deterrence narrative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deterrence_equilibrium_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
