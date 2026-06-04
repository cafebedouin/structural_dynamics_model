% ============================================================================
% CONSTRAINT STORY: graphe_paranomon__weapon_of_faction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_graphe_paranomon__weapon_of_faction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: graphe_paranomon__weapon_of_faction_reading
 *   human_readable: Graphe Paranomon as Factional Weapon (Athenian Judicial Politics)
 *   domain: ancient_greek_law/institutional_dynamics
 *
 * SUMMARY:
 *   The graphe paranomon (action against unlawful proposal) was designed as a
 *   mechanism for the Athenian assembly to police its own decrees — a
 *   juridical self-binding device. Under the weapon-of-faction reading
 *   instantiated here, the mechanism becomes a tool of factional competition:
 *   rival orators and their organized supporters use the threat of
 *   prosecution to silence, discredit, or eliminate political opponents. The
 *   constraint exhibits the signature of a tangled rope: genuine coordination
 *   function (channeling factional conflict into legal form rather than
 *   direct violence) combined with asymmetric extraction (the prosecuting
 *   faction benefits while the target faction and unprepared proposers bear
 *   costs). Suppression is deployed tactically rather than as a formal
 *   juridical principle — the threat of atimia falls most heavily on
 *   politically isolated targets without factional backing. The theater ratio
 *   rises over the interval because the legal formality becomes increasingly
 *   performative: judges and jurors understand themselves as serving
 *   factional interests while maintaining the fiction of neutral
 *   adjudication.
 *
 * KEY AGENTS:
 *   - Prosecuting Faction: Organized faction with backing, resources, and rhetorical skill — primary beneficiary of the mechanism when it prosecutes rivals
 *   - Accused Proposer (Unorganized): Individual without factional support — primary victim; faces maximum suppression and extraction
 *   - Rival Faction: Organized opposition bloc — secondary victim; targets of prosecution campaigns when outmaneuvered rhetorically
 *   - Dikasteria (Jury Court): Institutional actor — benefits through legitimacy and power consolidation; maintains fiction of neutrality
 *   - Democratic Assembly Collective: Institutional body — experiences coordination function at aggregate level (manages factional conflict) alongside extraction (weaponized silencing)
 *   - Device Neutrality: Abstract victim — the graphe paranomon's claimed role as impartial self-binding mechanism is degraded through factional weaponization
 *   - Analytical Observer: Sees the constraint as contingent on factional organization; recognizes reading ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(graphe_paranomon__weapon_of_faction_reading, 0.58).
domain_priors:suppression_score(graphe_paranomon__weapon_of_faction_reading, 0.62).
domain_priors:theater_ratio(graphe_paranomon__weapon_of_faction_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(graphe_paranomon__weapon_of_faction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(graphe_paranomon__weapon_of_faction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(graphe_paranomon__weapon_of_faction_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(graphe_paranomon__weapon_of_faction_reading, tangled_rope).
narrative_ontology:human_readable(graphe_paranomon__weapon_of_faction_reading, "Graphe Paranomon as Factional Weapon (Athenian Judicial Politics)").
narrative_ontology:topic_domain(graphe_paranomon__weapon_of_faction_reading, "ancient_greek_law/institutional_dynamics").

domain_priors:requires_active_enforcement(graphe_paranomon__weapon_of_faction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(graphe_paranomon__weapon_of_faction_reading, 'b5bd02d1-5951-4507-8db1-be4b52adc5f6').
narrative_ontology:cs_kernel_codification('b5bd02d1-5951-4507-8db1-be4b52adc5f6', fixed_text).
narrative_ontology:cs_authority_grounding('b5bd02d1-5951-4507-8db1-be4b52adc5f6', extraction).
narrative_ontology:cs_interpretation_layer_present('b5bd02d1-5951-4507-8db1-be4b52adc5f6').
narrative_ontology:cs_reading_relation('b5bd02d1-5951-4507-8db1-be4b52adc5f6', graphe_paranomon__orator_risk_economy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5bd02d1-5951-4507-8db1-be4b52adc5f6', graphe_paranomon__self_binding_mechanism_reading, influences).
narrative_ontology:cs_axiom('b5bd02d1-5951-4507-8db1-be4b52adc5f6', foundational, prosecuting_faction_defines_operation).
narrative_ontology:cs_axiom_status(prosecuting_faction_defines_operation, holdable).
narrative_ontology:cs_axiom_grounding('b5bd02d1-5951-4507-8db1-be4b52adc5f6', prosecuting_faction_defines_operation, empirically_contingent).
narrative_ontology:cs_axiom('b5bd02d1-5951-4507-8db1-be4b52adc5f6', foundational, juridical_form_masks_factional_intent).
narrative_ontology:cs_axiom_status(juridical_form_masks_factional_intent, holdable).
narrative_ontology:cs_axiom_grounding('b5bd02d1-5951-4507-8db1-be4b52adc5f6', juridical_form_masks_factional_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('b5bd02d1-5951-4507-8db1-be4b52adc5f6', lawful_assembly_self_constraint).
narrative_ontology:cs_drift_state('b5bd02d1-5951-4507-8db1-be4b52adc5f6', factional_intensification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5bd02d1-5951-4507-8db1-be4b52adc5f6', '').
narrative_ontology:cs_kernel_id(graphe_paranomon__weapon_of_faction_reading, graphe_paranomon).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(graphe_paranomon__weapon_of_faction_reading, faction_prosecuting_case).
narrative_ontology:constraint_beneficiary(graphe_paranomon__weapon_of_faction_reading, successful_orator).
narrative_ontology:constraint_victim(graphe_paranomon__weapon_of_faction_reading, device_neutrality).
narrative_ontology:constraint_victim(graphe_paranomon__weapon_of_faction_reading, rival_faction).
narrative_ontology:constraint_victim(graphe_paranomon__weapon_of_faction_reading, political_initiative_bearer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED PROPOSER WITHOUT FACTION (SNARE) — Powerless individual without organized faction support faces full suppressive force of the mechanism. The graphe paranomon becomes a tool of elimination: the proposer cannot rely on peer advocates, cannot afford to mount a credible defense, and faces personal financial ruin (atimia). Extraction is maximal because the accused lacks organizational protection and the mechanism's coercive potential is unleashed without restraint.
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RIVAL FACTION LEADER (TANGLED ROPE) — Powerful actor with factional backing uses the graphe paranomon as a coordinated political move. The mechanism serves genuine coordination function: it channels factional competition into a legal form rather than direct violence. But it is also extractive: the mechanism allows the prosecuting faction to silence or discredit opponents through litigation threat. The actor experiences mixed costs (litigation risk, counter-prosecution) and benefits (discredit of rival, demonstration of faction power).
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: DIKASTERIA / JURY COURT (ROPE) — Institutional actor coordinating factional disputes through formal legal procedure. Jurors experience the constraint as coordination: the graphe paranomon provides a legitimate mechanism for managing succession of speakers and filtering proposals. The procedure creates beneficiary status for the court itself (legitimacy, authority) through the appearance of neutral adjudication. Extraction runs toward the institution through enhanced prestige and power consolidation.
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ATHENIAN ASSEMBLY (COLLECTIVE LEVEL, GENERATIONAL) (TANGLED ROPE) — At the collective institutional level, the graphe paranomon serves genuine coordination: it channels factional conflicts into adjudicated form, preventing direct violence and maintaining deliberative process. But it also enables extraction: the mechanism allows organized factions to suppress initiative from rivals or from lone proposers without factional backing. The assembly experiences both the benefit (coordination of factional conflict) and the cost (weaponization of procedure against political innovation).
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: JURIDICAL NEUTRALITY FICTION (PITON) — The graphe paranomon maintains a performance of neutral legality (impartial jury review, procedural formality) while actually functioning as a factional weapon. The theater is essential to the mechanism's operation: jurors must believe themselves neutral for their verdicts to carry legitimacy. The device is degraded from its self-binding function (Perspective 6 below) — it has become primarily a tool for factional prosecution rather than collective self-restraint. Theater ratio is high because the legal formality masks and sustains the factional operation.
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE, ALTERNATIVE FRAMINGS) — From the analytical vantage, the graphe paranomon is visible as a pure coordination mechanism with low extractiveness if framed as self-binding (the assembly constraining itself), OR as a high-extraction weapon if framed as factional tool. This perspective shows that the classification depends on which reading is instantiated. Under the weapon-of-faction reading (this constraint), the extractiveness and suppression are high; under the self-binding reading, they would be low. The analytical observer can see both frames simultaneously and recognizes that the constraint's type depends on which institutional narrative has ascendancy.
constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(graphe_paranomon__weapon_of_faction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(graphe_paranomon__weapon_of_faction_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(graphe_paranomon__weapon_of_faction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(graphe_paranomon__weapon_of_faction_reading, TR),
    TR >= 0.70.

:- end_tests(graphe_paranomon__weapon_of_faction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Under the weapon-of-faction reading, the constraint extracts significant value for the prosecuting faction — the threat of atimia creates powerful deterrent against rivals or independent proposers, allowing organized factions to control the assembly's agenda and suppress minority initiatives. The value reflects that extraction is substantial but not absolute: successful defense is possible (especially with factional backing), and counter-prosecution creates mutual vulnerability. Suppression (0.62): High. The threat of atimia (loss of civic rights and property seizure potential) is severe, and the burden of proof falls on the accused. However, suppression is not total because organized factions can mount credible defenses and counter-prosecutions. Theater ratio (0.68): Moderately high and rising. The legal form and jury procedure are partially performative — the device maintains an appearance of neutral adjudication while actually operating as a factional tool. Jurors may sincerely believe themselves neutral while acting on factional cues. The theater ratio increases over the interval as factional competition intensifies and the legal facade must work harder to sustain legitimacy. Claimed type (Tangled Rope): Correct because the mechanism coordinates factional competition (genuine coordination function) while enabling extraction (prosecution of rivals). All three elements are present: beneficiaries (prosecuting faction), victims (targets, rival factions), and active enforcement (jury verdicts carrying civic penalties).
 *
 * PERSPECTIVAL GAP:
 *   The weapon-of-faction reading produces a large perspectival gap. The prosecuting faction (institutional/arbitrage) sees rope or even scaffold — a coordination mechanism managing factional disputes. The dikasteria (institutional/arbitrage) sees rope — a legal procedure conferring legitimacy. The accused powerless proposer (powerless/trapped) sees snare — a system designed to eliminate them. The democratic assembly at the generational level sees tangled rope — both genuine coordination and factional extraction simultaneously. The juridical neutrality fiction (institutional/arbitrage, civilizational) sees piton — a degraded device that performs neutrality while serving factional interests. The analytical observer sees that the classification depends on which reading is instantiated and recognizes the committer ambiguity: is the device inherently factional, or do factions merely weaponize a neutral procedure? This reading answers: the device is structurally dependent on factional organization to function as described; it is a weapon because the factions make it one.
 *
 * DIRECTIONALITY LOGIC:
 *   The prosecuting faction derives d from beneficiary status + institutional power + arbitrage exit: they can exit the graphe system by shifting to extra-legal coercion, but they choose to use the legal form because it confers legitimacy and organizational efficiency. The accused powerless proposer derives d from victim status + powerless status + trapped exit: they cannot exit the system, have no factional backing, and bear maximum extraction. The rival organized faction derives d from victim status + organized status + constrained exit: they can mount counter-prosecution but at significant cost. The dikasteria derives d from beneficiary status (gains legitimacy and authority) + institutional status + arbitrage exit (judges are embedded in the assembly and cannot truly exit). The differing d values produce varying experienced extractiveness: beneficiaries with arbitrage exit experience negative or low chi; victims with constrained or trapped exit experience high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The weapon-of-faction reading resolves the mandatrophy by showing that the graphe paranomon is genuinely both coordination mechanism and extraction tool, depending on the agent's structural position. For the prosecuting faction, it is rope (coordination). For the accused without backing, it is snare (extraction). For the assembly collectively, it is tangled rope (both). This is not ambiguity about which type is correct — it is correct structural observation that different agents experience the same device differently based on their organizational position. The mandatrophy is resolved by recognizing that indexical classification tracks the agent's structural relationship to the constraint, not an objective property of the constraint itself. The weapon-of-faction reading makes explicit that the device's operation depends on which faction prosecutes successfully — the beneficiary set is not fixed but determined by litigation outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    factional_organization_threshold,
    'What level of factional coordination is required for the graphe paranomon to function as a weapon versus as a neutral procedural device?',
    'Historical analysis of prosecution patterns: concentration of graphai against single targets, correlation with factional conflicts, timing relative to known faction activities vs. timing based on proposal content alone',
    'If threshold is low (loose coordination suffices): snare classification confirmed for unprepared targets, weapon reading is dominant. If threshold is high (sustained organized campaign required): constraint is more rope-like, self-binding reading has more plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factional_organization_threshold, empirical, 'Factional coordination threshold for weaponization').

omega_variable(
    reading_committer_ambiguity,
    'Is the graphe paranomon inherently a factional weapon, or is weaponization contingent on how factions choose to deploy it?',
    'Comparative historical analysis: periods of factional stability (graphe used rarely or neutrally) vs. periods of acute factional conflict (graphe used intensively as weapon). If weapon use tracks factional polarization rather than being constant, weaponization is contingent; if graphe operates as weapon even during periods of low factional tension, it is structurally inherent.',
    'If contingent: the weapon-of-faction reading and self-binding reading coexist (different parties use same device differently). If inherent: weapon reading forecloses self-binding reading — the appearance of neutrality is itself the mechanism of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Committer ambiguity: whether weaponization is inherent or contingent').

omega_variable(
    atimia_enforcement_asymmetry,
    'Is the threat of atimia (loss of civic rights) applied equally regardless of factional backing, or do factions with superior organization manage to shield their members from conviction?',
    'Quantitative analysis of atimia outcomes: conviction rate by factional affiliation, severity of penalty by target faction, success rate in mounting defense organized vs. unorganized targets',
    'If applied equally: suppression is formal/legal (medium level). If asymmetric: suppression is factional (high level), confirming snare classification for unorganized targets and weapon-of-faction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atimia_enforcement_asymmetry, empirical, 'Whether atimia enforcement is factionally asymmetric').

omega_variable(
    self_binding_counterfactual,
    'Would the device''s structure and enforcement patterns remain identical if factions were eliminated and the assembly operated without organized opposition blocs?',
    'Theoretical analysis (requires comparative case study or simulation): if graphe paranomon were administered under conditions of no factional organization, would suppression, extractiveness, and theater ratio remain the same? What minimum level of organization is required for the mechanism to function as described?',
    'If structure identical without factions: weapon reading is reading of surface dynamics, not deep structure (self-binding reading is more fundamental). If structure collapses without factions: weapon reading is correct about what the device actually does (factions are not incidental to its operation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_binding_counterfactual, conceptual, 'Whether mechanism depends on factional organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(graphe_paranomon__weapon_of_faction_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gp_weapon_tr_t0, graphe_paranomon__weapon_of_faction_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gp_weapon_tr_t3, graphe_paranomon__weapon_of_faction_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(gp_weapon_tr_t6, graphe_paranomon__weapon_of_faction_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(gp_weapon_be_t0, graphe_paranomon__weapon_of_faction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gp_weapon_be_t3, graphe_paranomon__weapon_of_faction_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(gp_weapon_be_t6, graphe_paranomon__weapon_of_faction_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gp_weapon_su_t0, graphe_paranomon__weapon_of_faction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gp_weapon_su_t3, graphe_paranomon__weapon_of_faction_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(gp_weapon_su_t6, graphe_paranomon__weapon_of_faction_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(graphe_paranomon__weapon_of_faction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(graphe_paranomon__weapon_of_faction_reading, graphe_paranomon__orator_risk_economy_reading).
narrative_ontology:affects_constraint(graphe_paranomon__weapon_of_faction_reading, graphe_paranomon__self_binding_mechanism_reading).
narrative_ontology:affects_constraint(graphe_paranomon__weapon_of_faction_reading, athenian_factional_dynamics_institutional_escalation).

% DUAL FORMULATION NOTE:
% The graphe paranomon is a single institutional device (the kernel) instantiated in three structurally distinct constraints corresponding to three readings of what the device does. Each reading has different ε (extractiveness), different beneficiary/victim structure, different claimed type. The weapon-of-faction reading has ε=0.58 (moderate-high extraction); the self-binding reading would have ε≈0.20 (low extraction, coordination focus); the orator-risk reading would have ε≈0.35 (moderate, selection mechanism). These are not measurement perspectives on one constraint; they are three constraints derived from three readings of the same kernel. The network links show the dependency: all three readings affect institutional dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(graphe_paranomon__weapon_of_faction_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
