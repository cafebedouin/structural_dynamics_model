% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo: Normative Prohibition of Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   The nuclear taboo reading holds that total war became normatively
 *   prohibited through a constructed taboo against nuclear use, independent
 *   of the material capability to wage it. The constraint is not that nuclear
 *   war is impossible — arsenals exist and deterrence operates — but that
 *   nuclear use has been moved from the realm of strategic calculation to the
 *   realm of the morally unthinkable. This taboo generates its own
 *   enforcement machinery: the NPT regime, IAEA safeguards, export control
 *   regimes (NSG), security assurances, and no-first-use pledges. The reading
 *   predicts the taboo weakens if norm entrepreneurs (disarmament NGOs,
 *   middle-power states like Ireland, Mexico, Austria) exit the enforcement
 *   coalition, and that non-nuclear states face a qualitatively different
 *   constraint structure — they are bound by the taboo while nuclear-armed
 *   states retain the weapons the taboo nominally condemns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.75).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo: Normative Prohibition of Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'ea8e3820-8427-4168-98d6-83a5ba76464d').
narrative_ontology:cs_kernel_codification('ea8e3820-8427-4168-98d6-83a5ba76464d', formalized).
narrative_ontology:cs_authority_grounding('ea8e3820-8427-4168-98d6-83a5ba76464d', practice).
narrative_ontology:cs_interpretation_layer_present('ea8e3820-8427-4168-98d6-83a5ba76464d').
narrative_ontology:cs_reading_relation('ea8e3820-8427-4168-98d6-83a5ba76464d', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea8e3820-8427-4168-98d6-83a5ba76464d', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('ea8e3820-8427-4168-98d6-83a5ba76464d', foundational, nuclear_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(nuclear_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('ea8e3820-8427-4168-98d6-83a5ba76464d', nuclear_use_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('ea8e3820-8427-4168-98d6-83a5ba76464d', secondary, nonproliferation_as_global_public_good).
narrative_ontology:cs_axiom_status(nonproliferation_as_global_public_good, holdable).
narrative_ontology:cs_axiom_grounding('ea8e3820-8427-4168-98d6-83a5ba76464d', nonproliferation_as_global_public_good, conventional).
narrative_ontology:cs_reference_frame('ea8e3820-8427-4168-98d6-83a5ba76464d', npt_article_vi_bargain).
narrative_ontology:cs_drift_state('ea8e3820-8427-4168-98d6-83a5ba76464d', post_2010_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea8e3820-8427-4168-98d6-83a5ba76464d', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states_npt).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states_non_npt).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, potential_proliferators).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, nuclear_use_categorically_impermissible).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, nonproliferation_as_global_public_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear weapon states (US, Russia, UK, France, China) plus de facto possessors. They set the rules of the non-proliferation regime, control the Security Council enforcement machinery, and benefit from a permanent legal monopoly on nuclear weapons. They bear maintenance costs but these are sunk costs of great-power status. Their exit option is arbitrage: they can reinterpret obligations (e.g., modernization not disarmament) without regime collapse.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% Middle-power states (Ireland, Mexico, Austria, New Zealand, South Africa) and transnational networks (ICAN, Pugwash, IPPNW) that champion the taboo. They gain moral authority, diplomatic influence, and institutional positions by policing the norm. Their exit is mobile: they can shift advocacy focus, but the taboo's persistence depends on their sustained pressure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, agenda_setter).

% The 186 NPT non-nuclear parties. They accepted the taboo's constraints (no acquisition, IAEA safeguards) in exchange for Article VI disarmament commitments and peaceful use cooperation. The disarmament bargain has not been honored; peaceful use cooperation is conditional. Their exit is constrained: withdrawal triggers Article X crisis, sanctions risk, and security vulnerability. They pay through sovereignty costs and intrusive verification.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states_npt, payer,
    moderate, biographical, constrained, global).

% India, Pakistan, Israel — states with nuclear weapons outside the NPT. They bear the taboo's normative condemnation and partial sanctions but gained the strategic asset. They are excluded from the regime's benefits (peaceful cooperation, security assurances) but not from its normative claim. Their exit is constrained: they cannot rejoin as non-nuclear states, and the regime refuses to recognize their status.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states_non_npt, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states_non_npt, excluded).

% States assessed as having proliferation intent or capacity (historically Iran, Libya, Syria; currently none declared). They face the full enforcement machinery: sanctions, interdiction, sabotage, military threat. The constraint is designed to make their exit (acquisition) prohibitively costly. They are the primary extraction targets — the regime's suppression is calibrated to them.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, potential_proliferators, payer,
    powerless, immediate, trapped, national).

% Academic strategists, think tanks, government advisory bodies. They analyze the taboo's stability, model erosion scenarios, and advise policymakers. They neither collect nor pay; they map the structure. Their analytical exit means they can adopt any reading (taboo, deterrence, unthinkability) without material consequence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, security_analysts_strategic_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear use by moving it from strategic calculation to moral prohibition; coordinates expectations that nuclear weapons will not be used, enabling a world where major powers do not fight total war.
% TRANSFER_FUNCTION: Moves sovereignty over ultimate weapons decisions from non-nuclear states to the nuclear-armed incumbents, mediated through the NPT regime. Non-nuclear states transfer the option to acquire; nuclear states retain the weapons and the authority to authorize or deny others' access.
% ABSENT_VOICES: States that never joined the NPT and acquired weapons anyway (India, Pakistan, Israel, North Korea) — they would argue the taboo is hypocritical hierarchy maintenance. Future generations who inherit the taboo's enforcement costs without the founding fear. Victims of nuclear testing and uranium mining (indigenous communities, downwinders) — excluded from the regime's beneficiary calculus.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo and NPT regime vanished overnight, multiple states would likely pursue nuclear weapons within years (Japan, South Korea, Germany, Saudi Arabia, Turkey, Iran). The nuclear hierarchy would collapse into multi-polar proliferation. The coordination function (preventing use via norm) would disappear, leaving only deterrence — which the taboo reading argues is insufficient without the normative layer.
% FOUNDING_PROBLEM: After 1945, the demonstration of nuclear destruction created a shared recognition that total war had become suicidal. The founding problem was how to prevent nuclear use while managing the spread of nuclear technology — solved by constructing a normative prohibition (the taboo) and institutionalizing it through the NPT (1968).
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states attest the problem (preventing use) is live and the taboo works. Non-nuclear states (NAM, G77) attest the problem (preventing use) is live but the bargain (disarmament) is dead — the taboo persists as hierarchy. Disarmament NGOs attest the problem is live and the taboo must deepen to elimination. No single corroboration exists outside the benefiting parties; the status is genuinely contested across the stakeholder seats.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: non-nuclear states surrender the sovereign option to acquire nuclear weapons and accept intrusive verification, while nuclear states retain arsenals and modernization programs. The asymmetry is the extraction. Suppression (0.75) is high because the regime actively prevents proliferation through sanctions, interdiction, and security guarantees — not merely through persuasion. Theater ratio (0.28) is low-moderate: the taboo has real behavioral effect (no nuclear use since 1945), but a growing share of enforcement activity maintains the hierarchy rather than preventing use. Accessibility collapse (0.58) reflects that alternatives (nuclear hedging, latent capability) exist but carry escalating costs. Resistance (0.45) is moderate: NPT review conferences reveal persistent non-nuclear dissatisfaction, but overt defiance is rare.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the arrangement is coordination: preventing use, managing proliferation, providing security assurances. From the non-nuclear NPT state seat, the same structure is extraction: a permanent hierarchy enforced through a bargain (Article VI disarmament) that has not been honored. From the non-NPT nuclear state seat, the constraint is exclusion: they are punished for acquiring what the incumbents keep. The engine computes this divergence from the structural data — the declared beneficiary/victim lists and power/exit asymmetries drive the per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are structural beneficiaries: they collect status, strategic monopoly, and institutional authority while bearing the costs of maintenance (which they would bear regardless). Norm entrepreneurs are beneficiaries: they gain moral authority and institutional influence. Non-nuclear NPT states are payers: they accept constraint without the compensating strategic asset. Non-NPT nuclear states (India, Pakistan, Israel, North Korea) occupy a distinct payer/excluded seat: they bear isolation costs but gain the strategic asset. Potential proliferators are trapped payers: the regime is designed to make their exit prohibitively costly. The analytical observer seat sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear use and limiting spread) remains live, but the original bargain (non-proliferation for disarmament) is dead. The constraint persists not because the coordination function works — the taboo prevents use but the hierarchy generates its own instability — but because no coalition can dismantle it. Nuclear states won't disarm; non-nuclear states can't force them; norm entrepreneurs lack enforcement power. This is not a piton (the taboo has real function) but a tangled rope where the coordination function (preventing use) is real but the extraction (permanent hierarchy) is structural and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_causal_efficacy_vs_deterrence,
    'Is the nuclear taboo causally independent of material deterrence, or is it epiphenomenal to mutual vulnerability?',
    'Counterfactual analysis of crises where deterrence held but taboo rhetoric was absent (e.g., early Cold War), and cases where taboo rhetoric appeared without stable deterrence (e.g., post-1991). Requires disaggregating normative inhibition from calculated restraint.',
    'If epiphenomenal, the constraint reduces to deterrence_equilibrium_reading; if independent, the normative layer generates distinct enforcement mechanisms and extraction patterns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_causal_efficacy_vs_deterrence, conceptual, 'Whether the taboo has autonomous causal force or merely rationalizes deterrence.').

omega_variable(
    taboo_universality_vs_nuclear_privilege,
    'Does the taboo apply symmetrically to all nuclear possessors, or does it legitimate the existing nuclear hierarchy?',
    'Compare enforcement intensity against established vs. aspirant proliferators; track whether taboo discourse constrains nuclear-armed states'' modernization and doctrinal expansion.',
    'If the taboo primarily constrains aspirants while legitimizing incumbents'' arsenals, the extraction is structural hierarchy maintenance, not universal prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_universality_vs_nuclear_privilege, empirical, 'Whether the taboo functions as universal norm or club good for nuclear incumbents.').

omega_variable(
    kernel_reading_nuclear_taboo,
    'This constraint is the nuclear_taboo_reading of the contested kernel total_war_possibility_space. How does this reading''s structural claim differ from its siblings?',
    'Compare the three readings'' beneficiary/victim structures, enforcement mechanisms, and predictions about taboo erosion. The nuclear_taboo_reading predicts weakening when norm entrepreneurs exit; deterrence_equilibrium_reading predicts stability while mutual vulnerability holds; space_contraction_reading predicts irreversibility.',
    'Different readings imply different constraint types, different stakeholder seats, and different temporal trajectories. The engine must compute each reading as a separate constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_nuclear_taboo, conceptual, 'Committer frame: this reading instantiates nuclear taboo as constructed normative prohibition with active enforcement, distinct from deterrence equilibrium and strategic unthinkability.').

omega_variable(
    suppression_mechanism_npt_regime,
    'Is the NPT regime''s suppression structural (sanctions, export controls, security guarantees) or internalized (states believe proliferation is illegitimate)?',
    'Track compliance trajectories after regime pressure eases (e.g., post-JCPOA Iran, post-1994 South Africa). If suppression persists without enforcement, internalization is significant.',
    'If substantially internalized, the constraint''s effective suppression exceeds structural measures — states carry the prohibition as identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_npt_regime, empirical, 'Structural vs. internalized suppression in the non-proliferation regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_ntr_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(twps_ntr_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(twps_ntr_tr_t1968, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(twps_ntr_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(twps_ntr_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(twps_ntr_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(twps_ntr_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(twps_ntr_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(twps_ntr_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(twps_ntr_be_t1968, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(twps_ntr_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(twps_ntr_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(twps_ntr_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(twps_ntr_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(twps_ntr_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(twps_ntr_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(twps_ntr_su_t1968, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(twps_ntr_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(twps_ntr_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(twps_ntr_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(twps_ntr_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_disarmament_obligation).

% DUAL FORMULATION NOTE:
% Part of the total_war_possibility_space constraint family. This reading (nuclear_taboo_reading) decomposes the kernel into a normative prohibition with active enforcement. The deterrence_equilibrium_reading models the same kernel as material mutual vulnerability; the space_contraction_reading models it as cognitive unthinkability. All three share the referent (total war possibility space) but instantiate different constraints with different ε, different stakeholder structures, and different temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.25).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, organized, 0.35).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, moderate, 0.7).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
