% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment: Insurrectionist Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'insurrectionist reading' of the Second
 *   Amendment, asserting an individual right to possess arms, including
 *   military-grade weapons, as a deterrent or means of resistance against a
 *   potentially tyrannical government. This interpretation places a high
 *   value on individual armed capacity, viewing state disarmament efforts as
 *   precursors to tyranny. It is a highly contested reading within
 *   constitutional law and political theory, with significant implications
 *   for firearms policy and the balance of power between citizens and the
 *   state.
 *
 * KEY AGENTS:
 *   - armed_citizens_claiming_deterrent_legitimacy: Primary beneficiary (powerful/constrained) — benefits from expanded arms access, claims legitimacy for potential resistance.
 *   - state_security_apparatus: Primary victim (institutional/trapped) — faces potential armed opposition, constrained in disarmament efforts.
 *   - legislators_seeking_gun_control: Victim (institutional/constrained) — efforts to regulate firearms are hampered by this reading.
 *   - firearms_manufacturers: Beneficiary (organized/arbitrage) — profits from expanded market for military-grade arms.
 *   - gun_rights_advocacy_groups: Beneficiary (organized/mobile) — gains political power and influence from this reading.
 *   - civilians_in_conflict_zones: Victim (powerless/trapped) — bears the direct costs of potential armed conflict or increased gun violence.
 *   - constitutional_scholars: Observer (analytical/analytical) — analyzes the historical and legal basis of this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment: Insurrectionist Reading").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'aa8b831e-77d2-4b98-9a99-dfe9b1249d84').
narrative_ontology:cs_kernel_codification('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', fixed_text).
narrative_ontology:cs_authority_grounding('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', lineage).
narrative_ontology:cs_interpretation_layer_present('aa8b831e-77d2-4b98-9a99-dfe9b1249d84').
narrative_ontology:cs_reading_relation('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', foundational, individual_armed_capacity_essential_for_liberty).
narrative_ontology:cs_axiom_status(individual_armed_capacity_essential_for_liberty, holdable).
narrative_ontology:cs_axiom_grounding('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', individual_armed_capacity_essential_for_liberty, deontological).
narrative_ontology:cs_axiom('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', secondary, state_disarmament_precedes_tyranny).
narrative_ontology:cs_axiom_status(state_disarmament_precedes_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', state_disarmament_precedes_tyranny, empirically_contingent).
narrative_ontology:cs_reference_frame('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', armed_populace_check_on_tyranny).
narrative_ontology:cs_drift_state('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', contemporary_mass_violence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa8b831e-77d2-4b98-9a99-dfe9b1249d84', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, gun_rights_advocacy_groups).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, legislators_seeking_gun_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who believe their right to bear arms, including military-grade weapons, is essential for deterring government overreach or for potential armed resistance. They actively resist gun control measures and interpret the Second Amendment broadly.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    powerful, generational, constrained, national).

% Law enforcement, military, and intelligence agencies tasked with maintaining public order and national security. They face the challenge of potential armed opposition from citizens and are constrained in their ability to disarm the populace, which they view as a threat to stability.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% Elected officials and policymakers who advocate for stricter firearms regulations to reduce gun violence and enhance public safety. Their legislative efforts are often challenged and overturned based on this reading of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, legislators_seeking_gun_control, payer,
    institutional, biographical, constrained, national).

% Companies that produce and sell firearms, particularly those that benefit from the expanded market for military-style weapons and accessories. They actively lobby against gun control and support interpretations that maximize gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers, beneficiary,
    organized, generational, arbitrage, global).

% Organizations that champion gun ownership rights and actively promote the insurrectionist reading of the Second Amendment. They mobilize voters, fund legal challenges, and exert significant political pressure to prevent firearms regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_rights_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Individuals and communities who are directly exposed to gun violence, mass shootings, or the potential for armed civil unrest. They bear the human cost of widespread access to firearms and the erosion of state authority.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Academics and legal experts who study the history, text, and evolving interpretations of the Second Amendment. They provide critical analysis but do not directly participate in the political or legal enforcement of the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to coordinate a decentralized defense against potential government overreach, ensuring that the populace retains the capacity to resist tyranny and preserve liberty.
% TRANSFER_FUNCTION: Transfers the burden of potential armed conflict and the costs of widespread firearm access from the state to individual citizens and society at large, while transferring political power and market opportunities to armed citizens and the firearms industry.
% ABSENT_VOICES: Victims of gun violence, public health experts, and international human rights organizations are largely excluded from the constitutional debate, or their concerns are reframed as secondary to the 'right' to armed resistance. They would argue for a reinterpretation that prioritizes collective safety.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished overnight, it would fundamentally alter the landscape of firearms policy, potentially leading to significant restrictions on gun ownership, a reassertion of state authority in public safety, and a shift in the balance of power between citizens and the government. The firearms industry would face severe market contraction.
% FOUNDING_PROBLEM: The founding problem was to ensure that a free state could be maintained by a well-regulated militia, and to provide a check against potential federal tyranny by preserving the people's right to keep and bear arms.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including gun rights advocacy groups and some legal scholars, argue the threat of tyranny remains live. Opponents, including public safety advocates and other constitutional scholars, contend that the nature of government and warfare has changed, rendering the 'insurrectionist' premise obsolete and that the constraint now primarily serves private interests. Historical analysis from outside the benefiting parties suggests the original intent was more focused on collective militia service than individual insurrection.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it purports a coordination function (deterring tyranny, protecting liberty) but demonstrably involves asymmetric extraction and requires active enforcement to maintain. Extractiveness (0.65) is high due to the costs imposed on the state's ability to maintain public order and the potential for violence. Suppression (0.75) is also high, as the state's efforts to regulate firearms are actively resisted and undermined by this reading. Theater ratio (0.4) reflects that while the 'deterrence against tyranny' narrative is performative, there are real-world consequences and active enforcement of the right to bear arms, including legal challenges to gun control. The rising extractiveness and suppression over time reflect increasing polarization and the hardening of positions around this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'armed_citizens_claiming_deterrent_legitimacy', this constraint is a vital Rope, preserving liberty and acting as a check on government power. From the perspective of 'state_security_apparatus' and 'civilians_in_conflict_zones', it is a Snare, enabling violence and undermining collective security. The engine's classification as Tangled Rope reflects the hybrid nature of its claimed coordination function and its actual extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   'Armed_citizens_claiming_deterrent_legitimacy', 'firearms_manufacturers', and 'gun_rights_advocacy_groups' are beneficiaries (low d) as they gain expanded rights, markets, and political influence. 'State_security_apparatus', 'civilians_in_conflict_zones', and 'legislators_seeking_gun_control' are victims (high d) as they bear the costs of increased armed capacity and constrained regulatory power. The 'insurrectionist_reading' structurally subsidizes the armed citizen while extracting from the state's capacity to govern.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the 'insurrectionist_reading' as a pure Mountain (natural law) or Rope (pure coordination). While it claims to coordinate against tyranny, its high extractiveness and suppression, coupled with identifiable victims, reveal its hybrid nature. The 'founding_problem_status' being 'contested' further highlights the ongoing debate about whether the original mandate (deterring tyranny) is still live or if the constraint has drifted into a mechanism for rent-seeking and political power for its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of the Second Amendment''s original intent, or a modern political construction?',
    'Historical and legal scholarship on the framing era''s understanding of ''militia'' and ''bear arms'' in relation to individual insurrectionary capacity.',
    'If a modern construction, its legitimacy as a constitutional constraint is weakened, potentially reclassifying it as a Snare or Piton sustained by political power rather than legal principle. If genuine, it reinforces its Mountain-like claim to constitutional permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''insurrectionist_reading'' of the ''second_amendment_boundary'' kernel. Sibling readings (''individual_right_reading'', ''militia_conditioned_reading'') offer alternative interpretations of the Second Amendment''s scope and purpose, leading to different classifications and beneficiary/victim sets.').

omega_variable(
    tyranny_threshold_ambiguity,
    'What constitutes ''tyrannical government'' sufficient to justify armed resistance, and who adjudicates this threshold?',
    'No clear resolution mechanism exists; the threshold is inherently subjective and contested, leading to perpetual ambiguity and potential for abuse.',
    'The absence of an objective threshold means the constraint''s justification is self-referential for its beneficiaries, amplifying its extractive potential and making it harder to challenge through conventional legal means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tyranny_threshold_ambiguity, preference, 'The core justification for this reading is inherently ambiguous, allowing beneficiaries to define the conditions for their own ''right'' to armed resistance.').

omega_variable(
    military_arms_scope_ambiguity,
    'Does the ''right to bear arms'' under this reading extend to military-grade weapons, and if so, what limits apply?',
    'Judicial rulings or legislative action explicitly defining the types of arms protected under an insurrectionist interpretation.',
    'If military-grade arms are protected, the constraint''s impact on public safety and state authority is significantly amplified, increasing the victim set and the level of suppression required to maintain order. If limited, the reading''s core premise of effective resistance capacity is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_arms_scope_ambiguity, empirical, 'The scope of protected arms under this reading is contested, directly impacting its practical implications and the severity of its potential outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__insurrectionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__insurrectionist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__insurrectionist_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, firearms_licensing_regulations).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, assault_weapons_ban).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel. Each reading has a different structural interpretation, leading to different classifications and impacts on related firearms policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
