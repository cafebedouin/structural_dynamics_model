% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Heller/McDonald Line)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story captures the individual-right reading of the Second
 *   Amendment (the Heller/McDonald/Bruen line) as a structurally extractive
 *   constraint. The reading treats the prefatory militia clause as merely
 *   purposive and non-limiting, establishing a pre-existing individual right
 *   to possess firearms for self-defense. This reading requires active
 *   judicial enforcement to strike down regulations, shields the firearms
 *   market from democratic regulation, and imposes asymmetric costs on
 *   victims of gun violence. The claimed type is tangled_rope: there is a
 *   genuine coordination function (constitutional floor for self-defense
 *   against state monopoly of force) but also clear asymmetric extraction
 *   (victims bear lethal costs while beneficiaries gain protected access and
 *   commercial shield). The engine will compute per-seat classifications from
 *   the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.75).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.8).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading (Heller/McDonald Line)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'e63f3ec4-0c83-4473-b597-cb412caff693').
narrative_ontology:cs_kernel_codification('e63f3ec4-0c83-4473-b597-cb412caff693', formalized).
narrative_ontology:cs_authority_grounding('e63f3ec4-0c83-4473-b597-cb412caff693', lineage).
narrative_ontology:cs_interpretation_layer_present('e63f3ec4-0c83-4473-b597-cb412caff693').
narrative_ontology:cs_reading_relation('e63f3ec4-0c83-4473-b597-cb412caff693', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_reading_relation('e63f3ec4-0c83-4473-b597-cb412caff693', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('e63f3ec4-0c83-4473-b597-cb412caff693', foundational, individual_self_defense_core).
narrative_ontology:cs_axiom_status(individual_self_defense_core, holdable).
narrative_ontology:cs_axiom_grounding('e63f3ec4-0c83-4473-b597-cb412caff693', individual_self_defense_core, deontological).
narrative_ontology:cs_axiom('e63f3ec4-0c83-4473-b597-cb412caff693', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('e63f3ec4-0c83-4473-b597-cb412caff693', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_axiom('e63f3ec4-0c83-4473-b597-cb412caff693', secondary, presumptive_invalidity_of_regulation).
narrative_ontology:cs_axiom_status(presumptive_invalidity_of_regulation, holdable).
narrative_ontology:cs_axiom_grounding('e63f3ec4-0c83-4473-b597-cb412caff693', presumptive_invalidity_of_regulation, conventional).
narrative_ontology:cs_reference_frame('e63f3ec4-0c83-4473-b597-cb412caff693', founding_federalism_provision).
narrative_ontology:cs_drift_state('e63f3ec4-0c83-4473-b597-cb412caff693', post_bruen_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e63f3ec4-0c83-4473-b597-cb412caff693', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_regulators).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, federal_regulators).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_self_defense_as_core_right).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, prefatory_clause_non_limiting_interpretation).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, presumptive_invalidity_of_gun_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to possess firearms for self-defense independent of militia service. Can purchase, possess, and carry with substantially reduced regulatory burden. Exit from the constraint is not desired — they are the constituency the reading empowers.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_gun_owners, beneficiary,
    powerful, biographical, mobile, national).

% Operates a market constitutionally shielded from many forms of regulation. Manufacturers, dealers, and advocacy organizations (NRA, NSSF) fund litigation and lobbying to expand the reading's scope. Gains flow directly to this seat through protected commercial activity.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, firearms_industry, agenda_setter).

% Lose regulatory authority over firearms within their jurisdictions. Must craft laws that survive strict scrutiny or history-and-tradition tests. Bear political costs of both regulating and failing to regulate. Cannot exit the federal constitutional framework.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_regulators, payer,
    institutional, generational, constrained, national).

% ATF and other agencies face narrowed rulemaking authority. Background check systems, commerce regulations, and tracing requirements face constitutional challenges. Operate within the same binding interpretation as states.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_regulators, payer,
    institutional, generational, constrained, national).

% Individuals killed or injured in shootings enabled by the regulatory vacuum this reading creates. Includes mass shooting victims, accidental shootings, and criminal firearm use. No exit from the risk environment; the constraint's operation directly produces their harm.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Face heightened lethality risk when abusers retain firearm access under broad individual right protections. Surrender orders and prohibitions face constitutional challenge. Trapped in relationships where the constraint arms their abuser.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Individuals in suicidal crisis whose access to firearms — protected by this reading — converts attempt to completion. The constraint prevents waiting periods, storage requirements, and removal mechanisms that would reduce impulsive firearm suicide. No meaningful exit from the constraint's effect in crisis moments.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access, payer,
    powerless, immediate, trapped, local).

% Supreme Court and lower courts author and enforce the reading through Heller, McDonald, Bruen, and subsequent cases. Define the scope of 'history and tradition' test. Their interpretive choices are the active enforcement mechanism maintaining the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and researchers advocating for gun violence prevention. Their policy proposals (universal background checks, assault weapon bans, red flag laws) are structurally excluded by the reading's presumptive invalidity framework. Would object to the constraint's victim-producing operation but lack standing in the constitutional framework.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional floor for individual self-defense against state disarmament, resolving the collective-action problem of mutual vulnerability by guaranteeing each person the means of self-protection without reliance on state monopoly of force.
% TRANSFER_FUNCTION: Moves regulatory authority from legislatures (state and federal) to courts; moves risk of gun violence from gun owners (who retain access) to the general public and specifically vulnerable populations (domestic violence victims, suicidal individuals, communities with high gun density); moves commercial protection to firearms industry.
% ABSENT_VOICES: Future victims of gun violence not yet harmed; communities disproportionately affected by gun homicide (predominantly Black and Latino urban communities); international peers with lower firearm homicide rates whose regulatory models are foreclosed; the founding generation's collective-defense understanding which the individual-right reading displaces.
% DISAPPEARANCE_RATIONALE: If the individual-right reading vanished overnight, the regulatory landscape would revert to pre-Heller framework: states and Congress could enact comprehensive firearm regulations (licensing, bans, registration, waiting periods) without strict scrutiny. The firearms market would lose constitutional shield. Gun violence rates would likely shift as regulatory space opened. The world would rearrange dramatically.
% FOUNDING_PROBLEM: Anti-federalist fear that the new federal government would disarm state militias by disarming the citizenry; also individual fear of disarmament by a potentially tyrannical central government. The Amendment was a federalism provision protecting state militia capacity from federal interference.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Rakove, Cornell, Waldman) corroborate the collective/federalism understanding from outside the beneficiary set. Originalist scholars (Volokh, Lund) contest this, arguing individual right was understood at founding. No consensus outside the ideological camps benefiting from each reading.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) reflects the massive regulatory authority transferred from legislatures to courts and the lethal costs externalized to victims. Suppression (0.8) is high because the reading's persistence depends on active judicial enforcement — the history-and-tradition test, strict scrutiny, and categorical bans on certain regulations — not on voluntary compliance. Theater ratio (0.4) captures that the 'militia purpose' language performs a legitimating function while the operative clause does the real work. Accessibility collapse (0.85) is high because once this reading is entrenched as binding precedent, alternative readings (militia-conditioned) are practically foreclosed within the legal framework. Resistance (0.7) reflects sustained political, scholarly, and public opposition. The measurement series shows the constraint's transformation from a dormant provision (Miller era) to an actively extractive one (post-Heller/Bruen).
 *
 * PERSPECTIVAL GAP:
 *   From the gun owner/industry seat, this is a rope: genuine coordination against state overreach. From the victim seats, it is a snare: pure extraction of safety for others' rights. From the regulator seat, it is a tangled rope: some coordination value (clear rules) but overwhelming extraction of democratic authority. The engine computes this divergence from the declared power/exit/role structure — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and firearms industry are structural beneficiaries (d near 0): they collect protected rights and commercial shield. State/federal regulators are payers with constrained exit (d near 0.7): they lose authority but cannot exit the constitutional system. Victims (gun violence, DV, suicide) are full targets (d near 1.0): trapped, powerless, bearing lethal costs with zero exit. Courts are agenda_setters with analytical exit (d ~0.5): they administer but also bear institutional legitimacy costs. Public safety advocates are excluded (d undefined): structurally locked out of the constitutional conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disarmament of state militias) is contested as live vs. dead. If dead, the constraint persists as mandate atrophy: a federalism provision repurposed as individual rights shield. The reading's expansion (Heller→McDonald→Bruen) shows mandate drift: each case extends the constraint beyond the founding problem. The coordination function (self-defense floor) is real but the extraction (regulatory paralysis, victim externalities) far exceeds it. This is not a scaffold — no sunset, no transition logic. It is a tangled rope that has accumulated extraction over 85 years of doctrinal evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the second_amendment_boundary kernel, or does it collapse into the kernel itself?',
    'Compare structural outputs (beneficiaries, victims, extractiveness, type) across all three declared readings. If each produces a stable, distinct classification profile, they are distinct constraints linked by network.affects_constraints. If profiles converge, the kernel may not be genuinely contested at the structural level.',
    'If readings collapse, the kernel_id frame is analytical error — there is one constraint with observer disagreement, not multiple constraints. If distinct, each reading gets its own ε and classification, and the kernel is a genuine site of structural contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel/reading decomposition reflects structural reality or analytical imposition.').

omega_variable(
    coordination_vs_cover,
    'Is the individual self-defense coordination function genuine, or is it cover for firearms market protection and regulatory capture?',
    'Test whether the reading''s doctrinal development (Heller→Bruen) tracks self-defense necessity or market expansion. If doctrine expands to protect AR-15s, high-capacity magazines, and commercial sale structures beyond any plausible self-defense need, the coordination story is cover.',
    'If cover, the constraint reclassifies toward snare (coordination function is pretense). If genuine but overextended, remains tangled_rope with high extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_cover, conceptual, 'Whether the asserted coordination function matches the constraint''s actual doctrinal trajectory.').

omega_variable(
    historical_interpretation_ambiguity,
    'Does the founding-era evidence support an individual right unconnected to militia service, or is that a modern construction?',
    'Historical consensus from non-originalist scholars (Cornell, Rakove, Waldman, Bogus) vs. originalist scholars (Volokh, Lund, Malcolm). The engine does not resolve this; the omega records that ε is reading-indexed over a fixed referent — the abolitionist/welfarist split (OQ-26) applies: different readings author different ε for the same standing arrangement.',
    'If individual right is modern construction, the constraint''s emerges_naturally is false (correctly authored) and its claimed_type as tangled_rope reflects constructed extraction. If founding-era individual right is real, the constraint has deeper natural-law footing but still extracts asymmetrically in modern conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_interpretation_ambiguity, empirical, 'Historical grounding of the reading''s core premise — affects naturalness assessment but not extraction measurement.').

omega_variable(
    victim_set_boundary,
    'Where does the victim set end? Does it include only direct shooting victims, or also communities traumatized by gun violence, healthcare systems bearing costs, and democratic legitimacy eroded by regulatory paralysis?',
    'Expand victim stakeholders to include community_trauma_bearers, healthcare_cost_bearers, democratic_legitimacy_payers. Measure whether expanded victim set changes classification or only increases extraction magnitude.',
    'Expanded victim set increases extractiveness magnitude and may shift classification toward snare if coordination function becomes negligible relative to extraction. Current victim set is conservative minimum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Boundary of the victim set for extraction measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1939, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__individual_right_reading, theater_ratio, 1939, 0.1).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__individual_right_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_boundary__individual_right_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_boundary__individual_right_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__individual_right_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__individual_right_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(seco_be_t1986, second_amendment_boundary__individual_right_reading, base_extractiveness, 1986, 0.25).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(seco_be_t2022, second_amendment_boundary__individual_right_reading, base_extractiveness, 2022, 0.72).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__individual_right_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__individual_right_reading, suppression_requirement, 1968, 0.25).
narrative_ontology:measurement(seco_su_t1986, second_amendment_boundary__individual_right_reading, suppression_requirement, 1986, 0.3).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(seco_su_t2022, second_amendment_boundary__individual_right_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_commerce_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, state_preemption_of_local_gun_laws).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, domestic_violence_firearm_prohibitions).

% DUAL FORMULATION NOTE:
% This reading and militia_conditioned_reading are dual formulations of the same constitutional text: one treats the prefatory clause as non-limiting (individual right), the other as scope-defining (collective right). They share the kernel (Second Amendment text) but instantiate different constraints with different beneficiary/victim structures and different ε values. The insurrectionist_reading is a third formulation that adds a teleological purpose (tyranny resistance) to the individual right frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
