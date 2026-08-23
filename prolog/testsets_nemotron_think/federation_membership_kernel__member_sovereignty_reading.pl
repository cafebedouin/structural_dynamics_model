% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty Reading of Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   This constraint story captures the member_sovereignty_reading of the
 *   federation_membership_kernel — the claim that free movement rights must
 *   be bounded by national welfare state capacity and labor market
 *   protection, with member states retaining authority to exclude
 *   economically inactive migrants. The reading presents itself as protecting
 *   social solidarity institutions (tangled_rope coordination function) while
 *   structurally extracting from mobile populations and sending states
 *   (asymmetric extraction). The engine will compute per-seat classifications
 *   from the declared structural data; this story's claimed_type
 *   (tangled_rope) and metrics are authored independently per the
 *   claim/metric independence rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty Reading of Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'cfb314d5-0252-4a77-b540-f61f754dff45').
narrative_ontology:cs_kernel_codification('cfb314d5-0252-4a77-b540-f61f754dff45', formalized).
narrative_ontology:cs_authority_grounding('cfb314d5-0252-4a77-b540-f61f754dff45', extraction).
narrative_ontology:cs_interpretation_layer_present('cfb314d5-0252-4a77-b540-f61f754dff45').
narrative_ontology:cs_reading_relation('cfb314d5-0252-4a77-b540-f61f754dff45', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfb314d5-0252-4a77-b540-f61f754dff45', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('cfb314d5-0252-4a77-b540-f61f754dff45', foundational, national_welfare_autonomy_primacy).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('cfb314d5-0252-4a77-b540-f61f754dff45', national_welfare_autonomy_primacy, conventional).
narrative_ontology:cs_axiom('cfb314d5-0252-4a77-b540-f61f754dff45', foundational, economically_inactive_exclusion_legitimacy).
narrative_ontology:cs_axiom_status(economically_inactive_exclusion_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cfb314d5-0252-4a77-b540-f61f754dff45', economically_inactive_exclusion_legitimacy, conventional).
narrative_ontology:cs_reference_frame('cfb314d5-0252-4a77-b540-f61f754dff45', maastricht_welfare_competence_reservation).
narrative_ontology:cs_drift_state('cfb314d5-0252-4a77-b540-f61f754dff45', post_crisis_austerity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cfb314d5-0252-4a77-b540-f61f754dff45', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_beneficiaries).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_market_incumbents).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers_economically_active).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, mobile_workers_economically_active).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, national_welfare_state_sovereignty).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, social_solidarity_institution_protection).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, fiscal_federalism_subsidiarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the rules bounding free movement through national legislation, bilateral agreements, and ECJ litigation strategy. They control welfare access criteria, labor market tests, and expulsion powers. They collect the political benefit of protecting national solidarity institutions while exporting adjustment costs to sending states and mobile populations. Their exit option is treaty opt-out or veto power in Council formations.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% Face direct exclusion from welfare systems, residence rights, and labor market access in receiving states. Include retirees, students, caregivers, jobseekers, and family members without independent economic activity. Their mobility is bounded by resource tests they cannot meet. Exit means return to sending states with depleted support networks or irregular status. They bear the fiscal externalization cost that receiving states refuse to socialize.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, local).

% Experience intensified brain drain as receiving states select for economically active migrants while excluding dependents. Their human capital investment leaves with mobile workers, but the fiscal returns (taxes, care labor, social reproduction) are not reciprocated. They cannot easily exit the sending state's demographic trajectory. Their constrained exit reflects EU-level policy asymmetry: free movement of labor without free movement of welfare solidarity.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, payer,
    moderate, biographical, constrained, regional).

% Gain labor market access in receiving states but face precarious residence tied to employment, restricted family reunification, and no portable welfare entitlements. They pay into receiving state systems but cannot claim equivalent benefits if economically inactive. Their dual position: beneficiaries of mobility rights, payers of the sovereignty premium. Exit options constrained by career specificity, language, and credential recognition barriers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers_economically_active, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, mobile_workers_economically_active, beneficiary).

% Citizens and long-term residents of receiving states whose welfare entitlements are insulated from fiscal pressure by excluding economically inactive migrants. They benefit from sustained contribution rates and protected benefit levels. Their mobility is high (can move within federation with full rights). They organize politically to defend the sovereignty reading against integration pressure.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_beneficiaries, beneficiary,
    organized, generational, mobile, continental).

% Workers in receiving states protected from wage compression and competition by mobility restrictions that filter for economic activity and allow expulsion of non-workers. They benefit from labor market insulation while mobile workers bear the flexibility cost. Their organizations (unions, professional bodies) lobby for maintained sovereignty controls. Exit options high within national labor market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_market_incumbents, beneficiary,
    organized, biographical, mobile, national).

% ECJ, Commission, Parliament attempt to interpret free movement expansively (integration_reading) but are constrained by member state vetoes in Council and treaty amendment requirements. They issue rulings that incrementally expand mobility (e.g., jobseeker status, student maintenance) but face political pushback. Their analytical seat sees the full structural asymmetry but lacks enforcement leverage against sovereign welfare competence.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_supranational_institutions, observer,
    institutional, generational, analytical, continental).

% Would object to the asymmetry of bearing brain drain costs without fiscal reciprocity but are structurally excluded from receiving states' welfare sovereignty decisions. Their leverage is limited to EU budget negotiations and cohesion policy conditionality. They cannot exit the federation without enormous cost. Their exclusion is baked into the treaty architecture: welfare competence remains national while mobility competence is shared.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_member_states, excluded,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects national welfare state fiscal sustainability and labor market coherence by bounding free movement to economically active participation, preventing adverse selection into generous welfare systems and maintaining contribution-based solidarity.
% TRANSFER_FUNCTION: Moves fiscal burden and labor market adjustment costs from receiving state welfare systems to economically inactive migrants and sending states; moves mobility rights from mobile workers to member state discretion; moves brain drain costs from receiving to sending states without compensation.
% ABSENT_VOICES: Economically inactive migrants (structurally excluded by definition), sending state populations experiencing brain drain without fiscal transfer, future mobile workers whose rights are pre-emptively bounded by current sovereignty claims, third-country nationals affected by externalized border controls.
% DISAPPEARANCE_RATIONALE: If member state authority to exclude economically inactive migrants vanished, receiving state welfare systems would face unbounded fiscal exposure from non-contributor migration, labor markets would adjust through wage compression rather than exclusion, free movement would expand to de facto universal mobility within the federation, and sending states would gain leverage for fiscal reciprocity demands — the entire EU social policy architecture would reorganize around either fiscal federalism or welfare retrenchment.
% FOUNDING_PROBLEM: Post-war European integration needed to reconcile free movement of workers with nationally bounded welfare states that lacked fiscal capacity for universal coverage of non-contributors; member states would not join a federation that socialized welfare costs without harmonizing benefit levels.
% FOUNDING_PROBLEM_CORROBORATION: Treaty negotiating history (Maastricht 1992, Amsterdam 1997) shows member states explicitly reserved welfare competence in Protocols and Declarations; independent fiscal federalism literature (Oates 1972, Rodden 2006) corroborates the fiscal federalism dilemma as structural; ECJ case law (Dano 2014, Alimanovic 2015, CG 2020) confirms the contested status — the Court upholds exclusions while recognizing they fragment Union citizenship.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the fiscal externalization onto sending states and the mobility restrictions on economically inactive populations. Suppression (0.72) is high because the constraint persists through active enforcement: residence tests, resource requirements, expulsion orders, and ECJ litigation defending national discretion. Theater ratio (0.42) captures the growing performative dimension — solidarity rhetoric masks fiscal protectionism as ECJ rulings incrementally expand mobility. Accessibility collapse (0.58) is moderate: alternatives (fiscal federalism, portable benefits) exist but are politically blocked. Resistance (0.55) comes from mobile workers, sending states, and supranational institutions but is fragmented across venues.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (receiving member states) experiences this as genuine coordination: they built welfare states requiring bounded solidarity. The payer seats (economically inactive migrants, sending state workers) experience it as extraction: their mobility and fiscal futures are constrained by rules they did not choose. The engine computes this divergence from power/exit/spatial_scope declarations. The dual-role mobile workers (payer/beneficiary) sit at the structural fault line — their classification will reveal whether the coordination function meaningfully offsets the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving member states are structural beneficiaries (d ~ 0.15) — they set rules, collect political benefits, export costs. Economically inactive migrants are full targets (d ~ 0.95) — trapped, powerless, bear exclusion directly. Sending state workers are targets (d ~ 0.8) — constrained exit, bear brain drain costs. Mobile workers are symmetric-payers (d ~ 0.6) — gain access but pay sovereignty premium. Receiving state welfare beneficiaries and labor incumbents are beneficiaries (d ~ 0.2). EU institutions are analytical observers (d ~ 0.5) — see structure but lack leverage. Sending member states are excluded (d ~ 0.7) — bear costs without voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling mobility with nationally bounded welfare) remains contested — not dead, not fully live. The coordination function (preventing welfare tourism) is real but the extraction has grown beyond the founding justification: brain drain intensification, expanding exclusion categories, and declining reciprocity suggest mandatrophy drift. The constraint persists because fixing it requires treaty change (prohibitive fixing_cost) and the gains flow to receiving state political coalitions (gain_flow: receiving_member_states). This is not a piton — the function has not atrophied; it has been weaponized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the member_sovereignty_reading a genuine coordination necessity for welfare state survival, or an extraction cover that uses welfare protection as a pretext for mobility restriction?',
    'Counterfactual fiscal federalism simulation: if receiving states received fiscal transfers equal to the marginal cost of economically inactive migrants, would they maintain exclusion rules? Comparative analysis of federal systems with fiscal equalization (Germany, Canada) vs. without.',
    'If genuine coordination, the constraint is a necessary tangled_rope with irreducible extraction floor. If extraction cover, it is a snare with mobility restriction as the primary function. The classification divergence across seats would sharpen: payer seats would compute snare, agenda_setter would compute rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the sovereignty reading''s coordination function is structurally necessary or strategically constructed.').

omega_variable(
    welfare_capacity_measurement,
    'What constitutes ''welfare state capacity'' as the bounding criterion — fiscal headroom, administrative capacity, political solidarity, or a strategic ambiguity that expands with migration pressure?',
    'Longitudinal coding of national government positions in Council negotiations and ECJ interventions: track whether capacity claims correlate with objective fiscal indicators or with political cycles and migration salience.',
    'If capacity is objectively measurable, the constraint has a natural boundary. If strategically ambiguous, the constraint''s extraction boundary expands endogenously — a ratchet mechanism characteristic of snares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_capacity_measurement, empirical, 'Whether the bounding criterion is a fixed structural limit or a movable political target.').

omega_variable(
    brain_drain_causality,
    'Does the sovereignty reading''s restriction on economically inactive mobility cause intensified brain drain from sending states, or does brain drain reflect independent sending-state failures that the reading merely fails to compensate?',
    'Difference-in-differences analysis of sending state emigration patterns before/after major sovereignty reading jurisprudence (Dano 2014, Alimanovic 2015) compared to non-EU sending states with similar economic profiles.',
    'If causal, the constraint''s extraction extends intergenerationally to sending state development trajectories — a structural externality not priced in the coordination calculus. If correlational, the extraction claim is weaker and the coordination function more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_causality, empirical, 'Causal attribution of brain drain to mobility asymmetry vs. sending state domestic factors.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (legal barriers, resource tests, expulsion machinery) or does it include internalized suppression (migrants self-excluding due to anticipated hostility, welfare administrators applying rules beyond legal minimum)?',
    'Post-exit suppression trajectory study: track economically inactive migrants who acquire economic activity or citizenship — does suppression experience persist? Survey welfare administrators on discretionary application of resource tests.',
    'If internalized component is significant, effective suppression is higher than legal structure suggests — the constraint operates through subject formation as well as coercion. This would increase χ for payer seats beyond the structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal-administrative constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_msr_tr_t1992, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(fmk_msr_tr_t1997, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(fmk_msr_tr_t2004, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(fmk_msr_tr_t2011, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(fmk_msr_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(fmk_msr_tr_t2024, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fmk_msr_be_t1992, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(fmk_msr_be_t1997, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1997, 0.52).
narrative_ontology:measurement(fmk_msr_be_t2004, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(fmk_msr_be_t2011, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2011, 0.62).
narrative_ontology:measurement(fmk_msr_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(fmk_msr_be_t2024, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmk_msr_su_t1992, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(fmk_msr_su_t1997, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1997, 0.6).
narrative_ontology:measurement(fmk_msr_su_t2004, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(fmk_msr_su_t2011, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(fmk_msr_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(fmk_msr_su_t2024, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the federation_membership_kernel alongside integration_reading (expansive mobility as citizenship right, supranational authority) and welfare_coordination_reading (coordinated national systems with anti-social-dumping rules). The three readings share the treaty kernel but instantiate different constraints: integration_reading has low ε (coordination dominant), this reading has moderate-high ε (extraction dominant), welfare_coordination_reading has moderate ε (coordination with enforcement). They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
