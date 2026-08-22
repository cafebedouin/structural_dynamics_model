% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Flood Preparedness: Technical Retention / Ceremonial Diffusion
 *   domain: governance/disaster preparedness
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the preparedness_retention
 *   kernel: Dutch flood preparedness is neither purely live competence
 *   (competence_reading) nor purely hollow ritual (husk_reading), but a
 *   bifurcated system in which real, exercised technical mastery is retained
 *   within specialized institutions (Rijkswaterstaat, regional water boards,
 *   professional engineering corps) while the broader population's
 *   relationship to preparedness has drifted toward ceremonial participation
 *   — drills, commemorations, and symbolic civil-defense roles that transmit
 *   reassurance rather than operational skill. The dual-track structure is
 *   the story's defining feature: it is simultaneously a genuine coordination
 *   solution (concentrating scarce expertise efficiently) and an extraction
 *   mechanism (institutional continuity and professional monopoly purchased
 *   at the cost of distributed societal resilience). Under the ε-invariance
 *   discipline, this reading's ε (0.55) is a property of THIS reading's
 *   account of the standing stratified arrangement — not an average of the
 *   sibling readings' ε values, and not the ε of a hypothetically
 *   fully-distributed alternative.
 *
 * KEY AGENTS:
 *   - rijkswaterstaat: primary agenda-setter and beneficiary (institutional/arbitrage) — retains technical competence and directs resource allocation
 *   - regional_water_boards: beneficiary/agenda-setter (organized/constrained) — historic semi-autonomous technical bodies now professionalized
 *   - specialized_engineering_corps: beneficiary (powerful/mobile) — exclusive holders of live operational knowledge
 *   - coastal_municipal_residents: primary payer (moderate/trapped) — dependent on centralized competence, no distributed fallback
 *   - volunteer_civil_defense_networks: payer (powerless/constrained) — displaced local tacit knowledge, now ceremonial
 *   - peripheral_polder_communities: payer (powerless/trapped) — furthest from investment, first deprioritized under stress
 *   - national_disaster_scholars: analytical observer — sees the full stratification structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.55).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Flood Preparedness: Technical Retention / Ceremonial Diffusion").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '7225fbda-e869-4d9f-af70-dabfdf1a7fbb').
narrative_ontology:cs_kernel_codification('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', distributed).
narrative_ontology:cs_authority_grounding('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', expertise).
narrative_ontology:cs_interpretation_layer_present('7225fbda-e869-4d9f-af70-dabfdf1a7fbb').
narrative_ontology:cs_reading_relation('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', foundational, competence_and_ceremony_are_structurally_separable_tracks).
narrative_ontology:cs_axiom_status(competence_and_ceremony_are_structurally_separable_tracks, holdable).
narrative_ontology:cs_axiom_grounding('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', competence_and_ceremony_are_structurally_separable_tracks, empirically_contingent).
narrative_ontology:cs_axiom('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', secondary, institutional_continuity_is_a_legitimate_independent_value_from_distributed_resilience).
narrative_ontology:cs_axiom_status(institutional_continuity_is_a_legitimate_independent_value_from_distributed_resilience, holdable).
narrative_ontology:cs_axiom_grounding('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', institutional_continuity_is_a_legitimate_independent_value_from_distributed_resilience, instrumental).
narrative_ontology:cs_reference_frame('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', post_1953_professionalized_defense_doctrine).
narrative_ontology:cs_drift_state('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', contemporary_climate_adaptation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7225fbda-e869-4d9f-af70-dabfdf1a7fbb', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, regional_water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_engineering_corps).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, coastal_municipal_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_polder_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the national flood defense system, retains deep technical expertise in hydraulic engineering and storm surge modeling, and decides which parts of the preparedness system get investment in live competence versus symbolic maintenance. Its institutional continuity and budgetary authority depend on being seen as the indispensable technical guarantor of safety.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, rijkswaterstaat, beneficiary).

% Historic, semi-autonomous bodies that maintain operational dike and polder management competence and levy taxes to fund it. They retain real technical capacity but have professionalized the function into a closed, specialist corps, reducing the general population's participatory role to voting for board members and attending ceremonial inspections.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, regional_water_boards, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, regional_water_boards, agenda_setter).

% The professionals — engineers, hydrologists, emergency planners — who hold the live, exercised knowledge of flood mechanics and defense operation. Their career paths, credentials, and institutional standing depend on being the exclusive holders of this competence; they benefit from a system that concentrates rather than distributes technical knowledge.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_engineering_corps, beneficiary,
    powerful, biographical, mobile, national).

% Live behind the dikes and depend entirely on the technical institutions functioning correctly. They participate in annual flood drills and civil-defense ceremonies that feel like preparedness but transmit little actual operational skill; if the centralized system fails or is overwhelmed, they have no distributed fallback competence of their own.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, coastal_municipal_residents, payer,
    moderate, biographical, trapped, regional).

% Community-level volunteer groups that once carried real local flood-response knowledge (sandbag brigades, local watch systems) but have been steadily displaced by professionalized emergency services. They now mostly perform scripted roles in staged exercises, losing the tacit, improvisational knowledge that once made them a genuine second line of defense.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks, payer,
    powerless, biographical, constrained, local).

% Small, low-lying agricultural communities furthest from major urban flood-defense investment. They inherit ceremonial preparedness rituals — flags, commemorations, school programs — without meaningful local technical capacity, and are structurally the first to be deprioritized if the centralized system is stretched across multiple simultaneous emergencies.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, peripheral_polder_communities, payer,
    powerless, generational, trapped, local).

% Academics and policy analysts who study the gap between institutional competence retention and societal preparedness, drawing on comparative flood-disaster case studies (1953 flood, Katrina, other delta nations) to assess whether the stratified system is resilient or fragile under multi-site stress.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, national_disaster_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating scarce, expensive-to-maintain technical expertise (hydraulic engineering, storm surge forecasting, dike maintenance) in specialized institutions solves a genuine problem: this knowledge is too costly and too consequential to distribute thinly across the whole population, and professionalization prevents catastrophic operational error.
% TRANSFER_FUNCTION: The arrangement moves genuine operational competence and decision authority upward into Rijkswaterstaat and the water boards, while distributing downward only symbolic reassurance (drills, commemorations, ceremonial inspections) to municipalities and volunteer networks — effectively trading distributed resilience capacity for institutional continuity and professional monopoly on expertise.
% ABSENT_VOICES: Volunteer civil defense networks and peripheral polder communities would object that their historical, tacit local knowledge has been displaced rather than supplemented — they are folded into ceremonial roles in national drills but are not consulted on whether the centralization trade-off is one they would have chosen, and their voice is structurally absent from technical planning committees dominated by credentialed engineers.
% DISAPPEARANCE_RATIONALE: If the stratified system vanished overnight — if Rijkswaterstaat and the water boards lost their concentrated technical staff and budgets simultaneously — flood defense operation would have no fallback: the ceremonial layer transmits no real operational skill, so the distributed population could not substitute for the lost centralized competence. The arrangement is genuinely load-bearing, which is precisely what makes its single-point-of-failure structure consequential.
% FOUNDING_PROBLEM: After catastrophic flooding events (notably 1953), the founding problem was that distributed, amateur, locally-organized flood response had proven insufficient against modern hydraulic engineering challenges — professionalized, centralized technical capacity was needed to design and maintain a nationally coherent defense system.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and the water boards attest the founding problem remains fully live — climate change and sea-level rise, they argue, make centralized technical mastery more necessary than ever. Independent disaster-resilience scholars and comparative case studies (e.g., post-Katrina governance reviews) corroborate that the technical problem is real and unsolved, but also document that the erosion of distributed local competence has itself become a new, un-corroborated-by-beneficiaries risk: no institution with a stake in the current arrangement is positioned to certify that ceremonial diffusion is a safe substitute for the local knowledge it replaced.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects a genuine, moderate transfer: real competence and decision authority accrue to specialized institutions while the broader population receives symbolic reassurance rather than substantive capacity — this is neither the near-zero extraction of a working coordination system (competence_reading) nor the high extraction of pure ritual capture (husk_reading), but a structurally intermediate value proper to a hybrid arrangement. Suppression (0.45) is moderate: professionalization does not coercively suppress alternatives so much as it structurally out-competes distributed local knowledge over time through resource allocation and credentialing barriers. Theater ratio (0.5) captures that roughly half of the total preparedness activity (measured across the whole population, not just the technical core) is ceremonial rather than functional — this is a story-level average across a genuinely bifurcated system, honestly reflecting the coexistence of both tracks. Accessibility collapse (0.6) is elevated because once the professionalized track is established, returning to genuinely distributed competence becomes progressively harder — institutional path-dependency and credential barriers foreclose easy reversal. Resistance (0.35) is moderate-low: peripheral and volunteer actors express dissatisfaction but lack organizational power to contest the stratification.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat, the water boards, and the engineering corps sit near the beneficiary end of directionality: they retain arbitrage-grade or mobile exit options, institutional continuity, and professional standing that the arrangement actively protects. Coastal residents, volunteer networks, and polder communities sit near the target end: their exit options range from constrained to fully trapped (they cannot relocate away from flood risk, and cannot independently reconstitute lost distributed competence), and the constraint's ceremonial layer specifically substitutes for what would otherwise be their own operational capacity. This is the structural core of the hybrid reading's victim declaration: distributed resilience is what is extracted, even though no money changes hands in the conventional sense — the transfer is of capacity and agency, concentrated upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than a clean rope or a clean snare) is the mechanism that prevents mislabeling this arrangement as either pure coordination or pure extraction. It IS solving a real problem (flood engineering genuinely benefits from professionalization) — a snare label would miss this. But it also genuinely concentrates risk and disempowers distributed actors — a rope label would launder the single-point-of-failure risk as costless coordination. The founding problem (1953-era flood catastrophe) remains partially live, which sustains the coordination function's legitimacy, but the way the solution has evolved — hollowing out the distributed layer into ceremony rather than supplementing it — is the drift the tangled_rope classification is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratification_boundary_location,
    'Where exactly does the boundary between the technically-competent core and the ceremonially-preserved periphery sit, and is that boundary itself drifting outward (i.e., is more of the system becoming ceremonial over time)?',
    'Longitudinal audit of actual operational drill outcomes, response-time data, and personnel competence testing at municipal versus national levels, compared across multiple decades.',
    'If the boundary is stable, the hybrid reading is durable; if the ceremonial zone is expanding into what was previously genuine local competence, this reading is trending toward the husk_reading over time, which would require decomposing into a new time-indexed story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_location, empirical, 'Whether the technical/ceremonial boundary is stable or expanding.').

omega_variable(
    single_point_of_failure_risk,
    'Does the centralization of technical competence create a genuine single point of failure (e.g., simultaneous multi-site flooding overwhelming the concentrated professional corps), or does professional mobility and mutual-aid agreements between water boards provide adequate redundancy?',
    'Stress-test simulation or historical case comparison (e.g., how the system performed under multiple simultaneous regional emergencies) with independent engineering review outside Rijkswaterstaat''s own assessment.',
    'If redundancy is inadequate, the victim declaration (distributed_resilience) is empirically substantiated and the tangled_rope classification is strongly supported; if redundancy is robust, the extraction claim weakens and the arrangement drifts closer to a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_point_of_failure_risk, empirical, 'Whether centralized competence constitutes genuine systemic fragility.').

omega_variable(
    reading_selection_ambiguity,
    'Is the hybrid framing itself the most defensible account of the kernel, or is it a compromise position that avoids committing to either the fuller competence_reading or the fuller husk_reading claim?',
    'Cross-reference with the two sibling stories'' own metrics and stakeholder structures; assess which reading best fits independent audit data on drill efficacy at both institutional and community levels.',
    'If evidence trends toward husk_reading''s account of the technical core itself, the beneficiary declarations here (specialized_engineering_corps as genuine competence holders) would need revision; if evidence trends toward competence_reading''s account of the periphery, the victim declarations would weaken.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Whether hybrid framing is the best-fit reading or an unresolved compromise between siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(prep_tr_t60, preparedness_retention__hybrid_reading, theater_ratio, 60, 0.5).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(prep_be_t60, preparedness_retention__hybrid_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__hybrid_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(prep_su_t60, preparedness_retention__hybrid_reading, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_retention kernel. competence_reading treats the whole apparatus as live coordination (low ε); husk_reading treats the whole apparatus as decayed ritual (high ε, snare-leaning); this hybrid_reading splits the difference structurally rather than numerically — it is not an average of the other two ε values but an independently-authored account of a genuinely bifurcated system, with its own beneficiary set (institutional continuity) and victim set (distributed resilience) that neither sibling reading declares in the same form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
