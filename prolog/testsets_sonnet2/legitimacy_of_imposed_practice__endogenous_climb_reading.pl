% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: State-Decreed Practice Displacement Without Internalization (Endogenous Climb Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state decrees the replacement of a lunar calendar and
 *   traditional dress with standardized alternatives, treating decree
 *   authority as sufficient to produce compliance. This reading of the shared
 *   kernel holds that the decree fails on its own terms: the lunar calendar
 *   persists in rural and ritual use for decades, and dress reform achieves
 *   only partial urban diffusion accompanied by private retention of the old
 *   forms — evidence, on this reading, of adoption without internalization.
 *   Rising theater_ratio and suppression_requirement over the interval trace
 *   the state's shift from expecting voluntary adoption to escalating
 *   enforcement theater as the gap between decree and lived practice widens
 *   rather than closes. This story instantiates the endogenous_climb_reading
 *   of the kernel legitimacy_of_imposed_practice; it does not represent or
 *   average over the sibling readings (exogenous_override_reading,
 *   hybrid_scaffolding_reading), which are separate constraints with their
 *   own ε and structural data.
 *
 * KEY AGENTS:
 *   - state_reform_ministry: agenda_setter — decrees displacement, escalates enforcement as compliance fails to materialize
 *   - communities_preserving_autonomy: beneficiary — continue lived practice unchanged, bear no material cost of the decree's persistence
 *   - state_modernization_program: payer — the reform's stated goal (displacement) is never achieved despite decades of investment
 *   - urban_compliant_households: payer/beneficiary — bear double-practice costs while gaining conditional access to state institutions
 *   - traditionalist_elders: excluded — hold the functional knowledge that predicted the failure, never consulted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.61).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "State-Decreed Practice Displacement Without Internalization (Endogenous Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'f56b10bd-ab55-4c76-bf52-176123fbd51c').
narrative_ontology:cs_kernel_codification('f56b10bd-ab55-4c76-bf52-176123fbd51c', distributed).
narrative_ontology:cs_authority_grounding('f56b10bd-ab55-4c76-bf52-176123fbd51c', extraction).
narrative_ontology:cs_interpretation_layer_present('f56b10bd-ab55-4c76-bf52-176123fbd51c').
narrative_ontology:cs_reading_relation('f56b10bd-ab55-4c76-bf52-176123fbd51c', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('f56b10bd-ab55-4c76-bf52-176123fbd51c', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('f56b10bd-ab55-4c76-bf52-176123fbd51c', foundational, displacement_requires_bottom_up_internalization).
narrative_ontology:cs_axiom_status(displacement_requires_bottom_up_internalization, holdable).
narrative_ontology:cs_axiom_grounding('f56b10bd-ab55-4c76-bf52-176123fbd51c', displacement_requires_bottom_up_internalization, empirically_contingent).
narrative_ontology:cs_axiom('f56b10bd-ab55-4c76-bf52-176123fbd51c', secondary, decree_absent_adoption_pathway_produces_durable_noncompliance).
narrative_ontology:cs_axiom_status(decree_absent_adoption_pathway_produces_durable_noncompliance, holdable).
narrative_ontology:cs_axiom_grounding('f56b10bd-ab55-4c76-bf52-176123fbd51c', decree_absent_adoption_pathway_produces_durable_noncompliance, empirically_contingent).
narrative_ontology:cs_reference_frame('f56b10bd-ab55-4c76-bf52-176123fbd51c', pre_decree_customary_practice_equilibrium).
narrative_ontology:cs_drift_state('f56b10bd-ab55-4c76-bf52-176123fbd51c', post_decree_multigenerational, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f56b10bd-ab55-4c76-bf52-176123fbd51c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, informal_local_authorities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_program).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliant_households).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_enforcement_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliant_households).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees replacement of the lunar calendar and traditional dress with standardized state alternatives, framing this as modernization and national unification. Issues enforcement circulars, monitors compliance through local officials, and treats non-adoption as backwardness to be corrected rather than as evidence the mandate lacks a bottom-up adoption pathway.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_reform_ministry, agenda_setter,
    institutional, generational, arbitrage, national).

% Continue observing the lunar calendar for planting, ritual, and market timing and retain traditional dress in private and rural settings, regardless of official decree. They gain nothing materially from defying the state but preserve a functioning practice ecology that the imposed calendar cannot replace, since the decree offers no felt reason to abandon what already works for them.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    moderate, generational, constrained, regional).

% The reform's own success metric — displacement achieved — never materializes. Decades after decree, the lunar calendar persists in daily rural life; dress reform achieves only partial urban diffusion, with private retention undermining the claim of adoption. The program bears the cost of continued enforcement spending, credibility loss, and repeated re-launch cycles, without ever collecting the legitimacy it sought.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_program, payer,
    institutional, generational, trapped, national).

% Adopt the new dress code publicly to access state employment, education, and urban social standing, while retaining older dress privately at home or among kin. They pay a double cost — maintaining two wardrobes and two identities — without the promised full transition ever completing around them, since the surrounding population never fully follows.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliant_households, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliant_households, beneficiary).

% Tasked with certifying compliance to superiors while living among populations who have not internalized the new practices. They routinely falsify or exaggerate compliance reports to satisfy the ministry, absorbing the friction between decree and reality, and risk censure if the gap becomes visible.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials, payer).

% Devotes recurring budget and personnel to inspections, campaigns, and punitive measures meant to force adoption, decade after decade, without the underlying practice ever taking root voluntarily. Bears the sunk cost of enforcement that never becomes unnecessary because the practice it enforces never internalizes.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_enforcement_apparatus, payer,
    institutional, generational, trapped, national).

% Hold the practical and cosmological knowledge embedded in the lunar calendar and traditional dress but are not consulted in the design of the replacement systems. Their objection — that the new system serves no function the old one didn't already serve, and breaks functions it doesn't — is never solicited by the reform ministry.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, traditionalist_elders, excluded,
    powerless, generational, trapped, regional).

% Study the persistence of the lunar calendar and the incomplete dress transition as evidence for a general claim: decreed practice displacement without an internalization pathway produces durable non-compliance regardless of enforcement duration or intensity.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historical_state_formation_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, standardizing calendar and dress could coordinate administrative scheduling, labor markets, and a unified national self-image across a diverse population. The endogenous-climb reading holds that this coordination function is real in the abstract but was never actually achieved by decree — the coordination good was promised, not delivered.
% TRANSFER_FUNCTION: Moves enforcement costs, compliance-reporting labor, and reputational risk from the state onto local officials and rural populations, while moving cultural authority and adoption timeline credit nominally to the state — credit the reading holds the state never actually earns, since adoption never completes.
% ABSENT_VOICES: Traditionalist elders and rural practitioners whose calendar and dress carry embedded ritual, agricultural, and kinship function were not consulted in designing the replacement; their functional objections would have predicted the failure mode the state now lives with.
% DISAPPEARANCE_RATIONALE: The state would say the world rearranges badly (chaos, reversion, loss of unification project); communities practicing continuity would say the world is largely unchanged, since they never adopted the imposed practice in the first place — the decree's disappearance would mainly relieve enforcement pressure and double-wardrobe costs without altering daily practice.
% FOUNDING_PROBLEM: The state sought to unify a fragmented population under standardized administrative time and dress to accelerate modernization, reduce regional distinctiveness read as backwardness, and align with international/urban elite norms.
% FOUNDING_PROBLEM_CORROBORATION: The reform ministry attests the modernization problem remains live and requires continued enforcement. Historical state-formation scholars, working from compliance-report archives and ethnographic accounts of continued lunar and dress practice, attest that the problem as originally framed (administrative unification) was never actually solved by decree and that the persistence of old practice for decades demonstrates the imposed pathway lacked the internalization mechanism required for real displacement.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects real but partial costs: enforcement resources, double-practice burdens on urban compliant households, and reputational costs to the state, but not a fully captured extraction regime since the targeted population largely retains functional autonomy. Suppression (0.72) is high because the state relies on inspection, penalty, and compliance-report pressure rather than persuasion or incentive — precisely the enforcement-without-internalization pattern the reading names. accessibility_collapse is low (0.35) because the old calendar and dress remain genuinely accessible and practiced; the decree never actually closes off the alternative, it only criminalizes or stigmatizes it. resistance is high (0.74): quiet, durable, distributed non-compliance across decades is itself a form of resistance, not passive drift.
 *
 * DIRECTIONALITY LOGIC:
 *   communities_preserving_autonomy sit near the beneficiary end: the constraint's operation (state enforcement pressure) does not actually change their practice, so directionality derives low effective extraction despite nominal target status in the decree's own framing. state_modernization_program and state_enforcement_apparatus sit as institutional payers: they invest continuously and collect nothing, an unusual but real position for an institutional actor. urban_compliant_households carry dual directionality — partial beneficiary (access gained), partial payer (double-practice cost) — reflected in their dual role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative/cultural unification) is genuinely contested as live vs. dead: the state insists it is live and requires continued enforcement; the historical record the reading relies on shows the specific decree pathway never solved it and the mandate has arguably outlived any evidence of working. Classifying this as tangled_rope rather than snare preserves the genuine (if unrealized) coordination aspiration — unification is not itself illegitimate — while registering that the enforcement apparatus persisting past any internalization is where the extraction actually accrues (compliance-reporting labor, enforcement budget, double-wardrobe costs), distinct from a pure extraction reading that would deny any coordination motive existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_endogenous_climb,
    'This constraint is one reading (endogenous_climb_reading) of the kernel legitimacy_of_imposed_practice. Do the exogenous_override_reading and hybrid_scaffolding_reading siblings characterize the same historical episode more accurately for some sub-populations (e.g., urban elites under hybrid scaffolding) even as this reading holds for rural/traditional populations?',
    'Disaggregate compliance data by region and class: if urban populations show hybrid-scaffolding-consistent adoption (ideological messaging producing quasi-endogenous pull) while rural populations show pure climb-failure, the kernel may require reading-mixture rather than single-reading dominance, but each sub-population''s constraint remains a separate story per the ε-invariance principle.',
    'If validated, this reading''s ε and classification hold specifically for the rural/traditional population segment; the hybrid_scaffolding_reading''s separate story would carry the urban segment''s higher partial-success ε. Confirms the decomposition rather than collapsing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_endogenous_climb, conceptual, 'Whether the endogenous-climb-failure account applies uniformly or only to specific sub-populations within the broader imposed-practice episode.').

omega_variable(
    internalization_pathway_existence,
    'Was there ever a feasible bottom-up adoption pathway (e.g., generational education, economic incentive alignment, community co-design) that the state failed to build, or was the practice displacement attempted always structurally incompatible with the target population''s material conditions regardless of pathway design?',
    'Comparative case analysis against other state modernization episodes where bottom-up pathways were deliberately constructed (e.g., phased incentive programs, community liaison structures) and did achieve durable displacement within a generation.',
    'If feasible pathways existed and were simply not built, the state''s decree-only approach is a policy failure with clear counterfactual remedy, strengthening the tangled_rope reading (coordination goal was achievable, extraction was avoidable). If no such pathway was ever feasible given material conditions, the founding problem itself may have been mis-specified, pushing the classification toward pure extraction (snare) with unification as pure pretext.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_pathway_existence, empirical, 'Whether failure was a remediable pathway-design gap or a structural incompatibility with the modernization goal itself.').

omega_variable(
    cs_framing_kernel_vs_enforcement_apparatus,
    'Is the correct kernel-level framing the imposed practice itself (calendar/dress decree) or the deeper legitimacy claim layered above it — the state''s claim to authority to define national practice at all? A reading that treats the decree as the kernel classifies enforcement failure as policy failure; a reading that treats the state''s definitional authority as the kernel classifies the same failure as evidence against the authority claim itself.',
    'Track whether failure of specific decrees (calendar, dress) erodes the state''s general claim to define national practice in subsequent policy domains, or whether each decree failure is treated as isolated and the general authority claim survives intact.',
    'If the deeper authority-claim framing is correct, this constraint''s persistent failure feeds a cross-domain erosion of state definitional legitimacy beyond calendar/dress specifically — a broader and more severe classification than a single-policy tangled_rope reading suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_enforcement_apparatus, conceptual, 'Alternative framing: the decree-as-kernel versus the state''s general definitional authority as the true kernel under contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 32, 0.56).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kernel legitimacy_of_imposed_practice. exogenous_override_reading claims decree authority alone suffices for displacement (predicting low extraction, high compliance, minimal enforcement drift). hybrid_scaffolding_reading claims ideological reinforcement layered on decree achieves partial quasi-endogenous displacement (predicting moderate extraction, moderate theater). This reading (endogenous_climb_reading) claims decree without internalization pathway fails structurally, predicting persistent high suppression_requirement and rising theater_ratio without corresponding extractiveness collapse — the enforcement apparatus works harder for the same non-result. Each story carries its own ε and stakeholder structure per the ε-invariance principle; they are not measurement-parameter variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
