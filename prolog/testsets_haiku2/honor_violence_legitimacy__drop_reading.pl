% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor-Violence Legitimacy (Drop Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the drop reading of the
 *   honor_violence_legitimacy kernel: a commitment system in which dueling
 *   remained legitimated as an honor-satisfaction mechanism even as its
 *   practice became rare due to accumulating external costs (state monopoly
 *   on violence, legal liability, insurance loss, state-imposed liability for
 *   dueling deaths). The reading asserts that the legitimacy of
 *   dueling-as-honor persisted structurally during the decline period—the
 *   kernel itself did not change—but practice frequency fell because the
 *   external costs imposed by modernizing state structures made the exercise
 *   of the legitimate right economically irrational. The kernel is contested:
 *   a contraction reading argues the honor-principle itself was redefined to
 *   exclude violence; a composite reading argues both mechanisms operated.
 *   This story models the drop reading only.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.62).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.58).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor-Violence Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, 'e5f63207-e8cb-4348-901e-9b2487e8a3e3').
narrative_ontology:cs_kernel_codification('e5f63207-e8cb-4348-901e-9b2487e8a3e3', fixed_text).
narrative_ontology:cs_authority_grounding('e5f63207-e8cb-4348-901e-9b2487e8a3e3', lineage).
narrative_ontology:cs_interpretation_layer_present('e5f63207-e8cb-4348-901e-9b2487e8a3e3').
narrative_ontology:cs_reading_relation('e5f63207-e8cb-4348-901e-9b2487e8a3e3', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5f63207-e8cb-4348-901e-9b2487e8a3e3', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('e5f63207-e8cb-4348-901e-9b2487e8a3e3', foundational, legitimacy_independent_of_frequency).
narrative_ontology:cs_axiom_status(legitimacy_independent_of_frequency, holdable).
narrative_ontology:cs_axiom_grounding('e5f63207-e8cb-4348-901e-9b2487e8a3e3', legitimacy_independent_of_frequency, deontological).
narrative_ontology:cs_axiom('e5f63207-e8cb-4348-901e-9b2487e8a3e3', foundational, external_costs_suppress_but_do_not_invalidate).
narrative_ontology:cs_axiom_status(external_costs_suppress_but_do_not_invalidate, holdable).
narrative_ontology:cs_axiom_grounding('e5f63207-e8cb-4348-901e-9b2487e8a3e3', external_costs_suppress_but_do_not_invalidate, instrumental).
narrative_ontology:cs_reference_frame('e5f63207-e8cb-4348-901e-9b2487e8a3e3', aristocratic_honor_as_violence_readiness).
narrative_ontology:cs_drift_state('e5f63207-e8cb-4348-901e-9b2487e8a3e3', contemporary_state_monopoly_enforcement, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e5f63207-e8cb-4348-901e-9b2487e8a3e3', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_bearing_elite).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duelists_paying_external_costs).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, societies_bearing_violence_externality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, literary_and_cultural_carriers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and maintain the legitimacy claim that honor requires readiness to duel and that dueling is a legitimate conflict-resolution mechanism for high-status disputes. They set cultural standards, literary norms, military honor codes, and diplomatic expectations. They benefit from the constraint's existence because it provides status differentiation, conflict resolution under their control, and identity validation. Their exit from the constraint is technically possible (they could redefine honor) but costly in social standing and institutional authority.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_bearing_elite, agenda_setter,
    powerful, generational, mobile, national).

% Participate in or face pressure to participate in duels to maintain honor and social standing. They bear the immediate external costs: legal jeopardy (criminal prosecution, civil liability), bodily risk (death or injury), financial loss (liability exposure, insurance exclusions), and social/professional consequences (court-martial, loss of position). Their exit is constrained by identity-fusion: a military officer, aristocrat, or honor-bearing professional may see their self-concept and social role as inseparable from the duty to defend honor through combat. Even as costs rise, the identity lock makes exit psychologically unavailable or experienced as social death.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duelists_paying_external_costs, payer,
    moderate, biographical, identity_locked, national).

% Bear the externality costs of honor-violence: deaths, injuries, social instability from unresolved private conflicts that become public violence. They have no direct say in the honor-constraint's operation but pay in lives and social order. Their exit options are constrained by the constraint's persistence in high-status groups; they cannot unilaterally withdraw from bearing the externality. They can and do advocate for legal suppression, but that advocacy is the content of the state's enforcer role, not the societies' own independent action.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, societies_bearing_violence_externality, payer,
    organized, generational, constrained, national).

% Enforces the external costs through law (criminal statutes against dueling, homicide liability), policy (insurance exclusion standards, military codes prohibiting dueling), and institutional pressure. The state works to suppress dueling as part of the monopoly-on-violence project. The state's enforcement activity increases during periods of low practice frequency (high theater_ratio) because the constraint persists despite low legitimacy in the broader population—the state must work harder to prevent the honor-elite from reigniting the practice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Maintain and transmit the conceptual availability of dueling as an honor-mechanism through narrative, historical memory, literary tradition, and cultural mythology. They benefit from the constraint's legitimacy because it provides rich symbolic and narrative material. As practice declines, their role becomes increasingly theatrical and performative—they keep dueling 'thinkable' even as it becomes extinct. Their role distinguishes the drop reading from the contraction reading: their existence and function support the claim that legitimacy persists independent of practice frequency.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, literary_and_cultural_carriers, beneficiary,
    moderate, generational, mobile, national).

% Argue for redefining honor away from violence, for legal suppression of dueling, and for alternative mechanisms of status and conflict resolution. They are structurally excluded from setting the honor standard during the decline period, though their advocacy is part of the causal pressure that generates external costs. Their position supports the contraction reading's claim that the honor-principle itself is under redefinition. Under the drop reading, they are obstacles to the perpetuation of the legitimate mechanism, not participants in its operation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, reform_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, honor_bearing_elite).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling provides a private, elite-controlled mechanism for resolving high-status disputes, validating honor claims, and differentiating social rank through demonstrated willingness to risk death. It coordinates expectations among the honor-bearing elite about what honor requires and how conflicts are legitimately concluded—alternatives (legal courts, arbitration, negotiation) are available but are experienced as inadequate to honor-stakes questions.
% TRANSFER_FUNCTION: Transfers status, social position, and legitimacy from the challengers to the victors; transfers bodily risk, legal liability, and mortality to the duelists; transfers externality costs (violence, instability, social discord) to broader societies. The constraint also transfers legitimacy-value to the honor-bearing elite who maintain and enforce the standard.
% ABSENT_VOICES: The dead (those killed in duels, whose voice is silenced by death), future generations (who inherit the institutional memory and normative framework), and those excluded by class or gender from participation in honor-dueling (who cannot voice their objections to the constraint within the system that defines honor). Reform advocates (reformers, legal scholars, physicians documenting duel casualties) are excluded from setting the honor standard, though their voices do appear in competing discourses during the decline period.
% DISAPPEARANCE_RATIONALE: If dueling disappeared entirely overnight, the immediate world effect depends on which reading is true: under the drop reading, societies would rearrange their honor-satisfaction mechanisms (legal redress, formalized competition, alternative status displays), honor-bearing elite would lose a source of legitimacy and status differentiation, and the identity-fused duelists would face acute identity crisis. The external-cost apparatus would become unnecessary. Under the contraction reading, honor itself would have already rearranged (redefined away from violence), so dueling's disappearance would be an epilogue to a already-completed principle shift. The contest is real: the answer depends on whether the legitimacy claim persists or has been reframed.
% FOUNDING_PROBLEM: In hierarchical societies with dispersed power (pre-state or weak-state), the honor-duel solves the problem of how high-status individuals settle disputes and validate claims to authority without centralized legal recourse. It provides a recognized, rule-bound mechanism for private conflict resolution that does not require state intervention and validates status through demonstrated courage and skill. For the honor-bearing elite, it is a coordination mechanism that substitutes for state courts.
% FOUNDING_PROBLEM_CORROBORATION: The honor-bearing elite and military/diplomatic traditions attest the founding problem is live: honor requires readiness to defend it through combat, and dueling provides that mechanism. Legal reformers, state apparatus, and insurance industries attest the founding problem is solved and the constraint has become vestigial: alternative conflict-resolution mechanisms (courts, arbitration, professional credentials, formalized competition) now adequately validate status and resolve disputes without violence. The contest cannot be resolved by appeal to a single authoritative voice; it is the substance of the kernel disagreement.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at the interval endpoint (0.62) reflects a tangled_rope structure: honor-bearing elite benefit from the constraint's existence (the legitimate claim to duel provides status differentiation and conflict-resolution mechanism they control), while duelists pay the external costs (legal jeopardy, liability exposure, deaths). The constraint requires active enforcement: state legal apparatus must penalize dueling, insurers must exclude duel deaths, military or diplomatic honor codes must be enforced against violence. Suppression grows over the interval (0.35→0.58) as the external-cost apparatus hardens: statutes criminalizing dueling, liability doctrine, insurance exclusions, military codes. Theater ratio rises sharply (0.22→0.51) as the constraint's persistence becomes increasingly detached from actual practice—by the late interval, the legitimacy claim (honor-requirement-to-duel) is maintained in literary, rhetorical, and conceptual forms while actual dueling becomes performative, rare, or extinct. Accessibility collapse is low (0.48) because alternatives to honor-satisfaction emerge continuously throughout the interval: legislation offers formal justice, insurance socializes liability, military codes substitute for personal blood-honor. Resistance is high (0.71) because dueling is actively defended by status-dependent groups (aristocracy, military, certain professions) even as the broader population and state apparatus suppress it.
 *
 * PERSPECTIVAL GAP:
 *   From the honor-bearing elite's seat, the constraint is a still-legitimate coordination mechanism for managing status and conflict resolution—practice is low but the principle remains. From the duelist's seat (especially if identity-fused to honor), the same constraint is increasingly irrational economic extraction dressed in legitimacy language, with exit unavailable due to identity lock. From the state's seat, the constraint is an atrophying holdout against the state monopoly on violence—enforcement activity increases precisely because the constraint persists despite low practice. The engine computes these divergences from the power/exit/beneficiary structural data; the reading does not adjudicate which seat is 'right'.
 *
 * DIRECTIONALITY LOGIC:
 *   The honor-bearing elite (agenda_setter, beneficiary) establish and maintain the legitimacy claim—they set the cultural/legal standard for what honor requires and how it is discharged. Duelists (payer, identity_locked) bear the external costs: legal jeopardy, bodily risk, insurance loss. The state apparatus (agenda_setter, enforcer) works to suppress the practice through law and policy. Societies writ large (payer) bear the externality cost of violence that the honor-mechanism generates. The directionality for duelists is pushed toward the target end (d near 1.0) by identity_locked exit: a duelist's social position, professional standing, and self-conception may be fused with the honor-bearing identity such that exit is not available even when costs rise. The elite's directionality is beneficiary-end (d near 0.0) because they collect legitimacy and status from the arrangement without personally bearing the rising external costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows mandatrophy symptoms: the founding problem (honor-satisfaction, status-differentiation, private conflict resolution among elite groups) persists in the institutional sense—honor remains a social good—but the mechanism (dueling) has become practically obsolete due to external constraint. The constraint persists by inertia and legitimacy claim rather than by active utility. However, the tangled_rope classification is correct because the constraint still extracts (status, legitimacy, identity-value for the elite) from those who bear the cost (duelists, states), and that extraction requires active enforcement (law, policy, insurance exclusion). A piton classification would fit better if the beneficiaries themselves no longer benefited substantially—but the honor-bearing elite continue to benefit from the legitimacy claim's existence even as practice declines. The rising theater_ratio (0.22→0.51) indicates increasing performative maintenance, suggesting drift toward piton, but the interval endpoint shows the constraint still classifies as tangled_rope because extraction is still real, even if practice is rare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_legitimacy_vs_frequency,
    'Under the drop reading, does dueling remain thinkable-and-legitimate as an honor mechanism even as participation becomes practically rare, or does low frequency itself gradually erode the structural legitimacy of the claim?',
    'Historical archive analysis: examine written defenses and legal challenges to dueling in periods of high vs. low practice frequency; assess whether advocates appeal to honor-mechanism principle or whether principle-language disappears as practice disappears.',
    'If legitimacy persists independent of frequency, the drop reading holds and dueling is a structurally legitimate but economically suppressed coordination mechanism (tangled_rope at axis). If legitimacy erodes with frequency, the constraint drifts toward the contraction reading (where the principle itself is redefined out of play) or composite reading (both mechanisms operated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_legitimacy_vs_frequency, empirical, 'Whether structural legitimacy of honor-violence mechanism persists when practice frequency drops.').

omega_variable(
    external_cost_origin_and_applicability,
    'Are the external costs (legal liability, state monopoly on violence, social insurance loss) exogenous impositions that constrain a still-legitimate practice, or are they endogenous re-framings of what honor itself is supposed to accomplish?',
    'Genealogical analysis of how honor-bearing groups narrate the cost boundary: do they claim the costs are obstacles imposed-from-outside (exogenous suppression), or do they claim the honor-mechanism itself should have never relied on violence (endogenous redefinition)? Track the rhetorical shift across the decline period.',
    'If costs are narrated as exogenous, the drop reading holds: legitimacy survives, practice drops due to external constraint (pure tangled_rope). If costs are narrated as endogenous revisions to what honor means, the constraint drifts toward contraction reading (the principle itself has changed). Mixed narratives across different honor-bearing groups support the composite reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_cost_origin_and_applicability, conceptual, 'Whether suppression of dueling arises from external constraint or internal redefinition of honor.').

omega_variable(
    dueling_cultural_availability_persistence,
    'In contemporary European/American contexts where dueling is legally prohibited and socially stigmatized, is dueling still conceptually available as a thinkable honor-mechanism (present in narrative, understood as an option even if not exercised), or has it become culturally opaque—no longer what ''honor'' means?',
    'Contemporary ethnographic or literary analysis: in modern honor-bearing subcultures (academic, military, gang-affiliated, professional), is dueling referenced, understood, or available as a conceptual option for honor satisfaction, even if not exercised? Or has the concept been entirely replaced by alternative honor mechanisms?',
    'Persistent conceptual availability (even if practically rare) supports the drop reading. Complete cultural opacity (dueling no longer what honor means) supports the contraction reading. Scattered availability across some subcultures but not others supports the composite reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dueling_cultural_availability_persistence, empirical, 'Whether dueling persists as thinkable honor-mechanism in contemporary cultures.').

omega_variable(
    sibling_reading_framing_difference,
    'This constraint is one reading of the kernel ''honor_violence_legitimacy''; the competing readings (contraction, composite) make different claims about whether the kernel changed. Are the readings genuinely about different structural mechanisms, or do they represent different narratives imposed on the same facts?',
    'Construct counterfactuals for each reading: what would have to be true for each reading to be correct? For drop: legitimacy persists, practice drops due to costs. For contraction: the honor-principle itself is redefined, making violence-dueling unthinkable. For composite: both operate simultaneously. Which counterfactual matches the historical evidence? Are the readings falsifiable, or do they post-hoc fit any historical outcome?',
    'If readings are genuinely falsifiable (different mechanisms that exclude each other), the kernel framing is sound and the engine''s reading_relations computation is meaningful. If they are post-hoc narratives that fit any outcome, the kernel framing itself is under-determined and the constraint should be decomposed or reframed at the story level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_difference, conceptual, 'Whether the three readings of honor_violence_legitimacy kernel are genuinely distinct mechanisms or different post-hoc narratives of the same trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hono_tr_t8, honor_violence_legitimacy__drop_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(hono_tr_t16, honor_violence_legitimacy__drop_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(hono_tr_t24, honor_violence_legitimacy__drop_reading, theater_ratio, 24, 0.49).
narrative_ontology:measurement(hono_tr_t32, honor_violence_legitimacy__drop_reading, theater_ratio, 32, 0.51).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hono_be_t8, honor_violence_legitimacy__drop_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(hono_be_t16, honor_violence_legitimacy__drop_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(hono_be_t24, honor_violence_legitimacy__drop_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(hono_be_t32, honor_violence_legitimacy__drop_reading, base_extractiveness, 32, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hono_su_t8, honor_violence_legitimacy__drop_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(hono_su_t16, honor_violence_legitimacy__drop_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(hono_su_t24, honor_violence_legitimacy__drop_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(hono_su_t32, honor_violence_legitimacy__drop_reading, suppression_requirement, 32, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story constraint family instantiating different readings of the contested kernel 'honor_violence_legitimacy'. The drop reading models dueling as a structurally legitimate practice suppressed by external costs; the contraction reading models dueling as becoming structurally unthinkable through redefinition of honor; the composite reading models both mechanisms operating in parallel. Each reading has its own ε, beneficiary/victim structure, and classification. All three share the kernel contest and are linked via network.affects_constraints. The three stories enable analysis of how a commitment system (honor) can decay through different mechanisms—cost-suppression vs. principle-redefinition vs. both—and whether the three readings are empirically distinguishable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
