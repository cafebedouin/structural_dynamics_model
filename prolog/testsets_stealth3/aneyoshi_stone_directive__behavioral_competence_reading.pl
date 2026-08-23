% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Directive — Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A granite marker stands in the hamlet of Aneyoshi on the Sanriku coast,
 *   carved after the 1896 Meiji tsunami and re-erected after 1933 with an
 *   instruction: do not build your dwelling below this point; high ground
 *   ensures the peace of descendants. This file instantiates the
 *   behavioral_competence_reading of that directive: for the 78 years between
 *   the 1933 Showa tsunami and the 2011 Tohoku tsunami, the instruction
 *   remained a live, binding input into household siting — maintained by a
 *   custodian lineage, carried by custom, and never tested by the sea until
 *   March 2011, when the water stopped just short of the stones and the
 *   hamlet above them stood intact. The referent of every metric below is
 *   that standing arrangement — the directive's governance of siting across
 *   the unvalidated gap — assessed by this reading's own lights. KEY AGENTS
 *   (by structural relationship): aneyoshi_village_households — collective
 *   beneficiary (organized/generational/constrained), bears the compliance
 *   premium and receives the survival payoff; village_custodian_lineage —
 *   administrator and secondary beneficiary
 *   (moderate/generational/identity_locked), keeps records, tends stones,
 *   transmits the instruction; coastal_modernization_planners — excluded
 *   voice (institutional/biographical/mobile), objects from outside the
 *   deliberation; disaster_science_community — analytical observer
 *   (analytical/generational/analytical), attests boundary accuracy from
 *   outside the arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Tsunami Stone Directive — Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '8360acd6-e5cb-4d46-ac7c-c59847dcf0a8').
narrative_ontology:cs_kernel_codification('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', fixed_text).
narrative_ontology:cs_authority_grounding('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', lineage).
narrative_ontology:cs_interpretation_layer_present('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8').
narrative_ontology:cs_reading_relation('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', foundational, inscription_carries_binding_authority).
narrative_ontology:cs_axiom_status(inscription_carries_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', inscription_carries_binding_authority, empirically_contingent).
narrative_ontology:cs_axiom('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', secondary, generational_custom_constitutes_enforcement).
narrative_ontology:cs_axiom_status(generational_custom_constitutes_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', generational_custom_constitutes_enforcement, conventional).
narrative_ontology:cs_reference_frame('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', instructive_hazard_boundary).
narrative_ontology:cs_drift_state('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', pre_2011_inter_catastrophe_peak, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8360acd6-e5cb-4d46-ac7c-c59847dcf0a8', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, village_custodian_lineage).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_hazard_memory_efficacy).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, tsunami_stone_boundary_accuracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% About thirty households in a steep ravine opening onto the Sanriku coast. They place their homes on the terraces above the stone markers and work the mountain slopes; the low ground beside the stream stays free of houses. Each family absorbs the small loss of convenient lowland building space, and each receives the same protection the practice buys. Leaving the valley would mean abandoning forestry and farmland tied to the family register, so households stay and build where the stones instruct.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_households, beneficiary,
    organized, generational, constrained, local).

% The headman family keeps the village's flood records, tends the stones, and retells the 1896 and 1933 accounts at gatherings. Custodianship passes down the family; the role is bound up with the family's standing and self-understanding, and setting it aside would break a chain the family regards as its reason for holding the position. The family lives under the same siting practice it administers and draws respect, not money, from keeping it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, village_custodian_lineage, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, village_custodian_lineage, beneficiary).

% Prefectural and national planners of the postwar decades promoted shorefront roads, ports, and industry along the Sanriku coast and treated old stone warnings as obstacles to rational land use. They never took part in the hamlet's deliberations; their objections live in planning documents and rezoning proposals aimed at the coast generally, not at this valley's practice. Their careers and projects sit elsewhere, so they bear no local cost from the practice they criticize.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_modernization_planners, excluded,
    institutional, biographical, mobile, regional).

% Geologists, historians, and ethnographers who reconstructed the 869 Jogan, 1611, 1896, and 1933 inundations and, after 2011, surveyed how close the water came to the stones. They publish the recurrence cadence and the boundary's accuracy from outside the valley; they hold no stake in the practice and can weigh rival explanations freely.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_science_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns household siting with a hazard boundary that recurs on a multi-generational cadence longer than reliable individual memory: the stones carry the run-up line and the instruction across the gap between catastrophes, so each generation builds where survival remains possible without needing to have seen the sea arrive.
% TRANSFER_FUNCTION: Moves nothing material. It transfers information — the hazard boundary and its justification — from the catastrophe-experienced generation to catastrophe-naive ones, and spreads the opportunity cost of the lowland plots across every generation as a shared premium.
% ABSENT_VOICES: Mid-century modernization planners and any household chafing at forgoing lowland plots were never seated in the arrangement's deliberation — the rule predates them and runs on custom rather than assembly. After 2011, neighboring settlements that lost everything for want of such a line speak with great force, but they enter only as testimony, never as parties.
% DISAPPEARANCE_RATIONALE: Had the directive vanished overnight in, say, 1960, the anchor for siting decisions would have been gone while living memory of 1933 was already fading; dwellings would have crept downslope toward the road and stream over the following decades, and by 2011 the hamlet would have stood inside the inundation zone and been destroyed with heavy loss of life. The rearrangement arrives on the sea's schedule, not immediately — which is exactly why the arrangement exists.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami, and again after 1933, survivors needed to keep descendants from rebuilding in the kill zone: the hazard recurs on a multi-decade cadence longer than dependable oral memory, so the boundary and the warning were cut into stone at the reach of the water.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 2011 inundation line itself attested the boundary within meters of the stones; Iwate Prefecture's tsunami-deposit record establishes the recurrence cadence (869 Jogan, 1611, 1896, 1933, 2011); and the disaster-science literature on Sanriku recurrence intervals independently confirms that the founding problem — a repeating hazard outrunning memory — remains live on this coast. No appeal to the village's own testimony is required.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.05 at interval end: the only cost the arrangement imposes is the forgone lowland house plot, borne diffusely by the same households who receive the protection, with no seat collecting from anyone else's compliance. Suppression is 0.05 and is authored as a raw structural property (unscaled by power or scope): there is no enforcement machinery at all — custom pulls, nothing coerces, and a household could physically build below the line at any time. Theater_ratio ends at 0.18: as the founding generation died, stone-tending and retelling grew relative to fresh siting decisions, so the performative share rose — but it stays far below piton territory, and the 2011 outcome shows the function intact. Accessibility_collapse is 0.60: once the instruction and its reason are understood, building below the line stops being a live alternative for villagers, yet the alternative remains physically open in a way gravity's is not — violation is cheap for decades and then total. Resistance is 0.05: no recorded defiance inside the valley; the excluded planners objected from outside, never within the deliberation. CLAIM/METRIC INDEPENDENCE: claimed_type is rope — this reading's structural verdict — while the source manifest hypothesized mountain. The divergence is deliberate data, not an error to reconcile: the directive is a carved human artifact requiring custodial transmission, so emerges_naturally=true cannot honestly be authored; what is mountain-like is the physics the stone encodes, which is a different constraint (see omega encoded_physics_vs_constructed_rule). The manifest's substantive intuition — no party collects from others' compliance — is honored: extraction is near the information_standard floor, and no seat captures. MEASUREMENTS: both series run on one shared grid (t=0,13,26,39,52,65,78) so every metric is authored at every examined time point; endpoints match base_properties. No suppression_requirement series is authored: the enforcement picture is static (custom throughout, no machinery built up or decayed), so the scalar covers it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently despite sitting at nominally similar village scale. From the custodian lineage's position the arrangement is a duty it administers and embodies — its identity is fused with the transmission chain, and it draws standing from the stones; from the ordinary households' position it is a small premium paid for a large shared protection; from the excluded planners' position it is an irrational obstacle to rational land use; from the observers' position it is a natural experiment in trans-generational warning systems. The engine computes these per-seat classifications from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiary (aneyoshi_village_households) derives a low d — the arrangement subsidizes its members' survival at the price of a small premium they impose on themselves, so the seat sits near but not at the full-beneficiary end. The custodian lineage sits lower still as administrator-plus-beneficiary, collecting non-material standing on top of protection. No victim seat exists anywhere in the structure: the arrangement extracts from no one, which is why no directionality_overrides are authored — the beneficiary declaration plus exit profile already yields the correct d for every seat, and the excluded and observer seats fall outside the derivation by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the sea returned in 2011 and will return again, so the mandate has not outlived its function and mandatrophy is not resolved. The classification prevents two opposite errors. First, the husk error: misreading 78 quiet years as functional death — the quiet years are the coordination succeeding, not failing, and a high-theater memorial verdict would only be reachable if the sibling reading's premises held. Second, the mountain error: misreading near-zero extraction as natural law — the stele is built, maintained, and neglectable, so emerges_naturally stays false and the constraint certifies (if at all) as rope, a constructed coordination of unusually low overhead. Rope preserves both distinctions: genuine coordination function, negligible extraction, no suppressed alternatives, no rent-collector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_causal_role,
    'Did the carved directive itself carry binding behavioral force over 1933–2011 siting decisions (this reading), or did terrain, habit, and settlement inertia carry compliance while the stone stood as post-hoc symbol (commemorative_husk_reading)?',
    'Pre-2011 archival tracing: household siting decisions against stone proximity and inscription literacy; custodial records of deliberations; interviews with pre-2011 generations on why they built where they built; comparison with nearby settlements lacking maintained stones.',
    'If the husk reading wins, this constraint recomputes as an inert husk (high theater, atrophied function) and the 2011 survival reattributes to terrain fortune; if this reading holds, the rope classification stands and the stones qualify as functioning trans-generational infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_causal_role, empirical, 'Which reading of the aneyoshi_stone_directive kernel matches the causal record of the inter-catastrophe gap.').

omega_variable(
    encoded_physics_vs_constructed_rule,
    'Is the operative constraint the natural hazard boundary the stone encodes (physics wearing a granite mask) or the constructed social rule ''build above the inscription''?',
    'Decompose per epsilon-invariance: author the hazard boundary alone (emerges naturally, extraction trivially zero) as its own story, and this directive-as-obeyed story separately. If compliance tracked the inscription rather than independently discoverable terrain cues, the constructed rule is the operative constraint.',
    'Mountain certification would require emerges_naturally=true, which a carved, custodially maintained stele cannot honestly carry; rope certification treats the directive as low-cost coordination. This omega marks exactly where the manifest''s mountain hypothesis and this file''s rope claim diverge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encoded_physics_vs_constructed_rule, conceptual, 'Where the constraint sits: in the physics or in the inscription.').

omega_variable(
    unvalidated_authority_decay,
    'How much binding authority survives 78 years without a validating event, and was Aneyoshi''s transmission mechanism inside or outside the decay window?',
    'Cross-community comparison of compliance fidelity against validation-gap length across the Sanriku coast; measure downslope drift in siting per decade since the last inundation.',
    'Fast decay would make the 2011 compliance approach luck and weaken the durability claim toward scaffold-like fragility; slow decay confirms the mechanism as durable infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unvalidated_authority_decay, empirical, 'Decay kinetics of unvalidated directive authority across generations.').

omega_variable(
    compliance_opportunity_cost,
    'What did forgoing lowland dwelling plots actually cost Aneyoshi households across 78 years — was the compliance premium material or negligible?',
    'Land-value and agricultural-yield reconstruction of the excluded lowland strip; counterfactual rent differentials against comparable Sanriku valleys without siting restrictions.',
    'Near-zero cost pushes extraction toward the coordination floor and strengthens a mountain-flavored reading; material cost confirms a real (if small) coordination price supporting the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_opportunity_cost, empirical, 'Magnitude of the compliance premium underwriting the extraction score.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t0, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t13, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 13, 0.07).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t13, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t26, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 26, 0.09).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t26, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t39, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 39, 0.11).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t39, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t52, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 52, 0.13).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t52, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t65, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 65, 0.16).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t65, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.18).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t0, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t13, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 13, 0.03).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t13, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t26, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 26, 0.04).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t26, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t39, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 39, 0.04).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t39, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t52, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 52, 0.04).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t52, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t65, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 65, 0.05).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t65, observed).
narrative_ontology:measurement(aneyoshi_behavioral_reading_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.05).
narrative_ontology:measurement_basis(aneyoshi_behavioral_reading_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: 'the Aneyoshi stone directive' is one persisting commitment read two ways. This file instantiates the behavioral_competence_reading (directive as live binding input into siting; extraction near the coordination floor, custom-carried). The sibling file instantiates the commemorative_husk_reading (same referent, assessed as inert memorial: high theater, atrophied function). Per OQ-26 the extraction values are reading-indexed over a fixed referent — the standing arrangement of 1933–2011 — and differ because the readings assess it by different lights. The readings' core premises are strict negations over the inscription's causal role in the gap, hence the forecloses edge; the empirical contest is carried by omega kernel_reading_causal_role in both files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
