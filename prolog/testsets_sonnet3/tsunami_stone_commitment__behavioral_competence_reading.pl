% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone as Live Behavioral Norm (Competence Reading)
 *   domain: disaster_anthropology/institutional_memory
 *
 * SUMMARY:
 *   Along tsunami-prone coastlines, stone markers inscribed with warnings
 *   such as 'do not build your homes below this point' were erected after
 *   historical tsunamis, in some cases centuries earlier. In the
 *   behavioral_competence_reading, these markers were not inert monuments but
 *   active anchors for a living oral tradition: elders retold the story of
 *   the marker's origin, reinforced the settlement norm at community
 *   gatherings, and treated the stone as a continuing behavioral instruction
 *   rather than a historical curiosity. When the 2011 tsunami struck,
 *   communities where this transmission had remained active saw settlement
 *   patterns that spared them from the worst damage, which this reading
 *   interprets as evidence the mechanism was functioning as designed, not
 *   merely coincidentally aligned with survival.
 *
 * KEY AGENTS:
 *   - coastal_village_residents: primary beneficiaries, moderate power, constrained exit — live under the norm and receive its protection
 *   - local_elders_and_transmitters: agenda-setters who actively administer transmission through ceremony and retelling
 *   - descendant_households: intergenerational inheritors of both the land-use benefit and the narrative obligation
 *   - younger_generation_migrants: excluded voices, physically absent from the transmission process
 *   - post_2011_disaster_researchers: analytical observers documenting the correlation between active transmission and survival outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone as Live Behavioral Norm (Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d').
narrative_ontology:cs_kernel_codification('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', fixed_text).
narrative_ontology:cs_authority_grounding('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', practice).
narrative_ontology:cs_interpretation_layer_present('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d').
narrative_ontology:cs_reading_relation('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', foundational, oral_transmission_preserved_causal_efficacy).
narrative_ontology:cs_axiom_status(oral_transmission_preserved_causal_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', oral_transmission_preserved_causal_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', secondary, inscribed_marker_functions_as_live_instruction_not_relic).
narrative_ontology:cs_axiom_status(inscribed_marker_functions_as_live_instruction_not_relic, holdable).
narrative_ontology:cs_axiom_grounding('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', inscribed_marker_functions_as_live_instruction_not_relic, conventional).
narrative_ontology:cs_reference_frame('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', post_disaster_founding_inscription_intact_transmission).
narrative_ontology:cs_drift_state('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', pre_2011_contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ee9c68f-ed6f-4bc1-9a8e-bd8b3fd23d0d', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, descendant_households).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, local_elders_and_transmitters).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, intergenerational_oral_transmission_efficacy).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, inscribed_landmark_as_behavioral_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live below the stone-marked line and are the direct beneficiaries of the settlement pattern it encodes: build homes above the marker, treat land below it as unsuitable for permanent dwelling. Their exit from the coastline is economically constrained (fishing livelihoods, land ties), but the constraint itself does not trap them into anything costly — it simply routes settlement upward, which they have internalized as ordinary practice rather than imposed rule.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents, beneficiary,
    moderate, biographical, constrained, local).

% Inherit both the land above the marker and the narrative obligation to retell why the marker matters. They benefit materially (survival advantage demonstrated in 2011) and socially (standing as keepers of ancestral warning). Their exit option is limited by the same economic geography as current residents, but they are not extracted from — the transmission duty costs storytelling time, not resources.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, descendant_households, beneficiary,
    moderate, generational, constrained, local).

% Actively narrate the stone's meaning at ceremonies, festivals, and household storytelling, reinforcing the do-not-build-below-this-line norm across generations. Their identity as community memory-keepers is bound up with the practice continuing to be taken seriously; if the norm dies, part of their social role dies with it. They administer the norm through repetition and social reinforcement rather than formal sanction.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, local_elders_and_transmitters, agenda_setter,
    moderate, generational, identity_locked, local).

% Have left for urban employment and were, in some documented cases, less attentive to the stone's warning or unfamiliar with its full narrative context. Their skepticism or indifference toward the inscription is rarely voiced in village deliberations about land use, since they are physically absent from the community that maintains the practice.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, younger_generation_migrants, excluded,
    powerless, biographical, mobile, national).

% Documented that villages which retained active oral transmission of the stone's warning suffered zero or minimal tsunami casualties in 2011, compared to nearby areas that had let the practice lapse or had newer settlements below the marked line. They assess the constraint from outside, without stake in its continuation.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, post_2011_disaster_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors a shared, geographically specific rule — do not build permanent dwellings below this stone — through a durable physical marker plus active oral transmission, solving the intergenerational problem of preserving low-frequency, high-consequence disaster knowledge across timescales longer than living memory.
% TRANSFER_FUNCTION: Moves nothing extractive between parties; it transfers a behavioral disposition (settlement restraint) and a narrative obligation (retelling) from each generation to the next, at the cost of forgone flatland-adjacent building convenience.
% ABSENT_VOICES: Younger residents who migrated to cities for work are largely absent from the village-level social process that keeps the narrative alive; their reduced exposure to repeated retellings is one channel by which the norm can weaken across generations, though in the stabilized-competence reading this had not yet eroded the core function as of 2011.
% DISAPPEARANCE_RATIONALE: If the stone and its associated oral tradition vanished overnight, the settlement-siting norm would lose its anchor: without the marker and its retelling, subsequent generations would lack a low-cost, memorable reference point for where past tsunamis reached, and land below the marked line would likely be resettled over a generation or two, as happened in villages where equivalent markers were forgotten or ignored.
% FOUNDING_PROBLEM: Communities needed to preserve a specific, actionable warning — this is how far past tsunamis reached, do not build permanent housing below this point — across a time horizon (multiple generations) far longer than any individual's living memory of the original catastrophic event.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 disaster researchers and journalists documented, from outside the villages themselves, that communities retaining the practice had measurably lower casualty rates than comparable communities without it — this is external corroboration that the founding problem (transmitting actionable disaster knowledge across generations) remained live and the mechanism remained functionally effective at time of test, not merely a claim made by the transmitting elders themselves.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near zero (0.04) because no party is identified who captures value at another's expense through this arrangement — the norm costs forgone coastal-adjacent land use, distributed across the whole community that also receives the safety benefit. Suppression is low (0.12): the norm operates through social reinforcement and narrative repetition, not coercive sanction, and residents who ignored it faced social rather than punitive consequences. Theater ratio is low and rises only slightly (0.05 to 0.08) reflecting mild ceremonial elaboration over time without displacing the functional core. Accessibility collapse is moderate (0.35) — alternative land-use choices were not eliminated, merely socially discouraged, which is consistent with genuine but non-coercive coordination rather than a mountain-grade natural constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the elders' agenda-setting seat, the practice is vindicated tradition under active maintenance. From the analytical observer seat (disaster researchers), the practice is a testable hypothesis about transmission fidelity, confirmed post hoc by differential casualty outcomes. From the excluded younger-generation-migrant seat, the practice may register as background cultural noise they were not present to fully absorb — a structural weak point this reading acknowledges but does not treat as having yet compromised the core function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (residents, descendant households, elders) sit near the low end of directionality because the arrangement subsidizes their safety at the cost of their own forgone convenience — there is no asymmetric extraction, only a shared cost-shared benefit structure. No victim group is declared because this reading holds that the mechanism functioned as intended: there is no identifiable party who was made worse off by the norm's operation. This is what disambiguates the reading from a snare or tangled_rope — the coordination story is not cover for extraction here, it is the whole structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting actionable disaster knowledge past living memory) remained live through 2011 under this reading, corroborated by researchers external to the transmitting community — this blocks a mandatrophy read (a mandate that persists after its function died) because the function did not die; the 2011 event is read as confirming continued live operation rather than exposing an empty ritual. Classifying this as rope rather than piton depends entirely on whether the competence claim (this reading) or the husk claim (the sibling) is correct — the two readings produce different classifications from the same physical marker, which is exactly the kind of natural-language ambiguity the kernel/reading structure exists to disambiguate rather than average over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_fidelity_vs_coincidence,
    'Was the 2011 survival differential actually caused by active, faithfully transmitted behavioral compliance with the stone''s warning, or by confounding factors (topography, unrelated settlement history, later building codes) that merely correlate with which villages still told the stone''s story?',
    'Fine-grained village-level ethnographic and settlement-history comparison controlling for topography, economic development timing, and independent land-use regulation, isolating the marginal effect of active oral transmission specifically.',
    'If the correlation is confounded, this reading''s core claim (live behavioral competence) collapses toward the commemorative_husk_reading — the marker''s continued presence would then be coincidental to survival rather than causal, and this constraint''s very low ε would no longer be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_fidelity_vs_coincidence, empirical, 'Whether the 2011 casualty differential is caused by active transmission or by confounded settlement factors.').

omega_variable(
    which_reading_is_this_kernel,
    'Is the tsunami_stone_commitment kernel best read as a case of intact, functioning behavioral transmission (this reading), a decayed symbolic husk with coincidental compliance (commemorative_husk_reading), or is the entire question better framed around what the 2011 event itself proved rather than the marker''s ongoing status (catastrophe_validation_axis)?',
    'Cross-village ethnographic fieldwork conducted before 2011 (if such records exist) comparing documented transmission practices against post-event outcomes, rather than inferring transmission vitality retrospectively from survival alone.',
    'Selecting this reading assigns near-zero extraction and a rope classification; selecting the husk reading would instead classify the same physical marker as a piton with performative rather than functional force. The two readings cannot be averaged — they describe structurally different constraints sharing one physical artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_this_kernel, conceptual, 'Which of the three kernel readings correctly characterizes the marker''s actual causal status, given that pre-2011 ethnographic baselines are sparse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.04).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.04).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the tsunami_stone_commitment kernel. behavioral_competence_reading claims the marker retained live, causally effective behavioral force through active transmission (very low ε, rope classification). commemorative_husk_reading claims the opposite structural fact about the same object — that transmission had decayed and 2011 compliance was substantially coincidental (a piton-flavored reading with different ε and beneficiary structure). catastrophe_validation_axis treats the 2011 event as the decisive test rather than asserting the marker's ongoing status. All three share the kernel but are authored as separate constraints per the ε-invariance principle, since forcing one story to average over these claims would produce an unstable, observer-relative ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
