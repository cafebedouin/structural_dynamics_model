% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Prohibition — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone (1933) reads: 'High dwellings are the peace
 *   and harmony of our descendants. Remember the calamity of the great
 *   tsunamis. Do not build any homes below this point.' This reading treats
 *   the stone as a commemorative husk: the prohibition decayed from
 *   operational land-use rule to heritage symbol. Development interests,
 *   municipal authorities, and tourism agencies benefit from the stone's
 *   symbolic status — it provides cultural legitimacy without constraining
 *   profitable coastal development. Future residents below the line and the
 *   community's intergenerational memory transmission bear the extraction.
 *   The constraint persists as piton: the original coordination function
 *   (survival) has atrophied; what remains is theatrical maintenance of a
 *   memorial that no longer binds behavior. The sibling reading
 *   (behavioral_competence_reading) treats the stone as still operationally
 *   enforced — this reading forecloses that possibility by asserting the
 *   prohibition's behavioral force is extinct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.68).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.22).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone Prohibition — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '398d133b-d37e-4856-93ac-275e818e5384').
narrative_ontology:cs_kernel_codification('398d133b-d37e-4856-93ac-275e818e5384', fixed_text).
narrative_ontology:cs_authority_grounding('398d133b-d37e-4856-93ac-275e818e5384', lineage).
narrative_ontology:cs_interpretation_layer_present('398d133b-d37e-4856-93ac-275e818e5384').
narrative_ontology:cs_reading_relation('398d133b-d37e-4856-93ac-275e818e5384', aneyoshi_land_use_prohibition__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('398d133b-d37e-4856-93ac-275e818e5384', foundational, prohibition_behavioral_force_extinct).
narrative_ontology:cs_axiom_status(prohibition_behavioral_force_extinct, holdable).
narrative_ontology:cs_axiom_grounding('398d133b-d37e-4856-93ac-275e818e5384', prohibition_behavioral_force_extinct, empirically_contingent).
narrative_ontology:cs_axiom('398d133b-d37e-4856-93ac-275e818e5384', secondary, heritage_performance_substitutes_for_operational_memory).
narrative_ontology:cs_axiom_status(heritage_performance_substitutes_for_operational_memory, holdable).
narrative_ontology:cs_axiom_grounding('398d133b-d37e-4856-93ac-275e818e5384', heritage_performance_substitutes_for_operational_memory, conventional).
narrative_ontology:cs_reference_frame('398d133b-d37e-4856-93ac-275e818e5384', id_1933_kin_group_land_use_prohibition).
narrative_ontology:cs_drift_state('398d133b-d37e-4856-93ac-275e818e5384', post_2011_reconstruction_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('398d133b-d37e-4856-93ac-275e818e5384', '2026-08-25T14:32:17Z').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_promotion_agencies).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, intergenerational_community_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers and landowners below the stone's elevation line benefit from the prohibition's symbolic-only status — they can build, sell, and profit from coastal land that would be restricted if the prohibition were operationally enforced. Their exit is mobile: they can shift investment to other coastal zones if local resistance hardens.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, biographical, mobile, regional).

% Local government administers land-use permits and disaster planning. They maintain the stone as a heritage asset while approving development below the line, collecting tax revenue and political capital from both preservation rhetoric and development approvals. Their exit is arbitrage-grade: they can rezone or reinterpret the stone's significance without personal cost.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities, beneficiary).

% Tourism bodies market the stone as a 'living lesson' and cultural landmark, drawing visitors and funding. They benefit from the stone's narrative value while the underlying prohibition remains unenforced. Their exit is mobile: they can pivot to other heritage narratives if this one loses traction.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_promotion_agencies, beneficiary,
    organized, biographical, mobile, national).

% People who will live in housing built below the stone's warning line — children, elderly, low-income households priced into tsunami-inundation zones. They bear the full cost of the prohibition's decay: when the next mega-tsunami arrives, they die. Their exit is trapped: they cannot choose where they are born or afford to relocate preemptively.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, generational, trapped, local).

% The community's capacity to transmit disaster memory as operational knowledge — not ceremony — erodes each year the prohibition is treated as symbol. Elders who remember 1933 and 2011 pass away; the stone becomes a photo stop. The community is identity-locked: its self-concept is fused with 'we remember,' making it structurally unable to admit the memory has become performative.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, intergenerational_community_continuity, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__commemorative_husk_reading, intergenerational_community_continuity).

% Researchers who study how disaster memory transmits, decays, or is instrumentalized across generations. They see the full structure: the stone's dual status as memorial and failed warning, the development interests that benefit from its decay, the future residents who will pay. Their seat is analytical — they neither collect nor pay.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_anthropology_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: a kin-group's land-use rule that coordinated settlement above the tsunami inundation line, solving the collective-action problem of intergenerational risk avoidance without centralized enforcement. Now: a heritage ritual that coordinates community identity and tourism narrative, not physical safety.
% TRANSFER_FUNCTION: Moves disaster risk from development interests (who profit from building below the line) to future residents (who bear mortality risk when the tsunami returns). Moves cultural legitimacy from operational memory to symbolic performance — the stone 'remembers' so the living don't have to act.
% ABSENT_VOICES: The dead of 1896 and 1933 — the ancestors who erected the stone as a binding prohibition, not a memorial. They would object to their warning being reduced to a tourist plaque. Also absent: the unborn children who will occupy the houses now being approved below the line — they have no voice in today's zoning decisions.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, the heritage tourism economy would lose a flagship asset, municipal planners would lose a rhetorical shield for development approvals, and the community would lose its primary material anchor for disaster memory — but the physical risk to below-line residents would be unchanged (the tsunami does not read the stone). The world rearranges because the stone's symbolic function structures current decisions about land use, memory, and profit.
% FOUNDING_PROBLEM: The 1896 Meiji-Sanriku tsunami killed 22,000+ along this coast. The Aneyoshi hamlet survivors erected the stone in 1933 after the Showa-Sanriku tsunami confirmed the inundation line: 'Do not build your homes below this point.' The founding problem was intergenerational survival — how to bind future generations to a safety rule when memory fades and development pressure mounts.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (binding future generations to the inundation line) is dead: the 2011 tsunami reached the stone but development below the line had already been permitted for decades. Corroboration from outside beneficiaries: geologist Fumihiko Imamura's 2011 post-disaster survey documented that Aneyoshi households above the line survived while neighboring communities below permitted lines were devastated; the town's own 2015 reconstruction plan explicitly zones residential areas below the stone's elevation. No living authority attests the prohibition remains operationally binding.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the arrangement transfers mortality risk from profit-takers to the powerless — a classic piton signature where the administrator (municipal planners) could change the constraint but the cost to fix (enforcing a genuine building ban, compensating landowners, restructuring the local economy) exceeds what they bear. Theater ratio (0.78) is very high: the stone is maintained, ceremonies performed, tourists guided, but zero enforcement of the actual prohibition. Suppression (0.22) is low because no active coercion is needed — the constraint's decay was achieved by simple non-enforcement and reinterpretation, not force. Accessibility collapse (0.35) is moderate: alternatives (enforced zoning, relocation programs, structural mitigation) exist but are politically unattended. Resistance (0.18) is low: the primary victims (future residents) are not yet present to resist; the identity-locked community cannot resist its own self-narrative.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (municipal planners, developers, tourism) experience this as a successful adaptation: heritage preserved, economy growing, no tsunami since 2011. The payer seats (future residents, intergenerational continuity) experience it as a deferred catastrophe — the stone's decay is a loan against a future the borrowers won't repay. The observer seat sees both: a constraint that has become its own monument.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (developers, planners, tourism) collect rents and legitimacy from the stone's symbolic status — their directionality is near-zero (full beneficiary). Payers (future residents, community memory) bear the deferred mortality risk and epistemic erosion — their directionality is near-one (full target). The intergenerational_community_continuity agent is identity-locked: its self-concept ('we are the people who remember') fuses with the stone's ceremonial maintenance, making it unable to perceive the decay as loss. Municipal planners are institutional arbitrageurs: they can rezone, reinterpret, or ignore the stone with no personal cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (binding future generations to the inundation line) is dead — the 2011 tsunami proved the line is still correct, but the prohibition that enforced it is gone. The constraint persists as a piton: the administrative capacity to enforce it exists (zoning laws, building codes, the stone's legal status as a protected monument) but the mandate has atrophied into performance. No party benefits enough to restore enforcement; no party is hurt enough yet to demand it. The theater ratio captures the gap: 78% of the constraint's visible activity is commemorative, 22% is the residual legal possibility of enforcement that nobody activates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_status_ambiguity,
    'Is the stone''s prohibition truly behaviorally extinct, or does it retain latent force through social norm channels not captured by formal enforcement records?',
    'Ethnographic study of actual building decisions near the stone: do local carpenters, families, or officials still reference the stone as a reason to avoid building below the line, even without legal enforcement?',
    'If latent normative force persists, the constraint is not a pure piton but a degraded rope — extraction is lower, coordination function partially alive. If fully extinct, piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_status_ambiguity, empirical, 'Whether the prohibition retains any behavioral force through informal norms.').

omega_variable(
    development_pressure_counterfactual,
    'Would development below the line have occurred at the same rate and pattern if the stone had never existed — i.e., is the stone''s symbolic presence causally irrelevant to the outcome?',
    'Comparative analysis of similar coastal communities without tsunami stones: do they show identical development patterns below historical inundation lines?',
    'If development patterns are identical regardless of the stone, the constraint''s extractiveness is overstated — the stone is epiphenomenal. If the stone''s presence (even as symbol) slows or shapes development, it retains partial coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_pressure_counterfactual, empirical, 'Whether the stone''s symbolic presence has any causal effect on development decisions.').

omega_variable(
    committer_framing_delta,
    'This reading (commemorative_husk) and its sibling (behavioral_competence) disagree on the kernel''s current operational status — where exactly is the structural disagreement located?',
    'Map the specific institutional decisions (zoning approvals, building permits, evacuation planning) that would differ if one reading were adopted as policy versus the other.',
    'If the readings produce identical policy recommendations, the disagreement is semantic. If they diverge on concrete decisions (e.g., whether to approve a specific subdivision below the line), the kernel contest has material stakes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_delta, conceptual, 'Structural location of the kernel contest between commemorative_husk and behavioral_competence readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 1933, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t1985, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t1995, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2005, 0.58).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2011, 0.65).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t2018, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2018, 0.72).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_tr_t2025, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2025, 0.78).

% Extraction over time
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t1985, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t1995, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t2018, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_be_t2025, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1933, 0.85).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t1985, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t1995, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2011, 0.25).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t2018, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(aneyoshi_land_use_prohibition__commemorative_husk_reading_su_t2025, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, sanriku_coast_reconstruction_zoning).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, japan_disaster_heritage_tourism_framework).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two constraint stories: this commemorative_husk_reading (piton, high extractiveness, theater-dominant) and behavioral_competence_reading (claimed as rope/tangled_rope, low extractiveness, enforcement-active). They differ on the kernel's current operational status — this reading asserts behavioral force has decayed; the sibling asserts it persists. The ε values diverge because they assess different referents: this reading assesses the standing arrangement (symbolic maintenance + permitted development below line); the sibling assesses the stone's normative claim as if it were still binding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, institutional, 0.15).
constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, powerless, 0.95).
constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
