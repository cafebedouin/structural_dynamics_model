% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone — Commemorative Husk Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, erected after the 1933 Showa Sanriku tsunami,
 *   originally carried a directive: 'Do not build your homes below this
 *   line.' For decades the village obeyed; Aneyoshi sat above the line and
 *   survived the 2011 tsunami with zero casualties. But the surrounding
 *   coastal zone — other municipalities, later development — did not obey.
 *   This reading holds that the stone's directive lost behavioral force
 *   during the inter-catastrophe period (1933–2011), becoming a commemorative
 *   husk: the stone remains, the ceremonies continue, but the no-build zone
 *   it mandated has been overwritten by development. The constraint now
 *   operates as extraction — development interests gain from the decay of the
 *   directive's force, while future residents pay the mortality cost. The
 *   claimed type is piton: a former coordination mechanism (the directive)
 *   whose function has atrophied, persisting through theatrical
 *   commemoration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.72).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.68).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Tsunami Stone — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '14d41f26-9481-419f-9862-03bba3814643').
narrative_ontology:cs_kernel_codification('14d41f26-9481-419f-9862-03bba3814643', fixed_text).
narrative_ontology:cs_authority_grounding('14d41f26-9481-419f-9862-03bba3814643', lineage).
narrative_ontology:cs_interpretation_layer_present('14d41f26-9481-419f-9862-03bba3814643').
narrative_ontology:cs_reading_relation('14d41f26-9481-419f-9862-03bba3814643', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('14d41f26-9481-419f-9862-03bba3814643', foundational, directive_force_requires_continuous_validation).
narrative_ontology:cs_axiom_status(directive_force_requires_continuous_validation, holdable).
narrative_ontology:cs_axiom_grounding('14d41f26-9481-419f-9862-03bba3814643', directive_force_requires_continuous_validation, empirically_contingent).
narrative_ontology:cs_axiom('14d41f26-9481-419f-9862-03bba3814643', foundational, commemoration_displaces_behavioral_authority).
narrative_ontology:cs_axiom_status(commemoration_displaces_behavioral_authority, holdable).
narrative_ontology:cs_axiom_grounding('14d41f26-9481-419f-9862-03bba3814643', commemoration_displaces_behavioral_authority, conventional).
narrative_ontology:cs_reference_frame('14d41f26-9481-419f-9862-03bba3814643', showa_sanriku_survivor_injunction).
narrative_ontology:cs_drift_state('14d41f26-9481-419f-9862-03bba3814643', post_2011_heritage_designation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('14d41f26-9481-419f-9862-03bba3814643', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, municipal_tax_base_officials).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, tourism_promotion_agencies).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_preservationists).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, ritual_commemoration_suffices_for_preparedness).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, economic_development_justifies_memory_decay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real estate developers, construction firms, and land speculators who profit from building in the inundation zone. The stone's decay from directive to monument removes the social stigma and regulatory friction that would otherwise attach to coastal development. They do not maintain the stone; they benefit from its silence.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, biographical, mobile, regional).

% Local government officials whose budgets depend on property tax revenue from coastal development. They administer building permits and land-use designations. The stone's commemorative framing lets them approve development while performing reverence — the ritual substitutes for the directive's behavioral force.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_tax_base_officials, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, municipal_tax_base_officials, agenda_setter).

% Regional tourism bodies that market the stone as a heritage site — 'the tsunami stone that saved Aneyoshi.' The commemorative reading is their product: it draws visitors, generates narrative content, and requires no behavioral restriction on visitors or developers. They maintain the stone's visibility, not its authority.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tourism_promotion_agencies, beneficiary,
    organized, biographical, mobile, regional).

% People who will live in the inundation zone because the stone no longer stops development. They bear the mortality risk of the next tsunami without the protective constraint. They have no voice in the stone's current framing — they are not yet born or not yet settled — and no exit from the risk once they are there.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Scholars, activists, and descendants of tsunami survivors who argue the stone's directive should still govern land use. They pay the cost of maintaining the counter-narrative — research, advocacy, community organizing — while being excluded from permit decisions and commemorative ceremonies. Their exit is constrained by professional and moral commitment.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_preservationists, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_preservationists, excluded).

% The few remaining households in Aneyoshi proper who still live above the stone's line. They hold the living memory of the directive but have ceded governance to municipal authorities. Their identity is fused with the stone's original meaning, yet they lack the power to enforce it. They witness the commemorative husk replacing the directive they grew up with.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_village_elders, observer,
    moderate, biographical, identity_locked, local).

% Researchers who study the stone as a case of institutional memory decay. They see the full structure: the directive's loss of behavioral force, the beneficiaries of that loss, the commemorative performance that masks it. They neither collect from nor pay into the constraint; they analyze it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_governance_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated intergenerational tsunami avoidance: 'Do not build your homes below this line.' It solved the collective-action problem of maintaining a no-build zone across generations without continuous enforcement.
% TRANSFER_FUNCTION: The commemorative reading transfers the protective force of the directive from future residents (who would be protected by the no-build zone) to development interests (who profit from building in the zone), mediated by municipal officials who convert protection into tax revenue and tourism agencies who convert memory into marketing.
% ABSENT_VOICES: The future residents who will die in the next tsunami because the stone no longer stops development — they are absent by definition. Also absent: the 1933 and 1896 tsunami dead whose survival injunction the stone carried; their directive has been overwritten by a commemorative narrative they cannot contest.
% DISAPPEARANCE_RATIONALE: If the commemorative framing vanished overnight and the stone's directive regained behavioral force, coastal development in the inundation zone would become socially and regulatory illegitimate. Property values would collapse, municipal budgets would shrink, tourism narratives would fracture, and the protective no-build zone would be reasserted — the world would rearrange around the restored directive.
% FOUNDING_PROBLEM: After the 1896 and 1933 tsunamis killed most of Aneyoshi's population, survivors erected the stone to solve the intergenerational memory problem: how to ensure descendants would not resettle the lethal zone when living memory faded.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — preventing resettlement of the inundation zone — is dead in the sense that the zone has been resettled and developed. Municipal land-use records, building permits issued below the stone's line, and the stone's own rebranding as a heritage site (documented in Iwate Prefecture tourism materials) corroborate this. The behavioral_competence_reading's proponents (some Aneyoshi elders and disaster scholars) contest the 'dead' status, arguing the directive remains morally binding; but the corroborating evidence for behavioral force loss comes from outside the benefiting parties: the developers and officials who act as if the directive has no force.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint now suppresses the economically rational (for developers) coastal development that the directive would forbid, transferring mortality risk to future residents. Suppression (0.68) reflects the active work of commemorative framing — ceremonies, heritage designation, tourism marketing — that displaces the directive's behavioral authority without repealing it. Theater ratio (0.55) is elevated: more than half the constraint's visible activity is commemorative performance rather than protective coordination. Accessibility collapse (0.45) is moderate — alternatives (relocating development, enforcing the directive) exist but are politically suppressed. Resistance (0.35) is low because the primary victims (future residents) are not yet present to resist, and the excluded voices (preservationists) lack enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the stone is a successful heritage asset — it draws tourists, enables development, and performs remembrance. From the payer seats, it is a lethal husk — the directive's decay is the mechanism that exposes future lives to tsunami risk. The agenda-setter seat (municipal officials) experiences the constraint as a manageable tension: they perform reverence while issuing permits. The engine computes these divergences from the structural data; the commemorative reading's claim (piton) captures the atrophy, but the metrics reveal the active extraction the performance conceals.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests, municipal officials, and tourism agencies are beneficiaries: they collect rents, tax revenue, and visitor spending from the zone the directive would protect. Their exit options are mobile or constrained — they can shift projects or jurisdictions. Future coastal residents are payers with trapped exit: they inherit the mortality risk with no say in the framing. Preservationists are payers with constrained exit: they bear advocacy costs but cannot stop development. Aneyoshi elders are observers with identity-locked exit: their self-concept is fused to the stone's original meaning, making disengagement psychologically costly. Scholars are analytical observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone's mandate — intergenerational tsunami avoidance — has outlived its function not because tsunamis ceased (the 2011 event proved the hazard persists) but because the social mechanism that transmitted the directive across generations broke. The commemorative reading resolves the mandatrophy by converting the directive into a ritual object: the mandate is declared fulfilled (the stone stands, the village survives) while the behavioral requirement is quietly dropped. This is not a scaffold with a sunset clause — there was no declared transition. It is a piton: the original coordination function atrophied, and the theatrical maintenance (ceremonies, heritage status) prevents the constraint's honest classification as a failed mountain or an acknowledged snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_decay_timing,
    'When exactly did the stone''s directive lose behavioral force — was it a gradual erosion or a discrete break (e.g., post-1960 coastal development policy, post-2011 heritage designation)?',
    'Municipal land-use records, building permit histories, and oral history interviews with Aneyoshi elders and officials to date the first permitted construction below the stone''s line and the administrative justifications used.',
    'A discrete break tied to a policy decision would support the extraction reading (active suppression); a gradual erosion would support the atrophy reading (piton dynamics). The timing also affects the measurement series'' inflection points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(directive_decay_timing, empirical, 'Timing and mechanism of the directive''s behavioral force loss').

omega_variable(
    commemorative_performance_as_suppression,
    'Is the commemorative framing (ceremonies, heritage status, tourism narrative) actively suppressing the directive''s behavioral authority, or is it merely a symptom of the directive''s prior decay?',
    'Compare municipalities with similar tsunami stones but different commemorative regimes: if active commemoration correlates with faster directive decay, the performance is suppressive; if decay precedes commemoration, it is symptomatic.',
    'If suppressive, the theater_ratio measures an active extraction mechanism (the performance displaces the directive); if symptomatic, the theater_ratio measures a lagging indicator. This changes the constraint''s classification dynamics — active suppression favors snare/tangled_rope; symptomatic atrophy favors piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_performance_as_suppression, conceptual, 'Whether commemoration causes or merely reflects directive decay').

omega_variable(
    kernel_framing_underdetermination,
    'Is the aneyoshi_stone_directive kernel best framed as a land-use constraint, a memory transmission mechanism, or a sacred boundary marker? Different framings yield different structural analyses.',
    'Cross-cultural comparison with other disaster stones (e.g., Hawaiian kapu markers, Indonesian tsunami mosques, Chilean memorial forests) to identify whether the kernel''s primary coordination function is spatial regulation, memory preservation, or sacralization — and whether the commemorative_husk_reading misidentifies the kernel''s true type.',
    'If the kernel is primarily a memory transmission mechanism, its decay into commemoration is functional success (memory preserved in ritual form), not extraction. If primarily a land-use constraint, the commemorative reading correctly identifies extraction. The framing determines which metrics are primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the kernel''s primary coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t1933, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t1960, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t1985, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t2000, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t2011, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2011, 0.51).
narrative_ontology:measurement(aneyoshi_comm_husk_tr_t2024, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(aneyoshi_comm_husk_be_t1933, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_comm_husk_be_t1960, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(aneyoshi_comm_husk_be_t1985, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(aneyoshi_comm_husk_be_t2000, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(aneyoshi_comm_husk_be_t2011, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(aneyoshi_comm_husk_be_t2024, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_comm_husk_su_t1933, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aneyoshi_comm_husk_su_t1960, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(aneyoshi_comm_husk_su_t1985, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(aneyoshi_comm_husk_su_t2000, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(aneyoshi_comm_husk_su_t2011, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement(aneyoshi_comm_husk_su_t2024, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, sanriku_coastal_land_use_regime).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, japan_disaster_heritage_governance).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel decomposes into two readings: behavioral_competence_reading (directive retains binding force, low epsilon, Mountain/Tangled Rope profile) and commemorative_husk_reading (directive lost force, high epsilon, Piton profile). They share the same physical stone and historical origin but instantiate different constraints with different ε, beneficiaries, and temporal trajectories. This reading authors the extraction that the commemorative framing enables; the sibling reading authors the coordination the directive originally provided.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, moderate, 0.75).
constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
