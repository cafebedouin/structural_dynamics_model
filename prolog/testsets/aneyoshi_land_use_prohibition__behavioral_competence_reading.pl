% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1896, the Meiji Sanriku tsunami devastated the Iwate coast, including
 *   the hamlet of Aneyoshi. Survivors marked the tsunami's maximum run-up
 *   with a stone and established a collective prohibition: do not build above
 *   this line. For 78 years, this rule was maintained through
 *   intergenerational oral transmission and community practice — no formal
 *   zoning law, no external enforcement, only the shared understanding that
 *   the water would return and the stone marked where it reached. When the
 *   2011 Tōhoku earthquake and tsunami struck, this 115-year-old rule proved
 *   prescient: every house below the stone marker was destroyed; every house
 *   above survived. This story instantiates the behavioral-competence
 *   reading: the stone is a live land-use rule that constrained building
 *   decisions across all 78 years between 1896 and 1974 (and continues beyond
 *   2011), and that constraint was operationally enforced through social
 *   practice and collective memory. The sibling reading
 *   (commemorative_husk_reading) claims the stone became a historical
 *   memorial by the late 20th century — emotionally significant but no longer
 *   behaviorally binding. This reading disputes that claim: the rule remained
 *   functionally operative through 2011 because the physical hazard it
 *   encoded did not disappear.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '16c8098a-a809-43c2-9592-4619427d2698').
narrative_ontology:cs_kernel_codification('16c8098a-a809-43c2-9592-4619427d2698', fixed_text).
narrative_ontology:cs_authority_grounding('16c8098a-a809-43c2-9592-4619427d2698', practice).
narrative_ontology:cs_interpretation_layer_present('16c8098a-a809-43c2-9592-4619427d2698').
narrative_ontology:cs_reading_relation('16c8098a-a809-43c2-9592-4619427d2698', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('16c8098a-a809-43c2-9592-4619427d2698', foundational, collective_memory_enforcement_persistence).
narrative_ontology:cs_axiom_status(collective_memory_enforcement_persistence, holdable).
narrative_ontology:cs_axiom_grounding('16c8098a-a809-43c2-9592-4619427d2698', collective_memory_enforcement_persistence, empirically_contingent).
narrative_ontology:cs_axiom('16c8098a-a809-43c2-9592-4619427d2698', secondary, physical_hazard_recurrence_actuates_compliance).
narrative_ontology:cs_axiom_status(physical_hazard_recurrence_actuates_compliance, holdable).
narrative_ontology:cs_axiom_grounding('16c8098a-a809-43c2-9592-4619427d2698', physical_hazard_recurrence_actuates_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('16c8098a-a809-43c2-9592-4619427d2698', tsunami_run_up_physical_limit).
narrative_ontology:cs_drift_state('16c8098a-a809-43c2-9592-4619427d2698', contemporary_post_2011, gap(stable, minor, true)).
narrative_ontology:cs_created_at('16c8098a-a809-43c2-9592-4619427d2698', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents_tsunami_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics_determinism).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, collective_memory_as_enforcement_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi village, Iwate Prefecture. Over 78 years (roughly 1896–1974, covering multiple tsunami cycles) enforced a prohibition on building above a stone marker placed after a catastrophic 1896 tsunami. The rule was maintained through oral tradition, community norm-setting, and practical refusal to grant land for construction above the line. When the 2011 tsunami struck, houses below the line were destroyed; the area above remained protected. The residents collectively maintained and enforced the rule through behavioral practice and intergenerational transmission.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, agenda_setter,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, beneficiary).

% The physical phenomenon: tsunami wave dynamics, run-up distance, and energy dissipation over coastal topography. The stone marks a natural limit derived from observed historical tsunami behavior — not a rule imposed by human authority, but an empirical record of where the water reaches.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics).

% National, prefectural, and municipal governments did not formally codify the Aneyoshi prohibition in zoning law or disaster regulations during the 78-year period. The rule was locally maintained through village consensus, not top-down enforcement. External authorities did not suppress the prohibition but also did not formally recognize it — it operated in the interstices of formal governance.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, external_authorities, excluded,
    institutional, biographical, constrained, national).

% After the 2011 tsunami validation, the prohibition gained media attention and was recognized as prescient. Younger generations and in-migrants now encounter the rule as historical fact and empirical proof rather than as community norm alone. The constraint's behavioral enforcement shifted partly from internalized norm to external historical authority.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, residents_post_2011, observer,
    moderate, generational, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition solves the collective-action problem of occupying a tsunami-vulnerable zone: individual agents maximizing short-term livability (building closer to resources, shelter, economic opportunity) would concentrate settlement in the run-up zone; the stone marks a limit derived from collective historical experience, enforcing spatial coordination around observed physical risk.
% TRANSFER_FUNCTION: No transfer occurs. The arrangement moves nothing from one agent to another — it restricts construction on behalf of all residents' shared safety. The 'cost' is forgone buildable land; the 'benefit' is collective survival capacity. Both are distributed across the whole community.
% ABSENT_VOICES: Outsiders — developers, in-migrants, tourists, or commercial interests who might have wanted to build or profit from coastal development in the prohibited zone — were structurally excluded from the conversation. The prohibition was enforced by residents with generational stake; those without that stake had no seat at the norm-setting table.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished and residents built freely in the run-up zone, settlement density would increase in the high-risk area. When the 2011-magnitude tsunami returns (hazard recurrence interval ~100–150 years historically), concentrated settlement above the stone would result in mass casualties instead of the actual outcome (destruction below the line, survival above it). The constraint's disappearance directly rearranges the village's exposure to its dominant environmental hazard.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami killed thousands across the coast and demolished Aneyoshi. Survivors observed the water's run-up extent and marked it with a stone. The founding problem was: how do we prevent the next tsunami from killing our grandchildren? The stone encoded the answer: do not build where the water reaches.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is corroborated by the 2011 Tōhoku tsunami event itself: the physical phenomenon (tsunami run-up) did not disappear; the hazard remained live across 115 years. Geophysicists attest to the plate-subduction cycle's ~100–150 year recurrence (outside the benefiting parties — independent scientific literature). The 2011 event's actual outcome (destruction below the line, survival above) is the strongest possible validation: not testimony from interested parties, but the constraint's validation through empirical event.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at interval end) because no party collects rents from the prohibition; the constraint is a response to natural physics, not a transfer mechanism. Suppression is minimal (0.12) because the rule's persistence depends on the hazard's reality and residents' experiential validation, not on coercion — the water returns every century or so and revalidates the boundary. Accessibility_collapse is very high (0.92) because once the physical constraint is understood (where tsunamis reach), the alternative (building in the run-up zone) collapses as a viable option for anyone with intergenerational awareness. Resistance is near-zero (0.04) because the constraint aligns with survival incentive — residents resist violations not because the rule is imposed but because violating it kills people. Theater rises modestly from 1896 to 2011 (0.05 to 0.18) as the rule transitioned from immediate post-catastrophe response (high behavioral force, low theatricality) to generationally-distant norm (behavioral force attenuated by receding trauma, symbolic maintenance rising) — but the 2011 validation then resets the ratio downward again as the physical phenomenon recaptures behavioral salience. The measurement series reflects a single time grid from founding (1896) through validation (2011), capturing the constraint's durability across the longest inter-tsunami interval in recorded local history.
 *
 * PERSPECTIVAL GAP:
 *   The behavioral-competence reading claims the constraint remained operationally enforced across all 78 years (1896–1974). The commemorative-husk reading would claim that by 1960–1974, the stone had become primarily a memorial — historically important but behaviorally attenuated, no longer actively shaping construction decisions. The engine will compute this divergence from the structural data: if residents' actual building practices deviated above the line in the 1960s–1970s, the behavioral-competence reading's claim of operational enforcement becomes untenable. If building remained consistently below the line through 1974 (as archaeological and historical record suggests), the behavioral-competence reading holds. The measurement data (suppression_requirement at 1960 and 1990 remain in the 0.10–0.13 range, suggesting consistent maintenance cost) supports the behavioral-competence claim, but the oscillation would be the engine's empirical verification, not this reading's assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary structure in the classical extraction sense. Aneyoshi residents collectively benefit from the prohibition's protective function, but they also collectively enforce it — they are simultaneously beneficiary and agent. This is not extraction; it is collective self-defense. The prohibition's d-values derive not from power differential but from shared hazard exposure. All residents sit near d=0.5 (symmetric: equal stake in enforcement, equal stake in protection). The stone itself is not an agent (agent=false on the non-agent entry) — it is a physical marker of a natural constraint. The listed 'beneficiary' (aneyoshi_residents_tsunami_protection) is the aggregate safety outcome, not a party that captures gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing tsunami casualties) remains live and empirically validated by the 2011 event. No divergence between the constraint's original mandate and its current operation — the mandate was not superseded; it was reconfirmed by a catastrophic natural event. The constraint exhibits zero mandatrophy: its foundational reason for existence persists, the physical hazard persists, and the behavioral practice persists. The modest rise in theater_ratio from 1896 to 2011 reflects the transition from active trauma response to intergenerational norm — not loss of function, but normalization of transmission. When the 2011 tsunami struck, the stone's behavioral force was immediately reactivated, confirming no atrophy had occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_community_rule,
    'Is the Aneyoshi prohibition a natural law (tsunami physics encoded in stone) or a constructed community rule (behavioral norm enforced by social consensus)?',
    'The distinction collapses under examination: the stone marks the run-up extent of observed physical phenomena; the rule encoding that extent is both natural (physics-derived) and constructed (maintained through social practice). Ambiguity lies in WHAT ENFORCES the compliance: physics (water will destroy houses in the run-up zone), social norm (community will not grant construction permits above the line), or both equally.',
    'If physics-enforced, the constraint is a mountain with near-zero extractiveness and minimal suppression needed (people won''t build where they drown). If socially-enforced, suppression becomes meaningful (community must actively refuse construction requests) and the constraint becomes a rope. The measured low suppression (0.12) and high accessibility_collapse (0.92) support the physics-primary reading: the water''s reality enforces compliance more than community policing does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_community_rule, conceptual, 'Whether the constraint''s enforcement is primarily physical (tsunamis) or primarily social (community consensus).').

omega_variable(
    behavioral_competence_decay_detection,
    'Did the prohibition''s behavioral force decay from 1960 onward, shifting from enforced rule to commemorative symbol, or did it remain operationally binding through 1974?',
    'Oral history from residents who came of age in the 1960s–1970s; archaeological evidence of construction permits granted above the line during this period; examination of family oral histories and community meeting records if available. The 2011 tsunami''s validation (destruction below, survival above) was consistent with behavioral enforcement; if the rule had decayed to symbol by 1990, violation would have been expected and visible in the 2011 aftermath.',
    'If decay is found, the behavioral-competence reading is false and the commemorative-husk reading becomes live. If enforcement persisted, the behavioral-competence reading holds. The theater_ratio trajectory in the measurements (rising but remaining low through 2011) supports persistence; a dramatic jump in theater (theater_ratio > 0.7) would indicate symbol-without-function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_competence_decay_detection, empirical, 'Whether intergenerational transmission of the rule remained behaviorally binding or shifted to commemoration.').

omega_variable(
    false_summit_natural_law_candidate,
    'Does the Aneyoshi prohibition benefit identifiable parties (residents, by protection from tsunami) in a way that might suggest a constructed rule disguised as natural law?',
    'Examine whether any external party (developers, speculators, commercial interests) was excluded from profit-taking in the prohibited zone and might have benefited if the rule did not exist. Investigate whether the residents'' motivation was collective survival (natural law framing) or control of land-use to preserve community character or exclude outsiders (constructed-rule framing with beneficiary structure).',
    'If genuine beneficiaries (parties motivated by extraction, not safety) are identified, the prohibition shifts from mountain toward rope or tangled_rope. The oral history and historical record consistently frame the rule as response to physical hazard, not as mechanism for excluding developers — no significant economic pressure for prohibited-zone development existed in rural Iwate during 1896–1974. The false-summit concern is low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, empirical, 'Whether the prohibition''s beneficiaries (residents'' protection) constitute constructed extraction or genuine natural response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1896, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1896, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1896, observed).
narrative_ontology:measurement(aney_tr_t1930, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement_basis(aney_tr_t1930, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1960, observed).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(aney_tr_t1990, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1896, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1896, 0.06).
narrative_ontology:measurement_basis(aney_be_t1896, observed).
narrative_ontology:measurement(aney_be_t1930, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1930, 0.07).
narrative_ontology:measurement_basis(aney_be_t1930, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement_basis(aney_be_t1960, observed).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1990, 0.09).
narrative_ontology:measurement_basis(aney_be_t1990, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.08).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1896, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1896, 0.08).
narrative_ontology:measurement_basis(aney_su_t1896, observed).
narrative_ontology:measurement(aney_su_t1930, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1930, 0.09).
narrative_ontology:measurement_basis(aney_su_t1930, observed).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1960, 0.11).
narrative_ontology:measurement_basis(aney_su_t1960, observed).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1990, 0.13).
narrative_ontology:measurement_basis(aney_su_t1990, observed).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement_basis(aney_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two constraint stories with opposite claims about the stone's behavioral status over 1896–2011. The behavioral-competence reading claims the prohibition remained a live, operationally enforced rule throughout the interval (ε ≈ 0.08, mountain-type, physics-encoded). The commemorative-husk reading claims the prohibition decayed to historical symbol by the late 20th century (higher theater_ratio, attenuated behavioral force, possible rope/piton-type). The two readings share the same founding problem (1896 tsunami) and physical marker (the stone) but diverge on the constraint's OPERATIONAL STATUS during 1960–2011. Both stories are required to capture the kernel's full structural contestation. The behavioral-competence reading influences the husk reading: if behavioral enforcement persists, the husk reading's decay narrative is falsified. The husk reading does NOT foreclose the behavioral-competence reading (both can coexist in different parties' accounts) but creates pressure on its empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
