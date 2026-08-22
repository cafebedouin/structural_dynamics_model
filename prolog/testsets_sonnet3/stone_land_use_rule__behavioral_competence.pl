% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Tsunami Stone as Binding Land-Use Prohibition (Behavioral-Competence Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   This story instantiates the behavioral-competence reading of the
 *   tsunami-stone kernel: the marker is read as a live, functioning land-use
 *   prohibition whose behavioral force is measurable in the actual settlement
 *   pattern sustained over 78 years, not merely in the persistence of the
 *   physical object. Under this reading, villagers who build hillside homes
 *   are making a costly, observable choice — accepting steeper terrain and
 *   higher construction expense — in continuous deference to the
 *   inscription's instruction. This reading's ε is low because the
 *   arrangement functions as coordination with almost no identifiable
 *   extraction: no party profits from another party's compliance, and the
 *   cost of the constraint (foregone lowland convenience) is borne by the
 *   same population that receives its benefit (reduced casualty risk). The
 *   sibling reading (commemorative_husk) treats the same stone as having
 *   decayed into symbolic gesture without behavioral force; that is a
 *   structurally distinct claim with its own ε and is authored as a separate
 *   constraint linked via network.affects_constraints, per the ε-invariance
 *   principle — this file does not average across the two readings or hedge
 *   between them.
 *
 * KEY AGENTS:
 *   - hillside_settlement_households: primary beneficiaries who bear the daily cost of compliance
 *   - future_generations_of_villagers: the ultimate beneficiary class, unable to consent
 *   - coastal_lowland_ecosystem_users: dual beneficiary/payer who use but do not sleep in the zone
 *   - village_elders_and_custodians: agenda-setters who maintain the practice without extracting rent
 *   - prospective_lowland_developers: excluded voice pressing for economically rational violation
 *   - disaster_anthropology_researchers: analytical observers assessing whether compliance is genuine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.28).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Tsunami Stone as Binding Land-Use Prohibition (Behavioral-Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'e0bd69a9-9d47-4be9-b5ee-ddab5e645191').
narrative_ontology:cs_kernel_codification('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', fixed_text).
narrative_ontology:cs_authority_grounding('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', practice).
narrative_ontology:cs_interpretation_layer_present('e0bd69a9-9d47-4be9-b5ee-ddab5e645191').
narrative_ontology:cs_reading_relation('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', foundational, sustained_practice_constitutes_binding_prohibition).
narrative_ontology:cs_axiom_status(sustained_practice_constitutes_binding_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', sustained_practice_constitutes_binding_prohibition, empirically_contingent).
narrative_ontology:cs_axiom('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', secondary, settlement_pattern_is_the_evidence_of_compliance).
narrative_ontology:cs_axiom_status(settlement_pattern_is_the_evidence_of_compliance, holdable).
narrative_ontology:cs_axiom_grounding('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', settlement_pattern_is_the_evidence_of_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', post_disaster_founding_inscription).
narrative_ontology:cs_drift_state('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', contemporary_78_year_mark, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0bd69a9-9d47-4be9-b5ee-ddab5e645191', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, hillside_settlement_households).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations_of_villagers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_lowland_ecosystem_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, coastal_lowland_ecosystem_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live above the stone's marked line, having accepted the steeper walk, thinner soil, and higher construction cost of hillside building. They receive the benefit of the rule daily as a background fact rather than a felt cost — the incline is a known price already paid, not an active grievance. When asked, they cite the stone and the practice of never building below it as the reason the village survived the most recent tsunami while lowland settlements elsewhere did not.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, hillside_settlement_households, beneficiary,
    moderate, generational, constrained, local).

% Not yet born or too young to have chosen the settlement pattern; they inherit the land-use boundary as an ambient fact of where the village already is. They cannot consent to or renegotiate the rule, but they are the population the 78-year compliance record is ultimately protecting from a low-probability, high-consequence event they have no direct memory of.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations_of_villagers, beneficiary,
    powerless, civilizational, trapped, local).

% Farm, fish, and gather in the lowland zone below the stone during daily life, accepting that permanent dwellings are not built there even though the land is flatter and more convenient for structures. They pay the cost of longer commutes back to hillside homes each evening but benefit from the same protection the rule provides, since they are not sleeping in the inundation zone when a wave arrives at night.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_lowland_ecosystem_users, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, coastal_lowland_ecosystem_users, payer).

% Maintain and retell the stone's inscription, walk newcomers and children to the boundary, and socially sanction proposals to build below the line. Their authority over the village's land-use norms is bound up with being the carriers of the inscription's meaning; they do not extract material rent from enforcing it, but their role and identity are constituted by keeping the practice alive.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, village_elders_and_custodians, agenda_setter,
    organized, generational, identity_locked, local).

% Outside investors or younger residents occasionally propose flatter, cheaper construction in the marked zone for economic reasons — proximity to the harbor, lower building cost. They are not part of the informal deliberation that reaffirms the boundary each generation; their preference for lowland building is overridden by social consensus rather than negotiated with them directly.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, prospective_lowland_developers, excluded,
    moderate, biographical, constrained, regional).

% Study the stone as a rare case of intergenerational hazard memory that produced measurable behavioral compliance rather than symbolic commemoration alone. They compare settlement patterns against inundation maps and interview residents to assess whether the prohibition is still functionally observed or has degraded into ritual.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_anthropology_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of hazard memory outliving the direct experience of the hazard: a single physical marker and an inherited daily practice of not building below it lets successive generations, who never witnessed the originating tsunami, coordinate settlement location without requiring each generation to independently rediscover or re-derive the safe elevation.
% TRANSFER_FUNCTION: Moves construction convenience and lowland economic advantage (flatter land, shorter commutes, cheaper building) away from all residents collectively, in exchange for reduced casualty risk during future tsunami events. No party extracts a rent from another party through this arrangement — the cost and the benefit land on largely the same population across time.
% ABSENT_VOICES: Prospective lowland developers and residents attracted by lower building costs are not formal parties to the ongoing social reaffirmation of the boundary; the rule persists through diffuse community sanction rather than a body that would hear their case. Future generations, the largest beneficiary class, obviously cannot speak for themselves and are represented only by the elders' custodianship.
% DISAPPEARANCE_RATIONALE: If the stone and the practice of observing it vanished overnight, the strongest available evidence is regional: nearby villages that lost or ignored equivalent markers were resettled in the inundation zone within a generation and suffered severe casualties in subsequent tsunamis. Removing the marker here would very plausibly be followed, over one or two generations, by economically-motivated lowland construction and a reoccupation of the hazard zone — the settlement pattern is not a coincidence sitting on top of the stone, it is produced by it.
% FOUNDING_PROBLEM: In 1933 (and reinforced after 1960), a tsunami destroyed lowland dwellings and killed residents who had settled below a certain elevation; survivors erected a stone marking that line with an inscription instructing descendants never to build below it, to prevent the community from forgetting the hazard once living memory of the wave faded.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster anthropologists and seismological hazard-mapping studies conducted decades after the marker's erection, with no stake in village land-use politics, corroborate that the marked elevation closely tracks the actual historical inundation line and that villages which retained comparable markers and settlement discipline suffered markedly lower casualties in the 2011 tsunami than nearby villages that had resettled the lowland zone. This corroboration comes from outside the village's own custodial class, which is the load-bearing evidence for treating the founding problem as still live rather than self-servingly claimed.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and essentially flat across the interval because, under this reading, the constraint's cost (accepting hillside terrain) and its benefit (reduced casualty risk) land on the same population across generations — there is no structural transfer from a payer class to a beneficiary class. Suppression is moderate (0.28), reflecting real social sanction against lowland building, but this sanction is diffuse community norm-enforcement rather than coercive apparatus; it is authored as non-zero because the excluded developer voice is genuinely overridden, not merely persuaded. Theater ratio is low and rises only slightly (0.03 to 0.08) — the practice remains substantially behavioral (actual settlement avoidance) rather than performative (ritual acknowledgment without practical effect), which is precisely the structural claim this reading makes and the commemorative_husk reading would contest.
 *
 * PERSPECTIVAL GAP:
 *   From the hillside households' seat, the constraint reads as an accepted, internalized cost of belonging to a place that survived — barely felt as a constraint at all after 78 years of continuous practice. From the excluded prospective-developer seat, the same arrangement reads as an unexamined social veto blocking economically rational land use with no formal recourse. The elders' agenda-setter seat experiences the constraint as identity-constitutive custodianship rather than either coordination or extraction in the ordinary sense — their exit option is coded identity_locked because relinquishing the marker's authority would dissolve their institutional role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hillside households, future generations, lowland ecosystem users) are coded with low-to-symmetric directionality because the same populations bear the cost of the arrangement across their lifespans — there is no separate extraction-target class. No victims are declared under this reading precisely because behavioral competence implies the cost and benefit are borne jointly, which is the central structural claim distinguishing this reading from a reading in which the rule primarily burdens one group for another's gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (hazard memory loss leading to reoccupation of the inundation zone) is authored as still live, corroborated by independent post-2011 casualty comparisons across villages with and without comparable markers — this is the evidence that prevents this reading from being classified as mandatrophy. A constraint whose founding problem had gone dead while the practice persisted by inertia would be the commemorative_husk reading's territory, not this one; the classification here depends on the behavioral record (actual non-construction below the line) rather than on the mere continued existence of the stone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_verification_method,
    'Is village-wide non-construction below the marked line actually verified against settlement records and hazard maps, or is the ''78 years of compliance'' claim itself partly a retrospective narrative construction by the custodial elders?',
    'Cross-reference historical land registry and construction permit records (where they exist) against the marked elevation line, and compare against satellite/aerial imagery over the interval to establish an independent settlement-pattern timeline not mediated by elder testimony.',
    'If independent record-keeping confirms sustained non-construction, the behavioral_competence reading is strongly supported. If the record shows drift toward lowland construction that the community narrative elides, this reading may itself be overstating present compliance and drifting toward the commemorative_husk pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_verification_method, empirical, 'Whether the sustained-compliance claim is independently verifiable or partly self-reported by the custodial class.').

omega_variable(
    behavioral_vs_commemorative_boundary_location,
    'At what point would declining behavioral observance (e.g., a first permitted lowland structure, or a generation that no longer walks the boundary with children) tip this constraint from the behavioral_competence reading into the commemorative_husk reading?',
    'Define and track a threshold set of observable practices (school ritual visits, actual construction permits issued below the line, elder succession continuity) and monitor for the first structural breach as a discrete event rather than assuming continuous gradual decay.',
    'Without a defined threshold, the two sibling readings could both claim the same ambiguous transitional evidence, making the kernel''s decomposition into two constraints appear arbitrary rather than structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_boundary_location, conceptual, 'The structural location of the boundary between the two sibling readings of the same kernel.').

omega_variable(
    excluded_developer_coalition_potential,
    'Could prospective lowland developers, if organized, mount a legitimate economic-rights challenge to the informal land-use prohibition, and would that constitute the rule''s first genuine adversarial contest rather than diffuse social consensus?',
    'Track whether any formal legal or municipal challenge to the marked boundary''s informal authority is ever filed, and whether local government incorporates the marker into zoning law (formalizing it) or allows economic pressure to erode it.',
    'Formalization into zoning law would shift this from a purely informal-norm rope toward a state-backed constraint with different suppression dynamics; an eventual successful challenge would be the clearest evidence of behavioral competence declining.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_developer_coalition_potential, preference, 'Whether latent developer interest could ever organize into an active challenge to the informal prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.04).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.05).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.05).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.06).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.07).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.1).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.11).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.11).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.12).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.12).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.08).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This story and stone_land_use_rule__commemorative_husk decompose a single natural-language label ('the tsunami stone's authority') into two structurally distinct constraints per the ε-invariance principle. This file (behavioral_competence) authors low ε (0.12) on the claim that daily spatial practice still enforces the boundary. The sibling (commemorative_husk) would author substantially higher theater_ratio and a different classification on the claim that the marker's behavioral force has decayed to symbolic acknowledgment. They share the same physical kernel (the inscribed stone, the founding 1933/1960 tsunami narrative) but are evaluated by different observables (actual settlement pattern vs. ritual/commemorative activity) and therefore must be separate stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
