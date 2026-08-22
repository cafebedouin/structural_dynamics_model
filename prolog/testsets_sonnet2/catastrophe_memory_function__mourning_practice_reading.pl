% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av as Mourning-Practice and Boundary-Norm Preservation
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the mourning_practice_reading (D1/D4) of the
 *   catastrophe_memory_function kernel: Tisha B'Av's fast and associated
 *   liturgical mourning are read as pure boundary-maintenance and
 *   grief-transmission ritual, with no claim that the practice transmits
 *   survival-competence or adaptive institutional capacity (that claim
 *   belongs to the sibling survival_competence_reading and
 *   hybrid_transformation_reading, which are separate constraint files).
 *   Under this reading the practice coordinates a real problem — sustaining
 *   shared catastrophic memory and communal boundary legibility across a
 *   dispersed, stateless population — while imposing real friction on members
 *   at the community's edges whose connection to the historical grief is
 *   attenuated but who still bear the boundary-marking cost of observance or
 *   its refusal.
 *
 * KEY AGENTS:
 *   - observant_community: primary beneficiary and co-bearer of the practice's cost
 *   - rabbinic_authorities: agenda-setters who interpret and administer the obligation
 *   - assimilation_leaning_members: bear boundary-marking cost without full buy-in
 *   - interfaith_households: bear disproportionate friction from the boundary logic
 *   - unaffiliated_descendants: excluded from the conversation, implicitly the category the boundary guards against
 *   - ritual_theorists: analytical observers of the coordination/friction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.32).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av as Mourning-Practice and Boundary-Norm Preservation").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '62a03732-a1c7-4d13-a4f7-1a20a7c9ce88').
narrative_ontology:cs_kernel_codification('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', fixed_text).
narrative_ontology:cs_authority_grounding('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', lineage).
narrative_ontology:cs_interpretation_layer_present('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88').
narrative_ontology:cs_reading_relation('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', foundational, ritual_function_exhausted_by_mourning_and_boundary).
narrative_ontology:cs_axiom_status(ritual_function_exhausted_by_mourning_and_boundary, holdable).
narrative_ontology:cs_axiom_grounding('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', ritual_function_exhausted_by_mourning_and_boundary, conventional).
narrative_ontology:cs_axiom('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', secondary, boundary_maintenance_requires_no_adaptive_content).
narrative_ontology:cs_axiom_status(boundary_maintenance_requires_no_adaptive_content, holdable).
narrative_ontology:cs_axiom_grounding('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', boundary_maintenance_requires_no_adaptive_content, empirically_contingent).
narrative_ontology:cs_reference_frame('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', rabbinic_mourning_boundary_consensus).
narrative_ontology:cs_drift_state('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', contemporary_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('62a03732-a1c7-4d13-a4f7-1a20a7c9ce88', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, assimilation_leaning_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, interfaith_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, observant_community).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, collective_memory_requires_ritual_encoding).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, boundary_maintenance_sustains_group_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the fast, the liturgical readings of Lamentations, and the mourning customs of the three weeks leading up to Tisha B'Av. Members gain a shared, embodied vocabulary of loss and a reinforced sense of communal belonging. The obligation is also a cost: fasting, restricted celebration, and a yearly re-immersion in grief that some experience as psychologically heavy, especially those with no direct connection to the historical destructions being mourned.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, observant_community, payer).

% Set and interpret the halakhic requirements of the fast and the mourning period, decide questions of exemption and application in modern circumstances, and preserve the liturgical texts. They do not personally bear the extraction the practice imposes on marginal or dissenting members; their position is secured by their interpretive authority rather than by the practice's continuation, though the practice's persistence is part of what makes their role meaningful.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Experience the day as a costly boundary marker rather than a meaningful commemorative practice — a demand for continued in-group loyalty they did not choose and would prefer to relax. Formal exit (non-observance) is possible but carries real social and familial cost: exclusion from communal recognition, tension with observant relatives, and a felt loss of belonging that keeps many complying against private preference.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, assimilation_leaning_members, payer,
    moderate, biographical, constrained, national).

% Navigate a practice built around endogamous historical narrative and boundary-maintenance that does not map cleanly onto mixed households. They bear disproportionate friction — negotiating whose relatives observe, whether children are raised to fast, and how the day's exclusionary logic (mourning FOR the group, defined against those outside it) lands on a family that straddles the line.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, interfaith_households, payer,
    powerless, biographical, constrained, national).

% Have exited communal life and are not consulted in how the day's boundary function is maintained or revised, though the practice's boundary-preserving logic implicitly treats their exit as the category the observance guards against. Their absence from communal debate means the case for softening the boundary-marking dimension of the day is rarely made from inside.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, unaffiliated_descendants, excluded,
    powerless, biographical, mobile, national).

% Study the day as an instance of collective-memory ritual, comparing its mourning/boundary function to other traditions' catastrophe commemorations. They document the coordination function and the friction it produces at the community's edges without themselves bearing either.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, repeated, embodied occasion through which a dispersed community re-enacts collective loss (the destructions of the Temples and associated catastrophes) and thereby re-establishes who belongs to the group experiencing that loss as its own — solving the coordination problem of maintaining a bounded identity across generations and geography without a central territorial or political institution to do it.
% TRANSFER_FUNCTION: Moves social cost from the group's edges toward its center: those most invested in continuity (rabbinic authorities, deeply observant families) receive reinforced belonging and interpretive standing, while those at the boundary — assimilation-leaning members, interfaith households, and by extension those who have exited — absorb the friction of a practice whose logic depends on marking who is inside and who is outside.
% ABSENT_VOICES: Unaffiliated descendants and those who have formally exited communal observance would likely argue that the day's boundary-maintenance function has calcified into exclusionary weight disproportionate to any living connection to the historical catastrophes; they are structurally not present in the interpretive conversations that set the practice's scope.
% DISAPPEARANCE_RATIONALE: Observant communities and rabbinic authorities would say the world rearranges substantially — collective memory of the destructions and the identity-boundary function they anchor would erode measurably within a generation or two. Assimilation-leaning members and outside observers would say the world changes far less than claimed: the historical memory persists in textual and cultural form (holidays, liturgy, historical education) independent of the specific fast-day mourning obligation, so the practice's disappearance would mainly relax a boundary cost rather than erase a memory function.
% FOUNDING_PROBLEM: A geographically dispersed people needed a recurring, embodied mechanism to keep a shared catastrophic memory (the destruction of the First and Second Temples, and later associated tragedies folded into the same date) from fading into abstraction, and to keep the boundary of 'who mourns this together' legible across generations without a shared territory or centralized institution.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and observant communities attest the problem remains live — diaspora dispersion and assimilation pressure are, if anything, greater now than at points in the historical past. Ritual theorists and sociologists of religion, writing from outside the beneficiary group, corroborate that the boundary-maintenance function is empirically real and measurable in retention/identity studies, but note independently that the same scholars also observe the mourning content itself (grief for specific historical events) has become increasingly abstracted from lived experience, which is precisely the divergence the survival_competence_reading and hybrid_transformation_reading dispute over what the ritual is actually still doing.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because under the D1/D4 reading the practice's primary product — shared grief-memory and boundary legibility — is a genuine, low-overhead coordination good for the group that holds it, not a rent extracted for someone else's benefit; the cost borne by boundary-adjacent members is real but is the byproduct of a coordination function, not its point. Suppression (0.32) reflects real but modest social cost of non-compliance — not coercive enforcement (no formal enforcement mechanism exists) but the felt weight of communal disapproval and family friction, which is meaningfully lower than a constraint whose persistence depends on active suppression of exit. Theater ratio starts low and rises slowly (0.10 to 0.22) reflecting a plausible modern drift toward more performative observance among assimilation-adjacent members as the historical referent (destroyed Temples, ancient catastrophe) grows more remote from lived experience, without yet indicating a full metric-substitution pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the observant-community and rabbinic-authority seats, the day functions cleanly as coordination: shared memory sustained, identity reinforced, no felt extraction. From the assimilation-leaning and interfaith-household seats, the same practice registers as an imposed boundary cost with declining connection to the historical grief it commemorates. The engine should compute these as structurally different experiences of the same low-ε coordination mechanism rather than requiring the story to average them into one felt intensity.
 *
 * DIRECTIONALITY LOGIC:
 *   Observant community and rabbinic authorities sit toward the beneficiary end: the ritual's coordination product (shared memory, legible boundary, communal cohesion) accrues to them directly, and their exit options (identity_locked, arbitrage via interpretive authority) reflect that they are structurally invested in continuation. Assimilation-leaning members and interfaith households sit toward the target end: they absorb the boundary-marking cost of a practice whose grief-content and in-group logic fit their situation poorly, and their exit options are constrained by real social cost rather than absent. Unaffiliated descendants have mobile exit precisely because they have already paid the cost of leaving; they are excluded from the deliberative process, not from the underlying tension.
 *
 * MANDATROPHY ANALYSIS:
 *   The mourning_practice_reading resists mislabeling the ritual as pure extraction: the coordination function (sustaining collective catastrophic memory and group boundary across a dispersed population with no central institution) is genuine and independently attested by ritual theorists outside the beneficiary group, not merely asserted by rabbinic authorities. It equally resists mislabeling the practice as costless pure coordination (a plain rope with no friction): boundary-adjacent members and interfaith households bear a real, non-trivial cost that the D1/D4 framing does not resolve away. The rope classification (rather than tangled_rope) reflects that no party is positioned as an extractive beneficiary collecting rents FROM the payers through enforcement machinery — the cost borne by boundary-adjacent members is a byproduct of belonging to a bounded group, not a transfer engineered to benefit rabbinic authorities or observant core members at their expense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    d1d4_vs_d5_functional_boundary,
    'Does Tisha B''Av ritual observance transmit anything properly described as survival-competence or adaptive institutional capacity (D5), or is its entire functional content exhausted by mourning-practice and boundary-maintenance (D1/D4)?',
    'Comparative ethnographic and historical analysis of how communities that observe the fast intensively versus loosely fared across documented crises requiring institutional adaptation (expulsions, forced migrations) — if intensive-observance communities show no differential adaptive capacity attributable to the ritual''s content itself, the D1/D4-only reading is supported; if they show differential capacity traceable to ritual-transmitted practices (decentralized leadership models, textual portability, etc.), the survival_competence_reading or hybrid_transformation_reading gains support instead.',
    'If D5 content is present, this story''s exclusive D1/D4 framing understates the ritual''s coordination value and this reading should be treated as a partial account rather than the complete structural truth about Tisha B''Av; the sibling readings would then carry more of the story''s classificatory weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(d1d4_vs_d5_functional_boundary, conceptual, 'Whether the kernel''s functional content is fully captured by the D1/D4 mourning/boundary reading or requires the D5 survival-competence component from a sibling reading.').

omega_variable(
    boundary_function_naturalness,
    'Is the boundary-maintenance function of Tisha B''Av a constructed institutional choice (subject to revision, contested by assimilation-leaning members) or a structurally necessary feature of any group identity that persists across dispersion (in which case the cost to boundary-adjacent members is unavoidable rather than a discretionary extraction)?',
    'Cross-tradition comparison: examine whether other stateless, dispersed groups that maintain long-term collective identity without ritual catastrophe-commemoration exist, and if so, what substitutes for the boundary function Tisha B''Av performs here.',
    'If boundary-maintenance strictly requires some such mechanism, the cost borne by assimilation-leaning members and interfaith households is closer to an irreducible feature of bounded group survival (lower moral weight against the practice); if substitutable mechanisms with lower friction exist and are not adopted, the specific cost pattern looks more like an unexamined institutional default than a necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_function_naturalness, conceptual, 'Whether the boundary-cost this reading identifies is a necessary feature of group persistence or a revisable institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the catastrophe_memory_function kernel (Tisha B'Av ritual observance). mourning_practice_reading claims exclusive D1/D4 content (mourning + boundary-maintenance, ε=0.28, rope). survival_competence_reading claims exclusive D5 content (survival-competence transmission for decentralized institutional continuity) and is expected to carry a different beneficiary structure and likely a different ε, since the coordination good it identifies (adaptive capacity) has a different cost/benefit distribution than pure boundary-maintenance. hybrid_transformation_reading claims both D1/D4 and D5 simultaneously and is expected to sit structurally between or above the other two on extraction, since it asserts the fullest functional load for the same ritual practice. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways — each has its own stable ε assessed by its own reading's lights, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
