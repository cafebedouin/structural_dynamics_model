% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command — Allegorical Displacement Reading
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the allegorical displacement reading
 *   of the herem kernel (herem_command_dt7). The reading relocates the
 *   conquest narratives of Deuteronomy 7 and Joshua from interethnic violence
 *   to internal spiritual warfare: the named 'nations' (Hittites,
 *   Girgashites, Amorites, Canaanites, Perizzites, Hivites, Jebusites)
 *   function as typological placeholders for vices (pride, lust, greed,
 *   etc.), and the command to 'devote to destruction' becomes a metaphor for
 *   mortifying sin. The constraint has effectively zero extractiveness on
 *   interethnic relations because its operational domain is the individual
 *   conscience or the community's internal moral formation. No external
 *   victim set exists; the 'victims' are abstract vices. The coordination
 *   function is the integration of scriptural violence into a coherent moral
 *   psychology without requiring literal enactment.
 *
 * KEY AGENTS:
 *   - allegorical_interpreter: Primary beneficiary (analytical/arbitrage) — gains coherent hermeneutic that preserves scriptural authority without moral injury
 *   - scriptural_community: Beneficiary (organized/constrained) — receives a non-violent reading that sustains communal identity
 *   - historical_critic: Observer (analytical/analytical) — evaluates the reading's philological and historical plausibility
 *   - durable_separation_adherent: Excluded (institutional/identity_locked) — holds the competing reading that the nations are literal and the separation mandate is binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command — Allegorical Displacement Reading").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '8c6e2bb6-0085-4a51-989d-2fad0ff357c3').
narrative_ontology:cs_kernel_codification('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', fixed_text).
narrative_ontology:cs_authority_grounding('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', lineage).
narrative_ontology:cs_interpretation_layer_present('8c6e2bb6-0085-4a51-989d-2fad0ff357c3').
narrative_ontology:cs_reading_relation('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', herem_command_dt7__contextual_supersession_reading, influences).
narrative_ontology:cs_axiom('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', foundational, herem_nations_are_typological_vices).
narrative_ontology:cs_axiom_status(herem_nations_are_typological_vices, holdable).
narrative_ontology:cs_axiom_grounding('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', herem_nations_are_typological_vices, conventional).
narrative_ontology:cs_axiom('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', foundational, conquest_is_internal_mortification).
narrative_ontology:cs_axiom_status(conquest_is_internal_mortification, holdable).
narrative_ontology:cs_axiom_grounding('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', conquest_is_internal_mortification, deontological).
narrative_ontology:cs_reference_frame('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', patristic_allegorical_tradition).
narrative_ontology:cs_drift_state('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', modern_historical_critical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c6e2bb6-0085-4a51-989d-2fad0ff357c3', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, allegorical_interpreter).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, scriptural_community).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_warfare_metaphor).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, internal_moral_conquest).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the herem texts as spiritual allegory, gaining a coherent non-violent hermeneutic that preserves scriptural authority. Can switch to other readings (historical-critical, literalist) with no material cost — the exit is intellectual, not existential.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, allegorical_interpreter, beneficiary,
    analytical, biographical, arbitrage, universal).

% A faith community that adopts this reading as its corporate hermeneutic. Receives a theologically stable way to retain the texts without endorsing violence. Exit is constrained by communal identity and formation — leaving the reading means leaving the community's interpretive framework.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, scriptural_community, beneficiary,
    organized, generational, constrained, global).

% Evaluates the reading's philological and historical claims from outside the commitment structure. Neither benefits nor pays; provides external validation or critique. Exit is trivial — the critic engages the reading as an object of study.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critic, observer,
    analytical, civilizational, analytical, universal).

% Holds the competing durable_separation reading of the same kernel. Experiences the allegorical reading as a threat to the textual authority that grounds their identity mandate. Cannot exit the conflict because their identity is fused with the literal reading; the allegorical reading's existence challenges their framework's coherence.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, durable_separation_adherent, excluded,
    institutional, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates violent conquest texts into a coherent moral psychology without requiring literal enactment, allowing communities to retain scriptural authority while rejecting interethnic violence.
% TRANSFER_FUNCTION: Moves the burden of 'conquest' from external warfare to internal self-discipline; the cost (spiritual effort) and benefit (virtue formation) accrue to the same agent.
% ABSENT_VOICES: The durable_separation adherents are structurally excluded from the allegorical reading's framework — they would object that the reading evacuates the text's plain sense and divine authority. They are present in the broader discourse but absent from this reading's internal logic.
% DISAPPEARANCE_RATIONALE: If this reading vanished, communities holding it would lose their primary hermeneutic for making the herem texts morally livable. They would either adopt a different reading (supersession, durable separation) or experience cognitive dissonance between text and conscience — the interpretive ecology would rearrange.
% FOUNDING_PROBLEM: How to read Deuteronomy 7 and Joshua's conquest narratives as authoritative scripture without endorsing or enacting genocide against named peoples.
% FOUNDING_PROBLEM_CORROBORATION: Origen (3rd c.), Augustine (4th/5th c.), and the broader patristic allegorical tradition attest this reading's antiquity and continuous use. Modern historical-critical scholars (e.g., Niditch, Seibert) corroborate from outside the benefiting tradition that the allegorical move is a documented historical strategy for domesticating violent texts, not a modern invention.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the constraint extracts nothing from identifiable agents — its 'cost' is the discipline of self-examination, which is the function itself. Suppression is low (0.12) because no alternatives are coercively foreclosed; competing readings coexist in the interpretive field. Theater ratio is minimal (0.05) because the reading's performative and functional aspects align: the metaphor does the work it claims to do. Accessibility collapse is moderately high (0.72) because once the typological logic is grasped, literalist readings appear as category errors — but this is epistemic, not coercive. Resistance is low (0.15) because the reading faces scholarly critique but no organized enforcement against it.
 *
 * PERSPECTIVAL GAP:
 *   The allegorical interpreter experiences this as a mountain (liberating hermeneutic). The durable_separation adherent experiences the SAME kernel as a rope or tangled_rope (binding identity mandate). The contextual_supersession reader experiences it as a scaffold (transitional arrangement). The engine will compute different per-seat types from the structural data of each reading's constraint story — this story only authors the allegorical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the constraint operates entirely within the agent's internal moral economy. The 'extraction' is the cost of self-discipline, which is also the benefit (virtue formation). Directionality is symmetric (d ≈ 0.5) for the sole structural agent: the practitioner of the reading. No external parties are coordinated or extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to read violent conquest texts without moral injury) remains live. The reading has not atrophied into performance; it continues to do genuine hermeneutic work for communities that hold it. No mandatrophy is resolved because the constraint's function (making the text livable) is ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this reading instantiate a distinct constraint from the durable_separation and contextual_supersession readings, or is it a interpretive variant of the same structural constraint?',
    'Compare the beneficiary/victim structures, extraction referents, and coordination functions across all three readings. If each reading produces a different ε-profile and different stakeholder surface, they are distinct constraints linked by kernel_id.',
    'If distinct constraints: each gets its own ε and classification. If interpretive variants: the kernel-level structure must be modeled as a single constraint with reading-dependent directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the allegorical displacement reading is a structurally distinct constraint from its sibling readings of the herem kernel').

omega_variable(
    extraction_relocation_completeness,
    'Is the extractiveness on interethnic relations truly zero, or does the allegorical reading still carry residual extractive force when deployed in communal contexts?',
    'Empirical study of how this reading functions in communities that hold it: does it reduce interethnic violence, or does it coexist with/defer to other readings that sustain extraction?',
    'If residual extraction exists, the constraint may be a scaffold or tangled_rope rather than a mountain. The reading''s claimed innocence would be a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_relocation_completeness, empirical, 'Whether the allegorical reading fully relocates extraction to the internal spiritual domain or retains external extractive effects').

omega_variable(
    metaphor_internalization_mechanism,
    'What mechanism ensures the ''internal warfare'' metaphor remains self-directed and does not become a template for external violence against those labeled as embodying the vices?',
    'Historical and sociological analysis of communities holding this reading: track whether metaphorical self-discipline correlates with reduced outgroup hostility or whether the typological mapping creates new victim categories.',
    'If the metaphor reliably contains violence internally, the reading is a genuine mountain. If it leaks into external targeting, it functions as a snare with a coordination cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphor_internalization_mechanism, empirical, 'Whether the internalization of herem violence is structurally stable or leaks into external extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_allegorical_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(herem_allegorical_tr_t50, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 50, 0.04).
narrative_ontology:measurement(herem_allegorical_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(herem_allegorical_tr_t150, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 150, 0.05).
narrative_ontology:measurement(herem_allegorical_tr_t200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(herem_allegorical_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(herem_allegorical_be_t50, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(herem_allegorical_be_t100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(herem_allegorical_be_t150, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 150, 0.08).
narrative_ontology:measurement(herem_allegorical_be_t200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 200, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(herem_allegorical_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(herem_allegorical_su_t50, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 50, 0.11).
narrative_ontology:measurement(herem_allegorical_su_t100, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement(herem_allegorical_su_t150, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 150, 0.12).
narrative_ontology:measurement(herem_allegorical_su_t200, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.06).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the herem_command_dt7 kernel. The allegorical reading (this story) has ε ≈ 0.08 on interethnic relations with victim set = {abstract vices}. The durable_separation reading has ε ≈ 0.65 with victim set = {designated_outsiders}. The contextual_supersession reading has ε ≈ 0.15 (residual authority of superseded text) with victim set = {}. All three are distinct constraints; the kernel is the shared textual object they interpret.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
