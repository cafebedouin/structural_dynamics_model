% ============================================================================
% CONSTRAINT STORY: proceduralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proceduralist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: proceduralist_reading
 *   human_readable: Precommitment Procedure as Gate on Evidentiary Standing (Proceduralist Reading)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the proceduralist reading of the kernel
 *   'positional disagreement becomes evidence only through a designed
 *   procedure.' Under this reading, evidentiary force attaches not to who
 *   holds a position or what standpoint they occupy, but to whether the
 *   position survived a costly, precommitted, adversarial test —
 *   preregistration, declared kill conditions, adversarial collaboration. The
 *   structural payoff of this reading is that it converts a standing-based
 *   axis (whose voice counts) into a compliance-based axis (who actually bore
 *   the cost of exposure to falsification versus who evaded or gamed it).
 *   That conversion is real and valuable where it holds, but it creates its
 *   own asymmetry: designing, funding, and successfully navigating the
 *   procedure requires resources and institutional sponsorship unevenly
 *   distributed across disputants, so procedural compliance itself becomes
 *   gateable and capturable even while claiming to be viewpoint-neutral.
 *
 * KEY AGENTS:
 *   - procedure_designers: institutional agenda-setters who write and administer the kill-condition protocols
 *   - well_resourced_research_programs: organized beneficiaries who can absorb procedural cost and negotiate favorable terms
 *   - under_resourced_disputants: moderate-power payers who must submit to procedure they cannot shape
 *   - positions_without_institutional_sponsors: powerless, structurally excluded from the procedure entirely
 *   - early_career_researchers_bound_by_kill_conditions: moderate-power payers exposed to asymmetric personal cost
 *   - procedure_evaders: organized actors who game or route around the procedure while claiming its credibility
 *   - field_observers: analytical seat tracking real versus theatrical compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proceduralist_reading, 0.52).
domain_priors:suppression_score(proceduralist_reading, 0.58).
domain_priors:theater_ratio(proceduralist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proceduralist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(proceduralist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(proceduralist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proceduralist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(proceduralist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proceduralist_reading, tangled_rope).
narrative_ontology:human_readable(proceduralist_reading, "Precommitment Procedure as Gate on Evidentiary Standing (Proceduralist Reading)").
narrative_ontology:topic_domain(proceduralist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(proceduralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proceduralist_reading, 'e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e').
narrative_ontology:cs_kernel_codification('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', formalized).
narrative_ontology:cs_authority_grounding('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', practice).
narrative_ontology:cs_interpretation_layer_present('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e').
narrative_ontology:cs_reading_relation('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', proceduralist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', proceduralist_reading__pragmatist_reading, influences).
narrative_ontology:cs_reading_relation('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', proceduralist_reading__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', foundational, evidentiary_force_derives_from_procedural_cost_not_position).
narrative_ontology:cs_axiom_status(evidentiary_force_derives_from_procedural_cost_not_position, holdable).
narrative_ontology:cs_axiom_grounding('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', evidentiary_force_derives_from_procedural_cost_not_position, conventional).
narrative_ontology:cs_axiom('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', secondary, compliance_exposure_is_the_relevant_beneficiary_axis).
narrative_ontology:cs_axiom_status(compliance_exposure_is_the_relevant_beneficiary_axis, holdable).
narrative_ontology:cs_axiom_grounding('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', compliance_exposure_is_the_relevant_beneficiary_axis, instrumental).
narrative_ontology:cs_reference_frame('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', adversarial_collaboration_as_neutral_arbiter).
narrative_ontology:cs_drift_state('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', contemporary_replication_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0df0372-88c3-43c2-8a3a-ca4aa4bc5e6e', '').
narrative_ontology:cs_kernel_id(proceduralist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proceduralist_reading, procedure_designers).
narrative_ontology:constraint_beneficiary(proceduralist_reading, well_resourced_research_programs).
narrative_ontology:constraint_beneficiary(proceduralist_reading, credentialing_bodies_citing_adversarial_collaboration).
narrative_ontology:constraint_victim(proceduralist_reading, under_resourced_disputants).
narrative_ontology:constraint_victim(proceduralist_reading, positions_without_institutional_sponsors).
narrative_ontology:constraint_victim(proceduralist_reading, early_career_researchers_bound_by_kill_conditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(proceduralist_reading, procedure_evaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the adversarial-collaboration protocols, preregistration templates, and kill-condition language that determine whether a positional disagreement counts as resolved evidence. They set the cost structure of the procedure itself and can revise it, position themselves as neutral referees, and are rarely bound by the kill conditions they write for others.
narrative_ontology:constraint_stakeholder(proceduralist_reading, procedure_designers, agenda_setter,
    institutional, generational, arbitrage, national).

% Have the staff, funding, and legal counsel to negotiate favorable kill-condition wording, run pilot studies before committing publicly, and absorb the reputational cost if a precommitted prediction fails. The procedure's cost is a manageable overhead for them rather than an existential filter.
narrative_ontology:constraint_stakeholder(proceduralist_reading, well_resourced_research_programs, beneficiary,
    organized, biographical, arbitrage, national).

% Must either submit their position to a costly, time-consuming precommitment procedure they cannot easily shape, or forfeit standing entirely — their claim is treated as mere assertion if they decline. They lack the capacity to negotiate the kill-condition terms and often cannot survive an unfavorable procedural outcome even when their underlying position has merit.
narrative_ontology:constraint_stakeholder(proceduralist_reading, under_resourced_disputants, payer,
    moderate, biographical, constrained, national).

% Have no institution willing to co-design or fund an adversarial collaboration on their behalf, so their disagreement never enters the procedure at all. They would object that the gate excludes positions by resource access rather than by epistemic merit, but they are not represented in the design process.
narrative_ontology:constraint_stakeholder(proceduralist_reading, positions_without_institutional_sponsors, excluded,
    powerless, biographical, trapped, national).

% Precommit to falsification criteria as a condition of participation and career advancement, then bear the full reputational and employment cost when the kill condition is triggered against their position — while senior collaborators and the procedure designers who wrote the terms bear comparatively little.
narrative_ontology:constraint_stakeholder(proceduralist_reading, early_career_researchers_bound_by_kill_conditions, payer,
    moderate, biographical, constrained, national).

% Game or route around the precommitment procedure — via post-hoc reinterpretation of kill conditions, selective preregistration, or simply declining rounds they expect to lose — while still claiming the reputational benefit of having 'gone through the process.' Their exposure to the procedure's cost is much lower than the disputants who play it straight.
narrative_ontology:constraint_stakeholder(proceduralist_reading, procedure_evaders, beneficiary,
    organized, biographical, mobile, national).

% Track which disputes were actually resolved by adversarial collaboration versus which were merely certified by having undergone the ritual. They can distinguish real kill-condition exposure from procedural theater, but their assessments carry no binding force on standing.
narrative_ontology:constraint_stakeholder(proceduralist_reading, field_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an otherwise undecidable clash of positions into something the field can act on, by forcing both sides to state in advance what evidence would count against them and then running the test — solving the real problem that unstructured debate never converges and never pays a cost for being wrong.
% TRANSFER_FUNCTION: Moves evidentiary standing from the position itself to whoever can afford, shape, or evade the procedure that tests it; resources (time, funding, institutional sponsorship, legal capacity to negotiate kill-condition wording) flow from disputants who must submit to procedure designers and well-resourced programs who can absorb or route around the procedure's cost.
% ABSENT_VOICES: Positions without institutional sponsors never reach the table because no one will fund or co-design a collaboration on their behalf; they would argue that the gate discriminates by resource access rather than by the truth-tracking properties of a position, but they have no seat in the design process to say so.
% DISAPPEARANCE_RATIONALE: If the procedural gate vanished, positional disagreements would revert to being adjudicated by rhetorical persuasion, standing, or raw institutional power with no forcing function at all — some disputes currently treated as 'resolved' would revert to open, and disputants currently excluded for lack of resources to run the procedure would be free to assert positions on the same rhetorical footing as anyone else. The field's citation and credentialing practices, which currently reward having undergone adversarial collaboration, would need a new currency.
% FOUNDING_PROBLEM: Unstructured positional disagreement in contested empirical and normative fields never converges: both sides can restate their priors indefinitely, no one pays a cost for being wrong, and disagreements persist as tribal markers rather than resolving. Preregistration and declared kill conditions were built to force a cost on being wrong and make evasion visible.
% FOUNDING_PROBLEM_CORROBORATION: Procedure designers and well-resourced programs attest the founding problem is still live and the procedure is functioning as intended. Field observers and researchers who study replication and adversarial-collaboration outcomes report a substantial fraction of nominally precommitted disputes are resolved by evasion, selective interpretation of kill conditions, or asymmetric cost-bearing rather than genuine exposure to falsification — corroboration from outside the beneficiary set exists but is contested by the designers themselves.
narrative_ontology:disappearance_verdict(proceduralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(proceduralist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(proceduralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(proceduralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(proceduralist_reading, 0.52, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proceduralist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(proceduralist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(proceduralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) reflects that the procedure genuinely solves a coordination problem (forcing precommitment converts otherwise endless disagreement into testable claims) but has drifted toward capturing resources from those least able to navigate its cost structure. Suppression (0.58) is moderate-high because declining the procedure is treated as forfeiting evidentiary standing entirely — there is no neutral middle ground once the procedure exists as the field's gatekeeping mechanism. Theater ratio rises from 0.18 to 0.40 over the interval, tracking the growing gap between disputes that undergo genuine kill-condition exposure and disputes that are merely certified as having 'gone through adversarial collaboration' while the substantive test was evaded or its terms renegotiated after the fact. Accessibility collapse (0.5) and resistance (0.55) are moderate: the procedure has real workable predecessors (unstructured peer debate, standing-based deference) that persist alongside it, and disputants excluded by resource constraints actively resist the framing that their exclusion reflects epistemic merit rather than capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality tracks compliance-exposure, not social position: procedure designers and well-resourced programs sit near the beneficiary end because they shape the terms and can absorb the procedure's cost without much personal exposure to a triggered kill condition. Under-resourced disputants and early-career researchers sit near the target end because they bear real exposure — a triggered kill condition costs them standing, career capital, or both — while lacking the capacity to negotiate favorable terms. Procedure evaders are a distinct beneficiary class: they collect the reputational credit of having undergone the ritual while structurally avoiding the exposure that gives the ritual its evidentiary force. This is the reading's structural signature: the beneficiary/victim split is defined by who evades or games the precommitment mechanism, not by prior social standing — a well-resourced actor bound by a real kill condition is exposed under this reading exactly as a marginal actor would be, and a marginal actor who successfully games the procedure would count as a structural beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unstructured disagreement never converges and never costs anyone anything for being wrong — remains partly live (contested framings persist across many fields) but the procedure's actual operation has partially decoupled from that problem: a growing share of what passes as adversarial collaboration is theater (rising theater_ratio) rather than genuine exposure to falsification. This is not full mandatrophy (the founding function still operates in some disputes) but the drift is the diagnostic the classification exists to catch — treating procedural compliance as self-certifying evidence, rather than checking whether the specific kill condition was live and binding, would let evaders capture standing without ever bearing the cost the procedure is supposed to impose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_vs_standing_confound,
    'When a well-resourced disputant navigates the procedure successfully, is that because their underlying position had more merit, or because they had more capacity to shape kill-condition terms favorably and absorb procedural cost?',
    'Compare outcomes across procedures where kill-condition terms were externally fixed (by a third party with no stake) versus negotiated between disputants; if the well-resourced party''s success rate drops sharply under externally-fixed terms, capacity was doing the work, not merit.',
    'If capacity dominates, the proceduralist reading''s claim to be standing-neutral is partly false — the compliance axis re-imports the standing axis it claims to replace, which would push this constraint toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_standing_confound, empirical, 'Whether procedural success under this reading tracks position merit or resource capacity to shape the procedure.').

omega_variable(
    kill_condition_bindingness_measurement,
    'How would an outside observer distinguish a genuinely binding kill condition (one that, when triggered, actually costs the losing party standing) from a nominal one that gets reinterpreted or waived after the fact?',
    'Audit a sample of adversarial collaborations for post-hoc renegotiation of kill-condition terms after preliminary results were known but before public commitment, and track whether triggered kill conditions actually altered subsequent citation, funding, or credentialing outcomes for the losing party.',
    'A high rate of post-hoc renegotiation or non-consequential triggering would substantiate the rising theater_ratio measurement and support reclassifying specific instances as pure procedural theater (piton-adjacent) rather than functioning coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kill_condition_bindingness_measurement, empirical, 'Whether declared kill conditions are actually binding in practice or nominally binding only.').

omega_variable(
    framing_choice_between_readings,
    'Is the compliance-based framing (this reading) the correct lens for THIS kernel, or does the standpoint_reading''s framing better capture what actually happens when procedures are designed by parties with unequal power to write the kill conditions in the first place?',
    'Trace procedure design history: if kill-condition language was itself negotiated asymmetrically (i.e., standing determined who got to write the compliance rules), the compliance axis is downstream of the standing axis rather than independent of it, which would blur the boundary between this reading and standpoint_reading.',
    'If standing determines compliance-rule design, this reading and standpoint_reading are less structurally distinct than the kernel''s four-way decomposition assumes for procedures with concentrated design power; the classification here would need revisiting for those cases specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_between_readings, conceptual, 'Whether the proceduralist reading''s compliance axis is genuinely independent of the standpoint reading''s standing axis, or derivative of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proceduralist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, proceduralist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(proc_tr_t4, proceduralist_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(proc_tr_t8, proceduralist_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(proc_tr_t12, proceduralist_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(proc_tr_t16, proceduralist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(proc_tr_t20, proceduralist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(proc_tr_t24, proceduralist_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, proceduralist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(proc_be_t4, proceduralist_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(proc_be_t8, proceduralist_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(proc_be_t12, proceduralist_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(proc_be_t16, proceduralist_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(proc_be_t20, proceduralist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(proc_be_t24, proceduralist_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, proceduralist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(proc_su_t4, proceduralist_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(proc_su_t8, proceduralist_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(proc_su_t12, proceduralist_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(proc_su_t16, proceduralist_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(proc_su_t20, proceduralist_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(proc_su_t24, proceduralist_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proceduralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(proceduralist_reading, standpoint_reading).
narrative_ontology:affects_constraint(proceduralist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(proceduralist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence. All four share the same kernel text (positional disagreement becomes legitimate evidence only through a designed, costly procedure) but author different beneficiary/victim structures, different ε, and different classifications: this reading (proceduralist) locates evidentiary force in procedural compliance-cost; standpoint_reading locates it in social position; pragmatist_reading locates it in downstream practical consequences; instrumentalist_reading locates it in institutional payoff independent of truth-tracking. Each is linked to the others via affects_constraints per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
