% ============================================================================
% CONSTRAINT STORY: ministerial_responsibility__select_committee_accountability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ministerial_responsibility__select_committee_accountability_reading, []).

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
 *   constraint_id: ministerial_responsibility__select_committee_accountability_reading
 *   human_readable: Ministerial Responsibility: Select Committee Accountability Reading
 *   domain: legal/doctrinal/parliamentary
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel of
 *   ministerial responsibility. The kernel itself is the stabilized
 *   parliamentary doctrine that ministers are personally accountable for
 *   their departments' actions — a commitment that has persisted in UK
 *   parliamentary tradition since the 19th century, but whose mechanisms and
 *   operationalization remain contested. This specific reading claims that
 *   accountability has migrated from the floor-based question time theater
 *   (where ministers deploy stock answers to prepared opposition questions)
 *   to select committees (where elected chairs control interrogation, can
 *   subpoena documents, can recall officials, can demand written evidence and
 *   follow-up appearances). The reading frames this migration as EFFECTIVE —
 *   select committees do the sustained interrogation that the chamber's
 *   theater cannot. The constraint thus models committee-based accountability
 *   as a tangled rope: genuine information extraction via committee power
 *   (asymmetric advantage to committees) alongside legitimate coordination
 *   (committees and executive both benefit from structured scrutiny that
 *   improves administration). The suppression metric (0.35) reflects that
 *   ministers can no longer evade via the chamber's procedural weaknesses —
 *   committees close off evasion routes through subpoena power and sustained
 *   multi-session interrogation. Extractiveness (0.38) is moderate:
 *   committees extract significant information and drive policy concessions,
 *   but the extraction is not coercive (there is no imprisonment, fine, or
 *   forced resignation triggered by committee findings alone). Theater ratio
 *   (0.28, low) reflects that committee interrogation, unlike question time,
 *   is substantively engaged: committees operate under public scrutiny but
 *   with genuine interrogation, documentary evidence, and adversarial
 *   exchange. The temporal trajectory (theater declining, extractiveness
 *   rising, suppression requirement falling) models the history of select
 *   committee institutionalization: as committees matured from advisory
 *   bodies (1970s-1980s) to powers-wielding interrogators with subpoena
 *   rights (1990s onward), their ability to suppress executive evasion
 *   increased while the need for coercive enforcement declined — committees
 *   became more effective through procedural power, not through increased
 *   force.
 *
 * KEY AGENTS:
 *   - Ministers Under Interrogation: powerful/constrained — subject to committee subpoena and sustained questioning; cannot evade via question time theater but benefit from structured forum for policy defense
 *   - Elected Committee Chairs: organized/mobile — control interrogation forum, set agenda, manage follow-up; experience this as solution to accountability problem (coordinate scrutiny)
 *   - Parliamentary Scrutiny Capacity: institutional/arbitrage — the abstract function of parliament's ability to extract information from executive; benefits from committee institutionalization
 *   - Chamber Question Time Theater: powerless/trapped — the floor-based ritual bypassed by committee migration; persists as performative spectacle with diminished accountability function
 *   - Executive Machinery (Government Departments): institutional/arbitrage — experiences both coordination (accountability forces better administration) and extraction (committees subpoena information and drive policy change)
 *   - Resignation Norm (Sibling Reading Implication): institutional/constrained — the ultimate accountability sanction has decayed; select committees may extract information and force policy change but cannot compel resignation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ministerial_responsibility__select_committee_accountability_reading, 0.38).
domain_priors:suppression_score(ministerial_responsibility__select_committee_accountability_reading, 0.35).
domain_priors:theater_ratio(ministerial_responsibility__select_committee_accountability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ministerial_responsibility__select_committee_accountability_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ministerial_responsibility__select_committee_accountability_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ministerial_responsibility__select_committee_accountability_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ministerial_responsibility__select_committee_accountability_reading, tangled_rope).
narrative_ontology:human_readable(ministerial_responsibility__select_committee_accountability_reading, "Ministerial Responsibility: Select Committee Accountability Reading").
narrative_ontology:topic_domain(ministerial_responsibility__select_committee_accountability_reading, "legal/doctrinal/parliamentary").

domain_priors:requires_active_enforcement(ministerial_responsibility__select_committee_accountability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ministerial_responsibility__select_committee_accountability_reading, '32f8e883-038c-4595-a077-d5ee44b0f2d2').
narrative_ontology:cs_kernel_codification('32f8e883-038c-4595-a077-d5ee44b0f2d2', formalized).
narrative_ontology:cs_authority_grounding('32f8e883-038c-4595-a077-d5ee44b0f2d2', lineage).
narrative_ontology:cs_interpretation_layer_present('32f8e883-038c-4595-a077-d5ee44b0f2d2').
narrative_ontology:cs_reading_relation('32f8e883-038c-4595-a077-d5ee44b0f2d2', ministerial_responsibility__agency_accountability_gap_reading, coexists_with).
narrative_ontology:cs_reading_relation('32f8e883-038c-4595-a077-d5ee44b0f2d2', ministerial_responsibility__resignation_norm_decay_reading, coexists_with).
narrative_ontology:cs_axiom('32f8e883-038c-4595-a077-d5ee44b0f2d2', foundational, select_committees_constitute_effective_accountability).
narrative_ontology:cs_axiom_status(select_committees_constitute_effective_accountability, holdable).
narrative_ontology:cs_axiom_grounding('32f8e883-038c-4595-a077-d5ee44b0f2d2', select_committees_constitute_effective_accountability, instrumental).
narrative_ontology:cs_axiom('32f8e883-038c-4595-a077-d5ee44b0f2d2', foundational, accountability_migrates_off_floor_to_committee).
narrative_ontology:cs_axiom_status(accountability_migrates_off_floor_to_committee, holdable).
narrative_ontology:cs_axiom_grounding('32f8e883-038c-4595-a077-d5ee44b0f2d2', accountability_migrates_off_floor_to_committee, empirically_contingent).
narrative_ontology:cs_reference_frame('32f8e883-038c-4595-a077-d5ee44b0f2d2', committee_based_accountability_operative).
narrative_ontology:cs_drift_state('32f8e883-038c-4595-a077-d5ee44b0f2d2', contemporary_selective_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32f8e883-038c-4595-a077-d5ee44b0f2d2', '').
narrative_ontology:cs_kernel_id(ministerial_responsibility__select_committee_accountability_reading, ministerial_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ministerial_responsibility__select_committee_accountability_reading, parliamentary_scrutiny_capacity).
narrative_ontology:constraint_beneficiary(ministerial_responsibility__select_committee_accountability_reading, elected_oversight_committees).
narrative_ontology:constraint_victim(ministerial_responsibility__select_committee_accountability_reading, executive_evasion_dynamics).
narrative_ontology:constraint_victim(ministerial_responsibility__select_committee_accountability_reading, chamber_question_time_theater).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINISTER (TANGLED ROPE) — Faces sustained interrogation from elected committee chairs who control the forum, interrogation duration, and follow-up scheduling. Cannot evade via parliamentary theater (set-piece Q&A). But also benefits: select committee format offers structured time to explain policy, build record, and coordinate with committee allies. The constraint extracts information and constrains evasion tactics while providing legitimate forum for defense. Chi moderate: constrained exit (high cost to refusing committee appearance) but also genuine coordination benefit (structured scrutiny beats ambush).
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY SCRUTINY COALITION (ROPE) — Elected committee chairs and cross-party committee members experience this as coordination mechanism: select committees solve a collective-action problem (how to extract sustained scrutiny from executives). Low extraction overhead relative to benefit; committees gain power and information. Exit options are mobile: committee members can refuse appointment or exit via non-reelection, but institutional incentives align toward participation. Pure coordination from this angle.
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CHAMBER QUESTION TIME THEATER (SNARE) — The floor-based interrogation ritual is bypassed and degraded by select committee migration. Question time survives as performative spectacle, but its accountability function is hollow — ministers prepare stock answers, opposition performs outrage, government benches applaud, the cycle repeats with zero information extraction. The theater cannot exit (it is constitutive of parliamentary identity), bears the cost of continued marginalization, and receives no compensating benefit. Pure snare.
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE MACHINERY (TANGLED ROPE) — Experiences select committees as both coordination and extraction. Coordination: committee accountability mechanisms force executive agencies to organize records, evidence, and coherent policy arguments — the constraint improves internal executive function. Extraction: committees can subpoena documents, recall officials, demand explanations, and create legislative pressure for policy change. The executive can arbitrage by preparing comprehensively (high cost) or stonewalling (reputational cost). Neither escape is free. Net: genuine coordination function (accountability drives better administration) alongside asymmetric extraction (committees extract information executives would prefer to withhold).
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL OVERSIGHT FUNCTION (ROPE) — From the civilizational/constitutional angle, select committees represent a structural solution to a perennial problem: how to maintain parliamentary scrutiny of the executive without destroying the government's capacity to govern. The constraint is pure coordination: it solves the separation-of-powers problem by institutionalizing a forum where scrutiny can occur at scale without bringing government to a halt via constant floor rebellion. Low extraction: the mechanism coordinates oversight, not extracts resources. No victimization — the function is symmetric.
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RESIGNATION NORM (PITON) — The formal legal rule that ministers must resign on departmental failure has atrophied. Select committees have migrated accountability, but the resignation convention (once the ultimate sanction) now persists mainly in the breach. Ministers apologize, acknowledge findings, implement recommendations — but stay in post. The norm survives through institutional inertia and occasional invocation (e.g., when pressure is extreme), but its functional force has decayed. Theater ratio high (~0.80): the norm is maintained performatively while the real accountability mechanism (select committee interrogation and policy forcing) operates elsewhere. The constraint is a degraded institution.
constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ministerial_responsibility__select_committee_accountability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ministerial_responsibility__select_committee_accountability_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ministerial_responsibility__select_committee_accountability_reading, TR),
    TR >= 0.70.

:- end_tests(ministerial_responsibility__select_committee_accountability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Select committees extract substantial information through subpoena power, forcing executives to prepare detailed evidence, submit to interrogation, and respond to recommendations. But extraction is bounded — committees cannot impose direct punishment; they operate via reputational pressure, policy-forcing through legislative leverage, and threat of further interrogation. The original floor-based accountability (question time) had extractiveness ~0.15 (easily evaded via prepared answers), so the committee migration represents a +0.23 increase in effective extraction. Suppression (0.35): Moderate. Ministers face reduced ability to evade through the chamber's procedural looseness. They cannot deflect with non-sequiturs or rhetorical flourishes; committees demand specific written evidence and technical answers. But suppression is not absolute — ministers can still stonewall, cite legal advice, defer to agency heads (agency delegation escape), or refuse sensitive details on security/cabinet confidentiality grounds. The temporal trajectory shows suppression declining from 0.60 to 0.35 because committees became more EFFECTIVE through procedural power and legitimacy, not through increased coercive force. Theater ratio (0.28): Low. Committee interrogation involves genuine adversarial exchange, documentary evidence, and technical interrogation. The ritual is not performative in the sense that question time is — there is real work happening, not staged opposition/government theater. The low theater reflects the reading's core claim: committees do the sustained work that the chamber cannot.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between readings that see accountability restored (this reading: select committees work) versus readings that see accountability eroded (agency_gap: delegation breaks the chain; resignation_decay: ultimate sanction atrophied). This reading's classification (tangled rope with moderate extraction) depends on the assumption that policy-forcing and information extraction matter even without resignations. If the sibling reading is correct (resignations alone count as real accountability), then this reading's extractiveness should be much lower — committees would be reclassified as scaffold (temporary theater pending resignation norm restoration) or even piton (performative interrogation with no real consequence).
 *
 * DIRECTIONALITY LOGIC:
 *   Ministers (powerful/constrained/national scope): d ≈ 0.55 (moderate-high targeting). They face interrogation they cannot evade and policy pressure they cannot ignore, but they also benefit from structured forum and some coalition support. Committees (organized/mobile/national scope): d ≈ 0.25 (moderate-low targeting). They experience low cost to participation and high benefit (power, information, legislative leverage). Question time theater (powerless/trapped/national scope): d ≈ 0.95 (maximum targeting). It has zero exit option, zero benefit, and bears full cost of marginalization. Executive (institutional/arbitrage/national scope): d ≈ 0.50 (symmetric). They experience genuine extraction via information subpoena and policy-forcing, but also genuine benefit via accountability-driven administrative improvement and structured defense opportunity. The tangled rope classification depends on this d=0.50 symmetry: f(d) ≈ 0.65, producing χ = 0.38 × 0.65 × 1.0 ≈ 0.25 (moderate extracted chi), which gates the tangled rope (0.40 ≤ χ ≤ 0.90 achievable). If d shifts toward 1.0 (full targeting of executive), χ rises and reclassification pressure toward snare emerges — this is exactly the case if agency delegation actually forecloses committee oversight, leaving the executive trapped with no escape route.
 *
 * MANDATROPHY ANALYSIS:
 *   Kernel readings do not resolve mandatrophy in the classical sense. Instead, they decompose a contested commitment into its alternative operationalizations, each with its own perspectival profile. The mandatrophy here is: 'What counts as effective ministerial accountability?' This reading answers: 'Select committee interrogation, information extraction, and policy-forcing.' The agency_gap reading answers: 'Only full chain accountability including agency heads.' The resignation_decay reading answers: 'Only resignation as ultimate sanction.' The three readings coexist because they are live positions in parliamentary discourse — each party can maintain its reading without logical contradiction. The engine's job is not to pick the 'right' reading but to show that each reading produces a structurally coherent constraint classification. This reading's tangled rope classification is robust (genuine coordination + asymmetric extraction both visible from the structural data) provided the empirical conditions hold (committees can actually interrogate, ministers must actually respond, policy-forcing does actually occur).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committee_independence_actual_vs_nominal,
    'Do select committees with government party majorities exercise genuine independent scrutiny, or do they perform accountability while protecting the executive from real consequences?',
    'Analysis of committee recommendations vs government uptake; comparison of recommendation implementation rates by committee party composition; longitudinal tracking of adversarial vs collaborative committee dynamics by parliamentary session',
    'If independent: select committees represent genuine constraint on executive evasion (tangled rope/rope classification sustained). If majoritarian: committees perform accountability theater while protecting government (reclassify as piton, with theater ratio rising to 0.65+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committee_independence_actual_vs_nominal, empirical, 'Whether select committee independence is structural or majoritarian').

omega_variable(
    kernel_reading_mutual_foreclosure,
    'Does this reading''s claim (accountability migrated to committees = effective constraint on ministerial evasion) logically foreclose the agency_accountability_gap_reading''s claim (agency delegation breaks the chain = responsibility diluted)?',
    'Structural analysis: if committees interrogate ministers on delegated agency matters, does that reconstitute the chain or does it leave agency heads unaccountable? Empirical test: track which accountability gaps (agency CEO actions vs ministerial oversight) are and are not closed by committee scrutiny.',
    'If foreclosure: select committees can chase accountability all the way to agency level (reading forecloses agency_gap). If not: agencies remain structurally outside the committee interrogation frame (readings coexist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_mutual_foreclosure, empirical, 'Whether select committees reconstitute accountability for delegated agency matters').

omega_variable(
    reading_versus_resignation_norm_relationship,
    'This reading claims accountability is effective via committee scrutiny. But if the resignation norm has decayed (as the sibling reading claims), what is the ultimate sanction? Does committee interrogation matter if no consequences (resignation) follow?',
    'Distinguish between two mechanisms: (1) direct policy change forced by committee pressure (government implements recommendation to avoid further interrogation), (2) resignation triggered by committee findings (ultimate accountability). Track which mechanism drives executive compliance in specific cases.',
    'If policy-forcing works without resignation: this reading''s extractiveness is accurate (0.38). If resignations are the only effective sanction: this reading overstates committee power (should drop extractiveness, reclassify as scaffold with sunset to some future resignation restoration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_resignation_norm_relationship, empirical, 'Whether committee accountability works via policy-forcing or depends on resignation norm').

omega_variable(
    sibling_reading_contest_is_this_a_kernel,
    'Is ministerial responsibility a single contested kernel with three reading interpretations, or are these three separate constraint families with different ε values and logical structures?',
    'Structural analysis of the kernel: Does a single stabilized commitment (the doctrine of ministerial responsibility) exist that all three readings are interpreting? Or does each reading construct a different object (committee accountability vs agency structure vs resignation norm)? If different objects, they should be separate constraint families, not readings of one kernel.',
    'If true kernel: reading_relations apply (forecloses/coexists/influences). If not: each story is independent with separate ε values and network links (affects_constraints edges), not kernel relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contest_is_this_a_kernel, conceptual, 'Whether ministerial responsibility is a single kernel or three separate constraint families').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ministerial_responsibility__select_committee_accountability_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minres_scar_tr_t0, ministerial_responsibility__select_committee_accountability_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(minres_scar_tr_t5, ministerial_responsibility__select_committee_accountability_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(minres_scar_tr_t10, ministerial_responsibility__select_committee_accountability_reading, theater_ratio, 10, 0.28).

% Extraction over time
narrative_ontology:measurement(minres_scar_be_t0, ministerial_responsibility__select_committee_accountability_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(minres_scar_be_t5, ministerial_responsibility__select_committee_accountability_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(minres_scar_be_t10, ministerial_responsibility__select_committee_accountability_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(minres_scar_su_t0, ministerial_responsibility__select_committee_accountability_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(minres_scar_su_t5, ministerial_responsibility__select_committee_accountability_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(minres_scar_su_t10, ministerial_responsibility__select_committee_accountability_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ministerial_responsibility__select_committee_accountability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ministerial_responsibility__select_committee_accountability_reading, ministerial_responsibility__agency_accountability_gap_reading).
narrative_ontology:affects_constraint(ministerial_responsibility__select_committee_accountability_reading, ministerial_responsibility__resignation_norm_decay_reading).

% DUAL FORMULATION NOTE:
% ministerial_responsibility is a contested kernel with three readings. This story instantiates the select_committee_accountability_reading. The sibling stories (agency_accountability_gap_reading, resignation_norm_decay_reading) are separate constraint files with different ε values and reference frames. All three are linked via network.affects_constraints to indicate they are readings of the same kernel, not independent constraints. Each reading has its own cs_structure.reading_relations and axioms that declare how it relates to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
