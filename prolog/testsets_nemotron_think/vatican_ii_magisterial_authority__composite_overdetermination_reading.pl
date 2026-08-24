% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority as Overdetermined Composite
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-65) produced sixteen documents ratified by 88-90%
 *   supermajorities. This reading argues the supermajority was achieved by
 *   deliberate overdetermination: key texts (Lumen Gentium, Gaudium et Spes,
 *   Dignitatis Humanae, Unitatis Redintegratio) encode incompatible
 *   ecclesiological visions — juridical primacy and sacramental collegiality,
 *   continuity and rupture, confessional unity and religious liberty — in
 *   formulations ambiguous enough for opposing factions to sign. The 10-12%
 *   rejection votes (highest on religious liberty and ecumenism) signal
 *   unresolved theological incompatibility embedded in the final texts.
 *   Post-conciliarly, hermeneutical control becomes the real locus of
 *   magisterial authority: the texts cannot implement themselves; their
 *   ambiguity requires an authoritative interpreter, and the Curia/papacy
 *   monopolizes that interpretation. Implementation divergence (e.g.,
 *   liturgical reform, ecumenical practice, collegiality exercise) is not a
 *   bug but the structural feature that sustains the constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority as Overdetermined Composite").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'c5b40d01-fc82-4775-829f-d6648a0f85a9').
narrative_ontology:cs_kernel_codification('c5b40d01-fc82-4775-829f-d6648a0f85a9', formalized).
narrative_ontology:cs_authority_grounding('c5b40d01-fc82-4775-829f-d6648a0f85a9', lineage).
narrative_ontology:cs_interpretation_layer_present('c5b40d01-fc82-4775-829f-d6648a0f85a9').
narrative_ontology:cs_reading_relation('c5b40d01-fc82-4775-829f-d6648a0f85a9', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b40d01-fc82-4775-829f-d6648a0f85a9', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('c5b40d01-fc82-4775-829f-d6648a0f85a9', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('c5b40d01-fc82-4775-829f-d6648a0f85a9', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('c5b40d01-fc82-4775-829f-d6648a0f85a9', secondary, hermeneutical_control_is_real_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_real_authority, holdable).
narrative_ontology:cs_axiom_grounding('c5b40d01-fc82-4775-829f-d6648a0f85a9', hermeneutical_control_is_real_authority, conventional).
narrative_ontology:cs_reference_frame('c5b40d01-fc82-4775-829f-d6648a0f85a9', conciliar_unity_through_ambiguity).
narrative_ontology:cs_drift_state('c5b40d01-fc82-4775-829f-d6648a0f85a9', post_conciliar_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5b40d01-fc82-4775-829f-d6648a0f85a9', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_episcopate).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_episcopate).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_churches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_churches).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_hermeneutic_of_continuity_and_rupture).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_interpretive_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the official interpretation of conciliar texts through the Congregation for the Doctrine of the Faith and papal magisterium. Sets the hermeneutical framework that determines which ambiguities resolve toward continuity and which toward rupture. Extracts authority from the very ambiguity the texts encode.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, generational, arbitrage, universal).

% Secured their ecclesiological vision (collegiality, religious liberty, ecumenism) encoded in conciliar texts through strategic ambiguity. Benefits from the hermeneutical space that allows progressive implementation while claiming continuity. Depends on curial goodwill for authoritative confirmation of their reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_episcopate, beneficiary,
    organized, biographical, constrained, global).

% Voted against key texts (10-12% rejection) but remain bound by the conciliar corpus. Their dissenting votes signal unresolved theological incompatibility. Experience the constraint as extraction: their episcopal authority is subordinated to a hermeneutic they reject, with exit blocked by identity-fusion with their office and ordination.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_episcopate, payer,
    organized, biographical, identity_locked, global).

% Subject to liturgical, catechetical, and governance implementations they did not choose and cannot influence. The composite texts authorize contradictory pastoral practices (e.g., vernacular vs. Latin, communion posture, ecumenical participation) depending on local bishop's hermeneutical alignment. No formal voice in interpretive disputes.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity, payer,
    powerless, biographical, constrained, universal).

% Bear implementation costs of divergent readings (parallel liturgical forms, conflicting catechesis, ecumenical confusion). Benefit from the collegiality doctrine that elevates their status, but this same doctrine is the vector for hermeneutical centralization. Caught between subsidiarity claims and roman interpretive monopoly.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_churches, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_churches, beneficiary).

% Produce the interpretive literature that fills the ambiguity. Their work is the raw material for magisterial decisions but they hold no authoritative vote. Some achieve influence through curial appointments; most remain external analysts of a constraint they document but do not control.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theologians, observer,
    moderate, biographical, mobile, global).

% Ecumenical dialogue partners whose engagement terms are set by the Catholic hermeneutic of the Council. The composite texts simultaneously invite dialogue (Nostra Aetate, Unitatis Redintegratio) and assert Catholic structural supremacy (Lumen Gentium 8). Would object to being objects of a unity they cannot co-author but are structurally excluded from the conciliar hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, eastern_orthodox_and_protestants, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieved conciliar unity (88-90% supermajority) across irreconcilable ecclesiological visions — juridical/institutional vs. communion/sacramental, papal primacy vs. episcopal collegiality, continuity vs. aggiornamento — by encoding both in ambiguous formulations that each faction could ratify.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar texts themselves to the living magisterium. The texts' deliberate ambiguity makes them hermeneutically inert without an authoritative interpreter; the magisterium becomes the real locus of authority by monopolizing the resolution of ambiguities it helped design.
% ABSENT_VOICES: Sedevacantist and conclavist groups (reject the Council's legitimacy entirely), radical reformers (wanted rupture made explicit), the global laity (no formal conciliar representation), and persecuted Churches behind Iron Curtain (represented by proxies). These voices would object to the compromise architecture but were excluded from the voting hall.
% DISAPPEARANCE_RATIONALE: If the composite constraint vanished overnight, the post-conciliar Church would lose its foundational unity mechanism. The 1962-65 consensus would fracture into at least three bodies: a continuity-only Church, a rupture-affirming Church, and a confused middle. The current single magisterium could not survive the explicit choice between incompatible ecclesiologies the texts currently conceal.
% FOUNDING_PROBLEM: Post-war Catholic unity threatened by: (1) theological polarization between ressourcement and neo-scholastic camps, (2) existential challenge of modernity (secularism, religious liberty, ecumenism), (3) curial centralization vs. episcopal collegiality tension, (4) need for a Council that would not schism the Church as Vatican I nearly did.
% FOUNDING_PROBLEM_CORROBORATION: Congar and de Lubac (ressourcement architects) attest the problem was real and the composite solution intentional. Ratzinger (as peritus and later prefect) attests the founding problem persists in post-conciliar fragmentation. Traditionalist bishops (Lefebvre, Castro Mayer) attested at the time that the problem was manufactured to force rupture. The 10-12% rejection votes themselves corroborate that a significant minority saw no solvable founding problem — only a trap.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the magisterium extracts interpretive monopoly from ambiguity it did not merely inherit but (on this reading) the conciliar architecture deliberately created. Suppression (0.55) is moderate: dissent is not crushed but channeled — traditionalists remain in communion but subordinated; progressives get implementation but not textual clarity. Theater (0.42) rises over time: the conciliar texts become ceremonial authorities invoked by all sides while real governance flows through curial instructions and papal acts that resolve ambiguities. Accessibility collapse (0.62) reflects that exit from the hermeneutic requires leaving the Church or adopting a schismatic identity. Resistance (0.48) is significant but fragmented: traditionalist resistance is identity-locked; progressive resistance takes the form of pushing ambiguities further.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Curia) experiences the constraint as genuine coordination: it built the interpretive framework that holds the Church together. The payer seats (traditionalist bishops, laity) experience it as enforced extraction: they are bound by a hermeneutic they did not choose and cannot escape. The beneficiary seat (progressive episcopate) experiences it as coordinated extraction: they benefit from the ambiguity but pay the price of curial dependence. The engine computes this divergence from the structural data — the claimed tangled_rope type reflects the structural reality that coordination and extraction are co-extensive in the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Roman Curia sits at d≈0.15 (full beneficiary): controls the interpretive monopoly, sets the agenda, has arbitrage-grade exit (can reform the Curia itself). Progressive episcopate at d≈0.35 (net beneficiary but constrained): gains hermeneutical space but depends on curial confirmation. Traditionalist episcopate at d≈0.85 (near full target): bound by texts they rejected, identity-locked by ordination and office. Laity at d≈0.75 (target): subject to implementations they cannot influence, constrained exit (social/identity costs). Local churches at d≈0.55 (near symmetric): gain collegial status but lose interpretive autonomy. Theologians at d≈0.5 (analytical): mobile, neither collecting nor paying. Eastern Orthodox/Protestants at d≈0.9 (excluded targets): trapped in dialogue terms they cannot co-author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war unity + modernity engagement) is contested: the Curia claims it remains live (secularism intensifies); traditionalists claim it was a pretext for rupture; progressives claim it was solved but the solution requires ongoing hermeneutical vigilance. The composite architecture prevents mandatrophy resolution by design: if the founding problem were declared dead, the ambiguity would lose its justification; if declared live, the ambiguity must persist. The constraint is a tangled_rope precisely because it cannot resolve its founding problem without destroying its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_ambiguity,
    'Was the ambiguity in conciliar texts a deliberate design choice by conciliar architects (Congar, Rahner, Ratzinger, Wojtyla) to secure supermajorities, or an emergent property of genuine theological convergence?',
    'Comparative analysis of conciliar preparatory schemata vs. final texts, periti correspondence, and voting records per schema. If key ambiguities appear in final texts but not schemata, and correlate with voting coalitions, intentionality is established.',
    'If deliberate, the composite is a designed coordination mechanism with built-in extraction (hermeneutical monopoly). If emergent, the extraction is post-hoc capture of genuine theological tension. Changes the constraint''s genealogical classification from designed_tangled_rope to evolved_tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_ambiguity, empirical, 'Whether the overdetermination was engineered or emerged.').

omega_variable(
    hermeneutical_monopoly_legitimacy,
    'Does the magisterium''s interpretive monopoly derive from legitimate succession (lineage grounding) or from the structural necessity of resolving ambiguities the magisterium itself helped encode (extraction grounding)?',
    'Genealogical analysis of post-conciliar magisterial acts: when the magisterium resolves an ambiguity, does it appeal to pre-conciliar tradition (lineage) or to the Council''s own ''spirit'' (extraction)? Track citation patterns in CDF documents 1965-present.',
    'If lineage, the constraint tends toward rope (coordination with legitimate authority). If extraction, it is a snare masquerading as tangled_rope. The current 0.68 extractiveness suggests extraction grounding dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_monopoly_legitimacy, conceptual, 'Grounding type of the interpretive monopoly — lineage vs. extraction.').

omega_variable(
    rejection_votes_signal,
    'Do the 10-12% rejection votes on key schemas represent principled theological dissent (incompatibility) or procedural protest (insufficient consultation, haste)?',
    'Content analysis of interventiones (oral interventions) and modi (written amendments) by rejecting bishops. Code for: theological objection vs. process objection. Cross-reference with post-conciliar trajectories of rejecting bishops (schism, silence, integration).',
    'If principled, the incompatibility is theological and the composite is structurally unstable. If procedural, the composite could have achieved higher consensus with better process — the extraction is contingent, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rejection_votes_signal, empirical, 'Nature of the conciliar dissent signal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2_comp_overdet_tr_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(v2_comp_overdet_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(v2_comp_overdet_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(v2_comp_overdet_tr_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(v2_comp_overdet_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(v2_comp_overdet_tr_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(v2_comp_overdet_tr_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(v2_comp_overdet_tr_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(v2_comp_overdet_be_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.25).
narrative_ontology:measurement(v2_comp_overdet_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(v2_comp_overdet_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(v2_comp_overdet_be_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(v2_comp_overdet_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(v2_comp_overdet_be_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(v2_comp_overdet_be_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(v2_comp_overdet_be_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(v2_comp_overdet_su_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(v2_comp_overdet_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(v2_comp_overdet_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(v2_comp_overdet_su_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(v2_comp_overdet_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(v2_comp_overdet_su_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(v2_comp_overdet_su_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(v2_comp_overdet_su_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_collegiality_implementation).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecumenical_dialogue_terms).

% DUAL FORMULATION NOTE:
% This constraint is one member of the vatican_ii_magisterial_authority kernel family. The continuity_reading claims the Council is a mountain (organic development, negligible extraction). The rupture_reading claims it is a snare (fundamental break, high extraction). This reading claims tangled_rope: the texts coordinate unity through overdetermination but extract hermeneutical monopoly. The three readings have different ε values (continuity ~0.15, rupture ~0.75, composite ~0.68) because they identify different standing arrangements as the referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
