% ============================================================================
% CONSTRAINT STORY: democratic_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_democratic_legitimacy_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: democratic_legitimacy_reading
 *   human_readable: Procedural Legitimacy Reading of Infrastructure-Siting Stability
 *   domain: political_economy/surveillance_studies/democratic_theory
 *
 * SUMMARY:
 *   This story instantiates the democratic-legitimacy reading of the
 *   stability-legitimacy kernel: stability around contested infrastructure
 *   siting (data centers, surveillance expansion) is legitimate to the degree
 *   it is procedurally grounded — real consent, real voice, real
 *   recallability of the decision-maker — rather than legitimate because it
 *   redistributes gains to the affected or because it deters resistance
 *   through coercion. The essay's two examples anchor the reading: the
 *   data-center protest shows communities exercising voice through the only
 *   channel offered (public comment, public assembly) and getting no binding
 *   response; the 'arrested for clapping' example shows the state treating a
 *   minimal, non-disruptive assertion of voice as a public-order violation,
 *   revealing that the procedural channel is not treated by the authorities
 *   as carrying real stakes. Under this reading, the constraint operates as a
 *   tangled rope: it has a genuine coordination function (a legible standard
 *   converts raw imposition into contestable, reviewable process) but is
 *   actively enforced to protect concentrated beneficiaries (developers,
 *   incumbent authorities) at the expense of host communities and dissidents
 *   who bear real costs without commensurate voice.
 *
 * KEY AGENTS:
 *   - incumbent_political_authorities: agenda_setter (institutional/arbitrage) — controls whether voice channels are binding
 *   - infrastructure_developers: beneficiary (powerful/mobile) — gains from low-cost, low-friction siting
 *   - data_center_host_towns: payer/excluded (powerless/trapped) — bears costs, denied binding voice
 *   - surveilled_dissidents: payer/excluded (powerless/constrained) — voice reclassified as security threat
 *   - disenfranchised_residents: excluded (powerless/trapped) — structurally absent from the procedure itself
 *   - civil_liberties_monitors: observer (organized/analytical) — documents the gap between form and substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(democratic_legitimacy_reading, 0.62).
domain_priors:suppression_score(democratic_legitimacy_reading, 0.68).
domain_priors:theater_ratio(democratic_legitimacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(democratic_legitimacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(democratic_legitimacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(democratic_legitimacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(democratic_legitimacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(democratic_legitimacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(democratic_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(democratic_legitimacy_reading, "Procedural Legitimacy Reading of Infrastructure-Siting Stability").
narrative_ontology:topic_domain(democratic_legitimacy_reading, "political_economy/surveillance_studies/democratic_theory").

domain_priors:requires_active_enforcement(democratic_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(democratic_legitimacy_reading, '053707ac-75db-4c92-8c92-56abdc1c06b7').
narrative_ontology:cs_kernel_codification('053707ac-75db-4c92-8c92-56abdc1c06b7', formalized).
narrative_ontology:cs_authority_grounding('053707ac-75db-4c92-8c92-56abdc1c06b7', practice).
narrative_ontology:cs_interpretation_layer_present('053707ac-75db-4c92-8c92-56abdc1c06b7').
narrative_ontology:cs_reading_relation('053707ac-75db-4c92-8c92-56abdc1c06b7', democratic_legitimacy_reading__redistributive_stabilization_reading, coexists_with).
narrative_ontology:cs_reading_relation('053707ac-75db-4c92-8c92-56abdc1c06b7', democratic_legitimacy_reading__repressive_stabilization_reading, influences).
narrative_ontology:cs_reading_relation('053707ac-75db-4c92-8c92-56abdc1c06b7', democratic_legitimacy_reading__collapse_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('053707ac-75db-4c92-8c92-56abdc1c06b7', foundational, legitimacy_requires_binding_recallable_voice).
narrative_ontology:cs_axiom_status(legitimacy_requires_binding_recallable_voice, holdable).
narrative_ontology:cs_axiom_grounding('053707ac-75db-4c92-8c92-56abdc1c06b7', legitimacy_requires_binding_recallable_voice, deontological).
narrative_ontology:cs_axiom('053707ac-75db-4c92-8c92-56abdc1c06b7', secondary, material_or_coercive_substitution_for_voice_is_illegitimate).
narrative_ontology:cs_axiom_status(material_or_coercive_substitution_for_voice_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('053707ac-75db-4c92-8c92-56abdc1c06b7', material_or_coercive_substitution_for_voice_is_illegitimate, conventional).
narrative_ontology:cs_reference_frame('053707ac-75db-4c92-8c92-56abdc1c06b7', binding_consultative_democracy).
narrative_ontology:cs_drift_state('053707ac-75db-4c92-8c92-56abdc1c06b7', contemporary_data_center_boom, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('053707ac-75db-4c92-8c92-56abdc1c06b7', '').
narrative_ontology:cs_kernel_id(democratic_legitimacy_reading, stability_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_reading, incumbent_political_authorities).
narrative_ontology:constraint_beneficiary(democratic_legitimacy_reading, infrastructure_developers).
narrative_ontology:constraint_victim(democratic_legitimacy_reading, data_center_host_towns).
narrative_ontology:constraint_victim(democratic_legitimacy_reading, surveilled_dissidents).
narrative_ontology:constraint_victim(democratic_legitimacy_reading, disenfranchised_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approve siting permits, set the terms under which affected communities may object, and control which grievances reach a recallable decision-maker versus which are diverted into consultation theater. They benefit from stability being read as procedurally legitimate because that reading requires no material transfer and licenses use of policing power against protest framed as disorder rather than as denied voice.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, incumbent_political_authorities, agenda_setter,
    institutional, biographical, arbitrage, national).

% Build and operate the data centers; they gain from a legitimacy standard that treats formal public-comment periods as sufficient consent, regardless of whether the comment record can actually change the siting decision. If the same infrastructure required binding local consent, that would raise their costs and timelines.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, infrastructure_developers, beneficiary,
    powerful, biographical, mobile, continental).

% Bear the water, land, noise, and land-use costs of hosting facilities they were notified about but could not actually block or renegotiate; local hearings occurred but no vote was recallable or binding. Their only visible recourse — public protest — was met with arrests, including for behavior as minor as clapping during a hearing, which is read by authorities as a public-order violation rather than as the voice-mechanism failing.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, data_center_host_towns, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(democratic_legitimacy_reading, data_center_host_towns, excluded).

% Organize or document opposition to siting and surveillance expansion; face monitoring, identification at protests, and selective prosecution. Their exclusion from voice is treated by authorities as a security matter rather than as evidence the procedural-consent channel has failed to carry their objection anywhere with teeth.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, surveilled_dissidents, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(democratic_legitimacy_reading, surveilled_dissidents, excluded).

% Lack standing in the formal comment process — non-citizens, renters without a recognized property interest, residents outside the narrowly drawn notification radius. They are structurally absent from the very procedure that is cited as proof of consent.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, disenfranchised_residents, excluded,
    powerless, generational, trapped, local).

% The formal apparatus — elections, recall petitions, judicial review of siting decisions — that would carry a democratic-legitimacy reading if it actually functioned as a binding check. Listed for completeness: this is the standard the reading measures against, not an actor with interests of its own.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, recallability_mechanisms, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(democratic_legitimacy_reading, recallability_mechanisms).

% Document arrest patterns at hearings and protests, track whether comment-period input demonstrably changed outcomes, and testify to whether the procedural-consent standard is met in substance or only in form.
narrative_ontology:constraint_stakeholder(democratic_legitimacy_reading, civil_liberties_monitors, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(democratic_legitimacy_reading, infrastructure_developers).
narrative_ontology:fixing_cost_class(democratic_legitimacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard for judging whether infrastructure siting and its enforcement are legitimate — consent obtained, voice exercised, decision-makers recallable — rather than leaving legitimacy to be asserted by whoever holds power.
% TRANSFER_FUNCTION: Moves the burden of proof for legitimacy from the state (show that consent was real and binding) onto affected communities (show that a formally-satisfied procedure was actually hollow) — a transfer of evidentiary and organizing cost from authorities to the people the siting decision falls on.
% ABSENT_VOICES: Disenfranchised residents outside the notification radius, non-citizen residents, and future residents (generational time horizon) are not represented in any comment period at all; surveilled dissidents are represented only as security risks in the record that does exist.
% DISAPPEARANCE_RATIONALE: Authorities would say nothing changes materially — permits still issue, facilities still get built — because the procedural veneer is not what actually determines siting outcomes. Host towns and civil liberties monitors would say the world rearranges: without even the fig leaf of a comment period, the arrests and permits would have to be justified on bare material or coercive grounds, exposing the redistribution-or-repression choice the procedural reading currently obscures.
% FOUNDING_PROBLEM: Post-industrial infrastructure siting (power plants, prisons, now data centers) repeatedly produced violent confrontations when imposed without any consultation; procedural consent mechanisms (hearings, comment periods, environmental review) were built to convert raw imposition into a legitimated, lower-conflict process.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning departments and developers attest the comment-period apparatus still functions as intended. Civil liberties monitors and academic democratic-theory literature (outside both the developer and the host-town parties) attest that in data-center siting specifically, comment is frequently advisory-only and unappealable, and that arrest of protesters for procedurally protected speech (the clapping arrests) is direct evidence the voice channel has been formally preserved but substantively hollowed.
narrative_ontology:disappearance_verdict(democratic_legitimacy_reading, contested).
narrative_ontology:founding_problem_status(democratic_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(democratic_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(democratic_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(democratic_legitimacy_reading, 0.62, 'claude-sonnet-5', 'surveillance_guillotines_2026_20260811_115130', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(democratic_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(democratic_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(democratic_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) and suppression (0.68) are both substantial but not maximal: some real consultative function persists (developers do sometimes alter plans in response to comment, some permits are contested successfully), which is why this is authored as tangled_rope rather than snare — the coordination function is not entirely fictional, but it is unevenly available and its enforcement (permitting law, protest policing) protects the asymmetry once it exists. Theater ratio (0.55) captures that comment periods increasingly function as documented process rather than as decision input; the clapping-arrest example is the clearest available signal that voice is protected in form (the right to attend a hearing) and punished in substance (any audible assertion of preference outside the sanctioned script). Accessibility collapse is moderate (0.5) — the formal channel exists and is nominally open, but its capacity to actually redirect outcomes has collapsed for host towns and dissidents even where the paperwork has not.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the arrangement looks like rope: a working, legitimate coordination mechanism that converts potential conflict into orderly process. From the host-town and dissident seats, the same structure computes closer to snare-adjacent tangled_rope: the coordination story is real enough to be citable in a legitimacy defense, but its actual function for them is to absorb objection without altering outcomes, while providing legal cover for policing genuine dissent (the clapping arrest) as disorder rather than as procedural failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent authorities and developers derive low d: they set and administer the standard and collect the low-friction siting outcome it produces. Host towns, dissidents, and disenfranchised residents derive high d: they are trapped or constrained, bear the costs, and are precisely the parties whose voice is formally solicited and substantively ignored. The exit gradient (arbitrage/mobile for beneficiaries vs. trapped/constrained for the excluded set) is the clearest structural marker distinguishing this reading's victim set from the redistributive reading's (material deprivation) or the repressive reading's (direct coercive target set) — here the defining harm is denial of a working voice mechanism, which is why data_center_host_towns are payers via exclusion rather than via direct financial extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — raw, non-consultative imposition producing violent siting conflicts — is only partially dead. Where comment periods are binding and recallable decision-makers exist, the arrangement still solves a live problem and should not be read as pure extraction. Where comment periods are advisory-only and enforcement targets the expression of voice itself, the founding problem has been formally 'solved' while its substance re-emerges as unaddressed grievance, and the arrangement has drifted toward serving as legitimation theater for outcomes that were going to happen regardless of the comment record. The tangled_rope classification, rather than snare, preserves the possibility that reform (making the channel actually binding) restores the original coordination function without requiring wholesale abolition — which is precisely the reading's structural point against both the redistributive-bribe and repressive-deterrent alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_versus_material_reading,
    'Is the legitimacy deficit in data-center siting genuinely procedural (the comment/hearing channel exists but is non-binding) or is it better read as material deprivation dressed in procedural language (host towns object because of water/land costs, and the procedural framing is secondary)?',
    'Compare outcomes across siting disputes: if communities with binding material compensation (payment-in-lieu, revenue sharing) show the same protest/arrest patterns as communities without compensation but with genuine binding voice, the procedural reading is vindicated; if compensation alone resolves conflict without any change to voice mechanisms, the redistributive reading better fits the data.',
    'If material compensation alone resolves the conflict, this story''s claimed_type and victim set should shift toward the redistributive_stabilization_reading''s structure rather than this reading''s procedural-exclusion structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_versus_material_reading, conceptual, 'Whether the observed conflict is best explained by procedural exclusion or by unaddressed material cost, per the kernel''s reading-choice.').

omega_variable(
    clapping_arrest_signal_strength,
    'How representative is the ''arrested for clapping'' example of systematic enforcement policy versus an isolated overreach by a specific officer or jurisdiction?',
    'Aggregate arrest-pattern data across multiple hearings and jurisdictions from civil liberties monitoring organizations; a pattern of low-disruption expressive-conduct arrests at siting hearings, versus a single anomalous incident, changes the strength of the inference to a systemic voice-suppression finding.',
    'If isolated, the suppression metric (0.68) may be overstated for the broader class of siting disputes; if systemic, it may be understated, and the theater_ratio trajectory should be read as an early-stage signal of accelerating drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clapping_arrest_signal_strength, empirical, 'Whether the anchoring example generalizes to a systemic enforcement pattern or represents an outlier.').

omega_variable(
    cs_framing_underdetermination_kernel_vs_hearing,
    'Is the correct commitment-system framing the siting-hearing procedure itself (a formalized kernel administered by planning authorities) or the broader constitutional/legal doctrine of due process and public participation that the hearing procedure claims to instantiate (a fixed-text kernel administered through judicial review)?',
    'Trace whether host-town legal challenges succeed by arguing procedural defects in a specific hearing (kernel = the hearing procedure) or by arguing the hearing procedure itself violates a higher due-process standard (kernel = constitutional doctrine); litigation outcomes reveal which framing courts actually apply.',
    'If the higher due-process kernel is operative, this constraint''s cs_structure authority_grounding might better be characterized as lineage (judicial doctrine) rather than the extraction/practice mix implied by treating the hearing procedure as the kernel; the axiom set and reading_relations would need revision accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination_kernel_vs_hearing, conceptual, 'Alternative framings of which kernel is actually under contest — the local hearing procedure or the constitutional due-process doctrine above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_tr_t0, democratic_legitimacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(demo_tr_t4, democratic_legitimacy_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(demo_tr_t8, democratic_legitimacy_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(demo_tr_t12, democratic_legitimacy_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(demo_tr_t16, democratic_legitimacy_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(demo_tr_t20, democratic_legitimacy_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement(demo_tr_t24, democratic_legitimacy_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(demo_be_t0, democratic_legitimacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(demo_be_t4, democratic_legitimacy_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(demo_be_t8, democratic_legitimacy_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(demo_be_t12, democratic_legitimacy_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(demo_be_t16, democratic_legitimacy_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(demo_be_t20, democratic_legitimacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(demo_be_t24, democratic_legitimacy_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(demo_su_t0, democratic_legitimacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(demo_su_t4, democratic_legitimacy_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(demo_su_t8, democratic_legitimacy_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(demo_su_t12, democratic_legitimacy_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(demo_su_t16, democratic_legitimacy_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(demo_su_t20, democratic_legitimacy_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(demo_su_t24, democratic_legitimacy_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(democratic_legitimacy_reading, redistributive_stabilization_reading).
narrative_ontology:affects_constraint(democratic_legitimacy_reading, repressive_stabilization_reading).
narrative_ontology:affects_constraint(democratic_legitimacy_reading, collapse_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the stability_legitimacy_kernel: the same underlying phenomenon (contested infrastructure siting stability) read through procedural (this story), redistributive, repressive, and collapse-inevitability lenses. Each reading authors its own beneficiary/victim structure and its own epsilon over the standing arrangement as its own lights see it. This reading's distinguishing structural feature is that the victim set is defined by voice-exclusion, not material shortfall or direct coercive targeting — data_center_host_towns and surveilled_dissidents are harmed by the absence of a binding voice mechanism even where material compensation or coercive suppression is not the primary lens.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
