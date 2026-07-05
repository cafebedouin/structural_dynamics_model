% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Mechanism — Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the legitimacy-erosion reading of the contested
 *   ietf_openness_commitment kernel. It is not a story about whether IETF
 *   standards serve the public (that is commons_stewardship_reading) or about
 *   resource asymmetry translating directly into technical gatekeeping (that
 *   is capture_substrate_reading). This reading's specific claim is narrower
 *   and structurally distinct: the rough-consensus mechanism ITSELF — the
 *   procedural device of judging when objections have been substantively
 *   addressed — is the thing being extracted from. Well-resourced factions do
 *   not merely win technical arguments; they extract the mechanism's
 *   legitimacy, spending down a finite commons of perceived neutrality to
 *   launder self-serving outcomes as consensus. The victim named here is the
 *   credibility of the mechanism, a subtly different object than the
 *   standards' technical content or the excluded implementers' direct costs
 *   (though those overlap).
 *
 * KEY AGENTS:
 *   - well_resourced_vendor_coalitions: primary beneficiary/agenda_setter (organized/arbitrage) — extracts legitimacy through sustained attendance and coordinated positioning
 *   - incumbent_working_group_chairs: agenda_setter (institutional/identity_locked) — professionally and identity-bound to declaring consensus, reluctant to rule against organized presence
 *   - independent_engineers: primary target (powerless/trapped) — cannot sustain the multi-year presence the mechanism now effectively requires
 *   - underfunded_implementers: secondary target (moderate/constrained) — absorb re-engineering costs from pre-negotiated outcomes
 *   - the_consensus_mechanism_itself: the diffuse, non-agent victim — its credibility is the actual resource being depleted
 *   - ietf_leadership_bodies: observer/agenda_setter (institutional/analytical) — see the pattern but are reluctant to formalize an informal norm
 *   - excluded_regional_and_civil_society_voices: excluded — structurally never present when calls are made
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.52).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Mechanism — Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '36a82511-e7e6-4c47-9ee2-3c8acb3b305a').
narrative_ontology:cs_kernel_codification('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', implicit).
narrative_ontology:cs_authority_grounding('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', practice).
narrative_ontology:cs_interpretation_layer_present('36a82511-e7e6-4c47-9ee2-3c8acb3b305a').
narrative_ontology:cs_reading_relation('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', foundational, consensus_legitimacy_is_a_depletable_commons).
narrative_ontology:cs_axiom_status(consensus_legitimacy_is_a_depletable_commons, holdable).
narrative_ontology:cs_axiom_grounding('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', consensus_legitimacy_is_a_depletable_commons, empirically_contingent).
narrative_ontology:cs_axiom('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', secondary, procedural_safeguards_are_insufficient_absent_resource_parity).
narrative_ontology:cs_axiom_status(procedural_safeguards_are_insufficient_absent_resource_parity, holdable).
narrative_ontology:cs_axiom_grounding('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', procedural_safeguards_are_insufficient_absent_resource_parity, empirically_contingent).
narrative_ontology:cs_reference_frame('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', engineering_meritocracy_convergence).
narrative_ontology:cs_drift_state('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', post_2010_vendor_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36a82511-e7e6-4c47-9ee2-3c8acb3b305a', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_working_group_chairs).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_engineers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send multiple full-time paid engineers to sustain presence across mailing lists, interim calls, and in-person meetings over years. Can outlast opposition through sheer attendance stamina, coordinate positions privately before meetings, and declare or contest 'rough consensus' calls from working group chairs drawn from their own ranks. Extract ratification of designs that favor their existing deployments while the process records the outcome as neutral technical agreement.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, agenda_setter).

% Hold the formal authority to judge when rough consensus exists, a judgment call with no bright-line test. Their professional identity and standing in the standards community are built on being seen as fair arbiters, which makes them reluctant to rule against the loudest, best-organized faction even when it correlates with resourcing rather than technical merit. Their continued chairship depends on not alienating the vendors who fund travel and staff time.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_working_group_chairs, agenda_setter,
    institutional, biographical, identity_locked, global).

% Participate as volunteers or with minimal employer support, unable to sustain presence across the multi-year timelines that determine outcomes. Their objections are procedurally solicited but practically unable to compete with organized attendance; they either accept outcomes decided while they were absent from a call, or exit the process entirely, ceding the field.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_engineers, payer,
    powerless, immediate, trapped, global).

% Smaller companies and open-source projects that must implement whatever standard emerges. When rough consensus ratifies a design shaped by incumbent deployment patterns, they bear disproportionate re-engineering cost to comply with a 'consensus' that was substantially pre-negotiated among larger players before it reached the floor.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_implementers, payer,
    moderate, biographical, constrained, global).

% The procedural device of 'rough consensus, running code' depends entirely on being perceived as legitimate — a judgment not reducible to vote-counting. Every instance where the hum favors whoever sent the most people erodes the credibility that gives the mechanism its coordinating force. The mechanism cannot defend itself; its erosion is diffuse and cumulative rather than any single actor's declared harm.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself).

% The IESG and IAB oversee the process, hear appeals, and could reform consensus-determination procedures. They observe capture patterns in aggregate across working groups but have limited appetite to formalize what has deliberately been kept informal, since formalization would itself require the kind of adversarial procedural fight the informal norm was designed to avoid.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership_bodies, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership_bodies, agenda_setter).

% Users in regions and communities without vendor sponsorship rarely attend at all; their interests in privacy, accessibility, or non-commercial use cases are represented, if at all, secondhand by sympathetic insiders. They would object to specific technical tradeoffs made in their absence but are structurally never in the room where rough consensus is declared.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, excluded_regional_and_civil_society_voices, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rough consensus avoids the gridlock of formal voting and the capture risk of a single controlling authority by asking working groups to converge on technically sound designs through iterative argument and running code, with chairs judging when objections have been substantively addressed rather than counting heads.
% TRANSFER_FUNCTION: Moves the power to shape global technical standards from whoever has the strongest argument to whoever can sustain the longest, best-coordinated organizational presence — transferring de facto standard-setting authority from distributed technical merit toward concentrated institutional resourcing, while the credibility of 'this was rough consensus' is spent as legitimating currency for outcomes that were substantially pre-decided.
% ABSENT_VOICES: Underfunded implementers, independent engineers without employer sponsorship, and civil-society or regional voices without vendor backing would object to specific consensus calls but are not present in sufficient numbers or duration to contest the chair's read of the room; their absence is structural, not chosen.
% DISAPPEARANCE_RATIONALE: Vendors currently benefiting from favorable consensus calls would say the process would collapse into paralysis or naked voting-bloc warfare without rough consensus as a legitimating frame. Critics would say what would actually disappear is a fig leaf — the same resource asymmetries would simply operate more visibly through formal voting, and independent engineers might be marginally better off knowing exactly where they stand rather than being told a captured outcome was neutral technical agreement.
% FOUNDING_PROBLEM: Early internet standards work needed a way to converge on interoperable technical designs quickly, without the procedural warfare of formal parliamentary voting or the capture risk of a single standards body controlled by any one government or vendor — 'rough consensus and running code' was built to reward working code and technical argument over political maneuvering.
% FOUNDING_PROBLEM_CORROBORATION: Long-time IETF participants and organizational historians (including retrospective accounts from former IAB/IESG members) attest that the original problem — avoiding capture by a single controlling party — was real and the mechanism initially served it. Academic studies of internet governance (outside any vendor's employ) and complaints documented in working-group appeal records from underfunded and independent participants corroborate that the mechanism now often functions to launder majority-resourcing outcomes as neutral technical consensus, a shift the vendors who benefit do not themselves attest to.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) targets the legitimacy commons specifically, not merely technical outcomes — this is why it is authored high despite the process having genuine procedural safeguards on paper. Suppression (0.52) is moderate: there is no formal barrier to participation, but sustained organizational presence functions as an effective informal filter. Theater ratio (0.58) is authored high and rising because an increasing share of consensus-declaration activity performs neutral technical deliberation while substantively ratifying outcomes settled through attendance endurance and pre-meeting coordination — this is the Goodhart signature of a coordination mechanism converted into a legitimation instrument. Accessibility collapse (0.4) is moderate, not high: alternative venues (competing standards bodies, de facto standards via running code outside IETF) still exist, so the collapse is partial. Resistance (0.61) is substantial: appeals, working-group objections, and academic critique of the process are active and ongoing, which is itself evidence this is not settled capture but a contested, live erosion.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced vendor coalitions sit near the full-beneficiary end: they extract validated legitimacy for outcomes they substantially pre-negotiate, at low marginal cost given existing staff allocation. Incumbent chairs are beneficiaries of continued institutional standing but are identity-locked rather than cleanly extractive — their professional self-concept is bound to being seen as neutral arbiters, which is precisely the mechanism vendors are extracting from, making the chairs simultaneously an instrument of and a hostage to the erosion. Independent engineers and underfunded implementers sit near the full-target end: trapped or constrained exit, bearing costs from decisions substantively made in venues they cannot sustain presence in. The consensus mechanism itself is marked as a non-agent payer (agent: false) — it collects no benefit and cannot act, but its credibility is the structural resource being depleted, which is the defining feature that separates this reading from the sibling readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding capture by any single controlling party through informal, merit-based convergence — is genuinely contested as live vs. dead rather than cleanly resolved either way. Corroboration from outside the benefiting vendor coalitions (academic governance studies, appeal records from marginalized participants, retrospective historian accounts) supports the reading that the mechanism has been substantially repurposed even as it retains its original justifying language. Classifying this as tangled_rope rather than snare preserves the fact that rough consensus still does real coordination work in many working groups; the erosion is real but not (yet) total, which is why extractiveness is authored at 0.68 rather than near-ceiling, and why resistance remains substantial rather than extinguished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_vs_output_capture_boundary,
    'Is the erosion of rough-consensus legitimacy a distinct phenomenon from capture of the standards themselves (capture_substrate_reading), or are these the same underlying capture observed through two different lenses?',
    'Track cases where the declared consensus outcome was later reversed or substantially revised after appeal — if legitimacy erosion and output capture co-vary tightly across cases, they may be one phenomenon; if working groups show output capture without legitimacy complaints (or vice versa), they are structurally separable.',
    'If inseparable, this reading and capture_substrate_reading should be merged rather than treated as siblings; if separable, the current three-way kernel decomposition is justified and each reading tracks real independent variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_vs_output_capture_boundary, conceptual, 'Whether legitimacy-mechanism erosion is analytically distinct from standards-output capture.').

omega_variable(
    chair_agency_vs_structural_capture,
    'To what extent do individual working-group chairs have genuine discretion to resist organized-presence pressure, versus being structurally compelled by the incentive environment regardless of individual integrity?',
    'Comparative study of consensus-call outcomes across chairs with varying tenure, employer independence, and community standing — if outcomes vary substantially by chair identity, individual agency matters; if outcomes are uniform regardless of who chairs, the structure fully determines the result.',
    'If chair agency matters significantly, targeted chair-selection or term-limit reforms could meaningfully address the erosion without restructuring the whole mechanism; if structurally determined, only structural reform (e.g., formalized objection-weighting) would help.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chair_agency_vs_structural_capture, empirical, 'Whether individual chair discretion or structural incentive fully explains capture-favoring consensus calls.').

omega_variable(
    informal_vs_formalized_reform_tradeoff,
    'Would formalizing rough-consensus determination (explicit criteria, appealable scoring) reduce capture, or would it simply relocate the capture into gaming the formal criteria while destroying the mechanism''s original flexibility advantage?',
    'Comparative analysis of standards bodies that use formal voting (ISO, W3C director-based models) versus IETF''s informal consensus, measured for relative capture rates and participant satisfaction among under-resourced parties.',
    'If formalization reduces net capture, IETF leadership has a concrete reform path; if formalization merely shifts the extraction target, the erosion is closer to an irreducible feature of any large-scale voluntary standards process, not a fixable defect of this particular mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_vs_formalized_reform_tradeoff, preference, 'Whether procedural formalization is a genuine remedy or merely relocates the capture dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 16, 0.49).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ietf_openness_commitment kernel. commons_stewardship_reading models the same institution as public-good interoperability infrastructure; capture_substrate_reading models it as a coordination substrate whose technical OUTPUTS are captured by resource advantage. This story isolates the narrower claim that the consensus-determination MECHANISM's own legitimacy is the extraction target — a commons of trust distinct from (but causally linked to) both the public-good function and the technical output.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
