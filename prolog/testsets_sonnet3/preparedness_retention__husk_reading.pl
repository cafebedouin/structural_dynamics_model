% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Flood Preparedness as Memorial Performance (Husk Reading)
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_retention
 *   kernel: the claim that flood defense drills and inspections have decayed
 *   into memorial performance — activity that produces the FEELING of
 *   retained competence (paperwork, ceremony, certification statistics)
 *   without the underlying tacit skill needed to respond to a genuine
 *   compound-failure D5 event. The founding coordination problem (verified
 *   readiness after a historical catastrophe) is treated, in this reading, as
 *   substantially decoupled from the apparatus that now claims to solve it.
 *   The apparatus still performs a coordination function — it schedules
 *   scarce specialist attention and produces auditable paper trails — but the
 *   metrics track visible compliance far more than they track live
 *   operational capacity, and the gap between the two has widened over the
 *   forty-year interval as the last generation with direct catastrophe
 *   experience has retired.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.52).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Flood Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, '94351dfb-6ed1-4558-aab9-dc1beb306390').
narrative_ontology:cs_kernel_codification('94351dfb-6ed1-4558-aab9-dc1beb306390', distributed).
narrative_ontology:cs_authority_grounding('94351dfb-6ed1-4558-aab9-dc1beb306390', practice).
narrative_ontology:cs_interpretation_layer_present('94351dfb-6ed1-4558-aab9-dc1beb306390').
narrative_ontology:cs_reading_relation('94351dfb-6ed1-4558-aab9-dc1beb306390', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('94351dfb-6ed1-4558-aab9-dc1beb306390', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('94351dfb-6ed1-4558-aab9-dc1beb306390', foundational, ceremonial_performance_displaces_tacit_skill).
narrative_ontology:cs_axiom_status(ceremonial_performance_displaces_tacit_skill, holdable).
narrative_ontology:cs_axiom_grounding('94351dfb-6ed1-4558-aab9-dc1beb306390', ceremonial_performance_displaces_tacit_skill, empirically_contingent).
narrative_ontology:cs_axiom('94351dfb-6ed1-4558-aab9-dc1beb306390', secondary, certification_signal_decouples_from_operational_capacity_over_generational_time).
narrative_ontology:cs_axiom_status(certification_signal_decouples_from_operational_capacity_over_generational_time, holdable).
narrative_ontology:cs_axiom_grounding('94351dfb-6ed1-4558-aab9-dc1beb306390', certification_signal_decouples_from_operational_capacity_over_generational_time, empirically_contingent).
narrative_ontology:cs_reference_frame('94351dfb-6ed1-4558-aab9-dc1beb306390', post_catastrophe_verified_readiness_mandate).
narrative_ontology:cs_drift_state('94351dfb-6ed1-4558-aab9-dc1beb306390', contemporary_post_last_survivor_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94351dfb-6ed1-4558-aab9-dc1beb306390', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, civil_defense_agency_leadership).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, municipal_certifying_authorities).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, coastal_residents).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, national_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the annual drill and inspection calendar, sets the certification criteria that municipalities must satisfy, and reports compliance rates upward to legislators and the public as evidence of readiness. Its budget, reputation, and continued mandate depend on the appearance of a well-drilled system, not on the system's performance during an actual D5-magnitude event, which has not occurred within any current leader's tenure.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, civil_defense_agency_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, civil_defense_agency_leadership, beneficiary).

% Administers local drills to satisfy the national checklist, signs off on inspection paperwork, and receives funding tied to certification status rather than to measured response competence. Career incentives reward smooth, incident-free drill days; surfacing real gaps in tacit skill risks funding cuts and reputational damage, so failure modes are quietly patched rather than reported.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, municipal_certifying_authorities, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, municipal_certifying_authorities, agenda_setter).

% Live inside the flood defense perimeter the drills are meant to protect. They receive periodic evacuation notices and reassurance that the system is 'certified ready,' but have no way to independently verify whether the certification reflects live operational competence or paperwork compliance. Their exit options are limited by housing cost, family ties, and the absence of credible alternative protection; they bear the full consequence if the performance is hollow.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, coastal_residents, payer,
    powerless, biographical, trapped, regional).

% Execute the drills as scripted and are evaluated on adherence to procedure rather than improvisational competence under novel failure conditions. Many privately report that real skills (rapid closure sequencing under partial equipment failure, coordinating across agencies during compound infrastructure loss) atrophy between the rare full-scale exercises, but raising this in the after-action review risks being read as insubordination or as jeopardizing the unit's certification score.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_emergency_responders, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, frontline_emergency_responders, excluded).

% Publish technical critiques arguing that current drill scenarios are simplified relative to plausible D5 compound-failure scenarios and that tacit skill decays faster than the certification cycle assumes. Their findings circulate in academic and engineering venues but are not incorporated into the certifying authority's pass/fail criteria, and they have no formal seat in the certification process.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, independent_flood_engineers, excluded,
    moderate, generational, constrained, national).

% Receives the agency's compliance statistics as the basis for continued appropriations and can point to high certification rates as evidence of due diligence discharged. Has limited independent capacity to audit whether certification tracks live competence and generally accepts the agency's self-reported metrics, which indirectly makes it a beneficiary of the performance it is nominally overseeing.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, national_legislature, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, national_legislature, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, civil_defense_agency_leadership).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a shared drill and inspection calendar coordinates scarce specialist attention (engineers, responders, equipment) across many municipalities so that readiness assessment happens on a predictable, comparable schedule rather than ad hoc.
% TRANSFER_FUNCTION: Moves public funds and political credibility from the state to the civil defense agency and municipal authorities in exchange for a certification signal; moves actual protective margin away from residents and responders, who receive reassurance rather than verified live capacity.
% ABSENT_VOICES: Independent flood engineers and frontline responders who privately doubt the certification's validity are structurally outside the sign-off process — their critiques appear in journals and informal debriefs but never enter the pass/fail criteria that determine funding and public messaging.
% DISAPPEARANCE_RATIONALE: If the drill-and-certification apparatus vanished overnight, the agency and municipalities argue infrastructure and legal defense preparedness would immediately erode (their claim: the ritual is load-bearing). Independent engineers and many responders argue the actual response capacity would barely change, since the ritual is already decoupled from live skill retention — the visible apparatus would disappear but the underlying competence gap, which already exists, would simply become visible instead of hidden. The verdict is genuinely contested between these two readings of the same events.
% FOUNDING_PROBLEM: After a catastrophic historical flood event, the state needed a mechanism to demonstrate to a traumatized public and to insurers/legislators that flood defense readiness was being maintained and verified on a recurring basis, rather than assumed.
% FOUNDING_PROBLEM_CORROBORATION: Independent flood engineers and academic emergency-management researchers, who sit outside the certifying and legislative beneficiary chain, attest that the original problem (verified live readiness) is no longer being solved by the current apparatus — their published post-drill analyses report simplified scenarios and untested compound-failure conditions. The civil defense agency and municipal authorities, who benefit from the certification signal, attest the problem remains live and well-served; no fully independent audit body with subpoena power over drill design has yet adjudicated between these two attestations.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio starts moderate (0.42) and rises to 0.81 by the end of the interval, reflecting the husk reading's core claim: as institutional memory of the original catastrophe fades, more of the apparatus's activity shifts toward producing certifiable signals (checklists, scripted drills, media-friendly demonstration exercises) and less toward exercises designed to surface and repair genuine capability gaps. Base extractiveness rises in parallel (0.38 to 0.68) because the apparatus increasingly extracts legitimacy and budget from a public that reasonably assumes certification implies capability. Suppression is moderate and rises slowly (0.30 to 0.52) — this is not a heavily coercive constraint; the suppression is mostly the quiet discouragement of dissenting after-action findings and the structural exclusion of independent critics from the certification criteria, rather than overt coercion of residents.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agency leadership and municipal certifying authorities sit near the beneficiary end: they collect budget, legitimacy, and career continuity from the certification signal, and their exit options (arbitrage/constrained but institutionally secure) buffer them from the consequences of a hollow performance. Coastal residents and frontline responders sit near the target end: residents are trapped by geography and housing cost and bear the full downside if the readiness signal is false; responders execute the ritual and privately bear the professional risk of naming its hollowness. Independent flood engineers are excluded rather than coordinated — their critique is structurally unable to reach the certification criteria, which is precisely what an omega below interrogates.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resists collapsing into a simple snare verdict because a genuine coordination function persists underneath the performance: someone must schedule specialist attention, and the checklist apparatus does produce SOME real information (equipment inventories, personnel rosters) even if it fails to test tacit skill under novel conditions. Classifying this as tangled_rope rather than pure snare honors that the drills are not merely theater from inception — they retain partial genuine function (the coordination half) while having drifted toward extraction of legitimacy that is not backed by tested capability (the extraction half). This is precisely the seat-divergence the engine should register: from the agency's seat, drills are functioning coordination; from the resident's seat, at the moment of a real D5 event, the same structure would be experienced as having extracted false assurance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_empirical_resolution,
    'Does live operational competence actually decay faster than the certification cycle assumes, or does the drill apparatus in fact preserve the tacit skills needed for a genuine D5 event, as the competence_reading claims?',
    'A full-scale, unscripted compound-failure exercise (multiple simultaneous infrastructure failures, no advance script, independent observers scoring improvisational performance rather than procedural adherence) would directly test whether tacit skill exists beneath the ceremonial layer. Absent such a test, the question remains genuinely open between readings.',
    'If competence is shown to be substantially retained, this story''s classification would be undermined in favor of the competence_reading or hybrid_reading; if the gap is confirmed empirically, the husk reading''s tangled_rope-with-rising-extraction trajectory would be strongly corroborated and could plausibly justify reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_empirical_resolution, empirical, 'Whether the husk reading''s core empirical claim (competence decay beneath ceremony) is true.').

omega_variable(
    committer_location_of_disagreement,
    'Where exactly does the husk reading''s disagreement with the hybrid_reading live — is it a disagreement about ALL preparedness institutions uniformly decaying, or does the husk reading implicitly concede that SOME specialized technical bodies (e.g., a national water authority) retain real competence while claiming the broader societal/municipal layer does not?',
    'A structural audit distinguishing specialized technical-institution competence from municipal/societal-level drill performance would locate whether the husk reading is a claim about the whole system or is actually a narrower claim that collapses into the hybrid_reading once decomposed by institution type.',
    'If the husk reading, examined closely, turns out to concede stratification, it may not be a fully distinct reading from hybrid_reading but a rhetorically stronger version of the same underlying structural claim — this would not change this story''s authored ε, but would change how the kernel''s three readings should be understood to relate to one another.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_location_of_disagreement, conceptual, 'Whether the husk reading is fully distinct from the hybrid reading or a stronger restatement of it.').

omega_variable(
    certification_capture_naturalness,
    'Is the divergence between certification signal and live competence an inevitable byproduct of any large-scale bureaucratic readiness system (a structural near-mountain), or is it a constructed, correctable feature of THIS specific certification design that benefits identifiable agents?',
    'Comparative study of preparedness systems in other domains (aviation safety, nuclear plant operations) that have historically achieved tighter coupling between certification and live competence, to establish whether the gap is a design choice or a structural inevitability of the scale involved.',
    'If comparable systems achieve tighter coupling, the gap here is a constructed feature serving the identified beneficiaries, supporting the tangled_rope/extractive reading; if no comparable system achieves tighter coupling at this scale, the gap may be closer to structurally inevitable, weakening the extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(certification_capture_naturalness, empirical, 'Whether the ceremony/competence gap is a constructed choice or a scale-inevitable feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.7).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__husk_reading, theater_ratio, 32, 0.77).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__husk_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__husk_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__husk_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__husk_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_retention kernel. competence_reading authors the same drill-and-inspection apparatus with low ε (genuine skill retention, rope-leaning); husk_reading (this story) authors it with substantial and rising ε (memorial performance decoupled from live competence, tangled_rope); hybrid_reading authors a stratified structure where specialized technical institutions retain competence while broader societal readiness becomes ceremonial (mixed ε across institutional strata). The three stories share the same underlying kernel text and drill calendar but diverge in beneficiary/victim structure and in whether the coordination function is judged intact. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
