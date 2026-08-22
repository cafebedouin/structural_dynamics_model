% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Study as Readiness-Maintenance for Deferred Sacrificial Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This story instantiates the messianic-deferral reading of the Kodashim
 *   commandment-status kernel: the sacrificial commandments are not abrogated
 *   but temporally suspended pending Temple restoration, and their intensive
 *   study functions as readiness-maintenance for that future contingency
 *   rather than as either present fulfillment (the study_as_performance
 *   reading) or acknowledgment of dead husk law (the performance_only
 *   reading). Over roughly two millennia of diaspora, this reading has
 *   coexisted with and structured a substantial institutional apparatus —
 *   yeshiva curricula, scholarly specialization, communal doctrine — whose
 *   legitimacy depends on restoration remaining a live future possibility
 *   rather than either an accomplished present or an abandoned past.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.38).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Study as Readiness-Maintenance for Deferred Sacrificial Commandment").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '88b775a0-201b-4a97-9431-fd5cdb6cf49e').
narrative_ontology:cs_kernel_codification('88b775a0-201b-4a97-9431-fd5cdb6cf49e', fixed_text).
narrative_ontology:cs_authority_grounding('88b775a0-201b-4a97-9431-fd5cdb6cf49e', lineage).
narrative_ontology:cs_interpretation_layer_present('88b775a0-201b-4a97-9431-fd5cdb6cf49e').
narrative_ontology:cs_reading_relation('88b775a0-201b-4a97-9431-fd5cdb6cf49e', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('88b775a0-201b-4a97-9431-fd5cdb6cf49e', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('88b775a0-201b-4a97-9431-fd5cdb6cf49e', foundational, commandment_retains_full_normative_force_in_suspension).
narrative_ontology:cs_axiom_status(commandment_retains_full_normative_force_in_suspension, holdable).
narrative_ontology:cs_axiom_grounding('88b775a0-201b-4a97-9431-fd5cdb6cf49e', commandment_retains_full_normative_force_in_suspension, deontological).
narrative_ontology:cs_axiom('88b775a0-201b-4a97-9431-fd5cdb6cf49e', foundational, study_constitutes_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('88b775a0-201b-4a97-9431-fd5cdb6cf49e', study_constitutes_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('88b775a0-201b-4a97-9431-fd5cdb6cf49e', temple_era_sacrificial_normativity).
narrative_ontology:cs_drift_state('88b775a0-201b-4a97-9431-fd5cdb6cf49e', contemporary_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88b775a0-201b-4a97-9431-fd5cdb6cf49e', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, kodashim_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_practical_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, students_directed_away_from_applied_halakha).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, communities_lacking_temple_infrastructure).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, temple_will_be_rebuilt_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, torah_eternality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curricular priority for Kodashim study, allocates teaching positions and prestige to scholars of sacrificial law, and frames the tractates as maintaining readiness for restoration. Draws funding, enrollment, and institutional legitimacy from sustaining this reading; has no exposure to the cost of the deferral since its output is the study itself, not the sacrifice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, beneficiary).

% Build careers, reputations, and lineages of transmitted expertise around a body of law with no current point of application. Their professional standing depends on the commandment remaining suspended-but-valid rather than either fulfilled or dismissed; a declaration that the law is a dead husk or that study alone fulfills it would each undercut their specific claim to be maintaining readiness for something real.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, kodashim_scholars, beneficiary,
    organized, generational, constrained, national).

% Rabbinic authorities who hold and transmit the doctrine that the Temple will be rebuilt and sacrificial law restored. They administer which texts count as authoritative continuations of the commandment and adjudicate disputes about its status. Their communal authority is partly constituted by being the interpreters of a still-live, still-binding-in-suspension law.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities, beneficiary).

% Community members whose scarce learning hours, communal resources, and educational attention are directed toward a legally suspended domain instead of toward applied halakha addressing marriage, business, health, and civic life. They bear an opportunity cost they did not choose and cannot easily contest without appearing to reject Torah study itself.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_practical_needs, payer,
    powerless, biographical, trapped, local).

% Yeshiva students whose curriculum time is substantially consumed by Kodashim and Taharot tractates governing a Temple that does not exist, at the expense of deeper training in areas of law they will actually need to navigate. Exiting the curriculum track means exiting the institution's credentialing and social network.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, students_directed_away_from_applied_halakha, payer,
    powerless, biographical, constrained, local).

% Diaspora and dispersed communities for whom the promise of restoration is maximally remote and who have no path to influence the doctrinal timeline. They inherit the deferral as a fixed premise of religious life without having had any voice in setting it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, communities_lacking_temple_infrastructure, payer,
    powerless, generational, trapped, regional).

% Hold the sibling reading that intellectual engagement with the sacrificial laws itself constitutes fulfillment of the commandment, not mere readiness-maintenance. They are excluded from this constraint's own framing, which treats their position as a category error — conflating preparation with performance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, study_as_performance_advocates, excluded,
    organized, generational, constrained, national).

% Hold the sibling view that the commandment is simply suspended husk without ongoing normative force absent an altar, and that treating it as live doctrine misallocates communal seriousness. They are present in halakhic discourse but structurally minority voices against the institutional mainstream that benefits from the deferral reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_pragmatists, excluded,
    moderate, biographical, constrained, national).

% Study the deferral doctrine as a structural mechanism comparable to other traditions' suspended-mandate constructs. Take no position on its theological validity but can trace how the doctrine sustains institutional continuity across the absence of its object.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed transmitted knowledge of the sacrificial system across centuries of Temple absence, so that if restoration occurs, the practical knowledge required to perform it correctly has not been lost. This is a genuine transmission-continuity problem distinct from either denying the law's validity or declaring it already fulfilled.
% TRANSFER_FUNCTION: Moves scarce communal attention, educational hours, and institutional prestige away from immediately applicable areas of law and toward a domain whose application is contingent on an event (Temple restoration) with no fixed timeline, concentrating interpretive authority and career capital in scholars and institutions who specialize in the suspended domain.
% ABSENT_VOICES: Advocates of study-as-performance are excluded from this reading's own framing (their position is treated as a category confusion), and halakhic pragmatists who read the law as simple suspended husk are structurally marginalized relative to institutions whose legitimacy depends on the deferral-with-readiness account remaining authoritative.
% DISAPPEARANCE_RATIONALE: If the messianic-deferral reading were abandoned overnight, yeshiva curricula, scholarly career structures, and a strand of communal identity built around readiness-maintenance would need to reorganize — but whether this counts as the world rearranging or merely relabeling itself is disputed: adherents say the underlying transmission function would be lost or reframed as pure antiquarianism; critics say the practical world of applied halakha would barely notice, since Kodashim study was never load-bearing for present observance.
% FOUNDING_PROBLEM: After the Temple's destruction, the rabbinic movement needed to explain why detailed, God-commanded sacrificial law should continue to be studied and transmitted with full seriousness despite having no possible present performance, without either declaring the commandments abrogated (theologically catastrophic) or claiming the law was now fulfilled by other means (which would compete with the sacrificial system's distinct character).
% FOUNDING_PROBLEM_CORROBORATION: Messianic restorationist authorities and Kodashim scholars attest the founding problem remains fully live — restoration is a genuine future contingency requiring maintained readiness. Halakhic pragmatists and some historians of rabbinic Judaism, positioned outside the institutional beneficiary set, attest that the doctrine's practical function shifted long ago from readiness-maintenance to institutional and pedagogical self-perpetuation, with restoration serving as an unfalsifiable horizon rather than an operative planning target.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) reflecting opportunity cost rather than direct material extraction: the primary cost borne by present-generation stakeholders is diverted attention and institutional resources, not confiscated wealth. Suppression is moderate (0.38) — dissenting readings (study-as-performance, pragmatist husk-reading) are not violently suppressed but are structurally disadvantaged relative to the institutionally dominant deferral reading, which controls curricular gatekeeping. Theater ratio is modest but rising (0.28 by interval end) reflecting a slow drift toward the maintenance-of-readiness framing becoming increasingly disconnected from any operative restoration timeline, without yet becoming purely performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva institutions and messianic restorationist authorities sit at the beneficiary end: they administer the doctrine, gain prestige and resource allocation from its centrality, and bear none of the opportunity cost. Kodashim scholars are secondary beneficiaries whose career capital is specifically tied to this reading remaining authoritative. Present-generation community members, students, and diaspora communities without temple-adjacent institutional stakes sit at the target end: they supply the scarce attention and resources redirected toward the suspended domain and have limited power to contest the curricular allocation, since doing so requires challenging a doctrine framed as core to Torah eternality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare preserves the genuine coordination function — knowledge transmission across a discontinuity is a real problem the deferral doctrine partially solves — while still registering the asymmetric cost this coordination imposes on those whose attention and resources it redirects. Calling this a pure snare would deny the real transmission-continuity value; calling it a pure rope would erase the opportunity cost borne by present-generation stakeholders who have no say in the curricular allocation. The founding_problem mismatch check (status=contested, verdict=contested) flags rather than resolves the question of whether the readiness-maintenance function is still load-bearing or has drifted into self-perpetuating institutional theater — that is precisely the empirical question the omega variables below are meant to hold open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kodashim_kernel_reading_disagreement,
    'Is the sacrificial commandment temporally suspended-but-still-binding (this reading), fulfilled through study alone (study_as_performance), or a dead husk with no present normative force absent an altar (performance_only)?',
    'No empirical resolution is possible; this is a live doctrinal dispute within rabbinic tradition turning on theological premises about the nature of divine commandment, the mechanism of restoration, and what counts as fulfillment. Different communities and authorities hold different readings permanently.',
    'The messianic_deferral reading (this constraint) authors moderate extraction from opportunity cost and treats present needs as legitimately subordinated to future contingency. The study_as_performance reading would treat the same study as itself constituting fulfillment, collapsing the opportunity-cost framing entirely (near-zero extraction, since nothing is deferred). The performance_only reading would treat continued intensive study as a category error, potentially reading the same institutional apparatus as extractive without even the readiness-maintenance justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kodashim_kernel_reading_disagreement, conceptual, 'The kernel contest itself: three incompatible readings of what studying suspended sacrificial law accomplishes.').

omega_variable(
    restoration_timeline_indeterminacy,
    'Does the doctrine''s indefinite deferral of a fixed restoration timeline function as genuine theological humility (the timing is God''s to determine) or as an unfalsifiable premise that insulates the institutional apparatus from ever having to justify its resource allocation against a concrete deadline?',
    'There is no empirical test; this turns on whether one reads open-ended eschatological timing as a feature (preserving divine sovereignty over history) or a structural convenience (permitting indefinite deferral of accountability). Historical comparison to other traditions'' suspended-mandate doctrines with fixed vs. open timelines could inform but not settle this.',
    'If the indeterminacy is read as structural convenience, the effective extraction is higher than the base_properties.extractiveness value suggests, since the deferral can never be falsified or resolved by any future event short of restoration itself. If read as genuine theological humility, the moderate extractiveness score stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_timeline_indeterminacy, conceptual, 'Whether unfalsifiable timing insulates the doctrine from accountability or reflects genuine theological constraint.').

omega_variable(
    present_generation_consent,
    'Do present-generation community members and students who bear the opportunity cost of Kodashim-centric curricula meaningfully consent to this allocation, or is it inherited as an unquestionable premise of religious education they have no realistic channel to contest?',
    'Survey or interview data on whether students and community members perceive curricular time allocation as a live choice versus a fixed inheritance; comparison across denominations and institutions with varying degrees of curricular flexibility.',
    'If consent is largely absent or coerced by social/institutional pressure, the payer classification for present_generation_practical_needs and students_directed_away_from_applied_halakha is strongly warranted and the tangled_rope classification is well-supported. If genuine informed consent and valuing of the practice is widespread, the extraction is better read as a chosen cost within a coordination good, weakening the victim framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_generation_consent, empirical, 'Whether the opportunity cost borne by present-generation stakeholders is meaningfully consented to.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.15).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.18).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.21).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.24).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.26).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__messianic_deferral, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__messianic_deferral, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__messianic_deferral, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% This constraint (messianic_deferral) forms a kernel triplet with kodashim_commandment_status__study_as_performance and kodashim_commandment_status__performance_only, all reading the same underlying kernel — the status of sacrificial commandments after Temple destruction. Each reading authors a distinct ε: this reading is moderate (0.42, opportunity-cost extraction with a genuine transmission-continuity coordination function); study_as_performance would author near-zero extraction (study IS fulfillment, nothing is deferred or wasted); performance_only would author extraction differently again, reading continued intensive study as potentially extractive institutional overhead attached to a commandment that has no remaining normative force. The three are linked structurally but never merged — per the ε-invariance principle, they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
