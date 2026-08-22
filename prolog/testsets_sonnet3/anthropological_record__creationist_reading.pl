% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates the creationist reading of the
 *   anthropological-record kernel: the claim that the human origins record
 *   reveals divine creation event(s) compatible with a scriptural timeline or
 *   designed complexity. This is ONE of three structurally distinct
 *   constraints emerging from the same underlying kernel — the naturalist
 *   reading (materialist origins via scientific method) and the indigenous
 *   epistemology reading (relational continuity via oral tradition) are
 *   separate constraints with their own epsilon and stakeholder structure,
 *   not alternative measurements of this one. Within its own communities,
 *   this reading requires active institutional enforcement (curriculum
 *   control, statements of faith, employment conditions) to maintain against
 *   competing evidence and competing readings, and it names identifiable
 *   beneficiaries (leadership, ministries) and identifiable payers
 *   (dissenters, students with no choice, scientists forced into a
 *   professional/communal bind). This is what distinguishes it, as a
 *   constraint, from a Mountain: the record itself does not enforce this
 *   reading — communities and institutions do.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, 'cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a').
narrative_ontology:cs_kernel_codification('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', fixed_text).
narrative_ontology:cs_authority_grounding('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', lineage).
narrative_ontology:cs_interpretation_layer_present('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a').
narrative_ontology:cs_reading_relation('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', foundational, scriptural_text_is_inerrant_historical_record).
narrative_ontology:cs_axiom_status(scriptural_text_is_inerrant_historical_record, holdable).
narrative_ontology:cs_axiom_grounding('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', scriptural_text_is_inerrant_historical_record, theological).
narrative_ontology:cs_axiom('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', foundational, divine_causation_required_for_origin_events).
narrative_ontology:cs_axiom_status(divine_causation_required_for_origin_events, holdable).
narrative_ontology:cs_axiom_grounding('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', divine_causation_required_for_origin_events, theological).
narrative_ontology:cs_axiom('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', secondary, empirical_dating_methods_subordinate_to_scriptural_chronology).
narrative_ontology:cs_axiom_status(empirical_dating_methods_subordinate_to_scriptural_chronology, holdable).
narrative_ontology:cs_axiom_grounding('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', empirical_dating_methods_subordinate_to_scriptural_chronology, conventional).
narrative_ontology:cs_created_at('cc7cdd2e-9f43-437d-8a9f-2699a9ca8b4a', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_institutional_leadership).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, affiliated_educational_ministries).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, congregational_dissenters).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, homeschooled_students).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, credentialed_scientists_within_faith_communities).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, scriptural_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, young_earth_or_designed_complexity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrinal boundaries for what counts as an admissible reading of the fossil and genomic record within affiliated churches, schools, and ministries. Administers curricula, statements of faith, and employment standards that require affirming a designed-complexity or young-earth timeline. Collects donations, tuition, and institutional loyalty that depend on maintaining the reading's authority; has the standing to revise doctrine but bears none of the personal cost of enforcing it.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, creationist_institutional_leadership, beneficiary).

% Publish curricula, museums, and media built on the creationist reading; their revenue and institutional identity depend on the reading's continued authority. They benefit from the enforcement apparatus without personally administering discipline.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, affiliated_educational_ministries, beneficiary,
    organized, generational, constrained, national).

% Lay members who find the naturalist evidence persuasive but face social exclusion, loss of standing, or family rupture if they voice doubt. Exit means leaving the community that has structured their relationships, marriage, and mutual aid; staying means suppressing disagreement.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, congregational_dissenters, payer,
    powerless, biographical, constrained, local).

% Receive curricula built exclusively on the creationist reading with no exposure to competing frameworks; have no say in the material presented to them and face a later cost re-entering discourse (higher education, professional science) that presumes the naturalist reading.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, homeschooled_students, payer,
    powerless, biographical, trapped, local).

% Trained biologists, geologists, and anthropologists who remain within the faith community. Face a forced choice between professional standing (which requires accepting the evidentiary basis of the naturalist reading) and community standing (which requires public affirmation of the creationist reading). Some manage this by silence, some by quiet exit, some by public affirmation against their own professional judgment.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, credentialed_scientists_within_faith_communities, payer,
    moderate, biographical, constrained, national).

% Holds the evidentiary consensus this reading displaces within its own communities. Is not a participant in the internal doctrinal conversation and has no standing to adjudicate it from outside; its findings are read by the reading's adherents as either irrelevant to revelation or actively hostile to it.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, naturalist_scientific_establishment, excluded,
    institutional, civilizational, analytical, global).

% Historians, sociologists of religion, and comparative epistemologists who study how the reading is maintained, transmitted, and contested without themselves being bound by it.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, creationist_institutional_leadership).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable interpretive framework that lets a religious community read the anthropological and fossil record as continuous with its scriptural commitments, avoiding the destabilization that a fully naturalist reading would pose to shared doctrine, moral authority, and communal identity.
% TRANSFER_FUNCTION: Moves interpretive authority and material resources (tuition, donations, curriculum sales) toward institutions and leaders who administer the creationist reading, and moves the cost of maintaining doctrinal conformity (social exclusion risk, professional forced-choice, restricted education) onto dissenters, credentialed scientists within the community, and children with no say in their own curriculum.
% ABSENT_VOICES: Congregants who privately find naturalist evidence compelling rarely speak in doctrinal settings where dissent carries social cost. Homeschooled children have no voice in curriculum design. Credentialed scientists within the community frequently self-censor rather than risk standing on either side.
% DISAPPEARANCE_RATIONALE: If the creationist reading's institutional enforcement vanished overnight, affiliated ministries would lose their doctrinal rationale and much of their funding base, homeschool curricula would need wholesale revision, congregational dissenters could voice disagreement without social cost, and credentialed scientists within the community would no longer face the forced choice between professional and communal standing — the material and social structure built around the reading would need to reorganize.
% FOUNDING_PROBLEM: The reading was built to preserve scriptural authority and communal identity against a perceived threat: that accepting a materialist account of human origins would undermine the authority of scripture on other matters and dissolve the doctrinal coherence holding the community together.
% FOUNDING_PROBLEM_CORROBORATION: Adherent leadership attests the problem (defense of scriptural authority) remains live and central. From outside the benefiting institutions, historians of American religion and sociologists studying evangelical institutional formation corroborate that the doctrinal boundary continues to function as a live identity marker and fundraising basis, not merely a residual claim — though they characterize its persistence as serving institutional cohesion and revenue rather than adjudicating the historical record.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 by interval end) reflects the resource and loyalty transfer toward institutional leadership and ministries, net of the real (non-zero) coordination benefit of shared doctrinal identity — this is not treated as a pure snare because the coordination function (preserving communal coherence and scriptural authority) is genuine, not merely a cover story. Suppression (0.62) is authored high because dissent carries real social and material cost independent of persuasiveness on the merits — congregants who find contrary evidence compelling face relational and institutional consequences for saying so, not merely for being wrong. Theater ratio (0.44) reflects that a substantial share of the apparatus (museums, apologetics literature, curriculum defense) has shifted toward performing certainty for an audience of donors and members rather than engaging live counter-evidence. Accessibility collapse is authored moderate (0.5), not mountain-high, because the alternative readings remain visibly available in the surrounding culture — the collapse is local to the community, not global. Resistance is authored high (0.68) because dissent, quiet exit, and internal scientific dissent are all observably present and growing.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership seat, the reading functions as settled doctrine defended against external and internal challenge — essentially a boundary-maintenance Rope. From the congregational dissenter or student seat with constrained/trapped exit, the same structure computes as enforced conformity with real material and relational stakes. The engine should compute these differently from the same structural data; the divergence is exactly what the framework is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits near the full-beneficiary end: it sets the doctrinal boundary, collects the resources that depend on the boundary holding, and bears little personal cost from enforcement. Congregational dissenters and homeschooled students sit near the full-target end: they bear the social, educational, and psychological cost of conformity with no meaningful say in the terms. Credentialed scientists within the community sit in an intermediate but still target-leaning position — moderate power (professional credentials give them some external leverage) but constrained exit (community identity), which is why they experience the forced choice rather than a clean exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defense of scriptural authority against perceived erosion) is authored as live, not dead — this blocks a premature snare classification: the coordination function this reading serves is not a decayed pretext but an active, contested claim within its communities. What keeps the classification from resolving to pure rope is the enforcement apparatus and the asymmetric cost distribution: the same structure that lets a community cohere around shared doctrine also extracts loyalty and resources from members with no real voice in the terms. Tangled Rope captures this correctly: genuine coordination function plus asymmetric extraction, requiring active enforcement to hold — exactly the profile the schema gates on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the creationist reading''s disagreement with the naturalist reading live — in the raw evidentiary record itself, in the interpretive framework applied to a shared record, or in the standing of scientific method as an adjudicative authority?',
    'This is a conceptual/framing question, not empirically resolvable by more fossils or genomic data: it depends on whether the community treats scientific method as authoritative on matters of ultimate origin. Sibling readings (naturalist_reading, indigenous_epistemology_reading) locate the disagreement differently.',
    'If the disagreement is purely interpretive (same evidence, different framework), the creationist reading''s suppression is better understood as boundary-maintenance against a rival authority claim. If it is evidentiary (denial of dating methods, fossil record itself), suppression is better understood as active resistance to a settled empirical finding — a materially different and more extractive profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the creationist/naturalist split is interpretive or evidentiary.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the creationist reading''s authority experienced by adherents as revealed, self-evident truth (naturalized, mountain-like within the community) or as a constructed doctrinal boundary actively maintained by identifiable institutional beneficiaries?',
    'Survey data on how adherents describe the reading''s status (revealed truth vs. taught doctrine); institutional financial disclosure showing revenue dependence on doctrinal conformity; longitudinal tracking of doctrinal statement revisions in response to institutional pressure versus evidentiary pressure.',
    'If experienced as pure revealed truth with no institutional dependency, the reading would read closer to a (locally naturalized) mountain from the inside. The presence of declared institutional beneficiaries with clear revenue dependence (this story''s FSM-relevant structural fact) is why this story is authored as tangled_rope rather than mountain — the false-summit signature is precisely what this omega documents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the reading functions as naturalized truth or constructed, beneficiary-dependent doctrine within its communities.').

omega_variable(
    credentialed_science_adjudicative_standing,
    'Within communities holding this reading, has credentialed science ever held adjudicative authority over origins claims, or has scriptural authority always been prior — i.e., is the ''loss'' of science''s adjudicative monopoly a change or a restoration?',
    'Historical tracing of doctrinal statements and institutional positions across the 20th and 21st centuries within the specific denominational traditions in question.',
    'If this is a restoration of a always-prior scriptural authority, the reading''s persistence is better modeled as continuous tradition (lower novelty, lower detectable ''suppression event''). If it is a reaction formed against a prior period of greater scientific deference within these same communities, the suppression metric should weight the historical shift more heavily.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialed_science_adjudicative_standing, empirical, 'Whether credentialed science''s loss of adjudicative monopoly in these communities is a recent reaction or a longstanding continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__creationist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__creationist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__creationist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__creationist_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__creationist_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(anth_be_t8, anthropological_record__creationist_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(anth_be_t16, anthropological_record__creationist_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(anth_be_t24, anthropological_record__creationist_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(anth_be_t32, anthropological_record__creationist_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(anth_be_t40, anthropological_record__creationist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(anth_su_t8, anthropological_record__creationist_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(anth_su_t16, anthropological_record__creationist_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(anth_su_t24, anthropological_record__creationist_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(anth_su_t32, anthropological_record__creationist_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(anth_su_t40, anthropological_record__creationist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.1).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'what the anthropological record reveals about human origins' per the ε-invariance principle. anthropological_record__naturalist_reading (materialist origins, scientific-method authority) and anthropological_record__indigenous_epistemology_reading (relational continuity, oral-tradition authority) are the other two members. Each carries its own ε, beneficiary/victim structure, and classification; this reading's ε (0.58, tangled_rope) is not commensurable with the siblings' ε values as a single averaged 'BGS-style' measurement — they describe structurally distinct claims about the same evidentiary substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
