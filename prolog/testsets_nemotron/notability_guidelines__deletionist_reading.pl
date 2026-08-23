% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: WP:N Notability Guidelines — Deletionist Reading
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   The Wikipedia notability guideline (WP:N) functions as a coordination
 *   mechanism for a global, open-edit encyclopedia. In the deletionist
 *   reading, WP:N is a necessary epistemic quality filter that prevents the
 *   commons from degrading into a repository of unverifiable claims,
 *   promotional content, and vanity pages. The constraint operates as a rope:
 *   it solves a genuine collective-action problem (how to maintain quality at
 *   scale without central editors) by coordinating contributors around a
 *   shared standard — significant coverage in reliable, independent secondary
 *   sources — and the beneficiaries are the global readership who receive a
 *   trustworthy reference work and the active editor community whose labor is
 *   made tractable by clear boundaries. There is no victim set in this
 *   reading: spam, vanity, and promotional content are not 'agents who bear
 *   costs' but noise that the filter correctly excludes. The guideline
 *   requires active enforcement (AfD processes, speedy deletion criteria) but
 *   the enforcement is proportionate to the coordination problem.
 *
 * KEY AGENTS:
 *   - global_readership: Primary beneficiary (analytical/arbitrage) — receives quality-preserved commons
 *   - active_editors_curators: Beneficiary/agenda_setter hybrid (organized/constrained) — their labor is made coherent by the boundary
 *   - subjects_of_borderline_articles: Excluded (powerless/trapped) — would-be subjects who fail notability but are not 'victims' in this reading
 *   - deletionist_editors: Agenda_setter (organized/constrained) — maintain the boundary through AfD participation
 *   - inclusionist_editors: Excluded (organized/constrained) — disagree with boundary placement but participate in the same process
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.28).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N Notability Guidelines — Deletionist Reading").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '5c78d661-6f93-4401-b68a-c821eb8375c7').
narrative_ontology:cs_kernel_codification('5c78d661-6f93-4401-b68a-c821eb8375c7', formalized).
narrative_ontology:cs_authority_grounding('5c78d661-6f93-4401-b68a-c821eb8375c7', practice).
narrative_ontology:cs_interpretation_layer_present('5c78d661-6f93-4401-b68a-c821eb8375c7').
narrative_ontology:cs_reading_relation('5c78d661-6f93-4401-b68a-c821eb8375c7', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c78d661-6f93-4401-b68a-c821eb8375c7', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('5c78d661-6f93-4401-b68a-c821eb8375c7', foundational, verifiability_precedes_inclusion).
narrative_ontology:cs_axiom_status(verifiability_precedes_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('5c78d661-6f93-4401-b68a-c821eb8375c7', verifiability_precedes_inclusion, conventional).
narrative_ontology:cs_axiom('5c78d661-6f93-4401-b68a-c821eb8375c7', foundational, encyclopedic_quality_requires_boundary).
narrative_ontology:cs_axiom_status(encyclopedic_quality_requires_boundary, holdable).
narrative_ontology:cs_axiom_grounding('5c78d661-6f93-4401-b68a-c821eb8375c7', encyclopedic_quality_requires_boundary, instrumental).
narrative_ontology:cs_reference_frame('5c78d661-6f93-4401-b68a-c821eb8375c7', deletionist_quality_filter).
narrative_ontology:cs_drift_state('5c78d661-6f93-4401-b68a-c821eb8375c7', contemporary_notability_standard, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c78d661-6f93-4401-b68a-c821eb8375c7', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, global_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, active_editors_curators).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, epistemic_quality_requires_boundary_maintenance).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, verifiability_precedes_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a trustworthy, quality-filtered encyclopedia. Can use any reference work (arbitrage exit). The constraint subsidizes them by preventing commons degradation — their benefit is the preserved epistemic quality of the resource they consult.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, global_readership, beneficiary,
    analytical, generational, arbitrage, global).

% Volunteer editors who maintain articles and enforce notability through AfD and patrolling. The constraint makes their labor coherent by providing a clear, defensible boundary — without it, effort would be spent on unverifiable content. Invested in the project (constrained exit). Also set the agenda by participating in boundary enforcement.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, active_editors_curators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, active_editors_curators, agenda_setter).

% Individuals, organizations, or topics that fail notability and are excluded from the main namespace. In the deletionist reading they are not victims — they are correctly filtered noise. They have no voice in the process (excluded) and no practical exit (trapped — they cannot make themselves notable). Draftspace and external publication are alternatives but do not confer encyclopedic inclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, subjects_of_borderline_articles, excluded,
    powerless, immediate, trapped, global).

% Editors who actively enforce notability through AfD nominations, speedy deletion tagging, and policy advocacy. They maintain the boundary that makes the coordination function work. Constrained exit — their identity and reputation are bound to the project.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, deletionist_editors, agenda_setter,
    organized, biographical, constrained, global).

% Editors who argue for broader inclusion and challenge notability boundaries. In this reading they are excluded from the constraint's beneficiary logic — they disagree with the boundary but participate in the same AfD process. Their exit is constrained (invested in the project). The exclusion here is perspectival: they are not excluded from the process but from the deletionist framing's beneficiary set.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, inclusionist_editors, excluded,
    organized, biographical, constrained, global).

% Sees the full structure: a coordination mechanism that solves the quality-at-scale problem for an open commons. Experiences no extraction, no suppression — the constraint is a structural feature of the knowledge infrastructure.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the open-edit commons from degrading into an unreliable repository by establishing a shared, defensible standard for inclusion: significant coverage in reliable, independent secondary sources. Solves the collective-action problem of quality maintenance without central editorial authority.
% TRANSFER_FUNCTION: Moves editorial labor away from unverifiable content toward verifiable topics. No resource transfer between agents — the 'cost' of meeting notability is the labor of establishing coverage, which is the coordination function itself.
% ABSENT_VOICES: Communities whose knowledge traditions lack institutional secondary sources (oral histories, indigenous knowledge, marginalized academic fields) — they are not in the room when notability is adjudicated because the epistemic standard itself excludes their evidentiary base. Pairs with the systemic_bias_in_reliable_sources omega.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, the main namespace would flood with promotional content, vanity pages, and unverifiable claims within weeks. Editor workload would become unmanageable, readership trust would collapse, and the project would lose its epistemic function — the world would rearrange around a degraded or forked encyclopedia.
% FOUNDING_PROBLEM: Early Wikipedia (2003–2005) faced explosive growth of unverifiable articles: vanity pages, promotional content, original research, and hoaxes. The community had no scalable way to distinguish encyclopedic topics from non-encyclopedic ones without central editors.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the deletionist community and by independent scholars of peer production (e.g., Halfaker et al. on Wikipedia's quality maintenance, Forte & Bruckman on decentralized coordination). The inclusionist community contests that the problem is still live, arguing the solution has become the problem. No neutral arbiter has resolved the dispute.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.12): the constraint extracts minimal surplus from participants — the 'cost' of meeting notability is the cost of doing the work to establish reliable coverage, which is the coordination function itself. Suppression (0.28) reflects active boundary enforcement (AfD, CSD) but is low because alternatives (draftspace, userspace, external publication) remain open — the constraint suppresses inclusion in the main namespace only. Theater ratio (0.15) is low because the enforcement machinery (AfD) is functional, not performative. Accessibility collapse (0.72) is moderately high: once the notability standard is understood, the space of viable articles collapses to those with significant independent coverage — but this is the coordination function, not extraction. Resistance (0.35) reflects ongoing inclusionist pressure but is contained within the deliberative process.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist seat experiences this as a rope: a working coordination mechanism that makes the project viable. The inclusionist seat (a different reading of the same kernel) experiences it as a snare: a gatekeeping apparatus that excludes marginalized knowledge. The engine computes this divergence from the same structural data — the deletionist reading declares no victims, the inclusionist reading declares victims. This story authors the deletionist seat only.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: global_readership (arbitrage exit — can use any reference work; the constraint subsidizes them by preserving quality) and active_editors_curators (constrained exit — invested in the project; the constraint makes their labor effective). No victims declared: entities that fail notability are not 'bearing costs of the constraint' — they are correctly filtered noise. The directionality derivation yields d ≈ 0.0 for readership (full beneficiary), d ≈ 0.3 for editors (net beneficiary with some maintenance cost), d ≈ 0.5 for borderline subjects (symmetric — they neither gain nor lose from the encyclopedia's boundary).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing commons degradation at scale) remains live — the coordination challenge of open editing at Wikipedia's scale has not been solved by any other mechanism. The constraint has not atrophied into a piton: enforcement is functional, not theatrical; the theater ratio has remained stable. No mandatrophy resolution declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_threshold_calibration,
    'Is the current notability threshold (significant coverage in reliable secondary sources) calibrated to the coordination optimum, or does it exclude content that would pass a lower but still functional threshold?',
    'Counterfactual analysis: simulate encyclopedia quality and editor workload under alternative thresholds using historical AfD data and editor retention metrics.',
    'If the threshold is stricter than coordination requires, the constraint carries hidden extraction (excluded content that would not degrade the commons) — the deletionist reading''s ''no victims'' claim would be false. If calibrated, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_threshold_calibration, empirical, 'Whether the notability standard is coordination-optimal or over-fitted').

omega_variable(
    systemic_bias_in_reliable_sources,
    'Does the ''reliable secondary sources'' criterion systematically disadvantage knowledge from marginalized communities that lack institutional media coverage?',
    'Audit of notability outcomes by topic demographics; comparison with inclusionist reading''s victim claims.',
    'If systemic bias exists, the deletionist reading''s ''no victims'' declaration is incomplete — the constraint would have a victim set the reading does not see. This is the structural delta between deletionist and inclusionist readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_bias_in_reliable_sources, empirical, 'Whether the coordination function''s epistemic standard carries distributive injustice').

omega_variable(
    committer_framing_deletionist_vs_inclusionist,
    'Is the notability_guidelines kernel a single constraint with observer-dependent classification, or are deletionist_reading and inclusionist_reading structurally distinct constraints with different ε?',
    'ε-invariance test: measure extraction from the deletionist seat (near-zero) and the inclusionist seat (substantial). If ε differs by reading, they are distinct constraints linked by network.affects_constraints.',
    'Confirms the ε-invariance principle: the kernel is a label, not a constraint. Each reading gets its own story with its own ε, beneficiaries, victims, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_deletionist_vs_inclusionist, conceptual, 'Kernel-reading structural decomposition per DP-001').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 2003, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2003, notability_guidelines__deletionist_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(nota_tr_t2007, notability_guidelines__deletionist_reading, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(nota_tr_t2011, notability_guidelines__deletionist_reading, theater_ratio, 2011, 0.14).
narrative_ontology:measurement(nota_tr_t2015, notability_guidelines__deletionist_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(nota_tr_t2019, notability_guidelines__deletionist_reading, theater_ratio, 2019, 0.15).
narrative_ontology:measurement(nota_tr_t2023, notability_guidelines__deletionist_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(nota_be_t2003, notability_guidelines__deletionist_reading, base_extractiveness, 2003, 0.08).
narrative_ontology:measurement(nota_be_t2007, notability_guidelines__deletionist_reading, base_extractiveness, 2007, 0.1).
narrative_ontology:measurement(nota_be_t2011, notability_guidelines__deletionist_reading, base_extractiveness, 2011, 0.11).
narrative_ontology:measurement(nota_be_t2015, notability_guidelines__deletionist_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(nota_be_t2019, notability_guidelines__deletionist_reading, base_extractiveness, 2019, 0.12).
narrative_ontology:measurement(nota_be_t2023, notability_guidelines__deletionist_reading, base_extractiveness, 2023, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2003, notability_guidelines__deletionist_reading, suppression_requirement, 2003, 0.2).
narrative_ontology:measurement(nota_su_t2007, notability_guidelines__deletionist_reading, suppression_requirement, 2007, 0.25).
narrative_ontology:measurement(nota_su_t2011, notability_guidelines__deletionist_reading, suppression_requirement, 2011, 0.27).
narrative_ontology:measurement(nota_su_t2015, notability_guidelines__deletionist_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(nota_su_t2019, notability_guidelines__deletionist_reading, suppression_requirement, 2019, 0.28).
narrative_ontology:measurement(nota_su_t2023, notability_guidelines__deletionist_reading, suppression_requirement, 2023, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.02).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel decomposes into three constraint stories: deletionist_reading (rope, ε≈0.12, beneficiaries=readership/editors, no victims), inclusionist_reading (snare/tangled_rope, higher ε, victims=marginalized_knowledge_communities), deliberative_reading (scaffold/tangled_rope, process-as-coordination). The deletionist reading's coordination function (quality threshold) is cited by the deliberative reading as the stable boundary that deliberation negotiates around, and by the inclusionist reading as the exclusion mechanism to be contested. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
