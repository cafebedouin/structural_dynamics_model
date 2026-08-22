% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope via Customary International Law Evolution
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes minimum
 *   humanitarian protections for armed conflicts. The treaty text addresses
 *   interstate wars explicitly; non-international armed conflicts are treated
 *   ambiguously. This reading interprets CA3 scope as a living customary
 *   norm: the boundary of applicability is determined by observable patterns
 *   of state practice and the accumulated legal opinions (opinio juris)
 *   underlying those patterns. As states encounter new conflict types and
 *   respond with consistent legal positions across multiple incidents, those
 *   positions accumulate as customary law, expanding or consolidating CA3's
 *   scope without formal amendment. The ICRC acts as interpreter and
 *   documenter of this evolution. This reading frames scope as procedurally
 *   generated rather than textually fixed — a coordination mechanism that
 *   preserves state sovereignty while enabling humanitarian standards to
 *   follow conflict realities. The claim/metric divergence is intentional:
 *   this reading CLAIMS rope (coordination solving an amendment-avoidance
 *   problem) while authoring metrics that reflect extraction (interpretive
 *   authority concentrated in ICRC, exclusion of non-state voices, procedural
 *   opacity). The engine computes that gap; the gap itself is the signal.
 *
 * KEY AGENTS:
 *   - ICRC: institutional interpreter of customary scope boundaries, documenter of state practice, agenda-setter for opinio juris recording
 *   - States as parties: generate practice through conflict conduct and policy; define scope collectively via practice patterns
 *   - Armed groups and non-state actors: subjects of scope determination but excluded from practice generation
 *   - Affected civilian populations: benefit from scope expansion when conflicts are brought within CA3 but have no voice in boundary-drawing
 *   - Expansive human rights advocates: excluded position; argue for broader interpretation but frame their argument as aspirational outside customary law discourse
 *   - Military commanders: navigate operationally within zones of scope ambiguity
 *   - Legal scholars: document and interpret state practice, influence opinio juris framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.28).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope via Customary International Law Evolution").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'c146b190-9a7d-4f0a-8a02-57b3e2ca8613').
narrative_ontology:cs_kernel_codification('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', fixed_text).
narrative_ontology:cs_authority_grounding('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', lineage).
narrative_ontology:cs_interpretation_layer_present('c146b190-9a7d-4f0a-8a02-57b3e2ca8613').
narrative_ontology:cs_reading_relation('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', foundational, state_practice_determines_scope).
narrative_ontology:cs_axiom_status(state_practice_determines_scope, holdable).
narrative_ontology:cs_axiom_grounding('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', state_practice_determines_scope, conventional).
narrative_ontology:cs_axiom('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', foundational, opinio_juris_aggregation_legitimates_expansion).
narrative_ontology:cs_axiom_status(opinio_juris_aggregation_legitimates_expansion, holdable).
narrative_ontology:cs_axiom_grounding('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', opinio_juris_aggregation_legitimates_expansion, instrumental).
narrative_ontology:cs_axiom('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', secondary, customary_law_preserves_state_sovereignty).
narrative_ontology:cs_axiom_status(customary_law_preserves_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', customary_law_preserves_state_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', treaty_text_supplemented_by_state_practice).
narrative_ontology:cs_drift_state('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', contemporary_non_international_armed_conflict_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c146b190-9a7d-4f0a-8a02-57b3e2ca8613', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_monitors).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, customary_law_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_as_parties).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, affected_civilian_populations).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_as_parties).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, military_operational_commanders).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_law_as_living_norm_system).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, state_practice_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies CA3 in field operations and legal analysis. Tracks state practice patterns across conflicts, documents customary law evolution, and publishes authoritative guidance on scope boundaries. Maintains institutional neutrality but exercises interpretive discretion over what counts as binding state practice and opinio juris.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, mobile, global).

% Generate state practice through their conduct in armed conflicts and policy statements. Their patterns collectively define customary scope. They benefit from a framework that evolves without formal amendment (preserving sovereignty and avoiding treaty renegotiation), but accept interpretive uncertainty as the cost of flexibility.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_as_parties, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, states_as_parties, payer).

% Are subjects of CA3 application but have no voice in state practice generation or customary law interpretation. Cannot influence whether their conflict's intensity/organization triggers the threshold; interpretation happens above and around them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, armed_groups_and_non_state_actors, excluded,
    powerless, immediate, trapped, local).

% Receive (or are denied) minimum humanitarian protections depending on where the customary scope boundary is drawn for their conflict. Have no institutional presence in opinio juris formation or state practice documentation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, affected_civilian_populations, beneficiary,
    powerless, biographical, trapped, local).

% Argue CA3 should apply to any organized armed violence as a floor standard. Are excluded from the customary law interpretation apparatus because their position is framed as aspirational rather than descriptive of actual state practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, expansive_human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Must navigate ambiguity about whether their conflict context triggers CA3 obligations. Operate in zones of unclear scope where the customary law boundary has not yet fully consolidated.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, military_operational_commanders, payer,
    moderate, immediate, constrained, local).

% Analyze patterns of state practice, track opinio juris evolution, and publish interpretations that feed into the customary law discourse. Influence the frame through which practice is documented and codified.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, legal_scholars_and_commentators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of applying minimum humanitarian standards to armed conflicts that do not fit neatly into traditional interstate war categories, without requiring frequent formal treaty amendments. Allows scope to expand as state practice evolves and humanitarian consensus builds, via procedural interpretation rather than substantive renegotiation.
% TRANSFER_FUNCTION: Moves interpretive authority from explicit treaty text to documented patterns of state conduct and implicit legal opinion. States collectively 'transfer' the rule-making power to customary process; ICRC and scholars transfer their field observations and legal analysis into the opinio juris record.
% ABSENT_VOICES: Armed groups and non-state actors cannot generate state practice or shape opinio juris; they are subjects of the scope determination but have no seat in the interpretive apparatus. Affected civilian populations also lack institutional voice in the customary law process. Expansive human rights advocates are excluded because their aspirational readings are treated as distinct from descriptive customary law analysis.
% DISAPPEARANCE_RATIONALE: If the customary law scope boundary vanished, each state would revert to its own interpretation of CA3 applicability, creating fragmentation. The coordination value of a shared (if evolving) scope would be lost, and humanitarian standards would become state-specific rather than universal. The ICRC's capacity to operate under consistent baseline rules in multiple conflict settings would collapse.
% FOUNDING_PROBLEM: CA3 was drafted in 1949 with interstate conflicts as the primary model. By the 1960s–1980s, armed conflicts increasingly took non-international forms (civil wars, liberation struggles, insurgencies). The treaty text did not formally address these; formal amendment was politically blocked. A procedural solution was needed: allow scope to expand through customary law recognition of state practice without reopening the treaty.
% FOUNDING_PROBLEM_CORROBORATION: ICRC reports from multiple decades document the proliferation of non-international armed conflicts and the operational necessity of applying minimum standards across conflict types. State practice in enforcing CA3-like protections across diverse conflict settings (documented in Security Council resolutions, state military manuals, national legislation) corroborates that the problem persists and the customary mechanism is actively solving it. Legal scholars independent of the ICRC (including critical voices) acknowledge that customary law is the vehicle through which scope has expanded.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.42 at interval end) because the customary mechanism genuinely solves a coordination problem — avoiding formal treaty amendment while allowing humanitarian standards to evolve. The extraction reflects interpretive authority concentrated in the ICRC, which exercises substantial discretion in determining what counts as state practice and opinio juris. Suppression is low (0.28) because no external force actively constrains alternatives; the mechanism is procedural rather than coercive. Theater rises over the interval (0.05 to 0.22) because interpretive machinery increasingly performs legitimacy (documentation, consensus-building, scholarly publication) relative to its functional coordination role. The measurement series tracks the evolution of the constraint from 1949 to 2024 on a single shared time grid, capturing the rising cost of opacity as scope applies to increasingly diverse conflict types. The rising theater ratio reflects growing institutional performance around opinio juris formation even as the core coordination function remains necessary.
 *
 * PERSPECTIVAL GAP:
 *   The ICRC and states perceive the constraint as essential coordination avoiding formal amendment costs. Armed groups and affected populations perceive it as arbitrary boundary-drawing that determines whether they receive protections. Military commanders perceive it as operationally ambiguous: uncertainty about scope applicability creates compliance dilemmas in borderline conflicts. The engine computes these divergent seatings from the structural data. The claim (rope/coordination) matches the beneficiary seats' perspective; the metrics (rising theater, excluded voices, concentrated authority) match the target and excluded seats' experience of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   ICRC and humanitarian monitors sit near the beneficiary end (d ≈ 0.2): they gain institutional authority and operational space from the customary law frame. States sit near symmetric (d ≈ 0.5): they benefit from procedural flexibility and sovereignty preservation, but bear the cost of interpretive uncertainty in their military operations. Armed groups and affected populations sit at the target end (d ≈ 0.8): they are subjects of scope determination with no voice in the process; whether their conflict triggers CA3 protections depends on interpretations others make. Expansive advocates sit outside the system (d = analytical): their position is excluded from the customary law apparatus by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (formal amendment blockage in the Cold War) remains live — states still resist formal treaty revision, and the proliferation of non-international armed conflicts continues. However, the customary law mechanism, while solving the amendment problem, has increasingly become a substitute for transparent renegotiation. The opinio juris process is less visible and more interpretively controlled than formal amendment would be. A mandatrophy reading would argue the mechanism persists because it serves states and the ICRC (who control the interpretation apparatus) more than it serves the populations it nominally protects. The coordination value is real but the extraction cost is rising (theater ratio climbing). A genuine rope would show stable or declining theater; this one shows theater rising as performance (documentation, consensus-building) grows relative to functional coordination. That trajectory is consistent with extraction-seeking behavior: maintaining legitimacy for a mechanism whose primary function has plateaued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opinio_juris_documentation_ambiguity,
    'What counts as credible evidence of opinio juris — the legal opinion underlying state practice — and who decides?',
    'Examine ICRC documentation methods: are formal statements, military manuals, Security Council resolutions, and NGO reporting weighted equally in opinio juris determination? Comparison across different conflict situations where ICRC applied different standards.',
    'If opinio juris standard is vague or ICRC-determined without external check, the mechanism is more extractive than coordinative — scope expansion becomes interpretive authority rather than descriptive customary law. If standards are transparent and externally validated, extraction is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opinio_juris_documentation_ambiguity, empirical, 'Opacity of opinio juris documentation and interpretive gatekeeping.').

omega_variable(
    state_practice_homogeneity_vs_contestation,
    'When states disagree on whether a conflict type triggers CA3, does the customary law mechanism resolve the disagreement or simply record it as unresolved?',
    'Case studies of conflicts where states took opposing positions on CA3 applicability (asymmetric conflicts, counterinsurgency campaigns, failed-state violence). Track whether ICRC took a position, whether states converged toward it, and whether convergence indicates genuine consensus or deference to institutional authority.',
    'If customary mechanism genuinely aggregates divergent practice into consensus, it is coordination. If ICRC''s position becomes de facto binding despite state disagreement, extraction dominates. A middle case is coordination with increasing ICRC authority concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_practice_homogeneity_vs_contestation, empirical, 'Whether customary law mechanism resolves state disagreement or masks it.').

omega_variable(
    reading_foreclosure_via_procedural_authority,
    'Does the customary law reading''s claim that scope is determined by practice and opinio juris logically foreclose the human rights reading''s claim that scope should be determined by rights principles?',
    'Examine whether the two readings dispute the SAME QUESTION (what determines scope) or different questions (is scope currently rights-driven vs. practice-driven). If the same question, do they have incompatible answers within a single framework?',
    'If readings dispute the same question and have incompatible answers (one says practice determines scope, the other says rights must), the relation is forecloses. If one describes current reality and the other describes desired future, they coexist. If one argues procedure and the other argues substance, they may coexist if procedure can be adjusted to serve substantive goals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_procedural_authority, conceptual, 'Logical relationship between customary-law-based and human-rights-based scope interpretations.').

omega_variable(
    extractiveness_source_ambiguity,
    'Is the measured extractiveness (0.42) a property of the customary law mechanism itself, or is it an artifact of how opinio juris happens to be documented and interpreted in contemporary practice?',
    'Counterfactual: if opinio juris documentation became transparent, participatory, and externally audited, would extractiveness drop substantially while maintaining customary scope function? Or is the opacity structural to the mechanism?',
    'If opacity is structural, the constraint is intrinsically extractive despite coordination function. If opacity is a contemporary implementation choice, the constraint could be coordinative with lower extraction. This determines whether the measured rope is accurate or whether the mechanism''s theoretical form masks snare-like practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_source_ambiguity, conceptual, 'Whether extractiveness reflects mechanism or implementation.').

omega_variable(
    suppression_mechanism_internalization,
    'Do states suppress alternative scope interpretations (human rights, expansive readings) through legal argument and institutional gatekeeping, or through structural barriers that would persist after ICRC authority ended?',
    'Survey of state legal positions: do states argue against expansive scope, or do they simply not participate in the argument? If ICRC''s authority vanished, would states immediately adopt broader interpretations or continue defending state-centric scope?',
    'If suppression is structural (institutional gatekeeping), it remains extractive. If suppression is maintained only through ICRC authority, it could be removed procedurally. This bears on whether extraction is intrinsic or implementation-contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative scope readings is structural or authority-dependent.').

omega_variable(
    beneficiary_specification_ambiguity,
    'The reading lists ICRC and humanitarian monitors as beneficiaries. Do they benefit from scope expansion (more conflicts covered) or from scope ambiguity (preserved interpretive discretion)?',
    'Track ICRC statements about preferred scope boundaries and whether they favor clarity or flexibility. Examine whether scope expansion has increased ICRC operational reach or authority concentration.',
    'If ICRC benefits from expansion, beneficiary framing is accurate. If ICRC benefits from ambiguity, the constraint may extract primarily through preserved uncertainty rather than through scope-broadening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_specification_ambiguity, empirical, 'Whether beneficiaries gain from scope expansion or from interpretive discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement_basis(comm_tr_t1949, observed).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement_basis(comm_tr_t1977, observed).
narrative_ontology:measurement(comm_tr_t1992, common_article_3_scope__icrc_customary_reading, theater_ratio, 1992, 0.16).
narrative_ontology:measurement_basis(comm_tr_t1992, observed).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement_basis(comm_be_t1949, observed).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.28).
narrative_ontology:measurement_basis(comm_be_t1977, observed).
narrative_ontology:measurement(comm_be_t1992, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(comm_be_t1992, observed).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.1).
narrative_ontology:measurement_basis(comm_su_t1949, observed).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.18).
narrative_ontology:measurement_basis(comm_su_t1977, observed).
narrative_ontology:measurement(comm_su_t1992, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1992, 0.23).
narrative_ontology:measurement_basis(comm_su_t1992, observed).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.26).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2015, 0.27).
narrative_ontology:measurement_basis(comm_su_t2015, observed).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.18).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% The constraint family for 'common_article_3_scope' consists of three structurally distinct readings of the same kernel (CA3 text). Each reading generates a different constraint with different ε, beneficiary structures, and type classifications. (1) The state-centric reading (constraint_id: common_article_3_scope__state_centric_reading) treats scope as fixed by textual categories — intensity, organization, non-international character — and computes as a mountain or narrow rope depending on whether states truly follow the categories or use them as cover story. (2) The expansive human rights reading (constraint_id: common_article_3_scope__expansive_human_rights_reading) treats scope as determined by rights principles and minimum standards for any organized violence, computing as a tangled rope (coordination of humanitarian floor + asymmetric authority concentration) or snare (if rights framing is cover for NGO authority expansion). (3) This reading (icrc_customary_reading) treats scope as determined by evolved state practice and opinio juris, computing as a rope with rising extraction. The three readings share the kernel (CA3 text, 1949 Geneva Conventions Article 3) but instantiate different constraints because their ε referents differ: the standing arrangement under contest is PERCEIVED DIFFERENTLY by each reading's authority structure. No single reading is the 'real' constraint; the three are three distinct measurements of contested legitimacy at the same focal point. Network effects: the state-centric reading upstream influences this one (state practice is the raw material the customary reading interprets); this reading upstream influences the expansive reading (by claiming practice-based authority, it forecloses or constrains rights-based alternatives). The interdependencies are causal/epistemic, not architectural.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
