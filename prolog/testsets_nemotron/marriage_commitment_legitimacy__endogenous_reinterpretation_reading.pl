% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: LDS 1890 Manifesto as Prophetic Revelation
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   This constraint story models the endogenous reinterpretation reading of
 *   the 1890 Manifesto (Official Declaration 1) as a distinct constraint. In
 *   this reading, the Manifesto is genuine prophetic revelation: God
 *   commanded the cessation of plural marriage to preserve the Church for
 *   higher purposes, framing monogamy as the new covenantal stage. The
 *   constraint coordinates the faithful around theological continuity through
 *   dispensational adaptation. Federal pressure is the catalyst that
 *   occasioned revelation, not its cause. This is one of three structurally
 *   distinct readings of the marriage_commitment_legitimacy kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.25).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "LDS 1890 Manifesto as Prophetic Revelation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/political/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '754042e8-141d-443f-93f2-abdb4f17a899').
narrative_ontology:cs_kernel_codification('754042e8-141d-443f-93f2-abdb4f17a899', formalized).
narrative_ontology:cs_authority_grounding('754042e8-141d-443f-93f2-abdb4f17a899', lineage).
narrative_ontology:cs_interpretation_layer_present('754042e8-141d-443f-93f2-abdb4f17a899').
narrative_ontology:cs_reading_relation('754042e8-141d-443f-93f2-abdb4f17a899', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('754042e8-141d-443f-93f2-abdb4f17a899', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('754042e8-141d-443f-93f2-abdb4f17a899', foundational, manifesto_as_genuine_revelation).
narrative_ontology:cs_axiom_status(manifesto_as_genuine_revelation, holdable).
narrative_ontology:cs_axiom_grounding('754042e8-141d-443f-93f2-abdb4f17a899', manifesto_as_genuine_revelation, deontological).
narrative_ontology:cs_axiom('754042e8-141d-443f-93f2-abdb4f17a899', secondary, federal_pressure_as_catalyst_not_cause).
narrative_ontology:cs_axiom_status(federal_pressure_as_catalyst_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('754042e8-141d-443f-93f2-abdb4f17a899', federal_pressure_as_catalyst_not_cause, instrumental).
narrative_ontology:cs_axiom('754042e8-141d-443f-93f2-abdb4f17a899', foundational, monogamy_as_covenantal_progression).
narrative_ontology:cs_axiom_status(monogamy_as_covenantal_progression, holdable).
narrative_ontology:cs_axiom_grounding('754042e8-141d-443f-93f2-abdb4f17a899', monogamy_as_covenantal_progression, deontological).
narrative_ontology:cs_reference_frame('754042e8-141d-443f-93f2-abdb4f17a899', prophetic_succession_continuity).
narrative_ontology:cs_drift_state('754042e8-141d-443f-93f2-abdb4f17a899', post_manifesto_consolidation, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('754042e8-141d-443f-93f2-abdb4f17a899', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_prophetic_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_institutional_continuity).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_command_theology).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, monogamy_as_covenantal_progression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine author of revelation whose prophetic succession legitimacy is preserved through the Manifesto as genuine revelation. The Manifesto affirms that prophetic authority adapts covenantal requirements across dispensations while maintaining unbroken divine authorship.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_prophetic_authority, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_prophetic_authority).

% The institutional Church that receives and implements the Manifesto. Its identity is fused with prophetic succession; abandoning the revelation frame would dissolve its self-understanding as the restored church. It administers the transition from plural to monogamous marriage as covenantal progression rather than capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_institutional_continuity, agenda_setter,
    institutional, generational, identity_locked, global).

% Latter-day Saints who accept the Manifesto as revelation. Their religious identity is constituted through prophetic authority; the revelation frame preserves theological coherence and prevents the crisis of a prophet 'wrong' about divine command. They receive spiritual continuity and institutional survival as benefits.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership, beneficiary,
    organized, biographical, identity_locked, global).

% Those who continued plural marriage after 1890, rejecting the Manifesto as revelation. They are structurally excluded from institutional fellowship and view the revelation frame as the mechanism of their marginalization. Their objection would be that the Manifesto was political capitulation, not revelation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, post_manifesto_polygamists, excluded,
    powerless, biographical, constrained, local).

% The U.S. federal government whose anti-polygamy legislation (Edmunds Act, Edmunds-Tucker Act) created the existential crisis. In this reading, federal pressure is the catalyst that occasioned revelation, not its cause. The federal seat observes the institutional response but does not author the theological frame.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_authority, observer,
    institutional, generational, analytical, national).

% Analytical observer who sees the full structural field: how the revelation frame preserves prophetic legitimacy, maintains institutional continuity, and coordinates the faithful through a covenantal progression narrative. Neither collects nor pays within the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the theological coherence of prophetic succession by framing the transition from plural to monogamous marriage as divinely commanded covenantal progression rather than institutional capitulation. This coordinates the faithful around a continuous revelation narrative, preventing schism over whether a prophet can err on divine command.
% TRANSFER_FUNCTION: Moves interpretive authority over the Manifesto from political pressure to divine command. The arrangement transfers the burden of explanation from 'why did the Church yield to federal power?' to 'how does this revelation fit the pattern of dispensational adaptation?' No material resources transfer; the transfer is epistemic and legitimating.
% ABSENT_VOICES: Post-Manifesto polygamists (fundamentalist lineages) who rejected the revelation frame are structurally excluded. They would object that the Manifesto was political capitulation, not revelation, and that the revelation frame is the mechanism of their excommunication. Dissenting scholars who read the Manifesto as pragmatic accommodation are likewise marginalized in official discourse.
% DISAPPEARANCE_RATIONALE: If the endogenous revelation frame vanished overnight, the LDS Church would face an immediate legitimacy crisis: either the prophet capitulated to federal power (breaking prophetic infallibility) or God changed His mind without revelation (breaking revelation continuity). The institutional identity, membership coherence, and theological framework would require restructuring. Fundamentalist schisms would gain retrospective validation.
% FOUNDING_PROBLEM: The 1887-1890 existential crisis: federal legislation (Edmunds-Tucker Act) threatened to disincorporate the Church, seize its assets, imprison its leadership, and dissolve its legal existence. The Church faced institutional death unless plural marriage was abandoned — but abandoning it under duress would undermine the prophetic claim that plural marriage was an eternal, unchangeable divine commandment.
% FOUNDING_PROBLEM_CORROBORATION: The federal legislative record (Edmunds Act 1882, Edmunds-Tucker Act 1887) and Supreme Court decisions (Late Corp. v. United States 1890) corroborate the existential threat from outside the beneficiary set. The Church's own historical records (Woodruff's journal, First Presidency correspondence) confirm the crisis was perceived as existential. The founding problem (institutional survival under federal assault) is dead — the Church survived, federal pressure abated, and Utah achieved statehood.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.15) because the revelation frame primarily coordinates rather than extracts — the faithful receive theological coherence and institutional survival; the institution preserves its founding claim. Low suppression (0.25) because the constraint persists through internalized commitment to prophetic authority, not active coercion (excommunication of dissenters occurs but is not the primary maintenance mechanism). Low theater (0.12) because the coordination function (theological continuity) is genuine and the revelation frame is sincerely held by the agenda-setting institution. Accessibility collapse is moderate-high (0.65) because the revelation frame structurally closes the 'prophet erred' alternative for identity-locked members. Resistance is moderate (0.40) from excluded fundamentalist lineages and critical scholars.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (institutional continuity) and beneficiary (faithful membership) seats experience this as genuine coordination — the revelation solves the crisis of prophetic legitimacy. The excluded seat (post-Manifesto polygamists) experiences it as extraction-through-boundary-maintenance. The engine computes this divergence from the structural data: identity_locked exit for beneficiaries creates low directionality (subsidy), while constrained exit for excluded creates higher directionality. The analytical observer seat computes the structural type without collecting or paying.
 *
 * DIRECTIONALITY LOGIC:
 *   Divine prophetic authority (non-agent beneficiary) and LDS institutional continuity (agenda_setter, identity_locked) are structural beneficiaries — the revelation frame preserves their legitimacy. Faithful membership (beneficiary, identity_locked) receives spiritual continuity. Post-Manifesto polygamists (excluded, constrained) bear the cost of exclusion but are not the extraction target of this reading — their exclusion is a boundary maintenance effect, not the constraint's function. Federal authority (observer) provided the catalytic pressure but does not author the frame. The analytical observer sees the full coordination structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — the Church survived, Utah achieved statehood (1896), and anti-polygamy enforcement faded. Yet the constraint (revelation frame) persists because it became load-bearing for prophetic legitimacy itself, not just crisis management. This is not mandatrophy in the extractive sense — the constraint still coordinates genuine theological continuity — but it has outlived its founding catalyst. The endogenous reading denies mandatrophy by claiming the revelation was always about covenantal progression, not federal pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation_ambiguity,
    'Is the Manifesto''s status as revelation vs. capitulation structurally decidable, or is it irreducibly framed by the observer''s prior commitment to prophetic authority?',
    'Comparative analysis of Woodruff''s private writings (journal, letters) vs. public rhetoric; if private writings show strategic calculation without revelatory language, the revelation frame is constructed. If private writings show genuine revelatory experience, the frame is endogenous.',
    'If constructed, this constraint reclassifies toward tangled_rope or snare (extraction of legitimacy from crisis). If genuine, the rope classification holds with divine authority as beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_capitulation_ambiguity, conceptual, 'Whether the revelation frame is a genuine epistemic state or a constructed legitimacy cover.').

omega_variable(
    committer_framing_underdetermination,
    'Does this reading foreclose the exogenous_override_reading within a single commitment framework, or do they coexist as competing framings held by different parties?',
    'Test whether a single party can simultaneously hold ''the Manifesto is genuine revelation'' and ''the Manifesto was federal capitulation with doctrine unchanged.'' If logically incompatible, forecloses; if held by different factions (institutional vs. fundamentalist), coexists_with.',
    'If forecloses, the kernel has a hard structural split. If coexists_with, the kernel sustains plural readings across different commitment communities. The reading_relations declaration below records this reading''s assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Commitment-system framing under-determination: whether sibling readings are logically incompatible or sociologically coexistent.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds faithful membership to the revelation frame? Professional/ecclesiastical identity? Relational identity (sealing ordinances)? Ideological identity (prophetic infallibility)?',
    'Qualitative study of member narratives: which identity disruption (loss of sealing validity, prophetic error, institutional apostasy) most predicts exit vs. doubling down when confronted with historical counter-evidence.',
    'Different identity mechanisms produce different exit elasticities. If sealing/relational, exit is nearly impossible (identity_locked). If ideological, some mobility exists when prophetic infallibility is reinterpreted. This affects directionality computation for the faithful_membership seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Identity-lock mechanism specificity for faithful membership seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1887, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1887, 0.05).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.1).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.11).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.12).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tr_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.12).

% Extraction over time
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1887, 0.05).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.1).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.12).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.14).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_be_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1887, 0.15).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.2).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.22).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.24).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.25).
narrative_ontology:measurement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_su_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_temple_sealing_authority).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_priesthood_restriction_1978_revelation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the marriage_commitment_legitimacy kernel into three readings with distinct ε values and beneficiary/victim structures. The endogenous reading (this story) has low ε (0.15) with divine authority as beneficiary. The exogenous reading has high ε with federal authority as extractor. The hybrid reading has moderate ε with institutional pragmatism as coordinator. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, organized, 0.2).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
