% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Woodruff's 1890 Manifesto as Endogenous Revelatory Reinterpretation of Marriage Doctrine
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous_reinterpretation_reading of the
 *   marriage_commitment_reversal kernel: the claim that Wilford Woodruff's
 *   September 23, 1890 revelation constitutes a genuine internal doctrinal
 *   development — God's will reinterpreted under changed circumstances —
 *   rather than a doctrinal fiction covering external capitulation, and
 *   rather than a mere practice suspension leaving Section 132 doctrinally
 *   untouched. Under this reading, the coordination function is real
 *   (institutional survival requires a theologically coherent account of
 *   change) but the extraction is also real (theological consistency and the
 *   families who complied with the superseded instruction absorb the cost of
 *   preserving the institution's interpretive authority intact). The sibling
 *   readings (exogenous_override_reading, practice_doctrine_gap) are NOT
 *   represented here as alternative measurements of the same ε — they are
 *   different constraints with different beneficiary/victim structures,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter, administers the Manifesto and controls its framing as revelation
 *   - plural_marriage_practicing_families: bears the material and social cost of the reversal
 *   - federal_government: structurally present in history, structurally absent from this reading's own causal account
 *   - historians_and_dissenting_theologians: analytical observers assessing timing and correspondence evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.44).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Woodruff's 1890 Manifesto as Endogenous Revelatory Reinterpretation of Marriage Doctrine").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '9e4105aa-d3a3-499b-9922-f3375de42980').
narrative_ontology:cs_kernel_codification('9e4105aa-d3a3-499b-9922-f3375de42980', fixed_text).
narrative_ontology:cs_authority_grounding('9e4105aa-d3a3-499b-9922-f3375de42980', lineage).
narrative_ontology:cs_interpretation_layer_present('9e4105aa-d3a3-499b-9922-f3375de42980').
narrative_ontology:cs_reading_relation('9e4105aa-d3a3-499b-9922-f3375de42980', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e4105aa-d3a3-499b-9922-f3375de42980', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('9e4105aa-d3a3-499b-9922-f3375de42980', foundational, continuing_revelation_can_reverse_prior_revelation).
narrative_ontology:cs_axiom_status(continuing_revelation_can_reverse_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9e4105aa-d3a3-499b-9922-f3375de42980', continuing_revelation_can_reverse_prior_revelation, theological).
narrative_ontology:cs_axiom('9e4105aa-d3a3-499b-9922-f3375de42980', secondary, prophetic_sincerity_independent_of_external_causation).
narrative_ontology:cs_axiom_status(prophetic_sincerity_independent_of_external_causation, holdable).
narrative_ontology:cs_axiom_grounding('9e4105aa-d3a3-499b-9922-f3375de42980', prophetic_sincerity_independent_of_external_causation, conventional).
narrative_ontology:cs_reference_frame('9e4105aa-d3a3-499b-9922-f3375de42980', section_132_eternal_marriage_revelation).
narrative_ontology:cs_drift_state('9e4105aa-d3a3-499b-9922-f3375de42980', post_1890_manifesto, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9e4105aa-d3a3-499b-9922-f3375de42980', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_office_continuity).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_practicing_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_claim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency, through Wilford Woodruff, issues the 1890 Manifesto declaring an end to the sanctioning of new plural marriages, framed as received revelation. Leadership retains sole authority to declare what God's will now requires, preserving the interpretive office intact through the reversal rather than being discredited by it. They administer the transition, decide which existing plural marriages are tolerated, and control the narrative that the change is continuity of revelation rather than capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership, beneficiary).

% The abstract institutional claim that the prophetic office retains unbroken authority to receive binding revelation is preserved by framing the reversal as divine instruction rather than external defeat. This is not an actor but a doctrinal asset whose value is protected by the revelation framing.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_office_continuity, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_office_continuity).

% Families who entered plural marriages under prior doctrinal instruction now face a doctrine reversed out from under them: some marriages tolerated quietly, others pressured toward dissolution or secrecy, with social and legal precarity depending on local enforcement. They cannot appeal to the prior revelation without being cast as out of step with the current one; their prior compliance becomes a liability rather than a merit.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_practicing_families, payer,
    powerless, biographical, trapped, regional).

% The proposition that God's will as expressed through eternal principle (Section 132) does not change with circumstance absorbs the cost of the reversal: the change is either explained as circumstantial application of an unchanged principle, or the principle itself must be understood as revisable, either way straining the internal coherence claim the institution otherwise relies on.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_claim, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_claim).

% Applied escalating legal pressure (Edmunds-Tucker Act, disincorporation, disenfranchisement, property seizure) that created the material conditions under which the revelation arrived, but is written out of the endogenous framing entirely — the reading under analysis treats the reversal as internally generated, so the federal role is present in the historical record but absent from this constraint's own coordination story.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, arbitrage, national).

% Ordinary members receive continuity of institutional stability, property, and legal standing restored by the reversal, but must also absorb the doctrinal whiplash of having sustained a principle now suspended, and must accept the revelation framing on faith without independent verification of the leadership's private deliberations.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_membership, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_membership, payer).

% Examine correspondence, journals, and the timing of the Manifesto relative to federal legal pressure and legal case outcomes (e.g. the pending Idaho test-oath cases), assessing whether the revelation account is best read as genuine internal doctrinal development or as theological packaging applied after the substantive decision was already forced.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, historians_and_dissenting_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which the institution can change a costly, existentially threatening practice without conceding that its interpretive authority was ever fallible or externally compelled — coordinating continued member loyalty and institutional survival around a single authoritative reinterpretation event.
% TRANSFER_FUNCTION: Moves the cost of doctrinal inconsistency away from the institution's interpretive authority and onto the theological coherence claim itself, and moves the material and social cost of the reversal onto families who had complied with the prior instruction.
% ABSENT_VOICES: The federal government's coercive role is structurally excluded from this reading's own account of causation — the reading requires that the revelation be endogenous, so acknowledging exogenous pressure as determinative would collapse the reading into its sibling (exogenous_override_reading). Plural-marriage families whose lives were reorganized by the reversal are also not parties to the revelation's articulation.
% DISAPPEARANCE_RATIONALE: If the endogenous-revelation framing were dropped and replaced by an acknowledged pure external-coercion account, the institution's continuing-revelation doctrine would face direct reputational and doctrinal exposure; leadership and much of the membership would contest that anything meaningful had disappeared (the practice change would remain, only the account of its cause would shift), while historians and theological critics would say the entire legitimating apparatus around that specific 1890 event depends on the framing persisting.
% FOUNDING_PROBLEM: The Church faced simultaneous federal seizure of assets, disincorporation, and disenfranchisement of members tied directly to the practice of plural marriage, threatening institutional survival; a mechanism was needed to end the practice without admitting the prior revelation had been wrong or that the institution had capitulated to external force.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians (including LDS-affiliated historians working outside the apologetic tradition) and legal historians examining the Edmunds-Tucker Act's timeline and the pending Idaho test-oath litigation attest that the practical problem the Manifesto solved — institutional survival under federal legal assault — no longer exists in any form; the institution itself, from within the benefiting seat, continues to attest the founding problem as a matter of divine timing rather than legal necessity, so corroboration from outside the benefiting party is available but contested by the party itself.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, contested).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high because the reading grants that a genuine coordination problem existed (institutional survival) and that the revelation account, however convenient, is not obviously fabricated — it may be sincerely held. Theater ratio climbs sharply from 1874 to 1890 (0.30 to 0.60) reflecting the increasing gap between the institution's public framing of continuous, principled revelation and the mounting external pressure documented in the historical record; it remains elevated (0.58) through 1904 because the revelation framing continues to be maintained as official doctrine long after the acute crisis passed. Suppression is moderate and slightly declines post-1890 as the acute enforcement crisis resolves.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is continuity: the same prophetic mechanism that instituted plural marriage now discontinues it, and the office's authority is undiminished. From the payer seat (families who complied with the earlier instruction), the same event is experienced as their prior obedience being retroactively devalued — they followed a directive from the same authoritative channel that now reverses it, with no available appeal within the system's own terms. The engine should register this asymmetry structurally, not as competing opinions about one fact but as different real exposures to the same reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits near the beneficiary end: it retains and indeed reinforces its interpretive monopoly through the reversal, at low structural cost to itself. Plural-marriage-practicing families sit near the target end: trapped exit options, biographical time horizon, and no institutional standing to contest the reinterpretation — their prior compliance becomes evidence of the very obsolescence being erased. Rank-and-file membership sits closer to symmetric: they benefit from restored legal standing but pay in doctrinal disorientation and being asked to accept a private revelatory event on institutional authority alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal legal existential threat) is dead by any external measure, yet the revelation account built to solve it has itself become a permanent doctrinal artifact — continuing revelation as a general doctrine outlives the specific crisis it was invoked to resolve in 1890. This is the diagnostic case for R5: founding_problem_status = dead, but disappearance_verdict is contested rather than world_rearranges, because the institution's account of the event has become load-bearing for its present-day theological self-understanding independent of the original crisis. Treating this as a pure Mountain (unchangeable natural theological fact) would hide the beneficiary structure; treating it as a pure Snare would understate the real coordination problem the institution faced circa 1887-1890. Tangled Rope holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_causation,
    'Was the September 23, 1890 revelation a genuine independent theological development, or a post-hoc sacralization of a decision already forced by the Edmunds-Tucker Act, pending disincorporation, and the Idaho test-oath cases?',
    'Comparative analysis of the timing of Woodruff''s private journal entries against the federal legal calendar (asset seizure deadlines, appellate rulings) already exists in the historical record; the omega is not whether the data exists but whether internal sincerity and external causation are mutually exclusive as an analytical matter — a sincere revelation prompted by circumstance is still causally downstream of the circumstance.',
    'If read as substantially caused by external coercion, this story''s claimed_type and ε converge toward the exogenous_override_reading sibling, and the beneficiary structure shifts from ''prophetic authority preserved'' toward ''prophetic authority rescued by circumstance it did not control'' — a materially different extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causation, conceptual, 'Whether internal revelatory sincerity and external causal necessity are compatible or mutually exclusive framings of the same event.').

omega_variable(
    theological_consistency_cost_bearer,
    'Does the theological consistency claim (that Section 132 as eternal principle is unchanged even though its practical application reversed) genuinely resolve the tension, or does it merely relocate the tension into a permanently unresolved doctrinal ambiguity?',
    'Track subsequent official statements (1904 Second Manifesto, 20th-century doctrinal commentary) for whether the institution ever directly addresses the change-of-God''s-will question versus consistently deflecting to circumstantial application language.',
    'If the tension is genuinely unresolved rather than resolved, the ''victim'' status of theological_consistency_claim is stronger than a settled doctrinal accommodation, supporting sustained tangled_rope classification rather than eventual drift to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_consistency_cost_bearer, conceptual, 'Whether the eternal-principle/circumstantial-application distinction genuinely resolves or merely defers the consistency problem.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_layer,
    'Is the kernel here the 1886 revelation text (Section 132) itself, or is it the higher-order legitimacy claim that the prophetic office can authoritatively reinterpret prior revelation without self-contradiction? Different framings produce different cs_pattern readings.',
    'Compare how the institution treats challenges to Section 132''s literal content versus challenges to the office''s authority to reinterpret it — if reinterpretive authority is defended far more vigorously than the specific 1886 text, the legitimacy-layer framing is the operative kernel.',
    'If the operative kernel is the reinterpretive-authority claim rather than the specific revelation text, authority_grounding shifts weight toward ''extraction'' (institutional benefit from the authority to revise) over ''lineage'' (fidelity to a fixed transmitted text), which would change how axiom foreclosure is computed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_layer, conceptual, 'Whether the contested kernel is the specific 1886 revelation or the general prophetic-reinterpretation authority claim layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1852, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1852, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1852, 0.15).
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1862, 0.2).
narrative_ontology:measurement(marr_tr_t1874, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1874, 0.3).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1887, 0.45).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.6).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t1852, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1852, 0.28).
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1862, 0.33).
narrative_ontology:measurement(marr_be_t1874, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1874, 0.41).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1887, 0.49).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1852, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1852, 0.35).
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1862, 0.38).
narrative_ontology:measurement(marr_su_t1874, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1874, 0.42).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1887, 0.5).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.47).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the marriage_commitment_reversal kernel. endogenous_reinterpretation_reading (this file) treats the 1890 Manifesto as genuine internal doctrinal development; exogenous_override_reading treats it as external coercion without doctrinal revision (Section 132 preserved as principle, practice suspended by force); practice_doctrine_gap treats the doctrine/practice split itself as the structurally ambiguous object, prior to adjudicating either causal account. Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
