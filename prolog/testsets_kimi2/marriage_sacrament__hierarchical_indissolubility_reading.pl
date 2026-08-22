% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage: Hierarchical Indissolubility Reading
 *   domain: religious/canon_law/political
 *
 * SUMMARY:
 *   The hierarchical indissolubility reading treats marriage as an
 *   ontological sacramental reality whose dissolution is impossible without
 *   hierarchical judgment of nullity. Under this reading, divorced and
 *   civilly remarried Catholics are excluded from Eucharistic communion
 *   unless they obtain a declaration of nullity through a canonical tribunal
 *   â a process that imposes financial costs, procedural delays, and
 *   psychological burdens. The constraint is one reading of the contested
 *   marriage_sacrament kernel; it forecloses the civic_pastoral_reading's
 *   premise that indissolubility is merely aspirational. The authored metrics
 *   describe a structure with high extraction and active enforcement, while
 *   the reading's own framework claims ontological necessity â the engine
 *   measures this divergence.
 *
 * KEY AGENTS:
 *   - church_hierarchy: Primary agenda-setter (institutional/global) â administers sacramental adjudication and enforces ontological indissolubility
 *   - divorced_catholics_seeking_remarriage: Primary target (powerless/identity_locked) â bears extraction through tribunal costs and Eucharistic exclusion
 *   - canonical_tribunal_staff: Secondary beneficiary (organized/regional) â collects fees and professional status from the annulment apparatus
 *   - progressive_theologians: Excluded voice (moderate/constrained) â advocates for pastoral discernment, structurally marginalized
 *   - catholic_scholars_observer: Analytical observer (analytical/global) â tracks divergence between ontological claims and institutional outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.84).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.78).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/canon_law/political").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'e68ccf56-61ff-4825-a3dc-f00a451b1e86').
narrative_ontology:cs_kernel_codification('e68ccf56-61ff-4825-a3dc-f00a451b1e86', formalized).
narrative_ontology:cs_authority_grounding('e68ccf56-61ff-4825-a3dc-f00a451b1e86', lineage).
narrative_ontology:cs_interpretation_layer_present('e68ccf56-61ff-4825-a3dc-f00a451b1e86').
narrative_ontology:cs_reading_relation('e68ccf56-61ff-4825-a3dc-f00a451b1e86', marriage_sacrament__civic_pastoral_reading, forecloses).
narrative_ontology:cs_axiom('e68ccf56-61ff-4825-a3dc-f00a451b1e86', foundational, indissolubility_constitutive_ontological).
narrative_ontology:cs_axiom_status(indissolubility_constitutive_ontological, holdable).
narrative_ontology:cs_axiom_grounding('e68ccf56-61ff-4825-a3dc-f00a451b1e86', indissolubility_constitutive_ontological, theological).
narrative_ontology:cs_axiom('e68ccf56-61ff-4825-a3dc-f00a451b1e86', foundational, sacramental_validity_requires_hierarchical_judgment).
narrative_ontology:cs_axiom_status(sacramental_validity_requires_hierarchical_judgment, holdable).
narrative_ontology:cs_axiom_grounding('e68ccf56-61ff-4825-a3dc-f00a451b1e86', sacramental_validity_requires_hierarchical_judgment, conventional).
narrative_ontology:cs_reference_frame('e68ccf56-61ff-4825-a3dc-f00a451b1e86', classical_sacramental_order).
narrative_ontology:cs_drift_state('e68ccf56-61ff-4825-a3dc-f00a451b1e86', contemporary_pastoral_discernment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e68ccf56-61ff-4825-a3dc-f00a451b1e86', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_staff).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the doctrine that marriage is an ontological sacramental reality whose indissolubility is constitutive, not merely aspirational. Retains exclusive authority to adjudicate marital validity through canonical tribunals and controls access to the Eucharist. Derives institutional power from being the sole legitimate interpreter of sacramental ontology.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, constrained, global).

% Administer annulment proceedings, collect tribunal fees, and apply canonical norms at the diocesan level. Their professional livelihood, canonical expertise, and role status depend on the hierarchical adjudication system remaining active, exclusive, and financially sustained.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_staff, beneficiary,
    organized, biographical, constrained, regional).

% Seek sacramental recognition of new unions and access to the Eucharist. Must submit to lengthy, costly canonical tribunal processes for a declaration of nullity or accept permanent exclusion from full sacramental participation. Exit is blocked by deep religious identity fusion, family ties, and community belonging; leaving the Church means forfeiting a core sense of self and social world.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Advocate for pastoral discernment and compassionate exceptions to absolute indissolubility. Their theological arguments are structurally marginalized in magisterial teaching and canonical practice under this reading; they are not party to the adjudication process and their proposals are treated as doctrinally out of bounds.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, progressive_theologians, excluded,
    moderate, generational, constrained, global).

% Analyze the canonical system as a socio-theological structure, tracking the divergence between ontological claims and institutional enforcement outcomes. They observe the asymmetry between the hierarchy's ontological rhetoric and the tribunal's bureaucratic operation without participating in the sacramental economy.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholic_scholars_observer, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Determines valid marriage for sacramental purposes through centralized hierarchical judgment, maintaining ontological consistency of indissolubility across the global Church.
% TRANSFER_FUNCTION: Moves money, time, and sacramental access from divorced Catholics seeking remarriage to canonical tribunals and the institutional hierarchy in exchange for adjudication of marital validity.
% ABSENT_VOICES: Civilly remarried Catholics who abandon the tribunal process due to cost or trauma, progressive theologians advocating pastoral exception, and divorced Catholics who have left the Church entirely and are no longer counted in pastoral statistics.
% DISAPPEARANCE_RATIONALE: Without the constraint, divorced and remarried Catholics would access the Eucharist without tribunal gatekeeping, canonical tribunal revenues would collapse, and the hierarchy's exclusive claim to adjudicate sacramental reality would dissolve â the internal economy of Catholic marriage would reorganize around pastoral or civil norms.
% FOUNDING_PROBLEM: Establishing a uniform, reliable mechanism to determine valid Christian marriage and prevent serial unions that would undermine the Church's public witness to indissolubility amid diverse local customs and political arrangements.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the problem remains live, citing secular individualism and family breakdown. Progressive theologians and sociologists of religion attest the problem has shifted: the system now primarily manages institutional boundary maintenance rather than pastoral care. Independent canonical statistics and sociological studies from outside the beneficiary parties support the boundary-maintenance reading.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the annulment process imposes significant financial and temporal costs, and the denial of Eucharist represents severe spiritual extraction. Suppression is high (0.78) because persistence depends on actively denying sacraments to those who bypass the tribunal and on identity-locking that makes exit psychologically costly. Theater is moderate (0.45): the canonical process has real procedural content, but an increasing share of activity performs ontological maintenance rather than genuine pastoral discernment. Accessibility collapse is high because, within the Catholic identity frame, no alternative path to full sacramental participation exists. Resistance is moderate because excluded Catholics and progressive theologians contest the arrangement but lack institutional power to alter it.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy experiences the constraint as necessary ontological guardianship (low d, potentially negative effective extraction as institutional authority is reinforced). The divorced Catholic payer experiences it as an extraction gate blocking sacramental access (high d, high effective extraction). The tribunal staff sit in between, deriving professional existence from the gate. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The church_hierarchy is the structural beneficiary and agenda-setter: the constraint concentrates sacramental adjudication in their hands, giving them a monopoly over a core life transition and its spiritual consequences. The canonical_tribunal_staff are secondary beneficiaries, collecting fees and professional status. Divorced_catholics_seeking_remarriage are the structural payers: they bear the financial, temporal, and spiritual costs of the process and its exclusions. Progressive theologians are structurally excluded from the conversation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â determining valid marriage amid diverse customs â is contested as to whether it remains live. The constraint prevents mislabeling by requiring both beneficiaries (the hierarchy, tribunal staff) and victims (divorced Catholics), with active enforcement (denial of sacraments). If the coordination function were absent, the engine would compute a snare; if the extraction were absent, it would read as a rope. The hybrid structure is exactly the tangled rope signature: a genuine coordination function (sacramental validity determination) fused with asymmetric extraction (costs and exclusion borne by the divorced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (canonical denial of sacraments, tribunal barriers, and community shaming) or internalized (religious identity fusion making departure unthinkable)?',
    'Post-exit trajectory study: if Catholics who leave the Church for civil remarriage experience rapid relief from suppression, the mechanism is primarily structural; if guilt, shame, and identity loss persist independent of institutional contact for years, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit, making the constraint more extractive than it appears. If structural, removal of enforcement would rapidly reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    tribunal_coordination_or_extraction,
    'Does the canonical tribunal system primarily coordinate a genuine sacramental validity problem, or does it function as an extraction mechanism using coordination as cover?',
    'Comparative analysis of annulment rates, costs, and outcomes across dioceses and over time; correlation between tribunal fees and diocesan revenue stress; assessment of whether nullity determinations track doctrinal criteria or pastoral and administrative pressures.',
    'If primarily extraction, reclassification toward snare is warranted and the coordination function is cover. If primarily coordination with incidental costs, the tangled_rope classification holds but the extraction metric should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_coordination_or_extraction, empirical, 'Coordination function versus extraction cover in tribunal system').

omega_variable(
    kernel_reading_foreclosure,
    'Does the hierarchical_indissolubility reading absolutely foreclose the civic_pastoral reading within the Catholic canonical framework, or can both coexist as live theological options under magisterial ambiguity?',
    'Analysis of magisterial documents (especially post-2016 apostolic exhortations), canonical practice variance across regions, and official responses to dubia: if magisterial teaching explicitly rules out pastoral discernment on indissolubility, foreclosure holds; if magisterial texts accommodate both frames regionally, the relation is better classified as coexists_with.',
    'If foreclosure is not absolute, the constraint''s legitimacy is contested from within the tradition, increasing resistance and potentially shifting the effective classification toward a less stable tangled_rope with higher theater. If absolute, the hierarchical reading functions as a closed commitment system with stronger suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Relationship between sibling readings of the marriage_sacrament kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
