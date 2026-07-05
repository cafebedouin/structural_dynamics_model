% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at as Reciprocal Obligation Between Pharaoh and Governed
 *   domain: religious/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the reciprocity reading of the Ma'at kernel:
 *   Ma'at is understood as a mutual bargain in which the Pharaoh is genuinely
 *   bound by obligations of justice, granary provisioning, flood-control
 *   administration, and defense, and can genuinely fail those obligations.
 *   This is structurally distinct from the divine_mandate_reading (where the
 *   ruler embodies Ma'at and cannot violate it by definition — no reciprocal
 *   breach is even conceivable) and from the distributed_maintenance_reading
 *   (where the burden of upholding cosmic order is spread across all social
 *   stations, not concentrated as a crown-to-subject exchange). The
 *   reciprocity reading is the only one of the three that structurally
 *   licenses withdrawal of support — tax evasion, labor refusal, nomarch
 *   secession — as a legitimate (not merely rebellious) response to perceived
 *   royal failure, which is why intermediate-period collapse narratives are
 *   best read through this lens rather than the other two.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.42).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.48).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at as Reciprocal Obligation Between Pharaoh and Governed").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "religious/political philosophy").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '10bbf84f-3ed6-4c00-9b73-85a809be71e3').
narrative_ontology:cs_kernel_codification('10bbf84f-3ed6-4c00-9b73-85a809be71e3', distributed).
narrative_ontology:cs_authority_grounding('10bbf84f-3ed6-4c00-9b73-85a809be71e3', practice).
narrative_ontology:cs_interpretation_layer_present('10bbf84f-3ed6-4c00-9b73-85a809be71e3').
narrative_ontology:cs_reading_relation('10bbf84f-3ed6-4c00-9b73-85a809be71e3', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('10bbf84f-3ed6-4c00-9b73-85a809be71e3', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('10bbf84f-3ed6-4c00-9b73-85a809be71e3', foundational, pharaoh_subject_to_reciprocal_obligation).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('10bbf84f-3ed6-4c00-9b73-85a809be71e3', pharaoh_subject_to_reciprocal_obligation, conventional).
narrative_ontology:cs_axiom('10bbf84f-3ed6-4c00-9b73-85a809be71e3', foundational, breach_legitimates_withdrawal).
narrative_ontology:cs_axiom_status(breach_legitimates_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('10bbf84f-3ed6-4c00-9b73-85a809be71e3', breach_legitimates_withdrawal, conventional).
narrative_ontology:cs_reference_frame('10bbf84f-3ed6-4c00-9b73-85a809be71e3', old_kingdom_reciprocal_covenant).
narrative_ontology:cs_drift_state('10bbf84f-3ed6-4c00-9b73-85a809be71e3', first_intermediate_period_collapse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10bbf84f-3ed6-4c00-9b73-85a809be71e3', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh_and_court).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasant_cultivators).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, peasant_cultivators).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_balance_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, reciprocal_kingship_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers grain redistribution, judicial rulings, flood-control works, and military protection, and in turn claims tribute, corvee labor, and religious legitimacy from the population. Under the reciprocity reading, the Pharaoh is bound by Ma'at, not identical with it — failure to deliver justice or stability is a breach the court must answer for, not a logical impossibility.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_and_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_and_court, beneficiary).

% Administers temple estates, certifies royal ritual performance of Ma'at, and receives substantial land grants and offerings in exchange for validating the reciprocal bargain. Positioned to judge whether the Pharaoh has upheld his side, giving priests real leverage over royal legitimacy.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, temple_priesthood, agenda_setter).

% Farm Nile-flooded land, pay grain tax, and depend on royal granary redistribution during famine and on royal-sponsored irrigation and flood works. Under the reciprocity reading they retain a structural claim: manifest crop failure, famine mismanagement, or unjust taxation can be read as Ma'at violated, legitimating withdrawal of labor, tax evasion, or open unrest without impiety.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasant_cultivators, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, peasant_cultivators, beneficiary).

% Conscripted seasonally for monument construction, canal maintenance, and quarry work in exchange for state provisioning of food and (in principle) protection and afterlife favor. Bear the corvee obligation directly; their consent to the reciprocal bargain is assumed by custom rather than negotiated.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, biographical, trapped, regional).

% Administer nomes on the Pharaoh's behalf and are positioned to break away or ally with rival claimants when the center fails to hold up its end (as during the First and Second Intermediate Periods). Their capacity for withdrawal is central to the reciprocity reading's enforcement mechanism but they are rarely voiced directly in surviving royal or temple sources.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_nomarchs, excluded,
    powerful, biographical, constrained, regional).

% Records tax assessments, court judgments, and famine relief, producing the documentary record (Instruction texts, tomb autobiographies, wisdom literature) that both attests to and adjudicates whether reciprocal obligations were met.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, scribal_bureaucracy, observer,
    moderate, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, scribal_bureaucracy, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mutual-obligation framework: subjects supply labor, grain tax, and deference; the crown supplies judicial order, famine relief, flood-control infrastructure, and defense — a genuine exchange that solves large-scale resource coordination along the Nile.
% TRANSFER_FUNCTION: Moves grain surplus, corvee labor, and tax revenue upward from cultivators and laborers to the crown and temples, and moves (in principle, conditionally) justice, granary relief, irrigation works, and military protection back down.
% ABSENT_VOICES: Provincial nomarchs and rank-and-file laborers rarely speak in the surviving record — the reciprocity claim is documented almost entirely by scribes and priests attached to the court, whose own material position depends on the bargain being seen as honored.
% DISAPPEARANCE_RATIONALE: Attested historical collapse periods (First and Second Intermediate Periods) show that when the reciprocal claim was widely perceived as breached — famine, weak central authority, foreign incursion unaddressed — nomarchs withdrew allegiance, taxation collapsed, and rival power centers formed; the reciprocity framework's disappearance is not hypothetical but has an attested historical instance.
% FOUNDING_PROBLEM: Coordinating irrigation, famine relief, justice administration, and defense along a river valley whose agricultural surplus depends on unpredictable Nile flood levels, requiring central authority that individual villages could not provide alone.
% FOUNDING_PROBLEM_CORROBORATION: Royal and temple sources (Instruction of Merikare, Loyalist Instruction) attest the bargain is being honored; but tomb autobiographies from intermediate-period local officials (e.g. Ankhtifi) and administrative correspondence from famine years attest breach and withdrawal from outside the primary beneficiary set, corroborating that the reciprocity claim was contestable in its own time, not merely by modern retrojection.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (peaking near 0.46 at t=300, settling to 0.42) reflecting a ceiling: reciprocity norms cap what the crown can extract before triggering legitimacy crisis, unlike an unconditional-mandate reading which would tolerate unbounded extraction. Suppression rises through the middle period (peak 0.58) as bureaucratic and priestly machinery hardens to enforce compliance and interpret ambiguous cases of 'breach,' then eases as the framework matures into settled custom. Theater ratio rises moderately (peak 0.35) as royal ritual performance (jubilee festivals, temple inscriptions proclaiming justice done) increasingly substitutes for verifiable delivery of famine relief and judicial access, particularly under weaker rulers.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and court sit near the beneficiary end structurally (control administration, capture surplus) but are NOT full beneficiaries under this reading — the reciprocal bargain means genuine downside exposure (loss of legitimacy, withdrawal of provincial support) distinguishes them from a divine-mandate Pharaoh who bears no such structural risk. Peasant cultivators and corvee laborers sit toward the target end (trapped exit, bear the tax/labor burden directly) but retain a partial exit valve the other readings deny them: legitimated resistance when reciprocity is perceived as breached. This is the structural delta the kernel context specifies — moderate extraction ceiling, not unbounded, precisely because the reciprocity frame gives the governed a recognized (if weak) lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading resists mandatrophy misclassification in both directions: it does not let genuine coordination (flood-control administration, famine granaries, judicial order) collapse into 'pure extraction dressed as duty,' because real services are structurally required of the crown and their absence is recognized as breach; but it also does not let the arrangement default to unquestioned tribute the way the divine_mandate_reading would, because the obligation is bilateral and enforceable through withdrawal, not merely through royal self-definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_divine_mandate_framing,
    'Do the surviving royal and wisdom-literature sources (e.g. Instruction of Merikare, Loyalist Instruction, Coffin Texts) support a genuinely bilateral reading of Ma''at, or does the reciprocity language function rhetorically within a framework where the Pharaoh''s embodiment of Ma''at is never actually in doubt?',
    'Close philological comparison across periods: does language describing royal failure (e.g. Ipuwer, Admonitions literature) treat failure as a genuine breach of a bilateral norm, or as cosmic disorder flowing FROM the ruler without implying the ruler had failed a distinct external standard? Textual continuity/discontinuity across the Intermediate Periods would be evidence.',
    'If sources support only rhetorical bilateralism nested inside an unconditional mandate frame, this reading would collapse toward the divine_mandate_reading and the ''legitimated resistance'' structural delta would not hold historically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_mandate_framing, conceptual, 'Whether the reciprocity framing is a genuinely separate normative structure or a rhetorical register within divine mandate.').

omega_variable(
    extraction_ceiling_enforceability,
    'Was the ''moderate extraction ceiling'' actually enforced by real withdrawal mechanisms (tax evasion, nomarch secession, labor refusal), or is the ceiling a modern historiographic inference from collapse episodes that had other primary causes (climate-driven Nile failure, foreign invasion)?',
    'Correlate documented instances of reduced central tax yield or provincial autonomy assertion against independently attested indicators of perceived royal failure (famine texts, absence of major building projects, temple donation records) versus against Nile flood-level proxy data (sediment records) to separate reciprocity-driven withdrawal from purely material collapse.',
    'If withdrawal correlates more strongly with material shocks than with perceived justice/reciprocity failure, the extraction ceiling in this reading is weaker than claimed and the constraint sits closer to unconditional extraction bounded only by capacity, not by norm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_ceiling_enforceability, empirical, 'Whether the reciprocity ceiling reflects genuine normative enforcement or coincides with unrelated material collapse.').

omega_variable(
    beneficiary_status_of_temple_priesthood,
    'Is the temple priesthood better modeled as a co-beneficiary of the reciprocal bargain (sharing extraction with the crown) or as an independent enforcement/arbitration layer whose interest is in bargain stability rather than extraction share?',
    'Examine temple land grant records and donation stelae across dynasties for correlation between temple wealth growth and periods of strong versus weak royal reciprocal performance.',
    'If priesthood wealth tracks royal extraction closely, temple_priesthood is correctly coded as co-beneficiary (as authored); if temple wealth is stable independent of royal performance, priesthood is better modeled as a neutral arbiter, weakening the tangled_rope classification toward a cleaner rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_temple_priesthood, empirical, 'Whether temple priesthood shares in extraction or functions as an independent stabilizing arbiter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(maat_tr_t200, maat_order_principle__reciprocity_reading, theater_ratio, 200, 0.28).
narrative_ontology:measurement(maat_tr_t300, maat_order_principle__reciprocity_reading, theater_ratio, 300, 0.35).
narrative_ontology:measurement(maat_tr_t400, maat_order_principle__reciprocity_reading, theater_ratio, 400, 0.32).
narrative_ontology:measurement(maat_tr_t500, maat_order_principle__reciprocity_reading, theater_ratio, 500, 0.3).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.34).
narrative_ontology:measurement(maat_be_t200, maat_order_principle__reciprocity_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(maat_be_t300, maat_order_principle__reciprocity_reading, base_extractiveness, 300, 0.46).
narrative_ontology:measurement(maat_be_t400, maat_order_principle__reciprocity_reading, base_extractiveness, 400, 0.44).
narrative_ontology:measurement(maat_be_t500, maat_order_principle__reciprocity_reading, base_extractiveness, 500, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement(maat_su_t200, maat_order_principle__reciprocity_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(maat_su_t300, maat_order_principle__reciprocity_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(maat_su_t400, maat_order_principle__reciprocity_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(maat_su_t500, maat_order_principle__reciprocity_reading, suppression_requirement, 500, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.15).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling readings of the maat_order_principle kernel, decomposed per the ε-invariance principle because the natural-language label 'Ma'at' conflates structurally distinct claims about where obligation sits and whether breach is conceivable. The divine_mandate_reading yields near-zero extraction ceiling risk (ruler cannot fail by definition — closer to mountain-like immunity from below); the distributed_maintenance_reading spreads obligation horizontally rather than concentrating it in a crown-subject exchange (weaker tangled-rope signature, more rope-like); this reciprocity_reading alone licenses a bilateral extraction ceiling enforced by legitimated withdrawal, producing the moderate, historically-attested extraction/suppression profile authored here. All three should be read together, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
