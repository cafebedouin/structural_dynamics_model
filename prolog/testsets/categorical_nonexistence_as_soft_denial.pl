% ============================================================================
% CONSTRAINT STORY: categorical_nonexistence_as_soft_denial
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_nonexistence_as_soft_denial, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: categorical_nonexistence_as_soft_denial
 *   human_readable: Categorical Nonexistence as Soft Denial
 *   domain: bureaucratic/institutional epistemology/procedural justice
 *
 * SUMMARY:
 *   A bureaucratic table processes claims by mapping them onto a finite
 *   schema of categories. When a claim does not fit any category — Ostrun's
 *   legally fused identity, an unrecorded child born outside the registry's
 *   anticipated family structures — the system does not evaluate and reject
 *   the claim; it lacks any row to record it in. The outcome is 'not seated'
 *   or 'no page exists,' phrased in the same procedural language used for
 *   routine closures, so that structurally unrelated petitioners receive an
 *   identical non-decision. There is no named officer, no adversary, no cited
 *   rule, and therefore no locatable point from which an appeal could be
 *   filed. The coordination function (fast, consistent processing of the
 *   great majority of claims that DO fit) is real; the extraction
 *   (petitioners bearing the harm of institutional incompleteness with zero
 *   recourse) rides on the same schema and is inseparable from it without
 *   redesigning the intake apparatus itself — hence tangled_rope rather than
 *   pure snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_nonexistence_as_soft_denial, 0.68).
domain_priors:suppression_score(categorical_nonexistence_as_soft_denial, 0.79).
domain_priors:theater_ratio(categorical_nonexistence_as_soft_denial, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_nonexistence_as_soft_denial, extractiveness, 0.68).
narrative_ontology:constraint_metric(categorical_nonexistence_as_soft_denial, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(categorical_nonexistence_as_soft_denial, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(categorical_nonexistence_as_soft_denial, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(categorical_nonexistence_as_soft_denial, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_nonexistence_as_soft_denial, tangled_rope).
narrative_ontology:human_readable(categorical_nonexistence_as_soft_denial, "Categorical Nonexistence as Soft Denial").
narrative_ontology:topic_domain(categorical_nonexistence_as_soft_denial, "bureaucratic/institutional epistemology/procedural justice").

domain_priors:requires_active_enforcement(categorical_nonexistence_as_soft_denial).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_nonexistence_as_soft_denial, the_table_as_institution).
narrative_ontology:constraint_victim(categorical_nonexistence_as_soft_denial, petitioners_with_unclassifiable_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(categorical_nonexistence_as_soft_denial, ostrun_the_fused_claimant).
narrative_ontology:constraint_victim(categorical_nonexistence_as_soft_denial, the_unrecorded_child).
narrative_ontology:constraint_vindicates(categorical_nonexistence_as_soft_denial, procedural_completeness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the master schema of categories a claim must fit into before any officer can act on it. When a claim does not map onto an existing category, the table does not deny it — it simply has no row to write the claim into, so no decision is ever recorded, no officer is named as having ruled, and no appeal docket can be opened. The table's authority rests on appearing procedurally complete; every claim it can seat looks handled, and every claim it cannot seat simply vanishes from view rather than counting against its record.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, the_table_as_institution, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(categorical_nonexistence_as_soft_denial, the_table_as_institution, beneficiary).

% Bring claims — a nonstandard family fusion, a birth outside the categories the registry recognizes — that do not map onto any existing intake category. They receive a 'not seated' or 'no page exists' notice using language indistinguishable from routine administrative closure. There is no named decision-maker, no cited rule, no adversary to contest, and therefore no forum in which to appeal. Their only path forward is to petition for a new category to be created, a process the table itself controls and is not obligated to initiate.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, petitioners_with_unclassifiable_claims, payer,
    powerless, biographical, trapped, local).

% Process the forms in front of them using the categories the table provides. When a claim doesn't fit, they issue the standard 'not seated' notice because that is the only tool available to them; they did not design the schema and cannot expand it. Individually they bear little responsibility, but collectively their compliance is what makes the foreclosure look like routine paperwork rather than a decision anyone made.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, clerks_and_intake_officers, observer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(categorical_nonexistence_as_soft_denial, clerks_and_intake_officers, agenda_setter).

% A claimant whose identity resulted from a legally and socially real fusion event has no category in the registry's schema — the system has 'person,' 'merger of estates,' and 'joint filing,' but nothing that captures the specific structure of the fusion. The claim is not evaluated and rejected; it is told no page exists on which to record it. Ostrun cannot appeal a decision that was never made.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, ostrun_the_fused_claimant, payer,
    powerless, biographical, trapped, local).

% Born under circumstances the birth registry's categories do not anticipate — multiple simultaneous claimed parentage, a jurisdictional gap, or a family structure the intake schema has no field for — the child's existence is administratively unrecorded rather than disputed. The identical 'not seated' procedural language appears here as in Ostrun's case, despite the claims being structurally unrelated, revealing that the foreclosure mechanism is the schema itself, not any judgment about the individual claim's merit.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, the_unrecorded_child, payer,
    powerless, generational, trapped, local).

% Would be positioned to compel the table to expand its categorical schema or create an appeal path for uncategorized claims, but rarely hears about these cases because there is no denial record to trigger oversight review — the absence of a decision-point means there is nothing on the docket to escalate. Their absence from the process is structural, not a choice to ignore the problem.
narrative_ontology:constraint_stakeholder(categorical_nonexistence_as_soft_denial, legislative_oversight_committee, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(categorical_nonexistence_as_soft_denial, the_table_as_institution).
narrative_ontology:fixing_cost_class(categorical_nonexistence_as_soft_denial, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The categorical schema lets the table process large volumes of claims efficiently by routing each one to a predefined procedure, avoiding case-by-case adjudication for the overwhelming majority of claims that do fit existing categories.
% TRANSFER_FUNCTION: Moves the cost of institutional incompleteness from the table (which would otherwise have to build new categories, adjudicate novel claims, or admit fault) onto petitioners whose claims fall outside the schema — they absorb the labor, delay, and harm of nonexistence with no compensating forum.
% ABSENT_VOICES: Petitioners with unclassifiable claims are, by construction, not represented in any decision record — there is no docket entry, no named respondent, and no adversarial proceeding in which their objection could be lodged. Oversight bodies that might otherwise intervene never see these cases because nothing is flagged as denied.
% DISAPPEARANCE_RATIONALE: If categorical nonexistence as an outcome class were eliminated — if every claim had to be either seated or actively denied with a named decision and reviewable rationale — the table would be forced to expand its schema continuously, create appeal pathways for edge cases, and accept a visible failure rate. Its current appearance of procedural completeness depends on structurally unrelated claims being funneled into the same unappealable non-outcome.
% FOUNDING_PROBLEM: The schema was originally built to give clerks a finite, tractable set of categories so that routine claims could be processed quickly and consistently without requiring case-by-case judicial-style review for every filing.
% FOUNDING_PROBLEM_CORROBORATION: The table's own administrators attest the schema still serves its throughput function for the vast majority of claims. Petitioners' advocates and the legislative oversight committee's occasional ad hoc reviews (triggered by media attention to cases like Ostrun's) attest that the schema now also functions as an unaccountable denial mechanism for a growing and structurally diverse set of claims — a function no one designed but which the table has no incentive to fix, since fixing it would require admitting a decision was never made where an outcome nonetheless occurred.
narrative_ontology:disappearance_verdict(categorical_nonexistence_as_soft_denial, world_rearranges).
narrative_ontology:founding_problem_status(categorical_nonexistence_as_soft_denial, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(categorical_nonexistence_as_soft_denial, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(categorical_nonexistence_as_soft_denial, 'none', 1).
narrative_ontology:epsilon_provenance(categorical_nonexistence_as_soft_denial, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_nonexistence_as_soft_denial_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(categorical_nonexistence_as_soft_denial, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(categorical_nonexistence_as_soft_denial_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction climbs over the measured interval (0.42 to 0.68) as the volume and diversity of unclassifiable claims grows faster than the schema is updated, widening the gap between claims the table can seat and claims it silently forecloses. Theater ratio rises in parallel (0.30 to 0.58) because the 'not seated' notice increasingly substitutes procedural-sounding language for any actual adjudicative act — it performs the appearance of a decision without the substance of one. Suppression is high throughout and rises further (0.60 to 0.79) because the absence of a decision-point is itself the suppression mechanism: there is no adversary to contest, no rule to challenge, no forum to escalate to. Accessibility collapse is authored high (0.72) because once a petitioner understands there is no category for their claim, the ordinary avenues of appeal, review, or advocacy simply do not attach to anything — they must instead petition for the schema itself to change, a wholly different and much higher-cost undertaking. Resistance is moderate (0.44): petitioners do resist, often publicly and sympathetically (as with Ostrun and the unrecorded child), but resistance has no structural purchase because there is no decision to resist against.
 *
 * PERSPECTIVAL GAP:
 *   From the table's seat, the schema is a genuine and necessary coordination mechanism — without categorical bounds, no claim could ever be processed efficiently, and the 'not seated' outcome is simply an honest report that the claim does not (yet) fall within the institution's mandate. From the petitioner's seat, the identical outcome is an unappealable denial dressed in the language of a non-event: the harm is real and total, but the institution bears no visible responsibility for causing it. The engine's per-seat computation should reflect this divergence directly from the structural data — the table's arbitrage-grade exit and institutional power push its type toward coordination, while the petitioners' trapped exit and powerless position push the same structure toward extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The table benefits from a schema that always appears complete: every claim it can seat is efficiently processed, and every claim it cannot seat disappears from its record rather than counting as a failure or a denial it must justify. This gives the table d near the beneficiary end. Petitioners with unclassifiable claims bear the full cost of the schema's incompleteness with no compensating mechanism — trapped exit options and powerless structural position push their d toward the full-target end. Clerks and intake officers occupy a narrower middle position: they enforce the schema but did not design it and have no power to expand it, so their directionality is closer to symmetric even though their compliance is what operationalizes the foreclosure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving clerks a tractable, finite set of categories so routine claims could be processed without case-by-case adjudication — remains partly live: most claims genuinely do fit the schema and are processed efficiently. But the schema has also acquired a second, unintended function: it operates as a zero-accountability denial mechanism for claims that fall outside it, a function nobody designed and which the table has no structural incentive to correct, since fixing it requires admitting that outcomes have been occurring without any decision being made. Classifying this as tangled_rope rather than snare preserves the reality of the coordination function for the majority of claims while still naming the asymmetric extraction inflicted on the minority whose claims do not fit — collapsing it to pure snare would erase the genuine efficiency the schema provides for the modal case; collapsing it to pure rope would erase the documented, repeated, cross-claimant harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    schema_incompleteness_negligence_vs_design,
    'Is the categorical schema''s incompleteness an unavoidable byproduct of any finite classification system, or has the table had ample opportunity and demonstrated capacity to expand the schema and simply declined to do so?',
    'Audit of how frequently and how quickly the table has historically added new categories in response to documented unclassifiable claims, compared to the rate at which such claims arise.',
    'If the table has consistently failed to expand the schema despite clear, repeated demonstration of need, the constraint drifts toward snare (deliberate extraction via engineered incompleteness). If schema expansion genuinely lags behind the unpredictable diversity of real claims despite good-faith effort, the tangled_rope characterization holds more cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schema_incompleteness_negligence_vs_design, empirical, 'Whether the schema''s gaps reflect negligence/design or unavoidable classificatory limits.').

omega_variable(
    appeal_pathway_existence_ambiguity,
    'Does a genuine, if obscure, pathway exist for petitioners to compel schema expansion (e.g., via legislative petition or judicial mandamus), or is the ''petition for a new category'' option itself illusory in practice?',
    'Case-outcome tracking: how many petitions for new categories have been filed, how many succeeded, and over what time horizon, compared to the volume of unclassifiable claims.',
    'A functioning, if slow, pathway would mitigate the suppression score and support keeping this as coordination-with-friction; a pathway that is nominally available but never successfully used would support treating the suppression as effectively total.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appeal_pathway_existence_ambiguity, empirical, 'Whether the nominal schema-expansion process is a real recourse or theater.').

omega_variable(
    the_table_agenda_setter_or_captured_bureaucracy,
    'Is ''the_table_as_institution'' a unified beneficiary intentionally preserving its appearance of completeness, or is it better modeled as a diffuse bureaucratic apparatus with no single actor who benefits, making this closer to a piton than a tangled_rope?',
    'Trace whether any identifiable office or role captures budget, prestige, or reduced liability specifically from the low visible denial rate the schema produces.',
    'If a concentrated beneficiary exists (e.g., leadership whose performance metrics are the closure rate), tangled_rope is correct. If the benefit is truly diffuse and no one profits, this would be better modeled as a piton with the table as agenda_setter absorbing only diffuse institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(the_table_agenda_setter_or_captured_bureaucracy, conceptual, 'Whether the beneficiary is concentrated (tangled_rope) or diffuse (piton).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_nonexistence_as_soft_denial, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cate_tr_t0, categorical_nonexistence_as_soft_denial, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cate_tr_t4, categorical_nonexistence_as_soft_denial, theater_ratio, 4, 0.36).
narrative_ontology:measurement(cate_tr_t8, categorical_nonexistence_as_soft_denial, theater_ratio, 8, 0.42).
narrative_ontology:measurement(cate_tr_t12, categorical_nonexistence_as_soft_denial, theater_ratio, 12, 0.47).
narrative_ontology:measurement(cate_tr_t16, categorical_nonexistence_as_soft_denial, theater_ratio, 16, 0.51).
narrative_ontology:measurement(cate_tr_t20, categorical_nonexistence_as_soft_denial, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cate_tr_t24, categorical_nonexistence_as_soft_denial, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(cate_be_t0, categorical_nonexistence_as_soft_denial, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cate_be_t4, categorical_nonexistence_as_soft_denial, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(cate_be_t8, categorical_nonexistence_as_soft_denial, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(cate_be_t12, categorical_nonexistence_as_soft_denial, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(cate_be_t16, categorical_nonexistence_as_soft_denial, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(cate_be_t20, categorical_nonexistence_as_soft_denial, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(cate_be_t24, categorical_nonexistence_as_soft_denial, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cate_su_t0, categorical_nonexistence_as_soft_denial, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cate_su_t4, categorical_nonexistence_as_soft_denial, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(cate_su_t8, categorical_nonexistence_as_soft_denial, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(cate_su_t12, categorical_nonexistence_as_soft_denial, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(cate_su_t16, categorical_nonexistence_as_soft_denial, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(cate_su_t20, categorical_nonexistence_as_soft_denial, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(cate_su_t24, categorical_nonexistence_as_soft_denial, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_nonexistence_as_soft_denial, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(categorical_nonexistence_as_soft_denial, 0.12).
narrative_ontology:affects_constraint(categorical_nonexistence_as_soft_denial, administrative_appeal_standing_requirements).
narrative_ontology:affects_constraint(categorical_nonexistence_as_soft_denial, vital_records_categorical_schema).

% DUAL FORMULATION NOTE:
% This story isolates the coordination/extraction hybrid produced specifically by categorical nonexistence (absence of a decision-point) as distinct from ordinary contested denial (presence of a decision-point that can be appealed). A sibling story addressing explicit denial-with-appeal-rights within the same registry system would carry a different epsilon and belongs in a separate file linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
