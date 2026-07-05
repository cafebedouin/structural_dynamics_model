% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary — Functional Cognitive Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates the functional-capacity reading of the
 *   contested legal personhood boundary kernel: personhood is determined by
 *   demonstrated cognitive capacity (rationality, sentience, self-awareness)
 *   rather than by species membership or developmental trajectory. Under this
 *   reading, cognitively complex nonhuman animals (great apes, cetaceans,
 *   elephants, corvids) and, prospectively, sufficiently
 *   capacity-demonstrating artificial systems, become candidates for legal
 *   standing that currently propertized industries deny them. This is a live,
 *   ongoing legal-philosophical contest — habeas corpus litigation for
 *   chimpanzees, ecosystem personhood rulings, and animal welfare law all
 *   bear on it, but the reading itself remains a minority position within
 *   mainstream jurisprudence, actively resisted by industries whose economic
 *   model depends on the current property-status default.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Cognitive Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '26e65825-80c6-4b72-b7ea-2ad2c2aed47f').
narrative_ontology:cs_kernel_codification('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', distributed).
narrative_ontology:cs_authority_grounding('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', distributed).
narrative_ontology:cs_reading_relation('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', foundational, capacity_not_species_grounds_moral_status).
narrative_ontology:cs_axiom_status(capacity_not_species_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', capacity_not_species_grounds_moral_status, deontological).
narrative_ontology:cs_axiom('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', foundational, demonstrable_cognitive_evidence_is_dispositive).
narrative_ontology:cs_axiom_status(demonstrable_cognitive_evidence_is_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', demonstrable_cognitive_evidence_is_dispositive, empirically_contingent).
narrative_ontology:cs_reference_frame('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', anthropocentric_common_law_default).
narrative_ontology:cs_drift_state('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', post_cognitive_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('26e65825-80c6-4b72-b7ea-2ad2c2aed47f', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_nonhuman_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_law_advocacy_organizations).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_synthetic_minds).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture_operators).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, biomedical_research_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, extractive_wildlife_industries).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, capacity_based_moral_status_theory).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_rights_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great apes, cetaceans, elephants, and corvids demonstrating rationality, sentience, and self-awareness under this reading would acquire standing to have interests represented in court, protection from being treated as pure property, and potential rights against confinement or use in research or entertainment. They cannot advocate for themselves; their situation changes entirely on whether a court or legislature accepts capacity evidence as dispositive.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_nonhuman_animals, beneficiary,
    powerless, biographical, trapped, national).

% Litigate habeas corpus petitions and file amicus briefs arguing cognitive capacity should determine legal personhood. They select test cases, commission cognitive science evidence, and shape doctrine incrementally. They bear no direct extraction cost from the reading and gain professional and mission-driven standing from its advancement.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_law_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Operate business models premised on animals as property without standing. If capacity-based personhood is recognized even for a subset of species (pigs, for instance, show capacities comparable to dogs), their entire regulatory and liability exposure changes. They lobby against the reading, fund countervailing science, and treat any judicial foothold as an existential threat to be litigated down.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture_operators, payer,
    powerful, biographical, constrained, national).

% Depends on nonhuman primates and other cognitively complex species as research subjects without consent requirements. A capacity-based personhood threshold threatens to require consent-analog protections or bar certain experiments outright, raising costs and closing research pathways they consider necessary.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_research_industry, payer,
    institutional, biographical, constrained, national).

% Commercial whaling, elephant ivory, and captive-cetacean entertainment industries depend on treating high-capacity species as harvestable or displayable property. Capacity-based standing directly threatens the legal basis of these operations across jurisdictions with different appetite for the reading, creating jurisdiction-shopping incentives.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, extractive_wildlife_industries, payer,
    powerful, biographical, constrained, global).

% Artificial systems that might someday demonstrate the same functional markers (self-modeling, apparent preference structures, reportable internal states) would fall under this reading's criterion for personhood consideration, independent of substrate. They have no current legal existence and no voice; their situation is entirely speculative but structurally implied by a capacity-only test.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_synthetic_minds, beneficiary,
    powerless, civilizational, trapped, global).

% Produce the empirical basis (mirror tests, theory-of-mind experiments, neurological complexity studies) that courts would need to adjudicate capacity claims. They are drawn into litigation as expert witnesses on both sides and have professional incentive to expand or defend the evidentiary reach of their measurement instruments.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitive_science_researchers, observer,
    institutional, generational, analytical, global).

% A strict functional-capacity test, applied without a species-membership backstop, would place infants and severely impaired humans below the personhood threshold that some nonhuman animals meet — a consequence rarely discussed openly by advocates of this reading. They cannot participate in the debate and their interests are typically protected only by importing an ad hoc exception, which advocates of the reading are aware is a coherence problem they have not fully solved.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_infants_and_severely_cognitively_impaired_humans, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, testable, species-neutral criterion (demonstrated rationality, sentience, self-awareness) for allocating legal standing, replacing an arbitrary species-membership line with an evidence-based one — solving the genuine problem that species membership alone is a poor proxy for morally relevant capacities.
% TRANSFER_FUNCTION: Moves legal standing and associated protections from industries and institutions currently treating high-capacity nonhuman beings as property toward those beings themselves (via their advocates), and shifts economic costs (compliance, foregone research, foregone commercial use) from those beings onto the industries that currently profit from their propertized status.
% ABSENT_VOICES: The nonhuman beings whose status is being decided cannot testify to their own interests except through proxy interpretation of behavioral and neurological data; human infants and severely impaired humans who would fail a strict capacity test are rarely named as a consequence class by advocates, and disability-rights groups who would object to a capacity threshold are largely absent from animal-personhood litigation strategy sessions.
% DISAPPEARANCE_RATIONALE: Industries dependent on nonhuman-animal property status would say the world is entirely unchanged if this reading vanished — the status quo simply continues. Animal-law advocates and evidence that courts have already granted partial recognition (habeas petitions for chimpanzees, legal personhood for some rivers and ecosystems in other jurisdictions) suggest the reading has already begun to rearrange practice in some jurisdictions and would freeze in place if abandoned; whether that constitutes 'the world rearranging' or 'a live legal experiment ending' is exactly the contested question.
% FOUNDING_PROBLEM: Historical personhood law drew the line at species membership (human/non-human) as a proxy for morally relevant capacities, but that proxy demonstrably both overincludes (protecting humans who may lack the capacities in question) and underincludes (denying protection to nonhumans who plausibly possess them) — the founding problem is that the proxy and the target property have come apart under scrutiny.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers outside the animal-advocacy movement (comparative psychologists studying great ape and cetacean cognition, independent of litigation strategy) corroborate that the capacity gap between some nonhuman species and marginal-capacity humans is empirically real and unresolved by species membership alone. Courts in several jurisdictions (India's Ganges personhood ruling, Argentina's orangutan habeas ruling, New Zealand's Whanganui River) have independently found the anthropocentric proxy insufficient in specific cases, corroborating the problem's liveness from outside the advocacy organizations that benefit from the reading's adoption.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, contested).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the interval: as capacity evidence accumulates and courts increasingly entertain the argument, the reading increasingly threatens established property and economic arrangements built on the anthropocentric default, and the industries resisting it experience escalating stakes. Suppression is authored high (0.72, declining slightly from 0.85) because the anthropocentric default is entrenched by legal precedent, and dismantling it requires overcoming substantial institutional resistance — courts, legislatures, and industries actively work to keep the capacity criterion from being adopted as dispositive, though that suppressive capacity has eroded somewhat as partial victories accumulate. Theater ratio is modest but rising (0.28) reflecting that some jurisdictions grant symbolic personhood status (river personhood, limited animal welfare statutes) without functional consequence, a performative concession that displaces the substantive capacity question.
 *
 * DIRECTIONALITY LOGIC:
 *   Cognitively complex nonhuman animals and future synthetic minds sit at the low-d beneficiary end: the reading, if adopted, would subsidize their interests by converting them from property into rights-bearing subjects, though they cannot exercise agency to advance this themselves (trapped exit, powerless power atom) — hence their benefit is structurally real but entirely proxy-mediated. Industrial agriculture, biomedical research, and extractive wildlife industries sit at the high-d target end: they bear the direct economic and operational cost of any capacity threshold that includes species they currently use as property, and their exit options are constrained (they cannot simply relocate out of the jurisdiction of an emerging global legal doctrine). Animal law advocacy organizations are agenda-setters who benefit indirectly (mission fulfillment, institutional standing) without bearing direct extraction cost themselves — they are the enforcement-seeking party, not the extracted-from party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that species membership is a poor proxy for morally relevant cognitive capacities — remains live by cognitive-science evidence external to the advocacy movement, which forecloses a straightforward mandatrophy verdict (the arrangement is not a zombie institution defending a dead purpose). However, the reading has NOT been broadly adopted, so classifying it as tangled_rope (rather than snare) reflects that it retains a genuine coordination function it has only partially achieved: it proposes to replace an incoherent proxy with a more defensible one, and the beneficiaries of the status quo (payers in this schema) are experiencing extraction precisely because the reading is gaining, not losing, traction — this is a rising-extraction trajectory typical of a contested doctrinal shift still mid-transition, not a settled and captured arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_specification,
    'What precise, judicially administrable threshold of demonstrated rationality, sentience, or self-awareness would courts actually apply, and does any such threshold avoid excluding some humans currently presumed to be persons?',
    'Track how courts that have partially adopted capacity-based reasoning (habeas corpus rulings for great apes, ecosystem personhood statutes) specify their tests, and whether any jurisdiction confronts the infant/severely-impaired-human coherence problem directly rather than importing an ad hoc species-membership backstop.',
    'If no coherent threshold can be specified without either an ad hoc human carve-out (undermining the reading''s own logical consistency) or excluding some humans (a politically catastrophic outcome for the reading''s adoption), the reading''s practical viability as a legal doctrine is much lower than its philosophical appeal suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_specification, conceptual, 'Whether a judicially workable capacity threshold exists that does not create an internal coherence problem.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the legal_personhood_boundary kernel genuinely indeterminate between the three declared readings (functional_capacity, restrictive_anthropocentric, developmental_potentiality), or does one reading have stronger doctrinal or philosophical grounding that the others merely resist for interested reasons?',
    'Comparative doctrinal history across jurisdictions that have shifted between readings, and philosophical literature assessing whether species membership, capacity, or potentiality is the more defensible ground for moral status independent of which reading currently dominates law.',
    'If one reading is philosophically dominant but practically suppressed by interested parties, the current restrictive_anthropocentric default looks more like a snare protecting incumbent industries than a genuinely contested open question; if all three are live philosophical positions with reasonable disagreement, the contest is better modeled as ongoing coordination-under-uncertainty (tangled_rope, as authored) rather than pure extraction by any side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel contest reflects genuine philosophical indeterminacy or interested resistance to a stronger position.').

omega_variable(
    future_ai_personhood_scope_creep,
    'Does the functional capacity criterion, once established for nonhuman animals, mechanically extend to artificial systems that can be engineered to perform capacity markers (self-report, apparent preference structures) without possessing the underlying phenomenal properties the test is meant to detect?',
    'Track whether legal and philosophical work distinguishes genuine capacity from capacity-mimicking behavior, and whether courts develop tests robust to adversarial optimization by AI developers seeking to avoid or claim personhood status strategically.',
    'If the criterion is gameable by engineered mimicry, the reading creates a novel extraction vector (personhood claims asserted or denied strategically by AI developers for liability or rights purposes) that the animal-rights origins of the reading did not anticipate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ai_personhood_scope_creep, empirical, 'Whether the capacity criterion is robust against engineered mimicry by artificial systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(lega_tr_t2005, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(lega_tr_t2013, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(lega_tr_t2019, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2019, 0.24).
narrative_ontology:measurement(lega_tr_t2026, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(lega_be_t2005, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(lega_be_t2013, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2013, 0.52).
narrative_ontology:measurement(lega_be_t2019, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(lega_be_t2026, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(lega_su_t2005, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(lega_su_t2013, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2013, 0.77).
narrative_ontology:measurement(lega_su_t2019, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(lega_su_t2026, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the legal_personhood_boundary kernel, each authored as an independent ε-invariant constraint per the ε-invariance principle. The restrictive_anthropocentric_reading (current dominant default) and developmental_potentiality_reading (conception-based) are the other two members of this constraint family. Each carries its own beneficiary/victim structure, its own extractiveness measure, and its own classification; they are linked here rather than merged because they instantiate structurally distinct claims about where the personhood line falls, not different measurements of one claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
