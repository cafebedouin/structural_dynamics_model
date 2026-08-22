% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Legal Personhood Boundary — Functional Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story instantiates the functional-capacity reading of the legal
 *   personhood boundary kernel: personhood tracks demonstrable cognitive
 *   capacity — rationality, sentience, self-awareness — regardless of species
 *   or biological substrate. Under this reading, the coordination function is
 *   genuine (closing a real protection gap for cognitively complex non-human
 *   beings) but the reading's operation, once litigated and legislated,
 *   extracts substantial costs from industries whose economic models depend
 *   on treating cognitively complex animals as property. The reading is
 *   authored here on its own terms, assessed by its own lights, as one
 *   contestant among three readings of the same kernel (see kernel_context).
 *   ε is authored for the standing arrangement as this reading contests it —
 *   the partial, jurisdiction-specific expansion of capacity-based standing
 *   currently underway — not for the fully-realized capacity-rights regime
 *   the reading would install if fully adopted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.71).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'aef81863-1eea-4a1c-a7a1-0af4f4061df0').
narrative_ontology:cs_kernel_codification('aef81863-1eea-4a1c-a7a1-0af4f4061df0', distributed).
narrative_ontology:cs_authority_grounding('aef81863-1eea-4a1c-a7a1-0af4f4061df0', distributed).
narrative_ontology:cs_reading_relation('aef81863-1eea-4a1c-a7a1-0af4f4061df0', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('aef81863-1eea-4a1c-a7a1-0af4f4061df0', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('aef81863-1eea-4a1c-a7a1-0af4f4061df0', foundational, capacity_not_species_grounds_status).
narrative_ontology:cs_axiom_status(capacity_not_species_grounds_status, holdable).
narrative_ontology:cs_axiom_grounding('aef81863-1eea-4a1c-a7a1-0af4f4061df0', capacity_not_species_grounds_status, deontological).
narrative_ontology:cs_axiom('aef81863-1eea-4a1c-a7a1-0af4f4061df0', secondary, demonstrable_sentience_is_the_relevant_threshold).
narrative_ontology:cs_axiom_status(demonstrable_sentience_is_the_relevant_threshold, holdable).
narrative_ontology:cs_axiom_grounding('aef81863-1eea-4a1c-a7a1-0af4f4061df0', demonstrable_sentience_is_the_relevant_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('aef81863-1eea-4a1c-a7a1-0af4f4061df0', species_membership_grounds_status).
narrative_ontology:cs_drift_state('aef81863-1eea-4a1c-a7a1-0af4f4061df0', post_comparative_cognition_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('aef81863-1eea-4a1c-a7a1-0af4f4061df0', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_animal_advocacy_orgs).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ai_rights_theorists).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, great_apes_and_cetaceans_as_represented_class).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, factory_farming_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, biomedical_animal_research_sector).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, entertainment_and_zoo_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate habeas corpus petitions and legislative campaigns arguing that great apes, cetaceans, elephants, and corvids meet the cognitive-capacity threshold for legal personhood. They set the litigation agenda, choose test cases, and administer the standard's expansion through case-by-case argument. They gain standing, funding, and legal precedent from every successful capacity-based ruling.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_animal_advocacy_orgs, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_animal_advocacy_orgs, beneficiary).

% Cannot represent themselves; if the capacity standard is adopted they gain rights against confinement and use without having sought them. They have no exit from the legal system's classification decisions — their status is entirely determined by whether advocates succeed in court on their behalf.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, great_apes_and_cetaceans_as_represented_class, beneficiary,
    powerless, biographical, trapped, national).

% Operates on the legal premise that livestock are property, not persons. A capacity-based personhood boundary threatens the core economic model: pigs and some poultry show capacity markers that could support future personhood claims, exposing existing operations to bans, tort liability, or forced restructuring. Their exit is political mobilization and regulatory capture, not relocation of the underlying legal exposure.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, factory_farming_industry, payer,
    institutional, biographical, constrained, national).

% Depends on legal classification of research subjects as property/non-persons to conduct experimentation without consent frameworks. Primate and cetacean research programs are the most exposed; a capacity ruling could require consent-analog protections or ban whole research categories. Exit means relocating research to jurisdictions with narrower personhood standards, which is costly and reputationally fraught.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_animal_research_sector, payer,
    institutional, biographical, constrained, national).

% Confine cognitively complex animals for display and performance under property law. Capacity-based personhood would undermine the legal basis for confinement itself, not just its conditions. Their exit options are lobbying against precedent-setting cases or restructuring toward sanctuary/conservation models before compelled to.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, entertainment_and_zoo_industries, payer,
    powerful, biographical, constrained, national).

% Not yet demonstrated to meet the capacity thresholds under live contest, but the reading's own logic extends to any entity meeting the criteria regardless of substrate. No current AI system has standing to assert or contest this; the category is present in the reading's structure but has no seated representative yet.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_ai_systems, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__functional_capacity_reading, future_ai_systems).

% Adjudicate individual capacity claims case by case, weighing scientific testimony on cognition against existing property and tort frameworks. They do not originate the capacity standard but decide whether and how far it is incorporated into binding law.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled, species-neutral criterion for extending legal protection to any entity that can suffer, reason, or self-reflect, replacing an arbitrary species line with a testable capacity threshold — solving the problem of protection gaps for beings that are cognitively complex but taxonomically non-human.
% TRANSFER_FUNCTION: Moves legal standing and protection from a purely human-boundary allocation toward capacity-qualifying non-human entities, and correspondingly moves economic exposure, liability risk, and operational constraint from industries that currently treat those entities as property toward those same industries as payers.
% ABSENT_VOICES: The animals themselves have no voice in the proceedings that determine their status — they are represented by advocacy organizations whose institutional incentives (funding, precedent-building, movement growth) may not track the animals' actual interests precisely. Future AI systems are also structurally absent: the reading's logic applies to them, but no such system currently has any means of asserting or contesting a claim.
% DISAPPEARANCE_RATIONALE: Advocacy organizations and the represented-class beneficiaries would say the world rearranges completely — precedent-based protections for apes, cetaceans, and other capacity-qualifying animals would collapse back into pure property status overnight. Industry payers would say relatively little changes in the near term since the reading has achieved only partial, jurisdiction-specific adoption; most animals remain unaffected by the reading's existence either way. The dispute is genuine: the reading's actual bite varies enormously by jurisdiction and species.
% FOUNDING_PROBLEM: Traditional personhood law drew the line at species membership (human vs. non-human) or at birth, leaving cognitively sophisticated non-human beings — and by extension any non-human intelligence — with no legal standing regardless of demonstrated capacity for suffering, reasoning, or self-awareness. The functional capacity reading was built to close that gap by substituting a testable trait-based criterion for an arbitrary taxonomic one.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive ethologists and comparative psychologists outside the advocacy movement (e.g., researchers documenting self-recognition, tool use, and grief behavior in non-primate species) corroborate that capacity gaps between some non-human animals and marginal-capacity humans are empirically real and unresolved by species-based law. Bioethicists skeptical of animal-rights advocacy nonetheless generally agree the underlying capacity-classification problem is live and unresolved, even where they reject the proposed remedy — this is corroboration of the problem's liveness from outside the reading's own beneficiary set, not endorsement of the reading's solution.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, contested).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.35 to 0.71) as the reading moves from academic argument toward binding precedent in some jurisdictions (habeas corpus rulings for great apes, dolphin personhood proposals, evolving research-consent frameworks) — each successful precedent increases the exposure of property-based industries. Suppression is authored moderately high and rising (0.40 to 0.68) because industries facing exposure actively lobby against precedent-setting cases and fund countervailing scientific testimony minimizing animal cognition, which is itself a form of active suppression of the reading's advance rather than passive disagreement. Theater ratio rises modestly (0.20 to 0.40) reflecting that some capacity-based protections that get adopted are more symbolic than operationally binding (declarations of dolphin personhood with no enforcement teeth, corporate 'higher welfare' certifications that gesture at capacity recognition without altering underlying property status).
 *
 * DIRECTIONALITY LOGIC:
 *   Advocacy organizations are the structural agenda-setters and secondary beneficiaries — they gain precedent, funding, and movement capital from every capacity ruling, d near the beneficiary end. The represented animal class is the primary intended beneficiary but has zero agency in the process — pure recipient, trapped exit, extreme power asymmetry even though nominally the entity the reading exists to protect. Property-dependent industries are the targets: their entire economic exposure changes as a direct function of the capacity standard's legal uptake, d near the full-target end, and their exit options (relocating operations, lobbying, restructuring) are all costly and constrained rather than a genuine alternative to bearing the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a testable capacity criterion for extending protection beyond an arbitrary species line — remains live by outside corroboration (comparative cognition research), which prevents this from being classified as a zombie mandate merely serving advocacy-sector institutional interests. But the reading's tangled-rope structure is real: it genuinely coordinates around closing a demonstrated capacity gap AND simultaneously imposes concentrated, asymmetric costs on specific industries through the same legal mechanism — both halves must be held together rather than treated as either pure coordination (which would erase the real economic transfer) or pure extraction (which would erase the genuine ethical coordination problem the reading responds to).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_validity,
    'Are current cognitive-capacity tests (mirror self-recognition, tool use, theory-of-mind proxies, neurological correlates of sentience) valid, species-neutral measures of the morally relevant capacities, or are they anthropocentric proxies that happen to favor animals cognitively similar to humans while excluding radically different cognitive architectures (e.g., cephalopods, distributed insect colonies, or future non-biological intelligences)?',
    'Convergent validation across independent comparative-cognition research programs testing capacity markers against functional/behavioral outcomes in taxonomically diverse species, ideally including architectures very unlike primate cognition.',
    'If current tests are anthropocentric proxies, the functional_capacity_reading may reproduce a narrower version of the anthropocentric line it claims to replace, just drawn around species with brains structurally similar to human brains rather than around humans alone — undermining its claim to be a principled, species-neutral alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_validity, empirical, 'Whether capacity tests are genuinely species-neutral or covertly anthropocentric.').

omega_variable(
    kernel_framing_under_determination,
    'Is the legal_personhood_boundary kernel better understood as a single contested empirical/moral question (what grounds moral status) with three competing answers, or as three genuinely incommensurable framings that do not share enough conceptual ground to be considered readings of ''the same'' kernel at all?',
    'Cross-reading analysis of whether courts and legislatures that adopt one reading treat the others as live alternatives within the same legal question, or as answering different questions entirely (e.g., whether personhood-at-conception debates and animal-cognition debates ever appear in the same judicial reasoning, or occupy entirely separate doctrinal tracks).',
    'If the readings are incommensurable, the network linkage between this story and its siblings should be weakened or reframed as parallel constraints rather than true kernel siblings; if commensurable, the current linkage structure holds and reading_relations (coexists_with vs. forecloses) are correctly assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three declared readings genuinely share one kernel or are separately-evolved doctrines loosely grouped under a shared label.').

omega_variable(
    future_ai_seat_vacancy,
    'Should future AI systems be treated as a present-tense structural category in this reading''s victim/beneficiary analysis even though no candidate system currently meets or contests the capacity threshold, or is this seat''s inclusion premature speculation that overstates the reading''s current structural delta?',
    'Track whether any AI system generates a live legal claim or test case under a capacity-based standard within the story''s interval; absence of any such case by T=40 would support treating the category as latent rather than active.',
    'If the seat remains permanently vacant, the reading''s practical delta is fully accounted for by the animal-welfare industries alone, and the AI dimension is better treated as a distinct anticipatory constraint rather than folded into this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ai_seat_vacancy, conceptual, 'Whether the AI-inclusion implication of this reading is a live structural feature or a speculative extension with no current seated party.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This story, restrictive_anthropocentric_reading, and developmental_potentiality_reading are three readings of one contested kernel (legal_personhood_boundary). Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure: functional_capacity_reading extends standing to capacity-qualifying non-humans and imposes costs on animal-use industries; restrictive_anthropocentric_reading holds the line at born humans with capacity and imposes no such costs; developmental_potentiality_reading extends standing to all human life-trajectory holders including pre-birth and imposes costs on reproductive-autonomy-dependent parties instead. The three do not share an ε value — they are not the same constraint measured differently, they are three distinct constructed arrangements competing to occupy the same legal category.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
