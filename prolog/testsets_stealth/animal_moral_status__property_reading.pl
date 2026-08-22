% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Property Classification (Property Reading of Moral Status)
 *   domain: applied ethics/legal philosophy/animal studies
 *
 * SUMMARY:
 *   This story instantiates the property_reading of the animal_moral_status
 *   kernel: the standing arrangement under which animals are classified as
 *   chattel property, their interests subordinate to human interests by
 *   definition, and owner discretion the default rule limited only by waste
 *   and inefficiency norms. The epsilon referent is the standing
 *   property-classification arrangement, assessed by this reading's own
 *   lights, in which no morally cognizable taking occurs; the reading
 *   therefore authors very low extractiveness. Constraint family: the same
 *   referent carries two sibling stories
 *   (animal_moral_status__welfare_reading,
 *   animal_moral_status__abolitionist_reading) with different victim sets and
 *   higher reading-indexed epsilon; the stories are linked by
 *   network.affects_constraints, and the epsilon differences across the
 *   family are the point of the decomposition, not a defect in this file. KEY
 *   AGENTS (by structural relationship): - animal_property_owners: principal
 *   beneficiary (organized/mobile) — hold title and decide use -
 *   commercial_animal_industries: beneficiary and political administrator
 *   (institutional/arbitrage) — operate the bulk of use and fund the
 *   framework's defense - animal_product_consumers: incidental beneficiary
 *   (moderate/mobile) - legislatures_and_courts: agenda_setter
 *   (institutional/analytical) — define and adjudicate the category -
 *   animals_as_class: excluded (powerless/trapped) — the class the
 *   arrangement classifies, present only as objects of title -
 *   animal_protection_movements: excluded (organized/constrained) — contest
 *   from outside the decision structure - applied_ethics_legal_scholars:
 *   analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.06).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.18).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Property Classification (Property Reading of Moral Status)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied ethics/legal philosophy/animal studies").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '17923732-1076-4c45-8035-2b6cf6b09eec').
narrative_ontology:cs_kernel_codification('17923732-1076-4c45-8035-2b6cf6b09eec', formalized).
narrative_ontology:cs_authority_grounding('17923732-1076-4c45-8035-2b6cf6b09eec', lineage).
narrative_ontology:cs_interpretation_layer_present('17923732-1076-4c45-8035-2b6cf6b09eec').
narrative_ontology:cs_reading_relation('17923732-1076-4c45-8035-2b6cf6b09eec', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('17923732-1076-4c45-8035-2b6cf6b09eec', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('17923732-1076-4c45-8035-2b6cf6b09eec', foundational, animals_subordinate_to_human_interests_by_definition).
narrative_ontology:cs_axiom_status(animals_subordinate_to_human_interests_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('17923732-1076-4c45-8035-2b6cf6b09eec', animals_subordinate_to_human_interests_by_definition, deontological).
narrative_ontology:cs_axiom('17923732-1076-4c45-8035-2b6cf6b09eec', secondary, owner_discretion_default_rule).
narrative_ontology:cs_axiom_status(owner_discretion_default_rule, holdable).
narrative_ontology:cs_axiom_grounding('17923732-1076-4c45-8035-2b6cf6b09eec', owner_discretion_default_rule, conventional).
narrative_ontology:cs_reference_frame('17923732-1076-4c45-8035-2b6cf6b09eec', natural_chattel_order).
narrative_ontology:cs_drift_state('17923732-1076-4c45-8035-2b6cf6b09eec', contemporary_animal_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('17923732-1076-4c45-8035-2b6cf6b09eec', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, commercial_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_dominion_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, chattel_classification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as chattels: farm livestock, companion animals, working animals. Decide housing, breeding, transport, sale, and killing without needing justification beyond ownership, subject only to waste-discipline and ordinary property law. Exit is easy in the relevant sense: they can sell, gift, or cease keeping animals and lose nothing but the asset.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_property_owners, beneficiary,
    organized, biographical, mobile, global).

% Operate the large-scale use of animals: intensive agriculture, biomedical research, entertainment, breeding. The classification secures their entire asset base, since their inventory consists of owned animals. They also fund legislative defense of the classification, model-agriculture statutes, and challenge-management measures, and can shift capital across sectors and jurisdictions if rules tighten in any one of them.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, commercial_animal_industries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__property_reading, commercial_animal_industries, agenda_setter).

% Purchase meat, dairy, eggs, leather, wool, and other animal products at prices shaped by unconstrained use. Benefit incidentally from the arrangement's output without administering it. Substitution toward plant alternatives is available but bounded by habit, culture, price, and availability.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Define the property category in statute and code, adjudicate ownership disputes, and decide the reach of welfare overlays. Treat the classification as the default background of private law; revisions arrive as refinements (anti-cruelty statutes, sentience clauses) that leave the category itself intact.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Are the class of beings the arrangement assigns to the category of owned things. Every aspect of their housing, handling, breeding, transport, and killing is decided by titleholders. They appear in the arrangement only as objects of ownership records, contracts, and insurance schedules. No procedural channel exists through which their condition enters decision-making except insofar as it registers in owner self-interest or public sentiment.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_as_class, excluded,
    powerless, biographical, trapped, global).

% Campaign for welfare statutes, enforcement, and ultimately status change from outside the arrangement's decision structure. Obtain standing only when legislatures or courts grant it, which is episodic and partial. Direct action exposes members to prosecution under property, trespass, and challenge-management statutes, so their leverage is confined to persuasion, litigation funded by donors, and ballot measures where available.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_protection_movements, excluded,
    organized, generational, constrained, global).

% Analyze the classification's history, doctrinal structure, and moral arguments across the full range of positions. Publish critiques and reconstructions; hold no decision power over the category but supply the vocabulary in which its contest is conducted.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, applied_ethics_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, commercial_animal_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates exclusive decision-rights over animals, including use, breeding, transfer, and killing, to titled owners, resolving rivalrous human claims over the same animals and enabling markets, credit secured on animal assets, insurance, and long-horizon investment in herds and facilities.
% TRANSFER_FUNCTION: Moves decision-control over animals' bodies, labor, reproduction, and lives to titleholders, and moves the products of animal use (meat, milk, eggs, labor, data, companionship) from owners to purchasers, with the classification itself ensuring nothing is owed back to the animals.
% ABSENT_VOICES: Animals, the class whose status the arrangement fixes, hold no seat in any legislature, court, or market where the classification is made; they are represented only derivatively, through owner self-interest or advocate argument. Animal-protection movements speak from outside the decision structure with no veto. Future generations bearing the ecological externalities of intensive use are likewise unrepresented in the arrangement's operation.
% DISAPPEARANCE_RATIONALE: If the classification vanished overnight, the legal form of pet ownership, livestock farming, biomedical research colonies, zoos, racing, and breeding would dissolve simultaneously: food systems would reorganize around new production and liability structures, research licensing would rebuild from scratch, and every contract treating an animal as an asset would become void or incoherent. The rearrangement would be total because the classification is the load-bearing legal form for an enormous share of the economy.
% FOUNDING_PROBLEM: Settled agricultural societies needed stable rules for exclusive control of valuable animals: who may use, breed, sell, or kill a given animal, and how herds could serve as collateral and inheritable wealth. Roman law classified animals as res; common law carried the chattel classification forward. The arrangement was built to secure exclusive human control over animals as productive assets.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship on Roman res classifications and common-law chattel doctrine attests the founding problem and its solution from outside the benefiting parties, and animal-law jurists across the welfare and abolitionist spectrum concede this historical function while disputing its sufficiency as a present-day justification. Corroboration exists for the problem's existence and continued performance; what is disputed, and only by the sibling readings, is whether performing it still requires assigning the category of property to sentient beings.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored from this reading's own seat and independently of the claimed type. Extractiveness 0.06: within the frame, use of owned animals is not a taking; the only discipline operating on owners is economic (waste is irrational, not wrongful), so almost nothing counts as extraction. Suppression 0.18: ordinary property enforcement plus, late in the interval, statutes managing direct-action challenges; suppression is a raw structural property and is not scaled by power or scope. Theater_ratio 0.35: a growing share of the arrangement's normative activity is humanitarian overlay (humane-slaughter, transport, enrichment codes) that largely codifies existing practice rather than binding it. Accessibility_collapse 0.68: once the definitional frame is accepted, alternatives are largely absorbed as refinements rather than replacements, but complete rejection of the frame remains conceptually live, so collapse is high but not total. Resistance 0.45: organized movements, litigation, and ballot measures meet the arrangement continuously. The temporal series share one grid (t=0..80): extractiveness is essentially flat, recording the frame's stability; theater rises monotonically with overlay accumulation; suppression_requirement rises late as challenge-management statutes appear. The claim (mountain, per the reading's presentation of the classification as natural and definitional) and the metrics are independent authored facts; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The owner, industry, and consumer seats experience the arrangement as the unremarkable background of ordinary economic life: title, sale, and use with no wrong anywhere in sight. The animal seat bears the arrangement's entire incidence while possessing, by the frame's own terms, no standing by which to register it. The observer seat sees the definitional move as the load-bearing wall: everything else follows from it. The engine computes these seats differently from the same structural data; the divergence between the animal seat's computed position and the frame's self-reported very-low extractiveness is expected output, not an authoring error.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (owners, industries, consumers) derive directionality near the beneficiary end; industries sit lowest with arbitrage-grade exit across sectors and jurisdictions. Animals carry no beneficiary or victim declaration in this reading, because the frame assigns them no standing; their directionality therefore derives from structural position alone: powerless, trapped, the arrangement's object, placing them nearest the full-target end. No directionality override is authored: the gap between the animal seat's structural directionality and the frame's refusal to count it is precisely the measurable content of this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, allocating exclusive control over valuable animals for agriculture, trade, and credit, remains live: property law performs it daily and no jurisdiction has abandoned the classification. Mandatrophy is not resolved, and the status=live x verdict=world_rearranges pairing is consistent, so no zombie flag is expected from the mismatch consumer. The drift risk sits in the overlay rather than the core: welfare statutes accumulating as humanitarian ornament (rising theater_ratio) could decay into inertial performance while the underlying classification persists untouched; the welfare_overlay_function omega tracks exactly this possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the property_reading of the animal_moral_status kernel; the welfare_reading and abolitionist_reading instantiate different constraints over the same referent. Which reading governs a given jurisdiction determines the victim set and the epsilon value entirely.',
    'Per-jurisdiction doctrinal enumeration: identify which readings are embodied in operative law (property default is universal; welfare overlays are widespread; abolitionist premises are nowhere operative as law) and track adoption events such as sentience clauses, habeas corpus petitions, and personhood initiatives.',
    'If a sibling reading becomes operative in a jurisdiction, that jurisdiction''s arrangement leaves this story''s epsilon bin entirely: welfare adoption raises epsilon to moderate and admits animals to the victim set; abolitionist adoption dissolves the property classification itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the animal_moral_status kernel; sibling readings change the victim set and epsilon over the identical referent.').

omega_variable(
    constructed_vs_natural_classification,
    'Is the subordination of animals to human interests a natural fact fixed by biology and history, or a constructed legal classification that identifiable beneficiaries maintain?',
    'Comparative jurisprudence and reclassification evidence: jurisdictions granting sentient-being status, court proceedings treating companion animals as more than chattels, and the transition economics of jurisdictions that tightened use rules. If the classification revises without civilizational cost, construction is established.',
    'If constructed, the mountain claim fails false-summit evaluation and the arrangement recomputes as an enforced construct with concentrated beneficiaries; if natural, the mountain classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_classification, empirical, 'Natural-law versus constructed ambiguity of animal property status (required omega for a mountain declaring beneficiaries).').

omega_variable(
    definitional_insulation,
    'Does the reading''s definitional move (''subordinate by definition'') make the arrangement unfalsifiable from inside, so that the authored very-low extractiveness reflects the frame''s refusal to count harms rather than their absence?',
    'External-accounting comparison: measure the arrangement''s effects using accounts the frame does not control (ecological, public-health, sentience science) and test whether the frame updates when those accounts conflict with its self-report.',
    'If the definition is doing all the work, the divergence between the animal seat''s computed full-target structural position and the frame''s very-low epsilon is itself the finding: the frame functions as an accounting device that excludes the affected class from the ledger, and classification should follow the structural data over the frame''s self-report.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_insulation, conceptual, 'Whether definitional framing insulates the arrangement from falsification by the entities it classifies.').

omega_variable(
    welfare_overlay_function,
    'Do welfare-statute overlays (humane-slaughter, transport, enrichment rules) constrain use in fact, or perform constraint while use intensity remains governed by owner economics?',
    'Dose-response audit: correlate overlay enforcement intensity with use-volume and welfare-outcome time series; distinguish statutes that bind operations from statutes that codify pre-existing practice.',
    'If overlays are largely performative, the rising theater_ratio series indicates the arrangement is accumulating ornamental limitation, supporting drift toward inertial performance in the overlay layer while the property core persists unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_overlay_function, empirical, 'Whether welfare overlays functionally limit use or theatrically decorate it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__property_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__property_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_moral_status__property_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(anim_tr_t60, observed).
narrative_ontology:measurement(anim_tr_t70, animal_moral_status__property_reading, theater_ratio, 70, 0.29).
narrative_ontology:measurement_basis(anim_tr_t70, observed).
narrative_ontology:measurement(anim_tr_t80, animal_moral_status__property_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement_basis(anim_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__property_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__property_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_moral_status__property_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement_basis(anim_be_t60, observed).
narrative_ontology:measurement(anim_be_t70, animal_moral_status__property_reading, base_extractiveness, 70, 0.06).
narrative_ontology:measurement_basis(anim_be_t70, observed).
narrative_ontology:measurement(anim_be_t80, animal_moral_status__property_reading, base_extractiveness, 80, 0.06).
narrative_ontology:measurement_basis(anim_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__property_reading, suppression_requirement, 10, 0.03).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.04).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__property_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.06).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.08).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_moral_status__property_reading, suppression_requirement, 60, 0.11).
narrative_ontology:measurement_basis(anim_su_t60, observed).
narrative_ontology:measurement(anim_su_t70, animal_moral_status__property_reading, suppression_requirement, 70, 0.14).
narrative_ontology:measurement_basis(anim_su_t70, observed).
narrative_ontology:measurement(anim_su_t80, animal_moral_status__property_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement_basis(anim_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the animal_moral_status kernel per the epsilon-invariance principle: 'the moral status of animals' is one colloquial label covering three structurally distinct arrangements. This story (property_reading) is the historical upstream frame: universal property default, no victim set, very low reading-indexed epsilon. The welfare_reading emerged inside property systems as regulated use with sentience recognized (moderate epsilon, animals partially admitted to the victim set). The abolitionist_reading rejects the classification itself (high epsilon, animals fully in the victim set). Each story carries its own epsilon, beneficiaries, and claimed type; the family links make the upstream frame's influence on both downstream readings traceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
