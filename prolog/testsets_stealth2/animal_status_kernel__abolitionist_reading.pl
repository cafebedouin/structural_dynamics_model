% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status Regime - Abolitionist Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the
 *   animal_status_kernel: animals are moral persons holding a basic right not
 *   to be property, property status is itself the injustice, and all use is
 *   categorically impermissible regardless of welfare conditions. Per the
 *   kernel-reading epsilon rule, the referent of extractiveness is the
 *   standing arrangement under contest - the global legal-institutional
 *   regime that classifies animals as property and licenses their use -
 *   assessed strictly by this reading's lights, under which every use
 *   violates a basic right and the victim set is the entire class of owned
 *   animals. The claimed type and the metrics are authored independently: the
 *   arrangement is claimed as tangled_rope because it demonstrably performs a
 *   real coordination function for its human participants (title, transfer,
 *   contract, veterinary and commercial standardization) while extracting
 *   totally from the animals passing through it under active legal
 *   enforcement; the metrics describe that operation as the abolitionist
 *   reading finds it. Sibling readings (property_reading, welfare_reading)
 *   are separate constraint files linked through network.affects_constraints;
 *   their structural deltas are recorded in omega variables rather than
 *   averaged into this one.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) - bear the arrangement's full cost; tens of billions of individuals annually
 *   - animal_agribusiness_operators: primary beneficiary and de facto agenda setter (institutional/arbitrage) - collects the largest revenue share and writes the standards its facilities are audited against
 *   - consumers_of_animal_products: mass beneficiary with incidental cost-bearing (moderate/mobile) - the demand side that sets the production quota
 *   - biomedical_research_institutions: secondary beneficiary (institutional/constrained) - locked into animal models by validation pathways
 *   - legislators_and_regulators: formal agenda setters with capture exposure (institutional/mobile) - administer the statutes, funded by the operator seat
 *   - abolitionist_advocates: excluded challengers (organized/identity_locked) - press the rights claim from outside the rooms where terms are fixed
 *   - animal_law_scholars: analytical observers (analytical/analytical) - see the full doctrinal structure at once
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.93).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.82).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status Regime - Abolitionist Reading").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '435a162a-5617-43a5-80a7-8a1f2f860479').
narrative_ontology:cs_kernel_codification('435a162a-5617-43a5-80a7-8a1f2f860479', distributed).
narrative_ontology:cs_authority_grounding('435a162a-5617-43a5-80a7-8a1f2f860479', distributed).
narrative_ontology:cs_reading_relation('435a162a-5617-43a5-80a7-8a1f2f860479', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('435a162a-5617-43a5-80a7-8a1f2f860479', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_axiom('435a162a-5617-43a5-80a7-8a1f2f860479', foundational, sentient_beings_have_right_not_to_be_property).
narrative_ontology:cs_axiom_status(sentient_beings_have_right_not_to_be_property, holdable).
narrative_ontology:cs_axiom_grounding('435a162a-5617-43a5-80a7-8a1f2f860479', sentient_beings_have_right_not_to_be_property, deontological).
narrative_ontology:cs_axiom('435a162a-5617-43a5-80a7-8a1f2f860479', foundational, all_animal_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(all_animal_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('435a162a-5617-43a5-80a7-8a1f2f860479', all_animal_use_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('435a162a-5617-43a5-80a7-8a1f2f860479', animals_as_moral_persons_not_property).
narrative_ontology:cs_drift_state('435a162a-5617-43a5-80a7-8a1f2f860479', contemporary_legal_order, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('435a162a-5617-43a5-80a7-8a1f2f860479', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agribusiness_operators).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, companion_animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, captive_entertainment_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, domesticated_companion_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, legislators_and_regulators).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the farms, feedlots, hatcheries, and processing chains that hold and slaughter the vast majority of domesticated animals. Set production standards, fund the lobbying that shapes animal legislation, and draft the welfare-audit templates their own facilities are audited against. Revenue from animal product sales flows to this seat; switching to plant or cultivated product lines is commercially available but currently less profitable, so exit is a portfolio decision rather than an escape.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agribusiness_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_agribusiness_operators, beneficiary).

% Hold and use animals in toxicology, disease modeling, and drug development. Regulatory submission pathways have historically required animal data, locking protocols in place; replacement methods exist for some assays and are advancing, but full substitution would require revalidating decades of accepted methodology. Grant funding and publication incentives reward continued animal work.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Acquire, keep, and care for animals as companions under the same property framework that governs livestock. They receive companionship and care relationships; the animals' status as owned objects means their fate - sale, euthanasia, relocation - rests with the owner. Individual exit (relinquishing ownership) is easy; the class-wide practice persists through affection and habit.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animal_owners, beneficiary,
    moderate, biographical, mobile, global).

% Buy meat, dairy, eggs, leather, and animal-tested products at prices supported directly by agricultural subsidies and indirectly by externalized environmental and health costs. Individual dietary exit is available and increasingly cheap; aggregate demand nonetheless sets the production quota the rest of the system fills. They also absorb some costs back as chronic-disease burden and ecological degradation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, consumers_of_animal_products, payer).

% Are bred into existence as inventory: confined, genetically selected for yield, transported, and slaughtered on schedules set entirely by the operators above them. Tens of billions of individuals per year pass through this position. They cannot exit, refuse, represent themselves, or accumulate; their interests enter human decision-making only as production parameters.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Are bred for and assigned to experimental protocols - toxicity dosing, surgery, behavioral deprivation - under oversight boards whose members share the property premise. They cannot decline, withdraw, or appeal; protocol approval by humans is the entirety of their procedural existence.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Are held in zoos, aquaria, circuses, and film production for display and performance. Conditions vary widely; the common feature is lifetime confinement for human viewing. A few jurisdictions have banned specific exhibits, but the class remains globally lawful.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, captive_entertainment_animals, payer,
    powerless, biographical, trapped, global).

% Live as owned dependents: their breeding, sale, sterilization, movement, and killing are owner decisions bounded only by cruelty statutes that penalize the manner, not the fact, of ownership. Breeding industries produce them to specification; shelters destroy the surplus.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, domesticated_companion_animals, payer,
    powerless, biographical, trapped, global).

% Campaign for the ending of animal property status itself - through scholarship, personhood litigation, undercover documentation, and boycott. Present in public discourse but largely absent from the legislative and standards committees where terms are fixed; several jurisdictions have responded to their documentation efforts with statutes restricting recording at facilities. Leaving the cause would forfeit the professional and moral identity built around it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% Write and enforce the animal statutes: cruelty floors, transport rules, slaughter-method mandates, research oversight. They receive campaign support and constituency pressure from the operator seat, and their regulatory templates are frequently drafted with industry input. Electoral exit is available; the statutory framework they administer persists across administrations.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legislators_and_regulators, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, legislators_and_regulators, beneficiary).

% Map the doctrinal structure - property classifications, standing rules, welfare-preemption clauses - and publish the comparative analyses that both movements cite. No enforcement or collection role; analytic distance is their position.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, animal_agribusiness_operators).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Property law coordinates human ownership, transfer, breeding, veterinary care, and commercial exchange of animals: clear title, enforceable contracts, standardized transactions across jurisdictions. It solves a real collective-action problem for the human parties who hold, trade, and depend on animals as assets.
% TRANSFER_FUNCTION: Moves the bodies, labor, reproductive capacity, and lives of animals - valued as assets - to human owners and industries; moves money from consumers to producers; places the entire moral and physical cost on the animals themselves, who receive nothing and cannot consent.
% ABSENT_VOICES: The animals themselves - the primary affected parties - have no voice in any forum that sets the terms; they cannot object, litigate, or vote. Their interests enter only filtered through human representatives, most of whom (industry, regulators, established welfare organizations) accept the property premise. Abolitionist voices exist in academia and activism but are excluded from the legislative and standard-setting rooms where the terms are actually fixed.
% DISAPPEARANCE_RATIONALE: If the property status of animals and the permission structure for their use vanished overnight, global food systems, biomedical research pipelines, pharmaceutical validation, clothing supply chains, and entertainment sectors would have to reorganize around plant-based and synthetic alternatives; trillions in assets would be repriced; dietary patterns would shift wholesale.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of governing human use of animals as resources: securing ownership, enabling trade, preventing theft, and standardizing the treatment of living assets across jurisdictions - extending the ancient institution of chattel property to sentient beings.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the benefiting parties corroborate the genealogy (chattel property extended to animals from Roman law through the common law), and animal-law scholars document the doctrine's continuity. No source outside the benefiting parties attests that the founding problem remains a genuine need rather than an artifact of the property premise: abolitionist scholars explicitly deny it, and even welfare-oriented legal scholars concede the property premise is doing the justificatory work. Corroboration exists for the historical existence of the problem, not for its continuing legitimacy.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.93, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.93 because, on this reading, the harm is not a surplus over some fair baseline - the baseline itself (holding persons as owned objects) is the violation, and the scale is expanding: terrestrial animal slaughter has roughly tripled since the interval opened. Suppression is 0.82 and is authored as a raw structural quantity, unscaled by power or scope: for the animal seats it is absolute (no exit, no refusal, no standing), and for human dissenters it operates through facility-recording bans, prosecution of rescuers, and the channelling of dissent into welfare framings that presuppose the property premise. Theater_ratio 0.62 reflects the welfare-certification boom - humane labels, audit industries, corporate pledges - which regulates the manner of use while entrenching its fact; a majority of the arrangement's visible moral activity is reassurance performance, though genuine inspection and amelioration continue. Accessibility_collapse is low (0.28): alternatives (plant-based foods, cultivated meat, non-animal assay panels) persist and multiply, and understanding the arrangement makes them more salient, not less. Resistance 0.58 tracks a growing rights movement, personhood litigation, and scholarly critique. All three temporal series run on one shared six-point grid, and the end-state values equal the scalar properties. Coalition note: the animal seats cannot form coalitions; the only coalition path runs through human proxies, which is why the excluded advocate seat carries structural weight.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the operator seat the arrangement is ordinary commerce - asset management with a compliance layer - and the welfare apparatus reads as due diligence. From the farmed-animal seat the same structure is the total condition of existence; there is no vantage from which it is partial. From the consumer seat it is background normality, visible only episodically. From the scholar seat the whole doctrinal machine is legible at once. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the operator, research, owner, and consumer seats toward the beneficiary end: revenue, labor, companionship, and products flow to them. Victim declarations place the four animal seats at the full-target end, and their trapped exit status pins them at the extreme, maximizing effective extraction; their global scope widens the verification problem and amplifies it further. Legislators sit dual: they administer the structure and collect campaign support from its largest beneficiaries, so their derived directionality sits nearer the beneficiary end than their neutral office alone would suggest. The advocate seat is excluded rather than seated in the derivation - it opposes the arrangement and remains commentary-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy here runs through the kernel contest rather than internal decay. The arrangement's founding mandate - governing animals as tradable assets - is neither dead nor plainly live: the benefiting parties attest it as a permanent governance need, while this reading and the wider rights literature attest it as an artifact of the property premise itself, hence contested. The classification discipline prevents two symmetrical errors. First, it blocks the pure-coordination mislabel: the welfare-regulatory surface looks like benign coordination, and the theater-ratio series exists precisely to catch the drift by which reassurance activity substitutes for the protective function. Second, it blocks the referent error of scoring the abolitionist demand instead of the standing arrangement - epsilon stays pointed at what exists. If the founding problem is eventually judged artifactual while the world still rearranges around the arrangement, the status-times-verdict mismatch fires and the arrangement re-reads as maintained by capture; if the problem is judged live, the tangled_rope claim stands with its heavy extraction term.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the abolitionist reading of animal_status_kernel; how would the property_reading or welfare_reading sibling change the structural classification of the same standing arrangement?',
    'Compile the sibling stories and compare victim-set membership, epsilon, and computed per-seat types across the three readings of the shared kernel.',
    'Under property_reading the victim set empties (animals are assets, not victims) and epsilon collapses toward the coordination floor; under welfare_reading the victim set narrows to suffering-salient cases and epsilon lands mid-range. The same arrangement classifies differently per reading - that divergence is the kernel contest itself, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed classification of the shared animal-status arrangement.').

omega_variable(
    welfare_reform_trajectory_effect,
    'Do incremental welfare reforms delay abolition (by entrenching use and lowering moral dissonance) or advance it (by building inspection infrastructure and shifting norms)?',
    'Longitudinal analysis of jurisdictions and firms after major welfare reforms: total animal use, demand elasticity of humane-labeled products, and conversion rates of welfare campaigns into rights-stage policy.',
    'If reforms entrench use, the arrangement''s coordination surface is purely stabilizing and the extraction-dominant reading strengthens; if they advance abolition, transitional dynamics appear inside the arrangement and the sunset question becomes live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_trajectory_effect, empirical, 'The live dispute between abolitionist and welfarist strategy over reform effects.').

omega_variable(
    personhood_threshold_within_reading,
    'Does this reading''s personhood attribution require only sentience, or further conditions such as autonomy or being a subject of a life - and which animals fall inside the victim set accordingly?',
    'Internal doctrinal analysis of the reading''s own texts (sentience-only positions versus richer subject-of-a-life criteria) applied to borderline classes: fish, decapods, insects.',
    'A narrower threshold shrinks the victim set and lowers effective extraction; a sentience-only threshold maximizes both. The reading''s internal boundary, not external criticism, sets the classification''s reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_threshold_within_reading, conceptual, 'Victim-set breadth depends on the reading''s internal personhood criterion.').

omega_variable(
    consumer_participation_mechanism,
    'Is mass consumer participation held in place by structural factors (price, subsidy, availability, habit infrastructure) or internalized factors (carnist ideology, dissociation)?',
    'Natural experiments where structural barriers drop: price parity of alternatives, default-option studies, cultivated-meat approval events - measure how much participation persists when friction is removed.',
    'If internalized, dismantling enforcement and subsidy machinery will not rapidly collapse participation and transition projections lengthen; if structural, removal of support collapses participation quickly and the arrangement''s persistence reads as enforced rather than chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_participation_mechanism, empirical, 'Structural versus internalized maintenance of the demand side.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.44).
narrative_ontology:measurement(anim_tr_t6, animal_status_kernel__abolitionist_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(anim_tr_t12, animal_status_kernel__abolitionist_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement(anim_tr_t18, animal_status_kernel__abolitionist_reading, theater_ratio, 18, 0.56).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__abolitionist_reading, theater_ratio, 24, 0.59).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.84).
narrative_ontology:measurement(anim_be_t6, animal_status_kernel__abolitionist_reading, base_extractiveness, 6, 0.86).
narrative_ontology:measurement(anim_be_t12, animal_status_kernel__abolitionist_reading, base_extractiveness, 12, 0.88).
narrative_ontology:measurement(anim_be_t18, animal_status_kernel__abolitionist_reading, base_extractiveness, 18, 0.9).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__abolitionist_reading, base_extractiveness, 24, 0.92).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(anim_su_t6, animal_status_kernel__abolitionist_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(anim_su_t12, animal_status_kernel__abolitionist_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(anim_su_t18, animal_status_kernel__abolitionist_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__abolitionist_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the animal question' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: the property_reading (epsilon near zero - no victims recognized, ownership rights are the frame), the welfare_reading (mid epsilon - suffering is the metric, property retained), and this abolitionist_reading (epsilon near maximum - the status itself is the violation, all owned animals are victims). Each file authors its own epsilon, victim set, and claimed type over the same standing arrangement; they are linked here because the upstream readings are cited as evidence within the downstream contest, and contamination propagates along those citations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
