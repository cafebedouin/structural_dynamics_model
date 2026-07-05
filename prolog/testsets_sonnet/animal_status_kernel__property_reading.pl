% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal-as-Property Legal Status (Property Reading of the Animal Status Kernel)
 *   domain: moral_philosophy/legal_theory/animal_ethics
 *
 * SUMMARY:
 *   This story instantiates the PROPERTY READING of the animal status kernel:
 *   animals are legal chattel, moral considerability is derivative of and
 *   exhausted by ownership rights, and economic value is the only value the
 *   legal system is equipped to register. This is one of three sibling
 *   readings of the same kernel (welfare_reading, abolitionist_reading —
 *   separate constraint files). Under the ε-invariance principle, this
 *   reading is generated as a clean, self-contained constraint: no hedging
 *   across readings, no averaged extraction. The property reading's
 *   extractiveness is high and rising because, structurally, there is no
 *   countervailing moral constraint operating inside this reading —
 *   anti-cruelty statutes that do exist historically function to protect
 *   owner property value and public sensibility (preventing 'waste' of a
 *   valuable asset, preventing offense to bystanders) rather than to
 *   recognize any interest the animal holds. As industrial animal use has
 *   intensified over the interval (confinement systems, industrial-scale
 *   slaughter, high-throughput research use), extraction has climbed from
 *   0.78 toward 0.91 with no internal mechanism to check it, because the
 *   reading provides none.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.91).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.72).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal-as-Property Legal Status (Property Reading of the Animal Status Kernel)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/legal_theory/animal_ethics").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'c47e515f-9232-4a98-a485-d8cf7df9ad5b').
narrative_ontology:cs_kernel_codification('c47e515f-9232-4a98-a485-d8cf7df9ad5b', formalized).
narrative_ontology:cs_authority_grounding('c47e515f-9232-4a98-a485-d8cf7df9ad5b', extraction).
narrative_ontology:cs_interpretation_layer_present('c47e515f-9232-4a98-a485-d8cf7df9ad5b').
narrative_ontology:cs_reading_relation('c47e515f-9232-4a98-a485-d8cf7df9ad5b', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('c47e515f-9232-4a98-a485-d8cf7df9ad5b', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('c47e515f-9232-4a98-a485-d8cf7df9ad5b', foundational, moral_status_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_status_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('c47e515f-9232-4a98-a485-d8cf7df9ad5b', moral_status_derives_from_ownership, conventional).
narrative_ontology:cs_axiom('c47e515f-9232-4a98-a485-d8cf7df9ad5b', foundational, economic_value_exhausts_relevant_value).
narrative_ontology:cs_axiom_status(economic_value_exhausts_relevant_value, holdable).
narrative_ontology:cs_axiom_grounding('c47e515f-9232-4a98-a485-d8cf7df9ad5b', economic_value_exhausts_relevant_value, instrumental).
narrative_ontology:cs_reference_frame('c47e515f-9232-4a98-a485-d8cf7df9ad5b', chattel_law_agrarian_baseline).
narrative_ontology:cs_drift_state('c47e515f-9232-4a98-a485-d8cf7df9ad5b', post_sentience_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c47e515f-9232-4a98-a485-d8cf7df9ad5b', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, livestock_industry_operators).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, fur_and_leather_producers).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, pet_breeding_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, commercial_animal_owners).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, confined_farm_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, laboratory_test_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, fur_bearing_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, working_and_companion_animals_under_neglectful_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate confinement systems (feedlots, battery cages, gestation crates) whose economics depend on treating animals as fungible capital assets with no legally cognizable interests beyond what preserves resale/production value. Lobbies to keep anti-cruelty statutes narrow and to keep animal welfare claims out of standing doctrine. Benefits directly from every marginal unit of confinement intensity that a genuine interest-holder could contest.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, livestock_industry_operators, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, livestock_industry_operators, agenda_setter).

% Use animals as experimental instruments under IACUC review that asks only whether procedures are 'scientifically justified,' not whether the animal has a claim against being used at all. Property framing lets institutional cost-benefit review substitute entirely for any animal-side interest weighing.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_research_institutions, beneficiary,
    institutional, generational, mobile, national).

% Extract economic value from animal bodies at the endpoint of the ownership chain; property status means the killing itself requires no moral justification beyond compliance with commercial slaughter regulation, which exists to standardize product quality, not to register animal interests.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, fur_and_leather_producers, beneficiary,
    organized, biographical, arbitrage, global).

% Cannot appear as a party to any proceeding concerning their own treatment; anti-cruelty statutes are enforced (when enforced at all) as protections of the owner's property value and public morals, not as vindication of any interest the animal itself holds. No exit exists from confinement, breeding schedules, or slaughter timing; the animal has no legal voice and the property reading structurally forecloses one.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, confined_farm_animals, excluded,
    powerless, immediate, trapped, local).

% Subject to procedures approved through institutional cost-benefit review in which the animal's own experience carries no independent weight — only compliance with procedural minimums that avoid 'unnecessary' pain, defined by researcher and institutional convenience, not by the animal's interests.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, laboratory_test_animals, excluded,
    powerless, immediate, trapped, local).

% Bred and killed as a terminal step in a commodity chain; the property reading means their death requires no justification beyond market demand for the resulting product.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, fur_bearing_animals, excluded,
    powerless, immediate, trapped, local).

% Depend entirely on an individual owner's discretion; because animals cannot hold rights against their owner under this reading, intervention is possible only when neglect crosses a threshold that damages the owner's own property interest or offends third-party sensibilities, not when it harms the animal per se.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, working_and_companion_animals_under_neglectful_owners, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, working_and_companion_animals_under_neglectful_owners, excluded).

% Petition legislatures and courts to recognize animal interests independent of ownership, but lack standing to sue on an animal's behalf in most jurisdictions under the property reading; their arguments are heard as policy preferences of humans, never as representation of a party with a cognizable claim.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_welfare_advocates, excluded,
    moderate, generational, constrained, national).

% Purchase the economic value the property reading makes cheaply available — meat, dairy, leather, tested pharmaceuticals — without the price incorporating any cost attributable to animal interests, since none are legally recognized. Free to substitute products but rarely bear any cost internalization pressure from this constraint.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, consumers_of_animal_products, beneficiary,
    moderate, immediate, mobile, national).

% Adjudicate disputes about animal treatment strictly within property law categories (conversion, damages to chattel, bailment) and have historically declined to extend standing or interest-based review to animals themselves; some jurists now openly question whether the property category itself is doing the analytical work or merely foreclosing the question.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_scholars_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns a single, administrable legal category to a heterogeneous set of human-animal relationships (livestock, research subjects, companions, wildlife-derived products) so that ownership, transfer, damages, and use can be resolved through the existing, well-developed apparatus of property law without requiring courts or legislatures to construct a new category of legal personhood or interest-holder.
% TRANSFER_FUNCTION: Moves the entire economic value generated by animal bodies, labor, and reproduction to the humans who hold title, while moving all costs of confinement, procedure, and slaughter onto the animals themselves, who have no mechanism to be compensated, to refuse, or to have their interests weighed in the transaction.
% ABSENT_VOICES: The animals themselves cannot appear as parties under any circumstance — this is not contingent exclusion but the constraint's defining structural feature. Animal welfare advocates attempt proxy representation but are structurally denied standing to assert an animal's own interest rather than a human policy preference. Would-be reformist legislators face concentrated, organized opposition from every beneficiary category listed above.
% DISAPPEARANCE_RATIONALE: If animals ceased to be a property category overnight (shifting toward even a welfare-interest or personhood category), industries built on confinement-intensity economics (factory farming, fur production, unregulated research use) would face immediate cost restructuring or closure; entire supply chains, insurance regimes, and agricultural law would require reconstruction around a new class of interest-holder. This is not a background fact of nature — its removal visibly reorganizes markets, which is itself evidence against the naturalness the reading implicitly claims.
% FOUNDING_PROBLEM: Early common law needed a stable, transferable category to resolve disputes over animals as productive assets — theft, trespass, damages, inheritance — in an agrarian economy where livestock was a primary form of wealth and no competing framework for animal interests existed or was contemplated.
% FOUNDING_PROBLEM_CORROBORATION: Agricultural economists and industry trade groups (the constraint's own beneficiaries) attest the property framework remains necessary for functioning livestock and research markets. Independent legal scholars outside those industries (e.g., animal law scholars publishing in mainstream law reviews, some sitting judges in obiter dicta) attest that the founding problem — resolving disputes over productive assets in an economy without any alternative interest-recognition framework — no longer describes contemporary conditions, since welfare-interest and limited-personhood frameworks now exist and are administrable elsewhere (e.g., some EU sentience statutes), meaning the property reading persists today primarily because it is profitable to those who hold title, not because no alternative framework is conceivable.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.91, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.91) is authored high because the animal, as a matter of the reading's own internal logic, cannot be a victim in the morally relevant sense — it is an asset, and assets do not suffer legally cognizable harm, only depreciation. Suppression (0.72) reflects the active legal and institutional apparatus (standing doctrine, evidentiary rules, IACUC review, agricultural exemptions to general animal cruelty statutes — 'ag-gag' laws) required to keep the property boundary from being contested; this is not passive default but actively maintained. Accessibility collapse (0.62) is moderate rather than maximal because alternative legal categories (limited personhood, sentience-based statutes) exist and are visibly gaining ground in some jurisdictions, meaning the property reading has not achieved mountain-grade closure — it is a defended position, not settled bedrock. Resistance (0.55) is substantial and organized (animal law scholarship, welfare advocacy, some judicial skepticism) though it has not yet dislodged the core doctrine. Theater ratio (0.28, rising) captures the growing gap between the proliferation of welfare-labeling and compliance certification programs and their near-total non-effect on the underlying property structure — these programs increasingly function as reputational cover rather than altering the extraction the reading permits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (livestock operators, research institutions, fur/leather producers, pet breeders, consumers) sit near the full-beneficiary end of directionality: the constraint subsidizes their economic activity by removing any legal requirement to weigh a countervailing interest. Victims (confined farm animals, lab animals, fur-bearing animals, animals of neglectful owners) cannot be assigned directionality in the normal sense at all under this reading — they are structurally barred from being parties, which is precisely the point the abolitionist sibling reading contests. They are declared as victims in base_properties because the extraction is real and measurable in outcome even though the reading's own logic denies them status as claimants; the engine's directionality derivation should treat their `trapped` exit and `powerless` power as pushing d to the target extreme, which correctly registers the asymmetry even though the reading itself would deny the legitimacy of that registration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a stable transferable category for disputes over productive livestock assets in an agrarian economy with no alternative framework) is largely dead as a *necessity* claim — alternative interest-recognition frameworks now exist and operate elsewhere — but the reading persists with full institutional force because it remains highly profitable to concentrated, organized beneficiaries, which is the mismatch pattern (founding_problem_status=contested trending toward dead, disappearance_verdict=world_rearranges) that flags capture rather than genuine ongoing coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_property_vs_welfare_boundary,
    'Is the property reading a distinct, coherent legal-moral framework, or is it better understood as the welfare reading''s degenerate case (welfare obligations set at zero)?',
    'Compare jurisdictions that formally hold the property reading (no welfare statute at all) against jurisdictions with welfare statutes that are unenforced in practice — if outcomes converge, the two readings are less structurally distinct than their doctrinal labels suggest, and the operative variable is enforcement intensity rather than which reading is nominally adopted.',
    'If the readings converge empirically, the property reading''s high extractiveness score is less a function of a unique moral framework and more a function of enforcement failure common to both readings, which would argue for merging analytical attention on enforcement mechanisms rather than doctrinal category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_property_vs_welfare_boundary, conceptual, 'Whether property and welfare readings are truly distinct or converge under weak enforcement.').

omega_variable(
    victim_status_under_denying_reading,
    'Can an entity be authored as a structural victim (base_properties.victims) when the reading being modeled explicitly denies that entity''s capacity to hold a claim at all?',
    'This is a framework-design question rather than an empirical one: it turns on whether the engine''s victim/extraction machinery is meant to register the reading''s internal logic (in which case animals should not appear as victims under property_reading) or to register real-world outcomes regardless of the reading''s self-justification (in which case they should).',
    'If the engine is meant to track only internal logic, this story''s victim declarations should be removed and the constraint would likely compute closer to mountain or rope from the property reading''s own internal frame (no cognizable harm is possible by definition) — which would itself be a demonstration of exactly the false-summit dynamic the framework exists to detect: a reading that declares its own extraction unmeasurable does not thereby make it zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_status_under_denying_reading, conceptual, 'Whether to author victims when the reading itself denies victim status is coherent.').

omega_variable(
    anti_cruelty_statute_beneficiary_ambiguity,
    'Do anti-cruelty statutes under the property reading protect animal interests at all, or exclusively owner property value and third-party sensibility?',
    'Examine enforcement patterns: cases are almost never brought against owners for conditions that reduce animal welfare but do not reduce commercial value (e.g., psychological distress in confinement that does not affect meat/milk yield) — if enforcement tracks value-preservation rather than suffering-reduction, the statutes function as owner-value protection, not animal protection.',
    'If confirmed, this further raises the effective extractiveness of the property reading, since the appearance of a welfare check is itself theater masking a value-protection mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_cruelty_statute_beneficiary_ambiguity, empirical, 'Whether anti-cruelty enforcement tracks animal suffering or owner asset value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__property_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__property_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__property_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__property_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__property_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__property_reading, base_extractiveness, 16, 0.86).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__property_reading, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__property_reading, base_extractiveness, 32, 0.9).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__property_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__property_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__property_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__property_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.1).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Sibling of animal_status_kernel__welfare_reading and animal_status_kernel__abolitionist_reading. All three instantiate the same contested kernel (the legal-moral status of animals) with structurally different beneficiary/victim sets and different ε: property_reading excludes animals from victim-standing entirely and authors the highest extractiveness (0.91) because no countervailing interest-weighing mechanism exists internally; welfare_reading includes animals as interest-holders whose suffering is weighed against use, producing a lower, tangled-rope-shaped extraction profile; abolitionist_reading treats the property category itself as the harm, producing a fundamentally different transfer_function (elimination of use rather than regulation of use). Do not average across these files — each is a separate, ε-invariant constraint linked here for contamination and network analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
