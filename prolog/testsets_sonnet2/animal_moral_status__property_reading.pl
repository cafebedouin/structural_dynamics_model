% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Animal Property Status: Interests Subordinate to Human Interests by Definition
 *   domain: applied_ethics/legal_philosophy/animal_studies
 *
 * SUMMARY:
 *   This constraint is one reading of the contested animal_moral_status
 *   kernel — the property reading, under which animals are legally and
 *   morally classified as resources or property whose interests have no
 *   independent standing; any consideration they receive is derivative of,
 *   and subordinate to, human interests (economic, scientific, recreational,
 *   nutritional). This reading imposes no constraint on use as such — an
 *   owner may use an animal for any human purpose — and only constrains
 *   outright waste or gross inefficiency (destroying a resource without
 *   benefit to anyone). The property reading is authored here as a clean,
 *   self-contained constraint: it does not describe or average over the
 *   welfare_reading (which concedes sentience and regulates cruelty within
 *   permitted use) or the abolitionist_reading (which holds that property
 *   status itself is the violation). Those are separate constraints, linked
 *   via network.affects_constraints, each with its own ε.
 *
 * KEY AGENTS:
 *   - livestock_producers: Primary beneficiary (organized/institutional, arbitrage exit) — collects economic value from treating animals as capital/inventory
 *   - animal_agriculture_industry: Primary beneficiary (institutional, arbitrage exit) — entire business model rests on the property classification
 *   - biomedical_research_industry: Beneficiary (institutional, constrained exit) — research access depends on animals lacking independent standing that would require consent-like protections
 *   - pet_and_working_animal_owners: Beneficiary (moderate, mobile exit) — ownership rights over companion and working animals depend on the same doctrine
 *   - consumers_of_animal_products: Diffuse beneficiary (organized, mobile exit) — benefits from low-cost animal products enabled by unconstrained use
 *   - animal_welfare_advocates: Excluded/resisting voice (organized, analytical exit) — contests the doctrine's naturalness from outside the beneficiary set
 *   - legal_scholars_of_animal_law: Observer (analytical, analytical exit) — analyzes the doctrine's structure without direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.15).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.3).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Property Status: Interests Subordinate to Human Interests by Definition").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy/animal_studies").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '567c356d-e234-49bc-8cdb-a9651e9aea5d').
narrative_ontology:cs_kernel_codification('567c356d-e234-49bc-8cdb-a9651e9aea5d', distributed).
narrative_ontology:cs_authority_grounding('567c356d-e234-49bc-8cdb-a9651e9aea5d', distributed).
narrative_ontology:cs_reading_relation('567c356d-e234-49bc-8cdb-a9651e9aea5d', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('567c356d-e234-49bc-8cdb-a9651e9aea5d', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('567c356d-e234-49bc-8cdb-a9651e9aea5d', foundational, animal_interests_have_no_independent_standing).
narrative_ontology:cs_axiom_status(animal_interests_have_no_independent_standing, holdable).
narrative_ontology:cs_axiom_grounding('567c356d-e234-49bc-8cdb-a9651e9aea5d', animal_interests_have_no_independent_standing, conventional).
narrative_ontology:cs_axiom('567c356d-e234-49bc-8cdb-a9651e9aea5d', foundational, use_constrained_only_by_waste_not_by_animal_interest).
narrative_ontology:cs_axiom_status(use_constrained_only_by_waste_not_by_animal_interest, holdable).
narrative_ontology:cs_axiom_grounding('567c356d-e234-49bc-8cdb-a9651e9aea5d', use_constrained_only_by_waste_not_by_animal_interest, instrumental).
narrative_ontology:cs_reference_frame('567c356d-e234-49bc-8cdb-a9651e9aea5d', classical_property_law_ownership_doctrine).
narrative_ontology:cs_drift_state('567c356d-e234-49bc-8cdb-a9651e9aea5d', contemporary_animal_cognition_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('567c356d-e234-49bc-8cdb-a9651e9aea5d', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, pet_and_working_animal_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_interest_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and manage animals as productive capital — inventory, machinery-equivalents whose value is measured in yield. The property classification means no independent animal interest can override a production decision; only waste (destroying value without economic benefit) is constrained. They lobby to maintain and extend agricultural exemptions from general animal-cruelty statutes.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, livestock_producers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__property_reading, livestock_producers, agenda_setter).

% An entire economic sector — meat, dairy, egg, and byproduct production and processing — is structured around treating animals as fungible resource units. The property doctrine is load-bearing for cost structures, financing, insurance, and trade law; its erosion would require systemic restructuring.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Uses animals as experimental subjects/tools under regulatory frameworks (e.g., IACUC-style review) that assume animals as property-like research materials subject to procedural safeguards against waste and unnecessary suffering, not to independent standing that could require consent-equivalent protections. Exit from the doctrine would require fundamentally different research paradigms.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, biomedical_research_industry, beneficiary,
    institutional, generational, constrained, national).

% Hold legal ownership over companion and working animals, entitling them to make unilateral decisions (rehoming, euthanasia, breeding, sale) that would be constrained if the animal held independent legal standing. Most experience this as an unremarkable background feature of ordinary life rather than a contested doctrine.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, pet_and_working_animal_owners, beneficiary,
    moderate, biographical, mobile, local).

% Benefit diffusely from lower prices and unconstrained supply chains that the property classification enables — no independent-standing consideration adds cost to production. Individually mobile (can choose alternative products) but collectively benefit from the doctrine's stability as a class.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, consumers_of_animal_products, beneficiary,
    organized, immediate, mobile, global).

% Contest the property reading from outside the beneficiary set, arguing that even conceding productive use, animals' capacity to suffer generates claims that a pure property/waste framework cannot register. They are excluded from the property reading's own internal accounting by definitional construction, not by procedural oversight — the reading's core move is precisely to exclude their concern as a category error.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, analytical, national).

% Study the doctrine's historical development, cross-jurisdictional variation, and internal tensions (e.g., anti-cruelty statutes carved out of a property framework) without a direct material stake in its persistence or abolition.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_scholars_of_animal_law, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, low-transaction-cost legal category (property) that lets humans allocate, trade, breed, use, and dispose of animals without needing to resolve or litigate any independent-interest claim on the animal's behalf — coordinating markets, research, agriculture, and companionship around a single settled ownership logic.
% TRANSFER_FUNCTION: Under this reading, nothing is 'transferred' from animals to humans in a morally cognizable sense, because animals are not accounted as parties with interests capable of being transferred from — the reading's own accounting registers only transfers among human parties (producer to consumer, owner to owner). This is itself the structurally significant fact the reading asserts.
% ABSENT_VOICES: Animal welfare and rights advocates are the clearest excluded voice — they would argue the property/waste framework is definitionally incapable of registering the harm the reading's own operation may produce, but this reading is authored from within a framework that treats their objection as external to the relevant legal-moral category, not as an internal correction it must accommodate.
% DISAPPEARANCE_RATIONALE: If the property classification of animals disappeared overnight (replaced by, say, a rights-bearing or trust-beneficiary status), livestock and biomedical industries would face immediate existential restructuring, ownership transactions (sale, breeding, euthanasia decisions) would require new legal processes, and entire supply chains and financing structures built on animals-as-inventory would need to be rebuilt around a different legal category. The scale of rearrangement is exactly what marks this as a constructed legal arrangement rather than a physical mountain, notwithstanding its claimed naturalness.
% FOUNDING_PROBLEM: Early legal systems needed a workable category for domesticated animals as objects of ownership, trade, inheritance, and productive use — resolving disputes over livestock, working animals, and animal products required a settled ownership framework analogous to other movable property.
% FOUNDING_PROBLEM_CORROBORATION: Property-owning industries attest the founding problem remains fully live (animals must be tradeable, financeable, insurable assets for modern agriculture and research to function). Legal historians and animal-law scholars — a seat outside the beneficiary set — attest that while the practical need for a workable ownership category was real historically, the doctrine's current scope (extending unconstrained-use logic to contexts involving sentient, cognitively sophisticated animals under industrial conditions unimaginable when the doctrine formed) substantially exceeds what the founding problem required, making the doctrine's current form at least partly a persistence-by-inertia-and-interest phenomenon rather than a pure solution to its original problem.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.15, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.15) because, from within this reading's own lights, the property classification is not experienced as extraction at all — there is no victim class within the reading's own terms (animals are not moral patients whose loss counts as a cost in this reading's accounting). Suppression is moderate (0.3): the doctrine is defended in law and social practice against welfare and abolitionist challengers, requiring some active maintenance (property statutes, agricultural exemptions from general animal cruelty law, research-animal exemptions), but it is not principally coercive against human parties — there is no human class being suppressed to sustain it, only animals excluded from the moral-patient category by definitional fiat. Accessibility collapse is high (0.7): once one accepts the property framing, alternative framings (welfare or rights) become hard to construct within the same legal-economic logic, since property law's own categories (owner/object, waste/non-waste) have no native slot for an object's own interests. Resistance is moderate (0.35): welfare and abolitionist movements actively contest the doctrine, but the doctrine's centrality to entire industries gives it substantial inertial defense.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (producers, industry, owners), this reading appears as settled background law — barely visible as a constraint at all, more like the water animals-as-property swim in. From the excluded seat (welfare/rights advocates), the same arrangement is visible as an actively defended, historically contingent legal choice with enormous stakes for billions of sentient beings. The engine computes these as different seat-level readings of the same structural data; the divergence is exactly what a kernel-reading analysis is built to surface, not something to resolve within this file.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (property owners, industry, consumers) sit near the full-beneficiary end of directionality — the constraint subsidizes their use of animals as inputs with no independent-standing constraint to internalize. There is no victim group authored in this reading's own base_properties, because within the property reading's own terms, animals are not agents whose costs register as extraction — this is the reading's defining structural move, not an oversight. (The abolitionist_reading and welfare_reading, as separate constraints, author animals or animal-interest-proxies within their own victim sets according to their own lights.)
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not directly at issue for a mountain claim, but the FSM omega documents the live question of whether this 'natural' classification has outlived any genealogical function it once had (e.g., early property law's practical need for a workable ownership category for domesticated animals) and now persists primarily because it is profitable to maintain, independent of whether it remains descriptively adequate to what is now known about animal cognition and sentience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_property_status,
    'Is the classification of animals as property a mind-independent structural fact (analogous to a natural law of ownership over non-rational beings) or a constructed legal-economic arrangement maintained because it benefits identifiable industries and consumers?',
    'Compare cross-jurisdictional and cross-historical variation in animal legal status: if property classification is a genuine mountain, it should show minimal variation across independent legal traditions with different economic interests; substantial variation correlated with the economic stake of the classifying institution would indicate a constructed arrangement wearing naturalized cover.',
    'If constructed, the FSM signature applies and the correct classification is tangled_rope or snare (property owners benefit, animals bear costs, enforcement via law is active) rather than mountain. If genuinely natural (grounded in an uncontested moral-status boundary), the mountain claim holds and beneficiary presence is incidental rather than diagnostic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_property_status, conceptual, 'Whether animal-as-property is natural-law-like or a constructed arrangement with identifiable beneficiaries (FSM trigger).').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly does the property_reading diverge from the welfare_reading and abolitionist_reading — is it a disagreement about facts (do animals have morally relevant interests?) or about the threshold at which interests generate constraints on use?',
    'Examine whether property-reading proponents deny animal sentience/interest-having as a factual matter, or concede sentience but deny that sentience alone generates standing independent of ownership. The welfare reading concedes sentience and regulates cruelty; the property reading''s distinguishing move is that even conceded suffering does not generate an independent claim — only waste/inefficiency constrains use.',
    'If the disagreement is factual (about sentience), it is resolvable by biology and should collapse toward welfare or abolitionist readings as evidence accumulates. If it is a threshold/definitional disagreement (interests exist but don''t bind), it is a stable normative fork that persists regardless of empirical findings about animal cognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locates the property reading''s core distinguishing move relative to its sibling readings within the animal_moral_status kernel.').

omega_variable(
    waste_inefficiency_constraint_scope,
    'Does the ''no constraint on use, only on waste/inefficiency'' feature of this reading function as a genuine limit, or is it vacuous in practice because ''waste'' is defined by the property owner''s own economic calculus?',
    'Examine anti-cruelty and waste statutes as actually enforced under property frameworks (e.g., livestock ''humane handling'' rules justified on carcass-quality/productivity grounds): if enforcement tracks economic loss to the owner rather than animal suffering per se, the constraint is definitionally circular and adds no real limit.',
    'If circular, the property reading''s claimed metrics (low extraction, low suppression) understate the actual leverage the reading exercises over animal treatment, since ''waste'' is set entirely by the beneficiary class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_inefficiency_constraint_scope, empirical, 'Whether the waste/inefficiency limit is a real constraint or an artifact of owner-defined economic calculus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(anim_tr_t80, animal_moral_status__property_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(anim_tr_t120, animal_moral_status__property_reading, theater_ratio, 120, 0.08).
narrative_ontology:measurement(anim_tr_t160, animal_moral_status__property_reading, theater_ratio, 160, 0.09).
narrative_ontology:measurement(anim_tr_t200, animal_moral_status__property_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(anim_be_t80, animal_moral_status__property_reading, base_extractiveness, 80, 0.13).
narrative_ontology:measurement(anim_be_t120, animal_moral_status__property_reading, base_extractiveness, 120, 0.14).
narrative_ontology:measurement(anim_be_t160, animal_moral_status__property_reading, base_extractiveness, 160, 0.15).
narrative_ontology:measurement(anim_be_t200, animal_moral_status__property_reading, base_extractiveness, 200, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_moral_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.05).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the colloquial 'animal moral status' claim per the ε-invariance principle: property_reading (this file, ε≈0.15, mountain claim, no victims), welfare_reading (ε mid-range, tangled_rope claim, victims are animals-within-permitted-use-suffering-beyond-regulated-limits), and abolitionist_reading (ε high, snare claim, victims are all animals under any use). The three are NOT the same constraint measured three ways — each reading has a structurally distinct victim set, beneficiary set, and enforcement story, per DP-001 ε-invariance. They are linked here via affects_constraints because each reading's legal and cultural dominance shapes the resource availability and legitimacy conditions available to the others (e.g., property_reading's legal entrenchment raises the cost of establishing welfare_reading's regulatory constraints, and welfare_reading's regulatory apparatus in turn provides partial cover that helps stabilize property_reading against abolitionist challenge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
