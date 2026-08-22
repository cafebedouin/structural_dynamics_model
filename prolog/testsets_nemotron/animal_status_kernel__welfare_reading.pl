% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Welfare-Regulated Animal Use (Animal Status Kernel — Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the welfare_reading of the
 *   animal_status_kernel — the position that animals are sentient beings
 *   whose suffering is morally relevant, making their use acceptable only
 *   when regulated to minimize pain, while retaining their property status
 *   subject to welfare obligations. It is one of three contested readings of
 *   the same kernel (animal_status_kernel), alongside the property_reading
 *   (animals as mere property, moral considerability derives from ownership)
 *   and the abolitionist_reading (animals as moral persons with the right not
 *   to be property). This reading emerged historically from the 1822 Martin's
 *   Act (first animal welfare law) through the 1966 US Animal Welfare Act and
 *   EU welfare directives, progressively expanding the victim set from
 *   working animals to farmed and laboratory animals while maintaining the
 *   property framework. The constraint functions as a tangled_rope: it
 *   coordinates a genuine social problem (public demand for animal
 *   protection, industry need for stable regulation, scientific need for
 *   standardized animal care) while extracting from animals through permitted
 *   use that welfare standards mitigate but do not eliminate. The 'new
 *   welfarism' critique from abolitionists argues welfare reforms make the
 *   public comfortable with continued use ('happy meat'), structurally
 *   legitimizing the property regime.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: Primary beneficiary (institutional/arbitrage) — gains stable regulatory framework, social license, and continued use rights
 *   - biomedical_research_institutions: Secondary beneficiary (institutional/arbitrage) — gains standardized animal care protocols and legal certainty
 *   - welfare_regulation_agencies: Agenda-setter/beneficiary (institutional/analytical) — administers enforcement, derives institutional purpose from the regime
 *   - farmed_animals: Primary victim (powerless/trapped) — bears confined lives and slaughter; welfare standards mitigate but do not eliminate suffering
 *   - laboratory_animals: Primary victim (powerless/trapped) — bears experimental procedures; welfare standards regulate but require use
 *   - commercial_breeding_animals: Secondary victim (powerless/trapped) — bears reproductive exploitation and hereditary defects
 *   - animal_advocacy_organizations: Contested beneficiary/payer (organized/constrained) — some gain regulatory footholds; abolitionist wing excluded
 *   - philosophical_abolitionists: Excluded observer (analytical/analytical) — argue the constraint legitimizes the injustice it purports to mitigate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.42).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.38).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Welfare-Regulated Animal Use (Animal Status Kernel — Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '62f54d71-1c59-4381-bc1e-0b52ea57eba8').
narrative_ontology:cs_kernel_codification('62f54d71-1c59-4381-bc1e-0b52ea57eba8', formalized).
narrative_ontology:cs_authority_grounding('62f54d71-1c59-4381-bc1e-0b52ea57eba8', lineage).
narrative_ontology:cs_interpretation_layer_present('62f54d71-1c59-4381-bc1e-0b52ea57eba8').
narrative_ontology:cs_reading_relation('62f54d71-1c59-4381-bc1e-0b52ea57eba8', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('62f54d71-1c59-4381-bc1e-0b52ea57eba8', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('62f54d71-1c59-4381-bc1e-0b52ea57eba8', foundational, sentience_grounds_moral_considerability).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_considerability, holdable).
narrative_ontology:cs_axiom_grounding('62f54d71-1c59-4381-bc1e-0b52ea57eba8', sentience_grounds_moral_considerability, deontological).
narrative_ontology:cs_axiom('62f54d71-1c59-4381-bc1e-0b52ea57eba8', foundational, property_status_compatible_with_welfare_obligations).
narrative_ontology:cs_axiom_status(property_status_compatible_with_welfare_obligations, holdable).
narrative_ontology:cs_axiom_grounding('62f54d71-1c59-4381-bc1e-0b52ea57eba8', property_status_compatible_with_welfare_obligations, conventional).
narrative_ontology:cs_reference_frame('62f54d71-1c59-4381-bc1e-0b52ea57eba8', anti_cruelty_property_law).
narrative_ontology:cs_drift_state('62f54d71-1c59-4381-bc1e-0b52ea57eba8', industrial_animal_use_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62f54d71-1c59-4381-bc1e-0b52ea57eba8', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, companion_animal_breeders).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_regulation_agencies).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, commercial_breeding_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_advocacy_organizations).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_advocacy_organizations).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_grounds_moral_considerability).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, suffering_minimization_as_regulatory_standard).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, property_status_compatible_with_welfare_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains stable regulatory framework, social license, and continued property rights over billions of animals annually. Welfare compliance costs are predictable and passed through to consumers; the regime prevents more disruptive abolitionist outcomes. Exit options include geographic arbitrage (moving production to lower-standard jurisdictions) and vertical integration to control welfare branding.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Gains standardized animal care protocols (Guide for the Care and Use of Laboratory Animals, EU Directive 2010/63) that provide legal certainty, scientific reproducibility, and public legitimacy. Welfare compliance is a condition of funding and publication. Exit options include outsourcing to lower-standard jurisdictions and developing non-animal methods (which remain marginal).
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Administers and enforces welfare standards (USDA APHIS, EU competent authorities, etc.). Derives institutional purpose, budget, and authority from the regulatory regime. Sets the agenda for standard-setting, inspection regimes, and enforcement priorities. Captured to some degree by regulated industries but also pressured by advocacy.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_regulation_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, welfare_regulation_agencies, beneficiary).

% Bears the core extraction: confined lives in industrial systems (battery cages, gestation crates, feedlots), routine mutilations without analgesia (castration, tail docking, debeaking), transport stress, and slaughter. Welfare standards mitigate some extremes (space allowances, stunning requirements) but do not challenge the property regime that makes them commodities. No exit — they are the resource.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bears experimental procedures (toxicity testing, disease modeling, basic research) that cause pain, distress, and death. Welfare standards require analgesia, humane endpoints, and refinement — but the 3Rs framework (Replace, Reduce, Refine) treats replacement as aspirational, not mandatory. No exit — they are the experimental apparatus.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Bears reproductive exploitation (forced breeding, separation from offspring) and hereditary defects from selection for production traits (broiler chickens, fast-growing pigs, high-yield dairy cows). Welfare standards address some husbandry issues but not the genetic burden. No exit — their bodies are the product.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, commercial_breeding_animals, payer,
    powerless, immediate, trapped, global).

% Welfare-oriented groups (Humane Society, RSPCA, Compassion in World Farming) gain regulatory footholds, inspection access, and legislative victories — they are beneficiaries of the regime's openness to incremental reform. Abolitionist wings and organizations (Animal Equality, Direct Action Everywhere) are structurally excluded from the regulatory conversation — they pay opportunity costs (resources spent on welfare campaigns that legitimize use) without representation. Exit from the welfare framework means losing institutional access.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_advocacy_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_advocacy_organizations, payer).

% Argue that property status itself is the injustice; welfare reforms legitimize 'happy meat' and stabilize the system. They would object to the victim set's limitation (only suffering-capacity, not rights), the coordination function's legitimacy (coordinating exploitation), and the authority grounding (property law). They are not in the regulatory room — their exit is intellectual, not institutional.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, philosophical_abolitionists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a multi-stakeholder coordination problem: public demands protection from cruelty; industry needs stable, predictable regulation for trade and investment; science needs standardized animal care for reproducibility; regulators need enforceable standards. The welfare framework provides a single, legally codified system that all parties can reference — replacing ad hoc cruelty prohibitions with positive standards of care.
% TRANSFER_FUNCTION: Moves compliance costs (housing upgrades, enrichment, veterinary care, inspection fees, record-keeping) from animals to industry; moves social license and continued use rights from public/regulators to industry; moves institutional authority and budget from legislature to welfare agencies. The net transfer is animals bearing confinement/slaughter/experimentation; industry bearing regulated costs; public gaining assurance; regulators gaining mandate.
% ABSENT_VOICES: The animals themselves — they cannot speak, vote, litigate, or organize. Abolitionist advocates and organizations who reject the property framework — they are excluded from regulatory advisory bodies and standard-setting processes. Future generations of animals who will be born into the system — they have no representation. Wild animals affected by agricultural expansion — outside the welfare regime's scope.
% DISAPPEARANCE_RATIONALE: If the welfare-regulated property regime vanished overnight, the animal agriculture and biomedical research industries would face immediate legal chaos — no standards for housing, transport, slaughter, or experimentation. Public outrage would force emergency regulation. The property regime would likely persist (animals remain property) but without welfare constraints, reverting to a property_reading-like state with higher extraction. Alternatively, abolitionist pressure could force a transition to non-use systems. Either way, the world rearranges: billions of animal lives, trillion-dollar industries, and global trade patterns would shift.
% FOUNDING_PROBLEM: Preventing cruelty to working animals (horses, cattle) in early 19th century urban industrial societies — the visible suffering of beasts of burden in streets and slaughterhouses that offended public morality and disrupted social order.
% FOUNDING_PROBLEM_CORROBORATION: Historical record confirms the 1822 Martin's Act targeted 'cruel and improper treatment of cattle' — corroborated by parliamentary debates and contemporary press. Industry and welfare agencies attest the problem is live (ongoing cruelty cases, new production systems create new welfare challenges). Abolitionists and independent ethicists attest the founding problem is dead (cruelty is now regulated) but the arrangement persists as a legitimized property regime — the 'new welfarism' critique from Francione, Regan, and contemporary abolitionist organizations corroborates the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).
:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: welfare regulations impose compliance costs on industry (housing, enrichment, veterinary care, slaughter methods) and constrain some extreme practices, but the property regime permits continued mass use and killing. Suppression (0.38) is moderate: animals are physically confined with no exit; human advocates face legal and economic barriers to abolition; but welfare regulations themselves create some protective floor. Theater ratio (0.28) is low-moderate: welfare enforcement has genuine function (inspections, standards, penalties) but a growing share of activity performs 'humane' branding while core use continues. Accessibility collapse (0.45) is moderate: alternatives exist (plant-based diets, non-animal methods, sanctuary models) but are structurally marginalized by the property regime. Resistance (0.52) is moderate: industry resists stricter standards; abolitionists resist the property framework; welfare advocates push for incremental expansion. The claimed type is tangled_rope because the constraint both coordinates (genuine multi-stakeholder problem: public morality, industry stability, scientific standardization) and extracts (animals bear net costs of use that welfare does not eliminate, industry captures net benefit of continued property rights).
 *
 * PERSPECTIVAL GAP:
 *   From the industry seat, the constraint is a rope: it solves coordination (public trust, trade standards, scientific reproducibility) with manageable costs. From the farmed animal seat, it is a snare: the property regime extracts their lives and welfare standards only modulate the extraction. From the abolitionist seat, it is a scaffold that failed its sunset: the founding problem (cruelty) persists and the property framework is the obstacle, not the solution. From the regulator seat, it is a tangled_rope: genuine coordination function with asymmetric extraction built in. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: animal_agriculture_industry, biomedical_research_institutions, companion_animal_breeders, welfare_regulation_agencies — all gain from the regulated property regime (stable use rights, social license, institutional mandate). Victims: farmed_animals, laboratory_animals, commercial_breeding_animals — sentient beings whose suffering is morally recognized but whose use continues; they bear the net costs (confinement, slaughter, experimentation) that welfare mitigates but does not eliminate. Animal_advocacy_organizations are dual-positioned: welfare-oriented groups gain regulatory access (beneficiary) but abolitionist wings are structurally excluded (payer/excluded). Philosophical_abolitionists are excluded observers — they would object to the property framework itself but are not in the regulatory conversation. Directionality derives from this structure: victims are powerless/trapped (d near 1.0), beneficiaries are institutional/arbitrage (d near 0.0), dual-positioned advocates are organized/constrained (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing cruelty to working animals) was live in 1822. By 2024, the problem has transformed: cruelty is regulated but mass use has expanded enormously. The welfare framework that solved the 19th-century problem now stabilizes a 21st-century system of industrial use. This is not pure mandatrophy (the coordination function remains live for public morality and trade) but a case where the constraint's extraction component has grown alongside its coordination function — a tangled_rope where the rope has lengthened but the snare has tightened. The 'new welfarism' critique identifies this: welfare reforms legitimize the property regime, making the constraint a net extraction mechanism from the abolitionist seat. The mandatrophy is partial and contested — hence founding_problem_status = contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_extraction_lock_in,
    'Do welfare reforms structurally reduce extraction from animals or legitimize and stabilize the underlying property regime, increasing net extraction over time?',
    'Longitudinal analysis of welfare regulation stringency vs. animal use volume and industry profitability; counterfactual assessment of abolitionist trajectory without welfare reforms.',
    'If reforms legitimize the property regime, the constraint is a scaffold for extraction rather than a genuine reduction; classification shifts toward snare from abolitionist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_extraction_lock_in, empirical, 'Whether welfare regulation functions as extraction-reduction or extraction-legitimation').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the welfare, property, and abolitionist readings disagree structurally — on the victim set, the extraction boundary, the coordination function, or the kernel''s authority grounding?',
    'Structural mapping of each reading''s beneficiary/victim declarations, claimed coordination function, and cs_structure authority_grounding; identify the minimal structural element whose change converts one reading to another.',
    'Locates the irreducible disagreement for cross-reading contamination analysis; determines whether readings are genuinely distinct constraints or framing variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement among kernel readings').

omega_variable(
    sentience_threshold_victim_inclusion,
    'Which animals'' suffering-capacity crosses the threshold for victim-set inclusion under this reading, and how is the threshold operationalized in regulation?',
    'Survey of welfare legislation scope (vertebrates, cephalopods, decapod crustaceans); phylogenetic and cognitive criteria used by regulatory bodies; industry compliance patterns at taxonomic boundaries.',
    'Determines the victim set''s extension and the constraint''s effective scope; a narrow threshold makes the constraint a partial snare for excluded taxa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_victim_inclusion, empirical, 'Taxonomic and operational boundary of victim-set inclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 1822, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_reading_tr_t1822, animal_status_kernel__welfare_reading, theater_ratio, 1822, 0.15).
narrative_ontology:measurement(animal_welfare_reading_tr_t1900, animal_status_kernel__welfare_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(animal_welfare_reading_tr_t1966, animal_status_kernel__welfare_reading, theater_ratio, 1966, 0.22).
narrative_ontology:measurement(animal_welfare_reading_tr_t1990, animal_status_kernel__welfare_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(animal_welfare_reading_tr_t2020, animal_status_kernel__welfare_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(animal_welfare_reading_tr_t2024, animal_status_kernel__welfare_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(animal_welfare_reading_be_t1822, animal_status_kernel__welfare_reading, base_extractiveness, 1822, 0.65).
narrative_ontology:measurement(animal_welfare_reading_be_t1900, animal_status_kernel__welfare_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(animal_welfare_reading_be_t1966, animal_status_kernel__welfare_reading, base_extractiveness, 1966, 0.48).
narrative_ontology:measurement(animal_welfare_reading_be_t1990, animal_status_kernel__welfare_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(animal_welfare_reading_be_t2020, animal_status_kernel__welfare_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(animal_welfare_reading_be_t2024, animal_status_kernel__welfare_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_reading_su_t1822, animal_status_kernel__welfare_reading, suppression_requirement, 1822, 0.25).
narrative_ontology:measurement(animal_welfare_reading_su_t1900, animal_status_kernel__welfare_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(animal_welfare_reading_su_t1966, animal_status_kernel__welfare_reading, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(animal_welfare_reading_su_t1990, animal_status_kernel__welfare_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(animal_welfare_reading_su_t2020, animal_status_kernel__welfare_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(animal_welfare_reading_su_t2024, animal_status_kernel__welfare_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.15).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_welfare_enforcement_regime).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_agriculture_subsidies).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, biomedical_research_funding).

% DUAL FORMULATION NOTE:
% This constraint is one member of the animal_status_kernel constraint family (3 readings). The property_reading declares animals as property with no intrinsic moral status (extraction-high, coordination-low). The abolitionist_reading declares animals as moral persons with right not to be property (extraction-zero for animals, coordination-high for abolitionist project). This welfare_reading sits between: animals as sentient property with welfare claims (moderate extraction, genuine coordination). The three readings share a kernel but have divergent ε values and structural relationships — they are distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, institutional, 0.15).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, organized, 0.45).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
