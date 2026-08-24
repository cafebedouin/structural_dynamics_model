% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Animal Welfare Regulatory Framework (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   The welfare reading of animal status holds that animals are sentient
 *   beings whose suffering matters morally, but that human use of animals
 *   remains permissible if regulated to minimize pain. Property status is
 *   retained but constrained by welfare obligations. This reading
 *   instantiates the animal_status_kernel by partially including animals in
 *   the victim-set via their capacity for suffering, while preserving the
 *   property framework that enables continued use. The constraint is the
 *   welfare regulatory regime itself — the body of laws, standards,
 *   inspections, and certifications that operationalize 'humane' use.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Regulatory Framework (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '20c8ef75-54fa-4b93-b7f4-3ff98eb07887').
narrative_ontology:cs_kernel_codification('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', distributed).
narrative_ontology:cs_authority_grounding('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', practice).
narrative_ontology:cs_interpretation_layer_present('20c8ef75-54fa-4b93-b7f4-3ff98eb07887').
narrative_ontology:cs_reading_relation('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', foundational, sentience_grounds_welfare_obligations).
narrative_ontology:cs_axiom_status(sentience_grounds_welfare_obligations, holdable).
narrative_ontology:cs_axiom_grounding('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', sentience_grounds_welfare_obligations, empirically_contingent).
narrative_ontology:cs_axiom('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', foundational, property_status_compatible_with_welfare).
narrative_ontology:cs_axiom_status(property_status_compatible_with_welfare, holdable).
narrative_ontology:cs_axiom_grounding('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', property_status_compatible_with_welfare, conventional).
narrative_ontology:cs_axiom('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', secondary, incremental_reform_preferable_to_abolition).
narrative_ontology:cs_axiom_status(incremental_reform_preferable_to_abolition, holdable).
narrative_ontology:cs_axiom_grounding('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', incremental_reform_preferable_to_abolition, instrumental).
narrative_ontology:cs_reference_frame('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', welfare_regulation_framework).
narrative_ontology:cs_drift_state('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', contemporary_animal_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20c8ef75-54fa-4b93-b7f4-3ff98eb07887', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_seeking_humane_products).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, research_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, wild_animals_under_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_industry).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, consumers_seeking_humane_products).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_based_moral_considerability).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulated_use_permissible).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, property_status_compatible_with_welfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create and enforce animal welfare standards through legislation, agency rulemaking, and inspection regimes. They balance industry viability against public pressure for humane treatment. Their authority derives from legislative mandate and administrative law.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, government_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Operate animal agriculture, research, and entertainment facilities under welfare regulations. They gain social license and market access from compliance, but bear costs of housing upgrades, veterinary care, inspection fees, and production inefficiencies. They lobby to shape standards at viable levels.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_industry, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_industry, payer).

% Advocate for stronger welfare standards, monitor compliance, and certify 'humane' products. They gain legitimacy, funding, and policy influence from incremental wins. Their strategy depends on the welfare framework's existence — abolition would eliminate their current operational model.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_welfare_organizations, agenda_setter).

% Purchase animal products with welfare certifications (cage-free, humane-certified, etc.). They gain moral comfort and perceived quality from welfare labels, but pay price premiums. Their choices are constrained by what the regulatory-market system makes available.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_seeking_humane_products, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, consumers_seeking_humane_products, payer).

% Argue that property status itself is the injustice and that welfare reforms entrench animal use by making it socially acceptable. They are structurally excluded from regulatory rulemaking, which presupposes regulated use as the legitimate frame. Their exit is constrained by the dominance of the welfare paradigm in law and public discourse.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Evaluates the welfare framework's structural operation across seats: industry capture of regulation, consumer moral licensing, abolitionist marginalization, and the actual trajectory of animal suffering under 'humane' standards.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expectations among producers, regulators, and consumers about minimum acceptable treatment of animals, creating a shared baseline that enables market exchange of animal products with reduced moral friction.
% TRANSFER_FUNCTION: Moves compliance costs (housing, veterinary care, monitoring, certification) from animals to industry, and moves price premiums from consumers to industry; moves moral legitimacy from abolitionist critique to regulated use, and moves regulatory authority from common-law property rules to administrative welfare standards.
% ABSENT_VOICES: The animals themselves cannot participate in the regulatory process; their interests are represented only indirectly through welfare advocates. Abolitionist advocates are excluded from the rulemaking frame, which treats 'how to use humanely' as the only legitimate question, not 'whether to use at all.' Future generations who might inherit a different moral framework are also absent.
% DISAPPEARANCE_RATIONALE: If welfare regulations vanished overnight, industry would revert to lowest-cost production (intensive confinement, minimal veterinary care), consumers would lose trusted humane labels, welfare organizations would lose their operational framework, and the legal baseline would revert to bare anti-cruelty statutes — the entire market-moral ecosystem around 'humane' animal products would collapse.
% FOUNDING_PROBLEM: Industrial animal use expanded in the 19th-20th centuries with minimal constraints, generating public outrage over visible cruelty. The welfare framework was built to legitimize continued use by imposing minimum standards that reduced the most egregious suffering while preserving the property/economic system.
% FOUNDING_PROBLEM_CORROBORATION: Industry and welfare organizations attest the problem is live (ongoing cruelty risks require vigilant regulation). Abolitionists and animal cognition researchers attest the problem is dead or transformed: the founding cruelty has been displaced by systemic harms (confinement, breeding, killing) that welfare standards do not address, and the framework now functions to legitimize rather than constrain use. Legislative histories and veterinary ethics literature from outside the benefiting parties document this shift.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because welfare regulations impose real compliance costs on industry (housing, veterinary care, monitoring) while permitting continued use and killing — the extraction is the surplus value industry retains by not adopting the abolitionist alternative. Suppression (0.55) reflects the structural marginalization of abolitionist alternatives: the regulatory frame defines 'humane' as the ceiling, not the floor, and legal standing for animals remains derivative. Theater ratio (0.42) captures the growing gap between welfare certification marketing and actual animal experience — 'humane' labels often certify practices that still involve significant confinement and early slaughter. Accessibility collapse (0.52) and resistance (0.58) reflect that vegan/abolition alternatives exist and are growing, but remain socially and legally non-default.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/welfare-org seat, the framework is genuine coordination solving the problem of uncontrolled cruelty. From the abolitionist seat, the same structure is extraction legitimized — the coordination story is cover for continued use. From the industry seat, it's a managed cost of doing business that also provides marketing value. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the author's assessment that both coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators sit near the beneficiary end (d ~ 0.2) — they gain authority and legitimacy from administering the framework. Animal industry is near symmetric (d ~ 0.5) — they pay compliance costs but capture the value of continued use and social license. Welfare organizations are beneficiaries (d ~ 0.3) — they gain resources and influence but depend on the framework's persistence. Consumers are near symmetric (d ~ 0.5) — they gain moral comfort but pay premiums. Abolitionist advocates are targets (d ~ 0.8) — their preferred outcome is structurally excluded by the framework's premises. Animals themselves are the ultimate payers but lack agency in this model.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (egregious industrial cruelty) has been substantially addressed in its original form, but the arrangement persists and has expanded to cover new species and practices. The welfare framework now functions partly to legitimize the system it was built to constrain — a classic mandatrophy pattern. However, unlike a pure piton, it still performs real coordination (reducing suffering relative to the unregulated counterfactual) and still faces active resistance from both industry (against stricter standards) and abolitionists (against the framework itself). The mandate has not fully atrophied; it has mutated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this constraint''s classification change when evaluated as one reading of a contested kernel versus a standalone constraint?',
    'Compare the engine''s per-seat classification of this reading against the classifications of the sibling readings (property_reading, abolitionist_reading) when each is authored as a separate constraint story. The kernel''s structural dynamics emerge from the pattern of divergences.',
    'If the welfare reading computes as tangled_rope while the property reading computes as snare and the abolitionist reading computes as mountain (or rope), the kernel itself exhibits a structural gradient that no single reading captures. This would validate the kernel decomposition approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee-frame structural delta: this reading is one instantiation of a contested kernel; its ε and seat divergences are reading-indexed, not kernel-indexed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of abolitionist alternatives structural (legal standing barriers, regulatory capture, industry lobbying) or internalized (public moral licensing via ''humane'' labels, cognitive dissonance reduction), or both?',
    'Post-reform suppression trajectory: if abolitionist mobilization increases after welfare victories (suggesting internalized suppression weakens), versus if it stalls (suggesting structural suppression dominates). Survey experiments on moral licensing effects of welfare labels.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than structural measures suggest — the public carries the suppression with them via moral licensing. This would increase effective extraction for the consumer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the welfare framework''s marginalization of abolition.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (reducing suffering) end and the extraction function (legitimizing continued use) begin? Are they structurally separable?',
    'Natural experiments from jurisdictions that have banned specific practices (e.g., battery cages, gestation crates): if suffering decreases without reducing overall animal use, coordination is separable; if industry exits or consolidates, the functions are coupled.',
    'If separable, the welfare framework could be strengthened toward a rope (pure coordination) without eliminating use. If coupled, any strengthening that threatens industry viability triggers enforcement capture — the constraint is inherently tangled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the welfare framework''s coordination and extraction components are structurally separable or necessarily coupled.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Do welfare obligations reflect a natural moral law (sentience generates duties) or a constructed political compromise (industry tolerance for regulation in exchange for social license)?',
    'Cross-cultural and historical comparison: if welfare standards converge on similar protections independent of industry power, natural-law interpretation gains support; if standards track industry capacity and political economy, constructed interpretation gains support.',
    'If natural law, the constraint trends toward mountain (low extraction, high accessibility_collapse). If constructed, it remains tangled_rope or trends toward snare as industry capture deepens. This is the false-summit question for any mountain claim about welfare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the welfare framework''s moral foundations are discovered or negotiated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_tr_t1960, animal_status_kernel__welfare_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(animal_welfare_tr_t1976, animal_status_kernel__welfare_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(animal_welfare_tr_t1992, animal_status_kernel__welfare_reading, theater_ratio, 1992, 0.31).
narrative_ontology:measurement(animal_welfare_tr_t2008, animal_status_kernel__welfare_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(animal_welfare_tr_t2016, animal_status_kernel__welfare_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(animal_welfare_tr_t2024, animal_status_kernel__welfare_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(animal_welfare_be_t1960, animal_status_kernel__welfare_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(animal_welfare_be_t1976, animal_status_kernel__welfare_reading, base_extractiveness, 1976, 0.32).
narrative_ontology:measurement(animal_welfare_be_t1992, animal_status_kernel__welfare_reading, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(animal_welfare_be_t2008, animal_status_kernel__welfare_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(animal_welfare_be_t2016, animal_status_kernel__welfare_reading, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement(animal_welfare_be_t2024, animal_status_kernel__welfare_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_su_t1960, animal_status_kernel__welfare_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(animal_welfare_su_t1976, animal_status_kernel__welfare_reading, suppression_requirement, 1976, 0.45).
narrative_ontology:measurement(animal_welfare_su_t1992, animal_status_kernel__welfare_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(animal_welfare_su_t2008, animal_status_kernel__welfare_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(animal_welfare_su_t2016, animal_status_kernel__welfare_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(animal_welfare_su_t2024, animal_status_kernel__welfare_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the animal_status_kernel family. The kernel decomposes into three readings with distinct ε values and victim-sets: property_reading (ε ≈ 0.15, victims = none/owners only), welfare_reading (ε ≈ 0.45, victims = animals via suffering), abolitionist_reading (ε ≈ 0.05, victims = none — but treats current use as snare). The welfare reading influences the property reading by establishing sentience as a regulatory criterion, and coexists with the abolitionist reading while creating moral-licensing pressure that stabilizes the property-welfare compromise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, institutional, 0.15).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, powerful, 0.48).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, organized, 0.35).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
