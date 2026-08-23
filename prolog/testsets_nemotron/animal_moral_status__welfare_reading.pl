% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Regulatory Framework (Humane Use Standard)
 *   domain: ethics/legal/animals
 *
 * SUMMARY:
 *   The welfare reading of animal moral status holds that animals are
 *   sentient beings whose suffering should be minimized within systems of
 *   regulated use — cruelty is wrong, but use is permissible. This constraint
 *   instantiates the regulatory architecture of 'humane' standards:
 *   anti-cruelty statutes, welfare codes for farming and research, inspection
 *   regimes, and certification systems. It claims to coordinate the genuine
 *   problem of preventing gratuitous suffering while allowing beneficial use.
 *   The metrics describe a constraint that has accumulated extraction over
 *   two centuries: what began as a coordination mechanism against overt
 *   cruelty has become a regulatory framework that legitimates
 *   industrial-scale use under 'humane' labels, with welfare organizations
 *   and regulated industries as co-beneficiaries of the legitimacy the
 *   framework provides, while animals remain in the victim set for suffering
 *   that falls within approved parameters. The constraint is claimed as
 *   tangled_rope because it possesses both genuine coordination (suffering
 *   reduction above a baseline) and asymmetric extraction (industry captures
 *   the 'humane' premium, welfare organizations capture institutional
 *   standing, animals bear the residual suffering). Active enforcement is
 *   required — the framework persists only through continuous regulatory
 *   maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.32).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.48).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Regulatory Framework (Humane Use Standard)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "ethics/legal/animals").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '27885e3a-f7bb-4d79-8c36-819c8bc6a984').
narrative_ontology:cs_kernel_codification('27885e3a-f7bb-4d79-8c36-819c8bc6a984', distributed).
narrative_ontology:cs_authority_grounding('27885e3a-f7bb-4d79-8c36-819c8bc6a984', practice).
narrative_ontology:cs_interpretation_layer_present('27885e3a-f7bb-4d79-8c36-819c8bc6a984').
narrative_ontology:cs_reading_relation('27885e3a-f7bb-4d79-8c36-819c8bc6a984', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('27885e3a-f7bb-4d79-8c36-819c8bc6a984', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('27885e3a-f7bb-4d79-8c36-819c8bc6a984', foundational, sentience_grounds_moral_considerability).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_considerability, holdable).
narrative_ontology:cs_axiom_grounding('27885e3a-f7bb-4d79-8c36-819c8bc6a984', sentience_grounds_moral_considerability, empirically_contingent).
narrative_ontology:cs_axiom('27885e3a-f7bb-4d79-8c36-819c8bc6a984', foundational, use_permissible_if_suffering_minimized).
narrative_ontology:cs_axiom_status(use_permissible_if_suffering_minimized, holdable).
narrative_ontology:cs_axiom_grounding('27885e3a-f7bb-4d79-8c36-819c8bc6a984', use_permissible_if_suffering_minimized, conventional).
narrative_ontology:cs_reference_frame('27885e3a-f7bb-4d79-8c36-819c8bc6a984', anti_cruelty_consensus_1820).
narrative_ontology:cs_drift_state('27885e3a-f7bb-4d79-8c36-819c8bc6a984', contemporary_industrial_regulatory_framework, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27885e3a-f7bb-4d79-8c36-819c8bc6a984', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, veterinary_profession).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, compliance_auditors).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, farmed_animals_under_humane_standards).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, laboratory_animals_under_regulatory_protocols).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, working_animals_under_welfare_codes).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_based_moral_considerability).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, cruelty_prohibition_as_moral_baseline).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, regulated_use_legitimacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large-scale animal agriculture, biomedical research institutions, and animal entertainment industries that operate under welfare regulations. They gain the 'humane' label that preserves social license, market access, and legal protection from more radical reform. Compliance costs are built into business models and passed to consumers. They influence standard-setting through industry representation on advisory bodies and lobbying. Exit from the welfare framework would mean losing the legitimacy premium — they are structurally locked into the system as beneficiaries.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, generational, arbitrage, global).

% NGOs and advocacy groups (e.g., RSPCA, Humane Society, Compassion in World Farming) that monitor, certify, and lobby within the welfare framework. They gain institutional standing, government funding, policy access, and donor revenue as the designated experts on 'humane' treatment. Their organizational survival depends on the framework's persistence — if use were abolished, their current role dissolves. They can exit by shifting to abolitionist advocacy, but that loses them the institutional seat at the regulatory table.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, beneficiary,
    organized, generational, mobile, global).

% Veterinarians serve as the technical enforcers and certifiers of welfare standards. The profession gains legal monopoly on animal health decisions within use systems, state-sanctioned authority, and a professional identity tied to 'animal welfare' rather than 'animal rights.' Exit would require abandoning the professional role in animal-use industries — a significant career and identity cost.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, veterinary_profession, beneficiary,
    organized, biographical, constrained, national).

% Third-party certification bodies (e.g., Global Animal Partnership, Certified Humane) that audit welfare compliance for market premium. They capture the economic value of the 'humane' label through audit fees and brand licensing. They are mobile — they could pivot to other certification markets — but the welfare certification niche is lucrative and growing.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, compliance_auditors, beneficiary,
    moderate, biographical, mobile, global).

% Billions of animals (chickens, pigs, cattle, fish) raised under welfare codes that permit confinement, mutilation without analgesia, early separation, and slaughter — all within 'humane' parameters. They cannot exit, cannot consent, and their suffering is legally sanctioned because it falls within approved standards. The constraint extracts their suffering and converts it into the 'humane' label that benefits the human institutional actors.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, farmed_animals_under_humane_standards, payer,
    powerless, immediate, trapped, global).

% Animals used in research under IACUC/ethics committee protocols that approve procedures causing pain, distress, and death when 'justified by scientific necessity' and 'minimized.' The welfare framework makes this use legitimate; the constraint extracts their suffering as the cost of scientific progress, with the 'humane' certification as the legitimating mechanism.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, laboratory_animals_under_regulatory_protocols, payer,
    powerless, immediate, trapped, global).

% Equids, camelids, elephants, and other working animals in transport, tourism, and labor under welfare codes that permit loads, hours, and conditions that cause chronic suffering but meet 'minimum standards.' Their suffering is the extracted resource that the 'humane' label legitimates.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, working_animals_under_welfare_codes, payer,
    powerless, immediate, trapped, regional).

% Advocates and scholars who argue that the welfare framework itself perpetuates victimization by making 'humane' use legitimate. They are structurally excluded from the regulatory table — their position (no use) is treated as outside the Overton window of 'reasonable' policy. They bear the cost of marginalization while their predicted victims (animals) remain in the system.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Industry groups and legal scholars who argue animals are property and welfare regulations are unjustified takings. They are excluded from the welfare framework's consensus but hold structural power through property law, trade agreements, and constitutional litigation. They would dismantle the welfare framework entirely if they could.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, property_rights_defenders, excluded,
    institutional, generational, arbitrage, national).

% The indexical classification seat: sees the full structure — the welfare reading as one constraint among three in the animal_moral_status kernel family, the extraction-coordination hybrid, the rising theater ratio, the mandatrophy of a 19th-century anti-cruelty impulse that now legitimates 21st-century industrial systems.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents gratuitous cruelty and establishes baseline care standards in systems where animals are used for food, research, labor, and entertainment. Solves the coordination problem of defining 'acceptable' suffering thresholds and enforcing them across fragmented industries.
% TRANSFER_FUNCTION: Moves moral legitimacy (the 'humane' label) from the animals' constrained suffering to regulated industries (market premium, social license) and welfare organizations (institutional standing, funding). Moves compliance costs from industries to consumers via price. Moves regulatory authority to veterinary and auditing professions.
% ABSENT_VOICES: Animals themselves — they cannot speak, consent, or exit. Abolitionist advocates — excluded from the regulatory consensus as 'unreasonable.' Wild animals affected by agricultural expansion — not covered by welfare codes for farmed animals. Future generations of animals — no representation in current standard-setting.
% DISAPPEARANCE_RATIONALE: If the welfare framework vanished overnight, industries would lose the 'humane' label that preserves social license, welfare organizations would lose their institutional seat, and the legal baseline against cruelty would revert to property-law minimalism (or expand to abolitionist prohibitions, depending on political momentum). The world of animal use would rearrange — either toward less regulated suffering or toward rights-based prohibition.
% FOUNDING_PROBLEM: In the early 19th century, animal use was unregulated: overt cruelty (bear-baiting, horse-beating, slaughterhouse sadism) was legal and common. The founding problem was preventing gratuitous, sadistic suffering in a world where animals were legally property with no protections.
% FOUNDING_PROBLEM_CORROBORATION: Historians of animal law (e.g., Pearson, Favre) attest the founding problem was overt cruelty in pre-regulatory use. Welfare organizations attest the problem is still live (ongoing cruelty cases). Abolitionist scholars (e.g., Francione) and some legal historians attest the founding problem is substantially solved in its original form — overt sadistic cruelty is now prohibited — and the framework persists as mandatrophy. Industry representatives attest the framework solves the ongoing problem of public trust. No single corroboration is universally accepted.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects that the constraint extracts compliance costs from industries but also extracts the moral credit for 'humane' treatment, which industries convert to market premium and welfare organizations convert to legitimacy. The extraction is moderate because the constraint does genuinely reduce suffering above a counterfactual baseline of zero regulation. Suppression (0.48) is structural: animals cannot exit the system, and human advocates face legal and economic barriers to challenging 'humane' standards. Theater ratio (0.38) has risen steadily — early enforcement targeted overt cruelty (low theater), while modern enforcement increasingly performs compliance with standards that permit significant suffering (e.g., confinement systems meeting 'enriched cage' standards). Accessibility collapse (0.42) is moderate: alternatives (plant-based systems, non-animal research) exist but are structurally disadvantaged by the welfare framework's legitimating effect. Resistance (0.55) is significant: from abolitionist critiques, industry lobbying against stricter standards, and public ambivalence.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare organization seat, the constraint appears as genuine coordination — a hard-won system that reduces suffering incrementally. From the industry seat, it appears as managed extraction — a cost of doing business that buys social license. From the animal's seat (modeled analytically), it appears as a snare — the 'humane' label makes their suffering legitimate and invisible. The engine computes this divergence from the structural data: same constraint, three different type experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulated animal industries are structural beneficiaries: they gain the 'humane' label that preserves social license and market access, while the compliance costs are passed to consumers and the regulatory burden is manageable for large operators (d ~ 0.2). Welfare organizations are beneficiaries: they gain institutional standing, funding streams, and policy access as the designated interpreters of 'humane' (d ~ 0.15). Veterinary profession and compliance auditors are secondary beneficiaries: professional monopoly on welfare certification. Animals under 'humane' systems are victims: they bear suffering that is legally sanctioned and structurally invisible because it falls within approved parameters (d ~ 0.85). The directionality gradient is steep: the constraint extracts moral legitimacy from the animals' suffering and distributes it to the human institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing gratuitous cruelty in pre-regulatory animal use) is substantially solved in its original form — overt sadistic cruelty is legally prohibited and socially condemned in most jurisdictions. Yet the arrangement has expanded rather than sunsetted: the regulatory framework now governs the details of industrial systems that would be unrecognizable to the 19th-century founders. The mandatrophy is unresolved: the constraint persists because it now serves the legitimacy needs of both industry and welfare organizations, not because the founding problem requires its current form. The theater ratio rise tracks this — more enforcement energy goes into maintaining the 'humane' label than into reducing suffering at the margin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_of_animal_moral_status_kernel,
    'Is this constraint a genuine coordination mechanism for reducing suffering, or an extraction apparatus that legitimates continued use while capturing the moral credit for ''humane'' treatment?',
    'Longitudinal analysis of welfare standard evolution: if standards tighten only when enforcement costs are low and industry compliance is high, the coordination function dominates; if standards tighten selectively where they preserve industry structure while expanding regulatory capture, the extraction function dominates.',
    'If extraction-dominant, the welfare reading functions as a tangled_rope with asymmetric benefit capture (industry gains legitimacy, welfare orgs gain institutional standing) while animals remain in the victim set for ''humane'' suffering. If coordination-dominant, it approaches a rope with genuine suffering reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reading_of_animal_moral_status_kernel, conceptual, 'Whether the welfare reading is primarily coordination or extraction in its structural operation').

omega_variable(
    sentience_threshold_boundary,
    'Where does the welfare reading draw the sentience threshold for moral considerability, and is that boundary stable or strategically placed?',
    'Comparative analysis of which taxa are covered by welfare regulations across jurisdictions and over time, correlated with scientific consensus shifts on sentience.',
    'If the boundary tracks scientific consensus, the reading has epistemic integrity; if it excludes taxa that are scientifically contested but economically convenient to exclude (e.g., decapod crustaceans, cephalopods in some jurisdictions), the boundary functions as an extraction-management tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_boundary, empirical, 'Stability and epistemic grounding of the sentience boundary in welfare regulations').

omega_variable(
    suppression_mechanism_ambiguity_welfare_enforcement,
    'Is the measured suppression structural (legal barriers to exit, economic dependency on animal use systems) or internalized (cognitive patterns that normalize ''humane'' suffering as acceptable)?',
    'Post-exit trajectory analysis: track individuals who leave animal-use industries — if suppression perception persists after structural barriers are removed, reclassify as partially internalized. Compare jurisdictions with different legal exit costs.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target (animals, and humans who would advocate more strongly) carries the suppression cognitively after structural exit. This affects classification: a structurally moderate suppression with high internalization operates closer to snare dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_welfare_enforcement, empirical, 'Structural vs. internalized suppression in the welfare regulatory regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1820, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t1820, animal_moral_status__welfare_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t1870, animal_moral_status__welfare_reading, theater_ratio, 1870, 0.18).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t1920, animal_moral_status__welfare_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.32).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t2025, animal_moral_status__welfare_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t1820, animal_moral_status__welfare_reading, base_extractiveness, 1820, 0.15).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t1870, animal_moral_status__welfare_reading, base_extractiveness, 1870, 0.22).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t1920, animal_moral_status__welfare_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.31).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t2025, animal_moral_status__welfare_reading, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t1820, animal_moral_status__welfare_reading, suppression_requirement, 1820, 0.25).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t1870, animal_moral_status__welfare_reading, suppression_requirement, 1870, 0.35).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t1920, animal_moral_status__welfare_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t2025, animal_moral_status__welfare_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.15).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_agriculture_regulatory_capture).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_research_ethics_committee_structure).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_welfare_certification_market).

% DUAL FORMULATION NOTE:
% This welfare reading decomposes the animal_moral_status kernel into a constraint on methods of use (not use itself). The property_reading and abolitionist_reading are separate constraints with different ε values, beneficiary/victim structures, and claimed types. They are linked here because they share the kernel and the welfare reading's regulatory framework is often cited as evidence against abolitionist claims (look, we regulate suffering) and property claims (look, we recognize sentience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, institutional, 0.15).
constraint_indexing:directionality_override(animal_moral_status__welfare_reading, organized, 0.2).
constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
