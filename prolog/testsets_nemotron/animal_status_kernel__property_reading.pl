% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animals as Property: Ownership-Derived Moral Considerability
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the property_reading of the
 *   animal_status_kernel. It asserts that animals are property, that any
 *   moral considerability they possess derives entirely from the property
 *   rights of their owners, and that economic value is the only relevant
 *   metric for evaluating their treatment. This is the dominant legal
 *   framework globally: animals are legal objects, not subjects. Anti-cruelty
 *   statutes exist but are interpreted to protect the owner's property
 *   interest — suffering is 'unnecessary' only when it serves no economic
 *   purpose for the owner. The constraint extracts the full value of animal
 *   lives for human economic benefit while suppressing the moral recognition
 *   of animal sentience and interests. The claimed type is snare: pure
 *   extraction masked by a thin coordination veneer (anti-cruelty laws that
 *   protect property value, not animals).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.82).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animals as Property: Ownership-Derived Moral Considerability").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b').
narrative_ontology:cs_kernel_codification('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', formalized).
narrative_ontology:cs_authority_grounding('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', extraction).
narrative_ontology:cs_interpretation_layer_present('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b').
narrative_ontology:cs_reading_relation('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', foundational, animals_are_property).
narrative_ontology:cs_axiom_status(animals_are_property, holdable).
narrative_ontology:cs_axiom_grounding('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', animals_are_property, conventional).
narrative_ontology:cs_axiom('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', foundational, moral_considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', moral_considerability_derives_from_ownership, conventional).
narrative_ontology:cs_axiom('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', foundational, economic_value_is_sole_relevant_metric).
narrative_ontology:cs_axiom_status(economic_value_is_sole_relevant_metric, holdable).
narrative_ontology:cs_axiom_grounding('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', economic_value_is_sole_relevant_metric, instrumental).
narrative_ontology:cs_reference_frame('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', classical_property_law_framework).
narrative_ontology:cs_drift_state('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', contemporary_sentience_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8cf2a1e1-76ba-457c-88c7-9a1c3b70a52b', '2026-08-14T12:00:00Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, commercial_breeders).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, entertainment_animal_operators).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, companion_animals_in_neglect).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, wild_animals_in_captivity).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, breeding_stock_animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_absolutism).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_value_as_sole_metric).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, ownership_entails_unrestricted_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the legal and moral boundaries of animal use through property rights. Set the terms of anti-cruelty statutes (which protect their property value, not animal interests). Exercise unrestricted use rights over animals they own. Lobby against welfare regulations that constrain profitable uses. Their exit from the constraint is trivial — they authored it and benefit from it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate at industrial scale using animals as property. The property framework enables confinement, selective breeding, early slaughter, and commodity treatment that maximize throughput per unit cost. Welfare regulations that do exist are shaped by industry lobbying to avoid meaningful constraints on production methods. Economic value is the only relevant metric — animals are inputs.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Use animals as research tools under property rights that permit invasive procedures, genetic modification, and terminal experiments. The constraint that moral considerability derives from ownership means no intrinsic limit on what can be done to an animal in research — only procedural oversight (IACUC) that protects institutional legitimacy, not animal interests. They capture the scientific and economic value; animals bear all costs.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Produce animals as inventory — puppies, exotic pets, livestock genetics. Property status means breeding stock can be kept in conditions that maximize reproductive output without regard for psychological or physical welfare beyond what affects market value. Culling of surplus or defective animals is a routine property management decision.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, commercial_breeders, beneficiary,
    organized, biographical, mobile, national).

% Use animals in circuses, films, racing, fighting, and displays. Property rights allow training methods, confinement, and performance demands that would be impermissible if animals had independent moral standing. Anti-cruelty laws apply only when the abuse reduces the animal's economic value to the owner — not when it causes suffering that doesn't affect market price.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, entertainment_animal_operators, beneficiary,
    organized, biographical, constrained, national).

% Born into property status with no exit. Their entire existence — confinement, mutilation without analgesia, separation from offspring, transport, slaughter — is structured by the owner's economic calculus. Anti-cruelty statutes do not protect them from standard industry practices (e.g., gestation crates, battery cages, maceration of male chicks) because those practices are deemed economically necessary and therefore not 'cruel' under the property framework. They bear 100% of the extraction.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bred or captured for use as research tools. Property status means they have no claim against pain, distress, isolation, or death inflicted in experiments — only the procedural requirement that the research protocol be approved. The constraint that moral considerability derives from ownership means their suffering counts only if it threatens data validity or institutional reputation. They are the primary extraction sink for biomedical knowledge production.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Legally property of owners who may neglect, abandon, or kill them with minimal legal consequence — anti-cruelty statutes require 'unnecessary' suffering, and neglect that doesn't destroy economic value often falls below enforcement thresholds. No right to veterinary care, social interaction, or freedom from chronic distress. Their moral considerability is entirely contingent on the owner's discretion.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, companion_animals_in_neglect, payer,
    powerless, immediate, trapped, local).

% Held in zoos, aquariums, private collections, roadside attractions. Property status permits lifetime confinement in enclosures that prevent species-typical behaviors, breeding programs that treat individuals as genetic stock, and 'surplus' management (killing healthy animals). Welfare regulations address physical health for display value, not psychological well-being or autonomy.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, wild_animals_in_captivity, payer,
    powerless, immediate, trapped, global).

% Kept alive solely for reproductive output — dairy cows, sow lines, stud animals, laboratory colonies. Property framework permits repeated forced impregnation, separation from offspring, and culling when productivity declines. Their bodies are capital equipment; moral considerability extends only to maintaining their economic function.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, breeding_stock_animals, payer,
    powerless, immediate, trapped, global).

% Enforce anti-cruelty statutes that are structurally constrained by the property framework: they can only prohibit suffering that is 'unnecessary' — where necessity is defined by the owner's economic purpose. Their enforcement activity legitimizes the system by creating the appearance of protection while the fundamental extraction continues unchanged.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_welfare_regulators, observer,
    institutional, generational, analytical, national).

% Provide medical care to animals within the property framework. Their ethical obligations are mediated by the client (owner) relationship — they cannot advocate for the animal against the owner's economic decisions. The property constraint shapes veterinary ethics: 'patient advocacy' stops at the owner's property rights.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, veterinary_profession, observer,
    organized, biographical, constrained, national).

% Analyze the legal architecture of animal property status. Document how the constraint operates: animals as legal objects, standing barriers, the 'unnecessary suffering' loophole, the economic necessity defense. Their work maps the constraint's mechanics but does not alter its operation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, enforceable legal framework for the allocation, use, and disposition of animal bodies and labor. Resolves disputes over animal control by assigning exclusive rights to owners. Enables markets in animal products, research, and entertainment by making animals transferable property.
% TRANSFER_FUNCTION: Moves the entire value of animal lives — bodies, labor, reproductive output, genetic material, suffering capacity — from animals to owners. Owners capture all economic value; animals bear all physical and psychological costs. Anti-cruelty statutes transfer only the marginal cost of avoiding 'unnecessary' suffering (where necessity = owner's economic interest) back to owners as compliance burden.
% ABSENT_VOICES: Animals themselves — the primary subjects of the constraint — have no voice in the legal or moral framework that defines them as property. Their interests are represented only insofar as they align with owner economic interests. Wild animal populations, future generations of farmed animals, and animals in jurisdictions without even minimal anti-cruelty laws are entirely absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the property reading vanished overnight — if animals were recognized as having moral considerability independent of ownership — the entire architecture of animal agriculture, biomedical research, commercial breeding, and animal entertainment would face existential legal and economic disruption. Markets in animal bodies would require new justification. Anti-cruelty statutes would become floors, not ceilings. The global food system, pharmaceutical pipeline, and companion animal trade would reorganize around a fundamentally different moral-legal baseline.
% FOUNDING_PROBLEM: Pre-modern legal systems needed a stable framework for resolving disputes over animal control, enabling agricultural production, and protecting human economic interests in livestock. The property classification provided a ready-made, well-understood legal category that could be extended to animals without inventing new jurisprudence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Favre, Sunstein, Francione) document that the property classification was a pragmatic legal expedient, not a moral discovery. The founding problem — dispute resolution and economic coordination in agrarian societies — is acknowledged as substantially transformed by industrialization, scientific understanding of sentience, and the scale of modern animal use. However, beneficiaries (animal agriculture, biomedical research) assert the problem remains live: they claim property status is still necessary for 'efficient' production and research. No corroborating source outside the beneficiary set endorses the claim that the original founding problem justifies the current scope of the constraint.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.88) is very high because the constraint places zero intrinsic moral limit on what owners may do to animals — the only limits are instrumental (protecting economic value). Suppression (0.82) is high because the property framework actively excludes animals from legal personhood, denies them standing, and uses the 'unnecessary suffering' standard to legitimize economically necessary suffering. Theater ratio (0.25) reflects that anti-cruelty laws perform a protective function while structurally ensuring that the vast majority of suffering (industrial agriculture, standard research protocols) remains legal. Accessibility collapse (0.75) is high because the property framework makes it conceptually difficult to imagine animals as anything but property — the legal category colonizes moral imagination. Resistance (0.45) is moderate: welfare reforms occur but operate within the property framework; abolitionist challenges remain marginal in legal practice.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats, the property framework looks like a stable, efficient coordination mechanism: it allocates animal resources, resolves disputes, enables markets. From the payer seats (animals), it is totalizing extraction with no exit and no recourse. The observer seats see both: the legal architecture is coherent and stable (mountain-like from the institutional perspective) while being maximally extractive from the subject perspective. The engine computes this divergence from the structural data — the property reading's claim that this is 'just how ownership works' is the cover story the framework tells itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and industries (agenda_setters, beneficiaries) sit at d ≈ 0.05–0.15: they capture the full economic value of animal use, control the regulatory framework, and face trivial exit costs. Animals (farmed, laboratory, companion in neglect, captive wild, breeding stock) are powerless, trapped, immediate-horizon payers at d ≈ 0.95–1.0: they bear 100% of the physical and psychological costs with zero exit, zero voice, and zero legal standing. Regulators, veterinarians, scholars (observers) sit at d ≈ 0.5 (analytical): they see the structure but are neither primary extractors nor primary victims. The property framework's directionality is maximally asymmetric — it is designed to be.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (agrarian dispute resolution and economic coordination) is substantially transformed — industrial animal use operates at a scale, intensity, and moral distance that the original property framework never contemplated. Yet the constraint persists and has intensified (extraction rising from 0.65 to 0.88 over the interval). The property classification has become a snare: it extracts massively from animals while using anti-cruelty statutes as a performative shield. The mandatrophy is unresolved — the constraint's mandate (stable property allocation) has been overtaken by its extraction function (industrial animal use), but the legal system treats the extraction as the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Is the animal_status_kernel best framed as (a) the legal classification of animals, (b) the moral considerability of animals, or (c) the relationship between legal status and moral considerability? Different framings yield different constraint boundaries and different ε values.',
    'Trace how each framing structures the beneficiary/victim sets and the coordination/extraction boundary. The legal-classification framing centers property law; the moral-considerability framing centers sentience and interests; the relationship framing centers the derivation claim (moral considerability derives from ownership). The three framings are not equivalent — they produce different constraint identities.',
    'If the kernel is the legal classification, the property_reading is a mountain (positive law is what it is). If the kernel is moral considerability, the property_reading is a snare (deriving morality from ownership is extractive cover). If the kernel is the derivation claim, the property_reading is a tangled_rope (coordination via property law + extraction via derivation). The classification depends on which framing the engine adopts — an irreducible conceptual ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s identity is legal, moral, or the derivation relation between them — and how that choice determines the constraint''s classification.').

omega_variable(
    welfare_regulation_as_coordination_or_theater,
    'Do anti-cruelty and welfare statutes represent genuine coordination (reducing suffering) or theatrical maintenance of the property framework (legitimizing extraction)?',
    'Measure the proportion of animal suffering that welfare regulations actually prevent vs. the proportion they render legally ''necessary.'' Track whether welfare reforms reduce total suffering or merely shift its legal characterization. Compare jurisdictions with stronger vs. weaker welfare laws on aggregate animal welfare outcomes.',
    'If welfare regulations are genuine coordination, the property_reading has a tangled_rope component (coordination + extraction). If they are theater, the property_reading is a pure snare. The theater_ratio trajectory (rising from 0.1 to 0.25) suggests increasing theatricality, but the coordination function (dispute resolution, market enablement) persists independently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_regulation_as_coordination_or_theater, empirical, 'Whether the welfare-regulation layer is a real coordination function or a performative legitimization of the extraction core.').

omega_variable(
    sentience_evidence_as_axiom_overriding_pressure,
    'Does the accumulation of scientific evidence for animal sentience, cognition, and emotional complexity constitute axiom_overriding pressure on the property_reading''s foundational axiom (ownership_entails_unrestricted_use)?',
    'Track whether the property_reading''s defenders modify their axioms in response to sentience evidence (e.g., shifting from ''animals don''t feel pain'' to ''pain doesn''t matter if economically necessary'' to ''we minimize pain within economic constraints''). Document whether the authority structure (legal system, industry) explicitly acknowledges the gap between sentience evidence and the property framework.',
    'If sentience evidence creates substantial axiom_overriding pressure that is unacknowledged by the authority structure, the property_reading''s drift_state magnitude is at least ''substantial'' and the constraint is in active drift toward reclassification. If the authority structure has already absorbed the evidence via reinterpretation (economic necessity as the filter), the drift is acknowledged but managed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_evidence_as_axiom_overriding_pressure, empirical, 'Whether scientific evidence of sentience functions as an overriding pressure on the property_reading''s core derivation claim, and whether that pressure is acknowledged or suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t1800, animal_status_kernel__property_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t1850, animal_status_kernel__property_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t1900, animal_status_kernel__property_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t1950, animal_status_kernel__property_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t2000, animal_status_kernel__property_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(animal_status_kernel__property_reading_tr_t2025, animal_status_kernel__property_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t1800, animal_status_kernel__property_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t1850, animal_status_kernel__property_reading, base_extractiveness, 1850, 0.72).
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t1900, animal_status_kernel__property_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t1950, animal_status_kernel__property_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t2000, animal_status_kernel__property_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(animal_status_kernel__property_reading_be_t2025, animal_status_kernel__property_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t1800, animal_status_kernel__property_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t1850, animal_status_kernel__property_reading, suppression_requirement, 1850, 0.62).
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t1900, animal_status_kernel__property_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t1950, animal_status_kernel__property_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t2000, animal_status_kernel__property_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(animal_status_kernel__property_reading_su_t2025, animal_status_kernel__property_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.15).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, anti_cruelty_statutes).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_welfare_acts).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, ag_gag_laws).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_enterprise_terrorism_act).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, vivisection_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, pet_trade_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, zoo_licensing_acts).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, laboratory_animal_welfare_regulations).

% DUAL FORMULATION NOTE:
% The animal_status_kernel decomposes into three constraint stories: property_reading (this file), welfare_reading, and abolitionist_reading. They share the kernel (the status question) but instantiate different constraints with different ε values, different beneficiary/victim structures, and different classifications. The property_reading (snare, ε=0.88) is the dominant legal framework. The welfare_reading (tangled_rope, ε≈0.45) is the regulatory overlay. The abolitionist_reading (mountain or scaffold depending on framing, ε≈0.05) is the aspirational counter-framework. The property_reading forecloses the abolitionist_reading and influences the welfare_reading. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__property_reading, institutional, 0.08).
constraint_indexing:directionality_override(animal_status_kernel__property_reading, organized, 0.12).
constraint_indexing:directionality_override(animal_status_kernel__property_reading, powerless, 0.97).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
