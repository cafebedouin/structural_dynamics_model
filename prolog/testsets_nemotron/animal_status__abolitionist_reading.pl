% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Status — Inherent Value Precluding All Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The abolitionist reading of animal status asserts that sentient animals
 *   are rights-holders with inherent value that categorically precludes all
 *   instrumental use by humans. This reading does not describe the current
 *   legal order (which treats animals as property) — it describes the
 *   standing arrangement of instrumental use (factory farming, animal
 *   research, wildlife management, pet breeding, entertainment) AS SEEN FROM
 *   the abolitionist commitment. From this reading's lights, the existing
 *   system is a snare: it extracts the totality of animals' lives, labor, and
 *   bodies for human benefit while suppressing the moral truth of their
 *   rights-holder status. The constraint's ε=0.92 reflects the abolitionist
 *   assessment of the standing arrangement's extractiveness — near-total,
 *   because every use category treats the animal as a means. Welfare reforms
 *   are rejected not because they don't reduce suffering, but because they
 *   function as legitimation: they make the underlying rights violation
 *   appear morally managed, stabilizing the extraction apparatus. The
 *   welfare_reading and property_reading are sibling constraints (separate
 *   files) with different ε values over the same referent.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: Primary beneficiary (institutional/arbitrage) — captures the overwhelming share of extraction value
 *   - biomedical_research_establishment: Primary beneficiary (institutional/constrained) — captures high-value extraction with some regulatory friction
 *   - farmed_animals: Primary victim (powerless/trapped) — bears total extraction (life, body, reproductive capacity) with zero exit
 *   - laboratory_animals: Primary victim (powerless/trapped) — bears total extraction in controlled environments with zero exit
 *   - animal_advocates_abolitionist: Observer (organized/analytical) — reads the structure as snare, pushes for abolition
 *   - animal_advocates_welfarist: Observer (organized/analytical) — reads the structure as amenable to reform, pushes for welfare improvements
 *   - legal_system: Agenda setter (institutional/constrained) — administers the property-status framework, resists rights recognition
 *   - consumers: Secondary beneficiary (organized/mobile) — benefit from cheap animal products, diffuse complicity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.78).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading of Animal Status — Inherent Value Precluding All Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '7d57542c-b535-4e47-a194-4a2977d60d88').
narrative_ontology:cs_kernel_codification('7d57542c-b535-4e47-a194-4a2977d60d88', distributed).
narrative_ontology:cs_authority_grounding('7d57542c-b535-4e47-a194-4a2977d60d88', distributed).
narrative_ontology:cs_reading_relation('7d57542c-b535-4e47-a194-4a2977d60d88', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('7d57542c-b535-4e47-a194-4a2977d60d88', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('7d57542c-b535-4e47-a194-4a2977d60d88', foundational, sentience_sufficient_for_inherent_value).
narrative_ontology:cs_axiom_status(sentience_sufficient_for_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('7d57542c-b535-4e47-a194-4a2977d60d88', sentience_sufficient_for_inherent_value, deontological).
narrative_ontology:cs_axiom('7d57542c-b535-4e47-a194-4a2977d60d88', foundational, instrumental_use_violates_inherent_value).
narrative_ontology:cs_axiom_status(instrumental_use_violates_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('7d57542c-b535-4e47-a194-4a2977d60d88', instrumental_use_violates_inherent_value, deontological).
narrative_ontology:cs_axiom('7d57542c-b535-4e47-a194-4a2977d60d88', secondary, welfare_reforms_legitimate_exploitation).
narrative_ontology:cs_axiom_status(welfare_reforms_legitimate_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('7d57542c-b535-4e47-a194-4a2977d60d88', welfare_reforms_legitimate_exploitation, instrumental).
narrative_ontology:cs_reference_frame('7d57542c-b535-4e47-a194-4a2977d60d88', moral_equality_of_sentient_beings).
narrative_ontology:cs_drift_state('7d57542c-b535-4e47-a194-4a2977d60d88', contemporary_animal_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d57542c-b535-4e47-a194-4a2977d60d88', '2026-08-14T12:00:00Z').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, wildlife_management_agencies).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, exotic_pet_trade).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_industry_using_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, wild_animals_under_management).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, companion_animals_in_breeding_trade).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_in_entertainment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, consumers).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, sentience_sufficient_for_rights).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, instrumental_use_incompatible_with_inherent_value).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, welfare_reforms_legitimate_exploitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of animal use through vertical integration, regulatory capture, and market power. Collects the overwhelming share of extraction value (trillions in annual revenue). Could pivot to plant-based systems but captures rents from the existing arrangement. Exit is arbitrage-grade: capital is mobile, infrastructure is replaceable, political influence protects the model.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Directs animal use in research through funding priorities, protocol review, and publication norms. Captures high-value extraction (knowledge, careers, patents, drug pipelines) with some regulatory friction (IACUCs, 3Rs mandates). Exit is constrained: non-animal methods exist but validation pathways are slow; institutional prestige and funding structures are locked to animal models.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, biomedical_research_establishment, beneficiary).

% Bear total extraction: lives shortened from years to weeks, bodies redesigned for yield, reproductive autonomy eliminated, social bonds severed, physical confinement absolute. Zero exit — no legal standing, no physical escape, no cognitive capacity to conceive alternatives. Every aspect of existence is instrumentalized for human consumption.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bear total extraction in controlled environments: induced disease, invasive procedures, terminal endpoints, social isolation, behavioral deprivation. Zero exit — bred for use, legally property, physically confined. The 3Rs (replacement, reduction, refinement) modulate intensity but do not challenge the instrumental framework.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Subject to lethal management (culling, hunting quotas, predator control), habitat conversion for agriculture, and population manipulation for human preferences. Exit is trapped at population level — no refuge from human-dominated landscapes, no legal standing to resist management decisions.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, wild_animals_under_management, payer,
    powerless, biographical, trapped, global).

% Bred for morphological extremes causing suffering, culled for aesthetic deviations, reproductive cycles controlled, genetic diversity sacrificed for breed standards. Exit is trapped — legally property, commercially produced, dependent on human guardians who may prioritize breed conformity over welfare.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, companion_animals_in_breeding_trade, payer,
    powerless, biographical, trapped, global).

% Forced performance, confinement, travel, training through coercion, social deprivation. Zero exit — legally property, commercially valuable only as performers, physically controlled. Sanctuaries exist but capacity is negligible relative to the population in use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_in_entertainment, payer,
    powerless, biographical, trapped, global).

% Read the standing arrangement as a snare. Push for legal personhood, rights recognition, and total abolition of instrumental use. Reject welfare reforms as legitimation. Their exit is analytical — they can observe, analyze, and advocate, but cannot directly alter the constraint's operation without institutional power.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_advocates_abolitionist, observer,
    organized, generational, analytical, global).

% Read the standing arrangement as amenable to incremental improvement. Push for welfare reforms (cage-free, enrichment, stunning methods, 3Rs). Accept instrumental use as given. Their exit is analytical — they work within the system to modify its parameters, not its nature.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_advocates_welfarist, observer,
    organized, generational, analytical, global).

% Administers the property-status framework: animals are chattel, welfare statutes create narrow exceptions, standing doctrines block rights claims. Resists rights recognition through precedent, standing requirements, and deference to legislative inaction. Exit is constrained — courts could recognize rights (as some have for rivers, corporations) but institutional inertia and separation-of-powers norms inhibit it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from cheap animal products, medical advances, and entertainment. Bear diffuse indirect costs (health externalities, environmental degradation, moral complicity). Exit is mobile — plant-based alternatives are increasingly available, accessible, and socially normalized. Their choices collectively shift demand but individual agency is limited by price, access, and cultural conditioning.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, consumers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing arrangement coordinates human food security, biomedical knowledge production, wildlife population management, and companion animal provisioning through the instrumental use of animals. The abolitionist reading holds this coordination is not necessary — all functions can be served by non-animal means — and the arrangement persists because it extracts value for powerful beneficiaries, not because it solves coordination problems that cannot otherwise be solved.
% TRANSFER_FUNCTION: Moves the totality of animals' lives, bodies, reproductive capacity, labor, and freedom from animals (victims) to human industries and consumers (beneficiaries). The transfer is not reciprocal — animals receive nothing but the conditions of their use. Welfare regulations transfer marginal improvements in conditions but leave the extractive structure intact.
% ABSENT_VOICES: The animals themselves are the primary absent voices — they cannot speak, litigate, vote, or organize. Their interests are represented only by human advocates who disagree on what those interests require. Future generations of animals (who will be born into the system) are also absent. Indigenous and traditional communities with non-instrumental animal relations are often excluded from policy discourse dominated by industrial and Western legal frameworks.
% DISAPPEARANCE_RATIONALE: If the property-status framework and its enforcement vanished overnight, animal agriculture would collapse (no legal basis for ownership, breeding, slaughter), biomedical research would be forced into non-animal methods immediately, wildlife management would shift from lethal control to coexistence, the pet trade would lose its legal foundation, and animals in entertainment would have no commercial basis. The global food system, research enterprise, and wildlife governance would reorganize around non-animal alternatives — a fundamental rearrangement.
% FOUNDING_PROBLEM: The property-status framework for animals was not 'built' at a single moment but evolved from Roman law (animals as res), Christian dominion theology, Cartesian mechanism (animals as automata), and industrial capitalism's need for scalable biological inputs. The functional founding problem was: how to organize biological resources for growing human populations with pre-scientific technology? The abolitionist reading holds this problem is solved — we now have the knowledge and technology to meet human needs without instrumental animal use.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (organizing biological resources with pre-industrial technology) is corroborated as dead by: (1) the existence of plant-based food systems that nutritionally adequacy at scale (FAO, IPCC, nutrition science consensus); (2) the rapid advancement of non-animal research methods (organoids, organs-on-chips, computational toxicology, human-relevant models) documented by NIH, EPA, EMA, and pharmaceutical industry roadmaps; (3) the demonstrated feasibility of coexistence-based wildlife management (rewilding projects, non-lethal predator deterrence, community-based conservation). No credible source outside the benefiting industries claims the original problem requires the current arrangement. The benefiting industries themselves do not claim the founding problem persists — they argue for the arrangement's efficiency, not its necessity.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε=0.92: From the abolitionist reading's lights, the standing arrangement extracts virtually everything from animals — their lives, bodily integrity, reproductive autonomy, social bonds, and freedom — for human benefit. The extraction is not marginal; it is constitutive of the use categories. Suppression=0.78: The constraint is maintained by legal property status, industrial infrastructure, cultural normalization, and active resistance to rights recognition. Animals have zero exit (trapped); human beneficiaries have high exit (arbitrage/constrained). Theater_ratio=0.15: The arrangement makes minimal performative gestures toward animal interests (welfare regulations) but these are functionally legitimation, not coordination — the core function is extraction. Accessibility_collapse=0.35: Alternatives (plant-based systems, non-animal research methods, wildlife coexistence) exist and are expanding but face structural barriers. Resistance=0.68: Growing abolitionist movement, legal challenges, and consumer shifts meet significant institutional pushback.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist seat reads the standing arrangement as pure extraction (snare) because the coordination function (food, knowledge, companionship) is achievable without instrumental use — the use is not necessary, only habitual and profitable. The property_reading seat reads the same arrangement as mountain-like (natural order of human dominion) or rope (efficient resource allocation). The welfare_reading seat reads it as tangled_rope (genuine coordination of human needs with animal welfare, but asymmetric extraction persists). The engine computes these per-seat types from the structural data; this story authors the abolitionist reading's structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (animal_agriculture_industry, biomedical_research_establishment, wildlife_management_agencies, exotic_pet_trade, entertainment_industry_using_animals) collect the extraction value — they are the agenda setters and direct beneficiaries of the property-status framework. Victims (farmed_animals, laboratory_animals, wild_animals_under_management, companion_animals_in_breeding_trade, animals_in_entertainment) bear the total extraction with zero exit — they are powerless and trapped. Consumers are diffuse secondary beneficiaries with mobile exit (can choose alternatives). Legal system is agenda setter with constrained exit (institutional inertia). Abolitionist and welfarist advocates are observers with analytical exit. The directionality derivation follows: victims are full targets (d→1.0), beneficiaries are full beneficiaries (d→0.0), consumers sit nearer symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate 'animals as property for human use' has outlived any plausible coordination function — the original problems (food security, medical knowledge, labor) are solvable without instrumental use. The arrangement persists purely through extraction momentum and legitimation theater (welfare reforms). This is not a degraded rope (piton) because extraction is active and increasing (intensification of animal agriculture, expansion of biomedical use), not atrophied. It is a snare: the coordination story is cover, persistence depends on coercion and suppressing the rights-holder alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the animal_status kernel (abolitionist_reading), distinct from welfare_reading and property_reading?',
    'The constraint story explicitly instantiates the abolitionist reading; sibling readings are separate constraint files. The kernel context in commentary records the relationship.',
    'Confirms ε-invariance: this reading''s ε (0.92) assesses the standing instrumental-use arrangement from the abolitionist lights, not a hypothetical rights-respecting world. The welfare and property readings will author different ε values over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the committer frame: one reading, one constraint, one ε').

omega_variable(
    welfare_reform_legitimation_mechanism,
    'Do welfare reforms functionally legitimize continued instrumental use by making it appear morally acceptable, as the abolitionist reading claims?',
    'Historical analysis of whether welfare improvements correlate with stabilization or expansion of use categories; counterfactual assessment of abolitionist movement traction with vs. without welfare reforms.',
    'If welfare reforms are legitimation, the standing arrangement''s extraction is higher than welfare metrics suggest — the abolitionist reading''s ε captures the full instrumental-use structure. If not, the abolitionist reading overstates extraction by treating all use as equally extractive regardless of welfare conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_legitimation_mechanism, empirical, 'Whether welfare reforms function as legitimation cover for continued extraction').

omega_variable(
    rights_ascription_vs_legal_personhood,
    'Does the abolitionist claim ''animals are rights-holders'' require legal personhood, or is it a moral claim that precedes and demands legal recognition?',
    'Jurisprudential analysis of rights theories: interest-based vs. will-based conceptions; historical cases of rights-holders without legal personhood (e.g., future generations, comatose humans).',
    'If rights-holder status requires legal personhood, the constraint''s suppression score (0.78) may overstate the current legal reality — animals lack standing to enforce rights. If moral rights precede legal recognition, the suppression score correctly captures the structural denial of moral standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_ascription_vs_legal_personhood, conceptual, 'Whether abolitionist rights-claim is moral-prelegal or legal-constitutive').

omega_variable(
    interspecies_justice_scope,
    'Does the abolitionist constraint extend to all sentient animals equally, or do cognitive complexity gradients create internal boundaries within the victim set?',
    'Comparative analysis of abolitionist literature: Regan''s subject-of-a-life criterion vs. Singer''s equal consideration of interests vs. Francione''s sentience-sufficiency; empirical work on sentience distribution across taxa.',
    'If the victim set is internally differentiated, the constraint''s ε=0.92 may overgeneralize — extraction intensity differs across use categories (factory farming vs. invertebrate research). If uniform, the single ε is descriptively adequate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interspecies_justice_scope, conceptual, 'Whether the victim set is unitary or stratified by cognitive criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_abolitionist_tr_t1975, animal_status__abolitionist_reading, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1985, animal_status__abolitionist_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1995, animal_status__abolitionist_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2005, animal_status__abolitionist_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2015, animal_status__abolitionist_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2025, animal_status__abolitionist_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(animal_status_abolitionist_be_t1975, animal_status__abolitionist_reading, base_extractiveness, 1975, 0.85).
narrative_ontology:measurement(animal_status_abolitionist_be_t1985, animal_status__abolitionist_reading, base_extractiveness, 1985, 0.88).
narrative_ontology:measurement(animal_status_abolitionist_be_t1995, animal_status__abolitionist_reading, base_extractiveness, 1995, 0.9).
narrative_ontology:measurement(animal_status_abolitionist_be_t2005, animal_status__abolitionist_reading, base_extractiveness, 2005, 0.91).
narrative_ontology:measurement(animal_status_abolitionist_be_t2015, animal_status__abolitionist_reading, base_extractiveness, 2015, 0.92).
narrative_ontology:measurement(animal_status_abolitionist_be_t2025, animal_status__abolitionist_reading, base_extractiveness, 2025, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_abolitionist_su_t1975, animal_status__abolitionist_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(animal_status_abolitionist_su_t1985, animal_status__abolitionist_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(animal_status_abolitionist_su_t1995, animal_status__abolitionist_reading, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement(animal_status_abolitionist_su_t2005, animal_status__abolitionist_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(animal_status_abolitionist_su_t2015, animal_status__abolitionist_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(animal_status_abolitionist_su_t2025, animal_status__abolitionist_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.15).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_agriculture_regulatory_framework).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, biomedical_research_oversight).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, wildlife_management_policy).

% DUAL FORMULATION NOTE:
% This constraint (abolitionist_reading) and its siblings (welfare_reading, property_reading) form a constraint family over the animal_status kernel. Each reading instantiates a different constraint with different ε, different victim/beneficiary structures, and different classification. The abolitionist reading's ε=0.92 assesses the standing instrumental-use arrangement from abolitionist lights; the welfare reading will author a lower ε (seeing welfare reforms as genuine coordination); the property reading will author near-zero ε (seeing the arrangement as natural/efficient). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__abolitionist_reading, institutional, 0.1).
constraint_indexing:directionality_override(animal_status__abolitionist_reading, powerless, 1.0).
constraint_indexing:directionality_override(animal_status__abolitionist_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
