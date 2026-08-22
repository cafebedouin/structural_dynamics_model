% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Instrumentalization as Rights Violation (Abolitionist Reading)
 *   domain: ethics/political_economy/law
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status asserts that animals are
 *   rights-holders with inherent value that precludes all instrumental use.
 *   Under this reading, any use of animals as means to human
 *   ends—agricultural production, pharmaceutical testing, entertainment,
 *   fashion, or resource extraction—constitutes a systematic rights
 *   violation. The referent for extractiveness is the standing arrangement of
 *   animal instrumentalization as it currently operates, assessed under the
 *   abolitionist reading's own lights: it is a snare extracting bodily
 *   autonomy, freedom, and life itself from animals to service human economic
 *   and cultural interests. The alternatives available to humans (plant-based
 *   food systems, synthetic materials, computational and in vitro testing
 *   methods, animal-free entertainment) exist; the constraint persists
 *   because the beneficiaries have sufficient power to maintain it through
 *   law, markets, and institutional design. This story instantiates ONE
 *   reading of the contested animal-status kernel; the property_reading and
 *   welfare_reading are separate constraints with different victim sets,
 *   beneficiary structures, and ε values.
 *
 * KEY AGENTS:
 *   - domesticated_animals: rights-violating victims (trapped, powerless)
 *   - wildlife_populations: systematic extraction targets (trapped, powerless)
 *   - agricultural commodity producers: agenda setters (organized, mobile exit)
 *   - pharmaceutical testing industry: agenda setters (organized, mobile exit)
 *   - fashion manufacturing: agenda setters (organized, mobile exit)
 *   - entertainment exploitation: agenda setters (organized, mobile exit)
 *   - abolitionist advocates: constrained observers and partial payers (moderate power)
 *   - welfare reformers: excluded from the core debate (moderate power, constrained)
 *   - regulatory authorities: institutional enforcement of property doctrine (institutional power)
 *   - consumers: diffuse beneficiaries of low-cost extraction (organized, mobile exit)
 *   - philosophical tradition (property doctrine): the non-agent kernel of authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumentalization as Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "ethics/political_economy/law").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '9561ca06-010d-4a9a-baf1-222a39cb8f18').
narrative_ontology:cs_kernel_codification('9561ca06-010d-4a9a-baf1-222a39cb8f18', fixed_text).
narrative_ontology:cs_authority_grounding('9561ca06-010d-4a9a-baf1-222a39cb8f18', extraction).
narrative_ontology:cs_interpretation_layer_present('9561ca06-010d-4a9a-baf1-222a39cb8f18').
narrative_ontology:cs_reading_relation('9561ca06-010d-4a9a-baf1-222a39cb8f18', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('9561ca06-010d-4a9a-baf1-222a39cb8f18', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('9561ca06-010d-4a9a-baf1-222a39cb8f18', foundational, animal_sentience_entails_rights).
narrative_ontology:cs_axiom_status(animal_sentience_entails_rights, holdable).
narrative_ontology:cs_axiom_grounding('9561ca06-010d-4a9a-baf1-222a39cb8f18', animal_sentience_entails_rights, deontological).
narrative_ontology:cs_axiom('9561ca06-010d-4a9a-baf1-222a39cb8f18', foundational, instrumental_use_violates_inherent_value).
narrative_ontology:cs_axiom_status(instrumental_use_violates_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('9561ca06-010d-4a9a-baf1-222a39cb8f18', instrumental_use_violates_inherent_value, deontological).
narrative_ontology:cs_reference_frame('9561ca06-010d-4a9a-baf1-222a39cb8f18', animals_as_moral_subjects).
narrative_ontology:cs_drift_state('9561ca06-010d-4a9a-baf1-222a39cb8f18', contemporary_post_animal_advocacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9561ca06-010d-4a9a-baf1-222a39cb8f18', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, agricultural_commodity_producers).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, pharmaceutical_testing_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, fashion_manufacturing).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_exploitation).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, domesticated_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, wildlife_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumer_preferences).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, abolitionist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjected to systematic instrumental use across agriculture, research, entertainment, and textiles. Their interests—survival, bodily autonomy, social connection—are overridden by economic logic that treats them as production inputs. They cannot exit, negotiate, or resist the constraint structurally; their capacity for suffering is acknowledged but discounted by the beneficiaries.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, domesticated_animals, payer,
    powerless, immediate, trapped, global).

% Subjected to resource extraction (fishing, hunting, logging, mining), habitat destruction, and managed exploitation justified by human economic benefit or population control. Their interests as species and as individual beings are subordinated to human utility. The constraint extends through property law, resource permitting, and wildlife management systems that treat animals as harvestable objects.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, wildlife_populations, payer,
    powerless, generational, trapped, global).

% Set production practices that treat animals as unit costs, selecting for breeding and slaughter efficiency. They defend the arrangement by citing economic necessity, consumer demand, and the absence of legal obligation to recognize animal interests independently. They actively enforce the constraint through property law, contractual exclusivity, and opposition to regulatory restrictions on killing methods or confinement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, agricultural_commodity_producers, agenda_setter,
    organized, biographical, mobile, global).

% Uses animals in toxicity, safety, and efficacy testing as the regulatory standard. They defend the constraint as scientifically necessary and legally mandated, despite in vitro alternatives existing. They actively enforce testing protocols that standardize animal suffering as an acceptable research cost.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, pharmaceutical_testing_industry, agenda_setter,
    organized, biographical, mobile, global).

% Uses animal skins, furs, down, and wool as commodities. They defend the constraint as tradition, aesthetic preference, and market demand. They actively enforce the constraint through supply contracts, trade agreements, and regulatory lobbying against restrictions on leather, fur, and wool production.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fashion_manufacturing, agenda_setter,
    organized, biographical, mobile, global).

% Uses animals in circuses, zoos, racing, and fighting as spectacle commodities. They defend the constraint as cultural tradition and viewer experience. They actively enforce the constraint through property claims on animals, contractual control of movement and breeding, and opposition to performance restrictions.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, entertainment_exploitation, agenda_setter,
    organized, biographical, mobile, global).

% Argue that the constraint violates the fundamental rights of animals and should be dismantled entirely. They bear costs through legal penalties for civil disobedience, economic pressure (boycotts hurt their livelihoods if they work in affected industries), and social marginalization. They observe the constraint's operation and seek to transform legal, cultural, and institutional frameworks that instantiate it. Their exit is identity-locked: they could materially exit (adopt welfare reformism, abandon advocacy) but their moral identity makes exit impossible without self-repudiation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, abolitionist_advocates, observer).

% Seek to reduce animal suffering within the framework of continued instrumental use (humane slaughter, cage-free systems, testing alternatives). From the abolitionist reading's perspective, they are excluded from the core debate because their reforms do not challenge the constraint itself—they legitimize and extend it by reducing the moral objections to ongoing use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reformers, excluded,
    moderate, biographical, constrained, global).

% Establish and enforce welfare standards, testing protocols, and resource-use permitting. They actively maintain the constraint by defining the boundary between permissible and impermissible use solely within welfare frameworks, refusing to recognize any inherent right to non-use. They exclude abolitionist framings from legitimate policy discourse.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, regulatory_authorities, agenda_setter,
    institutional, generational, mobile, national).

% The legal and philosophical doctrine that establishes animals as property with no independent moral standing. This tradition provides the justificatory framework that shields the constraint from challenge by refusing to recognize animals as right-holders at all. It is neither an agent nor a beneficiary, but the kernel of authority around which the constraint is organized.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, philosophical_tradition__property_doctrine, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status__abolitionist_reading, philosophical_tradition__property_doctrine).

% Benefit from low-cost animal products (meat, dairy, leather, pharmaceuticals, cosmetics). From the abolitionist reading, they are beneficiaries-by-extraction: their material interest in cheap animal commodities is enabled by the systematic violation of animals' inherent rights. Their exit is possible but economically costly and culturally friction-laden.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumer_preferences, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, agricultural_commodity_producers).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist reading's perspective: the constraint solves no genuine coordination problem. It is not a response to a scarcity that requires allocation or a collective-action problem that requires enforcement. Rather, it is a COVER STORY for extraction: the economic beneficiaries frame instrumental use as a solved coordination problem ('feeding humanity,' 'advancing medicine,' 'cultural tradition') to obscure that they have simply decided animals' interests do not count. The reading does not recognize a coordination function here—only a denial of standing.
% TRANSFER_FUNCTION: Moves the bodily autonomy, freedom from suffering, and life itself of animals into the economic calculus of humans as inputs to commodity production. Money and value flow from consumers to producers; animal bodies and interests flow from animals to humans without compensation, consent, or recognition of the loss. The transfer is one-directional and total: animals cannot refuse, negotiate terms, or retain any claim on their own capacities.
% ABSENT_VOICES: Animals themselves cannot speak or organize in the constraint's institutional contexts (legislatures, markets, property law). Their interests are represented only through advocacy organizations and philosophical argumentation, never through direct participation. The constraint's persistence depends structurally on this enforced silence—if animals could vote, testify, or establish property claims over themselves, the constraint would collapse immediately. Also excluded: the futures of wild animals and ecosystem integrity, which have no seat at the policy table where the constraint is enforced.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight—if animals were recognized as rights-holders and instrumental use prohibited—the world would rearrange profoundly: agriculture would shift to plant-based systems, pharmaceutical and cosmetic testing would accelerate alternative methods, food systems would reorganize around different protein sources and preparation, fashion would shift away from animal textiles, entertainment industries would reorient toward non-animal content, billions of animals currently in captivity would require redirection of resources and institutional change. The economic and cultural infrastructure built on animal instrumentalization is so extensive that its removal would constitute a systemic social reorganization.
% FOUNDING_PROBLEM: The abolitionist reading does not recognize a founding problem in the conventional sense—it reads the arrangement as a choice by the powerful to ignore animal interests, not as a solution to a necessity. If forced to state the founding problem the constraint's defenders would cite: pre-industrial societies needed to use animals for transportation, food, labor, and materials because no alternatives existed; modern animal use inherited that necessity but has become a choice defended by economic interests rather than survival need.
% FOUNDING_PROBLEM_CORROBORATION: The plant-based food industry, synthetic material science, and computer modeling in pharmaceutical testing all demonstrate that modern alternatives to animal use exist where historical necessity once existed. Independent animal welfare economists (e.g., Corey Breier, economists in the effective altruism community) attest that the founding problem is solved—the constraint persists not from necessity but from path dependence, capital investment, and regulatory capture by the agricultural and pharmaceutical industries. Biochemists and materials scientists independent of animal-use industries document viable alternatives; the persistence of animal use despite these alternatives is attested by all these external voices as a choice, not a necessity.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.95, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.95 at interval end) because the constraint takes from animals everything of intrinsic value—autonomy, bodily integrity, and life—and gives nothing back except treatment that meets welfare minimums designed not for animals' sake but to maintain commodity production efficiency and consumer satisfaction. The extraction is total: animals receive no compensation, no choice, and no recognition of the loss. Suppression is also extremely high (0.88) because the constraint's persistence depends on active enforcement through property law, contractual exclusivity, regulatory capture, and ideological conditioning that trains people from childhood to treat animal use as natural and inevitable. The accessibility_collapse is near-complete (0.92) because the alternatives to animal use—plant-based systems, synthetic materials, computational testing—exist but are deliberately made invisible or dismissed as infeasible by the beneficiaries. Once the constraint is understood (that viable alternatives exist and animals' interests matter), the collapse is almost total; the constraint can only persist through suppression of that understanding. Theater_ratio is moderate-to-low (0.42) because some real functionality exists (animals do provide certain services in current systems), but a growing share of enforcement activity defends the property doctrine itself rather than any genuine necessity—welfare improvements, 'humane' slaughter standards, and 'ethical' sourcing are theatrical activities that function primarily to legitimize the underlying extraction and mollify consumer guilt rather than to protect animal interests. Resistance is substantial (0.71) because abolitionist movements, animal advocacy organizations, and philosophical scholarship continue to mount coherent challenges to the constraint; the resistance does not yet translate to systemic change because the suppression (institutional, legal, ideological) is stronger, but the resistance is real and growing.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals (domesticated and wild) are fully in the target position: d approaches 1.0. They bear total extraction (life, autonomy, bodily integrity), have trapped or identity_locked exit (no choices available; their identity as 'food animals' or 'natural resources' is imposed by the constraint), and have powerless power atoms. The beneficiaries (agricultural producers, pharmaceutical industry, fashion, entertainment) occupy positions near d=0.0: they collect value directly, have mobile exit (they could transition to non-animal models but choose not to for profit), and have organized or institutional power. The regulatory authorities that enforce the constraint occupy d positions near beneficiaries (they are funded and staffed by people whose livelihoods depend on animal use industries; their exit from enforcement is constrained though not trapped). Welfare reformers occupy an ambiguous position: they are partly constrained payers (they pay costs for advocating restrictions) but also partly beneficiaries (they benefit from animal products and depend on the industries they reform). From the abolitionist reading, welfare reformers are not truly in the victim set because they accept the core premise of the constraint (that instrumental use is permissible); their exit is more mobile than it appears. Consumers are diffuse beneficiaries: they pay somewhat through higher prices in alternative systems, but they have mobile exit and substantial benefit collection through low-cost animal products.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply here. The founding problem (pre-industrial necessity for animal use) is dead—alternatives exist—but the constraint persists not through inertial maintenance (piton) but through active enforcement by beneficiaries with sufficient power to keep it in place. This is a snare, not a piton. The theater_ratio being moderate-to-low (not high) reflects that the constraint has genuine extractive function, not just performative maintenance. The beneficiaries actively defend the constraint; they are not zombie-institutional administrators. However, the measurement series shows theater_ratio rising slightly over the interval (from 0.28 to 0.42), suggesting that as the constraint faces more resistance, more enforcement activity becomes defensive and theatrical rather than coordinative. This is a pre-mandatrophy trajectory: as resistance grows and the founding problem becomes more obviously solved, the constraint may be moving toward a piton state in which it persists through institutional inertia and performance rather than active defense—but it is not there yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_commensurability,
    'Is animal sentience sufficient to ground rights-holding status, or is moral agency (reason, autonomy, reciprocal obligation-keeping) a necessary additional condition for rights?',
    'Philosophical and ethical analysis: does the abolitionist reading''s claim rest on sentience alone, or on a broader conception of inherent value? If sentience suffices, then the boundary of the victim set includes all sentient beings and the constraint applies to all animal use. If agency is necessary, the victim set narrows, and some uses may escape the constraint''s classification.',
    'The width of the victim set and the scope of the extractiveness measurement depend on this boundary. If sentience alone suffices, extractiveness is extreme for all animals. If agency is required, the abolitionist reading might classify some animal use as less extractive (e.g., use of non-sentient organisms) or as outside the constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_commensurability, conceptual, 'Whether rights-holding requires sentience alone or sentience + agency.').

omega_variable(
    alternative_viability_empirical,
    'Are plant-based food systems, synthetic materials, and computational/in vitro testing genuinely scalable to replace current animal use at comparable cost and performance?',
    'Longitudinal data on plant-based agriculture scaling, comparative cost analysis of synthetic vs. animal-derived materials, validation studies of non-animal testing methods against existing animal-model reliability.',
    'If alternatives are not scalable or are substantially more costly, the founding problem is not fully dead and the constraint has a residual coordination function—extractiveness would be lower and the classification might shift toward tangled_rope. If alternatives are scalable and cost-competitive, the founding problem is dead and the constraint is pure extraction—extractiveness remains at 0.95 and snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_viability_empirical, empirical, 'Whether viable alternatives to animal use actually exist at scale.').

omega_variable(
    reading_foreclosure_test,
    'Does the abolitionist reading''s assertion that animals are rights-holders logically foreclose the property reading''s assertion that animals are legal objects without independent moral standing within the same normative framework?',
    'Formal analysis of the logical structure: can a single legal and moral framework hold both ''animals are rights-holders'' and ''animals are property without moral standing'' without contradiction? Or do the readings occupy genuinely different frameworks (e.g., abolitionist: rights-based ethics; property: economic utilitarianism) such that the contradiction is framework-relative rather than absolute?',
    'If the readings logically foreclose each other within any single framework, the relation is ''forecloses'' and one reading must eventually win institutional adoption. If they occupy different frameworks, the relation is ''coexists_with'' and they will persist as live alternatives held by different constituencies. This affects the trajectory prediction: forecloses implies eventual institutional victory for one reading; coexists_with implies persistent institutional contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the abolitionist and property readings logically foreclose each other or occupy different frameworks.').

omega_variable(
    consumer_complicity_boundary,
    'Are consumers of animal products themselves targets of the constraint (victims who are forced to participate) or beneficiaries (who derive advantage from the low cost and cultural normalization of animal use)?',
    'Structural analysis: Can consumers easily exit animal-product markets without economic hardship or social penalty? If yes, they are beneficiaries (mobile exit). If no, they are partly victims of the constraint (trapped/constrained by economic or cultural pressure to participate). The answer likely differs by economic class and geography.',
    'If consumers are classified as beneficiaries, the constraint''s beneficiary set is vastly larger (billions) and the extraction is distributed broadly rather than concentrated—changing the class structure of the constraint. If consumers are classified as partly-victimized participants forced into complicity, the beneficiary set narrows to the producer/industry seats and the extraction is more concentrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_complicity_boundary, empirical, 'Whether consumers are beneficiaries or coerced participants in animal instrumentalization.').

omega_variable(
    identity_lock_mechanism__abolitionist_advocates,
    'For abolitionist advocates: is their constraint-exit prevented by external economic barriers (trapped), by fused professional or ethical identity (identity_locked), or by both? Do they experience the constraint as something done TO them or something they are structurally complicit in maintaining?',
    'Ethnographic and interview data with abolitionist advocates: do they report that they could materially exit the constraint''s frame by adopting welfare-reformist or property readings, but refuse on principle? Or do they report that their identity (activist, ethicist, vegan, animal-liberation advocate) is so fused with rejection of the constraint that exit feels impossible?',
    'If identity_locked, abolitionist advocates experience the constraint differently from trapped animals (they have a choice point but cannot bring themselves to take it); if trapped, they have no choice point. The exit_options value for advocates would be ''identity_locked'' vs. ''constrained'' accordingly, affecting directionality calculation for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism__abolitionist_advocates, empirical, 'Whether abolitionist advocates'' constraint-exit is identity-locked or economically trapped.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anim_tr_t5, animal_status__abolitionist_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(anim_tr_t15, animal_status__abolitionist_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(anim_tr_t25, animal_status__abolitionist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(anim_tr_t35, animal_status__abolitionist_reading, theater_ratio, 35, 0.425).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t5, animal_status__abolitionist_reading, base_extractiveness, 5, 0.89).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.91).
narrative_ontology:measurement(anim_be_t15, animal_status__abolitionist_reading, base_extractiveness, 15, 0.92).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(anim_be_t25, animal_status__abolitionist_reading, base_extractiveness, 25, 0.94).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.945).
narrative_ontology:measurement(anim_be_t35, animal_status__abolitionist_reading, base_extractiveness, 35, 0.94).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(anim_su_t5, animal_status__abolitionist_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(anim_su_t15, animal_status__abolitionist_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(anim_su_t25, animal_status__abolitionist_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.875).
narrative_ontology:measurement(anim_su_t35, animal_status__abolitionist_reading, suppression_requirement, 35, 0.88).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.05).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraint stories: abolitionist_reading (this story, animals as rights-holders, ε ≈ 0.95), welfare_reading (animals as sentient beings with constrained interests, ε ≈ 0.65), and property_reading (animals as legal objects, ε ≈ 0.08). The ε values differ by a wide margin because the three readings have fundamentally different victim sets, beneficiary structures, and assessments of what instrumental use extracts. The abolitionist reading sees total extraction (life, autonomy, bodily integrity); the welfare reading sees partial extraction (suffering, but use is permissible if constrained); the property reading sees no extraction (animals have no interests to extract from). These are not the same constraint viewed from different angles—they are three incompatible claims about the standing arrangement's nature, each with its own structural data and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
