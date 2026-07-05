% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Property Reading of Animal Moral Status
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   The property reading treats animals as legally and morally equivalent to
 *   other forms of ownable resource: land, tools, inventory. Their interests,
 *   however real biologically, generate no independent normative claim
 *   against use; the only constraint the reading recognizes is against
 *   economically wasteful destruction of value, not against harm to the
 *   animal as such. This is one of three structurally distinct readings of
 *   the animal-moral-status kernel (property_reading here; welfare_reading
 *   and abolitionist_reading are separate constraint stories). The property
 *   reading claims mountain status — it presents itself as following
 *   analytically from the definition of property and the historical absence
 *   of animal legal personhood, not as a constructed policy choice. Because
 *   it declares concentrated beneficiaries (industries and owners who profit
 *   from the reading's persistence), it is an explicit False Summit Mountain
 *   (FSM) candidate: the omega variables below document the
 *   natural-law-vs-constructed ambiguity the schema requires.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.12).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.28).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Property Reading of Animal Moral Status").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'ae59a787-327c-444e-a522-28fb4f180e03').
narrative_ontology:cs_kernel_codification('ae59a787-327c-444e-a522-28fb4f180e03', formalized).
narrative_ontology:cs_authority_grounding('ae59a787-327c-444e-a522-28fb4f180e03', lineage).
narrative_ontology:cs_interpretation_layer_present('ae59a787-327c-444e-a522-28fb4f180e03').
narrative_ontology:cs_reading_relation('ae59a787-327c-444e-a522-28fb4f180e03', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae59a787-327c-444e-a522-28fb4f180e03', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('ae59a787-327c-444e-a522-28fb4f180e03', foundational, interests_subordinate_by_definition).
narrative_ontology:cs_axiom_status(interests_subordinate_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('ae59a787-327c-444e-a522-28fb4f180e03', interests_subordinate_by_definition, conventional).
narrative_ontology:cs_axiom('ae59a787-327c-444e-a522-28fb4f180e03', secondary, waste_not_harm_is_the_only_cognizable_limit).
narrative_ontology:cs_axiom_status(waste_not_harm_is_the_only_cognizable_limit, holdable).
narrative_ontology:cs_axiom_grounding('ae59a787-327c-444e-a522-28fb4f180e03', waste_not_harm_is_the_only_cognizable_limit, instrumental).
narrative_ontology:cs_reference_frame('ae59a787-327c-444e-a522-28fb4f180e03', common_law_chattel_doctrine).
narrative_ontology:cs_drift_state('ae59a787-327c-444e-a522-28fb4f180e03', contemporary_animal_law_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ae59a787-327c-444e-a522-28fb4f180e03', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_agriculture_supply_chains).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners_general).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, instrumental_value_of_nonhuman_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own, breed, confine, and slaughter animals as capital and inventory. The property reading is the legal and normative foundation of their entire business model: animals are assets whose interests generate no independent claim against use, only against 'waste' (destroying value without economic purpose). They lobby to keep this reading codified in law.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, livestock_producers, beneficiary,
    organized, generational, arbitrage, national).

% Uses animals as experimental subjects and consumable research materials. Relies on the property reading to justify procedures that would be tortious or criminal if performed on a rights-bearing being. Regulatory oversight (IACUC-type bodies) addresses procedural efficiency and public perception, not animal standing.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, biomedical_research_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Bred, confined, used, and killed according to the economic and procedural interests of their owners. Under this reading they are not a victim class because a victim requires a standing interest recognized as morally weighty on its own terms — the reading defines their interests as subordinate by construction, so no transfer against their will can register as extraction within this framework. They have no voice, no proxy vote, and no legal standing to object.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, farmed_and_laboratory_animals, excluded,
    powerless, immediate, trapped, local).

% Purchase meat, dairy, leather, and tested pharmaceuticals at prices that reflect animals as fungible inputs rather than morally considerable beings. Can exit into alternative consumption patterns at personal cost, but the reading itself is not something any individual consumer's exit destabilizes.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, national).

% Argue that sentience grounds a moral claim independent of ownership status; they are heard in regulatory hearings about cruelty and confinement standards but are structurally barred from challenging the underlying property status itself, since welfare regulation as currently constituted presupposes it.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, national).

% Study how property law, common law precedent, and constitutional doctrine treat animals as chattel, and compare this reading's stability against emerging legal personhood litigation for cetaceans and great apes.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_and_philosophical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, livestock_producers).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, low-transaction-cost basis for exchange, breeding, use, and disposal of animals as economic assets — enabling markets in livestock, labor animals, research subjects, and companion animals to function under ordinary property law rather than requiring a novel legal category.
% TRANSFER_FUNCTION: Moves the entire burden of instrumental use — confinement, labor, bodily appropriation, death — onto animals, with no compensating claim running back to them; economic value flows to owners, users, and consumers.
% ABSENT_VOICES: The animals themselves have no standing to object under this reading by definition; animal welfare advocates and rights theorists are present in adjacent regulatory conversations about cruelty but are excluded from the conversation about the property status itself, since that status is the premise regulators operate within, not a live question for them.
% DISAPPEARANCE_RATIONALE: If the property reading vanished overnight and were not replaced by welfare or abolitionist frameworks, entire industries built on animals-as-assets (agriculture, biomedical research, entertainment, companion breeding) would lose their legal foundation for routine use; contracts, insurance, inheritance, and tax treatment of livestock would require wholesale reconstruction.
% FOUNDING_PROBLEM: Pre-modern and early-modern legal systems needed a coherent way to allocate use, ownership, transfer, and liability for animals within agrarian and early industrial economies — property law was the available category and animals were assimilated into it alongside land and chattel.
% FOUNDING_PROBLEM_CORROBORATION: Property owners and industry bodies attest the framework remains necessary for functioning markets and food security. Independent legal historians and comparative-law scholars outside the beneficiary set note that the founding problem (need for a workable allocation system) has been solved by every reading, including welfare and personhood frameworks used in other jurisdictions for sentient beings, undermining the claim that property status specifically is the only viable solution.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.12) and largely flat over the interval because, WITHIN this reading's own terms, there is no extraction to measure — animals are not a victim class under the framework's own premises, so the reading reports near-mountain metrics by construction. Suppression is moderate (0.28) rather than negligible: enforcement against animal-personhood litigation, anti-cruelty statute carve-outs preserving ordinary use, and legal doctrine actively foreclosing standing for animals as plaintiffs are real suppressive machinery, not passive absence of a problem. Accessibility collapse is high (0.72) because alternative legal categories (personhood, guardianship, trust-based standing) are foreclosed by settled precedent in most jurisdictions once someone tries to argue for them. Resistance (0.35) reflects a growing but still marginal legal and philosophical movement contesting the reading; it is not the near-zero resistance a genuine natural law would show.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners, agricultural industries, and biomedical research users are declared beneficiaries: they collect the economic value the property status enables and bear none of the reading's costs, which is why d sits near the beneficiary end for them. The animals themselves are deliberately NOT listed as victims — per the expected structural delta for this reading, victimhood is not a coherent category the reading's own logic can register, since the reading defines their interests as subordinate by definition rather than as interests that could be wrongly overridden. This is the central move the property reading makes and the central thing the sibling readings (welfare, abolitionist) dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a workable legal allocation mechanism for use of animals in an agrarian economy — is genuinely old and was genuinely a coordination problem at founding. Whether that problem remains live in a world with alternative legal technologies (limited personhood, guardianship-style standing, trust law analogues used for other non-human-but-morally-relevant entities like rivers or corporations) is exactly the mandatrophy question: the property reading may be solving a problem for which alternative, less totalizing solutions now exist, and its persistence may be sustained by the economic interests of beneficiaries rather than the absence of workable alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_natural_or_constructed,
    'Is animal property status a mountain — a definitional/analytic feature of what property and moral standing mean — or a constructed legal-historical artifact maintained because identifiable industries profit from it?',
    'Comparative legal history: examine jurisdictions and eras where alternative categories (sacred-animal law, limited animal personhood, communal stewardship regimes) governed human-animal relations without collapsing into either full property or full personhood; if viable alternatives persisted stably elsewhere, the property reading is not the unique analytic consequence of ''moral standing'' but one contingent legal choice among several.',
    'If constructed, the mountain claim fails and the constraint reclassifies toward tangled_rope or snare via the false_summit_mountain signature, given the declared concentrated beneficiaries (livestock and biomedical industries) who profit from the reading''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_natural_or_constructed, conceptual, 'Whether property-based animal status is a genuine conceptual necessity or an economically sustained convention.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three kernel readings (property, welfare, abolitionist) disagree — is it about a factual matter (animal sentience/interests), a definitional matter (what ''moral standing'' requires), or a policy/values matter (how much weight competing interests should receive)?',
    'Decompose the disagreement: sentience is now widely empirically uncontested across readings (even property-reading proponents rarely deny animal sentience outright); the disagreement is therefore primarily definitional/normative — whether sentience alone is sufficient to ground a moral claim against ownership, which the property reading answers ''no'' by stipulation.',
    'If the disagreement is purely definitional/normative rather than empirical, no future factual discovery about animal cognition will resolve the kernel contest — the three readings will remain coexisting positions rather than converging, which supports authoring reading_relations as coexists_with rather than forecloses for at least the welfare/property pair.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the property/welfare/abolitionist disagreement as normative-definitional rather than empirical.').

omega_variable(
    waste_constraint_as_proto_standing,
    'Does the property reading''s own internal constraint against ''wasteful'' destruction of animal value implicitly smuggle in a trace of independent moral standing, or is it purely an economic-efficiency constraint with no moral content?',
    'Examine cases where anti-waste rules are enforced even when economically neutral to the owner (e.g., gratuitous cruelty statutes that apply regardless of asset value) — if such cases exist and are common, the reading is not as purely instrumental as claimed and traces of welfare_reading logic have already migrated into it.',
    'If waste constraints turn out to be doing implicit welfare work, the property_reading''s claimed purity (ε v_low, no independent standing) is overstated and the reading is already hybridizing toward welfare_reading in practice, even where it remains property_reading in doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_constraint_as_proto_standing, empirical, 'Whether anti-waste/anti-cruelty carve-outs within property law already encode partial moral standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__property_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__property_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__property_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__property_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__property_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__property_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__property_reading, base_extractiveness, 24, 0.115).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__property_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_moral_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.1).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'animal moral status.' property_reading, welfare_reading, and abolitionist_reading share a kernel (the legal/moral status of nonhuman animals relative to human interests) but instantiate structurally distinct claims with different ε, different beneficiary/victim structures, and different classifications: property_reading claims mountain (ε v_low, no victim set, beneficiaries = owners/industries); welfare_reading is expected to claim tangled_rope or rope (moderate ε, regulated-use coordination with some victim recognition); abolitionist_reading is expected to claim snare (high ε, animals as explicit victim class, property status itself as the extraction mechanism). Each carries its own ε and stakeholder set per the ε-invariance principle; they are linked via affects_constraints rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
