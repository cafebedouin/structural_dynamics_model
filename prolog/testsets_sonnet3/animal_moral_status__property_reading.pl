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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Animal-as-Property Doctrine (Property Reading of Animal Moral Status)
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   This story authors the PROPERTY READING of the contested
 *   animal-moral-status kernel: the legal-philosophical position that animals
 *   are resources whose interests, if they exist at all, are subordinate by
 *   definition to human interests, and whose only cognizable legal
 *   protections address waste, disorder, or harm to human property interests
 *   rather than harm to the animal itself. This is one of three sibling
 *   readings of the same kernel (welfare_reading, abolitionist_reading are
 *   separate constraint stories, not part of this one). Per the ε-invariance
 *   principle, this story does not describe or average over the contest — it
 *   authors the property reading's own internally coherent structure, with
 *   its own stable ε, beneficiary set, and claimed type. The claimed type is
 *   mountain because, from within the property reading's own framework,
 *   treating animals as resources is presented as following necessarily from
 *   the definitional structure of property and personhood law, not as a
 *   policy choice requiring ongoing defense.
 *
 * KEY AGENTS:
 *   - livestock_producers: primary beneficiary (organized/arbitrage) — extracts productive and market value from animals as capital
 *   - animal_research_industry: beneficiary (institutional/arbitrage) — uses animals as experimental instruments under the property frame
 *   - pet_breeding_industry: beneficiary/agenda_setter (organized/mobile) — commodifies animals as bred goods
 *   - agricultural_landowners: beneficiary (powerful/arbitrage) — animals as fixtures of land-based capital
 *   - animals_used_in_production_and_research: excluded non-agent — has no legal standing under this reading by construction
 *   - welfare_advocacy_organizations: excluded — objections must be translated into human-interest categories to register
 *   - legal_philosophers_and_courts: analytical observer — administers and describes the doctrine's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.18).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.28).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal-as-Property Doctrine (Property Reading of Animal Moral Status)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '2a439abe-05c0-4d3d-8d73-e07928811daa').
narrative_ontology:cs_kernel_codification('2a439abe-05c0-4d3d-8d73-e07928811daa', formalized).
narrative_ontology:cs_authority_grounding('2a439abe-05c0-4d3d-8d73-e07928811daa', lineage).
narrative_ontology:cs_interpretation_layer_present('2a439abe-05c0-4d3d-8d73-e07928811daa').
narrative_ontology:cs_reading_relation('2a439abe-05c0-4d3d-8d73-e07928811daa', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('2a439abe-05c0-4d3d-8d73-e07928811daa', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('2a439abe-05c0-4d3d-8d73-e07928811daa', foundational, human_interest_priority_by_definition).
narrative_ontology:cs_axiom_status(human_interest_priority_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('2a439abe-05c0-4d3d-8d73-e07928811daa', human_interest_priority_by_definition, conventional).
narrative_ontology:cs_axiom('2a439abe-05c0-4d3d-8d73-e07928811daa', foundational, animal_interests_not_legally_cognizable).
narrative_ontology:cs_axiom_status(animal_interests_not_legally_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('2a439abe-05c0-4d3d-8d73-e07928811daa', animal_interests_not_legally_cognizable, empirically_contingent).
narrative_ontology:cs_reference_frame('2a439abe-05c0-4d3d-8d73-e07928811daa', chattel_property_common_law_baseline).
narrative_ontology:cs_drift_state('2a439abe-05c0-4d3d-8d73-e07928811daa', contemporary_sentience_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a439abe-05c0-4d3d-8d73-e07928811daa', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_research_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, pet_breeding_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, agricultural_landowners).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_interest_priority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own, breed, confine, and slaughter animals as capital and inventory. Under this reading, the only legal constraint on their conduct is waste or inefficiency (e.g. anti-cruelty statutes framed as protecting property value or public order, not animal interests). They can scale, sell, or dispose of animals as they would any other asset, and the reading legitimizes this without requiring justification against the animal's own interests.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, livestock_producers, beneficiary,
    organized, generational, arbitrage, national).

% Uses animals as experimental instruments. The property reading means institutional review addresses procedural humaneness and resource justification (cost, redundancy) rather than any independent claim the animal could assert. Exit from the arrangement would require conceding the animal has standing to object, which the reading forecloses by definition.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_research_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Breeds and sells animals as commodities, including for traits that predictably cause suffering (e.g. brachycephalic breeds). The property frame treats the animal as the produced good; complaints are handled as consumer-protection or contract disputes between humans, never as claims on behalf of the animal.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, pet_breeding_industry, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__property_reading, pet_breeding_industry, agenda_setter).

% Hold animals as fixtures of land-based production. Zoning, tax, and inheritance law treat livestock as chattel alongside equipment and structures, which stabilizes land valuation and credit access predicated on animals-as-assets.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, agricultural_landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Have no legal personhood or standing under this reading; their confinement, use, and death are dispositional questions for their owners, not questions the animal or a representative can raise as a rights claim. Listed as a non-agent entity for narrative completeness — the reading itself denies they are agents with interests the law must weigh.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_used_in_production_and_research, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, animals_used_in_production_and_research).

% Argue for regulated-use standards addressing suffering; under the property reading their claims are legally cognizable only insofar as they map onto waste, nuisance, or public-morals categories — not onto the animal's own interest. They are present in public debate but structurally outside the doctrine's own framework for what counts as a legitimate claim.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, welfare_advocacy_organizations, excluded,
    moderate, biographical, constrained, national).

% Adjudicate disputes using the property frame as the default legal category (chattel law, tort for damage to property, criminal cruelty statutes justified through public-order rationales). They can observe and describe the doctrine's operation without themselves being beneficiaries or payers.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_philosophers_and_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable legal category (property) for allocating rights of use, sale, and disposition over animals, which lets markets, credit, insurance, and liability systems function without having to adjudicate animal interests case by case.
% TRANSFER_FUNCTION: Moves the entire burden of justification onto anyone objecting to a use of an animal; nothing is 'transferred' from animals under this reading because animals are not parties who can hold or be deprived of anything the law recognizes as theirs.
% ABSENT_VOICES: Animals themselves have no voice by construction — the doctrine defines them out of the category of interest-holders. Welfare and abolitionist advocates are present in public discourse but their claims do not register as first-order legal claims under this reading; they must translate their objections into human-interest terms (public morals, waste, cruelty-as-disorder) to gain traction.
% DISAPPEARANCE_RATIONALE: If the property reading of animal moral status were abandoned overnight, entire industries built on animals as tradeable, disposable, usable assets (agriculture, research, breeding, entertainment) would require wholesale legal restructuring — contracts, secured lending against livestock, slaughter and research practices, and insurance regimes all presuppose animals-as-property. The world built on this reading would need a different legal substrate entirely.
% FOUNDING_PROBLEM: Pre-modern and early-modern legal systems needed a stable way to allocate use-rights over animals for food, labor, and transport among competing human claimants (theft, trespass, inheritance disputes) without needing a theory of animal interests at all.
% FOUNDING_PROBLEM_CORROBORATION: Property-law scholars and industry economists attest the allocation problem (who may use, sell, or claim an animal) remains genuinely live and requires some resolving category. Welfare scientists and philosophers of mind, writing from outside the beneficiary industries, attest that the empirical case for animal sentience and interest-bearing capacity has substantially strengthened since the doctrine's founding, which they argue undercuts the doctrine's original silence on animal interests even though the allocation problem itself persists.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) because, BY THE READING'S OWN LIGHTS, there is no extraction occurring at all — animals are not interest-holders from whom anything could be extracted; what looks like extraction from outside the reading (confinement, slaughter, breeding for suffering-prone traits) is, within the reading, simply use of a resource, no different in kind from using timber or ore. The small nonzero value reflects the reading's own internal acknowledgment that some conduct (waste, gratuitous cruelty causing public disorder) is constrained, which a strict reading would treat as approaching zero. Suppression (0.28) reflects that alternative legal categorizations (rights-bearing status) are foreclosed by definition within the doctrine, but this operates through conceptual exclusion rather than active coercive enforcement against animals themselves — there is no one to coerce, structurally, since animals are not parties. Accessibility collapse is high (0.72) because once the property-as-definitional-starting-point is accepted, alternative framings (welfare-based interest-balancing, rights-based standing) become nearly impossible to construct from within the same legal vocabulary without first rejecting the premise. Resistance is moderate (0.35): real and organized (welfare and abolitionist movements, changing sentience science) but historically contained within human-interest categories the doctrine itself defines as the only legitimate channel.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (producers, researchers, breeders, landowners) the arrangement reads as the natural, definitional background condition of using animals at all — not a constraint requiring justification but the absence of one. From the excluded seats (welfare advocates, and by extension the animals themselves as non-agents) the same arrangement reads as a foreclosure of a legitimate moral claim before it can even be raised. The engine's per-seat computation is expected to diverge sharply here: beneficiary seats likely compute as mountain or near-mountain (low d, low derived extraction); the excluded/non-agent seat, were it hypothetically given standing, would compute at the extreme opposite end — this asymmetry is exactly the seat divergence the framework exists to surface, without this story taking a position on which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (all four named groups) are the property reading's structural center of gravity — they set terms, capture value, and face minimal exit cost because the doctrine is the default legal backdrop they operate within, not a constraint they must navigate around. No victims are declared, consistent with the reading's own definitional structure: a reading that denies animals independent moral standing cannot, by its own lights, generate animal victims, since victimhood presupposes an interest capable of being set back. This is the expected structural delta named in the kernel context — animals are deliberately NOT placed in the victim set for this reading, and the non-agent flag on the animals stakeholder entry encodes this formally rather than smuggling standing back in through the stakeholder surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status mismatch is the analytically live element here: the founding problem (allocating use-rights among competing human claimants) remains genuinely live, which argues against treating the doctrine as a pure zombie arrangement. But the corroboration also surfaces that the doctrine's SILENCE on animal interests specifically was never itself required by the allocation problem — it was a byproduct of the era's assumptions about animal cognition, later challenged by sentience science. The property reading does not resolve this itself (it treats the silence as definitional, not as a byproduct up for revision), which is exactly why it is authored as a mountain from its own seat while the sibling readings would compute the same underlying arrangement very differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_vs_constructed_exclusion,
    'Is animals'' exclusion from moral-standing categories a genuine feature of a coherent, non-arbitrary definitional boundary (like the boundary between persons and inanimate objects), or is it a constructed exclusion that tracks historical convenience for the beneficiary industries rather than any principled distinction?',
    'Comparative analysis of the doctrine''s stated criteria for moral standing (rationality, language use, self-awareness) against contemporary comparative cognition research; if animals meet the doctrine''s own stated criteria better than the doctrine assumed at founding, the exclusion looks constructed rather than definitional.',
    'If the exclusion is constructed rather than principled, the property reading''s claimed mountain status is undermined — it would be more accurately understood as a tangled_rope or snare wearing the vocabulary of natural-law necessity, i.e. a false summit. If genuinely principled and stable under scrutiny, the mountain claim is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_vs_constructed_exclusion, conceptual, 'Whether animal exclusion from moral standing is a principled definitional boundary or a beneficiary-serving construction (FSM candidate).').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the property reading''s core premise (interests subordinate by definition) conflict with the welfare reading''s core premise (interests matter but use is permissible)? Is this a difference in ultimate values, or a difference in what each reading treats as the threshold question?',
    'Structural comparison: the welfare reading accepts that animals HAVE interests that must be weighed (even if outweighed by human interests in permitted-use contexts); the property reading denies animals have legally cognizable interests to weigh at all. The disagreement is located at the threshold question of interest-bearing capacity, not at the downstream question of how much weight to give competing interests.',
    'This locates the property/welfare split as being about STANDING (does the animal count as an interest-holder at all) rather than about WEIGHING (how much an animal''s interest counts once granted). A sibling reading that resolved standing in the animal''s favor would collapse into something structurally closer to the welfare reading even while retaining permitted-use conclusions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural disagreement between property and welfare readings at the standing/threshold question, not the weighing question.').

omega_variable(
    property_abolitionist_incompatibility,
    'Does the abolitionist reading''s core premise (property status itself is the violation, regardless of treatment) directly and unavoidably contradict the property reading''s core premise (property status is the legitimate, definitional baseline), such that no single legal framework could hold both simultaneously?',
    'Logical analysis: abolitionist reading asserts that ANY property categorization of a sentient being constitutes a rights violation; property reading asserts that property categorization is the legitimate default absent special justification. These are direct negations of each other at the level of the categorization itself, not merely different weightings of a shared premise.',
    'Supports classifying property_reading and abolitionist_reading as forecloses (rather than coexists_with) in cs_structure.reading_relations — a single legal framework cannot simultaneously treat property status as both the legitimate baseline and the violation itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_abolitionist_incompatibility, conceptual, 'Whether property and abolitionist readings are logically incompatible within any single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__property_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__property_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__property_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__property_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__property_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__property_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__property_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__property_reading, base_extractiveness, 32, 0.175).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_moral_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language concept 'animal moral status' per the ε-invariance principle: property_reading (this story, ε≈0.18, mountain-claimed, no animal victims), welfare_reading (separate story, expected moderate ε, tangled_rope-flavored — cruelty constrained but use permitted, animals partially recognized as interest-holders within a regulated-use structure), and abolitionist_reading (separate story, expected high ε, snare-claimed from its own seat — property status itself treated as the extraction mechanism, animals as the primary victim class). Each story authors its own ε assessed by that reading's own lights, per the fixed-referent rule for kernel-reading stories. The three stories together form the animal_moral_status kernel family and must each link to at least one other family member via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
