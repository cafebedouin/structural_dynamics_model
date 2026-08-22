% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Animal Instrumental-Use Regime (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story authors the abolitionist reading of the animal-status kernel:
 *   animals are rights-holders whose inherent value precludes instrumental
 *   use categorically, not merely use conducted under inadequate conditions.
 *   Under this reading, the standing arrangement — industrial agriculture,
 *   biomedical research use, fur production, and captive exhibition — is
 *   assessed as it is, by the abolitionist's own lights: as a use-regime that
 *   extracts life, labor, and suffering-capacity from animals with zero moral
 *   warrant for any of it, welfare improvements included. The referent of
 *   extractiveness is this standing arrangement, not the rights-respecting
 *   alternative the reading endorses (which would properly register as zero
 *   extraction in its own separate story, not here).
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: primary beneficiary and agenda-setter (institutional/arbitrage) — captures the commodity value of animal bodies
 *   - biomedical_research_industry: beneficiary and co-agenda-setter (institutional/constrained) — captures scientific and regulatory value from animal-model use
 *   - farmed_animals, laboratory_animals, fur_bearing_animals, captive_exhibition_animals: primary victims (powerless/trapped) — bear the entire cost of the arrangement with no legal capacity to resist
 *   - welfare_reform_organizations: excluded from the rights conversation under this reading — their negotiated standards are read as legitimation rather than genuine interest-representation
 *   - regulatory_agencies: agenda-setters whose statutes assume permissibility of use as a starting axiom, thereby converting use into a certified, legally stable practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.86).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental-Use Regime (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c').
narrative_ontology:cs_kernel_codification('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', distributed).
narrative_ontology:cs_authority_grounding('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', distributed).
narrative_ontology:cs_reading_relation('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', secondary, welfare_reform_constitutes_legitimation_not_remedy).
narrative_ontology:cs_axiom_status(welfare_reform_constitutes_legitimation_not_remedy, holdable).
narrative_ontology:cs_axiom_grounding('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', welfare_reform_constitutes_legitimation_not_remedy, deontological).
narrative_ontology:cs_reference_frame('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', pre_legal_personhood_use_default).
narrative_ontology:cs_drift_state('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', contemporary_animal_rights_movement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('a1564a7f-4e7c-4ca6-b54e-cadb5c40dc6c', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, fur_and_leather_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_and_exhibition_industry).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, fur_bearing_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, captive_exhibition_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, welfare_reform_organizations).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets breeding, confinement, and slaughter practices at industrial scale, lobbies against rights-based legal reform, and funds welfare-standard bodies that certify continued use as ethically adequate. Captures the entire value of animal bodies as commodities and treats reform pressure as a market-access risk to be managed rather than a claim to be conceded.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary).

% Uses animal subjects in testing protocols justified by regulatory requirements and scientific necessity claims, sets institutional review standards that presuppose permissibility of use, and funds research into alternatives only marginally, since full replacement would eliminate an established methodological and funding infrastructure.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_industry, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, biomedical_research_industry, agenda_setter).

% Extracts skins and pelts as a co-product or primary product of confinement and slaughter, marketing use as tradition or luxury; can relocate production to jurisdictions with weaker restrictions when a market imposes bans.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_and_leather_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Displays and performs animals for paying audiences, framing captivity as education or conservation; can rebrand or relocate operations if local regulation tightens, without altering the underlying confinement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, entertainment_and_exhibition_industry, beneficiary,
    powerful, biographical, mobile, national).

% Bred, confined, and killed for food and byproducts on a schedule set entirely by producers; have no legal standing to resist any aspect of the arrangement and no capacity to exit the confinement structures that determine their entire lived experience.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Subjected to procedures ranging from mild to lethal under institutional review that weighs their interests against research value but never treats those interests as excluding use altogether; cannot refuse, cannot leave, and are killed or discarded at study end regardless of outcome.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Confined in production systems whose sole purpose is pelt yield; welfare improvements, where present, adjust confinement conditions without altering that the animal exists to be killed for a non-necessary product.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_bearing_animals, payer,
    powerless, immediate, trapped, global).

% Held in enclosures for viewing and performance across a lifespan they did not choose; enrichment programs improve conditions inside captivity but do not question the legitimacy of holding them at all.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, captive_exhibition_animals, payer,
    powerless, biographical, trapped, national).

% Negotiate incremental confinement and slaughter standards with industry and government; from this reading's standpoint they are excluded from the rights conversation because their bargaining position accepts the legitimacy of use as a starting premise, and their reforms are read as extending the system's social license rather than dismantling it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, welfare_reform_organizations, beneficiary).

% Argue for rights-based legal personhood or use-prohibition rather than welfare regulation; largely shut out of legislative drafting processes that treat welfare-reform organizations as the legitimate animal-interest stakeholder, leaving the rights claim itself absent from most policy venues.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocacy_groups, excluded,
    moderate, generational, constrained, global).

% Purchase food, clothing, and entertainment access built on animal use; face essentially no barrier to substitution for most products, making their participation a matter of preference and habituation rather than necessity.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).

% Draft and enforce welfare statutes, husbandry codes, and research-oversight rules that regulate the manner of use but assume its permissibility as a starting axiom; certify industry compliance and thereby confer legal legitimacy on the underlying use structure.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The instrumental-use regime coordinates food production, biomedical research infrastructure, and consumer goods supply chains around a shared assumption that animal bodies and labor are available for human appropriation, allowing large-scale planning, investment, and regulation to proceed on a stable legal footing.
% TRANSFER_FUNCTION: Moves biological material, labor, and suffering-capacity from animals to producers, researchers, and consumers, converting confinement, use, and killing into food, tested products, garments, entertainment revenue, and scientific data — with no reciprocal flow returning anything to the animals themselves.
% ABSENT_VOICES: The animals who are killed or confined have no forum in which their interest in not being used at all could be represented; abolitionist advocacy organizations that would press exactly this claim are structurally excluded from standard-setting processes in favor of welfare organizations whose participation presupposes the legitimacy of use.
% DISAPPEARANCE_RATIONALE: If the instrumental-use regime were dissolved overnight, entire industries (industrial animal agriculture, animal-model biomedical testing, fur, captive exhibition) would cease to exist in their current form; supply chains, employment, research methodology, and consumer markets would all have to reorganize around non-animal alternatives. This is precisely why, from the abolitionist reading, the arrangement cannot be a natural fact — its disappearance would visibly rearrange large sectors of the economy, which is a sign of a constructed extraction structure rather than an unavoidable feature of the world.
% FOUNDING_PROBLEM: Historically framed as solving humanity's need for food, clothing, labor, medical knowledge, and entertainment by treating animals as available resources — a problem posed at a time when animals were not conceived as possible rights-holders at all.
% FOUNDING_PROBLEM_CORROBORATION: Industry and regulatory bodies attest the founding problem (need for food, medicine, materials) remains live and justifies continued use, citing nutritional and public-health necessity. Independent of the benefiting industries, philosophers working in animal-rights theory and some biomedical-ethics researchers attest that non-animal alternatives (plant-based and cultivated foods, in-vitro and computational models, synthetic materials) now address most of the originally-claimed necessity, making the 'necessity' framing of the founding problem largely obsolete rather than live — though this corroboration comes from a contesting rather than a neutral third party, since no fully disinterested outside verifier exists on a question this politically charged.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.91, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-maximal (0.91) because, under this reading, there is no category of instrumental use — however humane the conditions — that is not itself the extraction; welfare improvements do not reduce ε, they merely improve the terms under which extraction proceeds. Suppression is high (0.86) because the arrangement depends on animals having no legal standing to contest use, and on excluding rights-based advocacy from standard-setting venues where welfare organizations are treated as the legitimate animal-interest party. Theater ratio rises over the interval (0.30 -> 0.62) because welfare certifications, enrichment programs, and 'humane' labeling schemes have proliferated as the primary public-facing response to rights-based critique, while the underlying confinement-and-killing structure is unchanged — from the abolitionist reading, this is textbook metric substitution: the proxy goal (welfare compliance) displacing the real question (whether use is permissible at all). Accessibility collapse is authored moderate-low (0.35) rather than high, because plant-based, synthetic, and in-vitro alternatives to most use-categories now exist and are growing, meaning the instrumental-use regime's alternatives have NOT collapsed — the regime persists via active enforcement and market incumbency, not because no alternative exists. This is precisely why the claimed type is snare rather than mountain: a mountain would show high accessibility collapse and low resistance; this arrangement shows real, organized resistance (0.78) and workable alternatives, sustained instead by industry lobbying, regulatory capture, and consumer habituation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (industry, regulators) the arrangement reads as legitimate, welfare-regulated commerce serving genuine human needs. From the payer seats (the animals themselves, as represented by this reading) the identical structure is total, uncompensated appropriation of life and labor with no exit and no standing to object. The engine should compute these seats as structurally divergent — this divergence is exactly what the abolitionist reading asserts is being obscured by welfare framing, which offers itself as a mediating middle position that in fact administers only the beneficiary side of the ledger.
 *
 * DIRECTIONALITY LOGIC:
 *   Industry beneficiaries sit near the full-beneficiary end of directionality: they set terms, capture commodity and research value, and hold arbitrage-grade exit (relocating production, rebranding, adjusting supply chains) if any single jurisdiction restricts use. The animal victim groups sit at the full-target end: trapped exit options, no legal personhood, immediate time horizon (their entire relevant 'future' is the confinement period before use or slaughter). Consumers are authored as beneficiaries with mobile exit (substitution is generally available and low-cost), which is why their directionality is not treated as symmetric with the animals' — their benefit is optional and substitutable, unlike the animals' cost, which is total and non-negotiable from within the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare (not tangled_rope) reflects that, under this reading, there is no genuine coordination function being served for the party paying the cost — the animals gain nothing from the arrangement that offsets what they lose, so the tangled_rope's requirement of a real coordination benefit reaching the payer side is not met. This forecloses reading the welfare-regulated status quo as a legitimate hybrid needing only rebalancing; the abolitionist reading holds that no amount of welfare improvement converts extraction into coordination, because the coordination story (feeding/researching/clothing/entertaining humans) does not require using animals at all once alternatives exist — it only requires it given sunk industry investment and consumer habit, which is precisely the inertial-plus-enforcement profile of extraction dressed as necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_ambiguity,
    'Do animals possess inherent rights-holding status that categorically precludes instrumental use, or only interests that can be weighed against and outbalanced by human benefit?',
    'This is not resolvable by empirical data alone; it depends on which theory of moral status (rights-based, interest-based, or property-based) is adopted. Partial empirical inputs (evidence of animal sentience, cognitive complexity, capacity for suffering) narrow but do not close the normative gap.',
    'If rights-based status is correct, this reading''s near-maximal extractiveness and snare classification are the accurate description of the standing arrangement. If only interest-based status is correct, the welfare_reading''s much lower extractiveness is the accurate one, and this story''s ε would be an overstatement of what the arrangement actually does wrong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_ambiguity, conceptual, 'Whether animals hold rights (this reading) or merely interests (welfare reading) or neither (property reading) is the kernel-level dispute this story is one reading of.').

omega_variable(
    welfare_reform_as_legitimation_or_progress,
    'Do welfare reforms (improved confinement standards, enrichment programs, humane slaughter certification) function as genuine harm-reduction that should be credited, or as legitimating theater that entrenches the underlying use-structure by giving it a humane veneer?',
    'Longitudinal tracking of whether welfare-reform jurisdictions show declining or stable total animal use over time, and whether reform campaigns correlate with reduced or increased consumer demand for animal products.',
    'If reforms measurably reduce total use over time, the rising theater_ratio in this story may overstate the case; if reforms correlate with stable or rising use alongside improved public perception, the theater_ratio trajectory is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_progress, empirical, 'Whether welfare reform is genuine progress or a legitimating theater increasing consumer comfort with unchanged underlying extraction.').

omega_variable(
    necessity_of_use_given_alternatives,
    'Given the growing availability of plant-based, cultivated, and synthetic alternatives, is animal use still ''necessary'' for food, medicine, and materials in any sense that would justify treating accessibility_collapse as high rather than moderate?',
    'Comparative cost, safety, and efficacy analysis of animal-derived versus alternative products across food, biomedical, and materials categories, tracked over time as alternative technology matures.',
    'If alternatives are found broadly viable and cost-competitive, accessibility_collapse should be authored lower still, strengthening the snare classification (real alternatives are being actively suppressed by incumbency, not absent). If alternatives remain substantially inferior for key use-categories (e.g. certain biomedical research), the case for at least partial coordination function in those categories is stronger, pulling toward tangled_rope for that subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_use_given_alternatives, empirical, 'Whether the maturity of animal-use alternatives supports treating the regime as sustained purely by incumbency rather than genuine necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(anim_tr_t8, animal_status__abolitionist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(anim_tr_t16, animal_status__abolitionist_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(anim_tr_t24, animal_status__abolitionist_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement(anim_tr_t32, animal_status__abolitionist_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t8, animal_status__abolitionist_reading, base_extractiveness, 8, 0.89).
narrative_ontology:measurement(anim_be_t16, animal_status__abolitionist_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement(anim_be_t24, animal_status__abolitionist_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(anim_be_t32, animal_status__abolitionist_reading, base_extractiveness, 32, 0.91).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(anim_su_t8, animal_status__abolitionist_reading, suppression_requirement, 8, 0.81).
narrative_ontology:measurement(anim_su_t16, animal_status__abolitionist_reading, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(anim_su_t24, animal_status__abolitionist_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(anim_su_t32, animal_status__abolitionist_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'animal status' kernel per the ε-invariance principle: abolitionist_reading (this file, ε≈0.91, snare), welfare_reading (ε moderate, tangled_rope expected — genuine welfare-improvement coordination alongside continued extraction), and property_reading (ε near-zero, rope or mountain-adjacent — no rights violation recognized under this framing). All three share the identical standing arrangement as their descriptive object but diverge entirely in authored ε because they differ on the underlying moral-status premise. Each should link to the other two via affects_constraints; none should be read as measuring a different arrangement, only as differently-premised readings of the same one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
