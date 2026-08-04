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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Instrumental Use of Animals as Property (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   Under the abolitionist reading, the entire apparatus of animal use —
 *   agriculture, biomedical research, entertainment, fur — constitutes a
 *   single extractive arrangement resting on the legal fiction that animals
 *   are property rather than rights-holders. Welfare regulation, far from
 *   mitigating this extraction, is read as the mechanism that stabilizes it:
 *   by regulating the manner of use, welfare law forecloses the question of
 *   whether use itself is legitimate. The reading treats consumer-facing
 *   'humane' certifications and improved husbandry standards as theater —
 *   real in their effects on individual animals' proximate conditions, but
 *   functioning primarily to manufacture the social license that permits the
 *   extractive arrangement to continue and expand.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bears full extraction under any use category
 *   - animal_agriculture_industry: primary beneficiary and agenda_setter (institutional/arbitrage) — captures economic value, shapes legal and welfare framework
 *   - regulatory_agencies: secondary institutional actor (institutional/analytical) — administers the welfare-regulatory apparatus this reading identifies as legitimating
 *   - animal_welfare_organizations: excluded from this reading's own coalition despite nominal alignment — treated as complicit legitimators
 *   - consumers_of_animal_products: diffuse beneficiary with high theoretical exit capacity but low realized exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.72).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Instrumental Use of Animals as Property (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '48ed722f-eaba-49a4-84a9-769ba9c6c51b').
narrative_ontology:cs_kernel_codification('48ed722f-eaba-49a4-84a9-769ba9c6c51b', distributed).
narrative_ontology:cs_authority_grounding('48ed722f-eaba-49a4-84a9-769ba9c6c51b', distributed).
narrative_ontology:cs_reading_relation('48ed722f-eaba-49a4-84a9-769ba9c6c51b', animal_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('48ed722f-eaba-49a4-84a9-769ba9c6c51b', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('48ed722f-eaba-49a4-84a9-769ba9c6c51b', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('48ed722f-eaba-49a4-84a9-769ba9c6c51b', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('48ed722f-eaba-49a4-84a9-769ba9c6c51b', secondary, welfare_regulation_is_legitimation_not_remedy).
narrative_ontology:cs_axiom_status(welfare_regulation_is_legitimation_not_remedy, holdable).
narrative_ontology:cs_axiom_grounding('48ed722f-eaba-49a4-84a9-769ba9c6c51b', welfare_regulation_is_legitimation_not_remedy, conventional).
narrative_ontology:cs_reference_frame('48ed722f-eaba-49a4-84a9-769ba9c6c51b', pre_legal_natural_rights_of_sentient_beings).
narrative_ontology:cs_drift_state('48ed722f-eaba-49a4-84a9-769ba9c6c51b', contemporary_welfare_regulatory_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('48ed722f-eaba-49a4-84a9-769ba9c6c51b', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_and_exhibition_operators).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, fur_and_leather_producers).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, captive_wild_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, fur_bearing_animals).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, property_status_of_animals_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, human_exceptionalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and killed for food, fiber, and byproducts under legal property status. Under this reading they are full rights-bearers whose interest in continued existence and bodily autonomy is categorically overridden by the property framework; they have no legal standing to contest their use and no exit from the system that produces and disposes of them.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Used in research and product testing under institutional review frameworks that weigh their suffering against human benefit rather than treating it as inviolable. From this reading, any such weighing is illegitimate because it presupposes the animal's interests are already subordinate to human purposes.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Held in zoos, aquaria, and entertainment venues for human observation and profit. Confinement itself is read as a rights violation regardless of enclosure quality or accreditation standards, since the underlying premise — that a sentient being can be lawfully held for display — is what is contested.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, captive_wild_animals, payer,
    powerless, biographical, trapped, global).

% Raised or trapped for pelts. Under this reading there is no welfare threshold that legitimizes the practice; the killing itself, not the conditions preceding it, is the rights violation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_bearing_animals, payer,
    powerless, biographical, trapped, global).

% Sets production standards, lobbies for the legal property status of livestock, and funds welfare-improvement initiatives that this reading treats as legitimation rather than reform. Captures the economic value of animal bodies and has structural power to redefine the terms of the debate (e.g., 'humane certification') without conceding the underlying property claim.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary).

% Depend on animal models for regulatory approval pathways and basic research funding. Administer internal ethics review (IACUC-style bodies) that operationalizes the welfare framework this reading rejects as insufficient; institutional survival is partly contingent on continued access to animal subjects.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, biomedical_research_institutions, agenda_setter).

% Profit from ticketed access to captive animals. Frame accreditation and enclosure standards as sufficient ethical cover; under this reading no standard of confinement quality can cure the underlying rights violation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, entertainment_and_exhibition_operators, beneficiary,
    organized, biographical, constrained, national).

% Convert animal bodies into commercial goods. Increasingly marginalized by shifting consumer sentiment but structurally protected by property law that this reading identifies as the actual mechanism of harm.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_and_leather_producers, beneficiary,
    organized, biographical, constrained, global).

% Purchase and consume products of animal use at prices that do not reflect the rights violation this reading identifies. Have the most exit capacity of any beneficiary group — plant-based and cruelty-free alternatives are increasingly available — but face social, economic, and habitual friction against exiting.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).

% Advocate for improved conditions within the existing use framework — better enclosures, slaughter methods, transport rules. This reading treats their advocacy as structurally complicit: welfare reform is read as manufacturing consent for continued use rather than reducing harm, so welfare organizations are excluded from the abolitionist coalition's own account of legitimate reform even though they are nominally on the animals' side.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_welfare_organizations, excluded,
    organized, generational, constrained, national).

% Write and enforce welfare statutes (housing density, slaughter methods, research protocols) that presuppose the legitimacy of use and regulate only its manner. Under this reading, the existence of a welfare regulatory apparatus is itself the mechanism that forecloses the rights claim from being heard in law.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, from this reading's own lights: the arrangement does not solve a genuine collective-action problem for the animals it governs. It solves a coordination problem among human users of animals (supply chains, research protocols, entertainment licensing) at the animals' categorical expense — coordination among beneficiaries, not with the governed.
% TRANSFER_FUNCTION: Moves bodily autonomy, life, and the capacity to pursue their own interests from animals to human economic, dietary, scientific, and recreational purposes, formalized through property law and operationalized through welfare regulation that regulates manner of use rather than prohibiting use itself.
% ABSENT_VOICES: The governed parties (farmed, laboratory, captive, and fur-bearing animals) have no legal standing to object and are structurally incapable of representing themselves in the fora that set the terms of their use; animal welfare organizations that could partially voice their interests are, on this reading, coopted into the legitimation apparatus rather than genuinely absent, and grassroots abolitionist advocates are marginalized within welfare-dominated policy conversations.
% DISAPPEARANCE_RATIONALE: If the property-status framework for animals disappeared overnight, entire industries (animal agriculture, fur, much of biomedical research, exhibition-based entertainment) would have to reorganize around a rights-respecting alternative or cease; supply chains, research methodologies, and consumption patterns worldwide would be forced to restructure. The scale of rearrangement is itself evidence, on this reading, of how much economic value currently rides on treating the rights question as settled.
% FOUNDING_PROBLEM: The property framework was built to resolve competing human claims over animal bodies as economic and scientific resources — establishing clear ownership, liability, and transactional rules for livestock, research subjects, and captive wildlife in a growing market economy.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and philosophers of animal law (outside the beneficiary industries) corroborate that property-status doctrine was constructed specifically to enable secure commercial transactions in animal bodies, and continues to function exactly that way — courts, contracts, and insurance regimes still treat animals as fungible property. No corroboration for the founding problem's continued moral legitimacy comes from outside the beneficiary industries themselves; welfare organizations, while critical of specific practices, do not corroborate the abolitionist claim that the property framework itself (rather than its manner of exercise) is the live problem.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near-maximal (0.91) because this reading's core premise is that ANY instrumental use of a rights-bearing being extracts value that cannot be legitimately extracted — there is no threshold of 'humane' treatment that reduces ε to zero for a use category; welfare improvements shift where harm occurs, not whether extraction occurs. Suppression (0.72) reflects the structural and legal apparatus — property law, agricultural exemptions from anti-cruelty statutes, lack of legal standing for animals — that forecloses the rights claim from being heard in courts or legislatures. Theater ratio is authored as rising over time (0.25 to 0.58) because welfare certification schemes, corporate animal-welfare pledges, and 'ethical' sourcing labels have proliferated substantially over the measured interval while aggregate scale of use has not meaningfully declined — the reading interprets this as increasing performative cover for unchanged extraction. Accessibility collapse is authored low-moderate (0.35) deliberately: unlike a mountain, alternatives (plant-based systems, in-vitro research methods, non-exhibition wildlife conservation) are increasingly available and visible, which is precisely why this reading treats the arrangement as maintained by suppression and enforcement rather than by absence of alternatives. Resistance is authored high (0.78) reflecting the growing and organized abolitionist movement.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals across all four victim categories are declared full targets: powerless, trapped, no legal standing, and — critically for this reading — no directionality override toward beneficiary status is warranted regardless of how well-treated an individual animal is, because the extraction is definitional (use itself), not conditional on treatment quality. The agenda_setter/beneficiary institutional seats (animal agriculture, biomedical research) sit at the beneficiary end with strong exit/arbitrage capacity — they can relocate operations, rebrand, or lobby to preserve the property framework. Consumers are the most structurally mobile beneficiary group (real plant-based and alternative options exist) but face habitual and social friction, which the reading does not treat as removing their beneficiary status. Animal welfare organizations occupy a genuinely ambiguous seat: nominally aligned with animal interests but structurally positioned, on this reading, as legitimators of the very framework the abolitionist position rejects — hence excluded rather than allied.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists a mandatrophy misreading in a specific direction: because welfare reforms are visibly ongoing and improving (better enclosures, slaughter methods, research oversight), a surface reading might conclude the founding problem (unregulated, unlimited animal use) has been substantially resolved and the current arrangement is legitimate coordination. The abolitionist reading rejects this: it holds that the founding problem was never 'how to use animals humanely' but 'whether animals may be used as property at all,' and that welfare reform answers a different, narrower question than the one this reading holds open. The founding_problem_status is authored as live specifically to block the inference that visible welfare progress equals resolution of the rights question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_animal_status,
    'This story instantiates the abolitionist reading of the animal_status kernel (animals as rights-holders whose inherent value precludes all instrumental use). The welfare_reading (interests constrain but do not prohibit use) and the property_reading (animals as legal objects without independent moral standing) are sibling constraints, not represented in this story''s ε or stakeholder structure. Which reading should govern actual legal and policy practice?',
    'This is not empirically resolvable — it is a foundational disagreement about the grounds of moral status (sentience-sufficiency vs. rights-sufficiency vs. property-sufficiency) that different legal and philosophical traditions answer differently. Resolution would require either a jurisdiction-level legal settlement (e.g., a constitutional rights-holder designation for animals) or a convergence in applied ethics that this framework cannot adjudicate.',
    'If the property_reading governs, this constraint''s entire victim set (farmed_animals, laboratory_animals, etc.) is structurally ineligible to be victims at all — there is no rights violation because there is no rights-holder. If the welfare_reading governs, extraction drops sharply for well-regulated use categories and the constraint restructures toward tangled_rope (genuine welfare coordination plus residual extraction) rather than snare. The three readings produce materially different classifications from the same underlying practices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_animal_status, preference, 'Which of the three animal_status kernel readings should govern practice and law.').

omega_variable(
    welfare_reform_as_legitimation_or_progress,
    'Are welfare reforms (improved housing density standards, pre-slaughter stunning requirements, IACUC review, habitat enrichment) genuine harm-reduction that should be credited as such, or do they function primarily to legitimate continued use and forestall abolition?',
    'Track whether welfare reform historically correlates with reduced aggregate scale of use (a genuine-progress signature) or with increased scale of use accompanied by improved unit conditions (a legitimation signature). Longitudinal data on per-capita meat consumption, total animals used in research, and total captive wildlife populations against welfare-regulation timelines would be probative.',
    'If reforms correlate with declining aggregate use, the abolitionist reading''s theater_ratio assessment is overstated and the arrangement is more plausibly a tangled_rope in transition. If reforms correlate with stable or increasing aggregate use alongside improved unit conditions, the abolitionist reading''s legitimation thesis is corroborated and theater_ratio should be read as understated if anything.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_progress, empirical, 'Whether welfare reform functions as harm reduction or as legitimating cover for expanding use.').

omega_variable(
    moral_status_grounding_ambiguity,
    'Does inherent value/rights-holder status attach to all sentient animals uniformly, or does it vary by cognitive complexity, capacity for suffering, or other morally relevant criteria — and if it varies, does the abolitionist framework''s flat victim-set (treating all instrumental use of all animals as equivalent extraction) misrepresent the moral landscape?',
    'This is a conceptual question about the grounds of moral status that empirical comparative cognition research can inform but not settle — it depends on which theory of moral status (sentientism, rights-based, capacities-based) is adopted as foundational.',
    'If moral status is gradient rather than binary, the flat treatment of all victim categories (insects to primates) at uniform extractiveness may overstate extraction for lower-sentience categories and understate it for high-cognition categories, suggesting this single-ε story should itself decompose further along a sentience/capacity axis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_grounding_ambiguity, conceptual, 'Whether moral status is uniform across animal victim categories or requires further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_status__abolitionist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(anim_tr_t16, animal_status__abolitionist_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(anim_tr_t24, animal_status__abolitionist_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(anim_tr_t32, animal_status__abolitionist_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t8, animal_status__abolitionist_reading, base_extractiveness, 8, 0.89).
narrative_ontology:measurement(anim_be_t16, animal_status__abolitionist_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement(anim_be_t24, animal_status__abolitionist_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(anim_be_t32, animal_status__abolitionist_reading, base_extractiveness, 32, 0.91).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(anim_su_t8, animal_status__abolitionist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(anim_su_t16, animal_status__abolitionist_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(anim_su_t24, animal_status__abolitionist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(anim_su_t32, animal_status__abolitionist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the legal and moral status of animals' per the ε-invariance principle: measuring animal use through the abolitionist lens yields near-maximal extraction with welfare reform read as legitimation theater; measuring through the welfare lens yields moderate, conditional extraction with welfare reform read as genuine constraint; measuring through the property lens yields near-zero extraction because there is no independent moral standing to violate. These are not the same constraint viewed three ways — they have different victim sets, different beneficiary structures, and different classifications (snare vs. tangled_rope vs. rope/mountain-adjacent). All three link to each other via affects_constraints; each carries its own ε, stakeholders, and six_questions answers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
