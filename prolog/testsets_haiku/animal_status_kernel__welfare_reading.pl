% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Sentience + Regulated Use (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the contested animal
 *   status kernel. The reading holds that animals are sentient beings whose
 *   suffering is morally relevant, and that human use of animals is
 *   permissible if regulated to minimize pain—while retaining animals'
 *   property status but constraining it through welfare obligations. This is
 *   one of three live readings: the property reading (animals as property
 *   with moral standing only as owner interest), the abolitionist reading
 *   (animals as moral persons with the right not to be property), and this
 *   welfare reading (animals sentient and morally considerable, but use
 *   permissible under constraint). The welfare reading generates extractive
 *   operation because it permits continued commodification under the guise of
 *   moral concern, creating a structural asymmetry: the livestock industry
 *   retains use rights while appearing to accommodate moral objections;
 *   consumers gain moral legitimacy without behavioral change; animals'
 *   interests are acknowledged but not controlling. The measurement series
 *   shows extractiveness rising from 0.48 to 0.58 over the interval (0–30)
 *   and plateauing, with theater ratio rising sharply (0.28 to 0.50),
 *   indicating increasing performative rather than substantive welfare focus.
 *
 * KEY AGENTS:
 *   - sentient_animals_in_use (powerless, trapped): bear suffering under regulated-use framework; morally relevant but not controlling
 *   - livestock_industry (institutional, beneficiary/agenda-setter): sets welfare standards and retains use rights; benefits from moral legitimation while continuing extraction
 *   - consumer_base_satisfied_by_regulation (organized, beneficiary): benefits from moral reassurance that permits continued consumption without ethical dissonance
 *   - regulatory_institutions (institutional, agenda-setter): write and enforce welfare standards; gain governance authority and legitimacy without eliminating use
 *   - abolitionist_moral_movement (moderate, excluded): argues property status is categorically incompatible with acknowledged sentience; not represented in regulatory consensus
 *   - animal_liberation_philosophers (analytical, observer): provide the critique that welfare regulations serve as moral licensing, increasing consumption and potential aggregate suffering
 *   - scientific_animal_cognition_community (analytical, observer): establish factual foundation of sentience; remain neutral on normative use question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.58).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.42).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Sentience + Regulated Use (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '177a2d78-e343-453b-9b7c-9be1506c8536').
narrative_ontology:cs_kernel_codification('177a2d78-e343-453b-9b7c-9be1506c8536', distributed).
narrative_ontology:cs_authority_grounding('177a2d78-e343-453b-9b7c-9be1506c8536', distributed).
narrative_ontology:cs_reading_relation('177a2d78-e343-453b-9b7c-9be1506c8536', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('177a2d78-e343-453b-9b7c-9be1506c8536', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('177a2d78-e343-453b-9b7c-9be1506c8536', foundational, animal_sentience_moral_relevance).
narrative_ontology:cs_axiom_status(animal_sentience_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('177a2d78-e343-453b-9b7c-9be1506c8536', animal_sentience_moral_relevance, empirically_contingent).
narrative_ontology:cs_axiom('177a2d78-e343-453b-9b7c-9be1506c8536', foundational, property_status_constrainable_by_welfare).
narrative_ontology:cs_axiom_status(property_status_constrainable_by_welfare, holdable).
narrative_ontology:cs_axiom_grounding('177a2d78-e343-453b-9b7c-9be1506c8536', property_status_constrainable_by_welfare, deontological).
narrative_ontology:cs_reference_frame('177a2d78-e343-453b-9b7c-9be1506c8536', sentience_acknowledging_use_permitting).
narrative_ontology:cs_drift_state('177a2d78-e343-453b-9b7c-9be1506c8536', contemporary_moral_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('177a2d78-e343-453b-9b7c-9be1506c8536', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumer_base_satisfied_by_regulation).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, regulatory_institutions).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, sentient_animals_in_use).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, animal_sentience_thesis).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, moral_considerability_of_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals subject to human use (livestock, laboratory animals, entertainment animals) experience pain, fear, and confinement. The welfare reading acknowledges their suffering as morally relevant but permits their use if welfare regulations are followed. They have no exit from the use relationship and no voice in the rules governing their treatment. Their capacity to suffer is the property of moral relevance under this reading.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, sentient_animals_in_use, payer,
    powerless, immediate, trapped, global).

% Sets and enforces welfare standards (stall dimensions, transport duration, slaughter methods, anesthesia protocols). Retains property ownership and use rights; animals remain commodities but with constrained use parameters. Benefits from moral legitimation that welfare regulation provides while retaining commercial extraction. Operates under regulatory oversight that imposes compliance costs but permits continuation of animal agriculture at scale.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, livestock_industry, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, livestock_industry, beneficiary).

% Consumers who hold moral concern for animal suffering but accept regulated use. They benefit from the constraint because it permits continued consumption without moral dissonance: welfare labels, certification schemes, and regulatory assurance allow them to participate in animal agriculture while believing harm is minimized. Their exit option is to adopt abolitionist ethics or shift to plant-based alternatives, but the welfare reading's framing makes exit feel unnecessary.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumer_base_satisfied_by_regulation, beneficiary,
    organized, biographical, mobile, global).

% Writes, enforces, and adjudicate welfare regulations. Acts as arbiter of what constitutes adequate welfare; their rulings define the boundary between permissible and impermissible use. They benefit from the constraint because it gives them a governance role and legitimacy as protectors of animal interests without eliminating the underlying use relationship. Their power is constrained by economic and political pressure from the livestock industry.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, regulatory_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Advocates argue that property status itself is the injustice and that welfare reforms legitimize continued exploitation ('new welfarism'). They are excluded from the consensus that welfare regulation adequately addresses moral concern; their position is not represented in regulatory bodies and is actively contested by the industry and welfare-accepting consumers. They maintain that the reading is incoherent: if animals matter morally, property status is incompatible with that concern.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_moral_movement, excluded,
    moderate, generational, mobile, global).

% Intellectual seats that analyze the welfare reading's internal coherence and empirical claims. They provide the abolitionist critique that welfare regulations serve as moral cover for continued exploitation. They document the 'moral licensing' effect: welfare certification increases consumption by making consumers feel their consumption is ethical, potentially increasing aggregate animal suffering despite improved individual conditions.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_liberation_philosophers, observer,
    analytical, generational, analytical, global).

% Empirically documents animal sentience, emotional complexity, and capacity for suffering. Their work grounds the premise that suffering is morally relevant. They remain neutral on the normative question of whether regulated use is acceptable; their role is to provide the factual foundation the reading depends on.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, scientific_animal_cognition_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared moral framework that acknowledges animal sentience while permitting continued human use under welfare constraints. Solves the coordination problem of reconciling moral concern for animal suffering with existing economic dependence on animal agriculture by creating a middle position: animals are morally considerable but not inviolable; their suffering matters but regulated use is permissible if pain is minimized.
% TRANSFER_FUNCTION: Moves regulatory authority to institutions that constrain (but do not eliminate) animal use; imposes welfare compliance costs on the livestock industry; generates moral legitimacy and consumer peace-of-mind for those who wish to consume animal products while believing harm is minimized. The constraint transfers moral standing from property-owner absolutism (the property reading) to a system where animals have interests that constrain but do not eliminate their use.
% ABSENT_VOICES: Abolitionist moral philosophers and animal liberation advocates are excluded from regulatory bodies that write welfare standards; their argument that property status is categorically incompatible with moral consideration is not represented in standard welfare policy. Scientific researchers on animal cognition, while present, are consulted only on factual questions of sentience and capacity for suffering—normative questions about what follows from that capacity are not opened to them.
% DISAPPEARANCE_RATIONALE: If the welfare reading and its regulatory apparatus vanished overnight, the livestock industry would revert to the property reading (animals are property with no constraints beyond owner interest); moral consumers would face an unresolved tension between their stated concern for animal suffering and their participation in animal agriculture. Some would shift to abolitionist ethics and plant-based consumption; others would rationalize consumption under the property reading. The moral consensus that permitted regulated use would collapse, and the market would reorganize under either unrestricted property rights or abolitionist constraints.
% FOUNDING_PROBLEM: Historical animal use was justified by treating animals as property with no moral standing. As scientific evidence accumulated showing animal sentience and suffering capacity, moral concern grew. The founding problem was how to incorporate that moral concern without dismantling existing animal agriculture—a problem that the welfare reading 'solves' by acknowledging suffering's moral relevance while permitting use under constraints.
% FOUNDING_PROBLEM_CORROBORATION: Welfare scientists and regulatory bodies attest that the founding problem is live and the solution is workable: welfare regulations have reduced some suffering and can be improved. Abolitionists attest that the problem is not solved—that property status itself is incompatible with acknowledged sentience, and welfare regulations serve as moral cover that increases rather than decreases aggregate suffering. Independent empirical research on moral licensing effects and consumer behavior from philosophers and psychologists outside the industry supports the abolitionist critique that welfare frames increase consumption ('happy meat' effect).
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because welfare regulations impose real compliance costs on the livestock industry but do not eliminate the extraction relationship—animals remain commodities, use continues at scale, and the core economic structure persists. Suppression is lower than extractiveness (0.42 vs 0.58) because the welfare reading does not require violent suppression of animal resistance (animals cannot form political movements) but does require suppression of abolitionist moral arguments and silencing of the contradiction between acknowledged sentience and continued property status. Theater ratio rises sharply (0.28→0.50) because welfare certification, labeling, and regulatory performance increasingly function as moral reassurance for consumers rather than substantive pain reduction; empirical research on moral licensing shows welfare frames increase consumption. The measurement series is authored on a single shared time grid: extractiveness rises steeply from t=0 to t=30 (period of welfare-regulation expansion and moral legitimation), then plateaus (regulatory framework stabilizes); theater ratio rises throughout as the constraint matures and becomes performatively rather than functionally focused; suppression requirement is stable (abolitionist critique is constant; suppression effort does not increase sharply because the constraint is culturally normalized). This is a tangled rope: the coordination function is genuine (animals' sentience is scientifically established, welfare standards reduce some suffering), but the arrangement is asymmetric (animals' interests are acknowledged but not controlling; continued use extracts value from animals while appearing to protect them) and requires active enforcement (suppressing abolitionist moral arguments, preventing regulatory escape by industry, managing consumer moral licensing).
 *
 * PERSPECTIVAL GAP:
 *   The livestock industry and welfare-satisfied consumers perceive this as genuine coordination with cost: regulations impose real constraints on profitable practices, and the compromise acknowledges animal interests. Sentient animals and abolitionists perceive it as enforced extraction under moral cover: property status remains unchanged, use continues, and welfare regulations serve as moral legitimacy for continued commodification. The regulatory institutions perceive themselves as neutral arbiter protecting animal interests; abolitionists perceive them as captured by industry interests (captured regulators write standards that benefit from continued animal agriculture). The scientific community perceives themselves as providing factual foundation; abolitionists perceive that facts alone are being distorted by a framework that acknowledges suffering but refuses to draw the logical conclusion (that property status is incompatible with moral standing). The engine computes these seat divergences from the structural data—the asymmetry is real and derives from the power differential: powerless animals and moderate-power abolitionists vs. institutional industry and regulatory authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the livestock industry approaches 0.0 (full beneficiary: they set the rules, retain property rights, collect the value of animal commodities, face regulatory costs but retain permission to continue extraction). For sentient animals in use, d approaches 1.0 (full target: they bear the extraction—continued use, confinement, slaughter—and have no exit or voice). For consumer base, d is near 0.5 (symmetric, slightly toward beneficiary at 0.35–0.40: they benefit from moral legitimacy and continued access to products; they bear a diffuse moral cost if welfare frames increase consumption and aggregate suffering; their exit option is high—mobile, can shift to abolitionist ethics or plant-based—so directionality is dampened). For regulatory institutions, d is near 0.25 (mild beneficiary: they gain governance authority and institutional relevance; they have some cost in managing pressure from both industry and abolitionists, but their power is high and their exit options are unconstrained). For abolitionist movement, d is high (0.75–0.85, near target: they bear the cost of exclusion from moral consensus and regulatory voice; their moral framework is actively suppressed; exit is available but costly in social standing). No overrides are needed; the derivation from beneficiary/victim declarations and exit options produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids a false mandatrophy charge because its founding problem (how to incorporate acknowledged animal sentience into existing animal agriculture) is genuinely contested. The constraint does not claim to fully solve the founding problem; the welfare reading explicitly frames the problem as ongoing (welfare standards can always be improved, and the need for constraint is perpetual). However, the theater ratio rising to 0.50 and extractiveness plateauing at 0.58 suggest a drift toward performance: the constraint's justification is increasingly the appearance of moral concern rather than substantive suffering reduction. An omega variable documents whether this is mandatrophy (the original coordination problem—how to protect animals while preserving agriculture—has become impossible and the constraint now persists as theater and extraction), or whether the founding problem remains live and welfare improvement is genuinely possible. If abolitionists are correct that moral licensing increases consumption and aggregate suffering, then the founding problem has been reversed: the constraint increases rather than decreases harm, and the ethical mandate has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_licensing_empirical_question,
    'Do welfare regulations and ''happy meat'' certification increase aggregate animal suffering by increasing consumption volume, thereby potentially causing net harm despite improved individual welfare?',
    'Longitudinal empirical study tracking consumption volume, product mix, and animal population sizes before and after welfare certification expansion. Controlled comparison between markets with and without welfare labeling. Behavioral economics studies on moral licensing effects in animal product consumption.',
    'If moral licensing increases consumption and aggregate suffering exceeds welfare improvements, the constraint becomes mandatrophic: the founding problem (protecting animals while preserving agriculture) reverses into a problem (the constraint increases aggregate harm). The reading''s framing as a genuine compromise collapses, and it becomes a snare with performative welfare cover. If moral licensing is negligible or licensing is offset by voluntary consumption reduction from welfare-aware consumers, the reading''s coordination framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_licensing_empirical_question, empirical, 'Whether welfare certification increases consumption enough to negate welfare improvements.').

omega_variable(
    property_sentience_coherence,
    'Is property status logically compatible with acknowledged moral considerability of sentience and suffering capacity, or does acknowledging sentience entail rejection of property status?',
    'Philosophical analysis of the internal coherence of the welfare reading''s axioms. Test whether denying property status (the abolitionist premise) is logically entailed by accepting sentience''s moral relevance, or whether constraints-on-property-use is a coherent middle position. Examine historical cases where property status has been constrained by moral concerns (human slavery abolition, child labor restrictions) to see whether constraints eventually lead to property elimination or can stabilize at constraint.',
    'If property status entails lack of moral standing (the abolitionist logic), then the welfare reading is conceptually incoherent and cannot stabilize—it is a transient compromise that must eventually collapse into either property or abolition. If constraints-on-property can stabilize (property status + moral constraints), the welfare reading is a genuinely coherent third position. This is a conceptual rather than empirical question; resolution would clarify whether the reading is internally consistent or self-undermining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_sentience_coherence, conceptual, 'Logical coherence of property status with acknowledged moral standing of sentience.').

omega_variable(
    regulatory_capture_feedback,
    'To what degree do livestock industry interests shape welfare regulation, and does industry influence on standard-setting serve to maintain property rights while appearing to accommodate moral concerns?',
    'Analysis of regulatory body composition and funding sources; tracking of industry lobbying and regulatory capture mechanisms; comparison of proposed welfare standards from animal advocates vs. adopted standards vs. industry compliance costs to assess whose interests prevail. Post-regulation impact assessment: do adopted standards significantly reduce suffering, or do they remain at the minimum that maintains industry profitability?',
    'If regulatory capture is substantial, welfare regulations function as moral cover for continued property-based extraction—the constraint becomes a snare using welfare appearance to legitimize use. If regulatory capture is minimal and standards genuinely constrain for welfare reasons, the constraint is a functional tangled rope with real coordination value and real extraction asymmetry. The degree of capture determines whether the constraint''s claimed function (protecting sentient animals within use) is operational or performed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback, empirical, 'Degree of livestock industry influence on welfare regulation and whether it serves to maintain property rights.').

omega_variable(
    welfare_sentience_knowledge_gap,
    'Is the currently accepted science of animal sentience and suffering capacity stable and complete, or are we systematically underestimating animal cognitive and emotional complexity and thus underestimating the moral wrong of regulated use?',
    'Continued empirical research on animal cognition, emotion, and subjective experience. Focus on previously underestimated species (fish, cephalopods, arthropods) and phenomena (emotional contagion, anticipatory suffering, trauma). Long-term assessment of whether welfare standards themselves prove inadequate as evidence of deeper suffering emerges.',
    'If current sentience science is revealed as substantially incomplete, welfare standards built on it are inadequate, and the moral case for abolition strengthens. If science stabilizes around current understanding, welfare regulation can claim empirical grounding. This is an empirical/knowledge question: the answer shapes whether the welfare reading''s factual premises hold and whether welfare constraints are adequate expressions of sentience''s moral relevance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_sentience_knowledge_gap, empirical, 'Completeness and stability of scientific knowledge of animal sentience and suffering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement_basis(anim_tr_t40, projected).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__welfare_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement_basis(anim_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(anim_be_t40, projected).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__welfare_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(anim_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(anim_su_t40, projected).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__welfare_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(anim_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested animal_status_kernel. The kernel is the commitment to animal moral status and permissible boundaries of human use. Three readings decompose the kernel with structurally distinct ε values and stakeholder structures: property_reading (animals as property, no moral standing independent of owner interest; ε near 0.0, Mountain), abolitionist_reading (animals as moral persons with right not to be property; ε near 1.0, Snare), welfare_reading (this constraint — animals sentient and morally considerable, use permissible if regulated; ε moderate at 0.58, Tangled Rope). The readings differ on fundamental premises about what moral status animals possess and what follows from sentience. Each reading is instantiated in a separate constraint story; they are linked via network.affects_constraints to model the kernel contest. The welfare reading influences both siblings because it occupies a structural middle position: it acknowledges animal sentience (abolitionist premise) while retaining use rights (property premise), and thus creates pressure on both: on abolitionists to explain why acknowledged sentience does not entail right not to be property, and on property adherents to explain why sentience is not morally relevant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
