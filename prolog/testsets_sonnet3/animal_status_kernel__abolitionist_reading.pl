% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status Under the Abolitionist Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the
 *   animal_status_kernel: the claim that animals are moral persons holding a
 *   basic right against being property, that property status itself (not
 *   merely its poor administration) constitutes the injustice, and that all
 *   instrumental use is categorically impermissible regardless of welfare
 *   conditions attached to it. This reading is generated as its own clean
 *   constraint, per the ε-invariance principle: it does not average with,
 *   hedge against, or describe the property_reading or welfare_reading
 *   (sibling constraints, generated separately). Under this reading, the
 *   standing arrangement under contest is the existing legal-economic
 *   property regime governing animals — assessed by this reading's own lights
 *   as comprehensively and categorically extractive, since welfare
 *   improvements within that regime do not reduce the rights violation this
 *   reading identifies.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bears extraction across the full production cycle
 *   - laboratory_animals: primary target (powerless/trapped) — bears extraction through non-consensual research use
 *   - livestock_industry: primary beneficiary (institutional/arbitrage) — captures economic value from property status
 *   - legal_property_regime: agenda-setting doctrinal structure — the injustice this reading names directly
 *   - welfare_reformers: adjacent but excluded actor — pursues a strategically distinct, and on this reading potentially legitimizing, path
 *   - abolitionist_advocates: analytical/normative voice for this reading — argues the categorical position from outside formal lawmaking power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.78).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status Under the Abolitionist Reading").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '362f4a37-faac-4343-816d-742d96a4bcb7').
narrative_ontology:cs_kernel_codification('362f4a37-faac-4343-816d-742d96a4bcb7', distributed).
narrative_ontology:cs_authority_grounding('362f4a37-faac-4343-816d-742d96a4bcb7', distributed).
narrative_ontology:cs_reading_relation('362f4a37-faac-4343-816d-742d96a4bcb7', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('362f4a37-faac-4343-816d-742d96a4bcb7', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_axiom('362f4a37-faac-4343-816d-742d96a4bcb7', foundational, sentient_beings_are_moral_persons_with_rights_against_ownership).
narrative_ontology:cs_axiom_status(sentient_beings_are_moral_persons_with_rights_against_ownership, holdable).
narrative_ontology:cs_axiom_grounding('362f4a37-faac-4343-816d-742d96a4bcb7', sentient_beings_are_moral_persons_with_rights_against_ownership, deontological).
narrative_ontology:cs_axiom('362f4a37-faac-4343-816d-742d96a4bcb7', foundational, property_status_itself_constitutes_the_injustice_independent_of_treatment).
narrative_ontology:cs_axiom_status(property_status_itself_constitutes_the_injustice_independent_of_treatment, holdable).
narrative_ontology:cs_axiom_grounding('362f4a37-faac-4343-816d-742d96a4bcb7', property_status_itself_constitutes_the_injustice_independent_of_treatment, deontological).
narrative_ontology:cs_axiom('362f4a37-faac-4343-816d-742d96a4bcb7', secondary, welfare_improvement_does_not_reduce_rights_violation).
narrative_ontology:cs_axiom_status(welfare_improvement_does_not_reduce_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('362f4a37-faac-4343-816d-742d96a4bcb7', welfare_improvement_does_not_reduce_rights_violation, deontological).
narrative_ontology:cs_reference_frame('362f4a37-faac-4343-816d-742d96a4bcb7', pre_domestication_non_property_relation).
narrative_ontology:cs_drift_state('362f4a37-faac-4343-816d-742d96a4bcb7', contemporary_industrial_animal_use_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('362f4a37-faac-4343-816d-742d96a4bcb7', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, livestock_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_operators).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, pet_breeding_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, legal_property_regime).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, captive_display_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, companion_animals_as_property).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, companion_animals_as_property).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, categorical_moral_personhood_of_sentient_beings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and slaughtered as units of production under legal property status. On the abolitionist reading, every stage of this life cycle — regardless of welfare conditions applied to housing, transport, or slaughter method — constitutes a rights violation because the underlying property relation is itself the injury. They have no legal standing to exit or object; the reading holds this absence of standing is precisely what must change.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Used in research as property regardless of the scientific value produced. Institutional review boards regulate pain and distress but do not question ownership. Under this reading, IACUC welfare protocols function as elaborate management of an underlying injustice rather than a solution to it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Held in zoos, marine parks, and circuses as exhibited property. Accreditation standards govern enclosure size and enrichment but never ownership itself. Their confinement is categorically wrong under this reading independent of how spacious or enriched the enclosure is.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, captive_display_animals, payer,
    powerless, immediate, trapped, global).

% Legally classified as property that can be bought, sold, bred, and euthanized at owner discretion, even though many receive genuine care. The reading treats this as a harder case: the property relation is the same injustice even where affection, not exploitation, characterizes daily treatment — companionship does not cure the categorical wrong of ownership.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animals_as_property, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, companion_animals_as_property, beneficiary).

% Operates the entire economic infrastructure of animal agriculture, from breeding through slaughter and distribution. Lobbies against legal personhood recognition, funds welfare-science research that reframes the property relation as manageable rather than unjust, and can relocate production across jurisdictions to avoid stricter regimes.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, livestock_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, livestock_industry, agenda_setter).

% Rely on animal property status to conduct research without consent requirements. Depend on continued legal classification of research subjects as property to maintain current protocols; alternatives (in vitro, computational models) exist but are not universally substitutable yet.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, national).

% Generate revenue from displaying or performing animals held as property. Increasingly face reputational and regulatory pressure but retain legal cover through property status and welfare-compliance certification.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_operators, beneficiary,
    organized, biographical, constrained, national).

% Produces companion animals as commodities for sale, relying on the legal treatment of animals as property that can be manufactured, priced, and transferred. Faces little structural threat since companion-animal ownership carries strong cultural legitimacy independent of the abolitionist critique.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, pet_breeding_industry, beneficiary,
    organized, biographical, mobile, national).

% The body of statute and case law classifying animals as property (chattel), subject to owner disposal rights subject only to anti-cruelty statutes. This is the doctrinal structure the abolitionist reading identifies as the injustice itself, not merely its instrument.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_property_regime, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__abolitionist_reading, legal_property_regime).

% Animal welfare organizations pursuing incremental regulatory improvement (cage-free mandates, slaughter-method standards) within the property framework. The abolitionist reading treats their strategy as, at best, a distinct empirical bet about pathway to abolition and, at worst, a legitimizing partner to the property regime — their voice is present in animal advocacy generally but excluded from this reading's own categorical framework, which does not recognize welfare improvement as progress toward its goal.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reformers, excluded,
    organized, biographical, mobile, national).

% Legal scholars, philosophers, and activist organizations (in the tradition associated with Gary Francione and others) arguing for the categorical abolition of animal property status. They are excluded from lawmaking bodies and mainstream policy discourse, which is dominated by welfare-regulatory frameworks that this reading treats as an obstacle rather than a stepping stone.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, generational, mobile, global).

% Purchase and consume products derived from animals held as property, at prices that do not reflect the moral cost the abolitionist reading assigns to the underlying rights violation. Individually able to exit via dietary and consumption choices, though systemic dependence on animal-derived goods is deep and normalized.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None that this reading recognizes as legitimate: the property-status arrangement coordinates supply chains, research protocols, and consumer markets efficiently, but the reading holds that efficient coordination of an unjust relation is not a justification for it — the coordination function and the injustice are the same structure viewed from different angles.
% TRANSFER_FUNCTION: Moves labor, bodily integrity, reproductive control, and life itself from animals classified as property to the humans and institutions who own, sell, use, and consume them, with no compensating flow back to the animals under any welfare arrangement, however generous.
% ABSENT_VOICES: The animals themselves have no legal standing to object and cannot be represented except through proxy advocacy; welfare reformers who share concern but reject the categorical framing are present in the broader movement but structurally excluded from this reading's own analytical frame, which treats their incrementalist strategy as either naive or complicit.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight (the counterfactual this reading endorses), animal agriculture, biomedical research on animal subjects, exhibition industries, and companion-animal commerce would all require complete legal and economic reconstruction — the world's food, research, and entertainment systems are built on the premise this reading identifies as unjust.
% FOUNDING_PROBLEM: Historically, animal property status was established to secure agricultural productivity, enable scientific experimentation without consent barriers, and formalize ownership of domesticated and captive animals under existing property law frameworks inherited from Roman and English common law.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., scholarship tracing animal law's common-law property roots) attest the founding function was straightforwardly economic and predates any welfare consideration. Industry beneficiaries attest the arrangement remains functionally necessary for food security and medical progress. The abolitionist reading itself, corroborated by independent legal philosophers outside the beneficiary set, holds the founding problem was never legitimate to begin with — the dispute is not only about whether the problem is 'live' but whether it was ever a problem whose solution justified treating sentient beings as property.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.91, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.91) and rising slightly over the interval because, under this reading, welfare reforms that reduce measured suffering do not reduce the rights violation — the reading holds that the extraction is constituted by the property relation itself, so even a welfare-improving trajectory (which might lower extraction under the welfare_reading's own metrics) shows flat-to-rising extraction here as institutional entrenchment of the property framework continues via welfare-science legitimation. Suppression is high (0.78) and rising modestly, reflecting hardening legal and cultural mechanisms (agricultural exceptionalism in cruelty statutes, standing doctrines barring animal-interest litigation) that actively foreclose the abolitionist alternative. Theater ratio is kept low (0.2) because welfare-compliance activity, while functionally inadequate on this reading's own terms, is not being authored as mere performance — it is a real, if (on this reading) misdirected, coordination function serving the beneficiaries' interests. Accessibility collapse is moderate (0.35) rather than high because legal and cultural alternatives (personhood litigation, sanctuary movements, plant-based substitution) remain visible and actively contested, not foreclosed as a mountain's alternatives would be. Resistance is high (0.72): a substantial, organized, and growing movement (encompassing but distinct from welfare reformers) actively contests the property framework.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting legal_property_regime and beneficiary seats (livestock_industry, biomedical_research_institutions) would compute this arrangement as functional, welfare-regulated coordination — a rope or tangled_rope story from their own vantage, since they experience compliance costs as manageable overhead on a legitimate economic and scientific activity. The payer seats (farmed_animals, laboratory_animals, captive_display_animals) cannot self-report under this framework, which is itself part of what the reading identifies as the injustice — their seat is computed from powerless/trapped structural position rather than testimony, producing a computed classification sharply divergent from the agenda-setter's self-perception. This divergence is the story the reading exists to register, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Farmed, laboratory, captive-display, and companion animals held as property are declared victims because the reading holds they bear the entire cost of a rights violation with no consenting or compensating benefit; their power is powerless and exit is trapped by definition of the property relation, driving directionality toward the full-target end regardless of individual treatment quality. Livestock, biomedical research, exhibition, and breeding industries are beneficiaries with institutional-to-organized power and mobile-to-arbitrage exit, capturing economic and scientific value while facing limited binding constraint. Consumers are declared moderate-power beneficiaries with mobile exit (individual consumption choice exists) even though systemic dependence on animal products is described as deep — the directionality here reflects genuine, if diffuse, benefit capture without the trapped status assigned to the animals themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists the standard mandatrophy resolution move (arguing that a mandate has outlived a founding problem it once genuinely solved) because it denies the founding problem was ever a legitimate justification for treating sentient beings as property — the founding_problem_status is authored contested rather than dead, because the reading's own position is that the arrangement was never justified rather than that it has drifted from a once-valid function. This blocks the usual welfare-reform resolution path (incrementally updating the mandate to fit present welfare science) since, on this reading, no welfare update reaches the actual injustice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_pathway_ambiguity,
    'Do incremental welfare reforms (cage-free mandates, slaughter-method standards, enriched enclosures) function as a pathway toward eventual abolition of property status, or do they legitimize and entrench the property framework by making it appear adequately just?',
    'Longitudinal comparative study of jurisdictions with strong welfare regulation (e.g., EU farm animal welfare directives) versus jurisdictions with weaker welfare regulation, tracking whether property-status abolition proposals gain or lose political traction over multi-decade windows following welfare reform waves.',
    'If welfare reform is empirically shown to reduce pressure for abolition (a legitimation effect), the abolitionist reading''s categorical rejection of welfare-focused strategy is vindicated as strategically as well as morally correct. If welfare reform is shown to build public sentiment and institutional capacity that later supports abolition, the strategic tension dissolves and the two readings could be sequenced rather than opposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_pathway_ambiguity, empirical, 'Whether welfare reform advances or forestalls the abolitionist reading''s categorical goal — the central strategic dispute with the welfare_reading.').

omega_variable(
    moral_personhood_criterion_ambiguity,
    'What is the correct criterion for moral personhood that grounds the right against being property — sentience alone, a threshold of cognitive complexity, capacity for interests, or something else — and do all currently-used animal species meet it uniformly?',
    'Convergent evidence from comparative cognition, neuroscience of sentience, and philosophical argument regarding which capacities are necessary and sufficient for the relevant right; the resolution is partly conceptual (which criterion is correct) and partly empirical (which species meet it).',
    'If the personhood criterion is sentience alone, the victim-set is very broad (potentially including all farmed and laboratory vertebrates and beyond). If it requires higher cognitive capacities, the victim-set narrows substantially, changing which uses this reading classifies as categorically impermissible and how many stakeholders it implicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_personhood_criterion_ambiguity, conceptual, 'What grounds moral personhood and how broadly it extends across species, which determines the scope of this reading''s victim-set.').

omega_variable(
    property_abolition_vs_use_regulation_distinctness,
    'Is ''property status'' as a legal category actually separable from ''use,'' such that abolishing property status while permitting some forms of interaction (e.g., companion relationships, wildlife rehabilitation) is coherent, or does any regulated interaction with an entity necessarily reconstitute a property-like relation?',
    'Legal-theoretical analysis of proposed alternative legal statuses (e.g., guardianship models, legal personhood with restricted capacities) already implemented for some entities (rivers, great apes in limited jurisdictions) to assess whether a non-property, non-full-personhood intermediate status is coherent and stable.',
    'If a coherent non-property intermediate status exists, the abolitionist reading''s ''all use categorically impermissible'' claim may need refinement to ''all property-based use categorically impermissible,'' which changes the classification of certain constrained relationships (e.g., companion animals with strong welfare-plus-autonomy protections). If no coherent intermediate exists, the categorical claim as authored stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_abolition_vs_use_regulation_distinctness, conceptual, 'Whether property abolition is separable from use in general, affecting how broadly the categorical impermissibility claim extends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__abolitionist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__abolitionist_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__abolitionist_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__abolitionist_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__abolitionist_reading, base_extractiveness, 8, 0.86).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__abolitionist_reading, base_extractiveness, 16, 0.88).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__abolitionist_reading, base_extractiveness, 24, 0.89).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__abolitionist_reading, base_extractiveness, 32, 0.9).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__abolitionist_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__abolitionist_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__abolitionist_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__abolitionist_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'animal rights vs. animal welfare debate' under the animal_status_kernel per the epsilon-invariance principle: property_reading (animals as property, ε low-to-moderate from that reading's own lights since no rights violation is recognized), welfare_reading (animals as regulable sentient property, ε moderate — some extraction acknowledged but treated as legitimately reducible via regulation), and this abolitionist_reading (animals as moral persons wrongfully held as property, ε high — the property relation itself is the violation, unmitigated by welfare regulation). The three do not share an ε value because they are not measuring the same claim by different observables; they are three structurally distinct constraints that happen to share a natural-language label and a legal-institutional object (the animal property regime). Each has its own beneficiary/victim structure and stands or falls on its own metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
