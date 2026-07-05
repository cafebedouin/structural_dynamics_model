% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Abolitionist Reading of Animal Moral Status: Inherent Value Precluding All Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the animal-status
 *   kernel: animals are rights-holders whose inherent value categorically
 *   precludes instrumental use, so no welfare improvement can legitimize
 *   confinement, killing, experimentation, exhibition, or commodification.
 *   Extractiveness is authored near-maximal (0.91) because, from this
 *   reading's own premises, every current use category is a full rights
 *   violation with no legitimate portion of the transaction — there is no
 *   coordination residue to net against the extraction. Theater ratio rises
 *   across the interval (0.15 to 0.40) tracking the increasing sophistication
 *   of welfare-certification labeling ('cage-free', 'ethically sourced',
 *   'cruelty-free') that this reading treats as legitimation theater layered
 *   onto an unchanged instrumental-use structure rather than genuine progress
 *   toward cessation. Accessibility collapse is deliberately authored low
 *   (0.35) and resistance high (0.88): this reading does NOT describe a
 *   settled natural fact that has closed off alternatives — it describes a
 *   heavily contested philosophical claim that meets enormous resistance from
 *   industry, from welfare-reform advocates, and from mainstream consumer
 *   practice. That resistance profile is itself part of what makes this a
 *   claim-under-contest rather than an accepted mountain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.72).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading of Animal Moral Status: Inherent Value Precluding All Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '23f99d7c-aa19-4f60-8274-2116ab80ba84').
narrative_ontology:cs_kernel_codification('23f99d7c-aa19-4f60-8274-2116ab80ba84', distributed).
narrative_ontology:cs_authority_grounding('23f99d7c-aa19-4f60-8274-2116ab80ba84', distributed).
narrative_ontology:cs_reading_relation('23f99d7c-aa19-4f60-8274-2116ab80ba84', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('23f99d7c-aa19-4f60-8274-2116ab80ba84', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('23f99d7c-aa19-4f60-8274-2116ab80ba84', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('23f99d7c-aa19-4f60-8274-2116ab80ba84', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('23f99d7c-aa19-4f60-8274-2116ab80ba84', secondary, welfare_reform_constitutes_illegitimate_legitimation).
narrative_ontology:cs_axiom_status(welfare_reform_constitutes_illegitimate_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('23f99d7c-aa19-4f60-8274-2116ab80ba84', welfare_reform_constitutes_illegitimate_legitimation, instrumental).
narrative_ontology:cs_reference_frame('23f99d7c-aa19-4f60-8274-2116ab80ba84', categorical_rights_holder_status).
narrative_ontology:cs_drift_state('23f99d7c-aa19-4f60-8274-2116ab80ba84', contemporary_welfare_statute_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('23f99d7c-aa19-4f60-8274-2116ab80ba84', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_and_exhibition_operators).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, fur_and_leather_producers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, captive_wildlife).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, fur_bearing_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, companion_animals_bred_and_sold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, moral_patienthood_of_nonhuman_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and slaughtered for food and byproducts under legal regimes that treat them as property. Under this reading their confinement and killing is not a regulated harm to be minimized but a categorical rights violation with no legitimate instrumental justification, regardless of how humane the conditions are made.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Used in biomedical, cosmetic, and basic research under welfare-statute oversight that permits pain and death for human benefit. This reading holds that no quantum of scientific benefit can license using a rights-holder as a means, so every experimental use is a rights violation independent of protocol quality.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Held in zoos, aquariums, and circuses for exhibition, conservation-branding, and entertainment revenue. The abolitionist reading treats confinement for display as instrumental use of a rights-holder regardless of enclosure quality or conservation outcomes, foreclosing the conservation-through-captivity justification entirely.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, captive_wildlife, payer,
    powerless, immediate, trapped, global).

% Farmed or trapped for pelts. There is no welfare-improvement pathway that satisfies this reading, since the entire industry is instrumental use of a body for a non-necessary human product.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_bearing_animals, payer,
    powerless, immediate, trapped, global).

% Bred and sold as commodities in the pet trade. This reading treats commercial breeding and sale itself (not just neglect or cruelty) as a rights violation, since it treats a rights-holder as an item of commerce.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, companion_animals_bred_and_sold, payer,
    powerless, immediate, trapped, global).

% Operates the confinement, slaughter, and byproduct supply chains that the abolitionist reading would eliminate outright rather than reform. The industry's entire business model is the instrumental use this reading forecloses as legitimate; it has no path to compliance short of ceasing operations.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on animal models for regulatory approval pathways and basic research funding structures. Under this reading, no welfare improvement (better anesthesia, smaller sample sizes, IACUC oversight) legitimizes the practice; only cessation would satisfy the claim.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Zoos, circuses, and marine parks that generate revenue and cultural legitimacy from displaying captive animals. This reading rejects their conservation and education justifications as legitimation narratives for continued instrumental use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, entertainment_and_exhibition_operators, beneficiary,
    organized, biographical, constrained, national).

% Produce non-necessity goods from animal bodies. Under this reading the product category itself, not the production method, is the rights violation, so welfare certification schemes (e.g. 'ethically sourced fur') are read as extending rather than curing the harm.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_and_leather_producers, beneficiary,
    organized, biographical, constrained, global).

% Advocate for improved conditions, cage-free standards, and slaughter method reform within continued use. The abolitionist reading treats their incrementalist strategy as complicit legitimation of instrumental use rather than genuine progress, structurally excluding their framework from the moral conversation this reading conducts even though they are the dominant real-world advocacy voice for animals.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, welfare_reform_organizations, observer).

% Purchase meat, dairy, leather, and animal-tested products at prices that do not reflect the abolitionist reading's cost accounting. They benefit from low-priced instrumental use but under this reading are also implicated as demand-side participants in a rights violation, and unlike the animals they have full exit available (dietary and consumption choice).
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary).

% Legal scholars, philosophers, and advocacy organizations (in the tradition of Francione and Regan) who articulate and press this reading in courts, legislatures, and public discourse. They set the interpretive agenda for what counts as a rights violation under this framework and reject welfare reform as a legitimate incremental strategy.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no coordination function under this reading — it denies that any legitimate coordination problem is solved by instrumentalizing animals; the only 'coordination' it recognizes as legitimate is universal cessation of use, which is not a coordination mechanism among current parties but a demand for exit from the practice entirely.
% TRANSFER_FUNCTION: The current arrangement (which this reading indicts) moves bodily autonomy, life, and physical integrity from animals to human beneficiaries in the form of food, tested products, entertainment revenue, and companionship commodities; the abolitionist reading frames this as a one-directional transfer with no legitimate consideration flowing back.
% ABSENT_VOICES: The animals themselves cannot testify to their interests in a forum that adjudicates the claim; welfare reform organizations, who represent a large share of real-world animal advocacy, are treated by this reading as insufficiently radical and are effectively excluded from co-authoring the abolitionist position despite overlapping goals; industry actors participate extensively in policy debate but never as parties conceding the reading's core premise.
% DISAPPEARANCE_RATIONALE: If this reading's claim were adopted as binding legal doctrine overnight, animal agriculture, biomedical animal research, exhibition industries, and the fur and companion-animal trade would be required to cease rather than reform, eliminating trillion-dollar supply chains and forcing wholesale reconstruction of food systems, medical research pathways, and pet ownership norms.
% FOUNDING_PROBLEM: The perceived moral inadequacy of welfare regulation: that improving conditions within continued instrumental use (bigger cages, painless slaughter, IACUC review) treats animals as means to human ends no matter how humanely administered, and that only recognizing animals as rights-holders with inherent value can end that categorical wrong.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the advocacy movement by legal philosophers (e.g. critical animal law scholarship) documenting that welfare statutes have historically expanded industry legitimacy and output volume even as they improved discrete conditions — a pattern cited independently of abolitionist self-description. No corroboration exists from the beneficiary industries, who dispute the founding problem's premise entirely; that absence is itself part of the record.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
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
 *   The suppression score (0.72) reflects the coercive apparatus (property law, agricultural exemptions from anti-cruelty statutes, research-animal welfare acts) that this reading identifies as actively enforcing the instrumental-use regime it opposes — suppression here measures the current legal order's enforcement of property status against the rights claim, not any enforcement mechanism the abolitionist reading itself wields (it has none; it is a minority normative position, not an operating institution).
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist_advocates seat, every use category computes as full extraction with no coordination offset. From the animal_agriculture_industry and biomedical_research_institutions seats, the same arrangement would compute (under the sibling welfare or property readings) as legitimate regulated commerce or necessary scientific practice — the engine cannot and does not reconcile these; each reading is a separate constraint with its own ε, per the ε-invariance principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Every animal use category is fully populated with victims (d near 1.0, full target) because the reading's structural premise is that inherent value forecloses any legitimate offsetting benefit — there is no partial coordination function to net against the harm, unlike the welfare reading which would find genuine (if constrained) coordination value in humane-use standards. Industry beneficiaries sit at the low-d end with arbitrage-grade exit (they can relocate production, reformulate products, or exit categories under regulatory pressure) while the animals themselves are maximally trapped with zero exit — this is the widest directionality spread the schema can express, which is structurally appropriate to a reading whose entire claim is that no legitimate transaction exists.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy analysis in the conventional sense because it does not claim a founding problem has been solved and left a vestigial structure behind — it claims the founding problem (categorical rights violation) remains fully live and current welfare-statute frameworks are a legitimation layer, not a partial solution. The founding_problem_status is authored as 'live' rather than 'dead' precisely because the abolitionist claim is that welfare reform has NOT solved the problem, only rebranded it — welfare improvements are read as extending the practice's social license rather than reducing the underlying rights violation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_as_progress_or_legitimation,
    'Does incremental welfare reform (larger cages, painless slaughter methods, reduced research animal counts) constitute genuine moral progress toward reduced animal suffering, or does it primarily extend the social and legal license of instrumental use by making it appear humane?',
    'Longitudinal tracking of total animal use volume and aggregate suffering-adjusted harm following welfare reform episodes (e.g., battery cage bans, research 3Rs adoption) — if reform correlates with declining total use, it supports the welfare reading; if reform correlates with stable or increasing total use alongside improved per-unit conditions, it supports the abolitionist reading''s legitimation critique.',
    'If welfare reform is shown to reliably increase total use volume, this substantially strengthens the abolitionist reading''s core claim and would justify its rejection of the welfare reading as a genuine remedy; if reform reliably reduces total use over time, the abolitionist reading''s blanket rejection of incrementalism weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_progress_or_legitimation, empirical, 'Whether welfare reform functions as genuine progress or as legitimation of continued instrumental use — the central empirical fault line between this reading and welfare_reading.').

omega_variable(
    inherent_value_metaphysical_grounding,
    'Is ''inherent value'' a defensible metaphysical property that grounds categorical rights, or is it a contested philosophical posit that cannot bear the weight this reading places on it?',
    'No empirical resolution mechanism exists; this is a live question in moral philosophy (compare Regan''s rights-based inherent value account against interest-based and contractarian critiques). Resolution, if any, would come from sustained philosophical argument, not data.',
    'If inherent value cannot be non-arbitrarily grounded, the abolitionist reading''s categorical prohibition collapses into the welfare reading''s interest-balancing framework; if it can be grounded, the categorical prohibition holds independent of any welfare-improvement counterargument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inherent_value_metaphysical_grounding, conceptual, 'Whether the reading''s foundational metaphysical claim (inherent value) can bear the categorical prohibition it grounds.').

omega_variable(
    abolitionist_reading_as_committer_axis_choice,
    'Given that the animal_status kernel supports at least three coherent, mutually exclusive readings (abolitionist, welfare, property) each held by substantial constituencies, is the choice to author this reading as the primary lens for any given policy analysis itself doing normative work that should be surfaced rather than assumed?',
    'Cross-reference which reading a given legal system, institution, or advocacy campaign actually operates under (property_reading dominates most current legal codes; welfare_reading dominates statutory reform movements; abolitionist_reading dominates academic animal-rights philosophy and a minority of advocacy organizations) and disclose that operative reading explicitly rather than treating ''animal rights'' as a single settled position.',
    'Failing to disclose which reading is operative in a given context risks conflating three structurally distinct constraints with different victim sets, different ε values, and different practical prescriptions under one colloquial label (''animal rights''), exactly the ε-invariance failure mode the framework is designed to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abolitionist_reading_as_committer_axis_choice, conceptual, 'Documents the committer-axis structure: this story is one reading among three, and the choice of reading is itself the analytically significant fact, not incidental framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anim_tr_t8, animal_status__abolitionist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(anim_tr_t16, animal_status__abolitionist_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(anim_tr_t24, animal_status__abolitionist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(anim_tr_t32, animal_status__abolitionist_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(anim_be_t8, animal_status__abolitionist_reading, base_extractiveness, 8, 0.94).
narrative_ontology:measurement(anim_be_t16, animal_status__abolitionist_reading, base_extractiveness, 16, 0.93).
narrative_ontology:measurement(anim_be_t24, animal_status__abolitionist_reading, base_extractiveness, 24, 0.92).
narrative_ontology:measurement(anim_be_t32, animal_status__abolitionist_reading, base_extractiveness, 32, 0.915).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__abolitionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the animal_status kernel, each authored as a separate ε-invariant constraint per the decomposition principle. property_reading treats animals as unrestricted objects of ownership (near-total extraction, near-total suppression, minimal beneficiary/victim differentiation because no rights claim exists to violate). welfare_reading treats animals as interest-holders whose use is constrained but permitted (moderate extraction, genuine but partial coordination function, victims exist but alongside real welfare gains). This abolitionist_reading treats every instrumental use category as a full rights violation with no legitimate offsetting coordination value, producing the highest extractiveness and the most polarized directionality spread of the three. The property_reading is FORECLOSED by this reading because the abolitionist premise (animals are rights-holders) is logically incompatible with the property_reading's premise (animals are objects without independent moral standing) within any single legal framework — a jurisdiction cannot simultaneously hold both. The welfare_reading COEXISTS because both readings share the premise that animal interests matter morally; they diverge only on whether that shared premise entails prohibition or regulation, a disagreement that persists as a live dispute across different advocacy coalitions rather than a logical contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
