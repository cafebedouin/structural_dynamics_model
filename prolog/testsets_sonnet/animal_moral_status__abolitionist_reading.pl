% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Property Status as Categorical Rights Violation (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the animal moral
 *   status kernel: animals are rights-bearing individuals, and their legal
 *   categorization as property is itself the violation, independent of the
 *   treatment conditions imposed on them. Under this reading, welfare reforms
 *   — improved husbandry, 'humane' slaughter, enriched enclosures — do not
 *   remedy the wrong because they operate entirely within the property frame,
 *   adjusting the terms of use without disturbing the categorical claim that
 *   animals may be owned, bought, sold, and killed as a matter of ordinary
 *   human entitlement. There is no beneficiary group under this reading: the
 *   abolitionist claim holds that no party's interest is legitimately served
 *   by a use-relationship built on a categorical wrong, so apparent
 *   beneficiaries (consumers who believe 'humane' labeling resolves the
 *   ethical question) are experiencing a false resolution, not a real one.
 *   This is a sibling story to property_reading (which denies animals
 *   independent moral standing altogether) and welfare_reading (which accepts
 *   sentience and regulates suffering while permitting use); each reading is
 *   authored as its own ε-invariant constraint per the decomposition
 *   principle — the same underlying practices (farming, research, ownership)
 *   yield structurally different constraints depending on which reading is
 *   applied, because the beneficiary/victim structure and the transfer
 *   function differ sharply across readings.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary victim class (powerless/trapped) — bear the extraction regardless of treatment conditions
 *   - animal_industries: agenda_setter (organized/mobile) — design and defend the property architecture
 *   - welfare_regulators: agenda_setter/observer (institutional/constrained) — administer a framework this reading holds structurally incapable of resolving the wrong
 *   - welfare_consumers: nominal beneficiary (moderate/mobile) — experience moral relief the reading treats as illegitimate
 *   - abolitionist_advocates: excluded (moderate/constrained) — press the categorical claim from outside regulatory processes
 *   - legal_philosophers_of_animal_rights: analytical observer — document the gap between welfare regulation and rights recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.82).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.75).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status as Categorical Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'fdd59c17-0e27-4b2a-9066-48751a2ea5fc').
narrative_ontology:cs_kernel_codification('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', distributed).
narrative_ontology:cs_authority_grounding('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', distributed).
narrative_ontology:cs_reading_relation('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', foundational, property_status_itself_constitutes_the_violation).
narrative_ontology:cs_axiom_status(property_status_itself_constitutes_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', property_status_itself_constitutes_the_violation, deontological).
narrative_ontology:cs_axiom('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', secondary, regulated_use_cannot_remedy_a_categorical_wrong).
narrative_ontology:cs_axiom_status(regulated_use_cannot_remedy_a_categorical_wrong, holdable).
narrative_ontology:cs_axiom_grounding('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', regulated_use_cannot_remedy_a_categorical_wrong, conventional).
narrative_ontology:cs_reference_frame('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', pre_legal_personhood_use_relation).
narrative_ontology:cs_drift_state('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', contemporary_animal_law_movement, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('fdd59c17-0e27-4b2a-9066-48751a2ea5fc', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, companion_animals_under_ownership).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, working_and_entertainment_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, wild_animals_managed_as_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and killed as legal property regardless of welfare conditions. Under this reading, even a pasture-raised, painlessly slaughtered animal remains a victim of the property relation itself, because the relation authorizes the killing and use as a baseline entitlement rather than a wrong requiring justification. No exit exists from within the system that produced them.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Used as experimental instruments under regulatory frameworks that weigh their suffering against human benefit rather than treating their use as categorically impermissible. Their legal status as property is what makes the cost-benefit calculation possible at all.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Live under a legal regime of ownership even when materially well-treated; can be bought, sold, bred, or euthanized at an owner's discretion. Even affectionate, non-abusive relationships are, on this reading, structured by a property relation that treats a rights-bearing individual as a disposable asset.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, companion_animals_under_ownership, payer,
    powerless, biographical, trapped, national).

% Used for labor, transport, or spectacle; their bodies and behaviors are instrumentalized for human benefit under legal frameworks that treat this as ordinary commerce. Retirement or 'good treatment' does not alter the underlying property claim being enforced.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, working_and_entertainment_animals, payer,
    powerless, biographical, trapped, global).

% Classified by law as harvestable or manageable resources (game, fisheries stock, culled populations) even outside direct ownership, extending the property logic to populations rather than individuals. Their fate is set by human resource-management decisions in which they have no standing.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, wild_animals_managed_as_resources, payer,
    powerless, biographical, trapped, global).

% Agriculture, biomedical research, entertainment, and pet-trade sectors design, lobby for, and operate within the legal architecture that defines animals as property. They set welfare standards, certification schemes, and 'humane' labeling that this reading treats as legitimating cover rather than remedy. They can adapt business models faster than the legal category could be abolished.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_industries, agenda_setter,
    organized, generational, mobile, global).

% Administer anti-cruelty statutes and husbandry standards that regulate the conditions of use without questioning the underlying property status. From the abolitionist reading, their entire mandate presupposes the legitimacy of the relation this constraint identifies as the actual violation, making regulatory improvement structurally incapable of resolving the wrong.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_regulators, observer).

% Purchase 'humane,' 'cage-free,' or 'ethically sourced' animal products, believing the moral problem is resolved by conditions of treatment. This reading treats their moral relief as unearned: the underlying use-relationship persists regardless of labeling, so no coordination benefit actually accrues to any party — hence no true beneficiary group, only a perceived one.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_consumers, beneficiary,
    moderate, biographical, mobile, national).

% Argue that welfare reform legitimizes and entrenches use rather than ending it, and that the property category itself must be abolished. Largely excluded from legislative and regulatory processes, which are structured around welfare-reading premises that treat use as the baseline to be regulated rather than the wrong to be eliminated.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, civilizational, constrained, global).

% Analyze the structural difference between welfare regulation and rights recognition, arguing that legal personhood, not improved treatment, is the only coherent remedy for a property-status wrong. They document the categorical gap between what welfare law can achieve and what the abolitionist claim requires.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_philosophers_of_animal_rights, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading. The property-status arrangement is presented by its administrators as coordinating human use of animal labor, flesh, and bodies efficiently and predictably, but the abolitionist reading holds that no genuine coordination problem is being solved for the party whose interests are most directly at stake — the animals themselves are not parties to any coordination, only its object.
% TRANSFER_FUNCTION: Moves biological capacity, life, labor, and bodily integrity from animals to human users (producers, consumers, researchers, owners) via a legal category that authorizes disposal, sale, and killing as ordinary incidents of ownership rather than as harms requiring justification.
% ABSENT_VOICES: Animals themselves have no standing to object within any existing legal system; abolitionist advocates who would press the categorical claim are structurally excluded from legislative and regulatory venues built on welfare-reading premises. Their exclusion is total in the sense that even sympathetic reform processes cannot generate the outcome this reading requires (abolition of the category), because those processes only ever ask how use should be regulated.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight, entire industries (agriculture, biomedical research, entertainment, much of the pet trade) would be structurally impossible in their current form; legal personhood or guardianship frameworks would have to replace ownership; supply chains, food systems, and research methodologies would require fundamental reconstruction.
% FOUNDING_PROBLEM: Historically, property status was constructed to resolve competing human claims over animals as scarce, useful resources — settling disputes over ownership, use rights, and liability in agrarian and early industrial economies.
% FOUNDING_PROBLEM_CORROBORATION: Animal industries and welfare regulators attest the property framework remains necessary for functioning food systems, medical research, and orderly commerce. Abolitionist legal scholars and philosophers (Gary Francione and the broader abolitionist tradition), writing from outside the industries that benefit from the arrangement, attest that the original resource-allocation problem has been resolved by modern economic capacity and that the property category now functions primarily to authorize continued use rather than to solve any live coordination problem.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) because the reading holds that the transfer (bodily integrity, labor, life itself) from animal to human occurs regardless of treatment quality — the property relation, not the conditions of use, is what does the extracting. Suppression is authored substantial (0.75) because the entire welfare-regulatory apparatus, and the market structures built on it, actively foreclose the categorical remedy (personhood/abolition) by channeling reform energy into within-frame adjustments. Theater ratio is authored low-to-moderate and rising (0.08 to 0.20) reflecting the reading's claim that 'humane' certification and welfare-improvement programs increasingly function as legitimating performance for a use-relationship whose fundamental structure is unchanged — a modest but real Goodhart drift as labeling schemes proliferate. Accessibility collapse is authored moderate (0.4): unlike a mountain, alternatives to the property frame (personhood statutes, guardianship models) are conceptually available and increasingly litigated, so collapse is far from total. Resistance is authored substantial (0.7) reflecting active abolitionist advocacy, sanctuary movements, and litigation efforts contesting the property category directly.
 *
 * PERSPECTIVAL GAP:
 *   Animal industries and welfare regulators, from their agenda_setter seats, would experience the property framework as legitimate, functioning coordination — solving real problems of resource allocation, liability, and food security. From the payer seats (all animal groups), the same structure operates as categorical extraction unaffected by welfare improvements. Welfare_consumers occupy an intermediate, contested position: they perceive themselves as beneficiaries of ethical resolution, which this reading holds is a misperception generated by industry labeling practices rather than a genuine structural benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   All animal groups are declared victims with d near the full-target end: trapped exit options, powerless status, and a transfer function that runs entirely one direction (from animal to human user) regardless of treatment quality. No beneficiary group is declared in base_properties because the abolitionist reading denies that any party is legitimately served by a wrongful use-relationship; welfare_consumers appear as stakeholders with role=beneficiary to represent the perceived-benefit position the reading argues is illusory, but this is a structural claim about false consciousness, not a concession that real coordination benefit exists. Animal industries and welfare regulators are agenda_setters — organized/institutional actors whose exit options (mobile, constrained) reflect their capacity to adapt within or defend the existing frame, in sharp asymmetry with animals' trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem answer documents a genuine historical coordination function (resolving competing human claims over scarce animal resources in agrarian economies) that the abolitionist reading holds has become obsolete relative to modern economic capacity, while the property category persists and has been repurposed to authorize continued use. This is the mandatrophy pattern: founding_problem_status is authored 'contested' because industries and regulators (the benefiting parties) attest continued necessity while outside corroborators (abolitionist legal scholarship) attest the original problem is resolved and the category now serves extraction. The classification prevents mislabeling this as pure coordination (which the property_reading effectively claims) or as fully resolved cruelty regulation (which the welfare_reading claims) by treating the categorical structure itself, independent of treatment quality, as the object of analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingent_or_structural,
    'Is animal property status a contingent legal artifact that could be abolished through ordinary legal reform (supporting a tangled_rope reading, since the coordination function of resource allocation is separable from the extraction), or is it structurally load-bearing for the entire economic system of animal use (supporting a snare reading, since no coordination benefit survives abolition)?',
    'Comparative legal analysis of jurisdictions that have granted limited personhood or ''sentient being'' status to animals (e.g., statutory reforms in France, New Zealand, parts of the EU) and tracking whether underlying use-industries persisted, adapted, or collapsed following the status change.',
    'If contingent, the constraint is better modeled as tangled_rope (a real allocative coordination function exists alongside the extraction, and reform could preserve the former while eliminating the latter); if structural, snare is the more accurate classification since the entire arrangement is extraction with no separable coordination residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingent_or_structural, conceptual, 'Whether property status is a contingent extraction layer or the load-bearing structure of the entire use-system.').

omega_variable(
    welfare_reform_as_legitimation_or_genuine_progress,
    'Do welfare reforms (improved husbandry standards, ''humane'' certification, anti-cruelty statutes) function as genuine harm-reduction that abolitionists should credit, or as legitimating theater that entrenches the property category by giving it a moral gloss?',
    'Longitudinal tracking of whether welfare-reform jurisdictions show declining or stable rates of animal use over time; if use rates are flat or rising alongside welfare improvements, the legitimation-theater reading gains support; if use rates decline as welfare standards rise, the genuine-progress reading gains support.',
    'Directly affects the theater_ratio trajectory and whether this reading''s rising theater_ratio measurement (0.08 to 0.20) is accurately characterizing welfare reform as Goodhart drift versus mischaracterizing genuine incremental progress as mere performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_genuine_progress, empirical, 'Whether welfare reform measurably reduces the harm this reading identifies, or merely legitimizes its continuation.').

omega_variable(
    sibling_reading_incommensurability,
    'Are the property_reading, welfare_reading, and abolitionist_reading genuinely incommensurable framings resting on different foundational premises about moral status, or do they represent points on a single continuum of increasing moral consideration that could in principle converge through argument?',
    'Track whether legal and philosophical discourse treats shifts between these readings as continuous policy adjustment (supporting a continuum view) or as discrete paradigm shifts requiring wholesale reconceptualization of legal personhood (supporting incommensurability).',
    'If incommensurable, the three sibling constraints are correctly authored as entirely separate stories with no possible synthesis; if continuum-like, the network relationship between the readings (currently coexists_with) may better be modeled with influences edges showing gradual pressure from one reading toward another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incommensurability, conceptual, 'Whether the three kernel readings are structurally incommensurable or points on a convergent continuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__abolitionist_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__abolitionist_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__abolitionist_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__abolitionist_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__abolitionist_reading, base_extractiveness, 8, 0.79).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__abolitionist_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__abolitionist_reading, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__abolitionist_reading, base_extractiveness, 32, 0.815).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__abolitionist_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__abolitionist_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__abolitionist_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__abolitionist_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.05).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language kernel 'animal moral status' per the ε-invariance principle. property_reading yields near-zero extraction (no victim class recognized); welfare_reading yields moderate extraction bounded to cases of unregulated cruelty (victim set: animals subjected to substandard treatment within a use-permitting framework); abolitionist_reading (this story) yields high extraction across the entire class of animals under human dominion, because the wrong is located in the categorical relation rather than in treatment conditions. The three stories share source practices (farming, research, ownership) but are not the same constraint measured three ways — they differ in ε, in victim/beneficiary structure, and in transfer function, which is precisely why they are authored as three separate, linked files rather than one story with a framing parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
