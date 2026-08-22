% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Moral Status: Sentience-Constrained Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the contested
 *   animal-status kernel: animals are sentient beings whose interests matter
 *   morally and legally, but whose interests are balanced against, not
 *   treated as a threshold against, human uses of them. This produces a
 *   structure of statutory exemptions (agricultural exemptions from cruelty
 *   codes, licensed research protocols, humane-slaughter standards) that
 *   regulate the manner of instrumental use while leaving the fact of use
 *   intact. Under this reading, animals enter the victim set specifically for
 *   gratuitous or wanton cruelty (acts falling outside the recognized
 *   welfare-protocol exemptions) but are excluded from the victim set for
 *   standard instrumental use accompanied by welfare compliance —
 *   confinement, painful procedures, and slaughter performed within licensed
 *   or codified limits are not classified as harms this reading recognizes as
 *   extraction, even though the same acts performed outside the exemption
 *   structure would be. The extractiveness figure (ε ≈ 0.45) reflects the
 *   reading's own assessment that the exemption structure itself channels
 *   substantial foreclosed-interest costs onto animals even while remaining,
 *   on its own terms, a constrained and non-abolitionist arrangement.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: institutional beneficiary and co-agenda-setter — obtains legal cover for practices exempted from general cruelty standards
 *   - farmed_animals_in_intensive_systems: powerless, trapped payer — bears routine confinement and pain authorized by the welfare balance
 *   - laboratory_animals_in_permitted_protocols: powerless, trapped payer — bears harm sanctioned by harm-benefit review that structurally favors permitting use
 *   - legislatures_and_regulatory_agencies: institutional agenda-setter — writes and enforces the exemption thresholds
 *   - animal_advocacy_organizations: excluded observer — articulates animal interests without binding authority over the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.5).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare Reading of Animal Moral Status: Sentience-Constrained Instrumental Use").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '02e238b1-4fd5-40d0-a5e5-39cd93513319').
narrative_ontology:cs_kernel_codification('02e238b1-4fd5-40d0-a5e5-39cd93513319', distributed).
narrative_ontology:cs_authority_grounding('02e238b1-4fd5-40d0-a5e5-39cd93513319', distributed).
narrative_ontology:cs_reading_relation('02e238b1-4fd5-40d0-a5e5-39cd93513319', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('02e238b1-4fd5-40d0-a5e5-39cd93513319', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('02e238b1-4fd5-40d0-a5e5-39cd93513319', foundational, interests_constrain_but_do_not_prohibit_use).
narrative_ontology:cs_axiom_status(interests_constrain_but_do_not_prohibit_use, holdable).
narrative_ontology:cs_axiom_grounding('02e238b1-4fd5-40d0-a5e5-39cd93513319', interests_constrain_but_do_not_prohibit_use, deontological).
narrative_ontology:cs_axiom('02e238b1-4fd5-40d0-a5e5-39cd93513319', secondary, welfare_compliance_legitimates_instrumental_use).
narrative_ontology:cs_axiom_status(welfare_compliance_legitimates_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('02e238b1-4fd5-40d0-a5e5-39cd93513319', welfare_compliance_legitimates_instrumental_use, conventional).
narrative_ontology:cs_reference_frame('02e238b1-4fd5-40d0-a5e5-39cd93513319', sentience_recognition_without_rights).
narrative_ontology:cs_drift_state('02e238b1-4fd5-40d0-a5e5-39cd93513319', contemporary_industrial_agriculture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('02e238b1-4fd5-40d0-a5e5-39cd93513319', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals_in_intensive_systems).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals_in_permitted_protocols).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates intensive production systems and lobbies for the statutory welfare standards that define lawful treatment. Complies with minimum housing, transport, and slaughter standards and, by doing so, obtains legal cover for practices (confinement, mutilation without anesthesia in many jurisdictions, short lifespans before slaughter) that would be classed as cruelty if performed on a companion animal. The welfare frame lets the industry set the floor it must clear.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter).

% Bear routine practices (close confinement, painful procedures without anesthesia, early slaughter) authorized precisely because welfare statutes treat interest-satisfaction as balanceable against economic and practical considerations, not as a threshold that use must clear. They have no legal standing to contest their own treatment and no exit from the system that determines their birth, confinement, and death.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals_in_intensive_systems, payer,
    powerless, immediate, trapped, national).

% Undergo procedures that inflict pain, distress, or death when an ethics committee finds the scientific or commercial justification sufficient under a harm-benefit weighing that always resolves in favor of permitting some category of use. Welfare protocols govern how they may be used, never whether they may be used.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals_in_permitted_protocols, payer,
    powerless, immediate, trapped, national).

% Obtain licensed access to animal subjects through institutional review boards that apply welfare criteria (the 3Rs: replace, reduce, refine) as procedural gatekeeping rather than as a bar on use itself. Compliance with the welfare protocol is what converts otherwise-prohibited harm into lawful research.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, mobile, national).

% Purchase meat, dairy, eggs, and animal-tested goods at prices that do not internalize the animals' foreclosed interests, reassured by welfare labeling (cage-free, humanely raised) that the underlying use is legitimated rather than merely regulated. Can exit individually by changing consumption but bear no structural cost for not doing so.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, national).

% Design and audit welfare standards, earning certification fees and public legitimacy for occupying the space between unrestricted use and abolition. Their continued relevance depends on the instrumental-use structure persisting in some regulated form; full abolition would eliminate their function, and unrestricted property status would eliminate their market.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_certification_bodies, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_certification_bodies, agenda_setter).

% Argue that welfare exemptions legitimate the very harms they claim to limit, but participate in standard-setting processes as advisory, non-binding voices; legislatures and agencies retain final authority to weigh economic interests against animal interests, and rarely rule against economic interests.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_advocacy_organizations, observer).

% Write and enforce the welfare statutes and exemptions (e.g., agricultural exemptions from general anti-cruelty codes) that operationalize the sentience-but-not-rights framework, balancing economic testimony from industry against animal-interest testimony from advocacy groups and consistently codifying industry-favorable thresholds.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, legislatures_and_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legally administrable standard distinguishing permitted animal use from prohibited cruelty, allowing courts, regulators, producers, and consumers to coordinate around a common threshold rather than litigating every use case from first principles.
% TRANSFER_FUNCTION: Moves the cost of foreclosed interest-satisfaction (confinement, pain, curtailed life) from human beneficiaries who consume, research with, or profit from animal use onto the animals themselves, in exchange for procedural welfare protections that regulate the manner rather than the fact of use.
% ABSENT_VOICES: The animals themselves have no direct legal voice or standing to contest the balance struck on their behalf; animal advocacy organizations articulate their interests but hold no binding authority over the exemption structures, and are structurally positioned as commentators on a settlement legislatures and industry actors control.
% DISAPPEARANCE_RATIONALE: If the welfare-reading settlement disappeared overnight, either a property-reading regime (no independent standing at all) or an abolitionist-reading regime (no instrumental use) would have to take its place — animal agriculture, biomedical research licensing, and consumer product labeling would all have to reorganize around whichever replacement kernel-reading prevailed, since none of these institutions currently operate on either extreme.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century anti-cruelty movements sought to establish that animals' capacity to suffer mattered morally and legally, against a background property-reading regime that treated animals as mere objects with no independent moral relevance.
% FOUNDING_PROBLEM_CORROBORATION: Historians of animal law and welfare-science researchers attest the sentience recognition was a genuine and significant break from pure property status. Animal advocacy organizations and some legal scholars, external to the beneficiary industries, attest that the welfare-reading settlement has since been substantially captured by the industries it was meant to constrain, who treat compliance with minimum welfare standards as a license to continue practices the sentience recognition was originally invoked against.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.45 (the UKE_SCOPE hypothesis bin) because the welfare reading's own exemption architecture licenses substantial foreclosure of animal interests — confinement, painful procedures, curtailed lifespan — provided procedural welfare compliance is met; that architecture is real extraction by the reading's own terms, not merely by an abolitionist's external judgment. Suppression sits at 0.5: the animals affected cannot exit or contest their treatment, but the suppression is procedurally mediated (through licensing and standards bodies) rather than raw coercion, which keeps it from the higher end of the scale. Theater ratio rises over the interval (0.20 to 0.42) as welfare certification and labeling schemes proliferate relative to any actual reduction in confinement intensity or procedure severity — a growing share of the visible welfare apparatus is compliance signaling rather than binding constraint on practice. Accessibility collapse is moderate (0.4): once the sentience-with-constrained-use framework is understood, the property-reading alternative becomes hard to defend publicly, but the abolitionist alternative remains a live, actively argued position, so collapse is far from complete. Resistance is moderate-high (0.55): advocacy organizations, some legislators, and a growing consumer segment actively contest the exemption structure, distinguishing this from a settled mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats (industry, certification bodies, legislatures), the welfare reading looks like a genuine coordination achievement — a workable, humane-enough threshold that lets commerce and research proceed under real (if minimal) constraints. From the payer seats (the animals themselves, as represented by advocacy organizations), the same structure looks like the coordination story providing legal cover for the extraction it was meant to limit: welfare compliance becomes the price of continued, expanded use rather than a genuine floor. The engine computing tangled_rope from these structural facts reflects that both a real coordination function (a workable public standard) and real asymmetric extraction (foreclosed animal interests, no animal standing to contest) are simultaneously present and jointly required to sustain the arrangement through active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Industry, research institutions, certification bodies, and consumers are beneficiaries with low derived d: they capture value (product, research access, certification fees, cheap goods) from the constraint's operation and hold mobile or arbitrage-grade exit. Farmed and laboratory animals are victims with high derived d: they are trapped, powerless, and bear the foreclosed-interest cost directly, with no standing to alter the terms. Legislatures sit as the agenda-setting seat whose analytical exit option reflects their role adjudicating rather than bearing the constraint's costs. Advocacy organizations are excluded rather than coordinated or extracted-from directly — their exclusion from binding authority is itself part of the structure the six_questions absent_voices field documents.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading's founding problem — establishing that animal suffering matters morally against a property-reading baseline of zero moral standing — was genuinely live at its founding and remains partially live wherever unregulated cruelty persists. But the founding_problem_status is authored as contested rather than dead or fully live: welfare science and legal historians corroborate the original achievement, while advocacy organizations and critical legal scholars, external to the beneficiary set, corroborate that the settlement has been substantially captured by the industries whose practices it was meant to constrain, who now treat welfare-compliance as license rather than limit. Classifying this as tangled_rope rather than snare preserves the reading's own claim that constrained-but-permitted use is not equivalent to unlimited use (the coordination function is real and distinguishable from property-reading unrestricted use) while still registering that the exemption structure channels substantial, poorly-contested extraction onto a powerless, trapped population — exactly the asymmetric-extraction-alongside-genuine-coordination profile the tangled_rope classification exists to capture, rather than mislabeling either as pure coordination (rope) or pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_compliance_as_license_or_limit,
    'Does statutory welfare compliance function as a genuine floor on permissible harm, or has it been captured into functioning primarily as a license that legitimates the underlying instrumental use?',
    'Longitudinal comparison of confinement intensity, procedure severity, and animal lifespan/welfare-outcome metrics before and after welfare statute adoption in a given jurisdiction, controlling for industry consolidation and production intensification trends.',
    'If compliance functions mainly as license, the tangled_rope classification understates capture and the constraint drifts toward snare; if it functions as a genuine binding floor with measurable harm reduction, the coordination function is stronger than authored and drift toward rope is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_compliance_as_license_or_limit, empirical, 'Whether welfare compliance is a genuine constraint or a legitimating cover for expanded use.').

omega_variable(
    sentience_threshold_ambiguity,
    'Which species and to what degree of cognitive/affective complexity fall within the sentience-recognition threshold this reading relies on, and how contested is that boundary?',
    'Comparative review of statutory sentience definitions across jurisdictions and the underlying comparative-cognition research they cite or ignore (e.g., treatment of fish, invertebrates, insects relative to mammals).',
    'A narrower sentience threshold than assumed would exclude entire categories of currently-used animals from any welfare protection at all, shrinking this reading''s practical victim set and increasing effective extraction on the excluded categories; a broader threshold would expand it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_ambiguity, empirical, 'Uncertainty in where the sentience line is actually drawn across jurisdictions and species.').

omega_variable(
    reading_selection_under_determination,
    'Is the welfare reading a stable, independently-motivated moral position, or a politically negotiated compromise point between the property reading and the abolitionist reading that lacks independent philosophical grounding?',
    'Examine whether welfare-reading legal instruments derive their thresholds from an independent moral theory of animal interests, or whether threshold-setting tracks legislative bargaining outcomes between industry and advocacy pressure with no stable underlying principle.',
    'If the welfare reading is primarily a bargaining equilibrium rather than a principled position, its apparent stability is political rather than moral, and its extractiveness figure is better understood as a negotiated settlement point that could move substantially with shifts in relative lobbying power, rather than as tracking a fixed moral fact about animal interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the welfare reading is a principled moral position or a political equilibrium between the other two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t8, animal_status__welfare_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(anim_tr_t16, animal_status__welfare_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(anim_tr_t24, animal_status__welfare_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(anim_tr_t32, animal_status__welfare_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(anim_be_t8, animal_status__welfare_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(anim_be_t16, animal_status__welfare_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(anim_be_t24, animal_status__welfare_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(anim_be_t32, animal_status__welfare_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anim_su_t8, animal_status__welfare_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(anim_su_t16, animal_status__welfare_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(anim_su_t24, animal_status__welfare_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(anim_su_t32, animal_status__welfare_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_status__welfare_reading, abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the animal_status kernel. property_reading authors the baseline (no independent standing) this welfare reading claims to have moved beyond; abolitionist_reading authors the position this welfare reading structurally forecloses in one direction (any-instrumental-use-is-permissible) while coexisting with in another (whether the current threshold is defensible). All three share the kernel_id animal_status but author independent ε values, beneficiary/victim structures, and claimed types per the ε-invariance principle — none is derived from or averaged with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
