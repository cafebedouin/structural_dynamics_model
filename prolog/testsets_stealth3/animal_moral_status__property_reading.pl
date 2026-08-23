% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Animal Moral Status — Property Reading: Legal Chattel Classification
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   A legal-ontological classification: within the operating tradition of
 *   most modern jurisdictions, animals are property — stock, inventory,
 *   resources — with no independent moral standing, their interests counting
 *   only derivatively, as components of owner value or public sentiment. The
 *   arrangement this story is about is that standing classification as it
 *   governs the instrumental-use economy (livestock production, research
 *   colonies, companion-animal keeping). Its own tradition presents it as
 *   definitional rather than enacted ('subordinate by definition'); it is
 *   nonetheless written into statute books, adjudicated through
 *   chattel-inherited templates, shielded by interference statutes, and
 *   enormously valuable to identifiable holder classes. This file
 *   instantiates the property reading of the animal_moral_status kernel (see
 *   kernel_context); its epsilon is authored by that reading's own lights
 *   over the standing arrangement, yielding a very low value and an empty
 *   victim set by construction. The sibling files in the constraint family
 *   author different epsilon over the same referent; nothing here averages
 *   across them.
 *
 * KEY AGENTS:
 *   - - livestock_agribusiness: primary beneficiary (institutional/arbitrage) — holds the largest mass of titled animal assets; the classification converts biological stock into capital equipment
 *   - - biomedical_research_institutions: secondary beneficiary (institutional/arbitrage) — owns research colonies as depreciable, transferable assets
 *   - - animal_owning_households: diffuse beneficiary (moderate/mobile) — possession security and discretionary control over companion animals
 *   - - state_legal_apparatus: agenda setter/administrator (institutional/identity_locked) — writes, interprets, and enforces the chattel template across statutes, courts, and registries
 *   - - animal_advocacy_movements: excluded challenger (organized/identity_locked) — supplies the classification's principal resistance; its outputs are admitted only as property crimes or security concerns
 *   - - moral_philosophy_community: analytical observer (analytical/analytical) — traces the argument lineage and produces the conceptual alternatives the legal order declines to enact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.08).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.58).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Moral Status — Property Reading: Legal Chattel Classification").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__property_reading).
domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '724522ee-f1cf-41a6-a3ef-f1152848ed85').
narrative_ontology:cs_kernel_codification('724522ee-f1cf-41a6-a3ef-f1152848ed85', formalized).
narrative_ontology:cs_authority_grounding('724522ee-f1cf-41a6-a3ef-f1152848ed85', lineage).
narrative_ontology:cs_interpretation_layer_present('724522ee-f1cf-41a6-a3ef-f1152848ed85').
narrative_ontology:cs_reading_relation('724522ee-f1cf-41a6-a3ef-f1152848ed85', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('724522ee-f1cf-41a6-a3ef-f1152848ed85', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('724522ee-f1cf-41a6-a3ef-f1152848ed85', foundational, standing_requires_human_grade_rational_agency).
narrative_ontology:cs_axiom_status(standing_requires_human_grade_rational_agency, holdable).
narrative_ontology:cs_axiom_grounding('724522ee-f1cf-41a6-a3ef-f1152848ed85', standing_requires_human_grade_rational_agency, deontological).
narrative_ontology:cs_axiom('724522ee-f1cf-41a6-a3ef-f1152848ed85', secondary, animals_classifiable_as_chattel).
narrative_ontology:cs_axiom_status(animals_classifiable_as_chattel, holdable).
narrative_ontology:cs_axiom_grounding('724522ee-f1cf-41a6-a3ef-f1152848ed85', animals_classifiable_as_chattel, conventional).
narrative_ontology:cs_reference_frame('724522ee-f1cf-41a6-a3ef-f1152848ed85', property_ontology_of_animals).
narrative_ontology:cs_drift_state('724522ee-f1cf-41a6-a3ef-f1152848ed85', contemporary_sentience_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('724522ee-f1cf-41a6-a3ef-f1152848ed85', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, livestock_agribusiness).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_owning_households).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, rational_agency_moral_standing_criterion).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, chattel_incorporation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the largest mass of titled animal assets. Breeds, houses, transports, and processes animals whose legal standing is inventory: the classification lets it collateralize herds, insure stock, schedule depreciation, and liquidate biological capital like any other equipment. Its recognized costs are limited to disease losses and efficiency drag, and it can shift production to permissive jurisdictions when local rules bind.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, livestock_agribusiness, beneficiary,
    institutional, generational, arbitrage, global).

% Holds title to research colonies and moves animals between facilities as owned assets. Title confers protocol ownership, per-head accounting, and transfer rights; the costs it acknowledges are regulatory friction and replacement-stock expense. Studies can be relocated to jurisdictions with weaker oversight when domestic conditions tighten.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Hold title to companion animals. The classification secures possession (stray reclamation, anti-theft protection), grants discretionary veterinary and end-of-life decisions, and imposes no guardianship obligations. Benefits are small per household but broadly spread; relinquishing ownership is individually cheap, so attachment to the frame is voluntary rather than forced.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_owning_households, beneficiary,
    moderate, biographical, mobile, national).

% Writes and enforces the statutory categories — livestock statutes, companion-animal ownership law, theft definitions, interference and trespass provisions — and adjudicates disputes through property-law templates inherited from centuries of chattel jurisprudence. Registries, inspection regimes, and court dockets are staffed and structured around the template; treating animals under any other category would require rebuilding doctrines that constitute the apparatus's own transmitted authority.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).

% Campaign to reclassify animals as subjects: litigation, ballot initiatives, undercover investigation, open rescue. The frame admits their outputs only as property crimes — theft, vandalism, trespass — or as security concerns, never as claims about a patient's own standing. Members' commitments are identity-fused with the cause, so disengagement carries self-conceptual cost; they stand outside the frame's accounting while supplying its main organized opposition.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_advocacy_movements, excluded,
    organized, generational, identity_locked, global).

% Examines the classification from every available seat, tracing the argument lineage from ancient and Enlightenment sources through contemporary animal ethics, and producing the conceptual alternatives the legal order declines to enact. Holds no stake in the frame's revenues, enforcement, or legitimacy.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, moral_philosophy_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, livestock_agribusiness).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unambiguous, enforceable ownership and transfer boundaries over living beings used instrumentally — solving once, centrally, the problems of trade, collateralization, insurance, veterinary responsibility assignment, and liability allocation for creatures that act on their own initiative.
% TRANSFER_FUNCTION: Consolidates exclusive disposition rights over animal bodies, reproduction, labor, and products in title-holders; routes the entire surplus those bodies generate — work, offspring, milk, eggs, flesh, experimental data — to the holder, with no channel by which any portion accrues to or on behalf of the animal itself; circulates animals as capitalized assets through sale, lease, inheritance, and insurance markets among holders.
% ABSENT_VOICES: The animals whose status the classification fixes are the paradigmatic absent voices — unseatable by construction, since the reading's ontology assigns them object status and therefore no speaking part in any proceeding conducted about them. Beside them stand the advocacy movements (authored as excluded stakeholders), audible only when reframed as property crimes or security threats. Both absences explain the unanimity of the frame's internal consensus: it arises because no dissenting seat is admissible inside it.
% DISAPPEARANCE_RATIONALE: Overnight removal of the property classification would leave tens of billions of titled animals in legal limbo: ownership claims unenforceable, herd collateral and insurance void, laboratory colonies unownable, veterinary liability frameworks inoperative, and meat, dairy, and research supply chains stalling within days. The human economy organized around titled animals would rearrange immediately and violently.
% FOUNDING_PROBLEM: Legal systems faced the problem of incorporating beings that move, breed, labor, and die on their own into schemes of alienable wealth and assignable responsibility: how to buy, sell, bequeath, insure, borrow against, and impose liability concerning creatures that are not persons. The chattel/property classification was the founding solution, inherited from ancient and medieval stock law and refined through centuries of jurisprudence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legal-historical scholarship on the chattel treatment of animals in Roman, medieval English, and early American law traces the category to exactly this incorporation problem; comparative jurisprudence shows every surveyed legal system maintaining some incorporation category for animals; and advocacy-side legal scholars concede the administrative problem while disputing the property solution — a three-way attestation requiring no beneficiary testimony.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored very low (0.08) and reading-indexed: the referent is the standing instrumental-use arrangement, and this reading's own ontology seats no moral patient in it — animals are objects, so the arrangement registers no extraction from any subject; the only costs the frame itself counts (waste, disease loss, efficiency drag) fall on the holder seats, leaving residual extraction near the coordination floor for the declared coordination type. Suppression (0.58) is a raw, unscaled structural property: the frame forecloses subject-status claims inside legal practice, criminalizes interference with titled animals, and is partly internalized as common sense — only extractiveness is scaled downstream by directionality and scope. Accessibility collapse (0.60): once the chattel template is understood, subject-status alternatives collapse almost entirely within formal legal dealings while remaining vivid outside them. Resistance (0.62): incumbency has not bought quiescence — organized advocacy, litigation, and direct action contest the frame continuously. Theater (0.24, rising on a single shared time grid across all tracked metrics): the frame's functional load is heavy and real, but a growing share of its maintenance is defensive performance invoked against challengers. The claim is authored independently of these metrics: mountain, because the reading presents subordination as definitional law rather than enacted policy; the metrics describe enforced, contested, benefiting operation — that divergence is the false-summit signal this story exists to measure, not an inconsistency to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. Holder seats sit near the beneficiary pole: the classification subsidizes them (asset convertibility, liability shielding, collateralization), so effective extraction damps toward subsidy and their computed positions trend benign. The state_legal_apparatus seat is locked in the institutional sense — courts and registries have become the chattel template they administer; exit would require dismantling doctrines that constitute their own authority, a professional-and-institutional identity fusion, so that seat's position tracks the frame's stability rather than its fairness. The excluded challenger seat experiences the same structure as a foreclosure device: its claims are grammatically inadmissible inside the frame, which lands on it as suppression aimed at a rival project, not as a price charged to a participant. If the legal apparatus's identity frame broke — if a jurisdiction adopted subject-status categories wholesale — the apparatus seat would flip from anchor to exposed target overnight. Note also what the seat list cannot contain: the reading's ontology assigns the principal affected population object status, hence no seat — which is why this file's victim set is empty by construction and why epsilon here is reading-indexed rather than topic-indexed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d low for the three holder seats; the agenda-setter seat derives a mid-low d, absorbing legitimacy and continuity rents rather than commodity rents. The excluded seat carries no beneficiary or victim declaration, so its d falls to the canonical fallback — appropriate, since the frame neither pays nor subsidizes it; what the frame imposes on challengers is suppression, which enters the arithmetic unscaled and separate from effective extraction. Spatial scope is effectively global across the major holder seats, which mildly amplifies whatever extraction the engine computes — but with epsilon at 0.08 the amplification operates on a near-floor base. No directionality_overrides are authored: the derivation chain reproduces the structural picture without correction, and the story's interesting asymmetries live in foreclosure and enforcement, not in the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — incorporating self-moving, breeding, laboring beings into schemes of alienable wealth and assignable legal responsibility — is live: any legal order needs some incorporation category, and comparative jurisprudence shows every surveyed system maintaining one. Nothing here is mandatrophy-resolved; there is no sunset and no atrophied remnant performing a dead function. The classification risk runs in both directions: a pure-extraction reading of the arrangement would mislabel a functioning coordination substrate (that reading belongs to the abolitionist sibling file, where the victim set is populated and epsilon is high), while the mountain claim risks the opposite error — laundering an enforced, benefiting construct as natural law. The false-summit path exists precisely to catch the latter; this story declares the claim and the beneficiary structure honestly so the engine, not the author, settles which is true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_status,
    'Is the animal-as-property classification a definitional, natural-law-like fixture of the legal order, or a constructed convention sustained by identifiable beneficiaries and active enforcement machinery?',
    'Comparative legal history and persistence testing: whether the classification varies across jurisdictions without physical impossibility (guardianship ordinances, sentience clauses), and whether it survives withdrawal of defense or enforcement rather than only the presence of defenders.',
    'Resolves the mountain claim against a constructed-constraint reclassification; a constructed finding supports the false-summit signature path and migration toward a coordination/extraction hybrid verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_status, empirical, 'Whether the property status of animals is natural law or enforced construct.').

omega_variable(
    kernel_reading_position,
    'This file instantiates the property_reading of the kernel animal_moral_status; the sibling readings welfare_reading (sentience with regulated use) and abolitionist_reading (rights-bearing individuals, property status itself the violation) instantiate different constraints over the same standing arrangement — what structurally separates them, and where is the disagreement located?',
    'Adoption of a different ontological axiom by the governing authority (legislatures, courts): whichever category the operative legal order assigns to animals determines which sibling constraint is live in that jurisdiction.',
    'The separation between readings is located in exactly one element — the ontological category assigned to animals — which determines the victim set and therefore epsilon. If a sibling axiom is adopted, this file''s referent is re-read under the new ontology: animals enter the victim set, epsilon jumps, and classification migrates toward snare/tangled_rope; this story''s very low epsilon is reading-indexed, not a property of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the assigned ontological category.').

omega_variable(
    axiom_dependent_victim_boundary,
    'Is this reading''s empty victim set a structural fact of the standing arrangement, or an artifact of the reading''s own definitional axiom — a frame that defines its potential patients out of the accounting by fiat?',
    'Test the axiom''s elasticity: observe whether the property reading can absorb accumulating sentience evidence without moving the victim boundary, using jurisdictions where sentience-recognition clauses coexist with unchanged property status as natural experiments.',
    'If the boundary is axiom-artifact, the very low epsilon is contingent and the false-summit reclassification is the stable verdict; if the boundary is structural, the mountain certification chain retains standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_dependent_victim_boundary, conceptual, 'Whether the empty victim set reflects the arrangement or the reading''s definitional move.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression carried by external legal barriers (foreclosed subject-status claims, criminalized interference with titled animals) or by the culturally absorbed property frame that persists as common sense?',
    'Post-reform compliance trajectory in jurisdictions that adopt guardianship language or sentience clauses without changing enforcement intensity: if the old frame''s behavioral patterns persist after the barrier is removed, the internalized share is substantial.',
    'An internalized component raises effective suppression above the structural measure and would survive legal reform, shaping any transition-path analysis toward the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between legal foreclosure and absorbed cultural frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t6, animal_moral_status__property_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement_basis(anim_tr_t6, observed).
narrative_ontology:measurement(anim_tr_t12, animal_moral_status__property_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(anim_tr_t12, observed).
narrative_ontology:measurement(anim_tr_t18, animal_moral_status__property_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement_basis(anim_tr_t18, observed).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__property_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(anim_tr_t24, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__property_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(anim_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t6, animal_moral_status__property_reading, base_extractiveness, 6, 0.05).
narrative_ontology:measurement_basis(anim_be_t6, observed).
narrative_ontology:measurement(anim_be_t12, animal_moral_status__property_reading, base_extractiveness, 12, 0.06).
narrative_ontology:measurement_basis(anim_be_t12, observed).
narrative_ontology:measurement(anim_be_t18, animal_moral_status__property_reading, base_extractiveness, 18, 0.06).
narrative_ontology:measurement_basis(anim_be_t18, observed).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__property_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement_basis(anim_be_t24, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__property_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(anim_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t6, animal_moral_status__property_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement_basis(anim_su_t6, observed).
narrative_ontology:measurement(anim_su_t12, animal_moral_status__property_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(anim_su_t12, observed).
narrative_ontology:measurement(anim_su_t18, animal_moral_status__property_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(anim_su_t18, observed).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__property_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(anim_su_t24, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__property_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(anim_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% One kernel, three constraints: the colloquial label 'the moral status of animals' decomposes into three epsilon-invariant readings. This file (property_reading) authors epsilon over the standing instrumental-use arrangement by the reading's own lights — animals are objects, the victim set is empty, epsilon is very low. The welfare sibling authors intermediate epsilon (sentience weighted, use retained); the abolitionist sibling authors high epsilon (property status itself counted as the violation, animals seated as victims). Upstream/downstream: this reading is the incumbent baseline structuring the legal environment both siblings operate against — welfare protections layer onto chattel law without removing it, and abolitionist litigation attacks the chattel template directly. Family links run through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
