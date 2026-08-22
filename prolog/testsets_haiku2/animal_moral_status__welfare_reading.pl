% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Under Regulated Use (Sentience + Permission Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The welfare reading of the animal moral status kernel asserts that
 *   animals are sentient beings whose capacity to suffer entails moral
 *   obligation to minimize that suffering, but does NOT entail cessation of
 *   animal use. Use is permissible if regulated to reduce suffering; cruelty
 *   is wrong but humane use is ethical. This reading stabilizes a middle
 *   position between the property reading (animals have no independent moral
 *   standing) and the abolitionist reading (property status itself is the
 *   violation). The constraint operates by recognizing animal sentience while
 *   permitting continued use under welfare standards. Beneficiaries include
 *   welfare advocacy organizations (whose authority and funding ride on the
 *   constraint's acceptance of sentience and suffering as morally relevant),
 *   regulated industries (whose operations are legitimated by the permission
 *   to use), and human consumers (who satisfy moral concern without changing
 *   behavior). The victim—sentient animals under use—remains trapped within a
 *   system that recognizes their suffering while remaining organized around
 *   extracting their labor and bodies. The theater ratio rises over time
 *   because 'humane certification' becomes increasingly performative: welfare
 *   standards tighten in messaging while actual conditions for most
 *   industrial animals show limited improvement, suggesting the moral
 *   legitimacy function is decoupling from the functional reduction of
 *   suffering.
 *
 * KEY AGENTS:
 *   - sentient_animals_under_use: powerless, trapped — bear all costs of use; no voice in standards-setting
 *   - welfare_advocacy_organizations: organized, mobile — set enforcement standards; benefit from constraint's continued existence; agenda-setters
 *   - regulated_animal_use_industries: institutional, constrained — extract value; benefit from permission-to-use under welfare license; constrained by standards but not by use itself
 *   - human_consumers: organized, mobile — receive moral permission and product access; benefit from constraint's legitimation
 *   - abolitionist_advocates: moderate, constrained — excluded from beneficiary set; argue the constraint is rhetorical cover for victimization
 *   - regulatory_bodies: institutional, analytical — mediate between beneficiaries; interpret and enforce standards
 *   - property_rights_advocates: institutional (in agriculture), mobile — resist sentience recognition; excluded from wealthy democracies' legitimacy structures but retain institutional power in extraction industries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.52).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.48).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Under Regulated Use (Sentience + Permission Reading)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '2d174e11-7e52-48d2-9157-4ec3a5651812').
narrative_ontology:cs_kernel_codification('2d174e11-7e52-48d2-9157-4ec3a5651812', fixed_text).
narrative_ontology:cs_authority_grounding('2d174e11-7e52-48d2-9157-4ec3a5651812', lineage).
narrative_ontology:cs_interpretation_layer_present('2d174e11-7e52-48d2-9157-4ec3a5651812').
narrative_ontology:cs_reading_relation('2d174e11-7e52-48d2-9157-4ec3a5651812', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d174e11-7e52-48d2-9157-4ec3a5651812', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('2d174e11-7e52-48d2-9157-4ec3a5651812', foundational, animal_sentience_morally_relevant).
narrative_ontology:cs_axiom_status(animal_sentience_morally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('2d174e11-7e52-48d2-9157-4ec3a5651812', animal_sentience_morally_relevant, empirically_contingent).
narrative_ontology:cs_axiom('2d174e11-7e52-48d2-9157-4ec3a5651812', foundational, regulated_use_permissible_with_suffering_minimization).
narrative_ontology:cs_axiom_status(regulated_use_permissible_with_suffering_minimization, holdable).
narrative_ontology:cs_axiom_grounding('2d174e11-7e52-48d2-9157-4ec3a5651812', regulated_use_permissible_with_suffering_minimization, deontological).
narrative_ontology:cs_reference_frame('2d174e11-7e52-48d2-9157-4ec3a5651812', sentience_recognized_use_regulated_framework).
narrative_ontology:cs_drift_state('2d174e11-7e52-48d2-9157-4ec3a5651812', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d174e11-7e52-48d2-9157-4ec3a5651812', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_advocacy_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, human_consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, sentient_animals_under_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of regulated use systems: confinement, transportation, slaughter, separation from young, inability to exercise natural behaviors. The constraint permits use so long as it minimizes suffering, but suffering remains inherent to most regulated systems (dairy, egg production, meat, fiber, experimentation, entertainment). They have no voice in the constraint's formation or adjudication.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, sentient_animals_under_use, payer,
    powerless, immediate, trapped, global).

% Set standards for what counts as 'humane' use, lobby for enforcement mechanisms, certify compliance, and receive funding and legitimacy from this constraint's existence. They benefit from the constraint's recognition of sentience (which validates their advocacy) while also depending on the system of regulated use persisting (eliminating use would eliminate their primary function and funding stream). They are the primary shapers of enforcement and interpretation.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_advocacy_organizations, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_advocacy_organizations, beneficiary).

% Extract value from animal bodies and labor: dairy, meat production, egg production, fiber, pharmaceutical testing, entertainment. The constraint permits their core activity (use) while imposing welfare standards on its execution. They benefit from the sentience recognition because it legitimates regulated use and forestalls the more expensive abolitionist reading (property rights cessation). Compliance costs are lower than alternative regulatory regimes that would restrict use itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_use_industries, beneficiary,
    institutional, generational, constrained, global).

% Access animal products (food, clothing, medicine, entertainment) under a moral and legal framework that permits consumption. The constraint allows them to feel ethically comfortable consuming animal products if certified as 'humanely produced,' reducing cognitive dissonance and social friction. The constraint's recognition of animal sentience plus the permission to use animals allows moral satisfaction without requiring behavioral change.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, human_consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).

% Argue that the sentience recognition is rhetorical cover that legitimates property status and perpetuates victimization. They are structurally excluded from the constraint's beneficiary set because their core claim—that use itself is impermissible—contradicts the constraint's foundational axiom. Their objections are treated as marginal or extremist within the welfare framework's interpretive authority.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% Resist the constraint's recognition of animal sentience and suffering as morally relevant, arguing animals are resources with instrumental value only. They have been largely excluded from the constraint's legitimacy structure in wealthy democracies, though they retain institutional power in agricultural and extractive industries. The constraint's enforcement reduces their operational freedom.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, property_rights_advocates, excluded,
    institutional, generational, mobile, global).

% Interpret and enforce welfare standards, typically by delegating certification to welfare advocacy organizations or industry self-regulation. They mediate between the constraint's competing beneficiaries (welfare advocates want stronger enforcement; industries want looser standards) while maintaining the core framework: use is permitted, cruelty is forbidden.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Provide intellectual framework and legitimacy for the constraint, publishing work on animal sentience, welfare ethics, and the permissibility of use. They inhabit multiple seats: some develop welfare frameworks (supporting the constraint), others develop abolitionist frameworks (contesting it), still others defend property readings (contesting it). Their analytical work shapes which readings are live and which are marginalized.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, philosophers_and_ethicists, observer,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: how to permit continued use of animals for human benefit while acknowledging their sentience and suffering. Without such a coordination, divergent moral frameworks would produce legal chaos (some jurisdictions abolishing use, others denying sentience, others imposing ad-hoc restrictions). The constraint creates a unified framework that recognizes sentience while permitting use, allowing multi-stakeholder coordination around 'humane' practice standards.
% TRANSFER_FUNCTION: Moves three things: (1) moral legitimacy from sentience recognition to regulated use industries, allowing their continued operation under a welfare license; (2) labor and bodily value from animals to human consumers and industries; (3) authority over welfare standards from dispersed actors to organized advocacy organizations and regulatory bodies.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded—their core claim (use itself is wrong) contradicts the constraint's foundational axiom. Property-rights advocates are also largely excluded in wealthy democracies, though they retain institutional power in agriculture. Most importantly, animals themselves have no voice: they cannot consent, cannot adjudicate what 'minimal suffering' means for them, cannot change the standards that govern their confinement and slaughter. The constraint represents human ethical consensus about animals, not animals' own preferences.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if the recognition of animal sentience and the permission to use animals within welfare bounds evaporated—the world would reorganize along one of two trajectories: either toward the abolitionist reading (property status abolished, use restricted or forbidden), or toward the property reading (sentience denied or rendered legally irrelevant, use permitted with no welfare restrictions). The current constraint is a stabilized middle position; its disappearance would destabilize that equilibrium and force realignment toward one of the sibling readings.
% FOUNDING_PROBLEM: Early industrialization and intensification of animal agriculture created visible cruelty—mass confinement, transportation suffering, painful slaughter methods—that collided with emerging consciousness of animal suffering. The moral problem: how to sustain the benefits of animal agriculture and use while acknowledging that animals suffer and their suffering matters morally? The founding problem was the need to legitimize continued use in a world where sentience could no longer be credibly denied.
% FOUNDING_PROBLEM_CORROBORATION: Welfare advocates and regulatory bodies attest the founding problem remains live: industrial animal use creates suffering, and welfare standards are necessary to manage it. Philosophers and ethicists outside the beneficiary set document the historical rise of sentience recognition and the consequent need for frameworks that permit use while acknowledging moral obligation. However, abolitionist advocates and some independent ethicists attest that the founding problem has been mis-stated: the real problem is not 'how to make use humane' but 'whether use is permissible at all'—and that this constraint's framing forecloses the deeper moral question rather than resolving it. No corroborating source from outside the beneficiary set endorses the founding problem as stated by welfare advocates.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.52 reflects the constraint's dual character: it is lower than a pure snare because genuine coordination exists (the constraint does solve a real collective-action problem around welfare standards), but substantially higher than rope because the permission-to-use structure asymmetrically benefits industries and consumers while animals remain trapped victims. Suppression is moderate (0.48) because the constraint relies on internalized moral satisfaction—consumers believe humane use is ethical—rather than on external force alone. The rising theater ratio (0.15→0.61) is diagnostic: over 56 years, certifications and standards proliferated while actual confinement and slaughter conditions for most animals show limited functional improvement. This suggests the constraint's function has drifted from minimizing animal suffering to legitimating use through moral reassurance. Accessibility collapse is moderate (0.64) because alternatives to animal use exist (plant-based products, cellular agriculture) but are constrained by cost, marketing inertia, and the constraint's own legitimation of use. Resistance is high (0.71) because the abolitionist and property-rights readings continue to challenge the welfare frame; the constraint must actively suppress these alternatives through interpretive authority. The built-in asymmetry: animals are recognized as sentient (a constraint input) but denied any adjudicative role in determining what 'minimal suffering' means for them (a structural exclusion).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (welfare organizations) and beneficiary (industries) seats should compute as rope or weak tangled-rope: they see genuine coordination, agreed standards, improved conditions relative to pre-constraint. The payer seat (animals) computes as snare or severe tangled-rope: they receive recognition of sentience without operational power, permission-for-use without consent, and suffering minimization that does not eliminate suffering. The excluded seats (abolitionists, property advocates) compute the constraint as false-summit rope: appearance of ethical legitimacy masking continued victimization. The observer seat (regulators, ethicists) should compute near the engine's classification because they see the structural asymmetry without depending on the constraint for identity or income.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is the engine's computation, but the structural reasoning: welfare organizations and industries are beneficiaries with d near 0.2-0.3 (they benefit materially and reputationally; they set enforcement standards; they have arbitrage-grade exit—they could pivot to unregulated use if standards tightened further). Consumers sit near d=0.5 (symmetric: they receive moral permission but also carry diffuse cost through certification prices). Animals are victims with d near 0.9 (trapped, powerless, identity-locked to their biological category, no exit, no voice in standards). Abolitionist advocates sit outside the constraint (excluded, not in the beneficiary/payer division) but would have d near 1.0 if included because they would need to oppose the constraint structurally. The asymmetry is structural, not a matter of perspective: animals' confinement, transportation, slaughter, and separation are not discretionary costs they could reduce through exit or negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading carries a nascent mandatrophy signature: the founding problem (industrial animal agriculture's cruelty) was live in 1970s; welfare standards were built to solve it. By 2026, the founding problem's status is contested. Abolitionist and property-rights advocates argue it was mis-stated: the real problem is not cruelty-within-use but use-itself. The constraint's response has been to amplify theater (certifications, labels, standards-expansion in messaging) without proportional functional improvement in animal conditions. This is consistent with mandatrophy drift: the constraint persists because beneficiaries (industries, consumers, welfare organizations) benefit more from its continuation than from its dissolution, even if the founding problem is partially or largely dead. The recognition of sentience is real and has not been reversed; the extraction has not ceased and has arguably intensified (factory farming has expanded globally while welfare certification has expanded in wealthy democracies, creating a two-tier system). Mandatrophy_resolved is not yet applicable because the constraint's core elements remain defended—the permission-to-use is still enforced, welfare standards are still actively promulgated—but the theater trajectory and the contested founding-problem status suggest mandatrophy pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_recognition_vs_use_permission,
    'Is the recognition of animal sentience and moral standing compatible with the permission to use animals for human benefit, or does sentience entail a duty to cease use?',
    'Philosophical analysis (from outside the beneficiary set) of whether sentience + suffering entail use-prohibition, or whether sentience + welfare optimization is a coherent middle position. Historical/ cross-cultural comparison of societies that recognize sentience but permit use vs. those that restrict use.',
    'If sentience entails use-prohibition, the welfare reading is internally incoherent and forecloses toward the abolitionist reading. If sentience permits regulated use, the welfare reading is structurally sound and resists foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_recognition_vs_use_permission, conceptual, 'Whether sentience recognition and use permission are conceptually compatible or contradictory.').

omega_variable(
    functional_improvement_vs_theater_drift,
    'Does the rising theater ratio reflect genuine functional improvement in animal welfare that is simply under-reported, or does it reflect decoupling of moral messaging from animal-condition outcomes?',
    'Empirical comparison: industrial animal confinement metrics (stocking density, ability to express natural behavior, mortality rates, slaughter method injury rates) vs. certification-standard proliferation and marketing messaging. Multi-year tracking of conditions in certified vs. uncertified systems.',
    'If functional, the constraint is performing its coordination role despite increased performance messaging. If theater-dominated, the constraint has drifted toward mandatrophy: beneficiaries maintain legitimacy through messaging while actual animal conditions plateau or worsen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_improvement_vs_theater_drift, empirical, 'Whether welfare improvements track theater growth or diverge.').

omega_variable(
    animal_exclusion_from_adjudication,
    'Is the structural exclusion of animals from standard-setting and consent irreducible to the constraint''s operation, or could animals'' preferences/indicators be incorporated into welfare standard-adjudication?',
    'Pilot integration of animal behavior, physiological stress, and preference-testing data into welfare standards. Comparison of outcomes when animal-condition data is made primary (rather than subordinate to cost/industry input) in standard-setting.',
    'If irreducible, the constraint is built on a category exclusion (animals cannot speak/negotiate) that makes their victim status permanent. If incorporable, the constraint could be reformed to give animals indirect voice through their condition data, which would alter the directionality and potentially raise accessibility_collapse (alternatives to animal use would become more viable if use had to genuinely minimize suffering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_exclusion_from_adjudication, empirical, 'Whether animal input to welfare standards is structurally possible or categorically foreclosed.').

omega_variable(
    constraint_sibling_foreclosure,
    'Does this reading''s axiom—that animal sentience plus regulated use is the ethical framework—logically foreclose the abolitionist reading, or do the readings coexist as live positions?',
    'Philosophical analysis: if property is abolished (abolitionist premise), can regulated-use-with-sentience-recognition remain coherent? If use is prohibited (abolitionist premise), is the welfare permission-to-use internally contradictory?',
    'Forecloses: this reading logically excludes the abolitionist reading; only one can be true. Coexists: the readings represent different normative commitments; both remain defensible. High impact for engine classification of reader divergence and for constitutional authority (if one reading forecloses the other, the constraint carries a winner-take-all structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_sibling_foreclosure, conceptual, 'Whether this reading forecloses the abolitionist reading or coexists with it.').

omega_variable(
    beneficiary_identity_fusion,
    'Is the constraint''s persistence maintained by welfare organizations'' and industries'' identity fusion with the regulated-use framework (they would need to repudiate their own function to end use), or by genuine ongoing demand for the coordination function?',
    'Historical/counterfactual: if the founding problem (cruelty in unregulated use) were solved by a transition to plant-based alternatives, would beneficiary organizations agitate for continued animal use? If not, identity-fusion is primary.',
    'If identity-fusion is high, the constraint approaches piton (inertial persistence without core function). If genuine coordination demand is primary, the constraint retains rope/tangled-rope character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, conceptual, 'Whether beneficiary commitment to regulated use reflects identity-fusion or functional necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(anim_tr_t1970, projected).
narrative_ontology:measurement(anim_tr_t1985, animal_moral_status__welfare_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(anim_tr_t1985, observed).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(anim_tr_t2000, observed).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__welfare_reading, theater_ratio, 2010, 0.54).
narrative_ontology:measurement_basis(anim_tr_t2010, observed).
narrative_ontology:measurement(anim_tr_t2018, animal_moral_status__welfare_reading, theater_ratio, 2018, 0.58).
narrative_ontology:measurement_basis(anim_tr_t2018, observed).
narrative_ontology:measurement(anim_tr_t2026, animal_moral_status__welfare_reading, theater_ratio, 2026, 0.61).
narrative_ontology:measurement_basis(anim_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.85).
narrative_ontology:measurement_basis(anim_be_t1970, projected).
narrative_ontology:measurement(anim_be_t1985, animal_moral_status__welfare_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement_basis(anim_be_t1985, observed).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(anim_be_t2000, observed).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__welfare_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement_basis(anim_be_t2010, observed).
narrative_ontology:measurement(anim_be_t2018, animal_moral_status__welfare_reading, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement_basis(anim_be_t2018, observed).
narrative_ontology:measurement(anim_be_t2026, animal_moral_status__welfare_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(anim_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement_basis(anim_su_t1970, projected).
narrative_ontology:measurement(anim_su_t1985, animal_moral_status__welfare_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement_basis(anim_su_t1985, observed).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(anim_su_t2000, observed).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__welfare_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement_basis(anim_su_t2010, observed).
narrative_ontology:measurement(anim_su_t2018, animal_moral_status__welfare_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement_basis(anim_su_t2018, observed).
narrative_ontology:measurement(anim_su_t2026, animal_moral_status__welfare_reading, suppression_requirement, 2026, 0.48).
narrative_ontology:measurement_basis(anim_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.18).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel admits three distinct constraint readings. This story instantiates the welfare_reading: animals are sentient beings whose suffering should be minimized within regulated use. The property_reading denies independent moral standing to animals; the abolitionist_reading denies the permissibility of use itself. Each reading has distinct ε values, beneficiary/victim structures, and claimed types. They are linked as sibling readings of a contested kernel. The welfare reading coexists with both siblings as a live position in contemporary discourse; it influences the abolitionist reading (by establishing sentience recognition as a baseline that abolitionists must now work within) and coexists with the property reading (held by different institutional actors and jurisdictions). The ε-invariance principle: these are not three perspectives on one constraint; they are three different constraints grounded in different structural premises about animals' moral standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
