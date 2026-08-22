% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Sentience with Welfare Exemption (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status holds that animals are sentient
 *   beings with morally considerable interests, but this sentience constrains
 *   rather than prohibits their instrumental use by humans. Animals in
 *   agricultural, pharmaceutical, and entertainment systems are recognized as
 *   subjects of their own experiences (pain, fear, confinement stress) whose
 *   interests demand welfare protections—but the reading does not extend
 *   sentience-based recognition to a prohibition on use itself. The use
 *   persists, enforced by industries and certification bodies; what changes
 *   is that it must proceed with welfare safeguards (space, nutrition,
 *   veterinary care) that acknowledge the animal's sentience. This is
 *   structurally distinct from property reading (animals as unfeeling
 *   objects, use unrestricted except by statute) and from abolitionist
 *   reading (animals as rights-holders, all instrumental use prohibited). The
 *   welfare reading is a tangled rope: it coordinates genuine recognition of
 *   sentience with actual continuance of extraction, requiring active
 *   enforcement to suppress abolitionist challenges and to suppress the
 *   logical tension between 'sentient subject' and 'permissible use.'
 *
 * KEY AGENTS:
 *   - animals_in_captive_systems: bearers of the constraint's costs; sentience recognized, use permitted, escape impossible
 *   - instrumental_use_industries: set and enforce welfare standards within a use-permitting frame; benefit from continued access to animal labor/materials while bearing welfare compliance costs
 *   - human_consumers: benefit from cheap animal products; exit options are real but moderate-friction (veganism, synthetic alternatives)
 *   - welfare_certification_bodies: derive authority and revenue from the boundary between permissible-use-with-welfare and prohibited gratuitous-harm
 *   - abolitionist_movements: excluded from agenda-setting; would redraw the victim set to cover all instrumental use, not only gratuitous harm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Sentience with Welfare Exemption (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'b4f65015-ae89-4b56-bf55-9be6181fe0e1').
narrative_ontology:cs_kernel_codification('b4f65015-ae89-4b56-bf55-9be6181fe0e1', formalized).
narrative_ontology:cs_authority_grounding('b4f65015-ae89-4b56-bf55-9be6181fe0e1', extraction).
narrative_ontology:cs_interpretation_layer_present('b4f65015-ae89-4b56-bf55-9be6181fe0e1').
narrative_ontology:cs_reading_relation('b4f65015-ae89-4b56-bf55-9be6181fe0e1', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4f65015-ae89-4b56-bf55-9be6181fe0e1', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('b4f65015-ae89-4b56-bf55-9be6181fe0e1', foundational, sentience_constrains_use).
narrative_ontology:cs_axiom_status(sentience_constrains_use, holdable).
narrative_ontology:cs_axiom_grounding('b4f65015-ae89-4b56-bf55-9be6181fe0e1', sentience_constrains_use, empirically_contingent).
narrative_ontology:cs_axiom('b4f65015-ae89-4b56-bf55-9be6181fe0e1', foundational, instrumental_use_permissible_with_welfare).
narrative_ontology:cs_axiom_status(instrumental_use_permissible_with_welfare, holdable).
narrative_ontology:cs_axiom_grounding('b4f65015-ae89-4b56-bf55-9be6181fe0e1', instrumental_use_permissible_with_welfare, deontological).
narrative_ontology:cs_reference_frame('b4f65015-ae89-4b56-bf55-9be6181fe0e1', sentience_recognized_use_permitted).
narrative_ontology:cs_drift_state('b4f65015-ae89-4b56-bf55-9be6181fe0e1', contemporary_enhanced_sentience_science, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b4f65015-ae89-4b56-bf55-9be6181fe0e1', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, instrumental_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_consumers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_in_captive_systems).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, welfare_amelioration_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Confined in agricultural, research, or entertainment systems. Under the welfare reading, their sentience is recognized — they have interests in avoiding pain, stress, and deprivation — but recognition stops at welfare standards, not exit. They bear the costs of use (confinement, slaughter, experimentation) in exchange for mandated welfare minimums (space, food, veterinary care). Their structural position is that of beings whose interests constrain the terms of their use but not its existence.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_in_captive_systems, payer,
    powerless, biographical, trapped, global).

% Agriculture, pharmaceutical research, food production, entertainment. They set the operational rules for animal confinement and use within welfare-statute boundaries. They defend the arrangement as a pragmatic equilibrium: animals' sentience is recognized in law, but recognition does not prohibit use — only regulates its cruelty. They benefit from continued access to animal labor and materials while bearing compliance costs (welfare infrastructure, veterinary standards, reporting). Their constraint is enforcement of welfare statutes, not prohibition of use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, instrumental_use_industries, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, instrumental_use_industries, beneficiary).

% Access animal-derived products (food, medicines, materials) at prices calibrated to industrial-scale use. Under the welfare reading, they benefit from the arrangement's framing as ethically managed: animals' interests are respected via welfare standards, not prohibited from use. They can exit (veganism, synthetic alternatives) at moderate friction, but the arrangement's persistence depends on their continued consumption of industry outputs at subsidized cost relative to abolitionist alternatives.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, human_consumers, beneficiary,
    organized, biographical, mobile, global).

% NGOs, government agencies, private auditors that certify welfare compliance. They benefit from the existence of welfare standards (funding, authority, market differentiation) and set the terms of what counts as adequate welfare. They maintain the boundary between permissible use (with welfare safeguards) and impermissible gratuitous harm — a boundary that preserves both the industries they audit and their own institutional role. Their interests align with the constraint's persistence because its abolition would eliminate their function.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_certification_bodies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_certification_bodies, agenda_setter).

% Organized advocates for animal rights and the prohibition of instrumental use. They argue the welfare reading is a cover story — that sentience entails rights, not regulated servitude, and that welfare standards legitimize and stabilize the very use they seek to end. They are excluded from agenda-setting; their presence in law and discourse is limited to testimony and advocacy outside the operational framework. They would argue animals should not be in the victim set for any instrumental use, only for gratuitous harm.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_movements, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, instrumental_use_industries).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for recognizing animals' sentient interests while preserving human access to animal-derived resources: sentience demands welfare accommodation, but does not prohibit instrumental use. Coordinates across industries, consumers, and advocates around a compromise definition of ethical use—use that respects sentience within permissible bounds rather than abolishing use entirely.
% TRANSFER_FUNCTION: Transfers the cost of welfare compliance (confinement infrastructure, veterinary care, monitoring) to animals and industries, while transferring the benefit of continued cheap access to animal products to consumers and industries. Certification bodies receive institutional authority and revenue. Animals' interests are transferred into regulatory categories (space, food, pain standards) that constrain but do not prohibit use.
% ABSENT_VOICES: Abolitionist movements are structurally excluded from agenda-setting; they argue the welfare reading is itself a harm to animals (legitimizing servitude) and would insist animals not be in the 'victim set for use with protections' at all — they would argue for a victim set covering all instrumental use, not merely gratuitous harm. Their absence from the operational framework is maintained by the constraint itself.
% DISAPPEARANCE_RATIONALE: If this welfare-exemption reading vanished overnight and were replaced by abolitionist prohibition, industries would face capital reallocation (converting animal agriculture to crop agriculture, shifting pharmaceutical research to in vitro methods, eliminating certain entertainment sectors). If replaced by property reading (animals as unrestricted objects), welfare regulations would evaporate and industries would consolidate further. The constraint's specific framing — sentience-recognizing-but-use-permitting — shapes the economic, legal, and ethical landscape; its disappearance would force rearrangement across all three axes.
% FOUNDING_PROBLEM: Early industrial and laboratory use of animals proceeded with no legal recognition of their sentience, causing widespread harm that violated emerging ethical intuitions about pain and cognition. The welfare reading was built to solve the problem of unacknowledged animal suffering while preserving the economic systems dependent on animal use—to make use compatible with ethical conscience.
% FOUNDING_PROBLEM_CORROBORATION: Industries and welfare-certification bodies attest the founding problem (unregulated, consequence-indifferent use) is solved by welfare standards. Abolitionist movements and animal-cognition researchers contest the claim: they attest that the founding problem was not solved but relocated — welfare standards have legitimized and stabilized the use systems rather than reducing harm at scale. Independent empirical work (welfare science literature showing that standards are unevenly enforced, that sentience science outpaces welfare law, that certified-humane meat still requires mass slaughter) corroborates the contestation.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness score (0.45) reflects the constraint's core asymmetry: animals' sentience is vindicated (recognized, enforced in law), but this recognition does NOT extend to exit — use continues under welfare terms. The constraint extracts from animals at a moderate level because it does not claim they have no interests (low extraction would be property reading) nor does it prohibit use (zero extraction would be abolitionist reading). The 0.45 sits between extremes. Suppression is higher (0.62) because the constraint's persistence depends on actively excluding the abolitionist alternative — welfare standards serve dual purposes: they genuinely reduce some harms AND they stabilize the use system against abolition. The theater_ratio (0.48) is moderate, indicating that roughly equal shares of enforcement activity are functional (actual welfare oversight) and performative (certification theater that legitimizes use without proportionate harm reduction). The measurement trajectory shows extractiveness plateauing after t=20, indicating the reading has reached a stable maturity: sentience science continues to advance, but welfare law has stabilized around a settled compromise position. Suppression rises slightly then plateaus, suggesting enforcement infrastructure matured early and remains constant. Theater rises and plateaus, indicating increasing bureaucratization of welfare certification without functional deepening—a piton signature beginning to emerge at the edges (captured by the theta_ratio climb and plateau, not by type reclassification).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (industries + certification bodies), the constraint is a workable compromise: sentience is recognized and welfare is enforced, which satisfies ethical demands while preserving use. From the victim seat (animals in captive systems), the constraint is a enforced servitude: recognition of sentience without recognition of the right to exit produces a coordination that benefits from their acknowledgment while denying its logical conclusion. From the excluded seat (abolitionists), the constraint is a false compromise that stabilizes the very harm it claims to constrain. The engine computes per-seat classifications; these divergences are structural to the constraint, not errors in measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals (trapped, powerless) face high directionality (d approaching 1.0): they are the primary extraction target, their exit is blocked, and the constraint's operation is asymmetric against them. Industries (organized, powerful) face moderate directionality (d near 0.3–0.4): they are partially captured by welfare compliance costs, but they are the agenda-setter and retain arbitrage against consumers and regulators. Consumers face low directionality (d near 0.1–0.2): they benefit from subsidized animal products, and their exit options are real (alternative proteins, veganism). Welfare bodies face near-zero directionality (d near 0.05): they benefit from the constraint's existence without bearing its costs. The engine will compute these from beneficiary/victim declarations and exit options; the authored claim is tangled_rope, which requires both coordination (genuine welfare function) and asymmetric extraction (animals' powerless position).
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading's founding problem — unacknowledged animal suffering under industrial use — persists unresolved on large scales despite welfare improvements. The constraint's mandate (recognize sentience, enforce welfare protections) has not obsoleted the problem; rather, welfare standards have absorbed the pressure that might otherwise push toward abolition. This is a classical mandatrophy signal: the founding problem is not dead (contested status), the constraint persists (enforcement infrastructure solid), but the constraint no longer solves the problem it was built for — it manages the problem while stabilizing the use system. The high theater_ratio and plateau pattern corroborate this: welfare certification has become increasingly bureaucratic (theater) while harm on aggregate scales (farm sizes, slaughter volumes) has not proportionately declined. The classification as tangled_rope (not snare, not rope) correctly captures the hybrid structure: genuine coordination (welfare function) coupled with genuine extraction (permissive-use function). A snare classification would require that the coordination story be false; a rope classification would require that extraction be minor. The welfare reading operates with both at structural parity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_to_rights_closure,
    'Does the recognition of animal sentience logically entail the recognition of rights that would prohibit instrumental use, or is sentience compatible with permissive-use frameworks?',
    'Philosophical analysis and empirical observation of how different cultures and legal systems that recognize sentience operationalize it: do they always move toward abolitionist conclusions, or do some stabilize at welfare-protection-with-use frameworks?',
    'If sentience necessarily entails use-prohibition rights, the welfare reading is internally incoherent and unstable — it will tend toward abolitionist reclassification as sentience science advances. If sentience can be operationalized within permissive frameworks without logical contradiction, the welfare reading is stable and the distinction between this and abolitionist reading is not a false dichotomy but a genuine normative choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_to_rights_closure, conceptual, 'Whether animal sentience logically entails rights-based prohibition or is compatible with welfare-constrained use.').

omega_variable(
    welfare_standard_efficacy_gap,
    'What is the actual harm-reduction achieved by welfare standards relative to the harm prevented by prohibition, across industrial scales?',
    'Comparative empirical study: measure aggregate suffering under welfare-regulated use versus hypothetical prohibition (via case studies, cost-benefit analysis of alternatives like plant-based farming or in-vitro meat).',
    'If welfare standards reduce aggregate harm to within an acceptable threshold of prohibition outcomes, the welfare reading is justified as a pragmatic equilibrium. If welfare standards reduce harm negligibly while stabilizing continued use, the reading becomes a snare (coordination story covering pure extraction) rather than tangled_rope (genuine hybrid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_standard_efficacy_gap, empirical, 'Whether welfare standards meaningfully reduce harm or stabilize extraction under a consent narrative.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of abolitionist alternatives structurally enforced (legal barriers, property rights, market control) or internalized (consensus that welfare-constrained use is ethically acceptable)?',
    'Track abolitionist movements'' organizational capacity and political traction before/after welfare-law institutionalization; measure whether suppression persists if legal barriers are partially lifted (e.g., right to protest, media access, ballot initiatives).',
    'If suppression is primarily structural, removing legal barriers could shift the constraint rapidly toward abolitionist reading. If suppression is internalized (populations genuinely believe welfare-with-use is acceptable), the constraint is more stable against policy change. The distinction affects stability and type durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of abolition is structural law or internalized ethical consensus.').

omega_variable(
    false_sentience_naturalism,
    'Is animal sentience presented in this reading as a discovered fact (natural law, empirically grounded) or as a normative choice (a decision to treat them as sentient)?',
    'Compare the reading''s justification of sentience (appeal to cognition science, neurobiology, behavior) to abolitionist reading''s invocation of the same science — assess whether the difference in conclusion (use vs. prohibition) reflects different empirical claims or the same empirics read through different normative frames.',
    'If sentience is discovered, abolitionist reading is the correct inference and welfare reading is irrational. If sentience is normatively chosen, then both readings are defensible commitments to the same kernel and the choice is political/ethical, not empirical. This determines whether welfare reading is a false natural law hiding a constructed normative choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_sentience_naturalism, conceptual, 'Whether animal sentience is an empirically discovered fact or a normatively chosen commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_status__welfare_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status__welfare_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status__welfare_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_status__welfare_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_status__welfare_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_status__welfare_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_status__welfare_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_status__welfare_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_status__welfare_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.18).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel hosts three structurally distinct constraint stories, one per reading. This story (welfare_reading) treats animal sentience as grounds for welfare constraints on use. The abolitionist_reading treats sentience as entailing use prohibition. The property_reading treats animals as legal objects without sentience-based moral standing. The three stories share the kernel (animals' legal and moral status) but diverge in consequential structural ways: epsilon, victim sets, and enforcement mechanisms differ. All three are linked via network.affects_constraints to form the animal_status constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
