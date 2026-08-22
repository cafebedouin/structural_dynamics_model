% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Moral Status: Abolitionist Reading
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status holds that animals
 *   possess intrinsic rights by virtue of sentience; that property status is
 *   itself the violation, not merely a context for cruelty; and that all use
 *   perpetuates victimization regardless of welfare standards. This reading
 *   instantiates a constraint: the standing arrangement under contest is the
 *   property-status regime itself, which organizes animal relationships
 *   around human appropriation. From the abolitionist perspective, the
 *   constraint is a snare — it extracts bodies, labor, and lives from
 *   powerless victims (animals) whose interests are structurally excluded
 *   from consideration, maintained through legal enforcement, and legitimized
 *   through welfare-reform theater that accepts use-in-principle. The
 *   constraint has one victim class (animals under human dominion) and no
 *   beneficiary in the abolitionist framing — the goods humans receive are
 *   purchased through structural victimization, not coordinated benefit. This
 *   is NOT a claim that welfare improvements are worthless; it is a claim
 *   that they function as a suppression mechanism, extending the constraint's
 *   life by reducing resistance.
 *
 * KEY AGENTS:
 *   - animals_under_human_dominion: powerless, trapped, victims of structural appropriation
 *   - industrial_animal_agriculture: institutional agenda-setter, enforces and maintains property-status regime globally
 *   - welfare_reform_advocates: organized, constrained, inadvertently legitimize property status through incremental reform
 *   - legal_property_doctrine: institutional enforcement layer that codifies animals as non-agents
 *   - consumer_populations: moderate power, moderate exit, receive goods produced through use but bear diffuse costs
 *   - sanctuary_and_liberation_operators: excluded from mainstream institutional legitimacy, demonstrate feasibility of non-use relationships
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.89).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.81).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Moral Status: Abolitionist Reading").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '927b2ffd-4ebf-4a6e-9e87-e7667ff418bf').
narrative_ontology:cs_kernel_codification('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', distributed).
narrative_ontology:cs_authority_grounding('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', distributed).
narrative_ontology:cs_reading_relation('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', foundational, property_status_incompatible_with_rights).
narrative_ontology:cs_axiom_status(property_status_incompatible_with_rights, holdable).
narrative_ontology:cs_axiom_grounding('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', property_status_incompatible_with_rights, deontological).
narrative_ontology:cs_reference_frame('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', animal_moral_agency_recognition).
narrative_ontology:cs_drift_state('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', contemporary_anthropocene, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('927b2ffd-4ebf-4a6e-9e87-e7667ff418bf', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_reform_advocates).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consumer_populations).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, welfare_reform_advocates).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, consumer_populations).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, sentience_grounds_moral_status).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, property_status_incompatible_with_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All domesticated, captive, and wild animals whose existence is controlled by human systems. They bear the cost of use — labor extraction, breeding for traits that cause suffering, confinement, slaughter, medical experimentation, entertainment provision — regardless of how 'humanely' these practices are framed. Their bodies, reproductive capacity, and labor are appropriated; exit is structurally impossible.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, biographical, trapped, global).

% Operates the vast majority of animal confinement and slaughter systems globally. Enforces and maintains the property-status regime through legal frameworks, enforcement against sanctuary operations, political pressure on welfare-reform advocates, and normalization through cultural practice. Collects value from animals' bodies and labor; the abolitionist reading identifies this entire operation as structural victimization.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, industrial_animal_agriculture, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, industrial_animal_agriculture, payer).

% Campaign for reduced suffering within continued use systems — higher cage standards, slaughter speed improvements, anesthesia protocols. From the abolitionist perspective, they inadvertently legitimize property status by accepting use-in-principle, thereby perpetuating the deeper victimization structure. They bear the cost of incrementalism (the constraint persists); they collect a symbolic benefit (welfare marginal gains).
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_reform_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_reform_advocates, beneficiary).

% The intellectual tradition holding that animals possess intrinsic moral status by virtue of sentience, that property status is itself a violation incompatible with rights, and that all use perpetuates victimization. This reading produces the constraint by instantiating property-status-as-violation as the standing arrangement under contest.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, philosophical_abolitionist_tradition, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(animal_moral_status__abolitionist_reading, philosophical_abolitionist_tradition).

% The institutional framework that codifies animals as property, not rights-bearers. Enforced through contract law, ownership doctrines, and criminal protection of property interests over animal interests. The abolitionist reading names this doctrine itself as the extraction mechanism.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_property_doctrine, agenda_setter,
    institutional, civilizational, analytical, global).

% Receive goods (meat, dairy, eggs, leather, laboratory-tested pharmaceuticals) produced through animal use. They collect direct benefit (nutrition, goods, medical access); they also bear diffuse indirect costs (environmental degradation, zoonotic disease risk, moral complicity). From the abolitionist perspective, the benefit they receive is purchased through continued victimization and depends structurally on property status.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consumer_populations, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, consumer_populations, payer).

% Operate outside property-status frameworks, providing care without use extraction. Structurally excluded from mainstream institutional legitimacy; face legal and economic pressure from property-rights enforcement. Their existence demonstrates the feasibility of non-use relationships with animals but remains marginal and fragile.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, sanctuary_and_liberation_operators, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint instantiates the doctrine that animals are property, not rights-bearers. This doctrine 'coordinates' human societies by establishing a unified framework for animal appropriation — one clear rule (animals = property) rather than fragmented local regimes. But the abolitionist reading denies this is genuine coordination: instead of solving a coordination problem among symmetrical parties, it imposes a categorical asymmetry that forecloses animal agency entirely.
% TRANSFER_FUNCTION: Moves animals' bodies, labor, reproductive capacity, and biological time from animals (who bear 100% of the cost) to human users (agriculture, research, entertainment, consumption). The constraint's enforcement machinery ensures this transfer persists by excluding alternative framings (rights-based, sanctuary-based, non-use relationships).
% ABSENT_VOICES: Animals themselves cannot participate in the legal or political conversation about their own moral status. They are structurally excluded by virtue of the property-status regime. The abolitionist reading identifies this exclusion as central to how the constraint persists — were animals' interests represented at full moral weight, the property-status regime could not survive challenge. Sanctuary operators and animal liberation advocates are marginalized voices, kept outside institutional authority.
% DISAPPEARANCE_RATIONALE: If the constraint (property status as the organizing principle for animal relationships) disappeared overnight, industrial animal agriculture would cease, billions of confined animals would require immediate relocation/care/integration into non-use settings, supply chains for animal products would collapse, legal property doctrines would revert to rights-frameworks, and human societies would reorganize around non-appropriative relationships with animals. The economic, legal, and social infrastructure built on property status would require fundamental reconstruction.
% FOUNDING_PROBLEM: Humans needed a framework for organizing relationships with animals. Property status provided a unified doctrine: animals are objects of human dominion, appropriable for use without constraint on human interests.
% FOUNDING_PROBLEM_CORROBORATION: Industrial agriculture and property-doctrine advocates assert the founding problem is still live: animals need governance/organization, and property status provides clear rules. Abolitionist philosophers and animal liberation advocates assert the problem is misconceived — there is no genuine 'need' for a framework of appropriation; the need is for a framework of coexistence that respects animal autonomy. Sanctuary operators and non-use practitioners attest that animals can thrive in organized care settings without property status. The corroboration is split across opposed factions; no neutral party stands outside the contest.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.89) because the constraint appropriates 100% of animals' capacity and lifetime; there is no residual autonomy or benefit accrual to animals. Suppression is also high (0.81) because the property-status doctrine is enforced legally, economically, and culturally — alternatives (non-use, sanctuary care, recognition of animal agency) face structural barriers. Theater_ratio is moderately high (0.62) because welfare reforms constitute a substantial portion of visible enforcement activity: cage-size standards, slaughter-speed improvements, anesthesia protocols — these are real improvements but function primarily to reduce resistance to property status itself, not to dismantle the constraint. The measurement series show extraction and theater rising over time (50-year interval) — as property-status systems intensify (factory farming expansion, industrial scale-up), theater expands in proportion (welfare certifications, corporate welfare commitments, sanctuary media visibility) to suppress rising resistance. Suppression requirement rises as abolitionist and liberation voices grow.
 *
 * PERSPECTIVAL GAP:
 *   From the industrial-agriculture and legal-property-doctrine seats, the constraint is a benign coordination solution: animals are objects requiring governance; property status provides clear rules; welfare reforms improve outcomes without dismantling the system. From the animals' structural position (powerless, trapped, no seat at the table), the constraint is pure victimization — the property status itself, not cruelty within it, is the violation. From the welfare-reform seat, the constraint is partially justified (animals need protection from cruelty) but increasingly recognized as insufficient (property status itself limits what reforms can achieve). The engine should compute divergent type classifications across seats: institutional/powerful seats should compute toward rope or tangled_rope (seeing coordination benefits); powerless/trapped seats compute toward snare (structural victimization); organized/moderate seats compute toward tangled_rope or scaffold (seeing benefit + cost, seeking transition).
 *
 * DIRECTIONALITY LOGIC:
 *   Animals bear directional cost d=1.0 (full targets): they are powerless, trapped, with no exit options, and receive no benefit — the property status extracts their entire capacity. Industrial agriculture and legal property doctrine sit at d=0.0 to 0.1 (beneficiaries): they enforce the regime, collect its value, and have full mobility to adjust rules. Welfare reformers sit at d=0.5-0.6 (symmetric): they see genuine coordination benefit (animal protection) but also bear costs (constrained exit, moral ambiguity, limited leverage). Consumer populations sit at d=0.3-0.4: they receive goods but face growing knowledge/moral costs. The abolitionist reading does not declare them beneficiaries because it rejects the premise that extraction-purchased goods constitute legitimate benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (organizing animal relationships) has NOT been resolved; instead, the constraint has become self-perpetuating through institutional embedding and cultural normalization. Industrial agriculture persists not because it solves the original coordination problem more efficiently than alternatives, but because it captures the institutions that define 'efficiency' and 'coordination.' The theater (welfare reforms) grows precisely because the founding problem is unresolved — reform advocates try to solve it within the property-status frame, but the frame itself prevents resolution. The abolitionist reading identifies this as a zombie constraint: the coordination story is cover; persistence depends on coercion (legal enforcement of property doctrines, economic pressure on alternatives) and on suppressing exits (discrediting sanctuary models, marginalizing abolitionist philosophy). Mandatrophy is incipient: the constraint persists largely through institutional inertia and welfare theater, not through genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_vs_legal_personhood,
    'Does moral status flow directly from sentience (capacity to suffer), or does it require formal legal personhood status independent of sentience?',
    'Philosophical analysis comparing sentience-based ethics (Singer, Regan) with legal-positivist frameworks; empirical investigation of non-legal societies'' treatment of animals; comparison with moral status attributions in human contexts (infants, persons in permanent coma) where sentience and personhood diverge.',
    'If sentience suffices, property status is categorically unjustifiable; if personhood is required, property status remains defensible until legal reform occurs. This is the boundary between snare and tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_vs_legal_personhood, conceptual, 'Whether moral status derives from sentience or requires formal legal recognition.').

omega_variable(
    suppression_internalization_in_animals,
    'Is the measured suppression (animals'' failure to resist use) structural (external barriers: confinement, dependency, inability to organize) or internalized (animals have been psychologically shaped to accept subordination)?',
    'Behavioral studies of animals in sanctuaries vs. industrial settings; neurobiological investigation of learned helplessness in captive animals; comparison of wild vs. domesticated species'' resistance to use.',
    'If suppression is purely structural, property-status removal would enable rapid resistance; if internalized, post-abolition animals would carry suppression behavioral patterns. Changes the post-abolition transition narrative but not the snare classification itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_animals, empirical, 'Whether animal suppression is structural constraint or internalized adaptation.').

omega_variable(
    welfare_reform_as_cover_or_bridge,
    'Are welfare reforms a suppression mechanism that extends property-status life, or are they a necessary bridge toward abolition that shifts institutional capacity and moral consciousness?',
    'Historical analysis of reform-then-abolition trajectories (e.g., human slavery, child labor); investigation of whether welfare organizations systematically oppose abolition; measurement of whether welfare reform participants develop abolitionist commitments.',
    'If cover: theater_ratio analysis points toward snare consolidation. If bridge: theater_ratio reflects genuine transition dynamics and the constraint may be reclassifiable as scaffold (with hidden sunset) if welfare movements are tracked for abolitionist conversion. Low confidence because the resolution depends on empirical future trajectories not yet determined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_as_cover_or_bridge, empirical, 'Whether welfare reform extends or undermines property-status regime persistence.').

omega_variable(
    property_status_contingent_vs_structural,
    'Is property status a contingent institutional choice that could be reformed away (snare classification preserved), or is it structurally entailed by the organization of human society (moves toward mountain classification)?',
    'Anthropological/historical study of non-property-based animal relationships in pre-industrial societies; feasibility studies of post-property animal relationships in contemporary contexts; game-theoretic analysis of whether property-free systems can scale.',
    'If contingent: snare classification holds; reform/abolition is institutional choice, not natural limit. If structural: constraint may reclassify toward mountain with high accessibility_collapse. This is the deepest omega — it determines whether the constraint''s ε reflects human choices or natural limits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_status_contingent_vs_structural, conceptual, 'Whether property status is a contingent institutional arrangement or structurally necessary.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the abolitionist axiom (animals_are_rights_bearing_individuals) LOGICALLY FORECLOSE the property reading''s axiom (animals_are_property_without_standing), or do they merely COEXIST as contested positions in ongoing dispute?',
    'Formal logical analysis of the two axioms'' contradictory vs. contrary relationship; investigation of whether any philosophical framework could hold both within a single coherent system.',
    'If foreclosed: the reading_relations entry for property_reading should be ''forecloses'', indicating this reading''s core premise logically eliminates the sibling''s. If coexistent: ''coexists_with'', indicating both remain live positions in different parties'' frameworks. This determines engine-level kernel drift analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading logically forecloses the property reading or merely opposes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.41).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.57).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(anim_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.89).
narrative_ontology:measurement_basis(anim_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.81).
narrative_ontology:measurement_basis(anim_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel admits three distinct constraint stories: abolitionist_reading (this file, snare, ε~0.89), welfare_reading (tangled_rope, ε~0.55-0.65, coordination within use), property_reading (rope, ε~0.20-0.35, coordination justified). Each reading instantiates a different ε-invariant constraint because the referent is fixed (the standing property-status arrangement) but the reading's assessment differs radically. Abolitionist reading sees victimization; welfare reading sees mixed coordination-with-cruelty; property reading sees justified coordination. The omegas route the committer-frame ambiguities (sentience vs. personhood, contingency vs. structure, foreclosure vs. coexistence) to the corpus for data-driven resolution. Do NOT merge these into one constraint with measurement parameters — the readings are distinct constraints in a constraint family, linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
