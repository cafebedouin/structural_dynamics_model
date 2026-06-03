% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Welfare Regulation (Sentience + Constrained Use Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   The welfare reading of the animal status kernel represents a specific
 *   moral and legal compromise: animals are acknowledged as sentient beings
 *   whose suffering is morally relevant, yet property status is retained and
 *   use is permitted if regulated to minimize pain. This reading emerged as a
 *   middle position between traditional property-maximalist frameworks
 *   (animals are mere resources) and abolitionist frameworks (animals are
 *   moral non-property with rights not to be used). The welfare reading
 *   instantiates a distinct constraint structure combining genuine
 *   coordination (welfare regulations do reduce acute suffering) with
 *   extraction (the permission to use animals despite
 *   sentience-acknowledgment). The constraint's extractiveness has declined
 *   over the interval (0.72 → 0.48) as welfare standards tightened, while
 *   theater has risen (0.35 → 0.58), indicating growing performative content
 *   in certification and labeling schemes. The suppression requirement has
 *   increased (0.38 → 0.52) as maintaining public acceptance of use requires
 *   greater effort to manage the cognitive dissonance between
 *   sentience-acknowledgment and use-permission.
 *
 * KEY AGENTS:
 *   - Farmed Animals: Primary victim (powerless/trapped) — sentient beings acknowledged as moral patients yet legally confined to property status and extraction through use, breeding, and slaughter
 *   - Abolitionist Movement: Secondary beneficiary and organized victim (organized/constrained) — benefits from sentience-acknowledgment but suffers from welfare reforms that reduce pressure for use-prohibition
 *   - Animal Agriculture Industry: Primary beneficiary (institutional/arbitrage) — captures continued market access and moral legitimacy through welfare compliance; experiences constraint as coordination mechanism enabling differentiated marketing
 *   - Conscience-Seeking Consumer: Secondary beneficiary (powerful/mobile) — experiences welfare regulations as solving their moral conflict, enabling consumption without guilt
 *   - Legislative Reform Coalition: Powerful transitional actor (powerful/mobile) — sees welfare regulation as ratcheting mechanism toward eventual use-prohibition; supports welfare improvements as stepping-stones
 *   - Animal Protection Establishment: Institutional maintainer (institutional/arbitrage) — operates certification and advocacy infrastructure dependent on welfare paradigm persistence; high theater content
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees the reading's core structure: genuine suffering-reduction paired with systematic extraction enabled by property-status retention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.48).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.52).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Regulation (Sentience + Constrained Use Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '79c2610d-200c-4f1f-8713-5fdfb4f7517a').
narrative_ontology:cs_kernel_codification('79c2610d-200c-4f1f-8713-5fdfb4f7517a', formalized).
narrative_ontology:cs_authority_grounding('79c2610d-200c-4f1f-8713-5fdfb4f7517a', extraction).
narrative_ontology:cs_interpretation_layer_present('79c2610d-200c-4f1f-8713-5fdfb4f7517a').
narrative_ontology:cs_reading_relation('79c2610d-200c-4f1f-8713-5fdfb4f7517a', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('79c2610d-200c-4f1f-8713-5fdfb4f7517a', animal_status_kernel__abolitionist_reading, influences).
narrative_ontology:cs_axiom('79c2610d-200c-4f1f-8713-5fdfb4f7517a', foundational, animal_sentience_moral_relevance).
narrative_ontology:cs_axiom_status(animal_sentience_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('79c2610d-200c-4f1f-8713-5fdfb4f7517a', animal_sentience_moral_relevance, empirically_contingent).
narrative_ontology:cs_axiom('79c2610d-200c-4f1f-8713-5fdfb4f7517a', foundational, suffering_minimization_sufficient).
narrative_ontology:cs_axiom_status(suffering_minimization_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('79c2610d-200c-4f1f-8713-5fdfb4f7517a', suffering_minimization_sufficient, deontological).
narrative_ontology:cs_axiom('79c2610d-200c-4f1f-8713-5fdfb4f7517a', secondary, property_status_permissible).
narrative_ontology:cs_axiom_status(property_status_permissible, holdable).
narrative_ontology:cs_axiom_grounding('79c2610d-200c-4f1f-8713-5fdfb4f7517a', property_status_permissible, conventional).
narrative_ontology:cs_reference_frame('79c2610d-200c-4f1f-8713-5fdfb4f7517a', sentient_property_framework).
narrative_ontology:cs_drift_state('79c2610d-200c-4f1f-8713-5fdfb4f7517a', contemporary_welfare_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('79c2610d-200c-4f1f-8713-5fdfb4f7517a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumer_ethical_compromise).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, sentient_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, abolitionist_movement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FARMED ANIMAL (SNARE) — Trapped within the regulatory regime. Acknowledged as sentient yet denied exit from use. Welfare regulations provide marginal pain reduction but do not address the underlying extraction: confinement, selective breeding for rapid growth, truncated lifespan, death in slaughter. Maximum experienced extraction — the constraint permits continued use while creating illusion of moral consideration.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ABOLITIONIST MOVEMENT (TANGLED ROPE) — Organized but constrained. Benefits from welfare regulation's acknowledgment of sentience (legitimizes moral concern) but bears extraction cost: welfare reforms consolidate public acceptance of 'happy meat,' forestalling the abolitionist claim that property status itself is injustice. High suppression—mainstream discourse treats abolition as extremist; constrained exit through institutional capture of animal-protection discourse.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANIMAL AGRICULTURE INDUSTRY (ROPE) — Primary beneficiary (institutional/arbitrage). Welfare regulation operates as coordination mechanism: defines permitted practices, reduces liability exposure, enables marketing of 'responsibly sourced' products. Net beneficiary—regulatory burden is modest compared to the value of maintained market access and moral legitimacy. Experiences constraint as coordination, not extraction.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSCIENCE-SEEKING CONSUMER (ROPE) — Powerful agent experiencing coordination. Welfare regulations permit guilt-free consumption—the constraint solves the consumer's moral conflict by providing assurance of 'humane' use. High mobility (can exit through veganism) but welfare regime eliminates exit incentive by reframing consumption as compatible with moral consideration. Experiences extraction as coordination benefit.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE REFORM COALITION (SCAFFOLD) — Powerful actors (e.g., EU legislative bodies, consumer advocates) see welfare regulation as a transitional mechanism with sunset logic. Welfare standards ratchet upward across regulatory cycles; the constraint's function evolves toward abolition through accumulating welfare requirements. Sunset rationale: as welfare costs rise, remaining uses become economically marginal, creating convergence with abolitionist outcome through cost, not principle. Theater moderate—genuine regulatory function but embedded in larger transition narrative.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANIMAL PROTECTION ESTABLISHMENT (PITON) — Institutional actors (mainstream animal welfare organizations, certification bodies) maintain performative investment in the welfare paradigm. Theater is high—certification schemes (e.g., 'Certified Humane') create appearance of meaningful constraint while permitting continued high-volume use. The establishment's institutional survival depends on welfare paradigm persistence; it perpetuates theater while lacking functional mechanism to achieve stated welfare goals at scale. Degraded from rope (genuine coordination) by institutional inertia.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational context. This reading structurally combines genuine coordination (welfare regulations do reduce some suffering) with extraction (property status and use rights are retained despite sentience acknowledgment). The constraint generates the 'new welfarism' paradox: by making use morally acceptable under conditions, welfare regulation may reduce pressure for abolition while animals remain structurally victims of the use-system itself. Classification resolves the paradox—tangled rope captures both the coordination function and the extractive asymmetry.
constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_status_kernel__welfare_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(animal_status_kernel__welfare_reading, TR),
    TR >= 0.70.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. This reading is defined by permitting use while constraining it. The base extractiveness reflects the structural asymmetry: animals' suffering is recognized as morally relevant, yet they remain property whose use is legally permitted. The declining trajectory (0.72 → 0.48) captures the effect of tightening welfare standards—each regulatory cycle imposes greater costs on industry while reducing acute suffering. But extractiveness does not approach zero because the reading does not question property status itself; use continues. The moderate value distinguishes this reading from pure property (extractiveness ~0.85+) and from abolitionism (extractiveness ~0.05). Suppression (0.52): Moderate-high and rising. The reading requires active enforcement because it must suppress the abolitionist reframing—that property status itself is injustice. As welfare standards rise and animals' sentience becomes undeniable, maintaining public comfort with use requires greater effort: marketing ('happy meat'), certification theater, discourse management emphasizing regulatory adequacy. Theater ratio (0.58): Moderate and rising. Welfare certification schemes (Certified Humane, Rainforest Alliance animal programs) create appearance of meaningful constraint while permitting continued high-volume use. At t0, welfare regulation was primarily functional—actual housing/handling improvements. By t10, it has become increasingly theatrical—elaborate labeling creating moral reassurance with modest functional impact. The rising trajectory reflects institutional learning: as welfare costs rise, industry invests in perception-management rather than further functional improvement.
 *
 * PERSPECTIVAL GAP:
 *   The welfare reading produces maximum perspectival divergence across the indexical space. The farmed animal (powerless/trapped/biographical) sees a snare—sentience is acknowledged only to be instrumentalized. The abolitionist (organized/constrained/biographical) sees tangled rope—the reading advances their foundational claim while forestalling their concrete goal. The industry (institutional/arbitrage/immediate) sees rope—welfare regulation is coordination enabling continued use. The consumer (powerful/mobile/biographical) sees rope—their moral conflict is solved. The reform coalition (powerful/mobile/generational) sees scaffold—welfare ratcheting as stepping-stone toward eventual prohibition. The protection establishment (institutional/arbitrage/civilizational) sees piton—degraded from genuine coordination into theater. The analytical observer (analytical/analytical/civilizational) sees tangled rope—genuine coordination paired with systematic extraction. This seven-way perspectival divergence reveals the reading's core tension: it genuinely coordinates some goods (reduced acute suffering) while extracting others (maintained property status and use rights). No single perspective is 'correct'; the presheaf of perspectives encodes the reading's actual structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The welfare reading's beneficiary and victim structure determines directionality for each perspective. The industry (institutional/arbitrage) is the primary beneficiary—they experience low directionality (d ≈ 0.10) because welfare regulations are a modest coordination cost enabling continued access to animals as resources. The farmed animal (powerless/trapped) is the primary victim—they experience maximum directionality (d ≈ 0.95) because they bear the extraction (continued use despite sentience) with no exit. The abolitionist movement (organized/constrained) is structurally ambiguous: they are victims of the reading's success (welfare reforms reduce pressure for abolition) yet benefit from the reading's sentience-acknowledgment (which advances their foundational claim). Their directionality is moderate-high (d ≈ 0.55) reflecting constrained agency and mixed position. The consumer (powerful/mobile) has low directionality (d ≈ 0.25) because they benefit from the moral reassurance the reading provides. The analytical observer operates at directionality d ≈ 0.72 (standard for analytical position), recognizing both the coordination and extraction functions simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading avoids mandatrophy collapse by maintaining structural coherence at tangled_rope through genuine dual function: it coordinates suffering-reduction while extracting use-rights. This differs from false-rope (extraction disguised as coordination) and false-snare (coordination misclassified as extraction). The reading's vulnerability is not mandatrophy but rather the axiom_overriding omega (whether animal_sentience_moral_relevance eventually logically entails property-status prohibition). If this omega resolves toward entailment, the reading becomes unstable—its foundational axioms become incoherent, and the reading collapses into either property (by rejecting sentience-relevance) or abolitionism (by accepting sentience-relevance fully). The measurement trajectory showing declining extractiveness and rising theater suggests the reading may be unstable in the long term: as welfare costs accumulate, industry cannot maintain both sentience-acknowledgment and property-status without increasing theatrical apparatus. The scaffold perspective's sunset logic suggests the reading is a transition state rather than an equilibrium—welfare standards ratchet upward until remaining uses are economically marginal or morally untenable. But this transition is not mandatrophy; it is the reading's own logical evolution under conditions of its own success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_threshold_sufficiency,
    'At what level of welfare constraint does the reading''s core commitment—''suffering-minimization is the relevant moral criterion''—require transition to abolitionism?',
    'Logical analysis of the reading''s foundational axiom (suffering_minimization_sufficient) against empirical research on: (a) residual suffering under maximal welfare standards, (b) whether confinement/truncated lifespan constitute suffering independent of acute pain, (c) whether property status structurally prevents adequate sentience-protection',
    'If residual suffering is irreducible: the reading''s axiom becomes incoherent at high welfare thresholds, foreclosing the welfare reading and validating abolitionist reframing. If residual suffering can be minimized near-zero: the reading remains coherent and distinct from abolitionism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_threshold_sufficiency, empirical, 'Whether welfare constraints can achieve sufficient suffering-minimization to justify continued use').

omega_variable(
    property_status_moral_incoherence,
    'Does acknowledging animals as sentient beings whose suffering is morally relevant logically cohere with retaining their property status, or does sentience plus moral relevance entail rights-bearing personhood?',
    'Philosophical analysis of the reading''s two foundational axioms (animal_sentience_moral_relevance + property_status_permissible) for logical consistency. Comparison to historical precedent: was property status retained for human groups once acknowledged as sentient? Legal precedent: jurisdictions that formally acknowledge sentience but retain property status vs those that have moved toward personhood or use-prohibition.',
    'If the axioms are incoherent: the reading is internally contradictory, and the constraint must be reclassified as false coherence or forced into a different reading (property or abolitionist). If coherent: the reading represents a genuine third position, distinct from both property-only and abolitionist frames.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_moral_incoherence, conceptual, 'Whether sentience-acknowledgment plus property-status are logically coherent').

omega_variable(
    new_welfarism_feedback_loop,
    'Do welfare reforms actually reduce trajectory toward abolition (by enabling moral compromise), or do they increase it (by creating infrastructure for monitoring use and establishing precedent for use-constraint)?',
    'Historical tracking: in jurisdictions with decades of welfare ratcheting (EU, UK), has the proportion of vegans/vegetarians increased or decreased relative to countries without welfare infrastructure? Do animal protection organizations report that welfare gains facilitate or obstruct abolitionist organizing? Elite discourse analysis: do legislatures cite welfare adequacy as reason to extend use, or as stepping-stone to restriction?',
    'If welfare reforms reduce abolition trajectory: the reading''s ''new welfarism'' critique is valid—the constraint genuinely functions to forestall abolition. If reforms increase trajectory: the scaffold perspective is correct—welfare infrastructure accelerates eventual use-prohibition. Classification remains tangled_rope either way, but the causal mechanism (forestalling vs accelerating) differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_welfarism_feedback_loop, empirical, 'Whether welfare reforms reduce or increase trajectory toward abolition').

omega_variable(
    kernel_alternative_reading_forecast,
    'If this welfare reading were fully instantiated at maximum stringency (zero-pain housing, immediate slaughter, death-before-illness protocols), would the residual constraint be property_reading (animals as property with welfare obligations) or abolitionist_reading (animals as moral non-property)?',
    'Thought experiment: at the welfare reading''s logical terminus (minimal suffering under use), what axioms remain operative? Does the foundational commitment animal_sentience_moral_relevance still permit property status, or does maximal welfare-stringency force reclassification into abolitionist axiom (sentient_non_property)? What would legislators cite as the reading''s limiting case?',
    'If the reading''s terminus is property: welfare_reading is genuinely distinct from abolitionism, with its own structural stability. If the terminus is abolition: welfare_reading is unstable—it is a way-station, not an equilibrium. This determines whether the reading coexists_with or influences the abolitionist_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_reading_forecast, conceptual, 'Whether the welfare reading has its own equilibrium or is transitional toward abolition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(animal_welfare_tr_t5, animal_status_kernel__welfare_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(animal_welfare_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(animal_welfare_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(animal_welfare_be_t5, animal_status_kernel__welfare_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(animal_welfare_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(animal_welfare_su_t5, animal_status_kernel__welfare_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(animal_welfare_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel contains three structurally distinct constraints, each with its own extractiveness value, beneficiary/victim structure, and classification. The welfare_reading (this constraint, ε=0.48, Tangled Rope) differs fundamentally from property_reading (ε≈0.85, Snare/Mountain) and abolitionist_reading (ε≈0.08, Rope/Mountain). These are not different measurements of one constraint but three separate constraints grounded in the same contested kernel. They are linked through reading_relations in cs_structure rather than through network.affects_constraints, which models causal influence. The network edges here indicate that welfare regulation empirically affects and constrains both property-maximalist industrial practice and abolitionist organizing—these are downstreams of the welfare reading's institutional success.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
