% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status: Ownership-Centered Moral Framework
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the moral
 *   and legal status of non-human animals. The property reading frames
 *   animals as property subjects rather than moral persons, deriving all
 *   moral considerability from ownership rights and economic value. Under
 *   this reading, animals have no intrinsic moral standing; anti-cruelty laws
 *   protect animal welfare only insofar as they protect owner interests
 *   (market value of livestock, reputation of consumers, public health). The
 *   kernel itself — the fundamental question of what animals are and what
 *   obligations humans owe them — admits multiple incompatible readings, each
 *   grounding a distinct constraint with different beneficiaries, victims,
 *   and extractiveness profiles. This constraint is the property reading. Its
 *   sibling readings (welfare and abolitionist) are structurally distinct
 *   constraints that would appear in separate JSON files, linked via
 *   network.affects_constraints. The property reading classifies as SNARE:
 *   high extractiveness (animals as property can be exploited without moral
 *   constraint), high suppression (animals lack legal personhood and advocacy
 *   capacity), and performance of concern through anti-cruelty statutes
 *   (piton layer). The framework is internally coherent — it is not a failed
 *   coordination mechanism but a succeeding extraction mechanism with
 *   explicit beneficiaries (animal owners and the property law system) and
 *   explicit victims (non-human animals).
 *
 * KEY AGENTS:
 *   - Non-Human Animals: Primary victims (powerless/trapped) — lack legal personhood, moral standing, and exit options. Bear all costs of property status (confinement, use, death) with no countervailing protection except peripheral welfare regulations designed to protect owner interests.
 *   - Animal Owners: Primary beneficiaries (institutional/arbitrage) — gain unrestricted use rights. Property status legitimates use for food, labor, research, sport, and profit. Arbitrage exit — can sell, release, or use animals without moral constraint.
 *   - Property Law System: Beneficiary institution (institutional/arbitrage) — coordinates ownership rights and enforces property claims. Treats animals as fungible assets and welfare regulations as market stabilizers, not moral constraints.
 *   - Welfarist Reform Coalition: Secondary actor (organized/constrained) — accepts property status but advocates welfare constraints. Constrained exit — must work within property framework because direct challenge to ownership faces institutional resistance.
 *   - Anti-Cruelty Statute Regime: Performative enforcement mechanism (institutional/mobile) — creates appearance of moral consideration while leaving property status intact. Theater ratio reflects selective enforcement and trivialization relative to extraction benefit.
 *   - Analytical Observer: Universal context (analytical/analytical) — views the property framework as internally coherent but structurally extractive. The framework does not fail as coordination; it succeeds as extraction because it excludes its primary targets from the moral calculus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.82).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.68).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status: Ownership-Centered Moral Framework").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'fac5308d-9add-4129-b0d0-159ce4eec16c').
narrative_ontology:cs_kernel_codification('fac5308d-9add-4129-b0d0-159ce4eec16c', formalized).
narrative_ontology:cs_authority_grounding('fac5308d-9add-4129-b0d0-159ce4eec16c', extraction).
narrative_ontology:cs_interpretation_layer_present('fac5308d-9add-4129-b0d0-159ce4eec16c').
narrative_ontology:cs_reading_relation('fac5308d-9add-4129-b0d0-159ce4eec16c', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('fac5308d-9add-4129-b0d0-159ce4eec16c', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('fac5308d-9add-4129-b0d0-159ce4eec16c', foundational, ownership_as_moral_ground).
narrative_ontology:cs_axiom_status(ownership_as_moral_ground, holdable).
narrative_ontology:cs_axiom_grounding('fac5308d-9add-4129-b0d0-159ce4eec16c', ownership_as_moral_ground, conventional).
narrative_ontology:cs_axiom('fac5308d-9add-4129-b0d0-159ce4eec16c', foundational, economic_value_as_sufficient).
narrative_ontology:cs_axiom_status(economic_value_as_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('fac5308d-9add-4129-b0d0-159ce4eec16c', economic_value_as_sufficient, instrumental).
narrative_ontology:cs_axiom('fac5308d-9add-4129-b0d0-159ce4eec16c', secondary, legal_personhood_requirement).
narrative_ontology:cs_axiom_status(legal_personhood_requirement, holdable).
narrative_ontology:cs_axiom_grounding('fac5308d-9add-4129-b0d0-159ce4eec16c', legal_personhood_requirement, conventional).
narrative_ontology:cs_reference_frame('fac5308d-9add-4129-b0d0-159ce4eec16c', unrestricted_property_dominion).
narrative_ontology:cs_drift_state('fac5308d-9add-4129-b0d0-159ce4eec16c', contemporary_animal_ethics_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fac5308d-9add-4129-b0d0-159ce4eec16c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, property_law_system).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, non_human_animals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-HUMAN ANIMAL (SNARE) — Trapped by legal non-personhood; no moral standing to contest use; extraction is maximal and unmediated. The animal bears all costs (pain, confinement, death) and receives no countervailing protection except peripheral cruelty statutes designed to protect owner interests, not animal interests. Trapped exit — no legal mechanism for self-advocacy or exit from property ownership.
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL OWNER (ROPE) — Unrestricted use rights; extraction runs toward this agent. Experiences the constraint as pure coordination benefit — property status enables legitimate use for food, labor, companionship, and profit. No perceived extraction cost because the moral framework excludes the animal from the victim-set. Arbitrage exit — owner can sell, release, or cull animals without moral constraint.
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTY LAW SYSTEM (ROPE) — Coordinates ownership rights and economic value extraction. The legal framework sees animals as property and views welfare regulations as market stabilizers (protecting owner reputation and preventing market externalities), not as constraints on use rights. Arbitrage exit — the system can redefine animal status at any time, but chooses not to because property status maximizes economic value.
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELFARIST REFORM COALITION (TANGLED ROPE) — Organized advocates (animal welfare organizations, some legislatures) accept property status but impose welfare constraints. This perspective sees both coordination benefit (humane standards are enforceable) and extraction (the property framework itself remains unchallenged, allowing continued exploitation within 'humane' bounds). Constrained exit — reformers must work within the property system because direct challenge to property status faces institutional resistance.
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-CRUELTY STATUTE REGIME (PITON) — Performative constraint on animal exploitation. Anti-cruelty laws produce the appearance of moral consideration while leaving property status intact. Theater ratio reflects that enforcement is selective, penalties are trivial relative to extraction benefit, and the frame ('cruelty is bad; use is fine') naturalizes the property hierarchy. The regime persists through institutional inertia — stakeholders treat it as moral protection while it primarily protects owner interests (animal integrity as property asset, market confidence in consumer goods).
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — PROPERTY FRAMEWORK (SNARE) — From a universal analytical view, this reading classifies as snare: high extractiveness (0.82) with minimal coordination function (property rules enable use, not mutual benefit across species), high suppression (animals lack legal personhood and advocacy capacity), and low theater (the extraction mechanism is explicit — animals are property to be used at owner discretion). The framework is internally coherent but does not include animals in the moral calculus.
constraint_indexing:constraint_classification(animal_status_kernel__property_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_status_kernel__property_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_status_kernel__property_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(animal_status_kernel__property_reading, TR),
    TR >= 0.70.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82): Very high. The property reading permits unrestricted use of animals for any owner benefit, constrained only by peripheral anti-cruelty statutes that protect owner interests (market value, consumer confidence, public health), not animal interests. The animal's exclusion from the victim-set is the mechanism that produces high extractiveness — there is no countervailing moral constraint to limit use. The measurement trajectory (0.65 → 0.75 → 0.82) reflects increasing enforcement pressure from rising cultural salience of animal sentience; the property reading requires escalating suppression to maintain its coherence as alternative framings (welfare, abolitionist) gain institutional foothold. Suppression (0.68): Very high. Animals are legally non-persons with no advocacy capacity. The suppression mechanisms include: (a) legal prohibition on direct animal representation (animals cannot testify, sue, or hold property); (b) epistemic suppression of animal subjectivity (treating animals as stimulus-response mechanisms rather than sentient subjects with subjective experience); (c) institutional suppression of alternative moral frameworks (property law's implicit assertion that moral considerability derives only from ownership); (d) normalization through culturally embedded practices (animal agriculture, animal research, animal entertainment) that treat property use as ordinary and uncontested. Theater ratio (0.55): Moderate. Anti-cruelty statutes produce performative concern (the appearance that animal interests are being protected) while leaving property status unchallenged. The theater is not as high as a pure performance regime (piton) because the actual enforcement mechanisms (legal penalties for excessive cruelty) have some bite within the property framework. The theatrical element is that the statutes frame animal welfare as a side-effect of owner property protection, not as a right of the animal itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests different types across observers precisely because beneficiary and victim occupy incompatible structural positions relative to the property framework. The owner benefits; the animal is targeted. The framework is not a failed coordination mechanism — it coordinates ownership rights effectively. It is a succeeding extraction mechanism. The gap reveals that 'fair property system' is coherent only from the beneficiary's perspective; from the victim's perspective, the framework is a snare with no coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the extraction flow. Animal owners hold d ≈ 0.05 (full beneficiary with arbitrage exit → derived f(d) ≈ -0.12 → negative χ, experiencing the constraint as pure benefit). Non-human animals hold d ≈ 0.98 (full target with trapped exit → derived f(d) ≈ 1.40 → maximum χ, experiencing full extraction). The property law system holds d ≈ 0.10 (beneficiary with arbitrage exit → low χ). The welfarist coalition holds d ≈ 0.65 (mixed victim/advocate with constrained exit → moderate positive χ). The analytical observer holds d ≈ 0.73 (observer of extraction → canonical analytical d → baseline χ). No directionality overrides are needed; the structural derivation from beneficiary/victim + exit options produces the correct d values for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by making explicit that the property reading is NOT a coordination mechanism pretending to be pure extraction. It IS a pure extraction mechanism with transparent beneficiaries (owners, property law system) and transparent victims (animals). The mandatrophy dissolves because there is no contradiction between high extractiveness (0.82) and the snare classification — snare is the correct reading when extraction is explicit and suppression is high. The piton perspective (anti-cruelty statutes) represents theater layered on top of snare, not a separate mechanism. The welfare reform perspective (tangled rope) represents an attempt to reframe property as hybrid coordination-extraction, but the base constraint remains snare — the property reading itself excludes animals from the victim-set, making welfare concern peripheral. The abolitionist reading (sibling constraint, not this one) would classify as snare at a different baseline because it treats property status itself as the injustice. The analytical observer's snare classification confirms that property reading is internally coherent but structurally extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_considerability_grounding,
    'Does moral considerability derive exclusively from ownership/legal personhood, or does sentience/suffering provide independent moral relevance regardless of legal status?',
    'Philosophical argument or empirical demonstration that sentience generates moral claims independent of legal recognition. Cross-cultural analysis of societies that recognized animal moral standing before legal property frameworks were established.',
    'If ownership is exclusive ground: property reading remains coherent and no countervailing moral constraint applies. If sentience provides independent ground: property reading forecloses abolitionist reading only if property status is demonstrated to override sentience-based claims — a normative commitment, not a natural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_considerability_grounding, conceptual, 'Whether moral considerability derives from ownership or from sentience independent of legal status').

omega_variable(
    property_right_foundation,
    'Is the right to own animals a foundational moral entitlement, a derivative legal creation, or a contingent institutional arrangement?',
    'Historical analysis: did property-in-animals precede or follow the emergence of formal property law? Philosophical reconstruction of the justificatory basis for property claims over sentient beings.',
    'If foundational: property reading is an axiom not subject to revision. If derivative: property status is contingent on continued justification, vulnerable to reframing if justification collapses. If institutional: property reading coexists with abolitionist only because institutional power sustains the distinction, not because of logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_right_foundation, conceptual, 'Whether property rights over animals are foundational, derivative, or contingent').

omega_variable(
    economic_value_sufficiency,
    'Is economic value a sufficient ground for moral considerability, or does it require supplementation by sentience, autonomy, or other moral criteria?',
    'Counterfactual analysis: would a sentient being with zero economic value still warrant moral consideration? Would a non-sentient commodity with high economic value (e.g., a valuable mineral) warrant the same protections as a high-value animal?',
    'If economic value is sufficient: property reading requires no additional moral framework. If insufficient: the reading must either deny that animals are sentient (empirically false) or revise the sufficiency claim (conceptually unstable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_value_sufficiency, empirical, 'Whether economic value alone provides sufficient moral grounding').

omega_variable(
    enforcement_collapse_risk,
    'As animal sentience becomes culturally salient (through neuroscience, ethology, AI analogies), can the property reading''s enforcement mechanism (legal prohibition on moral consideration) withstand pressure to include animals in the moral calculus?',
    'Longitudinal analysis of legal decisions, legislative debate, and cultural norms surrounding animal status over 50+ years. Measurement of rate of reading shift (property → welfare → abolitionist) across jurisdictions.',
    'If enforcement is brittle: property reading may be foreclosed or collapsed by cultural drift as sentience recognition spreads. If robust: enforcement mechanism is sustainable against pressure (likely requires active suppression of alternative framings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_collapse_risk, empirical, 'Whether property reading''s enforcement mechanism can withstand increasing salience of animal sentience').

omega_variable(
    moral_status_exclusion_axiom,
    'Is the axiom that animals lack moral standing a holdable claim in contemporary philosophy, or has it been overridden by 50 years of animal ethics scholarship?',
    'Systematic review of contemporary philosophy departments, ethicist positions, and professional organization statements (American Philosophical Association, Journal of Animal Ethics, etc.). Measurement of percentage of professional philosophers who accept animal moral standing.',
    'If holdable: property reading remains live within academic philosophy. If overridden: property reading persists only in law and practice, not in philosophical justification — a structural bifurcation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_exclusion_axiom, empirical, 'Whether the axiom of animal moral exclusion remains philosophically holdable or has been overridden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_prop_theater_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(animal_prop_theater_t25, animal_status_kernel__property_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(animal_prop_theater_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(animal_prop_extractiveness_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(animal_prop_extractiveness_t25, animal_status_kernel__property_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(animal_prop_extractiveness_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(animal_prop_suppression_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(animal_prop_suppression_t25, animal_status_kernel__property_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(animal_prop_suppression_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal status kernel admits three structurally distinct readings with incompatible ε values and victim sets. This constraint (property reading, ε=0.82, snare) is linked to welfare reading (ε≈0.50, tangled rope) and abolitionist reading (ε≈0.85, snare/rope depending on perspective). The readings coexist in institutional space but are logically incompatible within any single moral framework. Each reading grounds different beneficiary/victim structures and produces different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
