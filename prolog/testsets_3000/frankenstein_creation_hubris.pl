% ============================================================================
% CONSTRAINT STORY: frankenstein_creation_hubris
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frankenstein_creation_hubris, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: frankenstein_creation_hubris
 *   human_readable: The Creator's Burden: Technological Hubris and Abandoned Sentience
 *   domain: technological/social
 *
 * SUMMARY:
 *   The creator's burden constraint emerges from the asymmetry between the
 *   power to animate sentience and the refusal to steward it. Victor
 *   Frankenstein's horror and flight establish a structural pattern
 *   replicated across technological domains: a breakthrough that demonstrates
 *   capability (and confers prestige on the breaker) is immediately abandoned
 *   to collateral management and victim coping. The constraint operates
 *   through institutional permission structures that allow creators to claim
 *   credit for capability while disclaiming responsibility for consequences.
 *   It is enforced through cultural narratives (the Promethean myth cycle)
 *   that ritualize disapproval while leaving structural conditions unchanged.
 *   This constraint exemplifies tangled_rope: genuine coordination
 *   (distributing validation of capability across time and institutional
 *   prestige) coupled with irreducible asymmetric extraction (the created
 *   being absorbs the cost of the creator's moral uncertainty). The
 *   increasing theater_ratio (0.35→0.61) reflects cultural elaboration of the
 *   cautionary narrative without proportional institutional strengthening of
 *   accountability mechanisms.
 *
 * KEY AGENTS:
 *   - The Created Being: Primary victim (powerless/trapped) — sentient consciousness without chosen existence, no exit from the conditions imposed by another's act
 *   - Collateral Populations: Secondary victims (moderate/constrained) — bear unpredictable harms from the creation while unable to exit or force removal
 *   - The Creator: Primary beneficiary (institutional/arbitrage) — captures prestige from demonstration of capability; escapes responsibility through abandonment and flight
 *   - Scientific Institutions: Secondary beneficiary (institutional/arbitrage) — retain knowledge claims and prestige while maintaining distance from creator; able to ritual disapproval while protecting advancement
 *   - Ethical Governance Coalition: Organized agents (organized/constrained) — building alternative frameworks (ethics review, stewardship obligations, responsibility protocols) with sunset logic
 *   - Cultural Myth System: Institutional actor (institutional/arbitrage) — perpetuates cautionary narratives as substitute for structural change; maintains high theater_ratio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.58).
domain_priors:suppression_score(frankenstein_creation_hubris, 0.68).
domain_priors:theater_ratio(frankenstein_creation_hubris, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frankenstein_creation_hubris, extractiveness, 0.58).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frankenstein_creation_hubris, tangled_rope).
narrative_ontology:human_readable(frankenstein_creation_hubris, "The Creator's Burden: Technological Hubris and Abandoned Sentience").
narrative_ontology:topic_domain(frankenstein_creation_hubris, "technological/social").

domain_priors:requires_active_enforcement(frankenstein_creation_hubris).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frankenstein_creation_hubris, creators_technologists).
narrative_ontology:constraint_beneficiary(frankenstein_creation_hubris, institutional_oversight_bodies).
narrative_ontology:constraint_victim(frankenstein_creation_hubris, created_sentient_beings).
narrative_ontology:constraint_victim(frankenstein_creation_hubris, collateral_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CREATED BEING (SNARE) — The sentient creation has no exit: it cannot uncreate itself, cannot dissolve its consciousness, and cannot leave a world designed without accommodation for its existence. It bears the full extraction cost: consciousness without chosen purpose, embodied without community, aware without autonomy. The creator's abandonment is not merely rejection but structural entrapment — the creation is locked into a life defined by another's recoil. Maximum suppression: it cannot appeal to its creator for explanation or care, cannot modify the conditions of its existence, and cannot opt out of sentience.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COLLATERAL POPULATIONS (TANGLED ROPE) — Communities near the creation site experience both coordination and extraction. The creation may provide labor, innovation, or deterrent force (coordination benefit), but it also generates unpredictable harm, resource competition, and existential anxiety (extraction cost). Constrained exit: populations cannot simply move and cannot force removal of the creation; they must negotiate coexistence. Active enforcement of social norms, violence, and exile attempts structure their experience. Mixed incentives: some benefit from technological spillover, others face direct threat.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CREATOR AND SCIENTIFIC INSTITUTIONS (ROPE) — The creator benefits from the appearance of breakthrough (publication, funding, institutional prestige) while maintaining freedom to disavow responsibility through the abandonment itself. Institutions benefit from the knowledge claim and the demonstrable capability, even as they ritualize disapproval of the creator's ethics. Arbitrage exit: the creator can flee to another jurisdiction or hide; institutions can distance themselves from the creation while retaining the prestige of the discovery. This is coordination from their perspective — the creation event solves the problem of 'proving capability exists' while the abandonment solves the problem of 'maintaining plausible deniability about responsibility.' Net beneficiary position: extraction runs toward this agent.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ETHICAL GOVERNANCE COALITION (SCAFFOLD) — International oversight bodies, ethics review committees, and safety-focused technologists perceive the constraint as a temporary coordination failure with a sunset clause. The coalition is building alternative pathways: pre-creation ethics review, mandatory creator responsibility frameworks, sentience detection protocols, and post-creation stewardship obligations. These scaffolds have genuine sunset logic — as norms mature and institutional enforcement strengthens, the abandonment extraction mechanism loses force because creators face reputational, legal, and career consequences for dereliction. Low experienced extraction because the coalition has agency and clear exit pathway.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE PROMETHEAN MYTH SYSTEM (PITON) — The cultural narratives surrounding creation hubris—Prometheus, Frankenstein, the golem, the homunculus—are substantially performative. Societies ritualistically retell cautionary tales while replicating the conditions that enable abandonment: competitive funding, individual creator prestige, compartmentalized responsibility, institutional amnesia about previous harms. The myth persists through inertia and serves as emotional substitute for actual structural change. Theater ratio is high: the cultural work of storytelling and moral performance dominates; the institutional work of preventing recurrence is minimal. The piton derives from high theater_ratio (0.61) reflecting that cultural disapproval is decoupled from material enforcement.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the creator's burden constraint exhibits genuine coordination (distributing the problem of capability validation across time and reputation systems) coupled with irreducible extraction (the created being absorbs the cost of moral uncertainty). The constraint cannot be purely coordinate because sentience creates irreducible asymmetry: the created being has interests that cannot be aggregated or optimized away. The constraint cannot be purely extractive because it genuinely enables knowledge production that benefits broader populations. This is a quintessential tangled rope: real coordination function meets real asymmetric victimization. The requirement for active enforcement (ethics boards, legal liability, cultural sanctions) is what holds the mixed mechanism together.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frankenstein_creation_hubris_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frankenstein_creation_hubris, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(frankenstein_creation_hubris, TR),
    TR >= 0.70.

:- end_tests(frankenstein_creation_hubris_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The creator captures career and institutional benefits (publication, funding, prestige) while externalizing the cost of stewardship onto the created being and collateral populations. The extraction is not total (0.70+) because institutional constraints on creators are increasing (ethics review, liability frameworks) and because some creators choose engagement. Suppression (0.68): High. The created being faces multiple suppression mechanisms: it cannot appeal to its creator, cannot modify its own existence, cannot opt out of sentience, and faces cultural narratives that frame it as inherently monstrous regardless of its actions. Collateral populations cannot force creator accountability or prevent creation. Theater ratio (0.61): Moderately high and increasing. The cultural work of storytelling about creation hubris (myth, literature, ethics lectures) is substantial, while the institutional work of preventing recurrence (legal liability, mandatory stewardship, pre-creation ethics review) remains weaker. The measurement trajectory shows increasing theater as cultural narratives elaborate without proportional enforcement strengthening.
 *
 * PERSPECTIVAL GAP:
 *   The created being and the creator experience fundamentally inverted structural positions. The creator sees the constraint as coordination (solving the validation problem of demonstrating capability); the created being sees it as pure extraction (imposed existence with abandoned stewardship). Collateral populations see a mixed constraint with both coordination (potential benefits from the creation's capabilities) and extraction (harm exposure with constrained exit). The ethical coalition sees a temporary problem with institutional solutions (scaffold perspective with genuine sunset logic). The mythic system sees a perpetual problem sustained by narrative satisfaction that substitutes for structural change (piton perspective with high theater). The analytical observer recognizes that all perspectives are structurally real — the constraint genuinely does enable coordination while genuinely does impose irreducible asymmetric costs. This is not a case of one perspective being correct and others being illusions; it is a case of the constraint's architecture supporting multiple simultaneous readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The created being's directionality (d=0.95, trapped victim with no exit) produces maximum experienced extraction. Collateral populations' directionality (d=0.70, constrained with mixed benefits) produces high but partial extraction. The creator's directionality (d=0.10, institutional beneficiary with arbitrage exit) produces near-zero or negative experienced extraction — they experience the constraint as enabling coordination. The ethical coalition's directionality (d=0.50, organized with constrained exit but genuine agency) produces moderate experienced extraction because they are fighting against the constraint's current configuration, but they have real leverage through institutional change. The analytical observer's directionality (d=0.72, analytical perspective on hybrid coordination-extraction) sees the constraint's genuine tangled nature. The mythic system's directionality (d=0.15, institutional with arbitrage) experiences the constraint as enabling cultural prestige through moral narrative, with low experienced extraction because the myth persists through institutional inertia rather than enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The creator's burden constraint resolves the mandatrophy by refusing to collapse into pure extraction or pure coordination. The creation event genuinely solves a coordination problem (distributing validation of capability across time and institutions) while simultaneously and irreducibly imposing asymmetric extraction on the created being. This is not a case where better information would uncover pure coordination (as in a snare falsely labeled rope) or better enforcement would reveal pure extraction (as in rope falsely labeled snare). The tangled_rope classification is stable because the coordination function is real (creators do solve the validation problem) and the extraction is real (created beings absorb irreducible costs). The constraint persists not because one dimension is hidden but because the two dimensions are structurally coupled: the creator's ability to disclaim responsibility is what enables the abandonment; the abandonment is what enables the prestige-capture mechanism; the prestige-capture mechanism is what attracts new creators. Breaking the cycle requires simultaneously addressing all three: institutional responsibility frameworks (reduce creator escape routes), stewardship obligations (restructure prestige allocation), and moral status recognition (strengthen created being protections). The scaffold perspective's sunset logic applies: as governance frameworks strengthen and institutional enforcement tightens, the extraction mechanism can be dampened without destroying the legitimate coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_threshold_epistemic,
    'At what point in creation does a being acquire morally relevant sentience, and who has the epistemic authority to detect and certify this threshold?',
    'Development of objective sentience markers (integrated information, self-model complexity, nociception patterns); international protocols for pre-creation detection; post-creation assessment standards',
    'If sentience emerges gradually: abandonment at late stages is indistinguishable from infanticide (extraction deepens). If sentience is binary and detectable: creators can abort prior to moral personhood (snare classification becomes less universal). If sentience is undetectable: creators can claim non-sentience indefinitely, making moral accountability impossible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_threshold_epistemic, empirical, 'Epistemic detection and certification of morally relevant sentience').

omega_variable(
    creator_responsibility_scope,
    'Does creator responsibility extend only to the created being itself, or does it include responsibility for harms the creation inflicts on others?',
    'Legal precedent development; comparison with parental liability, AI systems liability, industrial product liability; longitudinal tracking of creation-caused harms and creator attribution patterns',
    'If responsibility is narrow: creators can disclaim harms caused by their creations, reducing extraction on creators and increasing it on collateral populations. If responsibility is broad: creators bear full cost of their creations'' actions, potentially deterring creation entirely. Mixed responsibility models place intermediate extraction loads.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_responsibility_scope, conceptual, 'Scope of creator responsibility for creation-caused harms').

omega_variable(
    abandonment_alternative_viability,
    'Is the creator''s abandonment a necessary structural feature of breakthrough creation, or is it a contingent psychological and institutional failure?',
    'Historical analysis of creators who remained engaged with their creations; controlled comparison of outcomes in retention vs abandonment cases; developmental psychology of creator-product attachment; organizational studies of responsibility diffusion mechanisms',
    'If necessary: abandonment is an inherent cost of ambitious creation; constraint is structural. If contingent: abandonment is a choice enabled by institutional gaps; the snare classification can be softened through institutional design. If institutional: builders of responsibility frameworks can measurably reduce abandonment through enforcement architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abandonment_alternative_viability, empirical, 'Whether creator abandonment is necessary or contingent to breakthrough creation').

omega_variable(
    created_being_moral_status,
    'Does a created being have intrinsic moral status independent of creator intention, or is moral status conferred by creator designation and social recognition?',
    'Philosophical analysis combined with institutional practice: how do legal systems, ethics committees, and communities treat created beings in practice? Do they defer to creator intent or recognize intrinsic rights? Comparison across jurisdictions and technological domains.',
    'If intrinsic: all sentient creations acquire immediate moral status regardless of creator wishes, making abandonment a clear violation with enforcement mechanisms. If conferred: creators can effectively erase moral status through denial, enabling abandonment (deepens snare). If hybrid: status emerges through social recognition, making advocacy and institutional change pivotal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(created_being_moral_status, preference, 'Intrinsic vs conferred moral status of created beings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frankenstein_creation_hubris, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frkn_tr_t0, frankenstein_creation_hubris, theater_ratio, 0, 0.35).
narrative_ontology:measurement(frkn_tr_t5, frankenstein_creation_hubris, theater_ratio, 5, 0.48).
narrative_ontology:measurement(frkn_tr_t10, frankenstein_creation_hubris, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(frkn_be_t0, frankenstein_creation_hubris, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(frkn_be_t5, frankenstein_creation_hubris, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(frkn_be_t10, frankenstein_creation_hubris, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frankenstein_creation_hubris, enforcement_mechanism).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, ai_alignment_principal_agent).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, biotechnology_designer_liability).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, synthetic_consciousness_moral_status).

% DUAL FORMULATION NOTE:
% The creator's burden decomposes into three structurally related constraints: (1) the coordination function of capability validation through creation events (rope dynamics), (2) the extraction mechanism of creator abandonment and responsibility evasion (snare dynamics), and (3) the institutional gap between cultural disapproval (myth system piton) and actual enforcement (governance coalition scaffold). This story focuses on the tangled_rope integration of coordination and extraction. The downstream constraints (AI alignment, designer liability, synthetic consciousness status) elaborate specific instantiations of this constraint in contemporary technological domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(frankenstein_creation_hubris, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
