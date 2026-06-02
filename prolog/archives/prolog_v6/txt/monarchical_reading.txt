% ============================================================================
% CONSTRAINT STORY: monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monarchical_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monarchical_reading
 *   human_readable: Hereditary Succession as Legitimacy Mechanism
 *   domain: political_philosophy/constitutional_theory/authority_studies
 *
 * SUMMARY:
 *   The hereditary succession constraint grounds political authority in
 *   bloodline continuity. This is ONE READING of a contested kernel
 *   (sovereign_legitimacy). The monarchical reading claims that legitimacy
 *   derives from hereditary lineage: rule is justified because the ruler is
 *   the lawful heir. This reading produces a tangled_rope classification at
 *   the analytical level — it solves the succession coordination problem
 *   (answers 'who rules next?' with reference to a predetermined,
 *   bloodline-based rule) while simultaneously creating asymmetric extraction
 *   (the ruling lineage captures benefits of predetermined authority while
 *   non-hereditary populations are locked out of succession regardless of
 *   competence). The constraint's extractiveness (0.58) reflects asymmetric
 *   rule-making power that flows toward the hereditary beneficiaries. The
 *   suppression (0.68) reflects institutional barriers to challenging or
 *   exiting hereditary rule. The theater_ratio (0.55) reflects that
 *   hereditary legitimacy claims require ongoing ritual affirmation
 *   (coronations, genealogical recitation, dynastic mythology) to maintain
 *   perceived naturalness. Over the historical interval modeled (0-300 time
 *   units), extractiveness and theater have both risen as the constraint
 *   required greater institutional infrastructure to maintain hereditary
 *   legitimacy claims against competing readings (republican and
 *   mixed-constitutional).
 *
 * KEY AGENTS:
 *   - Ruling Lineage: Primary beneficiary (institutional/arbitrage) — derives legitimacy automatically from birth; predetermined succession; minimal performance accountability
 *   - Excluded Non-Hereditary Population: Primary victim (powerless/trapped) — no mechanism to obtain legitimacy through competence; ruled without participation in succession mechanism
 *   - Capable Non-Heir: Secondary victim (powerless/trapped) — individual capability irrelevant; birth status alone determines succession eligibility
 *   - Lower Hereditary Nobility: Mixed position (moderate/constrained) — benefits from hereditary immunity but faces suppression from hierarchy within the lineage
 *   - Republican Coalition: Organized challenger (organized/constrained) — works toward institutional replacement of hereditary mechanism; constrained by cost of constitutional reform
 *   - Mixed-Constitution Reformer: Institutional modifier (institutional/mobile) — builds parallel legitimacy sources (consent, representation, constitutional rights) that bound hereditary authority without eliminating it
 *   - Genealogical Determinism Advocate: Analytical naturalizer (analytical/analytical) — claims hereditary succession is immutable natural law; engine detects as false summit (naturalizing contingent institutional arrangement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monarchical_reading, 0.58).
domain_priors:suppression_score(monarchical_reading, 0.68).
domain_priors:theater_ratio(monarchical_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monarchical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monarchical_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monarchical_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monarchical_reading, tangled_rope).
narrative_ontology:human_readable(monarchical_reading, "Hereditary Succession as Legitimacy Mechanism").
narrative_ontology:topic_domain(monarchical_reading, "political_philosophy/constitutional_theory/authority_studies").

domain_priors:requires_active_enforcement(monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(monarchical_reading, fixed_text).
narrative_ontology:cs_authority_grounding(monarchical_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(monarchical_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monarchical_reading, ruling_lineage).
narrative_ontology:constraint_beneficiary(monarchical_reading, hereditary_nobility).
narrative_ontology:constraint_victim(monarchical_reading, non_hereditary_populations).
narrative_ontology:constraint_victim(monarchical_reading, excluded_capable_claimants).
narrative_ontology:constraint_victim(monarchical_reading, collective_governance_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED NON-HEREDITARY POPULATION (SNARE) — No mechanism for exit from rule by hereditary succession. Cannot obtain legitimacy through competence, capability, or consent because legitimacy is locked to bloodline. Full extraction: rules imposed without participation in succession or authority structure. Suppression is structural and generational — each cohort inherits the constraint without opportunity to revise the mechanism itself.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAPABLE NON-HEIR (SNARE) — An individual demonstrably more competent than the designated heir remains excluded from succession by birth status alone. Biographical-scale extraction: the individual's entire career is constrained by lineage-based exclusion. No mechanism within the hereditary system permits capability to override birth. Maximum experienced extraction for this agent — full suppression, no arbitrage.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LOWER HEREDITARY NOBILITY (TANGLED ROPE) — Derives legitimacy from bloodline but occupies subordinate position within the hereditary hierarchy. Benefits from the hereditary mechanism (legitimacy independent of performance) but constrained by rank within the lineage. Mixed extraction: enjoys hereditary immunity but faces suppression from higher ranks. Exit is constrained — can abandon noble status at cost of social exclusion, but cannot exit the hereditary system itself.
constraint_indexing:constraint_classification(monarchical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RULING LINEAGE (ROPE) — Primary beneficiary. Legitimacy derives automatically from bloodline; succession is predetermined; rule-making capacity is legally anchored in heredity. From this perspective, hereditary succession is a pure coordination mechanism solving the succession problem: who shall rule? Answer: the next in line. Arbitrary but efficient. The ruling lineage experiences no suppression — they have created the system to be transparent and self-reinforcing to themselves. Chi is near zero or negative (net benefit).
constraint_indexing:constraint_classification(monarchical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN COALITION (TANGLED ROPE) — Organized agents (revolutionary movements, constitutional reformers, representative bodies) see hereditary succession as a constraint that solves coordination (answers 'who rules?') but through an illegitimate mechanism (bloodline rather than consent/capability). Generational-scale exit options: the coalition can work toward institutional replacement (constitutional reform, revolution), but these are expensive and carry generational cost. Extraction is moderate — the system constrains but does not eliminate their political agency.
constraint_indexing:constraint_classification(monarchical_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MIXED-CONSTITUTION REFORMER (SCAFFOLD) — Institutional actor that operates within existing frameworks while building alternative legitimacy sources (constitutional protections, legislative power, representative bodies). Sees hereditary succession as a temporary problem being bounded and delegitimized by parallel structures. Suppression is moderate because the reformer has institutional position. Exit is generational because the constraint cannot be removed unilaterally — it requires constitutional negotiation. The scaffold perspective reflects that many monarchies transition to constitutional frameworks where hereditary succession persists but is constrained by other legitimacy mechanisms (consent of parliament, constitutional rights, separation of powers).
constraint_indexing:constraint_classification(monarchical_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: NATURAL LAW / GENEALOGICAL DETERMINISM (MOUNTAIN) — Civilizational-scale perspective claiming that hereditary succession is an immutable natural law: inheritance of property is natural, inheritance of position is a natural extension, bloodline continuity is the foundation of all social order. Lineage is irreducible. This perspective naturalizes the hereditary mechanism as inherent to human kinship. The engine's false-summit detector will evaluate whether beneficiaries exist (they do) and whether this classification is actually naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(monarchical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monarchical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monarchical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monarchical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The hereditary reading extracts asymmetric rule-making authority toward the lineage while locking out capable non-heirs and non-hereditary populations. The extraction is not total (some constraints on ruler power exist in practice; some consultation with nobility occurs) and is justified within the reading's frame as coordination necessity. But the asymmetry is substantial — the population bears the cost of hereditary rule while the lineage captures the benefit of predetermined authority. The value reflects that extractiveness is measured as the effective power asymmetry when all dimensions are accounted for. Suppression (0.68): Moderate-high. The constraint operates through institutional barriers (legal prohibition on non-hereditary succession, genealogical authentication requirements, cultural sanctification of bloodline) and epistemic suppression (the hereditary reading naturalizes itself through myth and ritual, making alternatives literally unthinkable within the frame). Suppression is high but not total because alternative readings exist (republican, mixed-constitutional) and some populations can imagine exit paths through institutional reform. Theater ratio (0.55): Moderate. The hereditary mechanism requires continuous ritual affirmation (coronations, genealogical recitation, dynastic mythology, heraldic ceremony) to maintain the perceived naturalness of bloodline-based legitimacy. However, the ratio is not as high as piton (0.70+) because the hereditary reading does provide a genuine coordination function (it does answer 'who rules next?' with a clear, predetermined rule). The theater is integral to the mechanism's operation, not an overlay on degraded function. The theater ratio increases over time because maintaining hereditary claims against republican and constitutional alternatives requires greater institutional investment in legitimacy performances.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The ruling lineage (institutional/arbitrage) experiences the hereditary mechanism as pure coordination — a simple, transparent rule answering the succession problem. They perceive no extraction or suppression, only efficiency. The excluded non-hereditary population (powerless/trapped) experiences maximum snare: no exit, no agency, ruled by birth status alone. The capable non-heir (powerless/biographical) experiences temporal snare: their entire biographical window is constrained by hereditary exclusion. The lower hereditary nobility (moderate/constrained) experiences mixed extraction and benefit — they gain hereditary legitimacy but lose rank-based agency. The republican coalition (organized/generational) experiences tangled_rope: the mechanism does coordinate succession but through an illegitimate (non-consensual) method; they see an exit path through generational institutional reform. The mixed-constitutional reformer (institutional/mobile) experiences scaffold: the hereditary mechanism is being bounded and delegitimized by parallel institutions that do not yet fully replace it. The genealogical determinist (analytical/civilizational) risks false-summit classification by naturalizing the hereditary reading as immutable law. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural position within (or outside) the hereditary system.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's relationship to the hereditary extraction flow. The ruling lineage benefits directly — they are the beneficiary group (d ≈ 0.05). The excluded non-hereditary population bears full extraction cost with zero exit options — they are the victim group with trapped exit (d ≈ 0.95). The capable non-heir is excluded despite competence — victim with trapped biographical exit (d ≈ 0.90). The lower hereditary nobility benefits from the hereditary mechanism but is constrained by rank — mixed beneficiary/victim status with constrained exit (d ≈ 0.50). The republican coalition works against the mechanism but has some political agency — victim with constrained generational exit (d ≈ 0.60). The mixed-constitutional reformer has institutional position and mobile exit (can implement constitutional reform) — moderate victim with mobile exit (d ≈ 0.45). The genealogical determinist has analytical position but is arguably identity-locked to the hereditary frame that naturalizes the mechanism — (analytical, identity_locked) at d ≈ 0.70. The directionality derivation chain prioritizes structural data (beneficiary/victim declarations) and then applies exit-option scaling. The result is a perspectival spectrum from d ≈ 0.05 (net beneficiary, arbitrage) to d ≈ 0.95 (net victim, trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the hereditary reading genuinely does provide a coordination function (succession mechanism) while simultaneously producing extraction (asymmetric rule-making power). The classification as tangled_rope is not a compromise or average — it reflects the actual structure. The coordination function is real: hereditary succession does answer 'who rules next?' with a clear, predetermined rule that eliminates many sources of dispute. The extraction is real: the mechanism systematically privileges the ruling lineage over capable non-heirs and non-hereditary populations. Both functions coexist. The mandatrophy is resolved by recognizing that real institutional constraints often solve coordination problems AND create extraction simultaneously. This is the definition of tangled_rope: genuine coordination function PLUS asymmetric extraction. The false-summit perspective (genealogical determinism) attempts to deny the extraction by naturalizing the entire mechanism as immutable law. The engine detects this as false summit because identifiable beneficiaries exist (the ruling lineage) — the naturalness claim is contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_mechanism_ambiguity,
    'Does the hereditary reading constitute a single logically coherent legitimacy mechanism, or does it confuse succession mechanism (answering ''how is the next ruler selected?'') with legitimacy grounding (answering ''why should this selection method be binding?'')?',
    'Examine the hereditary reading''s own justificatory claims: does it provide a reason WHY bloodline should determine legitimacy (beyond ''it has traditionally worked'') or does it merely describe HOW bloodline succession operates? Contrast with republican reading''s explicit appeal to consent; contrast with mixed-constitutional reading''s appeal to constitutional constraint.',
    'If succession and legitimacy are conflated: the hereditary reading is internally incoherent and should be reclassified as a snare with false-summit characteristics (naturalized extraction). If succession mechanism is separable from legitimacy grounding: the hereditary reading is coherent but contestable, and the omega resolves toward ''preference'' rather than ''empirical''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_mechanism_ambiguity, conceptual, 'Whether hereditary succession provides independent legitimacy grounding or merely describes succession mechanism').

omega_variable(
    sibling_reading_force,
    'This constraint is one reading (monarchical_reading) of the contested kernel sovereign_legitimacy. The sibling readings are republican_reading and mixed_constitutional_reading. How should the engine evaluate whether this monarchical reading is a true alternative or a cover story for snare-class extraction?',
    'Cross-reading comparison: if the republican reading and mixed_constitutional_reading produce lower extractiveness from the powerless perspective, the hereditary reading''s extractiveness is at least partly observable-dependent (violates ε-invariance). If all three readings produce similar extractiveness when properly decomposed, the kernel is genuinely contested rather than one reading being extractive and others revealing it.',
    'If monarchical reading''s higher extractiveness is reading-dependent (not intrinsic): reclassify to snare with note that the reading choice itself is the extraction mechanism. If reading-independent: the kernel genuinely supports multiple equilibria with different extraction rates per reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_force, conceptual, 'Whether extractiveness is intrinsic to the hereditary mechanism or dependent on choice of reading').

omega_variable(
    lineage_stability_vs_extraction,
    'The suppression value (0.68) reflects barriers to exit from hereditary rule. But how much of this suppression is functional (maintaining lineage stability, which ALL readings must solve) versus extractive (benefit flowing asymmetrically to the ruling lineage)?',
    'Decompose suppression into functional and extractive components: functional suppression = costs of ensuring orderly succession regardless of reading; extractive suppression = costs specific to the hereditary reading that would disappear under republican or mixed-constitutional readings. Empirical measurement: compare suppression levels across regimes with different succession mechanisms.',
    'If functional suppression > 0.40 and extractive suppression < 0.28: the constraint is more rope-like than tangled_rope. If extractive suppression > 0.35: the constraint''s extraction is robust to succession mechanism choice and represents a core hereditary feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lineage_stability_vs_extraction, empirical, 'Decomposition of suppression into functional versus extractive components').

omega_variable(
    performance_legitimacy_decoupling,
    'The hereditary reading makes legitimacy independent of ruler competence or capability. Is this decoupling a feature (stable governance regardless of individual ruler quality) or a bug (systematic incompetence when heir is incapable)?',
    'Historical comparative analysis: do hereditary monarchies show different failure rates than capability-based selection mechanisms? Do periods of capable heirs show stronger legitimacy? Do periods of incapable heirs show weaker legitimacy despite the hereditary mechanism''s claim to independence from performance?',
    'If decoupling is genuinely stable (legitimacy persists through incompetent heirs): validates the hereditary reading''s coordination function. If decoupling fails empirically (legitimacy collapses when heir is incapable): the hereditary reading''s claim to decouple performance from legitimacy is false, and it functions as snare-class extraction masked as stable governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_legitimacy_decoupling, empirical, 'Whether legitimacy truly decouples from ruler capability under hereditary mechanism').

omega_variable(
    contested_kernel_structure,
    'This constraint instantiates the monarchical reading of the kernel sovereign_legitimacy. The kernel is contested by sibling readings (republican_reading, mixed_constitutional_reading). Is the kernel itself fundamentally underdetermined, or do the readings differ only on empirical facts about which mechanism works best?',
    'Examine whether the readings disagree on: (a) factual claims (does hereditary succession actually work? is republican selection empirically stable?) — empirical disagreement; (b) value claims (should legitimacy derive from bloodline, consent, capability?) — normative disagreement; (c) fundamental frames (what counts as legitimacy at all?) — conceptual disagreement. Categorize the kernel debate accordingly.',
    'If empirical: the kernel is resolvable by historical evidence. If normative: the kernel reflects different value commitments and no single reading is ''correct''. If conceptual: the readings operate within incommensurable frames and the kernel marks a genuine structural ambiguity in authority theory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_structure, conceptual, 'Whether the sovereign_legitimacy kernel is empirically, normatively, or conceptually contested').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monarchical_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monarch_tr_t0, monarchical_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(monarch_tr_t100, monarchical_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(monarch_tr_t300, monarchical_reading, theater_ratio, 300, 0.62).

% Extraction over time
narrative_ontology:measurement(monarch_be_t0, monarchical_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(monarch_be_t100, monarchical_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(monarch_be_t300, monarchical_reading, base_extractiveness, 300, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(monarchical_reading, 0.18).
narrative_ontology:affects_constraint(monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(monarchical_reading, mixed_constitutional_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel is instantiated in three separate constraint stories, one per reading. Each reading produces different ε, different beneficiary/victim structure, and different classification. The monarchical_reading (this file) is the hereditary-succession version. The republican_reading decomposes authority into consent-based legitimacy. The mixed_constitutional_reading models hereditary succession bounded by constitutional constraint. All three stories are linked via network.affects_constraints to indicate they are alternative framings of a single contested kernel rather than independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monarchical_reading, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
