% ============================================================================
% CONSTRAINT STORY: pure_property
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pure_property, []).

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
 *   constraint_id: pure_property
 *   human_readable: Animals as Pure Economic Property
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the pure_property reading of the contested
 *   animal_moral_status kernel. Under this reading, non-human animals lack
 *   independent moral standing and are classified as economic resources whose
 *   use is justified by human ownership and benefit maximization. The
 *   constraint operates through legal property codification that denies
 *   animals' structural capacity to claim rights, organize resistance, or
 *   contest confinement and extraction. The beneficiary class is the
 *   collection of industries (agriculture, pharmaceutical testing, food
 *   production) that extract biological value from animals without
 *   compensation or consent. The victim is the captive animal population,
 *   which experiences maximum suppression through physical confinement and
 *   legal powerlessness. The theater_ratio (0.48) reflects the increasing
 *   deployment of welfare labeling, enrichment marketing, and
 *   humane-treatment narratives that obscure the baseline structure while
 *   leaving property status intact. The extractiveness trajectory (0.62 →
 *   0.68) shows modest accumulation as industries intensify production
 *   efficiency while maintaining welfare theater's plausible deniability.
 *
 * KEY AGENTS:
 *   - Non-human animals: Primary victim (powerless/trapped) — no legal standing, no exit capacity, subject to confinement and use at owner discretion
 *   - Agricultural industries: Primary beneficiary (institutional/arbitrage) — extract biological utility, reproductive capacity, and labor without negotiation or compensation
 *   - Food production sector: Secondary beneficiary (institutional/arbitrage) — capture consumer demand through property-based supply chains; suppress cost information about extraction
 *   - Pharmaceutical research establishment: Secondary beneficiary (institutional/arbitrage) — use animals for testing without consent; extract medical knowledge while suppressing alternative methodologies
 *   - Consumers: Moderate actor (moderate/constrained) — benefit from cheap animal products while bearing suppressed knowledge of extraction mechanism
 *   - Animal welfare reformers: Organized actor (organized/constrained) — attempt marginal constraint through regulatory intervention; see sunset possibility but face entrenched property logic
 *   - Abolitionists: Organized actors (organized/constrained) — contest entire property framework; see welfare measures as entrenchment rather than progress
 *   - Analytical observer: Civilizational context (analytical/analytical) — risks naturalizing contingent legal classification as inherent biological fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pure_property, 0.68).
domain_priors:suppression_score(pure_property, 0.72).
domain_priors:theater_ratio(pure_property, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pure_property, extractiveness, 0.68).
narrative_ontology:constraint_metric(pure_property, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pure_property, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pure_property, snare).
narrative_ontology:human_readable(pure_property, "Animals as Pure Economic Property").
narrative_ontology:topic_domain(pure_property, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(pure_property).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pure_property, '7b3c690d-d9bb-4169-94da-5f9a2beb1329').
narrative_ontology:cs_created_at('7b3c690d-d9bb-4169-94da-5f9a2beb1329', '').
narrative_ontology:cs_kernel_codification('7b3c690d-d9bb-4169-94da-5f9a2beb1329', formalized).
narrative_ontology:cs_authority_grounding('7b3c690d-d9bb-4169-94da-5f9a2beb1329', extraction).
narrative_ontology:cs_interpretation_layer_present('7b3c690d-d9bb-4169-94da-5f9a2beb1329').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pure_property, agricultural_industries).
narrative_ontology:constraint_beneficiary(pure_property, pharmaceutical_research).
narrative_ontology:constraint_beneficiary(pure_property, food_production_sector).
narrative_ontology:constraint_beneficiary(pure_property, pharmaceutical_testing_establishments).
narrative_ontology:constraint_victim(pure_property, non_human_animals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTIVE ANIMAL (SNARE) — Fully trapped with no structural exit option. Lacks legal standing to contest confinement, extraction of reproductive capacity, or lethal use. Experiences maximum suppression through physical incapacity to organize, exit, or claim legal protection. No coordination function from this perspective — pure extraction of biological utility.
constraint_indexing:constraint_classification(pure_property, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AGRICULTURAL INDUSTRY (ROPE) — Experiences the constraint as stable coordination mechanism. Property law enables capital accumulation, supply chain predictability, and efficient labor-to-output conversion. The industry benefits from animals' lack of legal standing — no negotiation, no work-stoppages, no liability claims. Extraction is complete but appears as neutral economic efficiency from this position.
constraint_indexing:constraint_classification(pure_property, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSUMER (TANGLED ROPE) — Moderate power; constrained exit (dietary change, supply switching, cost barriers). Benefits from cheap animal products while bearing cognitive dissonance cost of knowing extraction mechanism. Some coordination (standardized supply, food safety regulations) coexists with extraction (animals treated as inputs, not moral patients). Theater present in labeling (free-range, humane) that obscures extent of baseline confinement and control.
constraint_indexing:constraint_classification(pure_property, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WELFARE REFORMERS (SCAFFOLD) — Organized agents (animal advocacy groups, regulatory bodies, institutional reform advocates) see the constraint as temporarily enforceable but facing sunset pressure. Welfare regulations, enrichment requirements, and use-restriction laws represent marginal compression of suppression and extractiveness — not abolition of property status, but binding the constraint's scope. From this view, the pure property reading has a generational timer: as moral intuitions shift and alternative technologies emerge, absolute property claims will become legislatively unsustainable. However, this is aspirational; the actual sunset gate (regulatory success rate, lag time between advocacy and enforcement) remains contested.
constraint_indexing:constraint_classification(pure_property, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ABOLITIONISTS (SNARE) — Organized agents (animal rights philosophers, liberation activists, legal scholars) see pure property classification as the core extractive mechanism itself. From this position, the snare is not softened by welfare measures — it is reproduced by them. Welfare regulations entrench the property relationship by making it appear bearable, tempering the internal contradictions that would force systemic change. Extraction remains at maximum; suppression includes cognitive suppression (moral licensing through welfare theater). This perspective sees the constraint not as a coordination mechanism but as systematic denial of standing.
constraint_indexing:constraint_classification(pure_property, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW / CIVILIZATIONAL VIEW (MOUNTAIN) — From a civilizational perspective, some constraints on animal independence are 'natural' — predation, resource scarcity, intra-species hierarchy. This view renders property status as an extension of these natural limits: humans, as dominant species, naturally command resource allocation including animal bodies. The constraint appears immutable from this perspective. However, this is a false-summit candidate: the civilizational view naturalizes what is actually a specific legal/economic choice (property codification) contingent on power asymmetry, not on unchangeable natural law. The engine's false summit detection will flag this if beneficiaries are declared.
constraint_indexing:constraint_classification(pure_property, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pure_property_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pure_property, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pure_property, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pure_property, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pure_property_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The pure property classification enables complete extraction of animal biological value without consent or compensation. The industry captures reproductive capacity, labor (work animals), body parts (food, leather, fur), organs (pharmaceutical), and medical data through testing — all channels of extraction flow unidirectionally toward human beneficiaries. Animals receive input costs (feed, housing) without choice, and output is claimed as owner property. The extractiveness is not maximal (0.75+) because some welfare regulations create minor friction and some animals retain some autonomy within confinement. Suppression (0.72): High. Multi-layered. Structural suppression: animals lack legal standing to contest ownership, cannot exit confinement, cannot organize collective resistance, cannot claim property rights in themselves. Cognitive suppression: human institutional actors deploy multiple framings that deny animals' capacity for suffering (denial of sentience), deny relevance of sentience (property rights override moral status), or frame extraction as natural (predation logic applied to domestication). Theater ratio (0.48): Moderate-low. The constraint does not rely primarily on theatrical compliance — property law provides direct legal enforcement. However, theater increases over time as welfare labeling (free-range, grass-fed, humane certification) becomes marketing standard. These labels obscure baseline confinement and selective breeding while maintaining appearance of ethical concern. The trajectory from 0.32 to 0.48 reflects increasing public discomfort with extraction, met by theater expansion rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The pure property reading creates the deepest possible perspectival gap by design: it excludes the primary victim from moral standing. The animal cannot see the constraint as anything other than snare because it has no theoretical possibility of seeing coordination or benefit (no standing to negotiate, no capacity to exit, no legal agency). The industry sees rope — stable, mutually beneficial coordination of resource use. The analytical observer at civilizational scope risks seeing mountain — natural law — but this is a false summit: property law is a contingent institutional choice, not a feature of nature. The gap cannot be bridged within the pure property reading itself; it can only be resolved by moving to an alternative reading (welfare_regulated_use or abolitionist_rights) that grants animals some standing or acknowledges moral status independent of property law.
 *
 * DIRECTIONALITY LOGIC:
 *   The pure property reading produces directionality values that are extreme in magnitude. Animals as victims + trapped exit → d ≈ 0.98 (maximum target status) → f(d) ≈ 1.42 → χ amplified. Industries as beneficiaries + arbitrage exit → d ≈ 0.05 (full beneficiary status) → f(d) ≈ -0.12 → χ suppressed or negative. The asymmetry is maximal because the constraint is explicitly designed to create asymmetry: legal standing is granted to human owners and denied to animals. Animals cannot appear in the beneficiary set by definition — their structural position is victim-only. Industries cannot appear in the victim set because they control property definition itself. The directionality derivation does not require override because the structural relationships are explicit in law. Every agent's exit options are determined by their property relationship to the animals: owners have full arbitrage (can sell, repurpose, cull); animals have no exit (own body is property).
 *
 * MANDATROPHY ANALYSIS:
 *   The pure property reading resolves mandatrophy by making extraction and coordination mutually exclusive from the animal's perspective. There is no hybrid classification — the constraint is snare from the victim's view because coordination requires mutual benefit or consent, which property law explicitly denies. The industry's rope classification is not a counter-example; it reveals that rope and snare can be two readings of the same constraint structure from different power positions. The constraint is mandatrophy-resolved within the pure property frame: it is a pure extraction mechanism with no coordination component from the victim's perspective. The theater ratio's rise suggests that cognitive dissonance is driving welfare theater, but theater does not change the underlying classification — it is a response to reputational pressure, not a functional shift from pure extraction to hybrid coordination. The false-summit mountain view is diagnostically important: it reveals that claims of 'natural' animal property status are institutionally sustained, not naturally inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_sentience_boundary,
    'Which animals possess morally relevant sentience, and does that sentience status create standing obligations independent of property law?',
    'Neuroscientific evidence of pain perception, preference hierarchies, social bonding; cross-species comparison of neural correlates with human consciousness; philosophical framework for consciousness-based moral standing',
    'If wide sentience boundary (all vertebrates, cephalopods): property status becomes indefensible as moral claim. Constraint reclassifies to tangled_rope or snare with animals in victim set. If narrow boundary (humans only, or consciousness-requiring): property status remains coherent. If ambiguous: omega remains unresolved and constraint stays snare pending epistemological clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_sentience_boundary, empirical, 'Sentience threshold for moral standing').

omega_variable(
    kernel_reading_contest,
    'Is this constraint instantiating the pure_property reading of the animal_moral_status kernel, or should animals be classified under welfare_regulated_use or abolitionist_rights readings?',
    'Legal framework comparison (property law vs animal welfare statutes vs abolitionist precedents); institutional commitment analysis (which reading is entrenched in practice vs aspiration); temporal measurement of regulatory scope drift toward welfare or abolition',
    'Pure property reading: ε=0.68, snare from victim perspective, mountain from natural law view, rope from industry. Welfare regulated: ε=0.42, tangled_rope, welfare regulations bound extraction. Abolitionist: ε=0.75, snare intensified, property mechanism itself is the extraction. Three readings produce different ε and different victim/beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of animal_moral_status kernel is instantiated').

omega_variable(
    alternative_protein_disruption,
    'Do cultured meat, plant-based alternatives, and cellular agriculture provide genuine technological discontinuity, or do they represent market segmentation that leaves animal extraction intact for cost-sensitive sectors?',
    'Cost trajectory analysis (when cultured/plant alternatives reach price parity); market capture analysis (which animal products are actually displaced vs additive); regulatory adoption rates for alternative protein sectors',
    'If genuine discontinuity: scaffold perspective confirmed, sunset is structural. Pure property constraint becomes economically unsustainable within 20-30 years, reclassifying to piton (degraded). If segmentation: property constraint persists indefinitely in cost-minimizing sectors, ε remains high, scaffold sunset is aspirational only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_protein_disruption, empirical, 'Whether alternative proteins disrupt or entrench animal property status').

omega_variable(
    suppression_structural_vs_cognitive,
    'Is the suppression (0.72) primarily structural (animals lack physical capacity to organize, exit, or claim rights) or cognitive (human institutional actors actively deny animals'' capacity for suffering, deny standing, frame extraction as natural)?',
    'Institutional discourse analysis (explicit vs implicit denials of sentience); cross-cultural comparison of animal status frameworks; historical track record of animals'' rights claims (where animals have formal standing, do outcomes shift?)',
    'If primarily structural: suppression would drop if animals gained legal standing, regardless of technological change. If primarily cognitive: suppression would persist even with legal standing if institutional actors deny its relevance. Mixed suppression suggests both mechanisms require intervention for constraint to degrade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_cognitive, empirical, 'Structural vs cognitive suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pure_property, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pure_prop_tr_t0, pure_property, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pure_prop_tr_t5, pure_property, theater_ratio, 5, 0.4).
narrative_ontology:measurement(pure_prop_tr_t10, pure_property, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(pure_prop_be_t0, pure_property, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(pure_prop_be_t5, pure_property, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(pure_prop_be_t10, pure_property, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pure_property, resource_allocation).
narrative_ontology:boltzmann_floor_override(pure_property, 0.12).
narrative_ontology:affects_constraint(pure_property, welfare_regulated_use).
narrative_ontology:affects_constraint(pure_property, abolitionist_rights).

% DUAL FORMULATION NOTE:
% Pure property reading is one of three structurally distinct constraints on animal_moral_status kernel. The pure_property reading (this file) establishes the baseline property framework (ε=0.68). The welfare_regulated_use reading (sibling story) adds regulatory constraint on extraction (ε=0.42, tangled rope). The abolitionist_rights reading (sibling story) contests property status entirely (ε=0.80, snare with inverted standing). All three readings operate in the same legal/moral domain but produce incompatible classifications because they disagree on the fundamental question: do animals possess standing? Pure property: no. Welfare regulated: partial. Abolitionist: yes. These are not alternative measurements of one constraint; they are alternative framings of a contested kernel, producing different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
