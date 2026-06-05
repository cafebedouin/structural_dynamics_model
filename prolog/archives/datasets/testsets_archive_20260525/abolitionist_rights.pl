% ============================================================================
% CONSTRAINT STORY: abolitionist_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abolitionist_rights, []).

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
 *   constraint_id: abolitionist_rights
 *   human_readable: Animal Abolition: Rights-Holder Status vs. Property Regime
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The abolitionist animal rights reading treats the property regime
 *   governing sentient animals as structurally incompatible with animals'
 *   interests in autonomy, freedom from pain, social connection, and
 *   species-characteristic behavior. This is ONE reading of the contested
 *   kernel of animal moral status. The abolitionist reading does not deny
 *   that animals suffer (welfare advocates agree) or that some humans benefit
 *   from animal control (economic agents agree). Rather, it makes a
 *   philosophical claim: property status itself — the legal right to own and
 *   use an animal's body — violates the animal's interests in a way that
 *   cannot be remedied by welfare regulation alone. The constraint appears as
 *   a snare from the victim perspective (trapped animals with interests the
 *   regime denies), as tangled_rope from welfare advocates (mixed
 *   coordination and extraction), as rope from industrial beneficiaries (pure
 *   coordination of profitable production), as piton from welfare bureaucrats
 *   (inert legitimation theater), and as a false-summit mountain from those
 *   who naturalize property dominance as inevitable. The theater_ratio (0.68)
 *   reflects that welfare regulations create the appearance of moral
 *   consideration while leaving the core extraction mechanism (appropriation
 *   of animal bodies) intact. The suppression (0.82) is both structural
 *   (animals have no legal standing, no political capacity) and actively
 *   enforced (abolitionist direct action is criminalized; movement advocates
 *   face economic and social penalties). The extractiveness (0.78) reflects
 *   that the regime extracts near-total value from animal subjects while
 *   denying them any claim on the benefits or any voice in the arrangement.
 *
 * KEY AGENTS:
 *   - Sentient Animals: Primary victim (powerless/trapped) — biological dependence on human systems; no legal standing; interests in autonomy and freedom denied by property status
 *   - Ecosystem Integrity: Secondary victim (powerless/trapped) — property regime enables habitat destruction, pollution, and biodiversity collapse; no mechanism for ecosystem interests to register
 *   - Human Property Claimants: Primary beneficiary (institutional/arbitrage) — agriculture, industry, research, entertainment all depend on animal appropriation; high exit cost (would require abandoning profitable sectors)
 *   - Welfare Reform Movement: Moderate organized agent (moderate/constrained) — seeks to reduce suffering within property framework; has some institutional footholds but faces suppression and co-optation
 *   - Welfare Bureaucracy: Institutional actor (organized/constrained) — maintains performative protections; benefits from legitimating the system without abolishing it
 *   - Abolitionist Movement: Organized agent (organized/constrained) — seeks fundamental rights status for animals; faces criminalization, economic pressure, intellectual marginalization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalization of property regime as inherent to human dominance; abolitionist reading reveals this as contingent legal construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abolitionist_rights, 0.78).
domain_priors:suppression_score(abolitionist_rights, 0.82).
domain_priors:theater_ratio(abolitionist_rights, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abolitionist_rights, extractiveness, 0.78).
narrative_ontology:constraint_metric(abolitionist_rights, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(abolitionist_rights, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abolitionist_rights, snare).
narrative_ontology:human_readable(abolitionist_rights, "Animal Abolition: Rights-Holder Status vs. Property Regime").
narrative_ontology:topic_domain(abolitionist_rights, "applied_ethics/legal_philosophy/political_economy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abolitionist_rights, human_property_claimants).
narrative_ontology:constraint_victim(abolitionist_rights, sentient_animals).
narrative_ontology:constraint_victim(abolitionist_rights, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAPTIVE ANIMAL (SNARE) — Trapped by physical confinement and legal property status. Has interests (freedom from pain, autonomy, social connection, species-appropriate behavior) that the property regime systematically violates. No exit mechanism; no legal standing to claim or defend interests. Maximum extraction experienced as deprivation of capacity to live according to own nature. This perspective classifies the constraint as pure extraction with maximal suppression.
constraint_indexing:constraint_classification(abolitionist_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE WELFARE REFORM ADVOCATE (TANGLED ROPE) — Sees genuine coordination problem (humans and animals both benefit from reduced suffering) AND asymmetric extraction (animals bear disproportionate costs; reforms are often performative theater). Constrained by entrenched property norms and economic interests, but has some institutional footholds (animal protection laws, welfare standards). Perceives both the mixed function of welfare regulation and its limits.
constraint_indexing:constraint_classification(abolitionist_rights, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE AGRICULTURAL/INDUSTRIAL COMPLEX (ROPE) — Experiences the property regime as pure coordination: organizing production, distributing resources, enabling profitable use. The constraint (animals as property) solves the collective action problem of controlling animal labor, products, and bodies. Net beneficiary with high arbitrage capacity (exit the constraint only means ceasing profitable use, which is economically irrational within the system). From this perspective, the constraint is a successful coordination mechanism.
constraint_indexing:constraint_classification(abolitionist_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE NATURALIZATION VIEW / FALSE SUMMIT (MOUNTAIN) — From a civilizational/universal perspective, the property regime can be framed as emergent from human dominance of cognition and rationality: animals lack the capacity to form complex social contracts or institutions, therefore property relations reflect a natural hierarchy. The constraint appears as a law of nature (inevitable given species capabilities). However, this perspective naturalizes what the abolitionist reading reveals as a contingent legal and economic construction. The false summit detection engine will flag this classification as reclassifiable to snare: the beneficiaries (human economic interests) make the 'natural law' claim to defend the extraction.
constraint_indexing:constraint_classification(abolitionist_rights, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE WELFARE BUREAUCRACY (PITON) — Maintains the performative apparatus of animal protection law (slaughter regulations, housing standards, transport rules) that creates the appearance of moral consideration while leaving the property regime intact. Theater_ratio is high because compliance is often pro-forma; the core extraction (appropriation of animal bodies and products) continues unchallenged. The bureaucracy itself has become inertial — welfare advocates often serve to legitimize the system rather than abolish it. Piton classification reflects the degradation of protective function into legitimation theater.
constraint_indexing:constraint_classification(abolitionist_rights, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abolitionist_rights_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abolitionist_rights, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abolitionist_rights, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abolitionist_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abolitionist_rights, TR),
    TR >= 0.70.

:- end_tests(abolitionist_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The property regime extracts near-total value from animal subjects. Animals cannot keep any product of their labor, body, or reproduction; humans claim ownership of all. The extractiveness is maximal in the sense that the regime claims property right to the entire animal body and its capacities. The value 0.78 (rather than 0.95) reflects that this is measured as experienced deprivation relative to interests; some animals may not be conscious of the full scope of what is denied, which technically lowers measured extraction relative to the pure institutional claim. But from the abolitionist perspective, this is precisely what makes the property regime pernicious — it is designed to suppress the animal's consciousness of its own interests. Suppression (0.82): Very high. Animals face both structural and enforced suppression: structurally, they have no legal capacity to claim rights, no political voice, no ability to organize; actively, the abolitionist movement faces criminal penalties for direct action, economic retaliation, and intellectual marginalization. The suppression is extreme because it prevents not just resistance but even the naming of the constraint as problematic. Theater ratio (0.68): Moderately high. Welfare regulations (humane slaughter, housing standards, transport rules) create the appearance that animals are morally considered while the property regime remains intact. The theater is functional — it allows the constraint to persist by managing moral objections. The ratio is not maximal (0.95) because significant genuine welfare improvements do occur; some regulations do reduce suffering. But the theater is substantial because these improvements are advertised as proof that the system works, when the abolitionist reading insists that no amount of welfare can remedy the incompatibility of interests with property status.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading generates a maximum perspectival gap: the same property regime is experienced as rope (pure beneficial coordination) by those who profit, snare (pure extraction with no escape) by the animals, tangled_rope (mixed coordination and extraction) by welfare advocates, piton (degraded inert theater) by the bureaucracy, and false-summit mountain (naturalized inevitability) by those who seek to universalize it. The gap is not due to disagreement about facts (suffering is real, property benefits are real, suppression exists) but due to fundamental disagreement about the moral weight of animal interests. Abolitionism weights those interests absolutely; other perspectives weight them relative to human benefits or welfare. This difference in weighting produces entirely different classifications of the same constraint from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's high extractiveness (0.78) is derived from the observation that the property regime appropriates the entire animal body, behavior, and reproductive capacity while denying any share of benefits to the animal. The suppression (0.82) is derived from the combination of structural powerlessness (animals have no legal standing) and active enforcement (abolitionist movement faces criminalization). The direction of extraction is unambiguous: from animals to humans. Beneficiaries are those who profit from animal appropriation (food, labor, research, entertainment industries). Victims are the animals themselves and ecosystems degraded by industrial animal agriculture. The constraint's power to extract depends on maintaining animals as property rather than rights-holders — if animals achieved legal rights status, the extraction mechanism would collapse. Therefore, the constraint is a snare: its existence depends entirely on suppressing the alternative (abolitionist rights status) and maintaining animals in a condition where they cannot exercise agency or claim their own interests.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved in this constraint by recognizing that the abolitionist reading is internally consistent: it identifies a genuine extraction mechanism (property appropriation) with minimal coordination function (the regime is not required to solve a collective action problem; it exists primarily to enable profitable use). The welfare reading would see a tangled_rope (genuine coordination plus extraction), but the abolitionist reading rejects this framing as mystification. The snare classification stands because: (1) extractiveness is high (0.78+); (2) suppression is high (0.82); (3) chi ≥ 0.66 across all non-beneficiary perspectives; (4) the constraint's primary function is extraction, not coordination; (5) beneficiaries benefit precisely from denying alternative frameworks (rights status) that would eliminate the constraint. This is a resolved mandatrophy case: no ambiguity remains about whether the constraint is primarily extractive or primarily coordinative. It is extractive. The only ambiguity is whether animals actually have the interests the abolitionist framework attributes to them — that is an omega variable, not a mandatrophy problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_kernel_reading,
    'Is this constraint instantiating the abolitionist reading (animals as rights-holders with interests incompatible with property status) or the welfare reading (animals as sentient beings deserving of protection within property frameworks)?',
    'Clarity of agent classification: in abolitionist reading, animals are in victim set and use itself is violation; in welfare reading, animals are victims only of improper treatment, not of the property relationship itself. The readings produce different ε values (0.78 abolitionist vs ~0.35 welfare) and different beneficiary/victim structures.',
    'If abolitionist reading is adopted: constraint is snare with ε ≥ 0.66, no coordination function. If welfare reading dominates: constraint becomes tangled_rope with lower ε, significant coordination function (protecting animals from cruelty). The reading choice is not empirical but philosophical — which moral framework (rights-based or welfare-based) governs how we classify the relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_kernel_reading, conceptual, 'Which kernel reading governs: abolitionist rights vs. welfare-regulated use').

omega_variable(
    sentience_threshold_ambiguity,
    'Which animals count as sentient enough to possess the incompatible interests that trigger abolitionist classification? Where is the boundary of the victim set?',
    'Empirical investigation of neurological capacity for pain and suffering across species; philosophical analysis of what counts as ''interest'' and ''incompatibility''. Different thresholds produce different victim populations and thus different aggregate extractiveness values.',
    'If threshold includes only mammals/birds: victim set is restricted; extractiveness may be moderately high but not maximal. If threshold extends to all animals with nociceptors (including fish, cephalopods, arthropods): victim set expands dramatically; extractiveness approaches 0.85+. If threshold includes plants or ecosystems: victim set becomes universal, but claim of animal-specific rights becomes unclear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_ambiguity, empirical, 'Sentience threshold determining extent of victim set').

omega_variable(
    incompatibility_criterion,
    'What makes an animal''s interests ''incompatible'' with property status? Is the incompatibility absolute (property status violates any use of the animal) or relative (some uses compatible, others not)?',
    'Clarification through abolitionist theory: if animal has interests in autonomy, freedom from pain, social bonds, and species-characteristic behavior, then most uses humans make of animals violate these interests. The incompatibility is structural, not incidental. Test: would the animal consent if capable of informed choice? If answer is no for the central uses (food production, labor, experimentation, entertainment), then interests are indeed incompatible.',
    'If absolute incompatibility: extractiveness remains 0.78+, snare classification stands. If relative incompatibility: some uses (e.g., companionship) might be compatible, lowering effective extraction; constraint becomes mixed (tangled_rope) rather than pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incompatibility_criterion, conceptual, 'Whether animal interests are absolutely or relatively incompatible with property use').

omega_variable(
    resistance_mechanism_source,
    'Is the high suppression (0.82) primarily structural (legal regime prevents exit for animals) or enforced (humans actively suppress abolitionist movement)? Or both?',
    'Distinguish structural suppression (animals have no legal standing, no capacity to organize politically, biological dependence on human systems) from movement suppression (criminalization of sabotage, intellectual marginalization of abolitionist philosophy, economic pressure on advocates). Both are present; the distinction matters for understanding whether abolition requires legal reform, social movement, or fundamental epistemic shift.',
    'If primarily structural: abolition requires legal reform granting animal standing or reframing property rights. If primarily enforced: abolition requires political struggle and narrative shift. If both: abolition faces both structural and active resistance, making it a higher-extraction constraint to escape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resistance_mechanism_source, empirical, 'Source of suppression: structural or actively enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abolitionist_rights, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abol_tr_t0, abolitionist_rights, theater_ratio, 0, 0.55).
narrative_ontology:measurement(abol_tr_t50, abolitionist_rights, theater_ratio, 50, 0.62).
narrative_ontology:measurement(abol_tr_t100, abolitionist_rights, theater_ratio, 100, 0.68).
narrative_ontology:measurement(abol_tr_t25, abolitionist_rights, theater_ratio, 25, 0.6).

% Extraction over time
narrative_ontology:measurement(abol_be_t0, abolitionist_rights, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(abol_be_t50, abolitionist_rights, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(abol_be_t100, abolitionist_rights, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(abol_be_t25, abolitionist_rights, base_extractiveness, 25, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abolitionist_rights, resource_allocation).
narrative_ontology:affects_constraint(abolitionist_rights, welfare_regulated_use).
narrative_ontology:affects_constraint(abolitionist_rights, pure_property).

% DUAL FORMULATION NOTE:
% The animal moral status kernel decomposes into three constraint stories: (1) abolitionist_rights (this file, ε=0.78, snare) — animals as rights-holders with incompatible interests; (2) welfare_regulated_use (ε~0.35, tangled_rope) — animals as sentient beings deserving protection within property; (3) pure_property (ε~0.05, rope) — animals as objects without inherent interests. These are not the same constraint viewed from different angles; they are structurally different constraints generated from different readings of the contested kernel. Each reading produces different ε values and different beneficiary/victim structures. The abolitionist reading is upstream in the causal order — it claims that the welfare reading is a compromise that legitimizes the core extraction mechanism rather than abolishing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abolitionist_rights, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
