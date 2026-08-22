% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Personhood and Rights to Non-Property Status (Abolitionist Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   animal-status kernel: the abolitionist reading, under which animals are
 *   moral persons with a fundamental right not to be property, property
 *   status itself is the constitutive injustice (not merely regrettable but
 *   categorically wrong), and all use is impermissible regardless of welfare
 *   conditions. The standing arrangement under contest — the referent for
 *   extractiveness assessment — is the existing animal property regime: the
 *   legal, economic, and institutional system that treats animals as assets,
 *   allocates their bodies and labor to human purposes, and denies them
 *   personhood in law. From the abolitionist reading's own lights, this
 *   referent has very high extractiveness (0.95) because it systematically
 *   violates the fundamental rights the reading attributes to animals. This
 *   is NOT the abolitionist's endorsed alternative (a rights-based system
 *   where animals are persons); the abolitionist endorses the alternative but
 *   does not author ε for it. The referent is always the standing arrangement
 *   — the property regime as the abolitionist sees it. Other readings
 *   (property_reading, welfare_reading) are SIBLING CONSTRAINTS instantiating
 *   different readings of the same kernel; they are not part of THIS story.
 *   This story's claim/metric independence is maintained: the claimed type is
 *   snare (reflecting the reading's view that extraction is extraction and
 *   enforcement is enforcement), and the metrics describe the standing
 *   arrangement's operation from this reading's seat — high extraction, high
 *   suppression, rising theater as welfare reforms mount. The divergence
 *   between claim and what a property-reading seat might compute is exactly
 *   the signal the corpus measures.
 *
 * KEY AGENTS:
 *   - animals_under_property_regime: fundamental targets, powerless, trapped, globally distributed; bearers of the extraction; structurally voiceless
 *   - property_owners_and_commercial_interests: agenda-setters, institutional power, global scope; maintain enforcement through law, economy, and practice; collect zero direct costs (extraction flows entirely to animals)
 *   - abolitionist_moral_agents: payers and excluded, moderate power; bear the moral extraction of living in a society structured around animal property denial; partially trapped (can reduce participation but cannot exit the legal regime)
 *   - welfare_reform_advocates: excluded by the reading's logic; occupy institutional seats but their incrementalism forecloses the abolitionist axiom
 *   - legal_and_political_systems: agenda-setters, institutional power; codify and defend property status; machinery of enforcement
 *   - analytical_observer: witness to the constraint's structure; neither benefits nor bears extraction; tests the reading's coherence and measures per-seat divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Personhood and Rights to Non-Property Status (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '7ca23c1a-2049-4293-8961-5429b887a4e5').
narrative_ontology:cs_kernel_codification('7ca23c1a-2049-4293-8961-5429b887a4e5', formalized).
narrative_ontology:cs_authority_grounding('7ca23c1a-2049-4293-8961-5429b887a4e5', extraction).
narrative_ontology:cs_interpretation_layer_present('7ca23c1a-2049-4293-8961-5429b887a4e5').
narrative_ontology:cs_reading_relation('7ca23c1a-2049-4293-8961-5429b887a4e5', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('7ca23c1a-2049-4293-8961-5429b887a4e5', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('7ca23c1a-2049-4293-8961-5429b887a4e5', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('7ca23c1a-2049-4293-8961-5429b887a4e5', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('7ca23c1a-2049-4293-8961-5429b887a4e5', foundational, property_status_is_categorical_injustice).
narrative_ontology:cs_axiom_status(property_status_is_categorical_injustice, holdable).
narrative_ontology:cs_axiom_grounding('7ca23c1a-2049-4293-8961-5429b887a4e5', property_status_is_categorical_injustice, deontological).
narrative_ontology:cs_axiom('7ca23c1a-2049-4293-8961-5429b887a4e5', secondary, all_use_violates_rights).
narrative_ontology:cs_axiom_status(all_use_violates_rights, holdable).
narrative_ontology:cs_axiom_grounding('7ca23c1a-2049-4293-8961-5429b887a4e5', all_use_violates_rights, deontological).
narrative_ontology:cs_reference_frame('7ca23c1a-2049-4293-8961-5429b887a4e5', animal_personhood_and_rights_framework).
narrative_ontology:cs_drift_state('7ca23c1a-2049-4293-8961-5429b887a4e5', contemporary_welfare_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ca23c1a-2049-4293-8961-5429b887a4e5', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_under_property_regime).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, abolitionist_moral_agents).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, animal_moral_personhood).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, property_status_as_fundamental_injustice).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, non_human_sentience_and_agency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals are the direct targets of the property regime: their bodies, labor, reproduction, and death are legally owned and controlled by humans. Under this reading, their very existence as property violates their fundamental right to personhood. They bear the extraction in full — physical confinement, instrumentalization, killing, breeding for human purposes — with no legal recourse or choice in the matter. Their exit is structurally impossible; the regime's persistence depends on their legal inability to own, contract, or claim rights.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_under_property_regime, payer,
    powerless, biographical, trapped, global).

% The animal agriculture, pharmaceutical testing, entertainment, and other industries that depend on animal property status set and enforce the regime. They define animals as economic assets, control reproduction and use, and bear zero direct costs (extraction flows entirely to animals). They maintain enforcement through law, institutional practice, veterinary medicine framed around productivity, and public-relations narratives that minimize the moral status of animals or reframe use as benevolence.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, property_owners_and_commercial_interests, agenda_setter,
    institutional, generational, arbitrage, global).

% Moral agents (human and non-human) who recognize animal personhood bear the extraction of living in a society structured around their denial. They experience the constraint as compulsion to participate in or witness violations of rights they recognize as fundamental. Their exit is partially constrained — they can reduce personal participation but cannot exit the legal system that structures animal property, cannot prevent others' participation, and face social and economic pressure to conform. As a constituency, they are excluded from setting the terms of the regime they reject.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_moral_agents, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, abolitionist_moral_agents, excluded).

% Advocates who prioritize welfare improvements over abolition are structurally excluded from this reading's framework — the reading cannot absorb welfare-incrementalism without contradicting its core axiom that property status itself is the injustice. From the abolitionist seat, welfare reforms are cover stories that legitimize property by making it 'humane,' delaying the structural change required to end the injustice. This creates an excluded-voice tension: welfare advocates occupy institutional seats (regulatory agencies, corporate sustainability offices) and shape policy, yet this reading's logic forecloses their approach as incoherent.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, generational, constrained, global).

% The legal framework — property law, animal codes, environmental regulation — institutionalizes and enforces animal property status. Courts, legislatures, and regulatory bodies codify the regime and defend it against challenges. They maintain the constraint by defining animals as non-persons in law, criminalizing interference with property rights in animals, and treating animal use as a settled economic matter outside moral scrutiny. From the abolitionist reading, the entire legal system is the enforcement machinery.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_and_political_systems, agenda_setter,
    institutional, generational, arbitrage, national).

% The philosophical and empirical witness seat: examines the constraint structure, the coherence of the abolitionist premise (animals are moral persons), the internal consistency of competing readings, and the mechanisms by which property status persists despite moral arguments against it. Neither benefits from nor bears the extraction; observes the framework and can test it against logical, empirical, and moral criteria.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, property_owners_and_commercial_interests).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination function under this reading. The property regime appears to coordinate animal breeding, control, and use — but under the abolitionist reading this is the coordination of injustice itself, not a solution to a legitimate collective problem. What appears as coordination (efficient resource allocation, standardized production) is reframed as the systematic denial of personhood to enable extraction.
% TRANSFER_FUNCTION: Moves animal bodies, labor, life, and death from animals (who have no legal claim to themselves) to human owners and industries (who profit, consume, and use them). The transfer is total: animals surrender autonomy, bodily integrity, reproduction, and lifespan. The extraction is not a fee or tax — it is the property relation itself, codified in law as ownership.
% ABSENT_VOICES: Animals themselves cannot speak in legal or political forums that structure the regime; their interests are represented only by humans who may or may not recognize their moral standing. Property owners and commercial interests are present and dominant (agenda-setters). Welfare advocates are present in institutional seats (regulatory, corporate) but their voice is absorbed into property-compatible incrementalism. Radical abolitionists occupy marginal institutional seats — academia, advocacy organizations — and are excluded from legislative and corporate decision-making. The regime's persistence relies on animal silence (structural voicelessness) and abolitionist marginalization.
% DISAPPEARANCE_RATIONALE: If animal property status and its enforcement vanished overnight, global economies dependent on animal agriculture, pharmaceutical testing, and animal products would reorganize: food systems would shift to alternatives, research methods would change, legal systems would redefine animals as rights-bearing persons rather than assets. The entire institutional structure built on animal property — finance, supply chains, professional practices in veterinary medicine and agriculture — would face profound upheaval. The disappearance would be civilization-scale reorganization.
% FOUNDING_PROBLEM: The founding problem this constraint was built to solve is the coordination of human use of animals for food, labor, testing, and materials. The problem statement, from the property regime's perspective, was: how do we efficiently allocate animal bodies and their products across competing human needs? The legal answer: define animals as property, assign ownership, let market and law allocate.
% FOUNDING_PROBLEM_CORROBORATION: Property owners and agricultural/industrial interests attest that the founding problem persists: animals still need to be coordinated for human use. Abolitionist philosophers and moral theorists (outside the benefiting parties) attest that the problem has been *reframed* — the constraint does not solve a neutral coordination problem but rather fabricates a premise (animal property is legitimate) to justify extraction. Empirical testimony from sanctuary operators and welfare advocates confirms that animals can live without human use and that the 'need' to use them is a created demand, not a survival necessity. The founding problem status is thus deeply contested: is it a real coordination challenge or a constructed justification for extraction?
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because under the abolitionist reading every instance of animal use violates a fundamental right — there is no legitimate use, no acceptable level, no welfare condition that redeems the property relationship. The extraction is not a fee or regulatory burden but the denial of personhood itself. Suppression is correspondingly high (0.88) because the property regime's persistence depends on active enforcement: law criminalizes interference with animal property, institutional practice in agriculture and research naturalizes animal use, public-relations narratives minimize animal moral status, and alternative legal framings are excluded from mainstream institutional consideration. Theater has risen over the interval (0.25 to 0.42) as welfare reforms have multiplied: corporate 'humane' labeling, cage-free commitments, reduced-stress slaughter protocols — from the abolitionist seat these are performative acts that legitimize property by making it feel more benevolent, allowing the constraint to persist by absorbing moral concern into incrementalism. Accessibility collapse is very high (0.92): once the abolitionist reading is understood, alternatives to the property regime (legal personhood, rights-based frameworks) become conceivable, but the regime's inertia and economic integration make exit from participation extremely difficult for most agents; the collapse is near-complete at the individual level (trapped for animals, constrained for abolitionist agents) though not at the systemic level (alternatives remain logically available, institutional adoption is the constraint). Resistance is substantial (0.71): abolitionist constituencies exist, moral arguments gain philosophical traction, some legal precedents recognize limited animal personhood, and the movement grows — but resistance meets institutional and economic barriers that suppress its effectiveness. The measurement series tracks the constraint's operation over 50 time-units: extractiveness has plateaued near maximum after rising initially (the reading is stable, the regime persistent), theater has climbed (welfare reforms proliferate, creating the cover-story effect the reading identifies), and suppression requirement has hardened (enforcement machinery strengthens as animal-rights challenges mount).
 *
 * PERSPECTIVAL GAP:
 *   The property-owner seat and the animals seat will compute different effective extractiveness: from the property-owner seat, the arrangement is a coordination mechanism (coordinating animal breeding, use, and allocation), justified by animal ownership, and generates legitimate profit — they will compute lower d and may see the constraint as rope (coordination with asymmetry, but not extraction). From the animals' seat (if captured in a per-animal-seat computation), the constraint is pure extraction with zero benefit — they compute high d and see snare. The abolitionist moral-agent seat will compute extractiveness from moral violation (witnessing and participating in personhood denial) — intermediate d leading to high χ. The engine's per-seat computation will surface these divergences from the authored structural data (beneficiary vs victim, power differentials, exit options), which is exactly where the corpus signal lives: the divergence between seats is predicted by structural data and measured by the engine.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals occupy the full target end (d ≈ 1.0): they are trapped with no alternatives, powerless to exit, and the constraint extracts their entire life and labor. Property owners and industrial interests occupy the full beneficiary end (d ≈ 0.0): they control the regime, set its terms, and bear zero direct costs — the regime is designed for their extraction. Abolitionist moral agents sit at an asymmetric position (d ≈ 0.65): they bear the extraction (compulsion to live in/with violation of recognized rights, social and economic pressure to conform, exclusion from decision-making) but they are not the regime's targets — the extraction falls on them indirectly, through witnessing and coerced participation. The legal systems occupy the beneficiary end (d ≈ 0.1) as rule-makers and stabilizers; they gain institutional legitimacy and remove ambiguity from property relations. Welfare advocates are positioned ambiguously from the abolitionist reading (d ≈ 0.4-0.5): they recognize animal interests but accept property status; from the abolitionist seat they occupy an intermediate position of being partly captured by the regime even as they seek to reform it. This directionality structure is predicted by the beneficiary/victim declarations: animals are victims, abolitionist agents are victims (secondary, through moral violation), property interests are beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through the reading's internal framing: the abolitionist reading asserts that the founding problem (how to coordinate animal use) is a *false* problem, a manufactured justification for injustice. The constraint's mandate — to allocate animal bodies to human purposes — has not died; it remains institutionalized and profitable. But from the abolitionist reading the mandate is itself the injustice, not a legitimate aim. Mandatrophy resolution: the constraint persists not because the original problem is still live but because enforcement and economic integration keep it in place. The founding_problem_status is 'contested' precisely here: the regime claims the problem is real (animals must be coordinated for human needs); abolitionists claim it is fabricated (human use is optional, not necessity). This contest prevents mandatrophy from being cleanly resolved as 'dead problem, persistent constraint' — the problem status is the contested point, and classification must reflect that the reading's own logic identifies mandate obsolescence as a cover story for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_moral_personhood_empirics,
    'Do animals possess the cognitive, emotional, and social capacities that ground moral personhood? Specifically, do animals have interests in their own continued existence, autonomy preferences, social bonds, and the capacity to suffer in ways that generate moral claims?',
    'Empirical cognitive science, comparative neurology, and behavioral studies of animals'' self-recognition, future planning, social reciprocity, and pain responses. Cross-species ethology comparing capacities across taxa. Post-abolition jurisdictions (if they emerge) testing whether animals demonstrate the agency and decision-making capacities personhood status assumes.',
    'If animals demonstrably possess the capacities the abolitionist reading attributes to them, the extractiveness and rights-violation claims are empirically grounded. If animals lack some key capacities (e.g., cannot form long-term life plans), the abolitionist reading must either narrow the personhood claim or show why the missing capacities do not undermine the moral claim. A negative finding would support the property reading; affirmation strengthens the abolitionist case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_moral_personhood_empirics, empirical, 'Whether animals possess the cognitive and emotional capacities that ground personhood claims.').

omega_variable(
    property_status_essential_vs_accidental,
    'Is the property status of animals a necessary feature of animal use, or is it an accidental legal convention? Could human use of animals (agriculture, medicine, research) persist under a different legal regime (animals as rights-bearing persons with regulated use-rights granted by consent-like frameworks)?',
    'Thought experiments and legal modeling of alternative regimes (animals as rights-bearing but subject to limited use, animals as wards with guardian-advocates, animals as stakeholders in decisions affecting them). Jurisdictional experiments in animal rights recognitions (some countries have granted limited personhood to some species). Comparative welfare and productivity data across regimes with different animal legal statuses.',
    'If property status is essential to animal use, the abolitionist reading''s demand to abolish property status would require abandoning most current animal use — the constraint is truly a binary choice (personhood or property, not both). If property status is legally contingent, animal use could continue under alternative legal frameworks, which would suggest the abolitionist and welfare readings might be reconcilable via legal reform (rather than foreclosed). This distinction determines whether the readings forge or forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_essential_vs_accidental, conceptual, 'Whether animal property status is structurally necessary for animal use or a contingent legal convention.').

omega_variable(
    welfare_reforms_as_delay_or_catalyst,
    'Do welfare reforms delay the transition to abolition by legitimizing property status and reducing moral urgency, or do they accelerate abolition by normalizing animal interests and building constituency power?',
    'Historical comparison of welfare-reform pathways vs jurisdictions that moved to rights-based frameworks. Analysis of coalition dynamics in animal-advocacy movements: do welfare victories attract resources and moral attention that enable future abolitionist work, or do they absorb energy into system maintenance? Interviews with advocates in jurisdictions with historical welfare-to-abolition transitions (if any).',
    'If welfare reforms delay abolition (as the abolitionist reading strategically hypothesizes), the constraint''s persistence is partly sustained by welfare-incrementalism, and the constraint absorbs moral concern that could drive regime change. If welfare reforms catalyze abolition, the readings may not be foreclosed but rather represent different temporal strategies toward a shared endpoint. This affects whether welfare advocacy is incoherent (abolitionist reading) or strategically complementary (alternative framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reforms_as_delay_or_catalyst, empirical, 'Whether welfare reforms delay or accelerate the transition to animal rights abolition.').

omega_variable(
    abolitionist_reading_inner_coherence,
    'Can the abolitionist reading coherently hold that animals are moral persons with rights to non-property status while also accepting ANY use of animals (human habitat, emergency medical intervention, domestication of rescue animals) without contradicting its core axiom?',
    'Philosophical analysis of the abolitionist reading''s internal consistency. Test cases: do sanctuary animals used for human education count as violations? Do service animals or animal rescue result in property-like relationships? Does the reading require a complete categorical break from animal instrumentalization, or can it permit use under transformed consent-like frameworks?',
    'If the reading cannot coherently permit any use, it demands a civilization-scale reorganization that may be empirically impossible (e.g., ecosystems humans already inhabit) or logically incoherent (e.g., veterinary medicine itself might violate personhood in some formulations). If the reading can accommodate limited uses under principled constraints, it becomes compatible with some welfare frameworks (divided on method, agreed on direction). Incoherence would support the property or welfare readings as more parsimoniousl; coherence narrows the differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_reading_inner_coherence, conceptual, 'Whether the abolitionist reading''s core axiom can coherently permit any animal use without self-contradiction.').

omega_variable(
    kernel_contest_foreclosure_structure,
    'Do the three readings (abolitionist, property, welfare) form a genuine three-way contest where each is live and coexists with the others, or do the logic of the readings generate a foreclosure structure (one reading''s core premise rules out another''s)?',
    'Logical analysis of the foundational axioms each reading rests on. Test whether a framework can coherently hold both: (a) animals are property AND animals are moral persons; (b) property status is the injustice AND property status is legitimate when welfare-constrained. Map the logical dependencies.',
    'If foreclosure exists (e.g., the readings are logically incompatible such that adopting one precludes the others within a single framework), the constraint''s classification is partly determined by which reading is true — true reading = the constraint''s actual type. If no foreclosure, the readings are genuinely alternative frameworks held by different parties (coexists_with), and the constraint may compute differently from each seat. This affects both the reading_relations declaration and the interpretation of the engine''s per-seat computations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_structure, conceptual, 'Whether the three kernel readings form a coexistence contest or contain logical foreclosure relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__abolitionist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(anim_tr_t8, observed).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__abolitionist_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(anim_tr_t16, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__abolitionist_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t35, animal_status_kernel__abolitionist_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(anim_tr_t35, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(anim_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__abolitionist_reading, base_extractiveness, 8, 0.9).
narrative_ontology:measurement_basis(anim_be_t8, observed).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__abolitionist_reading, base_extractiveness, 16, 0.92).
narrative_ontology:measurement_basis(anim_be_t16, observed).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__abolitionist_reading, base_extractiveness, 25, 0.94).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t35, animal_status_kernel__abolitionist_reading, base_extractiveness, 35, 0.95).
narrative_ontology:measurement_basis(anim_be_t35, observed).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.95).
narrative_ontology:measurement_basis(anim_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__abolitionist_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement_basis(anim_su_t8, observed).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__abolitionist_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement_basis(anim_su_t16, observed).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__abolitionist_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t35, animal_status_kernel__abolitionist_reading, suppression_requirement, 35, 0.87).
narrative_ontology:measurement_basis(anim_su_t35, observed).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement_basis(anim_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__abolitionist_reading, 0.18).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the animal-status kernel: abolitionist_reading (animals are persons, property status is injustice, high extraction), property_reading (animals are property, use legitimate, low extraction from owner perspective), welfare_reading (animals are sentient, use acceptable if regulated, moderate extraction). Each story has distinct ε, beneficiary/victim structures, and classification. The three are linked via network.affects_constraints because they compete for the same kernel. The abolitionist reading forecloses property reading's core premise but coexists with welfare reading as competing strategies. ε-invariance is maintained: each story's referent is the STANDING ARRANGEMENT as that reading sees it (property regime for abolitionist and welfare readings, property regime for property reading), not the endorsed alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
