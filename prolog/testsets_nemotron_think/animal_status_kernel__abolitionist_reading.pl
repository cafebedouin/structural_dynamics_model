% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Regime (Abolitionist Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_status_kernel. The kernel — the question of animals' moral and
 *   legal status — admits three coherent readings: property_reading (animals
 *   are property, full stop), welfare_reading (animals are sentient property
 *   with welfare protections), and abolitionist_reading (animals are moral
 *   persons with a right not to be property). This story authors the
 *   abolitionist reading's analysis of the standing arrangement: the global
 *   property regime that treats animals as ownable, tradable, killable
 *   resources. From this reading's lights, the regime is a snare: it extracts
 *   massively from animals (ε=0.87), suppresses all alternatives
 *   (suppression=0.92), and maintains a performative welfare layer
 *   (theater_ratio=0.42) that legitimizes the extraction. The claimed_type is
 *   snare — the abolitionist reading sees no genuine coordination function
 *   for animals, only extraction. The property_reading and welfare_reading
 *   are sibling constraints (other files in the family); this story does not
 *   describe them but declares structural relations via
 *   cs_structure.reading_relations and network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.87).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.92).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Regime (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '69cdb831-32f8-4274-98c4-3a49148e97e3').
narrative_ontology:cs_kernel_codification('69cdb831-32f8-4274-98c4-3a49148e97e3', distributed).
narrative_ontology:cs_authority_grounding('69cdb831-32f8-4274-98c4-3a49148e97e3', distributed).
narrative_ontology:cs_reading_relation('69cdb831-32f8-4274-98c4-3a49148e97e3', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('69cdb831-32f8-4274-98c4-3a49148e97e3', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('69cdb831-32f8-4274-98c4-3a49148e97e3', foundational, animals_have_right_not_to_be_property).
narrative_ontology:cs_axiom_status(animals_have_right_not_to_be_property, holdable).
narrative_ontology:cs_axiom_grounding('69cdb831-32f8-4274-98c4-3a49148e97e3', animals_have_right_not_to_be_property, deontological).
narrative_ontology:cs_axiom('69cdb831-32f8-4274-98c4-3a49148e97e3', foundational, property_status_is_inherent_injustice).
narrative_ontology:cs_axiom_status(property_status_is_inherent_injustice, holdable).
narrative_ontology:cs_axiom_grounding('69cdb831-32f8-4274-98c4-3a49148e97e3', property_status_is_inherent_injustice, deontological).
narrative_ontology:cs_reference_frame('69cdb831-32f8-4274-98c4-3a49148e97e3', animals_as_moral_persons_with_basic_rights).
narrative_ontology:cs_drift_state('69cdb831-32f8-4274-98c4-3a49148e97e3', contemporary_animal_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('69cdb831-32f8-4274-98c4-3a49148e97e3', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_users).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, agricultural_commodity_traders).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animal_users).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, property_rights_absolutism).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, welfare_regulation_sufficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Billions of sentient beings held as legal property, bred, confined, and killed for human purposes. They bear the full cost of the property regime — their lives, liberty, and bodies are the resource extracted. No legal standing, no exit, no voice in the institutions that classify them as property. Resistance takes the form of individual struggle, escape attempts, and species-typical distress behaviors that the system treats as management problems.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals, payer,
    powerless, biographical, trapped, global).

% Individual consumers and small-scale users who purchase animal products and services. They benefit from cheap access to animal-derived goods (food, clothing, companionship, entertainment) whose prices externalize the costs onto animals. They pay indirectly through taxes subsidizing animal agriculture and research, and through health/environmental externalities. Exit is mobile — they can choose plant-based alternatives, but social infrastructure, subsidies, and cultural normalization create friction.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_users, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_users, payer).

% Concentrated corporate actors — industrial animal agriculture, pharmaceutical testing, fur/leather, entertainment — that capture the vast majority of economic value extracted from animals. They write the regulations that govern their own operations (regulatory capture), control the narrative through marketing and lobbying, and can shift production across jurisdictions to avoid welfare costs. Their exit is arbitrage-grade: they move capital to where the property regime is most permissive.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Universities, pharmaceutical companies, and funding agencies that rely on animals as research tools. They benefit from a legal and ethical framework that treats animal use as a default necessity rather than a rights violation requiring justification. Their exit is constrained by scientific paradigm lock-in, funding structures, and regulatory requirements that mandate animal testing — though emerging alternatives (organoids, AI modeling, human-relevant methods) are slowly expanding exit options.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, constrained, global).

% Global commodity traders (Cargill, ADM, Bunge, etc.) and financial instruments that treat animal lives as fungible commodities. They benefit from the property regime's legal infrastructure that enables financialization of animal bodies — futures contracts, livestock as collateral, trade agreements locking in market access. Their exit is pure arbitrage: they shift capital instantly across species, geographies, and asset classes as profitability dictates.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, agricultural_commodity_traders, beneficiary,
    institutional, generational, arbitrage, global).

% Rights-based advocates and legal theorists (Francione, Regan, Cochrane, and organizations like Nonhuman Rights Project) who argue the property regime is fundamentally unjust and must be abolished, not regulated. They set the abolitionist agenda through litigation, theory, and public education. Their exit is analytical — they can leave the movement but the structural analysis they advance remains available as a reading of the kernel. They bear reputational and professional costs for holding a marginalized position.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, analytical, global).

% Large animal protection organizations (HSUS, PETA, Mercy For Animals, CIWF) that pursue incremental welfare reforms within the property regime. They are structurally excluded from the abolitionist reading's framework because their theory of change accepts property status as a given and seeks only to constrain its worst excesses. They command vast resources and public attention, which the abolitionist reading argues diverts energy from abolition. Their exit is constrained by organizational identity, donor expectations, and institutional legitimacy within the current system.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reformers, excluded,
    organized, biographical, constrained, global).

% Legal scholars, economists, and political philosophers who defend animals as property — either as a natural law position (animals lack moral standing) or a pragmatic one (property regime enables efficient resource allocation). They hold institutional power through law schools, courts, and policy advisory roles. Their exit is analytical: they engage the abolitionist reading only to rebut it, not from within its framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, property_rights_theorists, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The property regime coordinates human access to animal bodies and labor across food, research, clothing, entertainment, and companionship sectors. It provides a unified legal framework for ownership, trade, liability, and inheritance of animal-as-property, reducing transaction costs for human users. This is a resource_allocation coordination function — but it coordinates exclusively among human beneficiaries; animals are the resource, not participants.
% TRANSFER_FUNCTION: Moves animal lives, reproductive capacity, labor, bodily substances (milk, eggs, wool, blood, organs), and behavioral compliance from animals to human users and industries. The transfer is backed by state enforcement (property law, anticruelty statutes that exempt standard industry practices, ag-gag laws, veterinary mandates). The consideration flows opposite: money, subsidies, legal protection, and social license flow from users/industries to the regulatory apparatus that maintains the regime.
% ABSENT_VOICES: Animals themselves — the primary victims — are structurally voiceless in human legal and political institutions. They cannot sue, vote, lobby, or testify. Future generations of animals (who will be born into the regime) are absent. Wild animals displaced by animal agriculture's land use are absent. The abolitionist reading argues that the property regime's legitimacy depends on this enforced silence; if animals could speak, the regime would collapse.
% DISAPPEARANCE_RATIONALE: If the property regime vanished overnight — animals recognized as legal persons with a right not to be property — the global food system, biomedical research paradigm, fashion industry, and entertainment sector would undergo immediate, catastrophic reorganization. Land use would shift (77% of agricultural land currently used for livestock), capital would flee animal industries, legal systems would need new frameworks for human-animal relations, and cultural practices would face existential crisis. The world would not stay the same; the property regime is load-bearing for the current human-animal order.
% FOUNDING_PROBLEM: The property regime was built to solve the coordination problem of allocating animal-derived resources among competing human claimants in early agricultural and pre-industrial societies. It established clear title, transfer rules, and dispute resolution for living beings treated as capital assets — enabling tax collection, inheritance, trade, and war mobilization (cavalry, draft animals). The regime's original function was coordinating human-human relations *through* animals as medium, not coordinating human-animal relations.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (e.g., Kenneth Pomeranz, James C. Scott) document that property-in-animals emerged with early states for taxation and control, not animal welfare. Anthropologists (e.g., David Graeber) show alternative human-animal relations (hunting-gathering, pastoralism with different property logics) that did not require chattel status. Legal scholars (Francione, Wise) argue the original coordination problem — allocating scarce animal labor in pre-industrial economies — is obsolete; modern plant-based and cellular agriculture can meet nutritional needs without animal property. No scholar outside the beneficiary set (animal industries, property theorists) defends the founding problem as live.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-maximal because the property regime extracts the entirety of animals' lives — their bodies, reproductive systems, labor, and liberty — for trivial or substitutable human ends (taste pleasure, fashion, curiosities, redundant research). The 0.87 reflects the abolitionist reading's assessment: virtually every animal use violates the basic right not to be property. Suppression is higher still (0.92) because the regime deploys the full force of law (property statutes, anticruelty exemptions, ag-gag, veterinary licensing, trade agreements) to prevent exit — animals cannot leave, advocates cannot buy their freedom at scale, and alternative systems are legally and economically disadvantaged. Theater_ratio=0.42 captures the welfare regulation layer: anticruelty laws, humane slaughter acts, cage-free mandates — real enough to be cited as 'progress' but structurally incapable of challenging the property status that makes extraction possible. The metrics describe the property regime's operation, not the abolitionist alternative (which would have ε≈0 for animals).
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from this structural data. From the animal seat: snare (high χ, trapped, no coordination). From animal_industries seat: rope or tangled_rope (low χ, genuine coordination among human users, active enforcement benefits them). From abolitionist_advocates seat: the constraint appears as a mountain of injustice — a structural evil masquerading as natural order. The welfare_reformer seat is the critical tension: they experience the regime as a tangled_rope (coordination via welfare standards, but extraction persists) and their presence legitimizes the regime. The engine's per-seat computation captures this divergence; the authored claimed_type (snare) reflects the abolitionist reading's structural judgment that the regime's *primary* function is extraction from animals, not coordination for humans.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are full targets (d→1.0): they are the source of all extracted value, have zero exit, and the constraint's persistence depends on their total subordination. Animal_users are near-beneficiaries (d→0.15): they receive cheap animal products subsidized by animal suffering and public funds; their exit to plant-based options is mobile but socially frictioned. Animal_industries and commodity_traders are deep beneficiaries (d→0.0): they capture the rent, write the rules, and hold arbitrage-grade exit. Biomedical_research sits at d≈0.25: they benefit from default access to animal bodies but face growing paradigm pressure from alternatives. Abolitionist_advocates are analytical observers (d=0.5 by definition) — they bear professional costs but the constraint does not extract from them structurally. Welfare_reformers and property_theorists are excluded seats: they occupy the discourse space but their frameworks (welfare regulation, property defense) are not coordinated by the property regime — they are either its legitimating cover (welfare) or its ideological anchor (property theory).
 *
 * MANDATROPHY ANALYSIS:
 *   The property regime's founding problem (coordinating human access to animal labor in pre-industrial economies) is dead — modern economies can meet human needs without animal property. Yet the regime persists and has intensified (extractiveness rose from 0.78 to 0.87, theater from 0.1 to 0.42). This is classic mandatrophy: the original coordination function has atrophied, but the extraction function has hypertrophied. The welfare layer (theater) expanded precisely as the founding problem died, serving to legitimate an arrangement that no longer serves its declared purpose. The abolitionist reading identifies this as the core strategic insight: welfare reforms do not solve the mandatrophy; they deepen the theater. The mandate (property status) has outlived its function; the resolution is abolition, not regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the animal_status_kernel a single constraint with three readings, or three distinct constraints sharing a label?',
    'Apply the ε-invariance test: if measuring the property regime''s extraction from animals yields ε≈0.87 (abolitionist), ε≈0.15 (property_reading: animals as property is not extraction but rightful ownership), and ε≈0.45 (welfare_reading: regulated use reduces but does not eliminate extraction), then ε is reading-indexed — the kernel label covers three structurally distinct constraints. Confirm by checking whether each reading''s ε is stable across observables within that reading.',
    'If three distinct constraints, each gets its own story (as authored here). If one constraint, the framework would need a measurement-basis parameter — which violates ε-invariance. The decomposition into three stories is the correct modeling choice per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel label denotes one constraint or a constraint family.').

omega_variable(
    welfare_reform_impact,
    'Do welfare reforms (cage-free, humane slaughter, enrichment requirements) delay abolition by legitimizing the property regime, or advance it by expanding the moral circle and building institutional capacity?',
    'Longitudinal analysis of jurisdictions with strong welfare laws (EU, UK, California) vs. weak ones: track abolitionist movement growth, public opinion on animal rights, industry consolidation, and alternative protein investment. Compare to historical analogies (child labor reforms → abolition of child labor; slave welfare codes → abolitionism).',
    'If welfare reforms delay abolition, the welfare_reading''s constraint is a snare-enabler (theater masking extraction). If they advance abolition, the welfare_reading is a scaffold (transitional coordination toward abolition). This determines whether the welfare_reading''s claimed_type (tangled_rope or scaffold) is structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_impact, empirical, 'Strategic effect of welfare reforms on abolitionist trajectory.').

omega_variable(
    animal_agency_measurement,
    'How to measure resistance and suppression for agents (animals) who cannot articulate preferences in human institutional terms?',
    'Ethological literature on avoidance behaviors, escape attempts, stereotypic behaviors, self-harm, and physiological stress markers in confined animals. Cross-reference with liberation events (sanctuary releases, accident escapes) to observe revealed preference. Develop a proxy metric for ''suppression experienced'' from behavioral data.',
    'If animal resistance is systematically higher than 0.68, the regime''s suppression is even more total than measured. If resistance is lower (due to learned helplessness, selective breeding for docility), the suppression metric may overstate active coercion vs. internalized subordination. This feeds the suppression_mechanism_ambiguity omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_agency_measurement, empirical, 'Measuring resistance and suppression for non-linguistic victims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.92) primarily structural (legal barriers, physical confinement) or internalized (selective breeding for docility, learned helplessness, maternal deprivation normalizing confinement)?',
    'Post-exit suppression trajectory: observe animals rescued to sanctuaries — if suppression behaviors (fear, passivity, inability to exercise species-typical agency) persist after physical barriers are removed, reclassify as partially internalized. Compare wild vs. domesticated conspecifics.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the animals carry the suppression with them after exit. This would increase the abolitionist reading''s assessed extractiveness (χ) for the animal seat, as internalized suppression is an extraction technology that survives formal abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animal victims.').

omega_variable(
    property_reading_foreclosure,
    'Does the abolitionist reading''s foundational axiom (animals_have_right_not_to_be_property) logically foreclose the property_reading in any single framework, or do they coexist as competing ideologies?',
    'Analyze whether a legal system could simultaneously recognize animals as property (for some purposes) and as rights-holders (for others) without contradiction. Examine existing hybrid regimes (e.g., trust law for pets, anti-cruelty statutes) — do they instantiate a stable middle ground or an unstable contradiction?',
    'If forecloses, the two readings cannot coexist in one framework — the kernel admits a binary split. If coexists_with, the kernel is permanently contested with no logical resolution, only political struggle. This determines the reading_relation: forecloses vs. coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_reading_foreclosure, conceptual, 'Logical relationship between abolitionist and property readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_abolitionist_tr_t1800, animal_status_kernel__abolitionist_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1850, animal_status_kernel__abolitionist_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1900, animal_status_kernel__abolitionist_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1950, animal_status_kernel__abolitionist_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1980, animal_status_kernel__abolitionist_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2000, animal_status_kernel__abolitionist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2024, animal_status_kernel__abolitionist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(animal_status_abolitionist_be_t1800, animal_status_kernel__abolitionist_reading, base_extractiveness, 1800, 0.78).
narrative_ontology:measurement(animal_status_abolitionist_be_t1850, animal_status_kernel__abolitionist_reading, base_extractiveness, 1850, 0.8).
narrative_ontology:measurement(animal_status_abolitionist_be_t1900, animal_status_kernel__abolitionist_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(animal_status_abolitionist_be_t1950, animal_status_kernel__abolitionist_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement(animal_status_abolitionist_be_t1980, animal_status_kernel__abolitionist_reading, base_extractiveness, 1980, 0.89).
narrative_ontology:measurement(animal_status_abolitionist_be_t2000, animal_status_kernel__abolitionist_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(animal_status_abolitionist_be_t2024, animal_status_kernel__abolitionist_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_abolitionist_su_t1800, animal_status_kernel__abolitionist_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(animal_status_abolitionist_su_t1850, animal_status_kernel__abolitionist_reading, suppression_requirement, 1850, 0.88).
narrative_ontology:measurement(animal_status_abolitionist_su_t1900, animal_status_kernel__abolitionist_reading, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement(animal_status_abolitionist_su_t1950, animal_status_kernel__abolitionist_reading, suppression_requirement, 1950, 0.93).
narrative_ontology:measurement(animal_status_abolitionist_su_t1980, animal_status_kernel__abolitionist_reading, suppression_requirement, 1980, 0.92).
narrative_ontology:measurement(animal_status_abolitionist_su_t2000, animal_status_kernel__abolitionist_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(animal_status_abolitionist_su_t2024, animal_status_kernel__abolitionist_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__abolitionist_reading, 0.15).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three in the animal_status_kernel family. The abolitionist_reading analyzes the property regime as a snare (ε=0.87). The property_reading analyzes the same regime as a rope (ε≈0.15, coordination among owners). The welfare_reading analyzes it as a tangled_rope (ε≈0.45, coordination + regulated extraction). The ε values differ by reading because the referent (property regime) is assessed from different structural positions — this is the ε-invariance principle in action: ε is a property of a reading, not a topic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, institutional, 0.05).
constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
