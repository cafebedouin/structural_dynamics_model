% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Property Status System (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story represents the abolitionist reading of the
 *   animal_moral_status kernel. The standing arrangement under contest is the
 *   global legal system that classifies nonhuman animals as property —
 *   objects that can be owned, bought, sold, killed, and used at the owner's
 *   discretion. The abolitionist reading assesses this arrangement as a
 *   snare: pure extraction masked by a coordination story (efficient resource
 *   allocation). The constraint extracts the entirety of animals' lives for
 *   human benefit, suppresses alternatives through law and force, and meets
 *   resistance from both animals (invisible, individualized) and human
 *   advocates. The claimed type is snare; the metrics reflect the
 *   abolitionist's assessment of the system's actual operation. The
 *   property_reading and welfare_reading are sibling constraints from the
 *   same kernel, linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status System (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '19b16be9-8441-4053-9c8a-6ff2b02d2456').
narrative_ontology:cs_kernel_codification('19b16be9-8441-4053-9c8a-6ff2b02d2456', distributed).
narrative_ontology:cs_authority_grounding('19b16be9-8441-4053-9c8a-6ff2b02d2456', extraction).
narrative_ontology:cs_interpretation_layer_present('19b16be9-8441-4053-9c8a-6ff2b02d2456').
narrative_ontology:cs_reading_relation('19b16be9-8441-4053-9c8a-6ff2b02d2456', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('19b16be9-8441-4053-9c8a-6ff2b02d2456', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('19b16be9-8441-4053-9c8a-6ff2b02d2456', foundational, animals_are_rights_bearers).
narrative_ontology:cs_axiom_status(animals_are_rights_bearers, holdable).
narrative_ontology:cs_axiom_grounding('19b16be9-8441-4053-9c8a-6ff2b02d2456', animals_are_rights_bearers, deontological).
narrative_ontology:cs_axiom('19b16be9-8441-4053-9c8a-6ff2b02d2456', foundational, property_status_is_rights_violation).
narrative_ontology:cs_axiom_status(property_status_is_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('19b16be9-8441-4053-9c8a-6ff2b02d2456', property_status_is_rights_violation, deontological).
narrative_ontology:cs_axiom('19b16be9-8441-4053-9c8a-6ff2b02d2456', secondary, all_use_perpetuates_victimization).
narrative_ontology:cs_axiom_status(all_use_perpetuates_victimization, holdable).
narrative_ontology:cs_axiom_grounding('19b16be9-8441-4053-9c8a-6ff2b02d2456', all_use_perpetuates_victimization, deontological).
narrative_ontology:cs_reference_frame('19b16be9-8441-4053-9c8a-6ff2b02d2456', human_supremacy_property_regime).
narrative_ontology:cs_drift_state('19b16be9-8441-4053-9c8a-6ff2b02d2456', contemporary_animal_rights_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19b16be9-8441-4053-9c8a-6ff2b02d2456', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, entertainment_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, pet_breeders_trade).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, human_consumers).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_reform_organizations).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, human_consumers).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, human_supremacy_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, property_rights_absolutism).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, instrumental_value_of_nonhuman_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All animals legally classified as property: farmed animals, laboratory animals, animals in entertainment, companion animals bred for trade, wild animals captured or managed as resources. They bear the full cost of the constraint — their lives, bodies, reproductive capacity, and liberty are allocated by owners. Exit is structurally impossible; they cannot leave the property system, petition courts, or organize resistance. Their interests are invisible to the legal framework that defines them as objects.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, immediate, trapped, global).

% Industrial producers of meat, dairy, eggs, and fiber. They receive the primary economic extraction from the property system — legal title to animal bodies and labor, exemption from cruelty statutes that would otherwise constrain production methods, and public subsidy (feed crops, water, waste externalization). They can shift production across jurisdictions, substitute species, or vertically integrate; their exit from any single regulation is high, but they defend the property system as a whole because it secures their asset base.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Universities, pharmaceutical companies, and contract research organizations that use animals as experimental models. They benefit from unrestricted access to animal subjects, legal protection from liability for harms inflicted in approved protocols, and public funding that treats animal use as a necessary scientific infrastructure. They can relocate facilities or outsource to less regulated jurisdictions, but the property status of animals is foundational to their operational model.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, mobile, global).

% Zoos, aquariums, circuses, film/TV production, and exotic pet trade. They profit from displaying, performing, or breeding animals as attractions. The property system grants them exclusive control over animal movement, reproduction, and public access. Exit options are narrower than agriculture or research — public scrutiny and welfare regulations constrain them more visibly — but they remain net beneficiaries of the property framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, entertainment_industry, beneficiary,
    organized, biographical, constrained, national).

% Commercial breeders, puppy mills, exotic animal dealers, and the retail chain that sells companion animals as merchandise. They benefit from the legal classification of animals as chattel — no duty of care beyond minimal welfare statutes, unrestricted breeding and culling, and consumer protection laws that treat animals as goods. They face growing regulatory pressure (breeding bans, retail sale prohibitions) but the property framework remains their core license.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, pet_breeders_trade, beneficiary,
    organized, biographical, constrained, national).

% The general public that purchases animal products, visits zoos, keeps pets, and benefits from medical advances developed through animal research. They receive cheap food, entertainment, companionship, and health benefits subsidized by the property system's externalization of animal suffering. They also pay indirectly through health costs of animal product consumption, zoonotic disease risk, and environmental degradation. Exit is individually easy (veganism, boycotts) but collectively constrained by food systems, cultural norms, and infrastructure.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, human_consumers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, human_consumers, payer).

% Legislatures, courts, and enforcement agencies that define, maintain, and adjudicate the property status of animals. They write the statutes that exempt animal agriculture from cruelty laws, uphold ownership rights against welfare claims, and set the boundaries of permissible use. They could change the classification — some jurisdictions have begun recognizing animals as sentient beings or legal persons for limited purposes — but institutional inertia, industry lobbying, and the systemic consequences of reclassification make reform incremental and contested.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Abolitionist organizations, legal theorists, and activists who argue that property status itself is the rights violation. They document the extraction, litigate for personhood or rights recognition, and build public pressure for systemic change. They do not collect rents from the constraint; their position is defined by opposition to it. Their analytical seat sees the full structure: the coordination function (human resource allocation) and the extraction function (animal victimization) as inseparable in the current system.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_rights_advocates, observer,
    moderate, generational, analytical, global).

% Organizations that seek to reduce suffering within the property system (larger cages, stunning requirements, enrichment standards). They benefit from the system's legitimacy — their funding, access, and regulatory role depend on the property framework persisting in a reformed shape. They are structurally positioned between abolition and industry: they negotiate with the agenda-setters, accept the property premise, and extract concessions that make the system more palatable without challenging its core.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_reform_organizations, observer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_reform_organizations, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The property system coordinates human allocation of animal bodies and labor across food, research, entertainment, and companionship markets. It provides a stable legal framework for ownership, transfer, and exclusive control, reducing transaction costs for industries that use animals as inputs.
% TRANSFER_FUNCTION: Moves animal lives, reproductive capacity, bodily integrity, and liberty from animals to human owners and industries. The transfer is total: the animal's entire existence is appropriated as resource. The return flow is suffering, premature death, and ecological degradation — borne by animals and, secondarily, by human communities.
% ABSENT_VOICES: The animals themselves are the primary excluded voice. They cannot speak in court, lobby legislatures, or organize boycotts. Their interests are represented only by human advocates who lack standing to assert animals' own rights. Future generations of animals (who will be born into the system) and wild animals displaced by animal agriculture's land use are also absent.
% DISAPPEARANCE_RATIONALE: If property status vanished overnight, the legal architecture of animal use would collapse. Industries would lose legal title to their 'inventory'; courts would have to adjudicate competing claims (custody, personhood, guardianship); food, research, and entertainment systems would face immediate restructuring. The human-animal relationship would shift from ownership to something else — guardianship, coexistence, or separation — rearranging economies, laws, and daily life globally.
% FOUNDING_PROBLEM: Early human societies needed a stable system to manage domesticated animals as reliable food, labor, and material sources. Property law provided clear title, inheritance rules, and dispute resolution for these valuable assets, enabling agricultural civilization to scale.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and archaeological records confirm domestication and early property systems co-evolved (e.g., cattle as wealth in pastoral societies, Roman law of mancipium). The abolitionist reading argues the founding problem (reliable resource access) is now solved by plant-based and cellular agriculture, making the property system obsolete; industry and legal institutions argue the problem remains live because animal products are still 'essential' for nutrition, research, and cultural practices. No neutral arbiter has settled this; the corroboration comes from outside the beneficiary set: UN FAO reports on livestock's environmental footprint, historical analyses of property law's evolution, and the existence of viable alternatives demonstrated by vegan populations and alternative protein markets.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-maximal (0.92) because the constraint appropriates 100% of the victim's autonomy, lifespan, and bodily integrity — the most total extraction possible. Suppression is high (0.88) because the system depends on legal enforcement (property law, agricultural exemptions, ag-gag laws, trespass statutes) and physical confinement to prevent exit; alternatives (sanctuaries, plant-based systems) are legally and economically marginalized. Theater ratio is low (0.15) because the system's functional core (converting animals into products) is genuine, not performative; the welfare regulations that exist are minimal and poorly enforced, not a theatrical facade. Accessibility collapse (0.75) reflects that once an animal enters the property system, alternatives (freedom, sanctuary) are nearly inaccessible — but not absolutely, as rescues and liberations occur. Resistance (0.65) captures both animal resistance (escapes, aggression, refusal) and human advocacy (litigation, direct action, market disruption), which is significant but structurally outmatched.
 *
 * PERSPECTIVAL GAP:
 *   From the animal's seat (if they could occupy one), the constraint is a snare of absolute extraction with no coordination benefit. From the industry seat, it appears as a rope (coordination of complex supply chains) with minimal extraction (they frame welfare costs as the price of coordination). From the legal institution seat, it is a scaffold (transitioning toward welfare recognition) or piton (inertial maintenance of settled law). The engine computes these divergences from the structural data; the abolitionist reading declares the snare classification as structurally true.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are full targets (d ≈ 1.0): they bear the total extraction, have zero exit, and are identity-locked into the property status by legal definition. Human industry beneficiaries (agriculture, research, entertainment, breeding) are near-full beneficiaries (d ≈ 0.0-0.15): they collect the extraction, control the rules, and have arbitrage-grade exit from any single jurisdiction. Human consumers are partial beneficiaries/payers (d ≈ 0.4-0.5): they receive subsidized products but pay health and ecological costs, with mobile exit individually. Legal institutions are agenda-setters with analytical exit (they could change the law but face institutional inertia). Animal advocates are observers (analytical exit). Welfare reform orgs are dual-positioned: they benefit from the system's legitimacy (secondary_role: beneficiary) while observing its structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The property system's founding problem (reliable animal resource access for pre-industrial societies) is contested as live or dead. The system persists not because it solves a current coordination problem better than alternatives, but because the beneficiaries (industries) have concentrated power to maintain it, while the victims (animals) have zero political voice. The mandatrophy is unresolved: the arrangement has outlived its original function but is locked in by power asymmetry. The welfare_reading functions as a mandatrophy-management layer — it absorbs pressure for change by offering reforms that preserve the property core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the animal_moral_status kernel a single contested commitment with multiple readings, or are the abolitionist, property, and welfare positions fundamentally different constraints with different referents?',
    'Test ε-invariance: if measuring the property system''s extraction via abolitionist lights (total appropriation of animal lives) yields ε ≈ 0.92, but via property_reading lights (efficient resource allocation) yields ε ≈ 0.1, and via welfare_reading lights (regulated use with suffering reduction) yields ε ≈ 0.4, then the kernel covers three structurally distinct constraints. Decompose into separate stories linked by network.affects_constraints.',
    'If the kernel decomposes, each reading gets its own ε, stakeholders, and classification. The abolitionist story''s snare classification stands; the property_reading would likely classify as rope; the welfare_reading as tangled_rope. The network edges would show property_reading → welfare_reading → abolitionist_reading as a degradation sequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label ''animal_moral_status'' conflates multiple constraints with different ε values.').

omega_variable(
    property_status_contingency,
    'Is the property status of animals a contingent legal construction that could be abolished by legislation, or a structural feature of human-animal relations that would persist in any complex society?',
    'Historical and cross-cultural analysis: do any complex societies lack animal property concepts? Legal theory: could guardianship/personhood frameworks replace property without collapsing food/medical systems? Empirical: do jurisdictions with stronger animal protections (e.g., Germany''s Basic Law, New Zealand''s sentience recognition) show systemic transformation or marginal adjustment?',
    'If contingent, the constraint is a snare (pure extraction maintained by power). If structural, it may be a tangled_rope (coordination function that cannot be removed without system collapse) or even a mountain (if some form of human dominion is inevitable). This determines whether abolition is a realistic endpoint or a theoretical limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, empirical, 'Whether animal property status is contingent or structurally necessary.').

omega_variable(
    animal_rights_bearer_capacity,
    'Can animals function as rights-bearers without human proxies, or does rights-ascription necessarily require human legal representation that reproduces the power asymmetry?',
    'Legal philosophy: analyze guardianship models (e.g., for children, incapacitated adults) for structural parallels. Political theory: examine whether any oppressed group has gained rights without human intermediaries. Empirical: study the Nonhuman Rights Project''s habeas corpus cases — do courts treat animal plaintiffs as rights-bearers or as objects of human advocacy?',
    'If animals cannot be rights-bearers without proxies, the abolitionist constraint (which posits animals as direct rights-holders) faces a structural implementation gap — the constraint''s remedy requires the very human mediation it critiques. This would not change the snare classification of the property system but would complicate the abolitionist alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_rights_bearer_capacity, conceptual, 'Whether animals can directly hold rights or require human proxies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of animal resistance and human alternatives primarily structural (legal barriers, physical confinement, economic incentives) or internalized (human supremacy ideology, speciesism, carnism as belief system)?',
    'Post-exit trajectory analysis: when humans exit animal use (go vegan), does suppression persist as internalized speciesism (discomfort, social friction, nutritional myths)? When animals escape (sanctuary cases), do they exhibit learned helplessness or rapid behavioral recovery? Comparative: analyze suppression in other total institutions (slavery, prisons) for structural vs. internalized components.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than legal/physical measures suggest — the ideology travels with agents after formal exit. This would increase the abolitionist reading''s suppression metric and strengthen the snare classification. If primarily structural, legal abolition would more completely dismantle the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanisms in the animal property system.').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment-system framing of this constraint (kernel = animal_moral_status, authority = legal property system) capture the abolitionist reading''s structure, or does the abolitionist reading reject the CS framework entirely by denying the kernel''s legitimacy?',
    'Analyze whether the abolitionist reading treats the property system as a commitment system with a kernel (it does: the kernel is ''animals are property'') or as an illegitimate imposition with no kernel (the kernel is a fiction). If the latter, the CS structure fields may misrepresent the reading''s own self-understanding.',
    'If the abolitionist reading rejects the CS framing, the cs_structure block should be omitted or marked as ''imposed_framing''. This would affect how the engine processes reading_relations and axioms — they would be analytical impositions, not the reading''s own commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the abolitionist reading accepts or rejects the commitment-system framing of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t100, animal_moral_status__abolitionist_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t150, animal_moral_status__abolitionist_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t200, animal_moral_status__abolitionist_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_tr_t250, animal_moral_status__abolitionist_reading, theater_ratio, 250, 0.15).

% Extraction over time
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t100, animal_moral_status__abolitionist_reading, base_extractiveness, 100, 0.9).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t150, animal_moral_status__abolitionist_reading, base_extractiveness, 150, 0.91).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t200, animal_moral_status__abolitionist_reading, base_extractiveness, 200, 0.915).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_be_t250, animal_moral_status__abolitionist_reading, base_extractiveness, 250, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t100, animal_moral_status__abolitionist_reading, suppression_requirement, 100, 0.83).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t150, animal_moral_status__abolitionist_reading, suppression_requirement, 150, 0.85).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t200, animal_moral_status__abolitionist_reading, suppression_requirement, 200, 0.87).
narrative_ontology:measurement(animal_moral_status__abolitionist_reading_su_t250, animal_moral_status__abolitionist_reading, suppression_requirement, 250, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.15).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint (abolitionist_reading) and its siblings (welfare_reading, property_reading) form a constraint family decomposing the animal_moral_status kernel. The property_reading (upstream, institutionalized) influences the welfare_reading (reformist), which influences the abolitionist_reading (radical) — each reading's ε increases as the coordination story thins and extraction becomes more visible. The property_reading claims rope (coordination); welfare_reading claims tangled_rope (coordination + extraction); abolitionist_reading claims snare (pure extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, powerless, 1.0).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, institutional, 0.1).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
