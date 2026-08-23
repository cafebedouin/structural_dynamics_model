% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Status — Sentience Constrains But Does Not Prohibit Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status — 'animals are sentient beings with
 *   interests that constrain but do not prohibit human use' — is the dominant
 *   legal-philosophical framework governing animal law globally. It
 *   originated as a 19th-century compromise between anti-cruelty reformers
 *   and animal-using industries, and has since been codified in welfare
 *   statutes (e.g., UK Animal Welfare Act 2006, US Animal Welfare Act 1966,
 *   EU directives) that prohibit 'unnecessary suffering' while exempting
 *   'standard agricultural practices,' 'approved research protocols,' and
 *   'customary husbandry.' The constraint operates as a Tangled Rope: its
 *   coordination function (preventing gratuitous cruelty) is genuine and
 *   widely valued, but its exemption structures extract massively from
 *   animals in instrumental use, and active enforcement is required to
 *   maintain the boundary between 'welfare-compliant' and 'gratuitous' harm.
 *   The reading's ε ~0.45 reflects the scale of this extraction: billions of
 *   animals annually experience suffering that the reading's own logic would
 *   prohibit if not for the exemptions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.52).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare Reading of Animal Status — Sentience Constrains But Does Not Prohibit Instrumental Use").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '5624369b-ff72-426f-ac07-f165d789d6c8').
narrative_ontology:cs_kernel_codification('5624369b-ff72-426f-ac07-f165d789d6c8', distributed).
narrative_ontology:cs_authority_grounding('5624369b-ff72-426f-ac07-f165d789d6c8', practice).
narrative_ontology:cs_interpretation_layer_present('5624369b-ff72-426f-ac07-f165d789d6c8').
narrative_ontology:cs_reading_relation('5624369b-ff72-426f-ac07-f165d789d6c8', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5624369b-ff72-426f-ac07-f165d789d6c8', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('5624369b-ff72-426f-ac07-f165d789d6c8', foundational, sentience_grounds_welfare_duties).
narrative_ontology:cs_axiom_status(sentience_grounds_welfare_duties, holdable).
narrative_ontology:cs_axiom_grounding('5624369b-ff72-426f-ac07-f165d789d6c8', sentience_grounds_welfare_duties, deontological).
narrative_ontology:cs_axiom('5624369b-ff72-426f-ac07-f165d789d6c8', secondary, human_interests_override_animal_interests_when_proportionate).
narrative_ontology:cs_axiom_status(human_interests_override_animal_interests_when_proportionate, holdable).
narrative_ontology:cs_axiom_grounding('5624369b-ff72-426f-ac07-f165d789d6c8', human_interests_override_animal_interests_when_proportionate, instrumental).
narrative_ontology:cs_reference_frame('5624369b-ff72-426f-ac07-f165d789d6c8', sentience_based_welfare_framework).
narrative_ontology:cs_drift_state('5624369b-ff72-426f-ac07-f165d789d6c8', contemporary_industrial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5624369b-ff72-426f-ac07-f165d789d6c8', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, companion_animal_breeders).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_subjected_to_gratuitous_harm).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_in_exempted_instrumental_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_grounds_welfare_duties).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, unnecessary_suffering_is_morally_prohibited).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, human_interests_can_override_animal_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shapes welfare standards through lobbying and regulatory capture; benefits from exemption structures that permit intensive confinement, painful procedures without analgesia, and slaughter practices that would be illegal if applied to companion animals. Exit is arbitrage-grade: can relocate production to jurisdictions with weaker standards.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, beneficiary).

% Operates under welfare regulations (IACUC oversight, the 3Rs framework) but receives broad exemptions for procedures deemed scientifically necessary. Benefits from the welfare reading's legitimizing frame: animal use is ethically sanctioned provided welfare boxes are checked. Exit is mobile — could shift to alternative methods but faces high switching costs and institutional inertia.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, mobile, global).

% Animals used in agriculture, research, and breeding whose suffering falls within legal exemption structures (standard agricultural practices, approved research protocols). They bear the physical costs of the constraint's extraction — confinement, mutilation, early death — while the welfare reading classifies this as non-victimizing because it occurs within permitted parameters. No exit exists.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_in_exempted_instrumental_use, payer,
    powerless, biographical, trapped, global).

% Animals suffering harm that exceeds even the welfare reading's permissive boundaries — cruelty, neglect, illegal fighting, unregulated testing. The welfare reading correctly identifies these as victims, but enforcement is sparse and penalties trivial. Their victimhood is recognized in law but not in practice; exit is impossible.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_subjected_to_gratuitous_harm, payer,
    powerless, biographical, trapped, global).

% Advocate for stronger welfare standards within the welfare reading's framework; benefit from the reading's legitimacy (it validates their work) but also critique its exemption structures. Their power is organized but limited to incremental reform. Exit is mobile — they could adopt abolitionist framing but would lose institutional access and funding.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_organizations, observer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_welfare_organizations, beneficiary).

% Hold that sentience grounds rights precluding all instrumental use; structurally excluded from welfare policymaking bodies, regulatory agencies, and industry negotiations. Their exclusion is functional: the welfare reading's legitimacy depends on being the 'reasonable middle' between abolition and property. Exit is constrained — they persist but cannot access the agenda-setting table.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% Defend the property reading: animals are objects, welfare statutes are takings, no independent moral standing. Excluded from mainstream welfare discourse but influential in agricultural and research lobbies. Their exit is arbitrage-grade — they operate through property law, contract, and political donations rather than welfare frameworks.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, property_rights_advocates, excluded,
    powerful, generational, arbitrage, national).

% Benefit from cheap, abundant animal products made possible by exemption structures; also benefit from welfare labeling that provides moral comfort without requiring behavioral change. Exit is constrained — plant-based alternatives exist but face price, availability, and cultural barriers. Their situation is dual: they gain from extraction but also from the coordination function (welfare labels reduce search costs for 'ethical' products).
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, constrained, global).

% Administer welfare standards (USDA, FDA, EFSA, etc.); captured by the industries they regulate but also subject to public pressure. They set the agenda for what counts as 'gratuitous' vs. 'necessary' harm. Their analytical seat is compromised by institutional capture; their agenda-setter seat enforces the exemption architecture.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, regulatory_agencies, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents gratuitous cruelty and establishes minimum care standards for animals under human control, solving a coordination problem where unchecked cruelty would provoke social instability, disease risk, and moral revulsion without centralized standards.
% TRANSFER_FUNCTION: Moves the costs of welfare compliance (space, analgesia, slower growth, enrichment) from animal users to animals themselves via exemption structures — the baseline welfare floor is set low enough that industrial practices remain profitable, transferring the difference between 'welfare-optimal' and 'welfare-legal' from human pockets to animal bodies.
% ABSENT_VOICES: Animals themselves (cannot speak), abolitionist advocates (excluded from regulatory tables), future generations (inherit depleted ecosystems and normalized instrumentalization), and small-scale farmers who cannot afford compliance costs that industrial operators externalize.
% DISAPPEARANCE_RATIONALE: If welfare laws vanished overnight, industrial animal use would intensify immediately (no space minimums, no analgesia requirements, no slaughter standards), public outrage would erupt, and the social license for animal agriculture would collapse — but the property reading would rush to fill the vacuum, likely producing a patchwork of weaker protections. The world rearranges violently.
% FOUNDING_PROBLEM: Industrialization of animal use in the 19th century created unprecedented suffering at scale; public outrage (anti-vivisection, anti-cruelty movements) threatened the social legitimacy of animal use entirely. The welfare reading emerged as a compromise: acknowledge sentience, prohibit 'unnecessary' suffering, but preserve instrumental use.
% FOUNDING_PROBLEM_CORROBORATION: Historians of animal protection (e.g., Susan Pearson, Janet Davis) document the 19th-century compromise; industry archives show welfare laws were often supported by large producers to disadvantage smaller competitors; abolitionist writings from the period (Salt, Carpenter) explicitly identify the welfare compromise as legitimizing continued use. No single party's attestation is uncontradicted.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the exemption gap between what welfare science recommends and what welfare law permits is vast (e.g., gestation crates, battery cages, unanesthetized castration, early weaning). Suppression 0.52: enforcement is active but selective — undercover investigations are criminalized (ag-gag laws), regulatory capture is documented, and penalties are trivial relative to industry revenue. Theater ratio 0.38: welfare labeling, certification schemes, and 'humane' marketing perform compliance while the underlying exemption architecture persists. Accessibility collapse 0.42: alternatives (plant-based, cultivated) exist but face structural barriers (subsidies, labeling laws, cultural inertia). Resistance 0.58: abolitionist movement grows, industry fights every incremental reform, and public opinion is shifting but fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the industry/regulator seat, this is a Rope: coordination works, cruelty is prevented, trade is facilitated. From the animal-in-exempted-use seat, this is a Snare: the coordination story is cover for industrial extraction. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both realities simultaneously. The welfare reading's own self-description is 'balanced'; the metrics reveal the balance is systematically tilted.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal agriculture and research institutions are agenda-setters and beneficiaries (d ~0.15): they write the exemptions, capture the regulators, and collect the profits. Animals in exempted use are full targets (d ~0.95): trapped, powerless, bearing the extraction. Animals in gratuitous harm are also targets (d ~0.9) but with slightly more legal recognition. Welfare organizations are near-symmetric (d ~0.5): they gain legitimacy and funding from the framework but contest its boundaries. Abolitionists are excluded (d not computed — they are not seated). Consumers are constrained beneficiaries (d ~0.3): they gain cheap products and moral comfort but pay in health/environmental costs. Regulatory agencies are dual: as agenda-setters they lean beneficiary (d ~0.25); as analytical observers they should be neutral but are captured.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (industrial cruelty threatening social legitimacy) is contested: industry says it's live (new cruelties emerge), abolitionists say it's dead (the compromise legitimized more use than it constrained), historians say it's transformed (the exemption architecture now *is* the extraction mechanism). The welfare reading prevents mislabeling: without it, all animal use would be property-reading extraction; with it, gratuitous harm is actually prohibited (coordination function real). But the exemption structures mean the coordination function has been colonized by extraction — a classic Tangled Rope, not a pure Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_committer_structure,
    'This constraint is one reading (welfare_reading) of the contested animal_status kernel. What structural elements do the sibling readings (abolitionist_reading, property_reading) change, and where is the disagreement located?',
    'Comparative constraint story analysis: generate all three readings as separate ε-invariant constraints and measure metric divergence (ε, beneficiaries, victims, claimed_type). The disagreement is located in: (1) victim set definition (which animals count as victims), (2) beneficiary set (who legitimately benefits), (3) coordination function (what problem the constraint solves).',
    'If the welfare reading''s victim set excludes animals in ''regulated'' instrumental use, but the exemption structures make that regulation nominal, the welfare reading''s ε is artificially low relative to its actual extraction. The abolitionist reading would assign those animals to the victim set, raising ε. The property reading would assign no animals to the victim set, lowering ε further. The kernel''s ε-invariance requires three separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_committer_structure, conceptual, 'Committer frame: this constraint is one reading of the animal_status kernel; structural deltas to sibling readings are the irreducible uncertainty.').

omega_variable(
    exemption_structure_extraction_boundary,
    'Where exactly does the welfare reading''s coordination function end and its extraction function begin? At what point does an exemption become too broad to be ''necessary''?',
    'Welfare science meta-analysis: compare legally permitted practices (stocking densities, mutilation without analgesia, weaning ages) against peer-reviewed welfare optima. The gap quantifies the extraction component. Regulatory capture studies: trace lobbying expenditures to specific exemption language.',
    'If the exemption boundary is arbitrary (politically negotiated, not welfare-science grounded), the coordination function is a fig leaf and the constraint trends toward Snare. If the boundary tracks welfare science with a consistent margin, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_structure_extraction_boundary, empirical, 'Whether the welfare/extraction boundary in exemption structures is principled or arbitrary.').

omega_variable(
    animal_victimhood_ontology,
    'Does the welfare reading''s exclusion of ''properly regulated'' instrumental use from the victim set reflect a genuine moral distinction, or is it a structural artifact of the reading''s own exemption architecture?',
    'Cross-reading comparison: the abolitionist reading includes all instrumentally used animals in the victim set; the property reading includes none. The welfare reading''s partial inclusion (gratuitous harm only) must be justified by a criterion that survives the exemption test. If ''gratuitous'' is defined circularly as ''what the exemptions don''t cover,'' the distinction collapses.',
    'If the victim-set distinction is circular, the welfare reading''s claimed_type (tangled_rope) masks a Snare: the coordination function exists only to legitimize the extraction. If the distinction is principled (e.g., suffering beyond what is physically necessary for the use), the Tangled Rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_victimhood_ontology, conceptual, 'Whether the welfare reading''s victim-set boundary is morally principled or structurally circular.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1822, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_welfare_tr_t1822, animal_status__welfare_reading, theater_ratio, 1822, 0.1).
narrative_ontology:measurement(animal_status_welfare_tr_t1876, animal_status__welfare_reading, theater_ratio, 1876, 0.18).
narrative_ontology:measurement(animal_status_welfare_tr_t1930, animal_status__welfare_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(animal_status_welfare_tr_t1966, animal_status__welfare_reading, theater_ratio, 1966, 0.32).
narrative_ontology:measurement(animal_status_welfare_tr_t1985, animal_status__welfare_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(animal_status_welfare_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(animal_status_welfare_tr_t2010, animal_status__welfare_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(animal_status_welfare_tr_t2025, animal_status__welfare_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(animal_status_welfare_be_t1822, animal_status__welfare_reading, base_extractiveness, 1822, 0.15).
narrative_ontology:measurement(animal_status_welfare_be_t1876, animal_status__welfare_reading, base_extractiveness, 1876, 0.22).
narrative_ontology:measurement(animal_status_welfare_be_t1930, animal_status__welfare_reading, base_extractiveness, 1930, 0.31).
narrative_ontology:measurement(animal_status_welfare_be_t1966, animal_status__welfare_reading, base_extractiveness, 1966, 0.38).
narrative_ontology:measurement(animal_status_welfare_be_t1985, animal_status__welfare_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement(animal_status_welfare_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(animal_status_welfare_be_t2010, animal_status__welfare_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(animal_status_welfare_be_t2025, animal_status__welfare_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_welfare_su_t1822, animal_status__welfare_reading, suppression_requirement, 1822, 0.25).
narrative_ontology:measurement(animal_status_welfare_su_t1876, animal_status__welfare_reading, suppression_requirement, 1876, 0.35).
narrative_ontology:measurement(animal_status_welfare_su_t1930, animal_status__welfare_reading, suppression_requirement, 1930, 0.42).
narrative_ontology:measurement(animal_status_welfare_su_t1966, animal_status__welfare_reading, suppression_requirement, 1966, 0.48).
narrative_ontology:measurement(animal_status_welfare_su_t1985, animal_status__welfare_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(animal_status_welfare_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement(animal_status_welfare_su_t2010, animal_status__welfare_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(animal_status_welfare_su_t2025, animal_status__welfare_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint (welfare_reading) is one of three readings of the animal_status kernel. The abolitionist_reading (animals as rights-holders) and property_reading (animals as objects) are sibling constraints with different ε, beneficiaries, victims, and claimed_type. All three form a constraint family. The welfare reading's exemption structures are the primary extraction mechanism; the abolitionist reading forecloses them; the property reading maximizes them. The network edges represent the kernel's internal contestation: each reading's legitimacy depends on the others' existence as alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, institutional, 0.15).
constraint_indexing:directionality_override(animal_status__welfare_reading, powerless, 0.95).
constraint_indexing:directionality_override(animal_status__welfare_reading, moderate, 0.3).
constraint_indexing:directionality_override(animal_status__welfare_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
