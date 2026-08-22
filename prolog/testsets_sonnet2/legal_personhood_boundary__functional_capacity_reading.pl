% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary — Functional Cognitive Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story authors the functional-capacity reading of the legal
 *   personhood boundary kernel: personhood tracks demonstrable cognitive
 *   capacity (rationality, sentience, self-awareness), not species
 *   membership. Under this reading, the standing arrangement under contest —
 *   the current property-based legal treatment of high-cognition non-human
 *   animals — is assessed as substantially extractive: industries built on
 *   treating capacity-bearing beings as property (industrial agriculture,
 *   biomedical testing, captive exhibition) extract economic value from
 *   beings this reading holds should carry rights-bearing status. The
 *   reading's endorsed alternative (full personhood recognition) is NOT the ε
 *   referent; ε describes the current arrangement as this reading's advocates
 *   see it. Litigation to date (chimpanzee and elephant habeas corpus
 *   petitions) has produced mixed and largely unsuccessful results, meaning
 *   the arrangement this reading criticizes remains dominant and actively
 *   defended — hence rising suppression_requirement as courts and
 *   legislatures develop more explicit doctrinal barriers to capacity-based
 *   claims over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.79).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.81).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Cognitive Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '2d7bad4a-bc12-42b7-ade3-e89457a34f24').
narrative_ontology:cs_kernel_codification('2d7bad4a-bc12-42b7-ade3-e89457a34f24', distributed).
narrative_ontology:cs_authority_grounding('2d7bad4a-bc12-42b7-ade3-e89457a34f24', distributed).
narrative_ontology:cs_reading_relation('2d7bad4a-bc12-42b7-ade3-e89457a34f24', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('2d7bad4a-bc12-42b7-ade3-e89457a34f24', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('2d7bad4a-bc12-42b7-ade3-e89457a34f24', foundational, capacity_not_species_grounds_moral_status).
narrative_ontology:cs_axiom_status(capacity_not_species_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('2d7bad4a-bc12-42b7-ade3-e89457a34f24', capacity_not_species_grounds_moral_status, deontological).
narrative_ontology:cs_axiom('2d7bad4a-bc12-42b7-ade3-e89457a34f24', secondary, demonstrable_cognition_is_the_operative_test).
narrative_ontology:cs_axiom_status(demonstrable_cognition_is_the_operative_test, holdable).
narrative_ontology:cs_axiom_grounding('2d7bad4a-bc12-42b7-ade3-e89457a34f24', demonstrable_cognition_is_the_operative_test, empirically_contingent).
narrative_ontology:cs_reference_frame('2d7bad4a-bc12-42b7-ade3-e89457a34f24', species_based_property_default).
narrative_ontology:cs_drift_state('2d7bad4a-bc12-42b7-ade3-e89457a34f24', contemporary_habeas_litigation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d7bad4a-bc12-42b7-ade3-e89457a34f24', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, great_apes_and_cetaceans_advocacy_beneficiaries).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_law_litigators).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, biomedical_alternative_research_sector).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, factory_farming_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, biomedical_animal_testing_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, entertainment_and_exhibition_animal_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, captive_high_cognition_animals).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, capacity_based_moral_status_theory).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_rights_predicate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great apes, elephants, cetaceans, and other animals with demonstrated higher cognition currently held as property in labs, zoos, and entertainment venues. Under the current anthropocentric standard they cannot hold rights, litigate their own confinement, or refuse use; under this reading they would gain habeas corpus standing, but until the reading is adopted by courts they remain property with no legal voice of their own.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, captive_high_cognition_animals, payer,
    powerless, biographical, trapped, national).

% Organizations and lawyers who bring habeas corpus and personhood petitions on behalf of captive animals, citing cognitive science to argue capacity, not species, should determine legal status. They administer the litigation strategy that would extend the boundary and stand to gain professional and organizational legitimacy if courts adopt the reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_law_litigators, agenda_setter,
    organized, generational, mobile, national).

% Cognitive ethologists, sanctuary operators, and advocacy coalitions whose research findings and institutional missions are vindicated if courts recognize capacity-based personhood. They benefit reputationally and materially (funding, sanctuary placements) without bearing the costs of reorganizing the industries the reading would disrupt.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, great_apes_and_cetaceans_advocacy_beneficiaries, beneficiary,
    moderate, generational, constrained, global).

% Firms developing organ-on-chip, computational modeling, and cell-culture alternatives to animal testing gain competitively if legal personhood status raises the cost or liability of using cognitively complex animal subjects. They did not create the coordination problem but stand to profit from its resolution in this direction.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_alternative_research_sector, beneficiary,
    organized, biographical, mobile, global).

% Industrial animal agriculture operators whose entire economic model depends on treating animals as property rather than rights-holders. A capacity-based standard, if extended past great apes and cetaceans toward pigs or other cognitively complex livestock, would require restructuring or abolishing core production methods; they have significant lobbying power to resist but face genuine legal exposure as the boundary tests keep advancing.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, factory_farming_industry, payer,
    institutional, biographical, constrained, national).

% Pharmaceutical and research institutions relying on animal testing protocols, including primate research. A capacity threshold that recognizes chimpanzee or macaque personhood would foreclose or dramatically raise the cost of standard testing regimes; substantial regulatory and reputational capital is invested in defending the current property status.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_animal_testing_industry, payer,
    institutional, biographical, constrained, national).

% Zoos, marine parks, and circuses holding cetaceans and great apes for exhibition. Personhood recognition would directly convert their core assets into rights-holders with standing to seek release, threatening the business model outright; they have resources to litigate but limited ability to relocate the underlying activity.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, entertainment_and_exhibition_animal_industry, payer,
    powerful, biographical, constrained, national).

% Judicial and legislative bodies adjudicating personhood petitions and considering statutory codification of capacity-based standards. They weigh scientific testimony on cognition against precedent grounded in species-based property law, and their rulings determine whether this reading gains binding legal force or remains persuasive advocacy.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% Advanced AI systems that might eventually demonstrate rationality, self-awareness, or sentience by the same functional tests this reading proposes. They are not yet part of any personhood debate in practice, have no advocates comparable to animal law organizations, and would be the next boundary test if the capacity standard is adopted and taken to its logical extension.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_synthetic_cognitive_agents, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, testable criterion — demonstrated cognitive capacity — for who counts as a rights-holder, replacing an ad hoc species line with a standard that can in principle be applied consistently across biological and (eventually) non-biological entities.
% TRANSFER_FUNCTION: Moves legal standing and the associated exemption from being used as property from institutions currently profiting off high-cognition animals (labs, exhibitors, industrial agriculture) toward the animals themselves and the advocacy/research infrastructure built around recognizing their capacity.
% ABSENT_VOICES: The animals themselves cannot testify to their own cognitive states except through proxy scientific measurement designed and interpreted by advocates; future synthetic cognitive agents have no standing or advocacy structure at all yet; smallholder farmers and indigenous communities whose subsistence practices involve animals with contested cognitive status are largely absent from the elite litigation and legislative venues where this reading is being tested.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live legal position overnight, captive high-cognition animals currently subject to ongoing habeas petitions would lose their strongest available legal theory and revert fully to property status; industries currently investing in defensive litigation and alternative research would recalibrate immediately. Whether this counts as 'world rearranges' or 'world unchanged' is itself contested between the reading's proponents (who see a rights revolution reversed) and its opponents (who see a return to settled law).
% FOUNDING_PROBLEM: Legal personhood historically tracked species membership (human = person, non-human = property) even as comparative cognitive science accumulated evidence that some non-human animals possess rationality, self-awareness, and suffering capacity comparable to or exceeding some legally recognized humans (e.g., infants, severely cognitively impaired adults) — producing an internal inconsistency the functional-capacity reading was built to resolve.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive ethologists and comparative psychologists outside the animal-law advocacy movement (e.g., published primate and cetacean cognition researchers with no litigation stake) corroborate that the empirical capacity gap between some non-human animals and some legally personhood-holding humans is real and measurable. Industry-funded bioethicists dispute the legal relevance of that empirical finding but do not generally dispute the underlying cognitive science itself.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, contested).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.55 to 0.79) reflecting increasing scientific documentation of cognitive capacity in captive animals alongside continued economic reliance on their property status — the gap between what capacity science shows and what law recognizes widens as testing/exhibition industries scale. Suppression is high and rising (0.62 to 0.81) because maintaining the species-based line increasingly requires active doctrinal work (courts distinguishing prior personhood-adjacent rulings, legislatures passing ag-gag and biomedical carve-outs) rather than passive default. Theater ratio is moderate and rising (0.25 to 0.40): some welfare-oriented reforms function as procedural theater — improved captivity standards, welfare certifications — that do not touch the underlying property/rights question this reading contests.
 *
 * DIRECTIONALITY LOGIC:
 *   Captive high-cognition animals are the clearest victims: trapped exit, no legal voice, direct subjects of the extraction (use as test subjects, exhibits, or production inputs). The three payer-industry stakeholders bear the cost of doctrinal change but hold institutional power and constrained (not trapped) exit — they can relocate operations, lobby, or adapt product lines, which tempers but does not eliminate their exposure. Animal law litigators and cognitive-capacity beneficiaries sit at low directionality toward this specific constraint — the arrangement as it stands does not extract from them; recognition would validate their work and mission. Future synthetic cognitive agents are excluded rather than beneficiaries or victims under the current reading — they are not yet a party to the debate, but the reading's logic extends toward them, which is exactly the boundary-creep the sibling readings warn against.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an empirical mismatch between demonstrated cognitive capacity and legal status assigned purely by species — remains live by outside corroboration (comparative cognitive science), which blocks a mandatrophy reading (the arrangement being defended is not a solved problem being milked by inertia; it is an actively contested boundary). The tangled_rope classification captures that the current property-based arrangement genuinely coordinates something (predictable commercial and research use of animals, settled expectations in industries built over generations) while also extracting asymmetrically from beings unable to advocate for themselves — both the coordination function and the extraction are real and simultaneous, which is precisely the tangled_rope signature rather than pure snare or pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_validity,
    'Are current comparative cognitive tests (mirror self-recognition, tool use, theory-of-mind proxies) valid and sufficiently precise measures of the morally relevant capacities (rationality, sentience, self-awareness), or do they smuggle in anthropocentric bias by testing for human-like cognition rather than capacity itself?',
    'Convergent validation across independent comparative cognition research programs using capacity measures not modeled on human benchmarks; philosophical resolution of what capacity is doing the normative work (sentience vs. rationality vs. self-awareness may not co-vary).',
    'If current tests are invalid or biased, the entire beneficiary/victim structure of this reading is miscalibrated — some currently-excluded species might qualify and some currently-included ones might not, changing which industries are actually exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_validity, empirical, 'Whether cognitive capacity tests validly track the morally relevant property this reading is built on.').

omega_variable(
    boundary_creep_to_synthetic_agents,
    'Does adopting a pure capacity-based standard for personhood logically commit courts to extending it to future AI systems that pass the same functional tests, and is that extension a feature or a reductio of this reading?',
    'Doctrinal analysis of whether the capacity criterion as stated is substrate-neutral by design or implicitly restricted to biological sentience; legislative clarification if adopted.',
    'If substrate-neutral, this reading eventually creates standing claims for synthetic cognitive agents, which either validates the reading''s internal consistency or is used by the restrictive_anthropocentric_reading as evidence the standard is unworkable and should be rejected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_creep_to_synthetic_agents, conceptual, 'Whether the capacity criterion logically extends beyond biological animals to future AI, and what that implies for the reading''s coherence.').

omega_variable(
    kernel_framing_institution_vs_legitimacy_claim,
    'Is the object under contest the institutional rule (which entities get standing in court) or the deeper legitimacy claim (what grounds moral status as such) — and does choosing one framing over the other change whether this reading counts as tangled_rope versus snare?',
    'Compare classification outcomes under an institution-framing (courts as the kernel-adjudicating authority) versus a legitimacy-framing (moral philosophy as the authority, courts merely implementing it) — if the two framings produce different cs_pattern results, document both.',
    'Under the institution framing, courts function as an active interpretive layer absorbing drift (supports tangled_rope with interpretation_layer_present); under the legitimacy framing, the underlying moral claim is contested at a level courts cannot resolve, which could push the classification toward a less settled snare-adjacent reading of the status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_institution_vs_legitimacy_claim, conceptual, 'Alternative framings of what the kernel actually is (institutional standing rule vs. underlying moral-status legitimacy claim) and how that choice affects classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.1).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the legal_personhood_boundary kernel. restrictive_anthropocentric_reading denies standing to the beneficiary class this reading creates (non-human animals, potentially future AI) and is this reading's most direct doctrinal antagonist. developmental_potentiality_reading operates on an orthogonal axis (human prenatal status) and shares no beneficiary/victim overlap with this reading but competes for the same personhood vocabulary and judicial bandwidth. Each reading carries its own independent ε, beneficiary/victim structure, and classification per the ε-invariance principle; this file does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
