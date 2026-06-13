% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Moral Status—Abolitionist Reading: Property Status as Structural Violation
 *   domain: ethical/legal/institutional
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status asserts that animals are
 *   rights-bearing individuals; that legal property classification is itself
 *   the violation (not merely how property is treated); and that all
 *   use—however 'humane'—perpetuates victimization. This reading makes
 *   animals the sole victim class and identifies property-rights holders
 *   (agriculture, pharmaceutical, research, food, entertainment industries)
 *   as the structural beneficiaries of a constraint that preserves
 *   non-personhood. The constraint operates as a snare: property
 *   classification is enforced by law, cultural narrative, and market
 *   dominance; alternatives are suppressed; and the victimized class
 *   (animals) has no legal standing to resist. Welfare incrementalism is
 *   treated as a competing constraint reading (the welfare_reading sibling),
 *   not as progress within the abolitionist framework.
 *
 * KEY AGENTS:
 *   - all_animals_under_human_dominion: the victim class, structurally powerless, trapped, unable to exit property status
 *   - property_rights_holders (agricultural, pharmaceutical, research, food industries): the agenda-setting beneficiary, institutional power, arbitrage exit (can shift products, not fundamentally reorganize)
 *   - legal and cultural authorities (legislatures, courts, professional bodies): codify and enforce property classification through statute, precedent, and standard-setting
 *   - welfare_advocates: excluded from this reading's framework because they accept use-with-regulation, a foundational premise the abolitionist reading rejects
 *   - abolitionist_movement: excluded from decision-making power but contests the kernel itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.89).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.76).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Moral Status—Abolitionist Reading: Property Status as Structural Violation").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "ethical/legal/institutional").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'a330f506-5094-4630-b835-f7857e2e8321').
narrative_ontology:cs_kernel_codification('a330f506-5094-4630-b835-f7857e2e8321', formalized).
narrative_ontology:cs_authority_grounding('a330f506-5094-4630-b835-f7857e2e8321', extraction).
narrative_ontology:cs_interpretation_layer_present('a330f506-5094-4630-b835-f7857e2e8321').
narrative_ontology:cs_reading_relation('a330f506-5094-4630-b835-f7857e2e8321', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('a330f506-5094-4630-b835-f7857e2e8321', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('a330f506-5094-4630-b835-f7857e2e8321', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('a330f506-5094-4630-b835-f7857e2e8321', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('a330f506-5094-4630-b835-f7857e2e8321', foundational, property_status_is_inherent_violation).
narrative_ontology:cs_axiom_status(property_status_is_inherent_violation, holdable).
narrative_ontology:cs_axiom_grounding('a330f506-5094-4630-b835-f7857e2e8321', property_status_is_inherent_violation, deontological).
narrative_ontology:cs_reference_frame('a330f506-5094-4630-b835-f7857e2e8321', moral_personhood_framework).
narrative_ontology:cs_drift_state('a330f506-5094-4630-b835-f7857e2e8321', contemporary_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a330f506-5094-4630-b835-f7857e2e8321', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, property_rights_holders).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, welfare_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classified as property by law and practice; subjected to use (food, clothing, labor, experimentation, entertainment) in every domain of human activity. Under the abolitionist reading, property status itself—independent of how humanely the use is administered—constitutes the violation. Exit from this status is not an option individual animals can exercise; they bear the cost of being classified as non-agents whose interests do not count as standing claims on human conduct.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, biographical, trapped, global).

% Agricultural, pharmaceutical, research, entertainment, and consumer industries that rely on animal use for production, profit, and operational capacity. They benefit from the legal and cultural classification of animals as property, which removes transaction costs and moral friction from extraction. They set enforcement policy through industry standards, lobbying, legal defense, and cultural narrative ('humane use is possible and sufficient').
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, property_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, property_rights_holders, agenda_setter).

% Seek to reduce animal suffering within frameworks that accept use as legitimate. They are excluded from the abolitionist reading's conversation in the sense that their foundational premise (use is permissible if regulated) is foreclosed by the abolitionist axiom. They operate as a competing constraint reading and pay costs through institutional energy spent defending minimalist harm-reduction against the abolitionist claim that harm-reduction legitimizes the underlying violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_advocates, excluded).

% Legislatures, courts, professional bodies, and cultural institutions that codify and enforce property status. They maintain the legal kernel through statutory animal codes that define animals as property, court precedent that denies standing to animals, and professional standards that treat animal interests as secondary to human convenience. Enforcement is theatrical—'animal welfare' regulations that create the appearance of protection while preserving property classification.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_and_cultural_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Activists, scholars, and advocates who hold that property status is the violation and use is categorically wrong. They are structurally excluded from standard legal and policy processes because the framing—property classification is non-negotiable—is already settled by the institutions. They contest the kernel itself, not merely the terms of use.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_movement, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, property_rights_holders).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates human social order around a baseline assumption: animals lack independent moral standing and may be classified as property. This coordinate—shared across agriculture, law, medicine, and culture—enables complex institutional arrangements (markets, property law, professional practice) that depend on uncontested animal availability for production. The abolitionist reading asserts this coordinate itself is a violation; there is no legitimate coordination problem it solves except 'how to extract from non-agents without moral friction.'
% TRANSFER_FUNCTION: Transfers the full capacity, body, and life of animals (classified as property) from animals' own interests to human purposes: labor, reproduction, body parts, and death in service of human consumption, research, entertainment, and convenience. The transfer is enforced by legal non-personhood and cultural denial of animal agency. Property-holders collect the material benefit (food, profit, scientific data, entertainment); animals bear the cost (captivity, pain, death, reproductive control, confinement).
% ABSENT_VOICES: Animals themselves cannot participate in the legal and cultural conversation that classifies them. The abolitionist reading emphasizes this structural exclusion: the victim class has no vote, no voice, no seat at the negotiating table. Welfare advocates are not absent but are excluded from the abolitionist framing because they accept the foundational premise (use is permissible). Competitors in downstream industries (alternative agriculture, synthetic food, non-animal research methods) are also largely absent, kept out by property-rights institutions and market dominance of animal-use industries.
% DISAPPEARANCE_RATIONALE: If the constraint—the legal and cultural classification of animals as property—disappeared, the entire human relationship to animals would reorganize: agriculture would shift to alternative production; pharmaceuticals and research would develop non-animal methods; food and clothing would become plant-based or synthetic; legal standing would extend to animals, enabling litigation and protection. The constraint is not a natural law; it is a constructed institutional order. Its disappearance would be a civilizational reorganization, not a return to nature.
% FOUNDING_PROBLEM: The founding problem, in the abolitionist reading, is not a genuine problem at all—it is a cover story. The abolitionist reading claims the constraint was built to preserve institutional convenience and profit, not to solve a real coordination problem. The articulated founding problem ('humans need food and materials; animals are a resource') is rejected as question-begging: the 'need' is constructed and the 'resource' status is exactly what the abolitionist reading rejects as the violation.
% FOUNDING_PROBLEM_CORROBORATION: The abolitionist reading rejects corroboration-by-beneficiaries: property-rights holders and food industries attest the founding problem is alive and urgent, but they are the parties benefiting from the constraint's persistence. Abolitionist philosophers (Singer, Regan, Adams, Francione) and non-beneficiary sources (environmental scientists showing agricultural viability of alternatives, technologists demonstrating non-animal research methods) argue the founding problem is constructed and its stated urgency is a post-hoc justification for inherited practices. No consensus corroboration exists; the disagreement IS the kernel contest.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).

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
 *   Extractiveness is very high (0.89 at interval end) because the constraint transfers the entire productive and biological capacity of animals to human purposes with no reciprocal benefit or consent mechanism. Property status removes all friction—animals are not parties to any negotiation. Suppression is high (0.76) because the constraint's persistence depends on: (1) legal non-personhood, preventing animals from holding rights or bringing claims; (2) cultural denial of animal agency ('they don't really suffer in the ways that matter' or 'this is natural hierarchy'); (3) market dominance making alternatives economically marginal; and (4) professional gatekeeping in research, agriculture, and medicine that excludes non-animal methods. Theater ratio (0.42) reflects the recent rise of 'humane' labeling, welfare certifications, and corporate animal-care commitments—visible activity that creates the appearance of progress without challenging property status. The measurement series shows extractiveness and theater both rising slightly over 40 time units: as abolitionist critique gains visibility, property-rights holders deploy more sophisticated welfare messaging (higher theater) while extraction itself intensifies (more animals, faster throughput, more intensive confinement in some sectors). Suppression requirement rises gradually as resistance mounts (from sanctuary movements, plant-based industry, non-animal research advocacy) and more enforcement effort is needed to maintain property classification against mounting alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary's perspective (property-rights holders, industries), this constraint is rational resource management—animals lack the cognitive or moral complexity to hold independent interests, use is permissible and necessary, and welfare improvements are genuine progress. From the victim's perspective (animals), property status is the crime; welfare improvements are theatricality that prolongs captivity. From the welfare advocate's perspective, this reading is absolutist and economically naive, while property-rights holders see it as ideologically extreme and impractical. The engine computes these perspectival positions from the structural data: the victim class (powerless, trapped, no exit) will register the constraint as snare across every institutional seat; the beneficiary class (institutional power, arbitrage exit, policy control) may experience it as rope or even coordination; the welfare advocates sit between, contesting the framing. The abolitionist reading's structural claim is that there is no legitimate seat that experiences this constraint as coordination—there is no real problem it solves except 'how to profit from non-persons.'
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the sole victim (d → 1.0, full target): trapped status, no exit options, powerless, no voice in the arrangement. Property-rights holders are the beneficiary (d → 0.0, full beneficiary): institutional power, set the rules, arbitrage to alternative profit sources if forced, benefit directly from the use-transfer. Welfare advocates are excluded from the abolitionist reading's core logic—they are neither beneficiaries nor victims in the structural sense; they contest the foundational premise. Legal authorities are agenda-setters (they enforce), not primary beneficiaries or victims. The directionality is stark and one-dimensional: the constraint moves value from a powerless, trapped class to a powerful, rule-setting class. No override is needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading rejects the welfare reading's claim that incremental harm-reduction solves the founding problem. Under the abolitionist axiom, the founding problem ('how to ethically use animals') is incoherent—there is no ethical use of property-classified beings. Welfare improvements do not reduce mandatrophy; they extend it by creating the illusion that property status is compatible with ethical treatment. The constraint persists despite persistent resistance (abolitionist movement, sanctuary networks, alternative-food innovation) because property-rights holders have institutional and market power to suppress alternatives and enforce non-personhood. Theater ratio rising indicates mandatrophy in action: welfare regulation is deployed not to solve the founding problem but to manage the political threat posed by the abolitionist reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_derivation,
    'On what grounds does the abolitionist reading derive rights-bearing status for animals? Is it sentience (capacity to suffer), agency (autonomy and self-direction), relational standing (membership in a moral community), or inherent worth (status independent of capacities)?',
    'Philosophical argument and cross-species empirical work. Different groundings produce different victim sets and different responses to marginal cases (insects, mollusks, fetal animals). Singer''s sentience-based ground differs from Regan''s agency-based ground.',
    'A sentience-based ground might admit some threshold for moral status; an agency-based ground might exclude very young animals; an inherent-worth ground extends categorically to all animals. The victim set size and the abolitionist constraint''s scope depend on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_derivation, conceptual, 'The foundation of the claim that animals have rights-bearing status.').

omega_variable(
    property_status_contingency,
    'Is property classification of animals a contingent institutional choice (could be otherwise), or a structural consequence of human biological and cognitive superiority (quasi-natural)?',
    'Historical and anthropological analysis. Evidence: animal-personhood frameworks in some Indigenous and non-Western traditions; contemporary legal experiments with non-human personhood (river personhood, animal standing in courts); alternative food systems and research methods that do not depend on animal use.',
    'If contingent, property status is a chosen constraint that could be abolished; if quasi-natural, abolition is utopian. This distinction affects whether the constraint is enforceable or inevitable. The abolitionist reading treats it as contingent; the property reading treats it as inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, empirical, 'Whether property classification is chosen or inevitable.').

omega_variable(
    suppression_internalization,
    'How much of the measured suppression (0.76) is structural (legal barriers, market dominance, resource control) versus internalized (humans are socialized to not perceive animal suffering as a standing moral claim)?',
    'Post-exposure trajectories: when humans encounter intensive abolitionist arguments and animal agency evidence (sanctuary visits, documentaries, philosophical texts), do they shift their moral perception, or do they retrench in denial? If they shift, suppression is partly internalized and partly structural; if they retrench, suppression is more structural.',
    'Internalized suppression persists even if legal and market barriers fall; it requires cultural reframing, not just institutional change. This affects the cost and timescale of abolition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'The balance between structural and internalized suppression of animal moral status recognition.').

omega_variable(
    welfare_reading_foreclosure,
    'Does the abolitionist axiom (property status is inherently violating) logically foreclose the welfare reading (regulated use is permissible), or do they merely coexist as live alternatives?',
    'Philosophical debate. The abolitionist claim is that any property-based use perpetuates victimization, which would logically foreclose welfare incrementalism. The welfare reading''s counterclaim is that harm-reduction is progress and use can be ethically permissible if regulations are strict enough.',
    'If the abolitionist reading forecloses welfare, they occupy the same decision-space and one must prevail. If they coexist, they represent different institutional visions that could persist in parallel (some jurisdictions abolish, others regulate). This affects the network relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reading_foreclosure, conceptual, 'Logical relationship between abolitionist and welfare readings of the animal_moral_status kernel.').

omega_variable(
    alternatives_viability,
    'Are technologically and economically viable alternatives to animal use currently available (plant-based food, cultivated meat, non-animal research methods, synthetic materials) or are they emerging but not yet scaled?',
    'Technology roadmap analysis, market data, regulatory environment study. Current status (circa 2026): plant-based proteins are viable at scale in some regions; cultivated meat is under regulatory review; non-animal testing methods are approved but not yet fully integrated into pharma pipelines.',
    'If alternatives are viable now, abolition is economically defensible; if they are emerging, abolition requires transition infrastructure. Theater ratio (0.42) suggests property-rights holders are signaling concern about viability—if alternatives truly were impossible, welfare messaging would be unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternatives_viability, empirical, 'Whether technological and economic alternatives to animal use exist or are foreseeable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_moral_status__abolitionist_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_moral_status__abolitionist_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__abolitionist_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.81).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_moral_status__abolitionist_reading, base_extractiveness, 5, 0.83).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_moral_status__abolitionist_reading, base_extractiveness, 15, 0.87).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__abolitionist_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_moral_status__abolitionist_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_moral_status__abolitionist_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__abolitionist_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.05).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three constraint stories: property_reading (animals are property, no independent standing), welfare_reading (animals are sentient, suffering is bad, regulated use is permissible), and abolitionist_reading (animals are rights-bearing, property status is the violation, no use is ethical). These are not different views of the same constraint; they are three structurally distinct constraints with different victim sets, different beneficiaries, and different ε values. The property_reading treats animals as non-moral agents (ε near 0, mountain-like); the welfare_reading treats animals as interests-bearing but use-permissible (ε moderate, tangled_rope); the abolitionist_reading treats animals as victims of property classification itself (ε high, snare). The readings compete for institutional legitimacy and legal codification. All three should be authored as separate constraint stories and linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
