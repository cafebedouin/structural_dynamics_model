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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Property Status as Structural Violation (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status treats the legal and
 *   institutional classification of animals as property—not regulation of
 *   use, not welfare optimization, but the classification itself—as the
 *   structural violation. From this reading, all use of animals, however
 *   'humanely' conducted, perpetuates victimization because it presupposes
 *   animals lack independent moral standing. The reading identifies animals
 *   as moral patients with interests (in bodily autonomy, life, freedom from
 *   exploitation) whose systematic denial of moral standing is enforced
 *   through property law and institutional practice. Extraction is high
 *   (0.89) because the constraint systematically transfers bodily autonomy
 *   and biological products without consent; suppression is substantial
 *   (0.76) because enforcing property status requires active exclusion of
 *   animals from moral consideration frameworks and suppression of cognitive
 *   dissonance in human beneficiaries. Theater ratio is moderate (0.42)
 *   because welfare regulations perform ethical concern while preserving
 *   property status—they are performative without addressing the foundational
 *   violation.
 *
 * KEY AGENTS:
 *   - domesticated_animals: victims of property classification (trapped, powerless)
 *   - wild_animals_under_human_dominion: victims of habitat control and extraction management (trapped, powerless)
 *   - captive_wildlife: victims of confinement justified by property law (trapped, powerless)
 *   - industrial_agricultural_operators: agenda-setters administering property-based extraction (institutional)
 *   - research_institutions: agenda-setters using animals as property-classified tools (institutional)
 *   - consumers_of_animal_products: beneficiaries of low-cost extraction enabled by property status (organized)
 *   - abolitionist_advocates: excluded from governance but articulating the core moral claim (moderate)
 *   - regulatory_authorities: observers presupposing property status (institutional)
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
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status as Structural Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '1d85f9d3-bfd5-45f4-b647-e9b75d449e1a').
narrative_ontology:cs_kernel_codification('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', distributed).
narrative_ontology:cs_authority_grounding('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', extraction).
narrative_ontology:cs_interpretation_layer_present('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a').
narrative_ontology:cs_reading_relation('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', foundational, animals_are_moral_patients).
narrative_ontology:cs_axiom_status(animals_are_moral_patients, holdable).
narrative_ontology:cs_axiom_grounding('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', animals_are_moral_patients, deontological).
narrative_ontology:cs_axiom('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', foundational, property_status_precludes_moral_standing).
narrative_ontology:cs_axiom_status(property_status_precludes_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', property_status_precludes_moral_standing, deontological).
narrative_ontology:cs_reference_frame('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', animals_as_rights_bearing_individuals).
narrative_ontology:cs_drift_state('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', contemporary_industrial_use_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d85f9d3-bfd5-45f4-b647-e9b75d449e1a', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, domesticated_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, wild_animals_under_human_dominion).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, captive_wildlife).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, animal_moral_agency).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, sentience_as_moral_ground).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, abolition_incompatible_with_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred into dependency on human systems, legally classified as property rather than rights-bearers. Bear all costs of the property arrangement—bodily confinement, reproductive control, labor extraction, slaughter—without legal standing to refuse or contest. Exit is impossible: reliance is biological and legal.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, domesticated_animals, payer,
    powerless, biographical, trapped, global).

% Subject to habitat control, hunting regulation, captive breeding, research, and ecosystem management that treats them as resources for human extraction. Property classification extends to wildlife governance: their status as objects of human use is enforced through regulation and law. Exit is impossible: habitat degradation and human expansion trap them structurally.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, wild_animals_under_human_dominion, payer,
    powerless, biographical, trapped, global).

% Held in zoos, circuses, research facilities, and entertainment venues under property law. Legal status as property means their confinement, breeding, and exploitation is classified as legitimate ownership rather than violation. Exit is impossible: confinement is absolute and legally protected.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, captive_wildlife, payer,
    powerless, biographical, trapped, global).

% Administer the property-based animal extraction system, extracting labor and biological products from animals classified as property. Justify use through welfare regulations that preserve property status while professionalizing cruelty mitigation. Set enforcement machinery to defend property rights and suppress exit alternatives (e.g., plant-based substitution framing as threat).
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, industrial_agricultural_operators, agenda_setter,
    institutional, generational, mobile, global).

% Use animals as research tools under property classification and institutional review frameworks that treat animal subjects as equipment rather than individuals with interests. Defend animal use in research via welfare standards (IACUC reviews) that presuppose property status and utility calculus.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, research_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from low-cost animal products enabled by property classification and extraction. Also bear secondary moral cost if they recognize animals as victims: cognitive dissonance maintained through suppression of slaughter reality and framing of welfare improvements as ethical solutions. Exit (dietary/consumption change) is available but framed as costly, extreme, or implausible.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, consumers_of_animal_products, payer).

% Articulate the property-as-violation premise and call for abolition of animal use systems. Excluded from governance frameworks that presuppose property status as legitimate; their core moral claim (animals are not property) is structurally absent from institutional decision-making. Exit would mean abandoning advocacy; they remain outside because the institutions they critique treat their premises as false rather than live alternatives.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, biographical, mobile, global).

% Develop welfare standards, enforce animal cruelty laws, and adjudicate animal-use disputes. Operate within a framework that presupposes property status; their regulatory power exists to manage extraction, not to question whether property classification is legitimate. They observe but do not adjudicate the foundational moral question.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, industrial_agricultural_operators).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading instantiates NO coordination function. The abolitionist reading rejects the premise that animal use solves a collective-action problem. Instead, it identifies property status as an imposed classification that concentrates benefits to human agents while systematically denying moral standing to animal agents. The 'coordination' is enforcement of a moral hierarchy, not solution of a coordination problem.
% TRANSFER_FUNCTION: Moves bodily autonomy, reproductive capacity, labor, and biological products from animals (classified as property) to human agents (classified as persons). The transfer is unidirectional and non-consensual: animals bear all costs; human beneficiaries (consumers, producers, researchers) collect benefits. Property status is the mechanism that makes the transfer legible and enforceable.
% ABSENT_VOICES: Animals themselves—the primary victims—have no standing in institutions that recognize their interests only through a human-advocate intermediary or through appeal to welfare regulations that presuppose property status. Abolitionist advocates articulate the reading but are structurally excluded from governance forums where animal use is defended and regulated. Non-human moral claims are absent not by chance but by design of the property framework.
% DISAPPEARANCE_RATIONALE: If property classification of animals and the use-relationships it enables disappeared overnight, human food systems, pharmaceutical development, entertainment industries, and ecological management would reorganize within years. Capital investment in animal agriculture would face crisis; supply chains dependent on animal products would collapse or be replaced by plant-based alternatives already technically available. The global distribution of labor, resource flows, and institutional authority would shift substantially.
% FOUNDING_PROBLEM: Animals are sensitive, living beings; human dominion over them, if anything, creates a moral obligation of non-harm that is violated by property status itself—treating morally significant individuals as objects for use. The founding problem from the abolitionist reading is not a coordination problem humans needed to solve, but a moral error—the classification of animals as property—that persists because it is institutionally entrenched and beneficially structured for human interests.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from non-human-interest constituencies outside the benefiting class: animal advocates, some philosophers and ethicists (Singer, Regan, Francione), and empirical primatology/ethology showing animal cognition and suffering incompatible with property status. Contestation comes from agricultural, research, and consumer interests that affirm property status as justified. The question of whether animals are property or rights-bearers remains actively disputed across jurisdictions and disciplines.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.89) and rising slightly over the interval (0.81→0.89) because industrial animal agriculture has intensified while nominal welfare improvements have lagged. Suppression is substantial (0.76) and stable because enforcing property classification requires continuous legal and institutional work to exclude animal moral claims—it is not maintained by inertia but by active defense. Theater ratio starts lower (0.25) and rises (→0.42) because welfare certification and 'humane' labeling have become more prominent performance mechanisms without reducing the property-status violation. Accessibility collapse is moderate (0.68) because alternatives exist (plant-based products, lab-grown meat, synthetic alternatives) but are actively framed as implausible, extreme, or insufficient—exit is possible but suppressed through narrative as well as structural means. Resistance is high (0.71) because abolitionist movements, plant-based advocacy, and animal welfare legislation all create countervailing pressure—the constraint persists despite substantial resistance. The abolitionist reading claims Snare (pure extraction, no coordination function) because it rejects the premise that animal use solves a genuine collective-action problem; instead, property classification is a chosen arrangement that benefits specific institutional actors (producers, researchers, consumers) while victimizing animals.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (industrial producers, research institutions) should compute this as coordination—they see animal property status as a legitimate and necessary basis for human flourishing. The victim seats (animals, though they have no explicit voice) would compute this as pure extraction and coercive confinement. Beneficiary-consumer seats would compute it as coordination + indirect cost they can suppress through narrative. The abolitionist advocate seat would compute it as Snare: extractive, justified by false premises, enforced through suppression of alternatives. The engine computes these per-seat classifications from the structural data; the authored claim reflects the abolitionist reading specifically.
 *
 * DIRECTIONALITY LOGIC:
 *   From the abolitionist reading: animals bear all costs (property status denies moral standing, confinement, extraction, death). Their directionality approaches full target (d near 1.0): trapped powerless agents with zero exit options. Human beneficiaries (consumers, producers, researchers) sit near beneficiary end: they benefit from property-classified extraction without bearing corresponding costs. Industrial and research institutions are agenda-setters: they administer and defend property status, collecting institutional prestige and material benefit. Regulatory authorities are observers: they operate within property-presupposing frameworks, not adjudicating property status itself. Abolitionist advocates are excluded: they articulate an alternative moral premise (animals are not property) but are structurally absent from institutions that presuppose property as legitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (animals deserve moral consideration and non-exploitation) remains CONTESTED—benefiting parties affirm property status as justified; non-benefiting parties (animals, advocates) reject it. Disappearance verdict is WORLD_REARRANGES: animal use systems are substantial institutional and economic arrangements. The mismatch (live founding problem + world_rearranges disappearance verdict) identifies the constraint as potentially zombie/captured—the founding problem (animals matter morally) is denied while the arranging persists. This is exactly the abolitionist claim: the constraint persists not because it solves an unsolved problem but because it distributes benefits to powerful human interests while denying moral standing to powerless animal interests. The theater ratio (0.42, rising) supports mandatrophy: welfare improvements are performative—they reduce cognitive dissonance without addressing property status itself, which is the abolitionist reading's core concern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingency,
    'Is the legal classification of animals as property a contingent institutional choice, or is it structurally entailed by the fact of human dominion?',
    'Historical and comparative analysis: jurisdictions that have legally recognized animal personhood (e.g., some protections for great apes in some contexts) against those maintaining strict property status. Theoretical analysis of whether property classification follows logically from power differential or is a chosen legal convention.',
    'If contingent, abolition is possible via legal reclassification while preserving human existence and flourishing—the constraint is Snare (extractive, enforced, suppressing alternatives). If structural, abolition would require species coexistence frameworks outside law—the constraint''s type remains Snare but remediation is civilizational rather than legislative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, conceptual, 'Whether property status is a contingent legal choice or structurally entailed.').

omega_variable(
    suppression_mechanism_externalized_consciousness,
    'Is the measured suppression (0.76) primarily structural (physical confinement, legal prohibition of animal self-advocacy) or internalized (humans psychologically compartmentalize animal suffering to maintain moral consistency with consumption)?',
    'Empirical: study individual behavioral shift when non-animal-product substitutes are made economically equivalent or cheaper (does suppression persist?). Cognitive science: examine the persistence and resilience of cognitive dissonance reduction strategies when the material incentive (low cost) is removed.',
    'If primarily structural, removing legal property status and closing confinement systems would rapidly reduce suppression. If significantly internalized, even legal abolition would face cultural and psychological resistance—humans would need to renegotiate their relationship to animal suffering, not just their legal and economic systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_externalized_consciousness, empirical, 'Whether suppression is structural/external or internalized/cognitive.').

omega_variable(
    kernel_reading_contested_status,
    'Is the abolitionist reading a coherent instantiation of the animal_moral_status kernel, or does it repudiate the kernel itself (treating property classification not as one reading but as the entire corrupted framework)?',
    'Philosophical analysis of the abolitionist premise: does it accept animal_moral_status as a contested terrain with multiple readings, or does it reject the very framework of property-status-as-a-reading? Can abolitionism coexist within the kernel with property and welfare readings, or is it incompatible with the kernel''s structure?',
    'If coherent-within-kernel, this is one reading among three; the engine treats reading_relations as coexists_with or forecloses per the structural analysis. If it repudiates the kernel framework itself, the constraint is not a reading but a foundational critique—a different object ontologically. If repudiation, the network edge to sibling readings reflects mutual exclusion at the level of legitimacy frames, not coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested_status, conceptual, 'Whether abolitionism is a reading within the kernel or a refusal of the kernel''s framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(anim_tr_t0, projected).
narrative_ontology:measurement(anim_tr_t5, animal_moral_status__abolitionist_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(anim_tr_t5, projected).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_moral_status__abolitionist_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__abolitionist_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.81).
narrative_ontology:measurement_basis(anim_be_t0, projected).
narrative_ontology:measurement(anim_be_t5, animal_moral_status__abolitionist_reading, base_extractiveness, 5, 0.83).
narrative_ontology:measurement_basis(anim_be_t5, projected).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_moral_status__abolitionist_reading, base_extractiveness, 15, 0.87).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__abolitionist_reading, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(anim_su_t0, projected).
narrative_ontology:measurement(anim_su_t5, animal_moral_status__abolitionist_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(anim_su_t5, projected).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_moral_status__abolitionist_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__abolitionist_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the contested kernel animal_moral_status. The property_reading and welfare_reading are separate constraints with different beneficiary/victim structures and ε values. Sibling readings differ on whether animals have moral standing (property: no; welfare: conditional; abolitionist: yes, absolute). The network edge indicates structural interdependence: abolition arguments directly challenge property-reading premises and pressure welfare-reading toward inadequacy. All three readings should be generated separately; they form a constraint family linked by this network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
