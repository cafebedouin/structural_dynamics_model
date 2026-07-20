% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Animal Rights Status
 *   domain: applied ethics / legal philosophy / political economy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_status kernel: the claim that nonhuman animals possess rights or
 *   inherent value that precludes all instrumental human use. The constraint
 *   prohibits animal agriculture, vivisection, entertainment using animals,
 *   and product extraction, coordinating human society around the recognition
 *   of animal personhood while extracting heavily from industries and
 *   consumers structured around animal use. The kernel decomposes three
 *   structurally distinct constraints from the colloquial label 'animal
 *   status' â property, welfare, and abolitionist readings â each with
 *   different Îµ values, victim sets, and enforcement requirements.
 *
 * KEY AGENTS:
 *   - nonhuman_animals: Primary beneficiary (powerless/trapped) â receive rights-based protection from instrumental use
 *   - animal_agriculture_sector: Primary payer (powerful/constrained) â bears the cost of production prohibition
 *   - biomedical_research_sector: Primary payer (institutional/constrained) â loses animal models and must retool
 *   - animal_entertainment_industry: Primary payer (organized/constrained) â prohibited from using animals in performance
 *   - animal_product_consumers: Secondary payer (organized/constrained) â lose access and bear substitution costs
 *   - abolitionist_legal_advocates: Agenda setter (organized/mobile) â administer and enforce the rights framework
 *   - welfare_reform_advocates: Excluded voice (organized/constrained) â accept regulated use and are rejected as legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.88).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Animal Rights Status").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied ethics / legal philosophy / political economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8').
narrative_ontology:cs_kernel_codification('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', formalized).
narrative_ontology:cs_authority_grounding('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', lineage).
narrative_ontology:cs_interpretation_layer_present('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8').
narrative_ontology:cs_reading_relation('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', foundational, species_boundary_morally_irrelevant).
narrative_ontology:cs_axiom_status(species_boundary_morally_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', species_boundary_morally_irrelevant, deontological).
narrative_ontology:cs_reference_frame('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', universal_animal_rights).
narrative_ontology:cs_drift_state('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', contemporary_legal_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cc08463a-f4ad-4d35-9aa8-1e0c0bf8dca8', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, nonhuman_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_agriculture_sector).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, biomedical_research_sector).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_entertainment_industry).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_product_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a legal framework that recognizes them as rights-holders and prohibits their use for food, research, entertainment, or labor. They do not choose to enter or exit the human legal system; their protection depends entirely on enforcement against human users.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, nonhuman_animals, beneficiary,
    powerless, biographical, trapped, global).

% Comprises industrial and traditional farming operations that breed, raise, and slaughter animals for food and fiber. Under the abolitionist constraint, these operations are prohibited and must transition to plant-based agriculture or cease, bearing the full capital and cultural cost of the transition.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_sector, payer,
    powerful, biographical, constrained, global).

% Comprises pharmaceutical companies, universities, and testing facilities that use animal models for research and regulatory compliance. The constraint removes their access to animal subjects and requires adoption of alternative methods, imposing retooling costs and challenging established scientific paradigms.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_sector, payer,
    institutional, biographical, constrained, global).

% Includes circuses, marine parks, racing, and film production that use animals for performance and spectacle. The constraint prohibits these activities entirely, forcing sectoral abandonment or transition to animatronic and human-centered formats.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_entertainment_industry, payer,
    organized, biographical, constrained, global).

% Individuals and households that purchase and consume meat, dairy, leather, and other animal-derived products. The constraint eliminates these goods from legal markets, requiring dietary and lifestyle adaptation and bearing the cost of substitute goods.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_product_consumers, payer,
    organized, biographical, constrained, global).

% Lawyers, legislators, and movement strategists who draft, litigate, and enforce animal rights statutes. They do not collect material rents from the constraint; their work is aimed at eliminating the property status of animals and closing welfare loopholes.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_legal_advocates, agenda_setter,
    organized, generational, mobile, global).

% Organizations and policymakers who promote improved conditions for animals within continued instrumental use frameworks, such as cage-free requirements or humane slaughter standards. The abolitionist reading rejects their position as legitimizing exploitation, excluding them from the constraint's legitimating discourse.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human conduct around the recognition that nonhuman animals are not resources, eliminating the collective-action problem of unchecked speciesist exploitation by removing the permission structure entirely.
% TRANSFER_FUNCTION: Transfers the legal and moral permission to use animals instrumentally from human industries and consumers to the animals themselves, who gain rights-based protection; also transfers social and economic costs of transition to the former users.
% ABSENT_VOICES: Welfare reform advocates who would permit regulated use; indigenous and traditional communities for whom animal use is culturally central; industry scientists who frame animal research as medically necessary. They are excluded because the abolitionist framework treats all instrumental use as categorically illegitimate, leaving no room for regulated compromise.
% DISAPPEARANCE_RATIONALE: If the abolitionist constraint disappeared, industrial animal agriculture, vivisection, and entertainment using animals would resume or expand; legal personhood for animals would collapse; markets, research pipelines, and food systems would reorganize around instrumental use.
% FOUNDING_PROBLEM: The systematic instrumentalization of sentient nonhuman animals as human property and resources, resulting in mass suffering, death, and ecological harm, compounded by the failure of welfare regulation to stop the exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Veterinary epidemiologists documenting industrial animal disease, ecological economists measuring livestock environmental externalities, and independent legal scholars analyzing rights-of-nature jurisprudence â attesting from seats not materially benefiting from abolitionist policy adoption.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint prohibits entire sectors rather than taxing or regulating them, extracting productive existence from animal-use industries. Suppression is similarly high (0.85) because persistence depends on actively preventing a vast, historically entrenched practice. Theater is moderate (0.35): the rights framework is substantively pursued, but institutionalization risks ritualistic enforcement that substitutes procedural compliance for genuine protection. Accessibility collapse is high (0.90) because accepting the abolitionist premise morally collapses instrumental use as an alternative. Resistance is very high (0.88) because powerful industries and cultural traditions actively oppose the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist agenda-setter seat experiences the constraint as the realization of moral progress and the correction of a categorical error. The animal-use industry payer seats experience it as existential extraction: their business models are prohibited, not regulated. The nonhuman animal beneficiary seat experiences it as the removal of exploitation. The divergence is structural: the same legal prohibition is liberation from one seat and destruction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Nonhuman animals are the structural beneficiaries (d near 0.0): the constraint subsidizes their protection by eliminating instrumental use. Animal-use industries and consumers are structural targets (d near 1.0): they bear the prohibition's costs. Abolitionist advocates are agenda-setters with mixed directionality (d moderate): they administer the constraint but do not capture extracted rents; their benefit is moral and political vindication, not material extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading resists mandatrophy by rejecting welfare reforms as legitimation. Where a welfare scaffold might atrophy into a snare (performative protection that sustains use), the abolitionist constraint refuses the coordination-with-extraction hybrid at the level of principle. Its tangled_rope classification comes from the structural fact that ending instrumental use genuinely coordinates protection (for animals) while extracting existence from industries â not from welfare-theater, but from a genuine rights-framework whose enforcement is inherently costly to some parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    global_scope_necessity,
    'Does the abolitionist constraint require universal global adoption to protect animals, or can partial jurisdiction-level implementation succeed without leakage undermining outcomes?',
    'Compare animal harm metrics in jurisdictions with strong abolitionist norms against trade-adjusted harm metrics accounting for imported animal products from non-abolitionist jurisdictions.',
    'If leakage nullifies local protection, the constraint functions only as global_infrastructure and its effective scope must be universal; if leakage is manageable, regional or national scope suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_scope_necessity, empirical, 'Whether abolitionist protection requires universal scope to be effective').

omega_variable(
    speciesism_suppression_mechanism,
    'Is human resistance to abolitionism driven primarily by structural economic dependence on animal use, or by internalized speciesist cognition that persists after economic incentives are removed?',
    'Measure resistance trajectories in post-industrial populations with low economic dependence on animal use but continued consumption; compare with populations where animal industries dominate employment.',
    'If internalized, effective suppression is higher than structural measures suggest and exit_options for human payers are closer to identity_locked; if structural, transition subsidies would reduce resistance significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speciesism_suppression_mechanism, conceptual, 'Structural vs internalized basis of resistance to animal rights').

omega_variable(
    framing_underdetermination,
    'Does framing the kernel as ''animal legal status'' versus ''scope of human moral community'' change the computed cs_pattern and coordination_type?',
    'Re-classify the constraint under each framing and compare whether identity_coordination (human moral identity) or enforcement_mechanism (legal rights) produces different coupling thresholds.',
    'If framing shifts the pattern, the constraint''s extractiveness scaling and Boltzmann floor change, affecting whether it reads as tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Framing under-determination in commitment system classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraints: property_reading (animals as legal objects), welfare_reading (regulated use with sentience constraints), and abolitionist_reading (rights-based prohibition). Each reading has a different Îµ, victim set, and enforcement logic. The abolitionist reading influences welfare discourse by rejecting regulated use as legitimation, but does not causally depend on the welfare reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
