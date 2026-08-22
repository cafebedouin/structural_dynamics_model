% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Animal Property Status as Structural Victimization (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the animal moral
 *   status kernel: animals are rights-bearing individuals for whom property
 *   status itself constitutes the violation, such that no degree of humane
 *   treatment within a use-relationship can render that relationship
 *   legitimate. Under this reading, welfare reform is not a mitigation of the
 *   underlying arrangement but the primary mechanism that stabilizes and
 *   legitimates it — certification schemes, humane slaughter standards, and
 *   enriched housing requirements operate entirely inside the property frame
 *   and therefore, by this reading's own lights, cannot resolve the violation
 *   they purport to soften. The referent of ε here is the standing
 *   property-based use arrangement as this reading sees it (high extraction),
 *   not the rights-respecting abolition the reading endorses as remedy —
 *   consistent with the fixed ε referent rule for kernel readings.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary targets (powerless/trapped) — bear the extraction the property relation authorizes
 *   - laboratory_animals: primary targets (powerless/trapped) — instrumentalized for research under regulated but property-preserving conditions
 *   - companion_animals_under_ownership: targets under a softer frame (powerless/trapped) — affectionate treatment does not dissolve legal disposability
 *   - captive_wildlife_and_entertainment_animals: targets (powerless/trapped) — held for exhibition under welfare codes that do not contest captivity itself
 *   - agricultural_and_biomedical_industries: primary beneficiary/agenda_setter (institutional/arbitrage) — administers and depends on the property framework
 *   - welfare_regulators_and_certifiers: administers the legitimating mechanism (institutional/constrained) — institutional survival depends on the property frame remaining intact
 *   - abolitionist_advocates: excluded voice (organized/constrained) — the reading's own position, structurally marginalized from rule-making
 *   - legal_scholars_and_courts: analytical observer (institutional/analytical) — adjudicates the doctrinal boundary this reading contests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.88).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.79).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status as Structural Victimization (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '176caeb9-20a2-4402-aa0c-d72029d4020b').
narrative_ontology:cs_kernel_codification('176caeb9-20a2-4402-aa0c-d72029d4020b', distributed).
narrative_ontology:cs_authority_grounding('176caeb9-20a2-4402-aa0c-d72029d4020b', distributed).
narrative_ontology:cs_reading_relation('176caeb9-20a2-4402-aa0c-d72029d4020b', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('176caeb9-20a2-4402-aa0c-d72029d4020b', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('176caeb9-20a2-4402-aa0c-d72029d4020b', foundational, property_status_is_intrinsically_a_rights_violation).
narrative_ontology:cs_axiom_status(property_status_is_intrinsically_a_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('176caeb9-20a2-4402-aa0c-d72029d4020b', property_status_is_intrinsically_a_rights_violation, deontological).
narrative_ontology:cs_axiom('176caeb9-20a2-4402-aa0c-d72029d4020b', secondary, humane_treatment_cannot_cure_a_disposability_relation).
narrative_ontology:cs_axiom_status(humane_treatment_cannot_cure_a_disposability_relation, holdable).
narrative_ontology:cs_axiom_grounding('176caeb9-20a2-4402-aa0c-d72029d4020b', humane_treatment_cannot_cure_a_disposability_relation, deontological).
narrative_ontology:cs_reference_frame('176caeb9-20a2-4402-aa0c-d72029d4020b', common_law_property_classification).
narrative_ontology:cs_drift_state('176caeb9-20a2-4402-aa0c-d72029d4020b', contemporary_personhood_litigation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('176caeb9-20a2-4402-aa0c-d72029d4020b', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, companion_animals_under_ownership).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, captive_wildlife_and_entertainment_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_regulators_and_certifiers).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consumers_of_animal_products_and_services).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, property_status_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, humane_use_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and slaughtered as legal property within agricultural production. Under this reading, their status as property is itself the injury, independent of how they are physically treated; welfare improvements do not alter the underlying ownership relation that authorizes their use and disposal.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Used in research contexts where their bodies are instrumentalized for human knowledge production. Regulatory 'humane' standards (anesthesia protocols, housing minimums) govern treatment but never contest the underlying premise that they may be owned and used as means.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Live under legal ownership even in relationships popularly framed as caring or reciprocal. Under this reading, affection does not dissolve the property relation; the animal remains disposable at the owner's discretion (sale, euthanasia, relinquishment), which is the violation regardless of how well the animal is treated.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, companion_animals_under_ownership, payer,
    powerless, biographical, trapped, global).

% Held for exhibition, tourism, or performance under licenses and welfare codes that regulate conditions without questioning the legitimacy of captivity itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, captive_wildlife_and_entertainment_animals, payer,
    powerless, biographical, trapped, global).

% Administer and lobby for the legal property framework, set the welfare standards that operate within it, and depend on continued lawful use of animals as inputs. From the abolitionist reading's perspective they set the terms of an arrangement whose core premise (ownership) is the thing being contested, not merely its execution.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, agricultural_and_biomedical_industries, agenda_setter,
    institutional, generational, arbitrage, global).

% Design and enforce humane-treatment standards (space requirements, slaughter methods, anesthesia rules) that operate entirely within the property frame. Their institutional legitimacy and continued funding depend on the premise that use can be rendered acceptable through regulation, which the abolitionist reading treats as the specific mechanism that launders the underlying violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulators_and_certifiers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_regulators_and_certifiers, beneficiary).

% Purchase food, research outcomes, entertainment, and companionship structured by the property relation. They can individually exit through consumption choices, but the legal architecture that produces the animals as property persists independent of any individual's choice to abstain.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consumers_of_animal_products_and_services, beneficiary,
    moderate, biographical, mobile, global).

% Argue that the entire property-based legal architecture is illegitimate and that welfare reform is a legitimating mechanism rather than a remedy. Largely excluded from regulatory rule-making, which is dominated by industry and welfare-science stakeholders who accept the property premise as a given starting point.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate the boundary of animal legal personhood and property status case by case. Their rulings currently uphold property status nearly universally, but the doctrinal question of whether some animals could hold limited legal personhood is actively litigated in several jurisdictions.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_scholars_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, agricultural_and_biomedical_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, under this reading — the arrangement solves a problem for human users (reliable, low-cost access to animal bodies, labor, and companionship) but does not solve any problem FOR the animals; from the abolitionist standpoint there is no genuine mutual coordination function to identify, only an extraction relation dressed as one.
% TRANSFER_FUNCTION: Moves bodily autonomy, offspring, labor, and life itself from animals to human owners, users, and consumers; welfare regulation moves a portion of the resulting surplus into compliance costs and certification labels without altering the direction of the underlying transfer.
% ABSENT_VOICES: Animals themselves cannot testify to interests in the legal or political process that sets the terms of their use; abolitionist advocates who would eliminate the property relation entirely are structurally marginalized in favor of industry and welfare-science voices who accept property status as the frame within which reform occurs.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight, the entire agricultural, biomedical research, entertainment, and companion-animal-commerce apparatus would have to reorganize around a non-ownership relationship to animals — supply chains, research protocols, and legal liability frameworks all depend on animals being classifiable as property that can be bought, sold, confined, and killed.
% FOUNDING_PROBLEM: Historically, animal property status was built to solve the problem of allocating agricultural, labor, and research resources predictably under a legal system that required clear title and liability rules for anything of economic value.
% FOUNDING_PROBLEM_CORROBORATION: Industry and welfare-regulatory bodies attest the arrangement still serves a live resource-allocation and food-security function. Independent legal scholars researching animal personhood doctrine (a source outside the beneficiary set) attest that the original resource-allocation problem no longer requires categorical property status to solve, and that the persistence of blanket property classification reflects institutional and economic inertia rather than continued necessity.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.88) because, by this reading's own premises, every unit of use under property status is a unit of victimization regardless of physical treatment quality — there is no floor at which 'humane' use stops counting as extraction. Suppression is high (0.79) because the arrangement's persistence depends on active legal enforcement of property rights over animals (anti-cruelty statutes explicitly preserve rather than abolish ownership, and legal personhood claims are routinely defeated in court) and on marginalizing the abolitionist position from regulatory participation. Theater ratio is authored substantial and rising (0.22 to 0.42) because welfare certification and humane-standard regimes increasingly perform legitimacy-conferral — 'humanely raised,' 'cruelty-free' labeling — while the underlying ownership and disposal relation is unchanged; this is precisely the mechanism this reading identifies as most objectionable. Accessibility collapse is moderate (0.40), lower than a mountain profile, because legal alternatives (personhood litigation, sanctuary movements, plant-based substitution) exist and are actively pursued, even though they have not yet displaced the dominant framework. Resistance is moderate (0.55): organized abolitionist advocacy is real and growing but currently overmatched by industry and welfare-regulatory incumbency.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (industry, welfare regulators), the arrangement reads as legitimate regulated use — a coordination structure balancing human need against animal welfare. From the payer seats (the animals themselves, as this reading insists on treating them as rights-bearing parties), the same structure reads as total extraction with no legitimate coordination function at all, because the thing being coordinated (reliable access to animal bodies) is precisely the violation. The engine computing divergent per-seat types from this same structural data is the intended signature of a contested kernel reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   All four animal-stakeholder groups are declared victims/payers with trapped exit — under the abolitionist premise, legal property status forecloses any structural exit for the animal, regardless of how the animal is materially treated, so directionality sits at the full-target end for all of them. Agricultural and biomedical industries and welfare regulators are structural beneficiaries/agenda-setters with institutional power and strong (arbitrage or constrained) exit, since their revenue, funding, or institutional mandate depends on continued lawful use. Consumers are moderate beneficiaries with individually mobile exit (they can choose not to consume), but their aggregate exit does not alter the legal architecture, which is why the constraint's persistence does not depend on any individual consumer's continued participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (predictable resource allocation for agricultural, labor, and research uses under clear legal title) is authored as contested rather than flatly dead: industry attests it remains live, while legal scholarship on animal personhood — corroboration from outside the beneficiary set — argues the original allocation problem no longer requires categorical property status to solve, since alternative legal constructs (trusts, limited personhood, guardianship models) could allocate the same resources without extending disposability. This mismatch (contested founding-problem status against a world_rearranges disappearance verdict) is exactly the signature the R5 genealogy interview is designed to surface: the arrangement may have outlived the necessity of its specific legal form even where a residual coordination need persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingent_or_structural,
    'Is animal property status a contingent legal choice that could be abolished through ordinary legislative or judicial change, or a structural feature of how legal systems must classify non-human interests?',
    'Track jurisdictions that have granted limited legal personhood or ''sentient being'' status distinct from property (e.g., certain civil code amendments in France, Quebec, New Zealand) and observe whether the underlying use-relationships actually change or whether property-equivalent treatment persists under a relabeled status.',
    'If contingent, this reading supports tangled_rope reclassification (a coordination function exists but rides on suppressible extraction that could be structurally removed); if structural (no legal system can function without some form of instrumentalizable classification for non-persons), the reading is closer to a genuine snare with no coordination function to recover, which is the classification authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_status_contingent_or_structural, conceptual, 'Whether property status is a removable legal artifact or an structural necessity of legal personhood systems.').

omega_variable(
    welfare_reform_as_legitimation_or_genuine_mitigation,
    'Does welfare regulation function primarily to legitimate continued use (a laundering mechanism, as this reading holds) or does it produce genuine, non-trivial reductions in suffering independent of its legitimating effect?',
    'Comparative analysis of animal welfare outcomes and industry practice change in jurisdictions with strong versus weak welfare regulation, holding the underlying property status constant, would separate the mitigation effect from the legitimation effect.',
    'If welfare reform produces substantial independent mitigation, the theater_ratio authored here (0.42) may be overstated and part of the regulatory apparatus should be read as a genuine (if partial) coordination function rather than pure legitimation theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_genuine_mitigation, empirical, 'Whether welfare regulation is substantively mitigating or primarily legitimating.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct framing of this kernel the property-status boundary (property vs. rights-bearing individual), or is there a more fundamental framing at the level of moral patienthood criteria (sentience, interests, autonomy) that would generate a different partition of readings entirely?',
    'Compare classifications under a moral-patienthood-criteria framing (grouping readings by which capacity — sentience, autonomy, or neither — grounds moral status) against the property-status framing used here; check whether the reading boundaries and resulting ε values shift.',
    'If the moral-patienthood framing produces a different reading partition (e.g., splitting the welfare_reading into a sentience-sufficient-for-protection reading and a sentience-insufficient reading), the three-reading kernel structure used here would be under-inclusive and additional sibling constraints would be needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative kernel framing by moral-patienthood criteria versus property-status boundary, and whether it changes the reading partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__abolitionist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__abolitionist_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__abolitionist_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__abolitionist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__abolitionist_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__abolitionist_reading, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__abolitionist_reading, base_extractiveness, 24, 0.85).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__abolitionist_reading, base_extractiveness, 32, 0.87).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__abolitionist_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__abolitionist_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__abolitionist_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__abolitionist_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_moral_status kernel, decomposed per the ε-invariance principle rather than treated as one story with a measurement parameter. property_reading authors near-zero extraction (animals categorically excluded from the moral patient class, so no victimhood is recognized structurally). welfare_reading authors moderate extraction (cruelty is wrong but regulated use is legitimate, so ε reflects only the gap between actual and ideal humane treatment). abolitionist_reading (this file) authors high extraction (all use under property status counts as victimization regardless of treatment quality). The three files share no beneficiary/victim data and must not be averaged; they are linked here so contamination and drift analysis can trace how empirical or legal developments in one reading (e.g., a jurisdiction granting limited personhood) propagate pressure onto the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
