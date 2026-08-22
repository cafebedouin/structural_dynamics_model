% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Instrumental Use of Animals as Rights Violation (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the contested
 *   animal-status kernel: animals hold inherent value and rights that
 *   categorically preclude their use as means to human ends, regardless of
 *   welfare conditions attached to that use. Under this reading, every
 *   category of instrumental use — agriculture, biomedical research,
 *   exhibition, fur/leather production — is extractive by construction,
 *   because the coordination story (humane treatment, scientific necessity,
 *   conservation education) is read as cover for an arrangement whose actual
 *   function is one-directional appropriation of animal bodies and lives.
 *   Welfare reforms, far from mitigating the extraction, are read as
 *   legitimation machinery that stabilizes and entrenches continued use by
 *   making it appear progressively more acceptable. This is a single reading
 *   among three siblings sharing the animal_status kernel (welfare_reading,
 *   property_reading); ε is authored for the standing arrangement of
 *   instrumental use as this reading's own lights assess it — not for the
 *   rights-respecting arrangement the reading endorses, which would trivially
 *   yield ε≈0.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: primary institutional beneficiary, sets terms of confinement and slaughter
 *   - farmed_animals, laboratory_animals, captive_exhibition_animals, fur_bearing_animals: full victim set under this reading, trapped exit, powerless
 *   - welfare_reform_organizations: excluded from this reading's own framework because their theory of change presupposes legitimate continued use
 *   - abolitionist_advocates: analytical observer seat advancing legal personhood as the resolution path
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.86).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Instrumental Use of Animals as Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'ba923c2f-d7e0-419b-99b5-4adfa4562ce1').
narrative_ontology:cs_kernel_codification('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', distributed).
narrative_ontology:cs_authority_grounding('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', distributed).
narrative_ontology:cs_reading_relation('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', foundational, animals_possess_inherent_value_precluding_use_as_means).
narrative_ontology:cs_axiom_status(animals_possess_inherent_value_precluding_use_as_means, holdable).
narrative_ontology:cs_axiom_grounding('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', animals_possess_inherent_value_precluding_use_as_means, deontological).
narrative_ontology:cs_axiom('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', foundational, welfare_improvement_does_not_offset_rights_violation).
narrative_ontology:cs_axiom_status(welfare_improvement_does_not_offset_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', welfare_improvement_does_not_offset_rights_violation, deontological).
narrative_ontology:cs_reference_frame('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', pre_rights_instrumental_use_default).
narrative_ontology:cs_drift_state('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', contemporary_animal_law_movement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ba923c2f-d7e0-419b-99b5-4adfa4562ce1', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, entertainment_and_exhibition_operators).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, fur_and_leather_manufacturers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, captive_exhibition_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, fur_bearing_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, welfare_reform_organizations).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, property_status_of_animals_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, welfare_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Breeds, confines, and slaughters animals for food production at industrial scale, capturing the entire economic surplus of the arrangement. Lobbies for welfare-standard legislation that stabilizes the practice of use itself rather than ending it, and funds research and certification schemes (humane labels, welfare audits) that this reading treats as legitimating cover rather than genuine reform.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter).

% Uses animals as experimental subjects under institutional review boards that weigh scientific benefit against animal suffering but never treat the animal's use itself as categorically foreclosed. Benefits from regulatory frameworks (three Rs: replace, reduce, refine) that this reading reads as optimizing extraction rather than eliminating it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Operates zoos, circuses, and marine parks that confine animals for display and revenue. Markets confinement as conservation or education, which this reading treats as narrative cover for continued captivity regardless of enclosure quality.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, entertainment_and_exhibition_operators, beneficiary,
    organized, biographical, mobile, national).

% Converts animal bodies into consumer goods after killing, treating the animal as raw material input to a supply chain. Has no exit cost from this reading's perspective other than product substitution, since the entire business model is premised on instrumental use this reading holds impermissible.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_and_leather_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Confined from birth to slaughter, bred and killed for human consumption with no capacity to exit, consent, or be represented in the arrangement that uses them. This reading holds that no degree of confinement improvement changes the fact of their use as means.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bred and used in experimental procedures ranging from observational studies to invasive testing, with institutional review weighing costs against human benefit rather than asking whether the use is permissible at all. Cannot exit; disposal after use is routine.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Held in enclosures for public display and breeding programs framed as conservation. Cannot leave, cannot decline exhibition, and this reading treats claimed conservation benefit as insufficient to license continued captivity.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, captive_exhibition_animals, payer,
    powerless, biographical, trapped, national).

% Raised or trapped specifically for pelts and hides, killed as the terminal step of production. This reading regards this category as the clearest case of pure instrumentalization with no coordination benefit to the animal whatsoever.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, fur_bearing_animals, payer,
    powerless, immediate, trapped, global).

% Campaigns for improved cage sizes, slaughter methods, and enclosure standards, achieving incremental reductions in suffering within continued use. This reading treats these organizations as structurally excluded from the abolitionist conversation because their theory of change presupposes the legitimacy of continued use, and reads their successes as entrenching rather than dismantling the arrangement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, welfare_reform_organizations, beneficiary).

% Purchases meat, dairy, leather, and animal-tested products at prices that do not reflect any cost internalized to the animal. Has full capacity to substitute toward non-animal alternatives at relatively low personal cost, distinguishing this seat from the trapped animal seats.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).

% Argues from rights-based and inherent-value premises that no degree of welfare reform can justify instrumental use, and works toward legal personhood or rights-holder status for animals rather than improved conditions of use. Positioned analytically outside the extraction structure but advocates for its complete dissolution rather than management.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem being solved between humans and animals under this reading — animals cannot consent to or benefit from the arrangement as coordination partners. The only coordination occurring is among human institutions (producers, researchers, regulators, consumers) to sustain and legitimate continued use.
% TRANSFER_FUNCTION: The arrangement transfers animal bodies, labor, reproductive capacity, and lives into economic value, scientific data, entertainment revenue, and consumer goods, flowing entirely from animals (who bear all costs including death) to human industries and consumers (who capture all benefits).
% ABSENT_VOICES: The animals themselves cannot testify to their own interests in any forum that adjudicates the arrangement; their situation is represented only through proxies (advocates, welfare scientists) whose testimony is systematically weighed against human economic and scientific interests rather than treated as dispositive. Welfare organizations are excluded from this reading's own conversation because they accept the premise this reading rejects.
% DISAPPEARANCE_RATIONALE: If instrumental use of animals were prohibited overnight, entire industries (agriculture, biomedical research, fur, exhibition) would need to reorganize around plant-based and synthetic alternatives, non-animal research methods, and sanctuary-based animal care — a transformation on the scale of the abolition of chattel slavery, which this reading's own literature frequently invokes as the closest structural analogy.
% FOUNDING_PROBLEM: Historically, animal use was framed as solving problems of food security, medical advancement, and economic development, treating animals as an available resource because no competing rights claim was recognized.
% FOUNDING_PROBLEM_CORROBORATION: Industry beneficiaries attest the founding problem (food security, medical necessity) remains live and justifies continued use, citing population growth and disease burden. Abolitionist philosophers and legal scholars (from outside the beneficiary set, e.g. animal law academics and moral philosophers) attest the founding problem has been substantially resolved by viable alternatives (plant-based nutrition, in vitro and computational research methods) and that the arrangement now persists primarily as institutional and economic inertia rather than necessity — though this corroboration itself remains contested by industry-funded researchers, so is not univocal.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.91, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-maximal (0.88-0.91) because under this reading every instance of instrumental use — including humane-certified and heavily welfare-regulated instances — constitutes rights violation with zero legitimate coordination offset; there is no discount for improved conditions since the rights violation is categorical, not gradient. Suppression is high (0.79-0.86, rising) because the arrangement depends on animals' complete inability to exit, contest, or be heard, and because legal and economic infrastructure (property status, welfare-sufficiency doctrine) actively forecloses rights-based challenges. Theater ratio rises substantially over the interval (0.22 to 0.58) reflecting this reading's core empirical claim: that welfare certification, humane labeling, and improved-enclosure marketing have proliferated as a growing share of industry activity precisely because they perform reform without altering the fact of use — the mechanism this reading identifies as legitimation absorbing what would otherwise be abolitionist pressure. Accessibility collapse is authored moderate (0.35) rather than high because, unlike a mountain, real alternatives (plant-based production, non-animal research methods, sanctuary models) are known, viable, and increasingly available — the collapse is incomplete, which is part of why this reading treats the arrangement as chosen extraction rather than inevitability. Resistance is high (0.78) reflecting the growing abolitionist and animal-rights movement actively contesting the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (agriculture, research institutions, exhibition operators, fur/leather manufacturers, consumers) sit near the full-beneficiary end: they capture all economic, scientific, or entertainment value and bear none of the cost internalized to the animal. Animal victim classes sit at the full-target extreme: trapped exit options, zero capacity for consent or exit, immediate time horizon (their situation is often terminal), and total exposure to whatever the arrangement does to them. Consumers are treated as beneficiaries with mobile exit (low personal switching cost to alternatives) rather than trapped victims, differentiating structural position from moral complicity. Welfare organizations occupy an unusual excluded-but-beneficiary-adjacent position: this reading treats their reform successes as functionally beneficial to the extractive industries (legitimation) even though the organizations' stated aim is to reduce animal suffering.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (food security, medical necessity) as substantially obsolete given viable alternatives, while the arrangement persists and, on this reading's account, intensifies its legitimation apparatus (rising theater_ratio) — a pattern consistent with mandatrophy: an arrangement whose original justification has eroded while institutional machinery sustaining it has not merely persisted but grown more sophisticated at appearing justified. The abolitionist reading's classification as snare (rather than tangled_rope) turns on this reading's core empirical claim: it denies there is any genuine coordination function on the animal side at all (animals gain nothing from being farmed, tested on, or displayed), which is the structural difference from the welfare reading's likely tangled_rope classification (which would grant some coordination function, e.g. legitimate research necessity, alongside extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_of_animal_interests,
    'Do animals possess inherent value and rights that categorically preclude instrumental use, or only interests that generate welfare constraints compatible with continued use?',
    'This is fundamentally a normative/philosophical question not resolvable by empirical data alone, though it is informed by comparative cognitive science, capacity-for-suffering research, and the historical trajectory of rights-expansion arguments (analogized to prior expansions of the moral circle). Different ethical frameworks (rights-based, interest-based utilitarian, contractarian) yield different answers.',
    'If the abolitionist premise is correct, the entire instrumental-use economy is a snare with zero legitimate coordination function. If the welfare premise is correct instead, portions of the same activity would classify as tangled_rope (genuine coordination plus extraction) rather than pure extraction. This is the central fork between the kernel''s three readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_of_animal_interests, preference, 'The kernel-level normative disagreement between abolitionist, welfare, and property readings of animal moral status.').

omega_variable(
    welfare_reform_as_legitimation_or_genuine_harm_reduction,
    'Do welfare reforms (improved cage sizes, slaughter standards, enclosure quality) function primarily to legitimate and stabilize continued use, or do they produce genuine, non-trivial harm reduction independent of their legitimating effect?',
    'Longitudinal tracking of whether welfare certification adoption correlates with reduced consumption/production volumes (consistent with genuine harm reduction and movement toward abolition) versus stable or increased volumes with increased price premiums and consumer approval (consistent with legitimation without reduction).',
    'If reforms are shown to reduce absolute animal suffering without increasing total use, the abolitionist reading''s blanket rejection of welfare reform as pure theater becomes harder to sustain and the reading''s theater_ratio trajectory would need revision. If reforms correlate with stable or expanding use, the legitimation reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_genuine_harm_reduction, empirical, 'Whether welfare reform functions as harm reduction or as extraction-legitimating theater, central to this reading''s rising theater_ratio claim.').

omega_variable(
    committer_structure_reading_disagreement_location,
    'Where exactly does the abolitionist reading''s disagreement with its siblings live structurally: in the beneficiary/victim set (does it differ on WHO is harmed), in the coordination-function claim (does it differ on WHETHER any coordination exists), or in the acceptability threshold (does it differ on HOW MUCH extraction is tolerable)?',
    'Comparative analysis of the three sibling constraint files'' declared coordination_function, beneficiaries/victims, and extractiveness values once all three are authored, to locate the precise structural fork.',
    'This determines whether the three readings are better modeled as disagreeing about facts (empirical), about framework (conceptual), or about values (preference) — which affects how contamination propagation and network analysis should treat the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_reading_disagreement_location, conceptual, 'Locating the precise structural disagreement among the three sibling readings of the animal_status kernel, per Rule 2 of the committer frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(anim_tr_t8, animal_status__abolitionist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(anim_tr_t16, animal_status__abolitionist_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(anim_tr_t24, animal_status__abolitionist_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(anim_tr_t32, animal_status__abolitionist_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t8, animal_status__abolitionist_reading, base_extractiveness, 8, 0.89).
narrative_ontology:measurement(anim_be_t16, animal_status__abolitionist_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement(anim_be_t24, animal_status__abolitionist_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(anim_be_t32, animal_status__abolitionist_reading, base_extractiveness, 32, 0.91).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.79).
narrative_ontology:measurement(anim_su_t8, animal_status__abolitionist_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(anim_su_t16, animal_status__abolitionist_reading, suppression_requirement, 16, 0.81).
narrative_ontology:measurement(anim_su_t24, animal_status__abolitionist_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(anim_su_t32, animal_status__abolitionist_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the animal_status kernel (abolitionist, welfare, property), each authored as a separate constraint per the ε-invariance principle. The abolitionist reading forecloses the property_reading's core premise (that animals lack moral standing sufficient to ground rights) — the two cannot coexist within a single coherent framework, since one asserts and the other denies animal rights-holder status as a categorical matter. The abolitionist reading coexists_with the welfare_reading in public discourse and animal-advocacy politics: both are live positions held by different factions of the animal protection movement, with welfare advocates often viewing abolitionists as unrealistic and abolitionists viewing welfare advocates as complicit in legitimation, but neither logically forecloses the other since welfare interests and rights-based inherent value are not strictly contradictory (an animal could have both, on the welfare_reading's own terms, though it stops short of the abolitionist conclusion). Expect the welfare_reading to author a materially lower extractiveness (partial coordination function via legitimate research/food-security necessity) and expect the property_reading to author near-zero extractiveness given its premise that no rights exist to violate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
