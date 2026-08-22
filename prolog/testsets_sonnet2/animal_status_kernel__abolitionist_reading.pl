% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status — Abolitionist Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   Under the abolitionist reading, the legal category of 'animal property'
 *   is not a spectrum of better- or worse-regulated use but a single
 *   categorical wrong: treating a being with interests as an object that can
 *   be owned, bought, sold, confined, and killed. Welfare reforms — bigger
 *   cages, painless slaughter methods, enrichment programs — do not touch the
 *   injustice this reading identifies, because the injustice is the ownership
 *   relation itself, not the suffering that accompanies it. This produces the
 *   reading's most distinctive structural feature: every improvement
 *   campaigned for by welfare-oriented actors is treated by this reading as
 *   either irrelevant to, or actively legitimizing of, the core wrong,
 *   generating a genuine strategic and philosophical rift with the welfare
 *   reading rather than a difference of degree.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.78).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status — Abolitionist Reading").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '176a0860-5e91-46df-ba66-ed508f09f014').
narrative_ontology:cs_kernel_codification('176a0860-5e91-46df-ba66-ed508f09f014', distributed).
narrative_ontology:cs_authority_grounding('176a0860-5e91-46df-ba66-ed508f09f014', distributed).
narrative_ontology:cs_reading_relation('176a0860-5e91-46df-ba66-ed508f09f014', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('176a0860-5e91-46df-ba66-ed508f09f014', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_axiom('176a0860-5e91-46df-ba66-ed508f09f014', foundational, property_status_itself_constitutes_injustice).
narrative_ontology:cs_axiom_status(property_status_itself_constitutes_injustice, holdable).
narrative_ontology:cs_axiom_grounding('176a0860-5e91-46df-ba66-ed508f09f014', property_status_itself_constitutes_injustice, deontological).
narrative_ontology:cs_axiom('176a0860-5e91-46df-ba66-ed508f09f014', foundational, welfare_conditions_are_morally_irrelevant_to_permissibility_of_use).
narrative_ontology:cs_axiom_status(welfare_conditions_are_morally_irrelevant_to_permissibility_of_use, holdable).
narrative_ontology:cs_axiom_grounding('176a0860-5e91-46df-ba66-ed508f09f014', welfare_conditions_are_morally_irrelevant_to_permissibility_of_use, deontological).
narrative_ontology:cs_reference_frame('176a0860-5e91-46df-ba66-ed508f09f014', animals_as_unowned_moral_persons).
narrative_ontology:cs_drift_state('176a0860-5e91-46df-ba66-ed508f09f014', contemporary_legal_personhood_litigation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('176a0860-5e91-46df-ba66-ed508f09f014', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, fur_and_leather_industry).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, captive_entertainment_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, fur_bearing_animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, property_status_as_categorical_injustice).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, moral_personhood_independent_of_species).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns, breeds, confines, and slaughters animals as commercial property, converting their bodies and reproductive capacity into commodities. Lobbies legislatures to keep property status intact and to route all reform pressure into welfare-standard adjustments rather than status change. Captures the entire value of the animals' lives and deaths.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, agenda_setter).

% Uses animals as experimental instruments justified by property status and research-exemption law. Depends on the legal classification of animals as usable research material; funds welfare-oriented 'humane research' standards to preempt abolition arguments while use itself continues unchanged.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Confines animals for display, performance, and recreational use (zoos, circuses, marine parks, racing). Monetizes the animals' captivity itself as the product. Can relocate operations across jurisdictions with more permissive property law when regulation tightens.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_industry, beneficiary,
    powerful, biographical, mobile, national).

% Breeds and kills animals specifically for their skins and fur, treating the animal as raw material with no interest that survives the transaction. Exit from any single jurisdiction's restrictions is easy because production shifts to less-regulated regions.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, fur_and_leather_industry, beneficiary,
    powerful, biographical, mobile, global).

% Bred into existence as commodities, confined for the duration of a foreshortened life, and killed for economic value. Under the abolitionist reading these are not welfare deficits to be minimized but the direct enactment of the core injustice: being owned at all. They have no legal standing to exit or object; every improvement in confinement conditions leaves the ownership relation, and therefore the injustice, intact.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Used as experimental subjects and destroyed or discarded when no longer useful. Institutional review boards can regulate procedure but cannot, under existing law, question the underlying premise that the animal may be used as property in the first place.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Held in permanent captivity as display or performance assets. Enclosure enrichment and veterinary care programs address suffering symptoms while leaving the possession relation that authorizes the captivity untouched.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, captive_entertainment_animals, payer,
    powerless, biographical, trapped, national).

% Bred solely to be killed for their pelts; have no use to the owner except as material. Under this reading, no welfare standard applied during the breeding period changes the fact that the animal exists and dies solely as an owned object.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, fur_bearing_animals, payer,
    powerless, immediate, trapped, global).

% Negotiate incremental confinement and slaughter-method standards with industry and legislators. From the abolitionist reading's perspective they are effectively excluded from the actual question at issue — they operate entirely within the property frame and their successes are read by this reading as legitimizing continued ownership rather than progress toward its end. They would object to this characterization if asked, arguing their reforms reduce suffering now and build political capacity for later abolition.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, welfare_reform_organizations, observer).

% Litigate and campaign for a categorical change in animals' legal status from property to rights-holder or person, refusing welfare-standard negotiation as legitimizing the underlying wrong. Have essentially no institutional power to compel the change directly; their leverage is public moral argument and strategic litigation seeking to establish legal personhood precedents.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocacy_organizations, agenda_setter,
    organized, generational, constrained, global).

% Adjudicate animal-status disputes and set the legal boundary of property law. Have so far uniformly declined to grant animals rights-holder or person status, instead layering welfare statutes onto an unmodified property base. Could, in principle, revise the underlying classification but have shown no institutional appetite to do so.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, under this reading. The property arrangement does not solve a genuine coordination problem between morally considerable parties — it authorizes one party (owners) to use another (sentient beings with interests) as means, which is the injustice itself rather than a solution to a shared problem.
% TRANSFER_FUNCTION: Moves the entire value of an animal's existence, reproductive capacity, labor, body, and death to the owner, while the animal bears all the cost — confinement, use, and typically premature death — with no compensating claim recognized in law.
% ABSENT_VOICES: The animals themselves have no legal voice or standing to contest their classification; welfare organizations that could amplify some of their interests are, on this reading, structurally captured by having accepted the property frame as the negotiating table, which removes the categorical objection from the conversation entirely.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight, entire industries built on ownership, breeding, confinement, and slaughter of animals would become legally impossible in their current form — agriculture, biomedical research using animal subjects, fur production, and captive entertainment would have to be reorganized or cease. This is precisely why the abolitionist reading treats the arrangement as load-bearing injustice rather than neutral background: its disappearance is consequential exactly because so much current economic activity depends on it.
% FOUNDING_PROBLEM: Historically, animal property law emerged to formalize ownership, inheritance, and commercial exchange of animals as agricultural and economic assets — solving problems of resource allocation and commercial certainty among humans, with animals as the object of the transaction rather than a party to it.
% FOUNDING_PROBLEM_CORROBORATION: The founding commercial-certainty problem is attested by legal historians and by the beneficiary industries themselves, who cite continued economic reliance on clear property title in animals as justification for retaining the classification. Abolitionist philosophers (Francione and others, writing from outside the beneficiary industries) corroborate that the problem the arrangement solves is real but argue it is a problem of convenience for owners, not a problem whose solution requires treating a sentient being as an object — i.e., they corroborate the genealogy while denying its continued moral legitimacy.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.91, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored very high (0.91) because, on this reading's own terms, every instance of use — food, research, entertainment, fur — constitutes a rights violation regardless of the welfare conditions under which it occurs; there is no threshold of 'humane' treatment that reduces the extraction, because the extraction is the ownership relation, not the treatment. Suppression is high and rising (0.70 to 0.78) because maintaining property status against a mature and organized abolitionist critique requires increasingly active legal and political defense — industry lobbying, welfare-standard co-optation, and legislative resistance to personhood litigation. Theater ratio rises over the interval (0.20 to 0.42) because welfare-labeling programs, humane certification schemes, and corporate 'animal welfare' commitments increasingly substitute visible compliance performance for any change in the underlying property status — exactly the pattern this reading identifies as function-preserving theater. Accessibility collapse is authored moderate-low (0.35) rather than mountain-high, because alternatives to the property frame (rights-holder status, legal personhood) are conceptually and even judicially articulated, if not yet institutionally adopted — the alternative has not collapsed, it has been refused.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary industries sit at the low-directionality end: they set terms, capture the entire economic value of the animals used, and retain arbitrage-grade exit across jurisdictions when regulation tightens. The animal victim classes sit at the maximal-directionality end: trapped, powerless, with no legal standing to exercise any exit at all — the paradigm case of a fully captured target. Welfare reform organizations are marked excluded/observer rather than payer or beneficiary because, from this reading's seat, their negotiating position inside the property frame removes them from being either a genuine coordinated party or a victim — they are treated as structurally adjacent but not squarely inside the constraint's operative injustice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — commercial certainty in ownership and exchange of animals as agricultural assets — is authored as live, not dead: the industries that rely on the classification still need it for exactly the reason it was created. This blocks a lazy mandatrophy read where the arrangement is dismissed as merely obsolete; the abolitionist critique is not that the original problem disappeared, but that solving a human commercial-certainty problem by denying moral status to a sentient party was never legitimate to begin with, regardless of whether the certainty problem is real and current.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_as_abolition_pathway_or_obstacle,
    'Do incremental welfare reforms function as a pathway toward eventual abolition of property status, or as a pressure-release mechanism that stabilizes and prolongs it?',
    'Longitudinal comparison of jurisdictions that pursued welfare-first strategies versus abolition-first strategies, tracking whether welfare gains correlate with later personhood/rights litigation success or with entrenchment of the property frame.',
    'If welfare reform empirically advances abolition, the abolitionist reading''s categorical rejection of welfare-oriented allies may be strategically counterproductive even if philosophically consistent. If welfare reform empirically forestalls abolition, it corroborates this reading''s core strategic claim and its structural exclusion of welfare organizations as a genuine ally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_as_abolition_pathway_or_obstacle, empirical, 'Whether welfare reform is a strategic pathway to or an obstacle against abolition.').

omega_variable(
    moral_personhood_boundary_ambiguity,
    'Which nonhuman beings, if any, meet the threshold for moral personhood this reading asserts, and is that threshold itself defensible against the objection that it is drawn to match pre-existing intuitions about which animals matter?',
    'Comparative work across cognitive science, philosophy of mind, and comparative ethology assessing whether proposed personhood criteria (sentience, self-awareness, interests, autonomy) draw a principled line or an intuition-matched one.',
    'If the personhood boundary cannot be drawn non-arbitrarily, the categorical claim that ALL use is impermissible may need to be qualified by species or capacity, softening the reading''s central claim toward something closer to a graduated framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_personhood_boundary_ambiguity, conceptual, 'Whether the moral-personhood criterion this reading relies on is principled or intuition-matched.').

omega_variable(
    reading_framing_kernel_choice,
    'Is the kernel here best framed as ''the legal-moral status of animals'' (a single contested classification question, as authored) or should it instead be framed at the level of ''the legitimacy of using sentient beings as means,'' with property status, welfare regulation, and personhood as three downstream institutional answers to that deeper question?',
    'Compare how each framing routes disagreement: the status-classification framing yields three coexisting institutional readings (as authored here); the deeper means-legitimacy framing might instead produce a forecloses relation between abolitionist and property readings (since ''usable as pure means'' and ''never usable as means'' cannot both hold in one framework) while still leaving welfare_reading as an unstable middle position.',
    'Under the alternative deeper framing, the abolitionist_reading to property_reading relation might be authored as forecloses rather than coexists_with, since the two make directly contradictory claims about permissible use-as-means; the coexists_with framing chosen here reflects that, empirically, both readings are held as live institutional positions by different legal systems simultaneously, which is a fact about the world rather than a fact about logical compatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_kernel_choice, conceptual, 'Alternative kernel framing (status-classification vs. means-legitimacy) would change the reading_relations classification between abolitionist and property readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__abolitionist_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__abolitionist_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__abolitionist_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__abolitionist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__abolitionist_reading, base_extractiveness, 8, 0.89).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__abolitionist_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__abolitionist_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__abolitionist_reading, base_extractiveness, 32, 0.91).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__abolitionist_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__abolitionist_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__abolitionist_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__abolitionist_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept of 'animal moral/legal status' per the ε-invariance principle. property_reading authors near-zero extraction from its own premises (no injustice recognized in ownership); welfare_reading authors moderate extraction consistent with a Tangled Rope (genuine welfare coordination function retained alongside continued ownership-based extraction); abolitionist_reading (this file) authors near-maximal extraction because it treats the ownership relation itself, independent of welfare conditions, as the violation. All three share the same underlying kernel (the legal-moral classification of nonhuman animals) but are authored as separate constraints with independent ε, beneficiary/victim sets, and classifications, linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
