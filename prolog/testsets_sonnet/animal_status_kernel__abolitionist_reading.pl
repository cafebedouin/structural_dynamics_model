% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Animal Property Status as Categorical Rights Violation (Abolitionist Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the animal status
 *   kernel: animals are moral persons whose basic right not to be property is
 *   categorically violated by the property relation itself, independent of
 *   welfare conditions. Under this reading, the coordination story told by
 *   property-holding institutions (that ownership enables responsible
 *   stewardship) is not a genuine coordination function but cover for
 *   extraction, because the parties whose interests are most directly at
 *   stake — the animals — have no legal capacity to contest the arrangement
 *   at all, only to have their suffering weighed within it. Extractiveness is
 *   authored high (0.88) because ANY use is treated as rights-violating under
 *   this reading, not merely poorly-regulated use. This is a substantively
 *   different ε from the welfare reading (which locates the injury in
 *   unregulated suffering, not in use per se) and from the property reading
 *   (which locates no injury at all) — per the ε-invariance principle, these
 *   are three separate constraint stories, not one story measured three ways.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: institutional beneficiary/agenda_setter, arbitrage exit — collects economic value from the property relation and shapes its legal maintenance
 *   - farmed_animals: powerless payer, trapped exit — bears the full extraction with no capacity for consent, contest, or representation
 *   - welfare_reform_organizations: excluded/secondary beneficiary — pursue a different remedy this reading treats as insufficient or counterproductive
 *   - abolitionist_animal_rights_advocates: excluded, powerless, civilizational time horizon — hold the position this constraint instantiates but no formal standing in the institutions they contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.72).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status as Categorical Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '18bfd76e-0c16-421a-8640-c50155ab56f3').
narrative_ontology:cs_kernel_codification('18bfd76e-0c16-421a-8640-c50155ab56f3', distributed).
narrative_ontology:cs_authority_grounding('18bfd76e-0c16-421a-8640-c50155ab56f3', distributed).
narrative_ontology:cs_reading_relation('18bfd76e-0c16-421a-8640-c50155ab56f3', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('18bfd76e-0c16-421a-8640-c50155ab56f3', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('18bfd76e-0c16-421a-8640-c50155ab56f3', foundational, property_status_itself_is_the_injury).
narrative_ontology:cs_axiom_status(property_status_itself_is_the_injury, holdable).
narrative_ontology:cs_axiom_grounding('18bfd76e-0c16-421a-8640-c50155ab56f3', property_status_itself_is_the_injury, deontological).
narrative_ontology:cs_axiom('18bfd76e-0c16-421a-8640-c50155ab56f3', foundational, animal_moral_personhood_grounds_basic_rights).
narrative_ontology:cs_axiom_status(animal_moral_personhood_grounds_basic_rights, holdable).
narrative_ontology:cs_axiom_grounding('18bfd76e-0c16-421a-8640-c50155ab56f3', animal_moral_personhood_grounds_basic_rights, deontological).
narrative_ontology:cs_axiom('18bfd76e-0c16-421a-8640-c50155ab56f3', secondary, welfare_conditions_are_morally_irrelevant_to_use_permissibility).
narrative_ontology:cs_axiom_status(welfare_conditions_are_morally_irrelevant_to_use_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('18bfd76e-0c16-421a-8640-c50155ab56f3', welfare_conditions_are_morally_irrelevant_to_use_permissibility, deontological).
narrative_ontology:cs_reference_frame('18bfd76e-0c16-421a-8640-c50155ab56f3', animals_as_categorical_rights_holders).
narrative_ontology:cs_drift_state('18bfd76e-0c16-421a-8640-c50155ab56f3', contemporary_legal_personhood_litigation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('18bfd76e-0c16-421a-8640-c50155ab56f3', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, fur_and_leather_industry).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, captive_wildlife).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, companion_animals_bred_and_sold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, welfare_reform_organizations).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns, breeds, confines, and slaughters animals as a matter of legally titled property, deriving direct economic value from every stage of an animal's existence. Lobbies to keep the property classification intact and to frame any regulatory change as a welfare adjustment rather than a status question. Faces essentially no exit pressure because the property frame is the foundation of the entire business model.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, beneficiary).

% Purchases, breeds, and uses animals as research instruments under legal property title, with institutional review boards evaluating welfare but never ownership status itself. Benefits from the property frame because it makes animals fungible research inputs rather than rights-holders whose use would require a categorically different justification.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, biomedical_research_industry, beneficiary).

% Zoos, circuses, and breeding operations hold animals as property for display and performance, monetizing their captivity. Can relocate operations or rebrand as 'conservation' or 'sanctuary' entities under regulatory pressure without ceding the underlying property claim.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, entertainment_and_exhibition_industry, beneficiary,
    organized, biographical, mobile, national).

% Breeds and kills animals specifically for their skins as a property-derived commodity. Global supply chains let this industry shift production to jurisdictions with the weakest welfare regulation, insulating it from any single jurisdiction's reform.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, fur_and_leather_industry, beneficiary,
    organized, biographical, mobile, global).

% Bred into existence, confined, and killed as legal property with no standing to contest their own use. Under this reading, no welfare improvement changes their basic situation: their fundamental injury is being classified as property at all, a status they cannot exit, contest, or be represented against within any existing legal system.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bred and used as instrumental research property; welfare protocols govern pain minimization but never their eligibility for use itself. This reading holds their harm is the eligibility, not the pain — a distinction current animal-welfare law does not recognize.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Held as property for display, breeding programs, or entertainment; even accredited 'sanctuary' designations do not transfer legal title away from human ownership. Confinement persists regardless of enclosure quality.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, captive_wildlife, payer,
    powerless, generational, trapped, national).

% Bred, bought, and sold as property even within relationships that read as affectionate; can be surrendered, resold, or euthanized at an owner's discretion because legal personhood was never conferred. The abolitionist reading treats even well-treated companion animals as harmed by the underlying property relation itself.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animals_bred_and_sold, payer,
    powerless, biographical, trapped, national).

% Pursue incremental welfare legislation (cage-free mandates, slaughter regulations) and are treated by this reading as, at best, harm-reduction efforts that leave the categorical injury untouched and, at worst, legitimizing collaborators who make the property system more durable by making it appear humane. Excluded from full standing in the abolitionist analysis because their strategic premises are treated as part of the problem, not a resolution to it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, welfare_reform_organizations, beneficiary).

% Purchase meat, dairy, eggs, leather, and entertainment access derived from animal property at prices that do not reflect the categorical harm the abolitionist reading identifies. Retain full ability to exit through consumption choices, unlike the animals themselves.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, immediate, mobile, global).

% Argue for the categorical position from outside any seat that benefits from animal use; hold no property, extract no value, and have no formal standing in agricultural, research, or legislative decision-making beyond advocacy and litigation. Represent the position this constraint story instantiates but are structurally excluded from the institutions whose practices they contest.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_animal_rights_advocates, excluded,
    powerless, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading as legitimate: the property-status arrangement does coordinate predictable ownership, transfer, and use of animals for economic and research purposes, but the abolitionist position holds this coordination is coordination of an injustice, not a genuine solution to a legitimate problem — there is no version of 'well-coordinated animal ownership' that this reading treats as acceptable.
% TRANSFER_FUNCTION: Moves economic value, research capacity, and consumable goods from animals (via their bodies, labor, reproductive capacity, and lives) to industries and consumers, at a cost the reading holds animals cannot consent to, contest, or be compensated for because they lack the legal personhood required to hold a claim against their own use.
% ABSENT_VOICES: The animals themselves are the paradigm absent voice — the reading's central claim is that no legal mechanism currently allows their interests to be represented as rights-claims rather than as welfare considerations weighed against owner interests. Abolitionist advocates attempt to speak for them but hold no formal standing in the property system itself.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight in favor of basic personhood rights against use, the entire economic architecture of animal agriculture, much of biomedical research, and large parts of the entertainment and fashion industries would become categorically impermissible rather than merely regulable — a rearrangement of global supply chains, research methodology, and consumption patterns on a scale comparable to the abolition of human chattel slavery, which is the reading's own explicit analogy.
% FOUNDING_PROBLEM: Animals are used, confined, bred, and killed for human economic and social benefit; legal systems have historically addressed the resulting suffering only through welfare regulation that leaves the underlying ownership relationship — and the industries built on it — fully intact.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the animal rights movement (e.g., property law theorists analyzing the legal category of 'living property') corroborate that animals occupy a unique and internally inconsistent legal category — treated as property for ownership and transfer purposes but subject to some welfare constraints inapplicable to ordinary property. This internal inconsistency is documented independently of advocacy positions, though the corroborating scholars do not necessarily endorse the abolitionist remedy.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is set high (0.88) and held nearly flat across the interval because the abolitionist reading's core claim is that the injury does not respond to welfare-conditions improvement — the reading explicitly rejects the premise that better treatment reduces the wrong, since the wrong is the property status itself, not the treatment within it. Suppression starts very high (0.85) reflecting the near-total legal and cultural entrenchment of the property frame historically, and is authored with a modest downward trajectory (to 0.72) reflecting genuine legal and cultural gains for the abolitionist position over the interval (some jurisdictions have granted limited legal standing or 'sentient being' status distinct from ordinary property, and some courts have heard habeas corpus arguments on behalf of specific animals) — though the reading holds these remain far short of the categorical personhood the position demands. Theater ratio is kept low and rising slightly (0.08 to 0.15) because most industry activity is genuine economic function under the property frame, not performance, though some rise reflects increasing 'humane-washing' marketing that the abolitionist reading treats as legitimizing theater rather than substantive change. Accessibility collapse is authored moderate (0.35), not mountain-high, because the abolitionist alternative (legal personhood for animals) is an actively contested, non-collapsed alternative with growing legal scholarship and litigation behind it — this is precisely NOT a settled natural fact, which is why claimed_type is snare rather than mountain and why resistance is authored high (0.78): this reading meets substantial organized resistance from every industry with a stake in the property frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (agriculture, biomedical research, entertainment, fur/leather industries) map directly to institutions holding legal property title and extracting economic value from that title — the engine should derive low d (near-beneficiary) for these institutional seats given their arbitrage-grade exit options. Victim declarations (farmed animals, laboratory animals, captive wildlife, companion animals) map to the entities whose bodies and lives are the object of the property relation and who are, by construction of the reading, incapable of holding an exit option above 'trapped' — the derivation chain should place these at or near full-target d regardless of any welfare improvements, because the reading's whole point is that d does not move with treatment quality, only with status change.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this arrangement as a settled coordination mechanism (property law solving a genuine resource-allocation problem) when the abolitionist reading holds the founding problem — humans deriving benefit from animal use without any categorical constraint on the legitimacy of that use — remains fully live and has never been addressed, only managed. Welfare reform is treated within this reading not as progress toward resolution but as a mechanism that stabilizes the underlying extractive arrangement by making it appear increasingly justified, which is itself the strategic tension this reading holds with the welfare reading: welfare reforms may function as harm-reduction, as a stepping-stone to abolition, or as an entrenchment mechanism, and this is an unresolved empirical question the story routes to omega rather than resolving by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_as_bridge_or_entrenchment,
    'Do incremental welfare reforms function empirically as a pathway toward eventual abolition of animal property status, or do they entrench and legitimize the property system by making it appear increasingly humane and therefore harder to dislodge?',
    'Long-run historical and comparative analysis of social movements that pursued incremental reform versus categorical abolition strategies (e.g., comparison with historical abolition movements against human chattel slavery, where gradualist and immediatist strategies produced contested legacies), combined with tracking of legal-status change following specific welfare reform campaigns.',
    'If reforms function as a bridge, the welfare reading and abolitionist reading are not purely adversarial but sequential/complementary, which would change how this story''s excluded welfare_reform_organizations stakeholder should be weighted. If reforms function as entrenchment, the abolitionist reading''s treatment of welfare organizations as, at best, insufficient allies is validated and the persistently high extractiveness measure is further supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_as_bridge_or_entrenchment, conceptual, 'Whether welfare reform advances or forestalls abolition of animal property status.').

omega_variable(
    animal_moral_personhood_ontological_status,
    'Is the claim that animals possess moral personhood sufficient to ground a basic right against property status an empirically/philosophically defensible position, or a contested normative extension beyond what animal cognition and moral-status science can currently establish?',
    'Continued interdisciplinary work in comparative cognition, philosophy of mind, and moral status theory; legal test cases (e.g., habeas corpus petitions on behalf of specific animals) that force courts to rule on personhood criteria.',
    'If moral personhood claims are broadly vindicated, the abolitionist reading''s extractiveness and victim-set claims gain independent corroboration beyond advocacy self-assertion. If personhood claims remain contested or are substantially narrowed, the reading''s categorical claims would need revision toward a more restricted victim-set (e.g., limited to animals meeting specific cognitive thresholds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_moral_personhood_ontological_status, conceptual, 'Whether the moral-personhood premise underlying the abolitionist reading is independently defensible.').

omega_variable(
    kernel_reading_framing_dependency,
    'Is the choice to treat ''property status itself'' as the unit of injury (rather than ''suffering under property status,'' the welfare reading''s unit) a framing that changes the classification independent of any empirical fact about animal welfare?',
    'None fully available — this is a genuine conceptual/normative fork in the kernel, not an empirical question resolvable by better data. The two readings could in principle produce identical predictions about which specific practices should be prohibited while disagreeing entirely about why.',
    'If the reading choice is purely normative/conceptual rather than empirical, this story''s classification (snare, high extractiveness) is a fact about the abolitionist framework''s own axioms, not a fact independently verifiable outside that framework — which is precisely why this story is authored as one reading among three rather than as a single adjudicated truth about animal ethics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_dependency, conceptual, 'Whether the abolitionist/welfare split is a framing choice rather than an empirical disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__abolitionist_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__abolitionist_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__abolitionist_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__abolitionist_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__abolitionist_reading, base_extractiveness, 8, 0.9).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__abolitionist_reading, base_extractiveness, 16, 0.89).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__abolitionist_reading, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__abolitionist_reading, base_extractiveness, 32, 0.88).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__abolitionist_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__abolitionist_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__abolitionist_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__abolitionist_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the animal_status_kernel. property_reading treats animals as ordinary property with no independent moral status and would classify the same underlying arrangement as, at most, a rope (efficient resource allocation) or unclassified natural economic relation. welfare_reading treats animals as sentient beings whose suffering matters but retains property status as legitimate if regulated, and would likely classify the arrangement as a tangled_rope (genuine coordination of resource use, but with asymmetric extraction requiring active welfare-enforcement to bound it). This abolitionist_reading classifies the identical set of underlying practices as a snare because it locates the injury in status rather than treatment, making no welfare regulation sufficient to convert the arrangement into legitimate coordination. All three stories share subject matter but diverge sharply in ε, victim-set, and claimed_type precisely because they instantiate different normative kernels — per the ε-invariance principle, this divergence is why they are three stories, not one story measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
