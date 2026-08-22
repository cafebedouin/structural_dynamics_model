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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Animal Property Status as Categorical Violation (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the contested
 *   animal-moral-status kernel: the claim that animals are rights-bearing
 *   individuals for whom property status itself — not merely mistreatment
 *   within that status — constitutes the violation. Under this reading,
 *   welfare reforms (cage-free housing, 'humane' slaughter, enriched
 *   laboratory enclosures) do not mitigate the harm; they legitimate and
 *   extend it by making the underlying use-relation more publicly palatable
 *   and legally durable. The referent for extractiveness is the standing
 *   property-status arrangement as this reading assesses it — not the
 *   rights-respecting alternative the reading endorses, which would trivially
 *   score near zero. Two sibling readings of the same kernel —
 *   property_reading (animals as pure resources) and welfare_reading
 *   (regulated humane use) — are NOT represented in this file; they are
 *   separate constraints with their own ε values, linked here by network
 *   edges.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bears confinement, use, and death under property title
 *   - laboratory_animals: primary target (powerless/trapped) — bears procedures authorized only by property/research-subject status
 *   - companion_animals_under_ownership: target (powerless/trapped) — bears discretionary control despite affective relationship
 *   - wild_animals_under_regulatory_taking: target (powerless/trapped) — bears licensed taking under resource-management logic
 *   - animal_agriculture_industry: primary beneficiary/agenda_setter (institutional/arbitrage) — sets and profits from the property framework
 *   - biomedical_research_institutions: beneficiary/agenda_setter (institutional/constrained) — administers welfare-compliance procedures that legitimate continued use
 *   - welfare_reform_organizations: excluded from this reading's coalition (organized/constrained) — treated as complicit by accepting the property frame
 *   - abolitionist_advocates: excluded from mainstream policy (moderate/constrained) — hold the reading but lack legislative access
 *   - consuming_public: beneficiary (organized/mobile) — benefits from low-cost access without individually bearing systemic responsibility
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
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status as Categorical Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'f9710bbe-7c41-449e-a7c9-cf319123faaa').
narrative_ontology:cs_kernel_codification('f9710bbe-7c41-449e-a7c9-cf319123faaa', distributed).
narrative_ontology:cs_authority_grounding('f9710bbe-7c41-449e-a7c9-cf319123faaa', distributed).
narrative_ontology:cs_reading_relation('f9710bbe-7c41-449e-a7c9-cf319123faaa', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('f9710bbe-7c41-449e-a7c9-cf319123faaa', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('f9710bbe-7c41-449e-a7c9-cf319123faaa', foundational, property_status_is_the_violation).
narrative_ontology:cs_axiom_status(property_status_is_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('f9710bbe-7c41-449e-a7c9-cf319123faaa', property_status_is_the_violation, deontological).
narrative_ontology:cs_axiom('f9710bbe-7c41-449e-a7c9-cf319123faaa', foundational, humane_use_does_not_negate_victimization).
narrative_ontology:cs_axiom_status(humane_use_does_not_negate_victimization, holdable).
narrative_ontology:cs_axiom_grounding('f9710bbe-7c41-449e-a7c9-cf319123faaa', humane_use_does_not_negate_victimization, deontological).
narrative_ontology:cs_reference_frame('f9710bbe-7c41-449e-a7c9-cf319123faaa', animals_as_categorically_non_property_moral_patients).
narrative_ontology:cs_drift_state('f9710bbe-7c41-449e-a7c9-cf319123faaa', contemporary_legal_personhood_litigation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f9710bbe-7c41-449e-a7c9-cf319123faaa', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, companion_animals_under_ownership).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, wild_animals_under_regulatory_taking).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consuming_public).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, property_status_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, human_dominion_over_animals).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, use_value_of_sentient_beings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and killed within systems of production regardless of welfare conditions; under this reading their status as property is itself the harm, prior to and independent of how they are treated within that status. They have no legal standing to object and no mechanism of exit from the category that defines them as usable.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Used in research under welfare-regulated protocols (the '3Rs') that this reading holds only humanize the extraction rather than end it; their designation as research property authorizes procedures no human research subject could be legally subjected to.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, national).

% Legally classified as property even within relationships of apparent care; can be sold, bred, confined, or euthanized at the owner's discretion. Under this reading, affection does not dissolve the underlying property relation that makes such discretion legally available in the first place.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, companion_animals_under_ownership, payer,
    powerless, biographical, trapped, national).

% Subject to licensed hunting, trapping, and habitat-taking regimes that treat wildlife populations as a resource to be managed for human use (sport, food, commerce). No property title attaches to an individual wild animal, but the regulatory apparatus that authorizes their killing operates on the same use-logic this reading identifies as the violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, wild_animals_under_regulatory_taking, payer,
    powerless, biographical, trapped, national).

% Sets and lobbies for the legal and regulatory framework establishing animals as property, funds welfare-science research that legitimizes continued use, and profits directly from the property relation this reading identifies as the violation itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer IACUC review and welfare-compliance protocols that this reading treats as procedural legitimation of use rather than a check on it; institutions depend on animal-model research for funding, publication, and regulatory approval pathways for pharmaceuticals.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, biomedical_research_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, biomedical_research_institutions, beneficiary).

% Occupy a position this reading treats as structurally complicit: they campaign to improve conditions within the property relation (cage-free standards, enriched enclosures) rather than to end the relation, and are largely excluded from abolitionist coalitions as insufficiently radical even though they are the dominant voice in animal-protection policy.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_reform_organizations, excluded,
    organized, biographical, constrained, national).

% Argue for the immediate cessation of animal use as property, not its regulation; are marginal within animal-law policymaking relative to welfarist organizations, and are frequently characterized by industry and by welfare reformers alike as unrealistic or extreme, which keeps their framing outside the operative legal and legislative conversation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% Consumes animal products, uses animal-tested goods, and keeps companion animals under property arrangements without generally examining the property-status question; benefits from low-cost access to animal-derived goods and services that the property framework makes possible, and has full exit options individually (dietary and consumption choices) even though the systemic framework does not depend on any single consumer's choice.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consuming_public, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading at the level of the animal-use relation itself: what property-status frameworks solve is a human coordination problem (stable title, exchangeability, predictable liability, tractable regulation) achieved by treating animals as objects of ownership rather than by recognizing any interest animals themselves have in the arrangement.
% TRANSFER_FUNCTION: Moves the entire benefit stream — food, labor, research data, companionship, entertainment, and resource value — from animals (who bear confinement, use, and death) to humans (individual owners, corporations, and research institutions), mediated by a property title that forecloses the animal's own interests from legal consideration.
% ABSENT_VOICES: The animals themselves have no voice by design — the constraint's very structure (property status) is what forecloses legal standing; abolitionist advocates who would object to the property relation itself are marginalized within animal-law policy relative to welfare reformers, who accept the property frame and negotiate only its terms.
% DISAPPEARANCE_RATIONALE: If animal property status were abolished overnight under this reading, animal agriculture, biomedical animal research, and companion-animal ownership as currently practiced would become legally impossible in their current form; supply chains, research pipelines, veterinary and pet industries, and food systems would need to reorganize entirely around a non-ownership relation to animals — this is precisely why the abolitionist reading treats the arrangement as load-bearing rather than incidental.
% FOUNDING_PROBLEM: Historically, property status for animals was established to resolve questions of ownership, liability, and exchange value in agrarian and early industrial economies — who owns a beast of burden, who is liable when it causes harm, how it may be sold or inherited.
% FOUNDING_PROBLEM_CORROBORATION: Animal agriculture and biomedical research institutions attest the property framework remains necessary for functioning food and research systems. Legal scholars in critical animal law (writing from outside both the industry and the welfare-reform apparatus) corroborate that the property categorization is a historical legal artifact rather than a moral necessity, and note it has been revised for other classes of beings (historically, enslaved persons; currently, some jurisdictions' 'sentient being' statutory carve-outs) without the underlying use-relation actually ending — evidence, on this reading, that welfare reform does not resolve what abolition identifies as the actual violation.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.88) because, by this reading's own lights, every instance of use — however regulated — is extraction from a rights-bearing individual reduced to property; welfare improvements do not reduce ε on this reading, they only reduce visibility of the underlying relation (reflected in the rising theater_ratio, 0.18→0.42, tracking the growth of welfare-certification programs, 'humane' labeling, and compliance theater over the interval). Suppression is high (0.79) and rising because maintaining the property-status framework against a growing rights-recognition movement (legal personhood litigation, sentience statutes) requires increasingly active legal and institutional defense. Accessibility_collapse is authored moderate-low (0.35), not high — unlike a genuine mountain, alternatives to the property framework are not conceptually foreclosed (legal personhood for animals is an active, litigated position); what collapses is political and economic tractability, not conceivability. Resistance is authored high (0.72), reflecting the substantial and growing abolitionist, legal-personhood, and critical-animal-studies movements contesting the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary group is named among the affected animals — the abolitionist reading holds that abolition eliminates the use-relationship entirely, so there is no animal beneficiary class, only victims across every category of animal use (farmed, laboratory, companion, wild). Human beneficiaries (industry, research institutions, consuming public) sit near the full-beneficiary end of directionality: they collect the value of use while bearing none of its cost. All four animal-victim groups sit at the full-target end: trapped exit options, powerless power atom, and structural inability to exit the category that defines them as usable — the property status IS the trap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is deliberately live rather than resolved: founding_problem_status is 'contested' because industry and research institutions attest the coordination function (stable title, tractable liability, functioning food/research systems) remains genuinely needed, while critical-animal-law scholarship attests the original coordination problem has been decoupled from any necessity to categorize sentient beings as property specifically. This is not a case of confidently mislabeling coordination as extraction or vice versa — the story preserves the contest rather than adjudicating it, which is the correct posture for a kernel reading: this file asserts the abolitionist reading's internally consistent classification (snare) without claiming to have resolved the underlying moral dispute the kernel exists to hold open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingent_or_structural,
    'Is animal property status a contingent legal choice that could be revised without disrupting the underlying human-animal relationship, or is it structurally load-bearing for the entire economic and legal apparatus of animal use?',
    'Comparative legal-historical analysis of jurisdictions that have revised property status for other classes of beings (e.g., partial legal personhood grants for some great apes, rivers, or ecosystems) and tracked whether the underlying use-relationships persisted, were reformed, or ended.',
    'If contingent, the constraint is better modeled as a tangled_rope — property status functions as removable legal scaffolding around a use-relationship that could survive its removal in modified form. If structural, snare is the more accurate classification — the use-relationship cannot survive without the property categorization, making the categorization itself the extractive mechanism rather than incidental to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingent_or_structural, conceptual, 'Whether animal property status is a removable legal artifact or structurally necessary to the practices it enables.').

omega_variable(
    kernel_reading_selection_bias,
    'Does this abolitionist reading, welfare_reading, and property_reading share a single underlying kernel (the moral status of animals), or do they in fact describe three empirically and normatively distinct arrangements that only appear to be ''the same debate'' because they share vocabulary?',
    'Track whether legal and philosophical arguments actually cross reading boundaries — i.e., whether welfare_reading advocates ever concede the abolitionist''s property-status critique as valid-but-impractical (suggesting shared kernel, differing strategic conclusions) versus treating the critique as a category error (suggesting genuinely separate kernels).',
    'If the readings share a kernel, cs_structure.reading_relations (coexists_with, influences) accurately models ongoing pressure between them. If they are genuinely separate kernels wearing a shared label, this file''s kernel_id assignment should be revisited per the ε-invariance decomposition principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether the three readings genuinely share a kernel or only share vocabulary.').

omega_variable(
    welfare_reform_complicity_or_pathway,
    'Do welfare reforms function as this reading claims — legitimating and extending use by making it appear humane — or do they function as an incremental pathway toward the abolitionist end-state by building the moral and legal infrastructure (sentience recognition, standing litigation precedent) that abolition eventually depends on?',
    'Longitudinal tracking of jurisdictions with strong welfare regimes: does welfare regulation historically precede rights expansion (pathway hypothesis) or substitute for it indefinitely (legitimation hypothesis)?',
    'If welfare reform is a pathway, this reading''s characterization of welfare organizations as ''excluded''/complicit understates their structural contribution and the snare classification may overstate the constraint''s stability. If legitimation, the classification and the excluded role for welfare organizations are both well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_complicity_or_pathway, empirical, 'Whether welfare reform accelerates or forecloses the abolitionist trajectory this reading argues for.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__abolitionist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__abolitionist_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__abolitionist_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__abolitionist_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__abolitionist_reading, base_extractiveness, 8, 0.83).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__abolitionist_reading, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__abolitionist_reading, base_extractiveness, 24, 0.86).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__abolitionist_reading, base_extractiveness, 32, 0.87).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__abolitionist_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__abolitionist_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__abolitionist_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__abolitionist_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.15).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_moral_status kernel, each authored as a separate file per the ε-invariance decomposition principle. property_reading authors a low-ε, largely uncontested arrangement (animals as resources; no violation recognized). welfare_reading authors a moderate-ε tangled_rope (coordination function around minimizing suffering, genuine but incomplete, contested at the margins). This file (abolitionist_reading) authors a high-ε snare: the property relation itself, not treatment within it, is identified as the extractive mechanism, with no beneficiary among the animals themselves. The three files share a referent (the standing arrangement of animal use under property status) but diverge sharply on ε, victim/beneficiary structure, and classification because they diverge on the underlying moral premise — exactly the situation the framework's kernel/reading architecture is designed to hold apart rather than average over.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
