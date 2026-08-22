% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Abolitionist Reading: Animals as Rights-Holders
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the abolitionist reading of the contested
 *   kernel animal_status: animals are rights-holders with inherent value that
 *   precludes their use as instruments for human purposes (food, research,
 *   entertainment, clothing, labor). Under this reading, the standing
 *   arrangement—in which animals are legal property and their use is
 *   unrestricted except by welfare statutes—constitutes a systematic rights
 *   violation. The abolitionist reading does not negotiate the terms of
 *   animal use; it opposes the use itself. It does not appear in legislation
 *   or mainstream policy (in most jurisdictions); it appears in philosophy,
 *   advocacy, and emerging judicial challenges. The constraint story models
 *   it as a snare because: (1) animals are fully the victims of the
 *   arrangement; (2) extractiveness is technically zero (no one 'benefits'
 *   from animal suffering in the sense of receiving a transfer—humans benefit
 *   from animal products, but the abolitionist reading does not count that as
 *   legitimate gain); (3) suppression is extraordinarily high (institutional,
 *   legal, and cultural machinery actively prevents recognition of animals as
 *   rights-holders); (4) persistence depends entirely on enforced denial of
 *   animal moral status; (5) no exit exists for animals; (6) the theatrical
 *   component (welfare theater, humane labeling) functions to legitimize the
 *   snare rather than constrain it. The claim/metric gap is analytically
 *   critical: the abolitionist reading itself claims snare-type; the authored
 *   metrics reflect that claim. There is no divergence to measure here—the
 *   reading is internally coherent about what the arrangement is. Divergence
 *   appears at the sibling-reading level: the welfare reading measures the
 *   same facts and arrives at tangled_rope (coordination + extraction); the
 *   property reading measures the same facts and arrives at rope (pure
 *   coordination). The kernel contest is about which reading correctly
 *   describes what is actually happening.
 *
 * KEY AGENTS:
 *   - nonhuman_animals: primary victim; powerless, trapped, subjected to systematic instrumental use (d = 1.0, full target)
 *   - industrial_animal_agriculture: primary agenda_setter; institutional power; sets use practices and defends property status (d = 0.0, full beneficiary of the arrangement, though abolitionist rejects the legitimacy of that benefit)
 *   - animal_product_consumers: organized beneficiaries; constrained exit (can substitute alternatives but face cost, cultural friction); benefit from suppressed animal moral status enabling cheap products (d = 0.2-0.3, beneficiary position)
 *   - legal_and_regulatory_systems: agenda-setter; enforces property status, denies standing, prosecutes resistance; the active enforcement layer that maintains suppression
 *   - abolitionist_advocacy: excluded; argues for recognition of animal rights and prohibition of use; their exclusion from regulatory and commercial decision-making is structural
 *   - philosophical_observer: analytical seat; examines the logical structure of rights-assignment, sentience, and value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.82).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading: Animals as Rights-Holders").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '1e0df816-be40-474f-9eeb-9e50b045a6e9').
narrative_ontology:cs_kernel_codification('1e0df816-be40-474f-9eeb-9e50b045a6e9', formalized).
narrative_ontology:cs_authority_grounding('1e0df816-be40-474f-9eeb-9e50b045a6e9', extraction).
narrative_ontology:cs_interpretation_layer_present('1e0df816-be40-474f-9eeb-9e50b045a6e9').
narrative_ontology:cs_reading_relation('1e0df816-be40-474f-9eeb-9e50b045a6e9', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e0df816-be40-474f-9eeb-9e50b045a6e9', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('1e0df816-be40-474f-9eeb-9e50b045a6e9', foundational, animal_sentience_grounds_moral_status).
narrative_ontology:cs_axiom_status(animal_sentience_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('1e0df816-be40-474f-9eeb-9e50b045a6e9', animal_sentience_grounds_moral_status, empirically_contingent).
narrative_ontology:cs_axiom('1e0df816-be40-474f-9eeb-9e50b045a6e9', foundational, rights_require_autonomy_not_rationality).
narrative_ontology:cs_axiom_status(rights_require_autonomy_not_rationality, holdable).
narrative_ontology:cs_axiom_grounding('1e0df816-be40-474f-9eeb-9e50b045a6e9', rights_require_autonomy_not_rationality, deontological).
narrative_ontology:cs_reference_frame('1e0df816-be40-474f-9eeb-9e50b045a6e9', animal_rights_framework).
narrative_ontology:cs_drift_state('1e0df816-be40-474f-9eeb-9e50b045a6e9', contemporary, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1e0df816-be40-474f-9eeb-9e50b045a6e9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, nonhuman_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Systematically captured, confined, bred, used, and killed at human discretion across agriculture, research, and entertainment. Under the abolitionist reading, they hold rights to bodily autonomy and freedom from being treated as property or resources. They cannot exit: captive animals cannot fend for themselves; wild populations are trapped by habitat destruction; legally, they lack standing to refuse use or seek damages. Their structural situation is maximum victimization with zero agency.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Controls production methods, sets scale of use, defends the property-status regime through lobbying and regulation. Collects direct revenue (sale of animal products) and indirect benefit (political influence, tax subsidies, market integration). Maintains the constraint through legal systems, marketing (obscuring production methods), and institutional inertia.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, industrial_animal_agriculture, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive affordable animal products (meat, dairy, eggs, leather, wool). Their exit options are constrained by cultural normalization (animal products embedded in diet and identity), economic accessibility (alternatives often higher cost), and invisibility of production (distance from slaughter and confinement). Alternatives exist but require deliberate choice against convenience.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_product_consumers, beneficiary,
    organized, biographical, constrained, global).

% Uses animals in biomedical research, toxicology testing, and drug development. Maintains the constraint through institutional acceptance of animal-derived data as regulatory standard, institutional norms in research ethics boards, and cost preference (animal testing is cheaper than alternatives in initial development). Collects benefit through regulatory approval speed and research efficiency.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, research_and_pharmaceutical_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Actively enforces animal property status through legal rules: denying animals standing to sue, protecting ownership against animal-welfare claims, criminalizing theft of animals but treating harm to animals in owner's hands as secondary offense. The institutional machinery that maintains suppression by preventing legal recognition of animal rights.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_and_regulatory_systems, agenda_setter,
    institutional, generational, analytical, national).

% Argues that instrumental animal use should be abolished entirely; animals should have legal rights to bodily autonomy and freedom from property status. Excluded from regulatory decision-making, commercial markets, and institutional spaces where animal use is determined. Their presence in these spaces is constrained by lack of legal standing, inability to represent animal interests (they can speak FOR animals but not as animals), and political marginalization.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocacy, excluded,
    organized, generational, constrained, global).

% Argue for constraining animal use through welfare standards (cage-free, slow-growth, humane slaughter) without prohibiting use entirely. From the abolitionist reading, their acceptance of use-in-general means they concede the fundamental snare-structure while negotiating its severity. Partly included in regulatory conversations (welfare statutes exist); the abolitionist reading treats this partial inclusion as legitimation of the snare rather than constraint on it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, generational, constrained, global).

% Examines whether animals possess properties (sentience, interests about their own futures, capacity for suffering) that would confer moral status independent of human use; whether property status is grounded in discovered fact or institutional choice; what the persistence of animal use reveals about institutional capacity to extend rights recognition; how three readings of the animal_status kernel diverge in their assessment of what the arrangement is and what should change.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, philosophical_analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The abolitionist reading denies that any genuine coordination problem is solved by instrumental animal use. What appears as coordination (centralized food production, research infrastructure, standardized use) serves only to suppress recognition of animal moral status and enable human benefit from violation. The underlying 'problem' is humans' desire to use animals at low cost—not a coordination problem but a preference whose satisfaction requires denial of animal sentience.
% TRANSFER_FUNCTION: Transfers animals' bodies, labor, reproductive capacity, lifespan, and autonomy from animals' own self-determination to human ownership and instrumental disposition. Economically: animals' value (as producers of meat, milk, eggs, labor, research data) flows to agriculture, research, and consumer benefit. Morally: the transfer is a taking of the animal's fundamental interests (to live autonomously, free from suffering) without consent, compensation, or legal recourse. The animal receives nothing; the taking is one-directional extraction.
% ABSENT_VOICES: Nonhuman animals are systemically absent from regulatory conversations and cannot voice their own objection: they lack legal standing, cannot testify, cannot form advocacy organizations, cannot refuse participation. Abolitionist and welfare advocates attempt to speak on their behalf, but their presence in decision-making bodies (regulatory agencies, corporate boards, research ethics committees) is minimal. A full hearing would require animals to have independent legal standing and explicit rejection of their property status—which would amount to dissolution of the constraint itself.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if animals were recognized as rights-holders, property status removed, and instrumental use prohibited—the world would reorganize dramatically: animal agriculture would collapse or shift entirely to plant-based production (supply chains reorient within months); research institutions would be forced to adopt alternative testing methods (many exist but are economically disfavored); consumer diets would shift or stabilize around plant-based foods and synthetic materials; legal systems would develop standing and remedies for animal violations; supply chains, labor practices, and economies of scale built on animal use would require fundamental restructuring. The constraint's disappearance is not marginal adjustment but systemic reorganization.
% FOUNDING_PROBLEM: The founding problem, from the abolitionist reading's perspective, is NOT a legitimate problem requiring the constraint's persistence, but rather the historical fact that humans discovered they could capture and use animals. The constraint was built to institutionalize and defend that capacity. The 'problem' to which it is often said to respond—ensuring food security, enabling research, providing livelihood—is reframed by the abolitionist reading: these needs can be met through alternatives (plant agriculture, cell-based protein, alternative research methods, renewable labor). The founding problem as typically stated by benefiting institutions is a false problem: it treats the satisfaction of existing preferences (for cheap animal products) as a necessity requiring rights violation.
% FOUNDING_PROBLEM_CORROBORATION: Industrial agriculture and research attest that animal use solves legitimate human needs and remains necessary. The abolitionist reading rejects this: alternatives exist (plant-based protein, cellular agriculture, in vitro and computational research methods) and are expanding in viability. Corroboration from outside the benefiting parties: biologists and ethicists (Singer, Regan, Adams, Korsgaard) argue that animal sentience and interests establish their moral status independently of whether satisfying human preferences requires violating that status. Comparative economists document the technical feasibility of food production without animal agriculture. NO corroboration is available from animals themselves, which is precisely the abolitionist structural point: animals cannot ratify the arrangement or defend its legitimacy; their systematic exclusion from any voice in the matter reveals the snare.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).
:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as 0.0 because under the abolitionist reading, there is no legitimate transfer occurring—what appears as value transfer is actually rights violation. An animal's body, labor, and life are taken and used against its interests, without compensation or consent. There is no coherent sense in which the animal 'benefits' or 'receives' anything; nor can the human benefit be counted as legitimate gain from an abolitionist perspective (the gain is predicated on violating the animal's rights). Suppression is extraordinarily high (0.82) because: (1) the constraint requires active legal enforcement to deny animals standing; (2) institutional systems (agriculture, research, consumer culture) actively work to prevent recognition of animal sentience and moral status; (3) cognitive suppression is culturally embedded (invisibility of slaughter, separation of 'pet' from 'food animal,' normalization of use); (4) the constraint persists because animals cannot exit, cannot voice resistance through legal channels, and cannot form political coalitions. Theater ratio (0.41) is high because: welfare certification, humane labeling, slow-growth certification, and cage-free production are presented as moral constraint on use, but from the abolitionist reading they serve primarily to reduce consumer cognitive dissonance and resistance while leaving the fundamental snare intact. As welfare practices increase, consumer willingness to purchase animal products often increases (the moral permission effect), which can increase total animal suffering despite per-animal improvements. Accessibility collapse (0.78) is high because: once an animal is in the system (captured, confined, bred for human use), exit is literally impossible—the animal cannot leave, cannot be freed (has no wild skills, no habitat to return to), and can only exit through death at human determination. Structurally, alternatives to animal use exist (plant-based agriculture, cell-based protein, alternative research methods), but their adoption requires intentional choice against entrenched practice, making them psychologically and economically collapsed for most actors. Resistance (0.71) is moderately high because: abolitionist advocates organize and publicize; consumers face growing moral uncertainty; some jurisdictions have begun restricting specific practices (foie gras bans, cosmetic testing prohibitions); scientific community debate continues on alternatives. But resistance is constrained by the powerlessness of animals themselves, the economic stakes of animal agriculture, and the cultural normalization of use.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between seats is profound and structural. From the industrial_animal_agriculture seat: this is efficient coordination solving a human need (food, research capacity) with voluntary participants (consumers benefit, workers employed, innovation enabled). Constraints are appropriate (welfare statutes prevent unnecessary cruelty), but prohibition would be unjustified loss of valuable coordination and livelihood. From the nonhuman_animals seat: the arrangement is pure victimization; they have no ability to consent, no choice, no exit, and no ability to advocate for themselves. The fact that humans find their use beneficial does not grant them the right to impose it. From the consumer seat: the benefits are real (affordable protein, familiar products) but increasingly uncertain (is the suffering proportionate to the benefit? are alternatives viable?). From the abolitionist_advocacy seat: the arrangement is a systematic rights violation grounded in denial of moral status to beings who clearly possess sentience and interests. From the legal_and_regulatory systems seat: the constraint is legitimate because animals lack the cognitive capacities (rationality, autonomy, moral agency) that ground rights in liberal legal theory; property rules are appropriate. From the philosophical_observer seat: the divergence hinges on whether sentience alone grounds moral status, and whether existing property-rights law is grounded in discovered facts about animals or in institutional choices that could be otherwise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for animals (nonhuman_animals stakeholder) is maximally toward the target end (d = 1.0): they are the systematic objects of the constraint; all costs accrue to them (capture, confinement, use, death); they have zero exit options (trapped, identity-locked into captive situations once captured); they receive zero benefit from the arrangement. Directionality for industrial_animal_agriculture is maximally toward beneficiary end (d = 0.0): they set the rules, collect economic value, and face no suppression or cost from the arrangement itself (costs are externalized to animals; regulatory costs are incorporated into production and passed to consumers). However, the abolitionist reading does NOT recognize this as legitimate benefit because it derives from rights violation. This is where the abolitionist reading diverges from the welfare and property readings: welfare and property readings would measure the extraction χ through the beneficiary-beneficiary lens (agriculture profits = coordination gain); abolitionist reading rejects the premise. Directionality for consumers: moderately beneficiary (d = 0.2-0.3) because they benefit from cheap products but face increasing moral uncertainty and cognitive dissonance; exit options are constrained (alternatives exist but require deliberate choice). For legal_and_regulatory systems: they function as the agenda-setter (d = 0.0) enforcing the property regime; they bear no cost and collect institutional legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading reveals mandatrophy (mandate outlived function) in the legal property regime as applied to animals. The founding mandate was to enable human use of animals for survival, livelihood, and progress (food security, labor, medical advance). Contemporary conditions: (1) food security does not require animal agriculture (alternative proteins are nutritionally adequate and increasingly affordable); (2) animal research is increasingly replaceable by computational and in vitro methods; (3) animal labor is obsolete in modern economies. The mandate has outlived its justification on empirical grounds. Persistence of the constraint is now explained by: institutional inertia (property law is entrenched), economic interests (animal agriculture is capital-intensive and politically powerful), cultural normalization (diet and practice are embedded in identity), and cognitive suppression (the invisibility of slaughter enables preference not to know). The welfare reading rejects this mandatrophy finding: it argues that animal use solves the ongoing coordination problem of food production, medical progress, and consumer satisfaction—the mandate is live. The property reading similarly treats animal use as a standing legitimate prerogative of human ownership. The abolitionist reading insists the mandate is dead and its persistence is purely extractive institutional inertia. This disagreement is NOT settled by metrics or empirical facts; it is a substantive normative disagreement about whether satisfying current human preferences for animal products is a legitimate function that justifies the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_abolitionist_welfare_coexistence,
    'Do abolitionist and welfare readings represent genuinely incommensurable moral stances, or are they variations on a shared framework about animal sentience and moral standing, differing only on stringency?',
    'Philosophical analysis of whether an abolitionist and a welfare reformer could endorse a single framework in which (1) animal sentience is morally relevant, (2) suffering is morally bad, but (3) they disagree on whether use-with-reduced-suffering is acceptable or whether use is categorically prohibited. If yes, coexists_with. If the abolitionist must reject the welfare premise that use-with-welfare is permissible, they foreclose each other.',
    'Determines reading_relations: if coexistence, the relationship is coexists_with (both live, neither logically eliminates the other in any single framework, but held by different parties and institutions). If foreclosure, the relationship is forecloses (core premises contradict such that no single consistent framework could hold both). The determination affects whether the constraint family is a continuous moral landscape or a bifurcation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_abolitionist_welfare_coexistence, conceptual, 'Whether abolitionist and welfare readings coexist or foreclose each other.').

omega_variable(
    kernel_reading_abolitionist_property_incommensurability,
    'Do abolitionist and property readings represent incommensurable foundational premises about what grounds moral status (sentience vs. rationality/autonomy), or do they rest on shared empirical disagreements that could in principle be reconciled?',
    'Analysis of whether the disagreement is truly foundational (about what properties ground moral status) or empirical (about whether animals possess those properties). If a property reading that denies rationality to animals but accepts sentience-as-grounding could shift the classification, the disagreement is partly empirical. If property reading insists that sentience alone does not ground rights, the disagreement is foundational and likely incommensurable.',
    'If empirical reconciliation is possible (e.g., discoveries about animal cognition shift the property reading''s empirical claims but not its principles), the readings coexist but are not foreclosing. If foundational, they foreclose each other—they cannot coexist in a single coherent moral framework because they rest on incompatible claims about what properties ground moral status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_abolitionist_property_incommensurability, conceptual, 'Whether abolitionist and property readings are empirically or foundationally incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_status__abolitionist_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status__abolitionist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status__abolitionist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_status__abolitionist_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_status__abolitionist_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_status__abolitionist_reading, base_extractiveness, 25, 0.0).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.0).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_status__abolitionist_reading, suppression_requirement, 5, 0.77).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_status__abolitionist_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_status__abolitionist_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.0).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel animal_status. The abolitionist reading (this story) declares animals to be rights-holders with inherent value precluding instrumental use, measuring the standing arrangement as a snare (pure victimization through institutional suppression of moral status). The welfare reading measures the same arrangement as tangled_rope (genuine coordination for food production + extractive excess beyond justifiable coordination costs). The property reading measures the same arrangement as mountain or rope (natural law or coordinated convention of human dominion). The three readings diverge in ε (0.0 vs. >0 vs. ≈0), beneficiary structure, and type classification. Each reading is a separate constraint story, linked via this network edge. The contestation is not empirical disagreement about the facts of animal biology or production systems; it is disagreement about what counts as a legitimate basis for moral status and what frameworks properly apply to the standing arrangement. Decomposition follows ε-invariance: each reading assesses the same standing arrangement but arrives at a radically different ε because each reading endorses a radically different reference frame for what moral and legal facts are relevant. The abolitionist reading measures against animal sentience and inherent value; the welfare reading measures against both coordination and extraction; the property reading measures against discovered natural dominion or coordinated human authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
