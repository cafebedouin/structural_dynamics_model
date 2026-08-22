% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Settlement: Sentience-Bounded Permissible Use
 *   domain: applied ethics/animal studies/legal philosophy
 *
 * SUMMARY:
 *   This story instantiates the welfare_reading of the contested kernel
 *   animal_moral_status: animals are sentient beings whose suffering should
 *   be minimized within systems of regulated use; cruelty is wrong, use is
 *   permissible. The referent of epsilon is the standing arrangement under
 *   contest — the actual system of statutorily floored, audited, and
 *   certified animal use — assessed by this reading's own lights, never by
 *   the endorsed ideal. Under those lights the arrangement has a genuine
 *   coordination function (an enforceable cruelty floor across dispersed
 *   users who would otherwise race to cheapest treatment) AND asymmetric
 *   extraction (the animals bear confinement, transport, and slaughter that
 *   certified standards permit; welfare organizations harvest legitimacy and
 *   donations from managing the problem; certified industries convert the
 *   label into public comfort and sustained demand). That dual structure is
 *   why the claimed type is tangled_rope. Per the epsilon-invariance
 *   principle, the colloquial label 'animal protection' decomposes into three
 *   structurally distinct constraints — this reading, the property_reading
 *   (animals as resources; the welfare overlay reads as interference with
 *   owner discretion), and the abolitionist_reading (all use is the
 *   violation; the welfare settlement reads as the extraction machine's
 *   public-relations arm). Each sibling is a separate file linked through
 *   network.affects_constraints; their epsilon values differ widely because
 *   their victim sets and beneficiary structures differ, not because epsilon
 *   is observer-relative within this story.
 *
 * KEY AGENTS:
 *   - sentient_farmed_animals: Principal target (powerless/trapped) — bears the use itself; receives the cruelty floor as a byproduct
 *   - research_animals_under_oversight: Principal target (powerless/trapped) — bears approved experimental procedures under protocol review
 *   - certified_animal_industries: Primary material beneficiary with real compliance costs (powerful/constrained) — converts the label into social license
 *   - animal_welfare_organizations: Legitimacy beneficiary (organized/identity_locked) — collects donations and relevance from managing the arrangement
 *   - ethically_motivated_consumers: Secondary beneficiary (moderate/mobile) — purchases moral reassurance; highest exit freedom in the settlement
 *   - welfare_regulators: Agenda setter (institutional/constrained) — draws the line between permitted suffering and punishable cruelty
 *   - abolitionist_advocates: Excluded voice (moderate/constrained) — rejects the cruelty/use line; locked out of certification governance and legislative negotiation
 *   - applied_ethicists: Analytical observer (analytical/analytical) — maps the argument structure without a seat in the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.5).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.38).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Settlement: Sentience-Bounded Permissible Use").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied ethics/animal studies/legal philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'a13b584c-441c-47ed-8e56-d24305ca88b2').
narrative_ontology:cs_kernel_codification('a13b584c-441c-47ed-8e56-d24305ca88b2', distributed).
narrative_ontology:cs_authority_grounding('a13b584c-441c-47ed-8e56-d24305ca88b2', expertise).
narrative_ontology:cs_interpretation_layer_present('a13b584c-441c-47ed-8e56-d24305ca88b2').
narrative_ontology:cs_reading_relation('a13b584c-441c-47ed-8e56-d24305ca88b2', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('a13b584c-441c-47ed-8e56-d24305ca88b2', animal_moral_status__abolitionist_reading, influences).
narrative_ontology:cs_axiom('a13b584c-441c-47ed-8e56-d24305ca88b2', foundational, cruelty_use_distinction).
narrative_ontology:cs_axiom_status(cruelty_use_distinction, holdable).
narrative_ontology:cs_axiom_grounding('a13b584c-441c-47ed-8e56-d24305ca88b2', cruelty_use_distinction, deontological).
narrative_ontology:cs_axiom('a13b584c-441c-47ed-8e56-d24305ca88b2', foundational, suffering_minimization_within_use_duty).
narrative_ontology:cs_axiom_status(suffering_minimization_within_use_duty, holdable).
narrative_ontology:cs_axiom_grounding('a13b584c-441c-47ed-8e56-d24305ca88b2', suffering_minimization_within_use_duty, deontological).
narrative_ontology:cs_axiom('a13b584c-441c-47ed-8e56-d24305ca88b2', secondary, certification_confers_market_legitimacy).
narrative_ontology:cs_axiom_status(certification_confers_market_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a13b584c-441c-47ed-8e56-d24305ca88b2', certification_confers_market_legitimacy, conventional).
narrative_ontology:cs_reference_frame('a13b584c-441c-47ed-8e56-d24305ca88b2', sentience_bounded_permissible_use).
narrative_ontology:cs_drift_state('a13b584c-441c-47ed-8e56-d24305ca88b2', contemporary_certification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a13b584c-441c-47ed-8e56-d24305ca88b2', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, certified_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, ethically_motivated_consumers).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, sentient_farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, research_animals_under_oversight).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, sentient_farmed_animals).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, research_animals_under_oversight).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, certified_animal_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, five_freedoms_framework).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, incremental_reform_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred into existence for food production. Protected against gratuitous cruelty by statute and audit, yet subjected to confinement densities, transport durations, and slaughter methods that certified standards expressly permit. Cannot consent, refuse, relocate, or advocate; their entire lives occur inside the arrangement, and the cruelty floor they receive arrives as a byproduct of a structure organized around their use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, sentient_farmed_animals, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, sentient_farmed_animals, beneficiary).

% Used in laboratories under protocol review boards that require justification, anesthesia, and minimization procedures. Protocols approve substantial suffering whenever framed as scientifically necessary. The animals bear experimental procedures they cannot decline; the oversight they receive bounds the suffering without ever presenting them a choice about undergoing it.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, research_animals_under_oversight, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, research_animals_under_oversight, beneficiary).

% Meat, dairy, egg, and research sectors operating under welfare codes and third-party certification. Compliance costs are real — housing retrofits, audits, record-keeping, slower throughput in places — but the certified label sustains consumer demand, deflects boycotts, and shields operations from the charge of cruelty. Some segments can relocate production to laxer jurisdictions; none can abandon certification without forfeiting welfare-sensitive markets.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, certified_animal_industries, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, certified_animal_industries, payer).

% Run certification schemes, draft model standards, lobby legislatures, and bring anti-cruelty prosecutions. Donations, membership, and institutional relevance flow from managing the suffering-of-used-animals problem. Mission statements bind them to improving use rather than ending it; an arrangement in which the question of use itself went live would dissolve their mandate, so their advocacy reliably stops at the standards boundary.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Purchase certified products to align consumption with their concern for animals, receiving moral reassurance that transfers the burden of vigilance to certifiers. They hold the highest exit freedom in the settlement: they can switch labels, buy uncertified, reduce consumption, or abstain entirely at low personal cost, and their purchasing signals are the demand-side input the certification market runs on.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, ethically_motivated_consumers, beneficiary,
    moderate, biographical, mobile, national).

% Government agencies that draft welfare codes, inspect facilities, and prosecute violations. They draw the operative line between permitted suffering and punishable cruelty — the single most consequential number in the arrangement. Budgets and statutory mandates tie them to administering the system; the question of use itself lies outside their delegated authority.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Scholars and campaign groups who hold that the welfare frame itself perpetuates the wrong — that 'humane' certification launders use rather than limiting it. They are locked out of certification governance and largely out of legislative negotiation, which is structured around standards-for-use. Their proposals fall outside the agenda the settlement defines, so their objection registers publicly but enters no decision forum.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% Map the positions, trace the argument structure, and identify where the settlement's premises hold together and where they do not. Hold no seat in the arrangement: they neither collect from it nor bear it, and their output feeds the disputing parties rather than the enforcement machinery.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, applied_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, certified_animal_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets an enforceable minimum-care floor across dispersed users of animals who would otherwise race toward the cheapest treatment, and manufactures the trust that lets welfare-labeled markets exist: buyers can rely on a label only because a shared, audited standard stands behind it.
% TRANSFER_FUNCTION: Moves compliance costs onto certified producers; moves moral reassurance to consumers; moves donations, legitimacy, and institutional relevance to the welfare organizations; and moves the residual burden of use — confinement, transport, slaughter — onto the animals themselves, who are the only party that transfers nothing and receives no invoice they could refuse.
% ABSENT_VOICES: The animals — the arrangement's principal cost-bearers — hold no seat anywhere in it; they object only through proxies whose own positions are beneficiary seats. Abolitionist advocates are also outside the room: certification governance and legislative negotiation are both structured around standards-for-use, so the question those voices would raise (use itself) is excluded by the agenda's form, not defeated by its answers.
% DISAPPEARANCE_RATIONALE: Use would not stop — it would continue without even the cruelty floor, with treatment set entirely by production economics. Certification markets, the welfare organizations' mandates, and the moral vocabulary of 'humane' would collapse together; industries would lose the social-license shield the label provides and face the abolitionist challenge undiluted; the entire dispute would reorganize around use-itself rather than methods-of-use.
% FOUNDING_PROBLEM: Gratuitous cruelty in animal use: baiting, neglect of working animals, and later the excesses of intensive production — suffering imposed without necessity or oversight, which the early anti-cruelty statutes (Martin's Act 1822, the RSPCA's founding 1824) and the subsequent regulatory apparatus were built to prevent.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: veterinary epidemiology and peer-reviewed animal-science literature document continuing welfare failures inside certified systems; undercover investigation footage published by independent media documents cruelty within audited facilities; the historical record of the anti-cruety movement's origins is attested by legal historians. The welfare organizations themselves attest the problem's liveness, but their testimony is discounted here precisely because they are beneficiaries — the independent veterinary and journalistic sources carry the corroboration.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.5 (low-to-moderate band): the arrangement genuinely reduces gratuitous cruelty relative to an unregulated baseline, but the certified floor permits substantial suffering, and the label launders it — from this reading's own lights the gap between promised minimization and delivered practice is the extraction. Suppression is 0.38 and is authored as a RAW structural property, unscaled by power or scope: the coercive machinery (statutes, inspections, prosecutions) is real but moderate, and its ideological component (the cruelty/use distinction forecloses questioning use) is carried separately in the omegas. Only extractiveness is scaled by the engine. Theater_ratio 0.48 reflects the certification era's heavy performative layer — audits and labels functioning substantially as marketing — alongside still-functional prosecution and standard-setting. Accessibility_collapse 0.35: alternatives remain genuinely available (uncertified purchase, reduced consumption, full abstention, abolitionist politics), so understanding the constraint does not collapse the option space the way a natural law would. Resistance 0.55: industries lobby against stricter floors, abolitionists attack the frame itself, and consumer price sensitivity repeatedly defeats reform proposals. The temporal series run on ONE shared grid (t = 0, 50, 100, 140, 170, 185, 200) with every tracked metric authored at every point. Suppression_requirement is authored as a rising series because enforcement-capacity build-up IS the traced dynamic here: inspection regimes, certification bodies, and welfare litigation matured steadily across the interval; a flat scalar would misrepresent that history. Base_extractiveness rises with post-war intensification and the certification boom (more total suffering under label cover); theater_ratio rises as Goodhart drift converts welfare metrics into marketing assets. The trajectory is monotonic, not cyclical — no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the regulator's seat the arrangement is a functioning legal regime doing what statutes say. From the industries' seat it is a manageable compliance cost that buys the social license worth far more than it costs. From the welfare organizations' seat it is the terrain of their entire institutional existence. From the animals' seat there is no perspective from which the arrangement is voluntary: every moment of their lives occurs inside it, and the cruelty floor they receive is a byproduct of a structure whose purpose is organizing their use. The classic coalition problem is at its maximum here: the principal victims cannot aggregate, litigate, or advocate — their interests enter the system only through human proxies, and the proxy structure (welfare organizations) is itself a beneficiary seat. That proxy mediation is the structural fact that keeps this a tangled_rope rather than a rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: welfare organizations, certified industries, and ethically motivated consumers sit near the beneficiary end (low d); the two animal groups sit near the target end (high d); regulators sit mid-range as administrators who neither fund the arrangement nor bear its bodily costs. Two overrides are declared because the automatic derivation would err in opposite directions. First, the animals carry a secondary beneficiary role (they DO receive the cruelty floor), which risks dragging their derived d well below their actual position as the arrangement's principal bearers; the override pins powerless agents at 0.93 — near-full target, retaining only the genuine subsidy of cruelty protection. Second, the industries' declared beneficiary role would derive a d near the pure-beneficiary end, ignoring that compliance costs are real and jurisdiction-shopping is only partially available; the override pins powerful agents at 0.3 — net beneficiary with non-trivial costs paid. Scope amplification is the engine's business: the arrangement operates at global scope, which the engine weighs when scaling effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents both symmetric mislabelings. Reading the arrangement as pure rope (its self-description) erases the extraction: the animals' unconsenting bearing of use, the organizations' mandate-harvest, the label's laundering function. Reading it as pure snare (the abolitionist description) erases the real coordination: cruelty prosecutions that succeed, standards that measurably reduced suffering for particular taxa and practices, and a genuine collective-action solution to the race-to-the-bottom in care costs. The mandatrophy question is answered through the R5 interview rather than a metric: the founding problem (gratuitous cruelty) is LIVE, so the arrangement has not outlived its function and no mandatrophy resolution is declared. The live risk is not obsolescence but drift — theater_ratio climbing toward majority-performative while the founding problem persists underneath, which is the signature of a coordination structure converting into its own justification. The measurements are designed to catch exactly that conversion if it completes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_within_kernel,
    'This constraint is one reading (welfare_reading) of the contested kernel animal_moral_status. How would instantiating a sibling reading change the structure?',
    'Compare compiled stories across the kernel: the property_reading removes animals from the victim set entirely (no independent standing, so the arrangement''s extraction collapses toward coordination cost alone and the beneficiary structure thins); the abolitionist_reading expands the victim set to all used animals regardless of conditions and converts the welfare organizations from beneficiaries into part of the extraction machinery. The disagreement is located at the cruelty/use line: whether sentience grounds only suffering-minimization claims within use, or claims against use itself.',
    'Classification is reading-indexed: the same standing arrangement computes as tangled_rope under this reading, as a near-pure coordination structure under the property reading, and as a snare-scale extraction under the abolitionist reading. Cross-reading comparison is valid only through the network edges, never by averaging epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_position_within_kernel, conceptual, 'Committer structure: this story instantiates one reading of a three-reading kernel; sibling readings instantiate different constraints with different victim sets and epsilon.').

omega_variable(
    humanewashing_rebound_effect,
    'Does welfare certification reduce net animal suffering, or does the moral license the label grants expand total use enough to offset per-animal gains?',
    'Natural experiments from jurisdictions with divergent certification penetration: compare per-capita consumption of animal products, total animals used, and measured welfare outcomes where labels gained or lost market share. Label-withdrawal events provide discontinuities.',
    'If labels expand total use, effective extraction rises above the authored value and the arrangement drifts snare-ward (the coordination story increasingly covers demand expansion); if substitution effects dominate, the rope-side reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanewashing_rebound_effect, empirical, 'Whether the certification layer nets out as suffering reduction or as demand-expanding moral license.').

omega_variable(
    threshold_calibration_basis,
    'Are welfare thresholds (stocking densities, transport durations, slaughter methods) calibrated to sentience science or to production economics?',
    'Track threshold-setting episodes: when animal-welfare science revises sentience or suffering findings, do standards move toward the science, or do they settle at the point compatible with prevailing production margins? Compare standards adopted in high-production-cost versus low-production-cost jurisdictions for the same species.',
    'If thresholds track economics, the cruelty/use distinction functions as a movable rationalization and the arrangement trends from tangled_rope toward snare; if thresholds track science, the coordination function is genuine and the extraction is bounded residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_basis, empirical, 'What the welfare floor actually optimizes: animal science or industry viability.').

omega_variable(
    welfare_org_mandate_capture,
    'Would the major animal welfare organizations support measures that would dissolve their own mandate (phase-outs of the uses they regulate), or does institutional identity lock their advocacy inside the manage-use frame?',
    'Examine organizational positions on concrete phase-out legislation and ballot measures versus standards legislation: donation-flow analysis, internal governance records, and revealed preference when forced to choose between a stricter standard and an end to a practice.',
    'If advocacy systematically stops at the mandate boundary, the beneficiary seat is identity-locked in a way that biases the whole settlement''s reform trajectory; the arrangement''s coordination function is then partly self-perpetuating rather than animal-directed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_org_mandate_capture, empirical, 'Tests whether the welfare organizations'' identity fusion with the manage-use mission distorts their advocacy.').

omega_variable(
    sentience_boundary_expansion,
    'Sentience findings keep extending the moral circle (fish, cephalopods, decapod crustaceans, insect nociception). Will the welfare frame''s standards extend fast enough to keep the promise that suffering within use is minimized?',
    'Compare the lag between published sentience evidence for a taxon and the adoption of any welfare standard covering that taxon, across taxa and jurisdictions.',
    'If the lag lengthens for newly included taxa, the gap between the frame''s promise and delivered practice widens mechanically, raising effective extraction over time independent of any change in intent; the drift_state''s practice_drift magnitude would escalate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_boundary_expansion, empirical, 'Whether the frame''s boundary tracks the science of sentience or lags it indefinitely.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t100, animal_moral_status__welfare_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(anim_tr_t100, observed).
narrative_ontology:measurement(anim_tr_t140, animal_moral_status__welfare_reading, theater_ratio, 140, 0.32).
narrative_ontology:measurement_basis(anim_tr_t140, observed).
narrative_ontology:measurement(anim_tr_t170, animal_moral_status__welfare_reading, theater_ratio, 170, 0.41).
narrative_ontology:measurement_basis(anim_tr_t170, observed).
narrative_ontology:measurement(anim_tr_t185, animal_moral_status__welfare_reading, theater_ratio, 185, 0.45).
narrative_ontology:measurement_basis(anim_tr_t185, observed).
narrative_ontology:measurement(anim_tr_t200, animal_moral_status__welfare_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement_basis(anim_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t100, animal_moral_status__welfare_reading, base_extractiveness, 100, 0.34).
narrative_ontology:measurement_basis(anim_be_t100, observed).
narrative_ontology:measurement(anim_be_t140, animal_moral_status__welfare_reading, base_extractiveness, 140, 0.42).
narrative_ontology:measurement_basis(anim_be_t140, observed).
narrative_ontology:measurement(anim_be_t170, animal_moral_status__welfare_reading, base_extractiveness, 170, 0.47).
narrative_ontology:measurement_basis(anim_be_t170, observed).
narrative_ontology:measurement(anim_be_t185, animal_moral_status__welfare_reading, base_extractiveness, 185, 0.49).
narrative_ontology:measurement_basis(anim_be_t185, observed).
narrative_ontology:measurement(anim_be_t200, animal_moral_status__welfare_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement_basis(anim_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__welfare_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t100, animal_moral_status__welfare_reading, suppression_requirement, 100, 0.24).
narrative_ontology:measurement_basis(anim_su_t100, observed).
narrative_ontology:measurement(anim_su_t140, animal_moral_status__welfare_reading, suppression_requirement, 140, 0.3).
narrative_ontology:measurement_basis(anim_su_t140, observed).
narrative_ontology:measurement(anim_su_t170, animal_moral_status__welfare_reading, suppression_requirement, 170, 0.35).
narrative_ontology:measurement_basis(anim_su_t170, observed).
narrative_ontology:measurement(anim_su_t185, animal_moral_status__welfare_reading, suppression_requirement, 185, 0.37).
narrative_ontology:measurement_basis(anim_su_t185, observed).
narrative_ontology:measurement(anim_su_t200, animal_moral_status__welfare_reading, suppression_requirement, 200, 0.38).
narrative_ontology:measurement_basis(anim_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'animal protection' decomposes into three stories per the epsilon-invariance principle. This welfare_reading carries moderate epsilon over the standing regulated-use arrangement, with animals victimized by methods-of-use but not by use as such. The property_reading (sibling file) authors near-zero extraction from the same arrangement viewed as owner-discretion management. The abolitionist_reading (sibling file) authors high extraction over the same arrangement viewed as use-as-violation, with the welfare organizations moved from the beneficiary column into the extraction machinery. Upstream/downstream: the welfare settlement is the dominant middle position and structurally pressures both siblings — it presupposes the property background it moderates, and it preempts the reform space the abolitionist reading needs. Edges here run from this story to both siblings; the siblings carry reciprocal edges in their own files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 0.93).
constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
