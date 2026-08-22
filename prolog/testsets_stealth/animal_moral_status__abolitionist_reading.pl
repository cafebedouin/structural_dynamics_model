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
 *   human_readable: Animal Moral Status — Abolitionist Reading (Property Status as the Violation)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the
 *   animal_moral_status kernel and evaluates the standing arrangement under
 *   contest: the global legal-cultural regime that classifies nonhuman
 *   animals as property and organizes their bodies, reproduction, labor, and
 *   lives around human use. Per the epsilon-referent rule for kernel
 *   readings, extractiveness is authored for THAT standing arrangement as the
 *   abolitionist reading sees it — never for the rights-respecting
 *   arrangement the reading would put in place, which would drive epsilon
 *   toward zero for every advocacy reading and destroy cross-reading
 *   comparability. Assumption flagged explicitly: the expected structural
 *   delta directs beneficiary: none, and this story honors it —
 *   base_properties declares no beneficiaries, because the reading holds that
 *   the use-relationship produces no legitimate beneficiary whose interest
 *   could ground the arrangement; the seats that receive its proceeds are
 *   modeled instead through stakeholders, gain_flow, and directionality
 *   overrides. The claimed type (snare) and the metrics are independent
 *   authored facts: the claim states what this reading holds structurally
 *   true; the metrics describe the arrangement's operation as this reading
 *   assesses it. The colloquial label animal ethics decomposes into three
 *   structurally distinct claims — this file, plus the property and welfare
 *   sibling readings linked in network.affects_constraints.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bears near-total extraction of bodies, reproduction, and lifespan
 *   - laboratory_animals: target (powerless/trapped) — bred into protocols that presuppose use
 *   - entertainment_animals: target (powerless/trapped) — held as display and performance assets
 *   - companion_animals: dual-positioned target (powerless/trapped) — cared for through, and constituted by, the ownership relation
 *   - animal_use_industries: administrator and recipient (institutional/arbitrage) — operates the arrangement and collects its proceeds
 *   - human_consumers_of_animal_products: diffuse beneficiary seat (moderate/mobile) — the demand base, with cheap exit
 *   - welfare_certification_bodies: cover-story operators (organized/mobile) — monetize assurance within continued use
 *   - animal_rights_theorists: analytical observer — sees the full structure from outside its administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Moral Status — Abolitionist Reading (Property Status as the Violation)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '0da3a9ab-2f52-4f2f-a2a5-56057bb816b2').
narrative_ontology:cs_kernel_codification('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', formalized).
narrative_ontology:cs_authority_grounding('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', extraction).
narrative_ontology:cs_interpretation_layer_present('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2').
narrative_ontology:cs_reading_relation('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', foundational, sentience_grounds_inviolable_rights).
narrative_ontology:cs_axiom_status(sentience_grounds_inviolable_rights, holdable).
narrative_ontology:cs_axiom_grounding('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', sentience_grounds_inviolable_rights, deontological).
narrative_ontology:cs_axiom('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', foundational, property_status_itself_is_the_violation).
narrative_ontology:cs_axiom_status(property_status_itself_is_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', property_status_itself_is_the_violation, deontological).
narrative_ontology:cs_axiom('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', secondary, humane_exploitation_is_contradiction_in_terms).
narrative_ontology:cs_axiom_status(humane_exploitation_is_contradiction_in_terms, holdable).
narrative_ontology:cs_axiom_grounding('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', humane_exploitation_is_contradiction_in_terms, deontological).
narrative_ontology:cs_reference_frame('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', sentience_based_legal_personhood).
narrative_ontology:cs_drift_state('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', contemporary_property_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0da3a9ab-2f52-4f2f-a2a5-56057bb816b2', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, entertainment_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, companion_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, companion_animals).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, human_consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_certification_bodies).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, property_status_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, humane_use_necessity_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, confined, and processed at industrial scale — tens of billions of land animals annually plus vastly more aquatic animals. Their bodies, reproductive cycles, and lifespans are scheduled around production targets, and slaughter typically arrives at a small fraction of natural lifespan. They cannot testify, petition, contract, or appeal in any forum where their status is decided, and no exit exists from inside the arrangement: the arrangement determines their existence from birth.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, farmed_animals, excluded).

% Are purpose-bred for experimental protocols under legal frameworks that regulate procedure while presupposing use. Their interests enter protocol review only as weighted considerations against human research aims; they have no standing to refuse participation or exit the protocol pipeline.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Are kept for display, performance, and human interaction. Welfare standards govern conditions of captivity while leaving captivity itself unquestioned; individuals are transferred between facilities by sale or loan as balance-sheet assets.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, entertainment_animals, payer,
    powerless, immediate, trapped, global).

% Live inside households as owned property. They receive care, affection, and veterinary protection channeled through owner interest — anti-cruelty law protects them chiefly as another person's valued possession. Breeding, sale, surrender, and euthanasia decisions belong to owners and markets; they cannot exit the ownership relation, only be transferred within it.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, companion_animals, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, companion_animals, beneficiary).

% Operate the arrangement end to end: breeding, confinement, processing, and sale of animals and animal products. They fund and shape the welfare-standard apparatus, and lobby for legal shields including standard-practice exemptions from cruelty statutes and restrictions on undercover investigation. Capital is mobile across sectors and jurisdictions — these enterprises are not bound to animal use, only invested in it.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animal_use_industries, beneficiary).

% Buy the outputs of animal use as convention, habit, and preference. Substitutes for nearly every use are commercially available and increasingly inexpensive; exit is a purchasing decision, exercised by a minority. Their aggregated demand is the arrangement's revenue base.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, human_consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).

% Audit and certify compliance with humane-treatment standards, collecting fees per operation certified. Their revenue exists only so long as animal use continues; they market assurance to consumers and legibility to regulators. Retooling for a post-use economy is available but would dissolve their function.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_certification_bodies, beneficiary,
    organized, biographical, mobile, continental).

% Develop and contest the rights-based analysis of animal status from outside the arrangement's administration. They see the full structure — property doctrine, welfare regulation, certification markets, consumer demand — as one arrangement, and publish the case that the structure, not any treatment level within it, is the wrong.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_rights_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Property law supplies a settled framework for human dealings involving animals: recognized title, transfer, liability, veterinary and movement regulation, and commercial exchange proceed through ownership rather than ad hoc dispute. Stated without evaluation; the reading denies this framework could justify what it coordinates.
% TRANSFER_FUNCTION: Moves the bodies, reproductive capacity, labor, and lives of animals into human use — converted into food, experimental data, entertainment, labor, and companionship — with the resulting value flowing from the animal class to industries, certification bodies, and consumers.
% ABSENT_VOICES: The animals themselves. Every forum where their status is decided — legislatures, courts, standards bodies, certification schemes — is composed of human parties; the victim class cannot testify, petition, vote, or fund representation, and enters the conversation only as filtered through human spokespeople, most of whom accept the use-framework they purport to temper. Also absent: the generations of animals not yet bred, whose existence the arrangement schedules. Their structural silence is not an oversight of this story; it is the arrangement's load-bearing feature, recorded here commentary-grade.
% DISAPPEARANCE_RATIONALE: If property status and the use-relationship vanished overnight, the world rearranges massively: food systems rebuild around plant agriculture, biomedical research redesigns around non-animal methods, billions of dependent animals require care arrangements, and property law, veterinary regulation, and rural economies rewrite themselves. Few arrangements short of money or the state would rearrange more.
% FOUNDING_PROBLEM: Domestication and the property law built on it arose to secure food supply, draft power, materials, and later scientific knowledge under conditions where no substitutes existed; property classification gave settled rules for managing valuable, mobile, self-reproducing assets.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: nutritional-science consensus (position papers of major dietetics bodies) attests plant-based diets meet needs at all life stages, corroding the food-necessity leg; agricultural-economic analyses document substitution feasibility; historians of domestication attest the scarcity origins. Industry bodies and research institutions attest continued necessity from inside the beneficiary structure. No corroboration is possible from the affected class itself — animals cannot attest anything — which is itself structural signal, noted here rather than resolved.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.92, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.92: within the use-relationship the appropriation is near-total — bodies, reproductive capacity, labor, and lifespan itself are scheduled around production; the reading knows no residue of the animal's life that the arrangement does not claim. Suppression 0.88 (raw and unscaled — the engine scales only extractiveness): confinement, breeding into dependency, exclusion from legal standing, and investigation bans close every exit; no governed party can decline the arrangement. Theater_ratio 0.65: the welfare and certification apparatus is, from this seat, substantially performative — its output is assurance and license, its effect the stabilization of use — though marginal suffering reductions are conceded, so not 1.0. Accessibility_collapse 0.35: alternatives to nearly every use are commercially available and increasingly cheap, and the arrangement persists anyway — low collapse is the reading's central empirical point. Resistance 0.2: the victim classes cannot form coalitions (no communication channel adequate to coordination, no standing, no mobility), so resistance is vicarious, mounted by human advocates who hold different power atoms and often accept the use-frame themselves; the low value is honest, not an oversight of coalition potential. Measurement series share one grid (seven points, all three metrics at every point, all observed); trajectories are monotonic — post-war intensification, the welfare-legislation wave, the certification boom, and recent enforcement hardening — with end-state values equal to the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the farmed_animals seat the arrangement is a snare operated at full-target directionality: total extraction, zero exit. From the animal_use_industries seat the same structure reads as ordinary commerce under background property law — the arrangement is invisible as a constraint because it is the surrounding medium; their computed extraction sits near the beneficiary pole. The welfare_certification_bodies seat experiences functional professional activity — audits, standards, improvement — and would compute low personal extraction while administering the cover story. The human_consumers seat experiences mild convenience, with exit priced at a grocery decision. The engine computes these divergences from power, exit, and role data; this story authors the structure and declines to adjudicate which seat sees truly — though the reading's own answer is on record in the axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations (four animal classes) plus trapped exit derive directionality near 1.0 for every animal seat — full targets — and global spatial scope amplifies their effective extraction further, since verification of conditions at scale is hardest where they are concentrated. This reading declares no beneficiaries by design, so the derivation chain has no beneficiary data for the human-side seats and would fall back to mid-range defaults that misstate their positions; explicit overrides repair this: institutional (animal_use_industries) to 0.07 — they administer the arrangement and its proceeds accrue to them; organized (welfare_certification_bodies) to 0.14 — fee-collectors on the assurance apparatus; moderate (human_consumers_of_animal_products) to 0.28 — net beneficiaries whose mobile exit damps their exposure toward the beneficiary pole. Overrides are used here precisely because the structural data that would normally derive these values is deliberately absent under the reading's no-beneficiary declaration; the substitution is recorded openly. gain_flow names animal_use_industries as the seat the extraction demonstrably accrues to — receipt, not endorsement. fixing_cost is prohibitive: dismantling property status means rebuilding food systems, research practice, and property law at civilizational scale — the standard objection, to which the reading answers that moral necessity does not price out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing protein, draft labor, and materials under pre-substitution scarcity — is contested: the reading attests it dead (substitutes exist for virtually every use), industry and research attest it live. Classification guards against mislabeling in both directions. The welfare framing would render the arrangement a rope — regulation as pure coordination, extraction laundered as care; the snare claim, with its cover-story logic, blocks that laundering: however humane, the coordination surface (property mechanics, commerce, certification) functions as the delivery rail for the extraction. Conversely, if substitution completes and the arrangement persists on inertia and theatrical maintenance alone — property forms maintained with no functioning demand behind them — the theater_ratio trajectory is the early-warning indicator of decay toward piton. The R5 mismatch consumer reads status=contested against verdict=world_rearranges: no zombie flag fires today, but the contested status is the live seam.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the abolitionist_reading instantiation of the animal_moral_status kernel; what structural deltas would the sibling readings (property_reading, welfare_reading) introduce, and where exactly is the disagreement located?',
    'Comparative classification across the three reading-stories of the shared kernel; the disagreement locates at the moral-status boundary — whether sentience grounds inalienable claims (abolitionist), no independent standing exists (property), or protected interests within use (welfare).',
    'property_reading empties the victim set and drops epsilon toward the coordination floor; welfare_reading narrows victims to treatment violations, restores a beneficiary set (certification and industry), and moves classification toward rope or tangled_rope. Cross-reading comparison is the corpus''s measurement, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and the locus of disagreement.').

omega_variable(
    property_status_contingency,
    'Is animal property status a contingent legal construct maintained by choice and enforcement, or a structural feature of human-animal interdependence that any workable order must retain?',
    'Legal-historical analysis of jurisdictions that have revised status language (personhood petitions, constitutional sentience clauses) combined with transition-feasibility studies of use-substitution at scale.',
    'Contingent supports the snare claim — pure extraction sustained by enforceable choice; if structural, a genuine coordination component exists and the type moves toward tangled_rope with a nonzero beneficiary set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, conceptual, 'The contingency-versus-structure hinge the expected delta names as deciding snare versus tangled_rope.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (confinement, legal exclusion from standing, investigation bans) or internalized (human conviction that use is natural and necessary, consumer habituation)?',
    'Compare use volumes and compliance in periods and places where enforcement lapses: if use persists without active policing, internalized normalization carries the load.',
    'If substantially internalized, effective suppression exceeds the structural measure, survives statutory reform, and reform strategy must target belief and default rather than enforcement alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized split of the suppression scalar.').

omega_variable(
    welfare_apparatus_function,
    'Does the welfare-regulation and certification apparatus reduce net animal suffering, or does it primarily legitimate and stabilize use (theatrical maintenance)?',
    'Outcome comparison across jurisdictions with strong versus weak welfare regimes: suffering intensity per animal, total use volumes, and the trend of both.',
    'A primarily theatrical apparatus raises the honest theater_ratio above the authored 0.65 and hardens the snare verdict; substantively protective regulation returns partial coordination credit and softens toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_apparatus_function, empirical, 'Functional versus performative share of the welfare and certification layer.').

omega_variable(
    use_definition_boundary,
    'Where does use end for this reading — do companion-animal keeping, conservation breeding, or therapeutic assistance count as violative use, or does the violation require commercial or exploitative deployment?',
    'Doctrinal analysis within the abolitionist tradition itself, where the extension of use is actively contested between pluralist and stricter positions.',
    'A broader extension widens the victim set and raises epsilon; a narrower one confines victims to commercial-use classes and lowers both — the reading''s own boundary, not the kernel''s, is what varies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_definition_boundary, conceptual, 'Intra-reading contest over the extension of the violation-making predicate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_moral_status__abolitionist_reading, theater_ratio, 60, 0.65).
narrative_ontology:measurement_basis(anim_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.91).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_moral_status__abolitionist_reading, base_extractiveness, 60, 0.92).
narrative_ontology:measurement_basis(anim_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.86).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_moral_status__abolitionist_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement_basis(anim_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label animal ethics decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different victim sets and different epsilon. animal_moral_status__property_reading is the upstream enacted baseline (animals as resources; negligible recognized extraction by construction); animal_moral_status__welfare_reading mediates (protected interests within use; moderate extraction with a real beneficiary set); this file, the abolitionist reading, is the radical challenge (total victim set by virtue of status; highest epsilon of the family). Each story links the others through network.affects_constraints; the upstream property reading is routinely cited as settled background by the welfare apparatus, which is the contamination edge this family exists to measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, institutional, 0.07).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, organized, 0.14).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, moderate, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
