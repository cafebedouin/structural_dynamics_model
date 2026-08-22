% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Animal Property and Instrumental-Use Regime (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist_reading of the
 *   animal_moral_status kernel (see kernel_context). The constraint under
 *   classification is the standing arrangement: the legal-institutional
 *   regime that holds animals as property and organizes their use for food,
 *   fiber, research, and display. Per the kernel-reading epsilon rule, the
 *   referent of epsilon is this standing arrangement as the abolitionist
 *   reading assesses it — never the rights-respecting arrangement the reading
 *   would install. On the reading's own lights, property status itself is the
 *   violation: every use, however humane, appropriates a rights-bearing
 *   individual, so extraction approaches totality (0.92), the welfare layer
 *   functions chiefly as legitimation (theater 0.65), and the arrangement
 *   persists through coercion and the closure of every alternative available
 *   to the governed class. Assumptions stated: the interval maps to
 *   approximately 1965-2025 (the intensive-confinement era through the
 *   welfare-certification boom); the victim set is enumerated by its three
 *   largest classes with display, sport, and working animals subsumed under
 *   the same property relation; and base_properties.beneficiaries is
 *   deliberately left undeclared per the reading's null-beneficiary
 *   structural delta, with material receipts recorded instead through
 *   gain_flow and the stakeholder situations (see beneficiary_nullity_scope
 *   omega and directionality_logic). The claimed_type (snare) and the metrics
 *   are authored independently: the claim states what the reading holds
 *   structurally true; the metrics describe the arrangement's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bears the bulk of the extraction; roughly eighty billion land animals annually are the arrangement's throughput
 *   - laboratory_animals: target (powerless/trapped) — bred to order for statutory testing and research protocols
 *   - domesticated_companion_animals: target (powerless/trapped) — the affection paradox: loved as individuals, legally things
 *   - animal_agribusiness: agenda setter and primary recipient (institutional/arbitrage) — operates the chains, shapes the law, collects the revenue
 *   - biomedical_research_establishment: beneficiary (institutional/constrained) — careers and statutory frameworks organized around animal models
 *   - animal_product_consumers: beneficiary (moderate/mobile) — receive the outputs at prices that externalize the animals' lives; cheapest human exit in the system
 *   - welfare_regulatory_bodies: theater operator, dual agenda-setter/beneficiary (institutional/identity_locked) — the office exists only inside the use-paradigm it oversees
 *   - abolitionist_advocacy_movements: excluded challenger (organized/trapped) — outside the forums where property status is maintained; increasingly answered with ag-gag statutes
 *   - animal_law_scholars: analytical observer (analytical/analytical) — maps the doctrinal architecture without administering anything
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
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property and Instrumental-Use Regime (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '6dd37988-84fe-478c-b9e8-13ff812334fc').
narrative_ontology:cs_kernel_codification('6dd37988-84fe-478c-b9e8-13ff812334fc', distributed).
narrative_ontology:cs_authority_grounding('6dd37988-84fe-478c-b9e8-13ff812334fc', distributed).
narrative_ontology:cs_reading_relation('6dd37988-84fe-478c-b9e8-13ff812334fc', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('6dd37988-84fe-478c-b9e8-13ff812334fc', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('6dd37988-84fe-478c-b9e8-13ff812334fc', foundational, sentience_confers_individual_rights).
narrative_ontology:cs_axiom_status(sentience_confers_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('6dd37988-84fe-478c-b9e8-13ff812334fc', sentience_confers_individual_rights, deontological).
narrative_ontology:cs_axiom('6dd37988-84fe-478c-b9e8-13ff812334fc', foundational, all_instrumental_use_is_rights_violation).
narrative_ontology:cs_axiom_status(all_instrumental_use_is_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('6dd37988-84fe-478c-b9e8-13ff812334fc', all_instrumental_use_is_rights_violation, deontological).
narrative_ontology:cs_reference_frame('6dd37988-84fe-478c-b9e8-13ff812334fc', animals_as_rights_bearing_persons).
narrative_ontology:cs_drift_state('6dd37988-84fe-478c-b9e8-13ff812334fc', contemporary_property_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6dd37988-84fe-478c-b9e8-13ff812334fc', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, domesticated_companion_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_regulatory_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, confined, and processed on schedules set by production cycles; their milk, eggs, offspring, and flesh are the commodities sold. An individual's life runs weeks to a few years, spent indoors at stocking densities set by code. They hold no legal standing apart from their owner's claim, and nothing in the arrangement offers them a way out, because the arrangement determines their conception, housing, handling, and death.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Are bred to order and assigned to experimental protocols that determine their housing, procedures, and endpoints. Statutory testing frameworks require or permit their use for many product classes. Substitutes exist for some uses and are advancing, but adoption runs through multi-year validation; the animals themselves cannot decline, leave, or be released.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Live as owned property even inside affectionate homes: their residence, diet, reproduction (routine sterilization), movement, and continued existence turn on an owner's preferences and circumstances. Shelters euthanize surplus populations while breeders manufacture replacements. The attachment is real and the ownership is simultaneous.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, domesticated_companion_animals, payer,
    powerless, biographical, trapped, local).

% Operates the breeding, confinement, transport, processing, and marketing chains at global scale, and shapes the legal frame it operates under through lobbying, standard-setting seats, and litigation. Revenue concentrates here: the arrangement's money flows to these firms. Capital moves freely across species, commodities, and jurisdictions; no particular animal or region is indispensable to the enterprise.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_agribusiness, agenda_setter,
    institutional, generational, arbitrage, global).

% Runs on animal models written into statutory testing requirements and funding streams; careers, curricula, and publication channels are organized around them. Non-animal methods are growing but validation is slow, so leaving the model means retooling institutions, not just experiments.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, constrained, global).

% Buy the outputs at prices that reflect only the handling costs, not the animals' lives; the difference is carried by the animals. Switching to plant alternatives is materially cheap almost everywhere, but habit, cuisine, and social identity keep most purchases on autopilot.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, immediate, mobile, global).

% Draft and audit the welfare layer — space allowances, stunning rules, transport durations — and certify compliance. The office exists only inside the use-paradigm: if use ended, the mandate, the expertise, and the budgets end with it. Inspectors and standard-setters have built careers on the proposition that overseeing use is the humane path.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulatory_bodies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_regulatory_bodies, beneficiary).

% Campaign for legal personhood and the end of use; investigate facilities, litigate at the margins, and argue that welfare reform deepens the arrangement by making it palatable. They hold no seat in the legislatures and trade bodies where property status is maintained, and several jurisdictions answer their investigations with ag-gag statutes and terrorism-adjacent prosecutions.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocacy_movements, excluded,
    organized, generational, trapped, global).

% Map the doctrinal architecture — property, welfare, rights — across jurisdictions and traditions, tracing how each frame assigns standing, and publish analyses that travel between movements, courts, and academies without administering anything.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, animal_agribusiness).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For human institutions, the arrangement solves supply: it secures reliable, storable, tradable flows of animal bodies and products — protein, fiber, labor, experimental substrates, spectacle — through standardized breeding, confinement, transport, and processing chains, with property law assigning each animal to an owner and welfare law setting minimum handling terms inside that assignment.
% TRANSFER_FUNCTION: Moves the bodies, labor, reproductive output, and lives of animals to human firms and households; moves money from consumers and research funders to the industries that breed, process, and certify; and moves legitimacy from the welfare apparatus to the use-system it overlays.
% ABSENT_VOICES: The animals whose interests the arrangement allocates hold no seat in any legislature, standards body, or court that sets property status; they appear only as objects of regulation. Abolitionist advocates stand outside the agenda-setting forums, and the generations of domesticated animals the system will breed into existence are represented by no one at all.
% DISAPPEARANCE_RATIONALE: Global food and research systems are built on the arrangement: cropping patterns, cold chains, medical testing pipelines, rural economies, and trade flows would all reorganize, and billions of living domesticated animals — bred into dependency — would need care arrangements that do not currently exist. Nothing about the rearrangement would be smooth, which is precisely the reading's grievance: the world has arranged itself around the violation.
% FOUNDING_PROBLEM: Securing animal-derived food, draft labor, fiber, and later experimental material for human societies in eras before plant-based and synthetic substitutes existed; property status was the legal technology that made animals reliably available as inputs.
% FOUNDING_PROBLEM_CORROBORATION: Industry bodies attest the founding problem is live (food security, research necessity), but they are the benefiting parties. Corroboration from outside the beneficiary set: independent nutrition-science bodies affirm plant-based dietary adequacy across life stages; agricultural-substitution economics documents feasible replacements for most uses; and historical scholarship documents the contingency of animal-property regimes across societies. No fully neutral arbiter exists, and the abolitionist reading itself disputes the sufficiency of that corroboration — the contest is real, which is why the status is authored contested rather than dead.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Epsilon 0.92: the arrangement's product IS the governed subject — bodies, milk, eggs, offspring, and finally life itself are appropriated, which is as close to total extraction as a material arrangement reaches; the reading denies that any welfare discount redeems it. Suppression 0.88: for the governed class, alternatives are nil — the arrangement determines conception, housing, handling, and death, and property law forecloses legal exit; the scalar is a raw structural property and is deliberately NOT scaled by power or scope (scope amplification happens engine-side). Theater 0.65: the welfare-certification layer expanded dramatically over the interval (audits, labels, welfare-washing), and the reading holds that its dominant function is keeping use palatable, with a minority share of real suffering relief. Accessibility_collapse 0.87: once property status is in force, no alternative exists for the governed; the residual reflects human-side substitutes the governed cannot reach. Resistance 0.3: advocacy movements grow and investigate, but the victim class cannot organize — no shared communicative medium at scale, no legal standing for aggregation, and individuals isolated as the unit of extraction — so resistance is proxy-only and marginal relative to the arrangement's scale (coalition check addressed; see cross_species_coalition_possibility omega). Measurements run on ONE shared time grid (t = 0,10,20,30,40,50,60) with every tracked metric authored at every point; suppression_requirement is authored because the story specifically traces enforcement-capacity hardening (ag-gag statutes, terrorism-adjacent prosecutions of investigators, securitized facility biosecurity) rather than mere extraction drift. The trajectories are monotonic ratchets, not cycles — no intermittent-reinforcement oscillation is present, so no cyclical battery applies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the animal seats the arrangement is total captivity ending in scheduled death — no partial reading is available from inside it. From animal_agribusiness the same structure is agriculture: a load-bearing civilizational system feeding billions, defended as heritage and food security. From welfare_regulatory_bodies it is humane oversight — and that seat is identity_locked in a specific sense: the office has become its function, so the possibility that oversight perpetuates what it moderates is not just rejected but unthinkable from inside the professional identity; if that identity frame broke, the seat's classification would change with it. Consumers experience the arrangement as ordinary market life, its costs invisible by design. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three victim classes derive high directionality (near full-target) from their declarations plus powerless power and trapped exit — the derivation chain handles them without overrides. The reading's null-beneficiary delta leaves base_properties.beneficiaries undeclared, which removes the beneficiary anchors the derivation would otherwise use for the collecting seats; directionality_overrides therefore pin those seats near the beneficiary end: institutional seats (agribusiness, research, regulators) at d=0.10 and consumers at d=0.25. Override granularity is per power atom, so the three institutional seats share one value; the residual differentiation (agribusiness is the deepest collector — it holds gain_flow — research is constrained by validation pipelines, regulators are identity_locked) is recorded qualitatively here and in the situations. Consumers sit nearest the human-arbitrage end: materially cheap exit, blunted by internalized normalization (see suppression_mechanism_split). Abolitionist advocates are pinned at d=0.5: excluded critics, neither collecting nor bearing the arrangement's direct costs, though ag-gag enforcement pushes them mildly toward the target side. The scholar seat is analytical and outside the flow. Global spatial scope on the major seats amplifies effective extraction engine-side (verification is hardest at planetary scale), which is descriptively apt: the arrangement's worst conditions are furthest from oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against laundering in both directions. Against rope-laundering: the welfare frame presents the arrangement as coordination with kindness — feeding people, advancing medicine — and the snare claim blocks that move, because a genuine coordination function does not require suppressing every alternative for the governed class nor criminalizing those who document conditions; the coordination story is cover, and the theater_ratio series shows the cover growing faster than the function. Against mountain-laundering: the arrangement presents itself as inevitable human ecology, but property status is statute, not physics — emerges_naturally is false, and the contingent_vs_structural omega carries the residual question. On the genealogy: the founding problem (securing animal-derived food, labor, and materials in scarcity eras) is largely obsolete given plant and synthetic substitutes, but status is authored contested rather than dead because the benefiting parties actively attest it live and the corroboration from outside is strong but not unanimous. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) does not trip the automatic zombie flag, but the theater trajectory keeps the piton-refinement path open if enforcement ever decoupled from function faster than it decouples now.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — abolitionist_reading — of the animal_moral_status kernel; what would the sibling readings (property_reading, welfare_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three reading-stories of the kernel: each instantiates the same standing arrangement under a different status premise; differences in victim sets, epsilon, and computed types localize the disagreement.',
    'property_reading empties the victim set (no independent standing means no victims, and epsilon collapses toward zero from its seat); welfare_reading shrinks the victim set to cruelty cases and certifies use as coordination. The disagreement is located in the status premise — whether sentience grounds rights or only welfare weighting — which moves the victim-set boundary without changing the arrangement itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is the abolitionist reading of the animal_moral_status kernel; sibling readings alter the victim-set boundary, not the referent.').

omega_variable(
    contingent_vs_structural_property_status,
    'Is animal property status a contingent legal construct that can be dismantled, or a structural feature of human ecology that no institutional change reaches?',
    'Historical analysis of legal-status reversals (slavery, married women''s property), substitution economics for animal-derived food and research inputs, and comparative jurisdictional variation in animal-law frameworks.',
    'Contingent status confirms the snare reading (constructed extraction, dismantle-able, defended by identifiable actors); structural status would push the arrangement toward mountain-like inevitability and render the abolitionist project inert. The claimed_type fork in the source delta resolves through this omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_vs_structural_property_status, conceptual, 'Whether the property paradigm is statute or substrate — the contingency question that separates snare from mountain-like readings.').

omega_variable(
    beneficiary_nullity_scope,
    'Does the reading''s null-beneficiary declaration describe the standing arrangement''s material flows (that nobody gains — materially false) or the reading''s refusal to certify any gain as legitimate benefit?',
    'Compare the receipt surface (gain_flow names animal_agribusiness as the seat the gains demonstrably accrue to) against the reading''s evaluative frame; observe how sibling readings declare beneficiaries outright.',
    'If beneficiaries were declared in base_properties, the derivation chain would open low-d seats and the machinery would see a standard winner/loser structure; the null declaration encodes the reading''s verdict that the use-relationship admits no legitimate beneficiary side, because abolition eliminates the relationship rather than redistributing it. Classification consequences ride on which layer (receipt vs. role) the engine weights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_nullity_scope, conceptual, 'Scope of the null-beneficiary stance: structural fact versus reading-indexed refusal.').

omega_variable(
    welfare_theater_vs_relief_share,
    'What share of welfare regulation reduces actual suffering versus legitimating continued use?',
    'Outcome studies measuring suffering reduction from specific welfare interventions, crossed with consumption-elasticity data: whether welfare labels raise total animal throughput enough to offset per-animal gains.',
    'A lower theater share would push the arrangement toward tangled_rope structure (real relief riding on real coordination); a higher share confirms the snare reading in which the welfare layer is the cover story. The authored 0.65 assumes majority-legitimation with minority-real relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_theater_vs_relief_share, empirical, 'Functional-versus-performative split inside the welfare layer.').

omega_variable(
    cross_species_coalition_possibility,
    'Can the powerless victim class ever acquire coalition power, or is cross-species aggregation structurally impossible?',
    'Study of representation mechanisms — legal guardianship, proxy advocacy, trustee models — and their track record in aggregating the interests of parties who cannot speak or organize.',
    'If proxy coalitions can aggregate animal interests, resistance rises above the authored 0.3 and the arrangement destabilizes toward contested enforcement; if aggregation is impossible, suppression stays total regardless of advocacy growth and the snare reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_species_coalition_possibility, empirical, 'Whether the coalition-power route is open to a victim class that cannot organize.').

omega_variable(
    domesticated_existence_tradeoff,
    'For species bred into dependent existence, does abolition mean liberation, or the planned end of domesticated kinds — and do currently living animals gain or lose?',
    'Normative analysis within the reading''s own tradition (phase-out versus sanctuary pathways) combined with welfare outcomes under each pathway.',
    'Changes what the endorsed alternative owes to current victims, and therefore what the reading''s disappearance_verdict implies; does not change the classification of the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domesticated_existence_tradeoff, preference, 'Value question inside the reading: existence of domesticated kinds versus cessation of breeding.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (confinement, property law, physical control of animals) or internalized (human-side normalization — carnism — that blunts otherwise-mobile exit)?',
    'Post-contact trajectory studies: whether consumers who encounter abolitionist arguments change behavior (internalized component weak) or rationalize continuity (internalized component strong), alongside the invariant physical facts of animal captivity.',
    'For the governed animal class the suppression is structural and near-total; the internalized component operates on human exit options, holding nominally mobile consumers in place. If internalization is the dominant human-side mechanism, consumer exit_options are effectively worse than the authored mobile rating suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized shares of the suppression picture across the two governed populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ams_abolitionist_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ams_abolitionist_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(ams_abolitionist_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ams_abolitionist_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(ams_abolitionist_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(ams_abolitionist_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.59).
narrative_ontology:measurement(ams_abolitionist_tr_t60, animal_moral_status__abolitionist_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(ams_abolitionist_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement(ams_abolitionist_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(ams_abolitionist_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(ams_abolitionist_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(ams_abolitionist_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(ams_abolitionist_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(ams_abolitionist_be_t60, animal_moral_status__abolitionist_reading, base_extractiveness, 60, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(ams_abolitionist_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ams_abolitionist_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(ams_abolitionist_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(ams_abolitionist_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(ams_abolitionist_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(ams_abolitionist_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.83).
narrative_ontology:measurement(ams_abolitionist_su_t60, animal_moral_status__abolitionist_reading, suppression_requirement, 60, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'animal ethics' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of the animal_moral_status kernel: property_reading (incumbent legal baseline; animals outside the victim set; epsilon near zero from its own seat), welfare_reading (partial victim set — cruelty cases; use certified as coordination), and this abolitionist_reading (total victim set; use per se as violation; epsilon maximal). The upstream story is property_reading: the established codification that the other two react against and that gives the family its contest structure. Each file links the other two via affects_constraints. Epsilon differs across the family because each reading assesses the SAME standing arrangement under a different status premise — the referent is fixed, the lights differ. Reciprocal links and this note belong in both sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, institutional, 0.1).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, moderate, 0.25).
constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
