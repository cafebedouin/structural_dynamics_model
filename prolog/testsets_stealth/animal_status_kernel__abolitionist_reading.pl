% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Animal Property Status Arrangement — Abolitionist Reading
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The colloquial
 *   label 'animal status' covers three structurally distinct claims, authored
 *   as separate constraint files per the epsilon-invariance principle: this
 *   abolitionist reading, the welfare reading, and the property reading. The
 *   epsilon referent here is fixed to the standing arrangement under contest
 *   — animals held as legal property and used instrumentally across
 *   agriculture, research, entertainment, and companionship — assessed by
 *   this reading's own lights, in which every use violates a basic right and
 *   the property relation is itself the wrong. Epsilon is therefore authored
 *   high (0.95); it is NOT authored for the rights-respecting arrangement
 *   this reading would institute, which would flatten every advocacy
 *   reading's epsilon toward zero. The structural delta against the siblings:
 *   the victim-set is fully inclusive (all animals held as property, in every
 *   use context), the demanded remedy is abolition of property status rather
 *   than regulation of use, and the permissibility threshold is categorical
 *   rather than welfare-indexed. The strategic tension with the welfare
 *   reading — whether incremental reforms delay or advance abolition — is an
 *   empirical dispute and is routed to an omega variable, not averaged into
 *   the metrics. KEY AGENTS (by structural relationship): - farmed_animals:
 *   Primary target (powerless/trapped) — bears the largest share of the
 *   arrangement's costs - laboratory_animals, entertainment_animals: Targets
 *   (powerless/trapped) — institutional and commercial use contexts -
 *   companion_animals: Target with incidental benefit (powerless/trapped) —
 *   owned dependents - industrial_animal_agriculture: Primary beneficiary
 *   (institutional/arbitrage) — collects the dominant margin -
 *   biomedical_research_institutions: Secondary beneficiary
 *   (institutional/constrained) - animal_product_consumers: Diffuse
 *   beneficiary (moderate/mobile) - companion_animal_trade: Beneficiary
 *   (organized/arbitrage) - property_law_apparatus: Agenda setter
 *   (institutional/constrained) — administers and enforces the status
 *   assignment - animal_rights_movement: Resisting payer
 *   (organized/identity_locked) — bears the enforcement costs of opposition -
 *   animal_ethics_theorists: Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.9).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status Arrangement — Abolitionist Reading").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '534bd692-40e3-430a-b5c9-65021d518d8e').
narrative_ontology:cs_kernel_codification('534bd692-40e3-430a-b5c9-65021d518d8e', distributed).
narrative_ontology:cs_authority_grounding('534bd692-40e3-430a-b5c9-65021d518d8e', distributed).
narrative_ontology:cs_reading_relation('534bd692-40e3-430a-b5c9-65021d518d8e', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('534bd692-40e3-430a-b5c9-65021d518d8e', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_axiom('534bd692-40e3-430a-b5c9-65021d518d8e', foundational, animals_have_basic_right_not_to_be_property).
narrative_ontology:cs_axiom_status(animals_have_basic_right_not_to_be_property, holdable).
narrative_ontology:cs_axiom_grounding('534bd692-40e3-430a-b5c9-65021d518d8e', animals_have_basic_right_not_to_be_property, deontological).
narrative_ontology:cs_axiom('534bd692-40e3-430a-b5c9-65021d518d8e', foundational, all_animal_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(all_animal_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('534bd692-40e3-430a-b5c9-65021d518d8e', all_animal_use_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('534bd692-40e3-430a-b5c9-65021d518d8e', secondary, welfare_reform_cannot_legitimate_use).
narrative_ontology:cs_axiom_status(welfare_reform_cannot_legitimate_use, holdable).
narrative_ontology:cs_axiom_grounding('534bd692-40e3-430a-b5c9-65021d518d8e', welfare_reform_cannot_legitimate_use, deontological).
narrative_ontology:cs_reference_frame('534bd692-40e3-430a-b5c9-65021d518d8e', animal_moral_personhood).
narrative_ontology:cs_drift_state('534bd692-40e3-430a-b5c9-65021d518d8e', contemporary_legal_order, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('534bd692-40e3-430a-b5c9-65021d518d8e', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, companion_animal_trade).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, entertainment_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, companion_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, companion_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animal_rights_movement).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, chattel_property_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, human_dominion_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred into existence at rates set by market demand, confined through growth and production, transported, and killed on schedules set by their owners' economics. Every condition of their lives — space, diet, social contact, reproduction, lifespan — is decided by the humans who hold title to them. They have no legal standing; their interests reach any decision-maker only when a human chooses to spend resources voicing them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, farmed_animals, excluded).

% Are bred for experimental protocols, housed inside institutions that own them, and subjected to procedures reviewed and approved by committees seated within the using institutions themselves. Their numbers and the severity of what is done to them are set by research agendas and funding cycles they cannot influence.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, laboratory_animals, excluded).

% Perform, carry, or display for paying audiences under ownership contracts; training methods and living conditions follow commercial requirements. Retirement, sale, transfer, or killing rests entirely with the title-holder.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, entertainment_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, entertainment_animals, excluded).

% Live as owned dependents: fed, housed, and medically treated at the owner's discretion, and equally subject to abandonment, breeding for traits that harm them, and killing at the owner's request. Their wellbeing depends wholly on the goodwill and resources of the humans who hold title; they cannot leave a household on their own behalf.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, companion_animals, beneficiary).

% Converts roughly seventy billion land animals per year into meat, dairy, and eggs under property law. Title is what renders the animals inventory: breedable, confinable, transportable, and killable without legal event. The industry funds standard-practice exemptions and statutes restricting documentation of routine conditions, and it collects the margin that treating animals as inputs rather than right-holders makes possible.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture, beneficiary,
    institutional, generational, arbitrage, global).

% Hold animal colonies as institutional assets and run experimental programs designed around assumed animal availability. Oversight committees sit inside the using institutions; external challenge is limited to paperwork review. Transitions to alternative methods are slow because careers, buildings, and grant lines are built around animal models.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Buy the outputs — meals, leather, cosmetics and medicines tested on animals — at prices that reflect the absence of any price on the animals' own claims. Individual exit through plant-based purchasing is available and increasingly convenient, but aggregate demand is what sets the breeding quotas, and most consumers never encounter the production conditions their purchases fund.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Breeds and sells animals as commodities: mass breeding operations, the exotic pet trade, designer breeding for appearance traits. Profit depends on animals remaining purchasable, breedable, and returnable goods, and the trade organizes against restrictions on breeding and sales.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animal_trade, beneficiary,
    organized, biographical, arbitrage, global).

% Legislatures, courts, and enforcement agencies define animals as chattels, register and defend titles, prosecute theft and vandalism against owners' interests, and increasingly criminalize unauthorized documentation of standard industry practices. The apparatus administers the status assignment as settled doctrine; altering it would require overturning centuries of property precedent against concentrated economic opposition.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, property_law_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Campaigns for legal personhood, conducts open rescues and undercover investigations, litigates standing tests it usually loses, and absorbs the resulting costs: prosecutions under ag-gag and interference statutes, terrorism enhancements, SLAPP suits, and infiltration by industry investigators. Commitment is constitutive for most members — the movement's identity is bound to the animals' cause, and exit means abandoning them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_rights_movement, payer,
    organized, generational, identity_locked, global).

% Map the arguments: personhood criteria, the property-welfare-abolition dispute, standing doctrine. They author the competing readings (this story is written from a seat adjacent to theirs) and supply the vocabulary in which the contest is conducted, but command no enforcement power.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_ethics_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform regime for assigning, transferring, and defending exclusive human control over animal bodies and their productive capacity — resolving ownership disputes, enabling trade, and securing investment in enterprises built on animal use.
% TRANSFER_FUNCTION: Moves animals' bodies, labor, reproductive capacity, and lives from the animals themselves to human owners and markets; converts them into meat, dairy, eggs, data, labor, spectacle, and companionship sold for human benefit, with revenue flowing up the supply chain to producers, processors, and retailers.
% ABSENT_VOICES: The animals themselves: every decision-making seat in the arrangement is human, and the beings whose bodies and lives are allocated hold no standing and appear only as objects of the proceedings. Their interests enter the conversation solely when a human advocate spends resources to voice them, and documentation statutes increasingly bar even that channel.
% DISAPPEARANCE_RATIONALE: If property status for animals and the use it licenses vanished overnight, food systems would reorganize around plant agriculture, research programs would accelerate non-animal methods, trillions in livestock and inventory assets would be repriced or stranded, breeding of tens of billions of animals annually would cease, and property law would require amendment across every jurisdiction — the human economy rearranges around the loss; the animals simply stop being bred into captivity.
% FOUNDING_PROBLEM: Securing reliable human access to animal labor, food, and materials: property status gave humans exclusive, transferable, defensible control over animals as productive assets, solving the problem of organizing food security and draft power before industrial alternatives existed.
% FOUNDING_PROBLEM_CORROBORATION: Historical and archaeological scholarship on domestication corroborates the founding problem (food security, draft power, material supply) as the arrangement's origin, from outside the benefiting parties. Nutritional and agronomic research — also outside the beneficiary industries — attests that the underlying need no longer uniquely requires animal use, which is why the status is contested rather than live: the problem persists, but the parties dispute whether this arrangement is still what solves it. No corroboration can come from the animals, who cannot attest anything.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.95 with the referent fixed to the standing arrangement, assessed by this reading's lights: on the abolitionist account every use violates a basic right, so the arrangement takes the whole of the animals' liberty, labor, reproduction, and lives, with no remainder of legitimate burden. Suppression is 0.90 and overwhelmingly structural: confinement is physical, standing is legally barred, and since roughly 2010 a dedicated enforcement layer (ag-gag statutes, interference prosecutions, terrorism enhancements) has grown specifically to block documentation and rescue. Theater is 0.52 and rising across the interval: welfare certification, humane labeling, and responsible-use governance now constitute the majority of the arrangement's public justificatory activity while property status — the operative mechanism — goes untouched, the signature of proxy goals displacing function. Accessibility collapse is 0.72: within the legal system, rights-based alternatives collapse almost completely (no standing, dismissal of personhood petitions), while extra-legal individual exit through plant-based consumption keeps the figure below natural-law levels. Resistance is 0.58: a sustained, organized movement contests the arrangement through litigation, investigation, and rescue, and meets enforcement rather than indifference. The claimed type is stated independently of the metrics: from this reading's seat the arrangement is a snare — its coordination story (efficient allocation of animal resources) is the taking itself described in administrative language, persistence depends on continuous enforcement, and the victims are named. Metrics were authored descriptively; where the engine's per-seat computations diverge from the claim, that divergence is the datum. All three tracked series run on one shared six-point grid (interval units approximate years since 1975, the modern movement's onset).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the farmed_animals seat the arrangement presents as total: no exit exists at any price, and every institution encountered administers their use. From industrial_animal_agriculture's seat the same structure is ordinary asset management — depreciation schedules, feed conversion ratios, liability rules. From the property_law_apparatus's seat it is neutral administration of settled doctrine. From the animal_rights_movement's seat it is an ongoing emergency that justifies lawbreaking. Nothing in the authored claim adjudicates among these; the engine derives each seat's classification from role, power, and exit, and the spread across seats is the perspectival measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The victim declarations put the four animal groups at the full-target end: trapped exit holds them at maximum directionality regardless of their powerlessness, so effective extraction saturates for those seats. The beneficiary declarations put the using industries near the beneficiary end, with arbitrage-grade exit damping further; consumers sit nearby but slightly toward symmetric because their benefit is mediated by purchase choice they can individually decline. The property_law_apparatus is neither declared beneficiary nor victim: it administers without collecting, but its authority is invested in the arrangement's continuity, giving a mild beneficiary tilt rather than symmetry. The animal_rights_movement occupies an unusual position — organized power but target-side exposure, since it pays the arrangement's enforcement costs out of its own members' liberty and assets; its derived directionality should sit well above symmetric. The analytical seat is exempt from the computation. No directionality overrides are declared: the beneficiary/victim and exit declarations carry the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing food and draft labor through ownership of living tools — has been partially superseded: plant agriculture and emerging precision fermentation can meet the underlying need without the arrangement. But the arrangement is not drifting into inertial maintenance: it is actively defended by concentrated beneficiaries who fund the enforcement layer, so the classification resolves to snare rather than piton despite the growing theatrical share (0.52). The welfare overlay is where piton dynamics would first appear — certification regimes whose function has migrated toward legitimation — and the theater series documents that migration crossing the 0.5 line late in the interval. No mandatrophy resolution is declared: from the beneficiaries' side the mandate is live. The R5 mismatch check reads a contested founding-problem status against a world_rearranges disappearance verdict and finds neither the dead-mandate-zombie pattern nor resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the animal_status_kernel — the abolitionist reading. How would the sibling readings change the constraint''s structure?',
    'Compare the family: animal_status_kernel__property_reading and animal_status_kernel__welfare_reading instantiate the alternatives as separate constraints with their own epsilon, victim-sets, and per-seat classifications.',
    'The property reading empties the victim-set (moral considerability derives from the owner''s rights; epsilon collapses toward the coordination-cost floor by its own lights). The welfare reading admits a partial victim-set (suffering counts, property status retained; mid-range epsilon, tangled-rope-shaped). The disagreement is located in the status assignment itself: whether not-being-property is a basic right animals hold. This file authors the abolitionist answer only and does not average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story is the abolitionist reading of the animal status kernel, one of three sibling readings.').

omega_variable(
    reform_ablation_dispute,
    'Do incremental welfare reforms advance or delay abolition?',
    'Comparative movement outcomes: track abolition-relevant legal changes (personhood petitions, use bans, standing grants) in reform-heavy versus abolition-prioritizing campaigns over matched periods.',
    'If reforms delay abolition — entrenching property status by palliating use — the welfare sibling functions as the standing arrangement''s maintenance mechanism and this reading''s foreclosure of it sharpens. If reforms advance abolition, the readings are strategically complementary despite being logically incompatible within a single framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_ablation_dispute, empirical, 'Strategic tension between categorical abolition and incremental welfare reform; the empirical dispute the sibling readings ride on.').

omega_variable(
    dominion_naturalness_ambiguity,
    'Is human dominion over animals a natural fact (omnivorous predation, domestication as mutualism) or a constructed arrangement maintained by enforcement?',
    'Distinguish biological predation from institutionalized property status: test whether the arrangement''s specific features — breeding into existence at industrial scale, confinement systems, legal standing bars, documentation bans — persist when the enforcement machinery is withdrawn.',
    'If natural, the arrangement approaches natural-law immunity and abolition is a category error; if constructed, the snare classification stands and enforcement withdrawal suffices for decay. The abolitionist reading asserts constructed; the property reading asserts natural. This ambiguity is the deepest fault line in the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominion_naturalness_ambiguity, conceptual, 'Natural-law versus constructed ambiguity of the dominion arrangement.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression structural (confinement, standing bars, prosecution of rescuers and documentarians) or internalized (consumer normalization that persists after exposure to conditions)?',
    'Post-disclosure demand trajectories: if consumption persists after documented exposure to production conditions, the internalized share is substantial; if demand tracks enforcement intensity, the structural share dominates.',
    'If internalized, dismantling the enforcement machinery alone will not collapse the arrangement — demand-side identity change is load-bearing, and effective suppression exceeds what the structural measure records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the arrangement''s suppression.').

omega_variable(
    victim_set_boundary,
    'Where does the abolitionist victim-set boundary sit — all sentient animals, or only some taxa (vertebrates, decapods, cephalopods, insects)?',
    'Sentience and valence research across taxa, with the reading''s own personhood criterion applied consistently to marginal cases.',
    'Full inclusion (as authored here) maximizes victim breadth and aggregate extraction; a restricted boundary shrinks the victim-set and lowers aggregate figures without changing the per-victim structure or the categorical permissibility threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Within-reading uncertainty over the victim-set boundary; distinct from the cross-reading disagreement carried by kernel_reading_position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t0, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t10, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t20, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t30, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t40, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t0, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t10, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t20, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t30, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t40, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.95).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t0, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t10, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t20, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t30, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t40, observed).
narrative_ontology:measurement(animal_status_kernel_abolitionist_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement_basis(animal_status_kernel_abolitionist_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% 'Animal status' colloquially names one debate but decomposes into three structurally distinct constraints per the epsilon-invariance principle: this abolitionist reading (victim-set: all animals held as property; any use impermissible; epsilon authored high against the standing arrangement), the welfare reading (partial victim-set: suffering counts, property retained; mid-range epsilon; coordination-plus-extraction shape), and the property reading (empty victim-set; considerability derives from the owner's rights; epsilon near the coordination floor by its own lights). The property reading is upstream — codified doctrine cited as the settled baseline; the welfare reading is downstream of it; this reading contests both and exerts structural pressure on welfare regulation by campaigning against reforms it holds entrenching. Each file carries its own epsilon, beneficiaries, and victims; the family is linked so legitimacy and contamination flows are traceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
