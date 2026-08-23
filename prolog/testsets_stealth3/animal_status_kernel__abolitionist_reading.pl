% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status — Abolitionist Reading (Persons, Not Property)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This file instantiates the abolitionist_reading of the
 *   animal_status_kernel: one of three competing readings of a single
 *   persisting commitment — what animals ARE, such that human arrangements
 *   concerning them are legitimate or not. The kernel is stabilized in law
 *   (every jurisdiction classifies animals as owned assets), in practice
 *   (husbandry, research, companionship), and in moral vocabulary; the three
 *   readings instantiate three DIFFERENT constraints from that one kernel.
 *   This story generates only the abolitionist reading, cleanly: its
 *   constraint is the standing arrangement under contest — the
 *   legal-and-social constitution of animals as owned, usable beings —
 *   assessed by this reading's own lights. Per the fixed epsilon-referent
 *   rule, epsilon here is authored for the standing property/use arrangement
 *   as the abolitionist sees it (near-total appropriation of animals' bodies,
 *   liberty, reproduction, and lives, with welfare regulation functioning as
 *   legitimating maintenance), never for the rights-respecting arrangement
 *   this reading would put in its place. Sibling readings — property_reading
 *   (animals as property, economic value exhaustive) and welfare_reading
 *   (sentient beings, use acceptable if pain-minimized) — are separate
 *   constraint files with their own epsilon, victim sets, and
 *   classifications; they are linked, not averaged. Claim and metrics are
 *   independent authored facts: claimed_type is stated from this reading's
 *   structural seat (snare — the coordination story is cover, persistence
 *   rides on coercion and exit-suppression, victims identifiable), while the
 *   metrics describe the arrangement's actual operation as this reading
 *   measures it; where the engine computes a different type for a given seat,
 *   that divergence is the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — tens of billions absorbed into confinement-production cycles; bear the arrangement's full cost
 *   - laboratory_animals: target (powerless/trapped) — bred to protocol, spent and killed inside facilities
 *   - companion_animals: target (powerless/trapped) — bred into dependency; owned, sterilized, surrendered, or killed at keeper discretion
 *   - industrial_animal_agriculture: primary beneficiary and agenda-setter (institutional/arbitrage) — sets genetics, housing, and slaughter standards; collects the largest share of the flow
 *   - animal_product_consumers: beneficiary (moderate/constrained) — receive products as the system default; alternatives exist but are frictioned
 *   - animal_welfare_regulators: enforcement administrator (institutional/mobile) — run inspection and review regimes inside the owning frame
 *   - large_welfare_organizations: beneficiary-administrator (institutional/identity_locked) — monetize and administer the reform layer; organizational identity fused with incrementalism
 *   - smallholder_livestock_farmers: dual-positioned (moderate/constrained) — benefit from the category while being squeezed by its industrial tier
 *   - abolitionist_advocates: excluded challenger (moderate/mobile) — argue the category itself is the defect; largely outside legislative negotiation
 *   - analytical_observer: analytical seat — evaluates genealogy, sentience evidence, and cross-jurisdiction trajectories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status — Abolitionist Reading (Persons, Not Property)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '26699ea1-6653-4f66-8dcf-9beda309772c').
narrative_ontology:cs_kernel_codification('26699ea1-6653-4f66-8dcf-9beda309772c', formalized).
narrative_ontology:cs_authority_grounding('26699ea1-6653-4f66-8dcf-9beda309772c', lineage).
narrative_ontology:cs_interpretation_layer_present('26699ea1-6653-4f66-8dcf-9beda309772c').
narrative_ontology:cs_reading_relation('26699ea1-6653-4f66-8dcf-9beda309772c', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('26699ea1-6653-4f66-8dcf-9beda309772c', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_axiom('26699ea1-6653-4f66-8dcf-9beda309772c', foundational, animals_hold_basic_right_not_to_be_property).
narrative_ontology:cs_axiom_status(animals_hold_basic_right_not_to_be_property, holdable).
narrative_ontology:cs_axiom_grounding('26699ea1-6653-4f66-8dcf-9beda309772c', animals_hold_basic_right_not_to_be_property, deontological).
narrative_ontology:cs_axiom('26699ea1-6653-4f66-8dcf-9beda309772c', foundational, all_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(all_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('26699ea1-6653-4f66-8dcf-9beda309772c', all_use_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('26699ea1-6653-4f66-8dcf-9beda309772c', secondary, welfare_reform_entrenches_property_status).
narrative_ontology:cs_axiom_status(welfare_reform_entrenches_property_status, holdable).
narrative_ontology:cs_axiom_grounding('26699ea1-6653-4f66-8dcf-9beda309772c', welfare_reform_entrenches_property_status, empirically_contingent).
narrative_ontology:cs_reference_frame('26699ea1-6653-4f66-8dcf-9beda309772c', animals_as_rights_bearing_persons).
narrative_ontology:cs_drift_state('26699ea1-6653-4f66-8dcf-9beda309772c', contemporary_legal_regime, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('26699ea1-6653-4f66-8dcf-9beda309772c', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, large_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, smallholder_livestock_farmers).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, companion_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, smallholder_livestock_farmers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raised in controlled confinement — barns, sheds, feedlots, cages — bred for production traits, moved through standardized feeding, milking, laying, fattening, transport, and slaughter schedules set by the industries and regulators above them. Everything about their environment, diet, reproduction, and death is decided by others. There is no location, social arrangement, or legal category available to them outside human custody; individual escape attempts occur and end in recapture or death.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Bred to order for research protocols; housed in facility cages under experimenters' schedules of procedures, dosing, deprivation, and killing at protocol end. Their numbers and conditions are set by grant funding, institutional review boards, and replacement-reduction-refinement policies. No life outside the facility exists for them; release is fatal and unauthorized.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Bred by humans for temperament and dependence, kept in homes under owners' decisions about food, movement, reproduction (typically sterilization), medical care, and euthanasia. Affection and care flow to them and they bond with their keepers, but every parameter of their existence is chosen for them; abandonment and surrender are common, and shelters kill surplus animals.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, companion_animals, payer,
    powerless, biographical, trapped, global).

% Operates concentrated breeding, hatching, feeding, processing, and distribution systems covering tens of billions of animals annually; sets genetics, housing density, transport, and slaughter standards; funds agricultural research and lobbies legislatures for favorable statutes and against restrictive ones; sells into retail and export markets. Annual revenue runs to the trillions; capital moves freely between species, regions, and product lines.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture, beneficiary).

% Buy meat, dairy, eggs, leather, wool, and animal-tested products as the default offerings of food systems; receive the products cheaply relative to income and rarely encounter production conditions. Alternatives exist and are expanding, but habit, cuisine, price signals, nutrition beliefs, and social settings keep most purchases in place.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, biographical, constrained, global).

% Draft and enforce welfare statutes and inspection regimes — stocking-density limits, stunning requirements, transport-duration caps, laboratory review boards — operating entirely within the existing legal category that treats animals as owned assets. Agencies publish compliance reports; enforcement varies by jurisdiction and budget. Officials can move between agencies, academia, or advocacy without personal dependency on the arrangement.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_welfare_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Run corporate campaigns, certification labels, and legislative welfare initiatives; raise donations in the hundreds of millions annually by publicizing reforms won (cage-free pledges, confinement bans). Staff careers and organizational brands are built on the reform model; internal debate over whether incremental campaigns undermine the larger goal is persistent and unresolved, and the organizations' histories make abandoning the model tantamount to dissolving themselves.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, large_welfare_organizations, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, large_welfare_organizations, agenda_setter).

% Keep modest herds for milk, meat, wool, or draft; depend on animal sales for household income and often for land tenure and credit. Squeezed between processor prices and retailer margins, many carry debt secured against livestock; exiting animal husbandry means losing livelihood assets built over generations.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, smallholder_livestock_farmers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, smallholder_livestock_farmers, payer).

% Scholars, lawyers, and campaign groups arguing that the legal category itself is the wrong to be removed; they file personhood litigation, publish, and run public education. Largely shut out of legislative negotiation, which proceeds among industry, agriculture committees, and large reform organizations; some face surveillance, infiltration charges, or prosecution under statutes restricting activism.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, generational, mobile, global).

% The philosophical and legal-theory seat evaluating the arrangement: traces the genealogy of the owning category, weighs sentience evidence against legal classifications, and compares jurisdictions and reform trajectories. Holds no stake in production or donation flows.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, industrial_animal_agriculture).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns exclusive, transferable, legally recognized control over each animal to a human owner, which resolves disputes over possession and liability, enables price formation and markets in animals and their products, standardizes veterinary custody and record-keeping, and lets states delegate day-to-day custodial responsibility to private parties.
% TRANSFER_FUNCTION: Moves animals' bodies, labor, reproductive output, and ultimately their lives to human users; moves purchase money up the supply chain from consumers to producers; moves moral reassurance from welfare institutions and certification schemes to consumers; moves political contributions and lobbying pressure from producers to legislators.
% ABSENT_VOICES: The animals themselves — the parties whose bodies the arrangement allocates — cannot speak, vote, litigate, or refuse; their interests reach decision forums only through human proxies whose funding and careers sit inside the same arrangement. Would-be objectors with no seat: the animals; future generations of animals whose existence the breeding system schedules; abolitionist advocates, who are present in public discourse but excluded from legislative negotiation.
% DISAPPEARANCE_RATIONALE: Food systems, biomedical research, clothing supply chains, pet-keeping, veterinary medicine, land use, and several trillion dollars of annual commerce are organized around the category; overnight removal would force simultaneous redesign of protein production, research methods, and law, and would strand billions of dependent domesticated animals with no habitat or self-care capacity.
% FOUNDING_PROBLEM: Securing reliable supplies of animal labor, food, fiber, and transport for human communities, and ordering conflicts among humans over valuable living assets — problems domestication addressed from the Neolithic onward and formal property law later standardized.
% FOUNDING_PROBLEM_CORROBORATION: Historical and archaeological scholarship corroborates the subsistence origin of domestication independently of any benefiting party; contemporary ethology and neuroscience corroborate the sentience evidence that reframes the problem. No source outside the benefiting parties attests that the owning relation remains the necessary solution — industry and reform organizations assert it from inside the arrangement, while abolitionist legal scholarship argues from outside that the framing itself is the defect.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.95: the categorical reading prices the arrangement at near-total appropriation — each governed animal's environment, diet, reproduction, offspring, movement, and death are decided and taken by others, and the care returned is discretionary, revocable, and beside the point of the violated right; the 0.05 headroom acknowledges the marginal welfare flow without letting it launder the total. Suppression 0.88 is authored RAW and UNSCALED (suppression is a structural property; the engine scales only extractiveness, by directionality and scope): confinement is physical and total, breeding for tractability removes refusal in advance, personhood is legally denied, and the enforcement ratchet against challengers (activist-restriction statutes, surveillance, prosecution) has visibly tightened since the 2000s. Theater_ratio 0.58: the welfare layer — certifications, cage-free pledges, humane labeling, audit reports — performs moral reassurance while leaving the owning relation intact; from this seat a majority of the arrangement's legitimating activity is performance, though the logistics of confinement and slaughter remain brutally functional, which keeps the ratio below the inertial band. Accessibility_collapse 0.88: for the governed class there is no alternative — no exit, refusal, or outside exists anywhere in the arrangement — while human-side alternatives (plant-based substitution) persist and grow, which is why the value stops short of the natural-law band. Resistance 0.38: animals resist continuously and are crushed routinely; human opposition (abstention, advocacy, litigation, direct action) is real, growing, and a durable minority. All three tracked series share one six-point grid (1975–2025, decade spacing) so every metric is authored at every examined time point. Trajectories are monotone rising because aggregate absorption of animals into the arrangement (annual slaughter volume roughly tripling over the interval) swamps campaign-level cycles — welfare-scandal, pledge, relaxation, accumulation cycles are visible inside each decade but net out to the recorded trend; the series records the net, not the noise. Suppression_requirement is tracked deliberately: this story's enforcement history is a ratchet (machinery built up against a growing challenge), not a static picture already captured by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the farmed-animal seat the arrangement is a total condition with no alternative — whatever the engine computes there will carry the trapped, full-target signature. From the industrial-agriculture seat the same structure is an orderly allocation system it administers profitably — coordination with rent attached. From the welfare-regulator seat it is a progressively improving regime (each statute a step), i.e., the transitional reading the welfare sibling institutionalizes. From the large-welfare-organization seat it is a career-and-brand platform whose identity fuses with incrementalism — identity_locked exit means the seat cannot reprice its own history even if the trajectory evidence turns against it. The consumer seat computes mild beneficiary positioning with constrained exit. The engine derives these divergences from the structural data; this story's snare claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations put the three animal classes at the full-target end (d approaching 1.0): they appear in the victims array, hold no exit option short of death, and their powerlessness is constitutive — the arrangement selected for it. Industrial agriculture derives near-full beneficiary positioning (d approaching 0.0): it appears in the beneficiaries array, sets the rules it obeys, and holds arbitrage-grade exit across species, regions, and product lines. Consumers derive low d (beneficiary, constrained exit): they collect the products but cannot fully exit their own habits and cuisines. Welfare regulators sit mid-low: they administer rather than own the flow, with mobile exit. Large welfare organizations derive low d from their beneficiary role, but their identity_locked exit marks them as the capture-prone seat — collected gains (donations, relevance) ride on the arrangement continuing in amended form. Smallholders are the one genuinely dual-positioned seat: beneficiary of the category, payer to its industrial tier; the derivation reads their beneficiary declaration first, which understates their squeeze. A directionality override could correct this, but overrides key on the power atom alone, and smallholders share the moderate atom with consumers — one override cannot separate the two seats, so none is authored and the derivation's role-first read is accepted as the lesser error. No other override candidates exist: every other seat's derived d matches its declared structure. Global spatial scope on the dominant seats amplifies effective extraction through verification difficulty, per the engine's scope modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution here runs against the grain of the welfare sibling. The welfare reading resolves the arrangement's history as gradual civilization — a rough institution being reformed toward adequacy, each statute a payment. The abolitionist reading refuses that resolution: the founding mandate (securing animal bodies for human use) is not obsolete — it is operating at record scale — so nothing here is inertial or spent; the arrangement is maintained by active, intensifying enforcement, which is why theater_ratio rises alongside suppression_requirement rather than replacing it. The analysis guards the opposite error too: claiming the whole arrangement as pure extraction does not license ignoring the welfare layer's real content (inspections, review boards, measurable condition changes for some animals); that content is carried honestly in the metrics and in the welfare_reform_trajectory omega, which tests whether the reform layer is extraction-management or transition. The R5 interview records the founding problem as contested rather than dead: the mismatch consumer therefore finds no zombie flag here — correctly, since the arrangement's mandate is grotesquely alive — while the corroboration field ensures the live attestation rests on outside evidence (subsistence-history scholarship, sentience science) rather than on the beneficiaries' own account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (abolitionist_reading) of the animal_status_kernel; where exactly do the three readings disagree, and what would adopting a sibling reading change structurally?',
    'Conceptual: resolution occurs when a jurisdiction, discipline, or community adopts one reading''s status assignment; compare instantiated legal and cultural frameworks across jurisdictions and over time.',
    'Adopting property_reading collapses the victim-set to nil (economic value exhausts the question) and drops epsilon toward the coordination floor; adopting welfare_reading shrinks the victim-set to suffering-above-threshold cases and lowers epsilon substantially while retaining the owning relation. This file''s high epsilon and full victim-set hold only under the abolitionist assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located at the moral-status boundary variable and the permissibility-of-use variable.').

omega_variable(
    welfare_reform_trajectory,
    'Do welfare reforms delay abolition (by making use more efficient and palatable, entrenching the owning relation behind improved conditions) or advance it (by building public concern, infrastructure, and momentum that later radicalizes)?',
    'Longitudinal cohort analysis: jurisdictions with major reform wins (cage-free pledges, confinement bans) tracked against matched controls for subsequent per-capita consumption, total animal use, and incidence of abolitionist legislation.',
    'If reforms delay abolition, the welfare layer functions as extraction management and the snare reading strengthens; if they advance it, the arrangement contains a transitional element the snare claim misses and a scaffold-stage analysis becomes apt for the reform layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_trajectory, empirical, 'The strategic dispute with the welfare sibling: whether incremental reform delays or advances abolition.').

omega_variable(
    victim_set_boundary,
    'Which animals fall inside the right not to be property — all sentient animals, vertebrates only, or all animals capable of valuing their own lives — and does the reading extend to free-living animals not currently owned?',
    'Convergence of sentience criteria (neurobiology, behavioral evidence) with the reading''s own personhood criterion; jurisdiction-by-jurisdiction extension of personhood litigation and its reception.',
    'Boundary placement scales the victim-set and aggregate extraction: a narrower boundary shrinks the target class and lowers measured extraction; extending the right to free-living animals adds hunting, culling, and habitat expropriation to the transfer surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Where the moral-person boundary falls determines the size and composition of the victim-set.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression is structural (physical confinement, legal denial of personhood, activist-restriction statutes) versus internalized on the human side (habit, cuisine, managed cognitive dissonance sustained by welfare reassurance)?',
    'Post-information trajectory studies: whether informed cohorts'' reduced animal-product consumption persists after campaigns end; whether legal liberalization (repeal of activism-restricting statutes) changes conduct without prior attitude change.',
    'If the internalized share dominates, repealing enforcement statutes would not dissolve the arrangement, and classifying suppression as structural-only would overstate how cheap legal remedies are; if structural shares dominate, enforcement decay would produce rapid relaxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized shares of the suppression holding the arrangement in place.').

omega_variable(
    domesticated_dependency_transition,
    'Domesticated animals cannot survive outside human custody; what does ending the arrangement require for the existing billions, and does that bred-in dependency make removal prohibitively costly however the transition is designed?',
    'Sanctuary-capacity modeling and phased-retirement economics for livestock populations; comparative analysis with prior large-scale institutional wind-downs.',
    'If transition costs are genuinely prohibitive, the arrangement persists even under consensus that it is unjust — a persistence source distinct from coercion that the classification should register separately; if costs are manageable, a prohibitive fixing-cost reflects political economy rather than necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domesticated_dependency_transition, empirical, 'Whether bred-in dependency makes removal costly independent of political will.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1975, animal_status_kernel__abolitionist_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(anim_tr_t1985, animal_status_kernel__abolitionist_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(anim_tr_t1995, animal_status_kernel__abolitionist_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(anim_tr_t2005, animal_status_kernel__abolitionist_reading, theater_ratio, 2005, 0.47).
narrative_ontology:measurement(anim_tr_t2015, animal_status_kernel__abolitionist_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(anim_tr_t2025, animal_status_kernel__abolitionist_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(anim_be_t1975, animal_status_kernel__abolitionist_reading, base_extractiveness, 1975, 0.82).
narrative_ontology:measurement(anim_be_t1985, animal_status_kernel__abolitionist_reading, base_extractiveness, 1985, 0.85).
narrative_ontology:measurement(anim_be_t1995, animal_status_kernel__abolitionist_reading, base_extractiveness, 1995, 0.87).
narrative_ontology:measurement(anim_be_t2005, animal_status_kernel__abolitionist_reading, base_extractiveness, 2005, 0.9).
narrative_ontology:measurement(anim_be_t2015, animal_status_kernel__abolitionist_reading, base_extractiveness, 2015, 0.93).
narrative_ontology:measurement(anim_be_t2025, animal_status_kernel__abolitionist_reading, base_extractiveness, 2025, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1975, animal_status_kernel__abolitionist_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(anim_su_t1985, animal_status_kernel__abolitionist_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(anim_su_t1995, animal_status_kernel__abolitionist_reading, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement(anim_su_t2005, animal_status_kernel__abolitionist_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(anim_su_t2015, animal_status_kernel__abolitionist_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(anim_su_t2025, animal_status_kernel__abolitionist_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the moral status of animals' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: this abolitionist reading (full victim-set, epsilon 0.95, snare-claimed), the property reading (nil victim-set, epsilon at floor, market-coordination frame), and the welfare reading (threshold-relieved victim-set, mid epsilon, regulated-use frame). Each file authors its own epsilon over the SAME standing-arrangement referent; the files are linked so contamination and coupling analysis can trace how movement in one reading's legitimacy conditions pressures the others. Upstream/downstream: the property reading is upstream (its legal codification is the arrangement all readings contest); the welfare reading sits between it and this one historically (welfare statutes amend the owning relation without revising it), which is why this reading's drift_state records repudiation_pressure against a frame the other two readings still inhabit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
