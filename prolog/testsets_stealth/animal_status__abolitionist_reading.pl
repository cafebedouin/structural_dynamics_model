% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Instrumental-Use Arrangement (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the animal_status kernel: the
 *   abolitionist reading, under which animals are rights-holders with
 *   inherent value and no instrumental use is permissible. Per the
 *   kernel-reading epsilon rule, the referent of extractiveness is the
 *   STANDING arrangement under contest — the existing legal-social order in
 *   which animals are property and instrumental use is lawful and pervasive —
 *   assessed by the abolitionist reading's own lights. It is NOT the
 *   rights-respecting arrangement this reading would enact (which would drive
 *   epsilon toward zero and make every advocacy reading trivially benign).
 *   The reading admits no low-extraction use categories: food, research, and
 *   entertainment uses all count at full weight, which is why epsilon is
 *   uniformly high across the victim set. The sibling readings
 *   (welfare_reading, property_reading) are separate constraints in separate
 *   files, linked through network.affects_constraints; their victim sets and
 *   epsilon values differ structurally, and nothing in this file averages
 *   over them. Claim and metrics are independently authored: the claimed type
 *   is snare because, from this seat, the arrangement's coordination story
 *   (necessary protein, necessary research models) is contested cover for an
 *   arrangement whose persistence depends on property-law coercion,
 *   criminalized investigation and rescue, and the total unavailability of
 *   exit to its victims — while the metrics simply describe how the
 *   arrangement operates.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bear the full cost of the arrangement
 *   - laboratory_animals: primary target (powerless/trapped) — bear the research cost
 *   - captive_entertainment_animals: secondary target (powerless/trapped)
 *   - industrial_animal_agriculture_operators: primary beneficiary and co-agenda-setter (institutional/arbitrage) — collect the bulk of the gains and fund the legal maintenance
 *   - biomedical_research_institutions: secondary beneficiary (institutional/arbitrage)
 *   - animal_product_consumers: incidental beneficiaries carrying diffuse cost share (moderate/mobile) — hold the cheapest exit in the system
 *   - abattoir_and_farm_workers: human cost-bearers (powerless/constrained)
 *   - animal_advocacy_movements: excluded resisters (organized/mobile) — outside the rule-making tables, exposed to enforcement
 *   - agricultural_legislators_and_regulators: agenda-setters (institutional/constrained) — administer the classification under industry electoral and financial gravity
 *   - welfare_certification_bodies: legitimation collectors (moderate/mobile) — paid only while use continues
 *   - abolitionist_rights_scholars: analytical observer (analytical/analytical) — see the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.93).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental-Use Arrangement (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'a72d97a8-374c-4e4f-8a96-bb956fd12f75').
narrative_ontology:cs_kernel_codification('a72d97a8-374c-4e4f-8a96-bb956fd12f75', formalized).
narrative_ontology:cs_authority_grounding('a72d97a8-374c-4e4f-8a96-bb956fd12f75', extraction).
narrative_ontology:cs_interpretation_layer_present('a72d97a8-374c-4e4f-8a96-bb956fd12f75').
narrative_ontology:cs_reading_relation('a72d97a8-374c-4e4f-8a96-bb956fd12f75', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('a72d97a8-374c-4e4f-8a96-bb956fd12f75', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('a72d97a8-374c-4e4f-8a96-bb956fd12f75', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('a72d97a8-374c-4e4f-8a96-bb956fd12f75', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('a72d97a8-374c-4e4f-8a96-bb956fd12f75', foundational, sentience_suffices_for_rights_holding).
narrative_ontology:cs_axiom_status(sentience_suffices_for_rights_holding, holdable).
narrative_ontology:cs_axiom_grounding('a72d97a8-374c-4e4f-8a96-bb956fd12f75', sentience_suffices_for_rights_holding, deontological).
narrative_ontology:cs_axiom('a72d97a8-374c-4e4f-8a96-bb956fd12f75', secondary, welfare_reform_functions_as_legitimation).
narrative_ontology:cs_axiom_status(welfare_reform_functions_as_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('a72d97a8-374c-4e4f-8a96-bb956fd12f75', welfare_reform_functions_as_legitimation, empirically_contingent).
narrative_ontology:cs_reference_frame('a72d97a8-374c-4e4f-8a96-bb956fd12f75', inherent_value_rights_baseline).
narrative_ontology:cs_drift_state('a72d97a8-374c-4e4f-8a96-bb956fd12f75', contemporary_positive_law, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a72d97a8-374c-4e4f-8a96-bb956fd12f75', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, industrial_animal_agriculture_operators).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, captive_entertainment_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, welfare_certification_bodies).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, abattoir_and_farm_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raised in controlled confinement, bred, fed, transported, and slaughtered on schedules set entirely by the operations that own them. Every condition of their lives — space, diet, reproduction, lifespan — follows contractual and regulatory choices made by others. They cannot refuse, leave, or negotiate; nothing they do alters the terms they live under.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bred into research colonies and assigned to protocols by review committees seated inside the using institutions. Their exposure to procedures, housing conditions, and endpoints follows the research calendar. No channel exists through which their interests enter the decision except as weighed by the people conducting the research.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Kept in zoos, aquaria, racing stables, and performance venues for display and spectacle. Transferred between facilities by sale or loan; routines and breeding managed around visitor revenue. Relocation occurs only when owners decide.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, captive_entertainment_animals, payer,
    powerless, biographical, trapped, regional).

% Hold animal stock as inventory and collect revenue from converting it into meat, dairy, eggs, hides, and feed inputs. Fund the lobbying, model legislation, and trade associations that maintain the legal classification their balance sheets rest on. Can restructure herds, shift species or geographies, or convert facilities in response to market or legal change.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, industrial_animal_agriculture_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, industrial_animal_agriculture_operators, agenda_setter).

% Maintain animal colonies as experimental infrastructure and publish from the resulting data. Internal boards set the limits on use. Funding cycles reward protocol continuity; switching modalities is possible but slow and costly, and contract or offshore options soften any single-jurisdiction restriction.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, continental).

% Purchase the outputs at prices made low by scale and by costs pushed outside the price — ecological load, public-health burden, and the moral weight of participation. Individually they can stop buying animal products at low personal cost, the cheapest exit available to anyone in the arrangement, though habit, culture, subsidy-shaped prices, and information asymmetry keep most from exercising it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_product_consumers, payer).

% Perform the killing and processing the arrangement requires, at high injury rates and with documented psychological toll, for wages set in thin rural labor markets. Immigration status and local employment concentration limit mobility; leaving usually means relocating households.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abattoir_and_farm_workers, payer,
    powerless, immediate, constrained, regional).

% Campaign for legal standing, run undercover investigations, litigate, and push for phase-out legislation. They sit outside the formal rule-making tables — agricultural committees and standards boards are composed of the regulated industries — and face arrest, prosecution under enterprise-terrorism statutes, and civil liability when they document conditions. Their leverage runs through public opinion and market campaigns rather than a seat in the process.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_advocacy_movements, excluded,
    organized, biographical, mobile, global).

% Write and administer the property classifications, welfare floors, inspection regimes, and speech restrictions that constitute the arrangement. Electoral dependence on producing districts and revolving-door employment with the industry constrain how far they can move against the operators who fund them.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, agricultural_legislators_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Charge producers for auditing and labeling compliance with welfare standards. Their revenue exists only so long as animal use continues and consumers seek reassurance; they therefore market the acceptability of use rather than its cessation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_certification_bodies, beneficiary,
    moderate, biographical, mobile, global).

% Develop the inherent-value argument, document the gap between stated welfare protections and practice, and supply the conceptual framework that advocacy movements deploy. They bear none of the arrangement's costs and collect none of its revenues; their seat is evaluative.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_rights_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, industrial_animal_agriculture_operators).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production, processing, and distribution of animal-derived goods at population scale: centralized ownership makes breeding, feeding, transport, slaughter, and disposal schedulable as logistics, and standardizes research subjects for experimental replication. The reading disputes the necessity of the animal-based method, not the existence of the logistical achievement.
% TRANSFER_FUNCTION: Moves the bodies, reproductive capacity, labor, and lives of animals to operators, laboratories, and venues as meat, dairy, eggs, data, and spectacle; moves the associated revenues up the supply chain; and moves the residual costs — zoonotic risk, dietary disease, ecological load, and the moral burden of participation — outward onto consumers, fenceline communities, and future generations.
% ABSENT_VOICES: The animals whose interests the arrangement allocates appear in no rule-making forum; their interests are voiced only by the parties using them. Independent advocates are kept outside agricultural committees and standards boards. Slaughterhouse workers' testimony is chilled by immigration status and blacklisting. Residents of fenceline communities bear water and air burdens without representation in siting decisions.
% DISAPPEARANCE_RATIONALE: Food systems, biomedical research, property law, welfare codes, and rural economies are all organized around the arrangement. Overnight removal forces wholesale substitution of plant and cell-cultivated provisioning, restructuring of research methodology, rewriting of property statutes, and repricing of every animal-derived commodity — the world rearranges.
% FOUNDING_PROBLEM: Securing reliable supplies of animal-derived food, labor, fiber, and experimental material under pre-industrial scarcity, when no substitutes existed and managing living stock required a tractable legal form — hence the classification of animals as property.
% FOUNDING_PROBLEM_CORROBORATION: Industry associations and agricultural ministries attest the problem is live, citing global protein demand and research-model continuity. Corroboration from outside the benefiting parties: dietetics-body consensus statements attest that well-planned plant-based patterns meet nutritional needs across life stages; land-use and food-systems research attests substitution feasibility at scale; historians of domestication attest the property classification predates and outlasted its original scarcity rationale. No seat that collects nothing from the arrangement attests that the necessity claim remains sound.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.93, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.93 because the transfer is near-total: the arrangement takes animals' bodies, liberty, reproduction, and lives, and returns nothing to them; the reading grants no use-category discount, so no averaging-down applies. Suppression is 0.85 and unscaled by power or scope — it is the raw structural fact of property law backed by police power, enterprise-terrorism and ag-gag statutes, worker precarity, and normalized defaults. Theater_ratio is 0.55: a substantial and growing share of the arrangement's activity is welfare auditing, enrichment mandates, and humane labeling that reassure purchasers while leaving the underlying operation unchanged. Accessibility_collapse is 0.45 — alternatives (plant-based provisioning, non-animal methods) remain accessible and expanding, so the constraint does not fully collapse them, though subsidy-shaped prices and information asymmetry partially obscure them. Resistance is 0.5: an organized movement exists and acts, but faces criminalization and exclusion from formal channels. On coalition power: the animal victims cannot form coalitions — they lack any agency channel — while the human cost-bearers (workers, fenceline communities) could in principle combine but are divided by geography, immigration status, and employment dependence. The temporal series run on one shared grid (every tracked metric authored at every time point 0–50). Trajectories are monotonic, not cyclical: industrialization concentrated and scaled the operation (rising base_extractiveness), welfare certification proliferated faster than any reduction it produced (rising theater_ratio), and enforcement machinery demonstrably hardened over the interval — enterprise-terrorism statutes, ag-gag waves, expanded policing of investigations — which is why suppression_requirement is tracked as a series rather than left as a static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the operator seat the arrangement appears as logistics it built and maintains: feed conversion, cold chains, standardized subjects — a resource-allocation achievement it stewards. From the animal seats the same structure registers as total and inescapable: powerless, trapped, no forum, no exit, every term of life set by others. Consumers sit near symmetric — real convenience gained, diffuse health, ecological, and moral costs carried, and a cheap exit most never take. Workers bear severe localized costs with constrained exit despite being human participants. Advocates bear targeted enforcement pressure while collecting no gains. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: operators (arbitrage-grade exit, institutional power, direct revenue) sit nearest the beneficiary end; research institutions and certification bodies similarly collect without bearing. Victim declarations map to high directionality: all three animal groups are powerless and trapped, placing them at the full-target end, where effective extraction is amplified rather than damped. Consumers carry a dual declaration (beneficiary with secondary payer) and mobile exit, landing them near symmetric. Workers are declared victims with constrained exit — high directionality despite human standing. Legislators are agenda-setters whose capture tilts them toward the beneficiary side of symmetric. No directionality overrides were needed: the beneficiary/victim declarations plus power and exit atoms already produce the correct ordering, and the commentary records the qualitative logic the derivation implements.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline guards against two symmetrical errors here. First, accepting the arrangement's coordination cover and coding it as rope or hybrid coordination: the founding problem (securing animal-derived goods under pre-industrial scarcity) is contested rather than live precisely because substitutes now exist, and a constraint whose necessity story is disputed by everyone who collects nothing from it should not inherit coordination credit by default. Second, the reverse error: treating the welfare-reform machinery as a transitional support heading toward dissolution. On this reading the welfare layer functions as legitimation — theater_ratio rises across the whole interval while extraction rises alongside it — which is the signature of maintenance, not transition; the absence of any sunset clause confirms no transition is declared. Mandatrophy is marked unresolved: the founding problem's status is genuinely contested, and declaring resolution would assert the reading's conclusion as settled fact. The R5 mismatch surface (contested status x world_rearranges verdict) is the honest entry point for the obsolescence question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the abolitionist reading of the animal_status kernel. Would the welfare or property sibling readings produce structurally different constraints with different victim sets and different epsilon over the same standing arrangement?',
    'Generate the sibling stories as separate files and compare victim sets, epsilon, and computed types across the family; never average the readings inside one constraint.',
    'Under the property reading the victim set empties by stipulation (no independent standing) and epsilon collapses toward the owners'' seat; under the welfare reading victims shrink to interests outweighed after balancing. Classification is indexical to the reading; merging readings would fabricate a single epsilon for three distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame uncertainty: which reading of the animal-status kernel a story instantiates determines its victim set and epsilon.').

omega_variable(
    instrumental_use_necessity,
    'Is continued instrumental use of animals necessary to meet human nutrition, medical, and research needs, or are substitutes sufficient across all use categories?',
    'Nutritional-adequacy studies of fully plant-based provisioning; scaling and cost-curve data for cultivated meat and non-animal research methodologies; regional food-security analysis where substitutes are claimed to fail.',
    'If substitutes suffice everywhere, the arrangement''s coordination cover fails and the extraction-first reading hardens; where substitutes genuinely fail in specific contexts, pockets of genuine coordination remain and epsilon drops locally, pulling toward hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_use_necessity, empirical, 'Whether the necessity claim sustaining the arrangement survives substitute technologies.').

omega_variable(
    welfare_reform_trajectory,
    'Do welfare reforms reduce the arrangement''s extraction over time, or do they entrench it by certifying use as acceptable (the legitimation hypothesis this reading asserts)?',
    'Track per-capita consumption, herd sizes, and enforcement outcomes before and after major welfare statutes; test whether reform precedes decline or correlates with expansion of use.',
    'If legitimation dominates, theater_ratio keeps climbing while extraction is undiminished and the arrangement ossifies; if reforms function as a genuine transition path, transitional-support dynamics appear and the terminal state changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_trajectory, empirical, 'Whether the welfare layer is a reduction mechanism or a legitimation mechanism.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is legal-economic enforcement (property law, enterprise-terrorism statutes, ag-gag regimes) versus internalized normalization (default dietary cultures, identity-fused food traditions) that would persist if enforcement lapsed?',
    'Compare compliance and consumption trajectories in jurisdictions where enforcement weakens; measure attitude change across generations exposed to differing enforcement intensity.',
    'If the internalized share is large, effective suppression exceeds the structural measure and outlasts legal repeal; post-repeal arrangements would still register constraint until norms decay, changing any transition forecast.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_abolitionist_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(animal_status_abolitionist_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(animal_status_abolitionist_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(animal_status_abolitionist_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(animal_status_abolitionist_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(animal_status_abolitionist_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(animal_status_abolitionist_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(animal_status_abolitionist_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(animal_status_abolitionist_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(animal_status_abolitionist_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.87).
narrative_ontology:measurement(animal_status_abolitionist_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(animal_status_abolitionist_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_abolitionist_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(animal_status_abolitionist_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(animal_status_abolitionist_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(animal_status_abolitionist_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(animal_status_abolitionist_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(animal_status_abolitionist_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what is the moral status of animals?' decomposes into three structurally distinct constraints sharing one kernel (animal_status): this abolitionist reading (rights-holders; every instrumental use counts at full weight; animals fully in the victim set), the welfare reading (interests constrain but permit use; victims limited to interests outweighed), and the property reading (no independent standing; victim set empty by stipulation). Each carries its own epsilon over the same standing arrangement. The property reading is the upstream positive-law baseline the other two contest; welfare reforms are the contested interface between the welfare and abolitionist stories. Sibling files: animal_status__welfare_reading, animal_status__property_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
