% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Moral Status - Welfare Reading (Regulated-Use Constraint)
 *   domain: applied ethics/legal philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the animal_moral_status kernel:
 *   the welfare reading, under which animals are sentient beings whose
 *   suffering morally weighs, cruelty is prohibited, and use itself is
 *   permissible provided suffering is minimized within regulated systems. The
 *   standing arrangement under contest - and the fixed epsilon referent - is
 *   the existing instrumental-use arrangement as governed by anti-cruelty
 *   statutes, humane-slaughter rules, and certification schemes, assessed by
 *   this reading's own lights. That arrangement delivers a genuine
 *   coordination function (enforceable common baselines preventing a race to
 *   the bottom in treatment) while channeling a persistent, lawful residue of
 *   animal suffering through the same structure, from which regulated
 *   industries draw social license and welfare organizations draw legitimacy,
 *   revenue, and institutional purpose. Per the epsilon-invariance principle,
 *   the colloquial concept 'animal moral status' decomposes into three
 *   structurally distinct constraints - one per reading - sharing a single
 *   referent but authoring different epsilon values; the sibling stories
 *   (property_reading, abolitionist_reading) carry their own metrics,
 *   stakeholders, and types, and are linked through
 *   network.affects_constraints. The claim/metric independence rule is
 *   honored: claimed_type is authored as tangled_rope because the structure
 *   exhibits both genuine coordination and asymmetric extraction under active
 *   enforcement, while the metrics are authored from the arrangement's
 *   observed operation without tuning toward any predicted engine output.
 *
 * KEY AGENTS:
 *   - - regulated_animal_use_industries: Primary beneficiary (powerful/arbitrage) - converts the permitted suffering budget into social license, shelf access, and price premiums
 *   - - animal_welfare_organizations: Legitimacy beneficiary and co-administrator (organized/identity_locked) - collects donations, audit fees, and standing from administering the framework
 *   - - farmed_animals_under_humane_standards: Primary target (powerless/trapped) - bears the lawful suffering residue at population scale
 *   - - research_animals_under_humane_standards: Secondary target (powerless/trapped) - bears suffering licensed as necessary under harm-benefit review
 *   - - legislators_and_regulators: Agenda setter (institutional/mobile) - writes and revises the codes under electoral and lobbying pressure
 *   - - consumers_of_certified_products: Incidental beneficiary/payer (moderate/mobile) - buys moral comfort, pays premiums
 *   - - abolitionist_advocates: Excluded critic (organized/constrained) - contests the framework's premise from outside the process
 *   - - animal_welfare_science_establishment: Analytical observer (institutional/analytical) - measures the gap between codes and realized outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.55).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.45).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Moral Status - Welfare Reading (Regulated-Use Constraint)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied ethics/legal philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '49ff2187-02bf-473d-b68a-bab5a623c3ad').
narrative_ontology:cs_kernel_codification('49ff2187-02bf-473d-b68a-bab5a623c3ad', distributed).
narrative_ontology:cs_authority_grounding('49ff2187-02bf-473d-b68a-bab5a623c3ad', expertise).
narrative_ontology:cs_interpretation_layer_present('49ff2187-02bf-473d-b68a-bab5a623c3ad').
narrative_ontology:cs_reading_relation('49ff2187-02bf-473d-b68a-bab5a623c3ad', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('49ff2187-02bf-473d-b68a-bab5a623c3ad', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('49ff2187-02bf-473d-b68a-bab5a623c3ad', foundational, use_permissible_if_suffering_minimized).
narrative_ontology:cs_axiom_status(use_permissible_if_suffering_minimized, holdable).
narrative_ontology:cs_axiom_grounding('49ff2187-02bf-473d-b68a-bab5a623c3ad', use_permissible_if_suffering_minimized, deontological).
narrative_ontology:cs_axiom('49ff2187-02bf-473d-b68a-bab5a623c3ad', foundational, sentience_grounds_protection_not_rights).
narrative_ontology:cs_axiom_status(sentience_grounds_protection_not_rights, holdable).
narrative_ontology:cs_axiom_grounding('49ff2187-02bf-473d-b68a-bab5a623c3ad', sentience_grounds_protection_not_rights, deontological).
narrative_ontology:cs_axiom('49ff2187-02bf-473d-b68a-bab5a623c3ad', secondary, certified_regulation_confers_legitimacy).
narrative_ontology:cs_axiom_status(certified_regulation_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('49ff2187-02bf-473d-b68a-bab5a623c3ad', certified_regulation_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('49ff2187-02bf-473d-b68a-bab5a623c3ad', sentience_weighted_regulated_use).
narrative_ontology:cs_drift_state('49ff2187-02bf-473d-b68a-bab5a623c3ad', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49ff2187-02bf-473d-b68a-bab5a623c3ad', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, farmed_animals_under_humane_standards).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, research_animals_under_humane_standards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers_of_certified_products).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, consumers_of_certified_products).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_grounds_moral_consideration).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, regulated_use_compatibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run certification and assurance schemes, co-draft welfare codes with regulators, prosecute or publicize cruelty cases, and solicit donations on the strength of their guardian role. Income arrives as membership dues, donations, and audit and certification fees paid largely by the industries they oversee. Staff careers, brand identity, and donor base are built entirely around the regulated-welfare model; pivoting to a fundamentally different program would dissolve the organization's purpose and its funding.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, animal_welfare_organizations, agenda_setter).

% Produce meat, dairy, eggs, and research services under welfare codes that set maximum stocking densities, transport durations, and slaughter methods. Compliance costs are real but are repaid many times over by the social license the certified-humane label purchases: retailers stock the products, consumers buy without moral discomfort, and stricter proposals stall at the answer 'already regulated.' When rules tighten in one jurisdiction, production can shift toward jurisdictions with weaker codes.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_use_industries, beneficiary,
    powerful, generational, arbitrage, global).

% Live out shortened lives inside the systems the codes govern: confined at lawful densities, transported within lawful time limits, slaughtered by lawful methods. Every increment of suffering the codes permit is borne by them individually. They cannot refuse the arrangements, leave the systems, or advocate for different ones, and their interests enter the process only through human representatives who hold independent institutional agendas.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, farmed_animals_under_humane_standards, payer,
    powerless, immediate, trapped, global).

% Are bred, housed, and used in procedures licensed under harm-benefit analysis frameworks that require justification, anesthesia where practicable, and humane endpoints. The license regime permits substantial suffering it classifies as necessary. The animals subject to it have no recourse and no representation apart from the ethics committees that authorize their use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, research_animals_under_humane_standards, payer,
    powerless, immediate, trapped, global).

% Buy animal products at scale and pay price premiums for welfare-certified lines when motivated. Certification relieves the moral discomfort of purchase; the premium is a modest recurring cost. Individual buyers can switch brands, drop to uncertified cheaper lines, or abstain entirely, and any single choice alters little.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers_of_certified_products, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, consumers_of_certified_products, payer).

% Enact and revise welfare statutes, set stocking-density and slaughter rules, fund inspection services, and respond to petitions from welfare groups and industry alike. Electoral cycles and industry lobbying shape how far standards tighten; officials rotate between posts and carry no personal lock-in to any particular code.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, legislators_and_regulators, agenda_setter,
    institutional, biographical, mobile, national).

% Campaign to end animal use altogether, arguing that regulating the terms of use entrenches it. They publish, protest, and litigate at the margins but hold no seats on standard-setting bodies, receive no consultation in code revisions, and are routinely characterized by both industry and welfare charities as extreme. Their exclusion from the conversation is stable across jurisdictions.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Veterinary and animal-science researchers who develop welfare indicators, publish on the gap between codes and realized outcomes, and staff the advisory committees that translate findings into standards. They assess the system from outside any direct gain or loss, though their research agendas and funding depend on the regulatory apparatus continuing to exist.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_science_establishment, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in animal-using markets: without common enforceable minimum standards, producers willing to absorb the cost of humane handling are undercut by those who cut corners, driving treatment toward the cheapest floor. Shared codes, inspection, and certification give conscientious producers a verifiable baseline, give consumers a legible signal, and give prosecutors a defined line between husbandry and cruelty.
% TRANSFER_FUNCTION: Moves a legally bounded quantity of animal suffering from animals to humans as product availability and price; moves compliance costs onto producers; moves assurance and legitimacy from certifying organizations to sellers in exchange for audit fees and endorsements; moves enforcement funding from taxpayers to inspection services.
% ABSENT_VOICES: The animals whose suffering the codes ration cannot participate and are represented only by proxies whose institutional survival depends on the framework persisting. Abolitionist advocates, who reject the premise that use can be justified at any standard of care, hold no seats in code revision or certification governance; their objection is registered only outside the process.
% DISAPPEARANCE_RATIONALE: Certification markets, charity-sector roles, inspection bureaucracies, and retailer sourcing standards all presuppose the framework. Overnight removal would leave industries without social license, welfare organizations without function, prosecutors without a cruelty line, and consumers without assurance signals; the entire animal-use economy would renegotiate its terms around whatever replaced it.
% FOUNDING_PROBLEM: Unchecked gratuitous cruelty: nineteenth-century working animals beaten in streets, livestock shipped without food or water, blood sports, and slaughterhouse practices that shocked the first inspectors - conduct with no countervailing benefit that any consensus could identify.
% FOUNDING_PROBLEM_CORROBORATION: The historical record and animal-law scholarship from outside the beneficiary set corroborate that the original founding problem (gratuitous working-animal and sport cruelty) receded sharply after the early statutes. Whether the current problem is live is disputed: welfare organizations and welfare scientists attest ongoing cruelty requiring the framework; abolitionist scholars attest that the founding problem as stated is largely solved and the framework now performs legitimation of use. No source outside the disputing camps adjudicates between them.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.55 because, by this reading's own lights, the arrangement leaves substantial lawful suffering in place - stocking densities, transport durations, and slaughter methods that sit inside the codes while producing the suffering the reading exists to minimize - and that residue scales with production volume even where per-animal standards improve. Suppression is 0.45: the constraint runs on active enforcement machinery (inspection, prosecution, certification revocation) and on discursive closure that channels dissent into regulatory processes and keeps the use-question itself off the agenda; suppression is authored as a raw structural property and is not scaled by power or scope. Theater is 0.42: certification audits, welfare labels, and assurance schemes perform reassurance as much as protection, but a substantive core of genuine standard-setting and enforcement remains. Accessibility collapse is low (0.35) because alternatives stay visibly alive - abstention, plant-based substitution, and abolitionist politics are growing rather than collapsing - so understanding the constraint does not close the exit landscape. Resistance is 0.55 and notably two-fronted: industry resists tightening from below while abolitionists contest the framework's legitimacy from outside, an unusual bidirectional resistance profile for a coordination structure. The measurement series run on one shared time grid (all three tracked metrics authored at every point 0-60); trajectories are monotonic rather than cyclical - production scaling outpaced standard-tightening throughout the interval, so extraction accumulation and enforcement build-up proceed together without oscillation. Receipt surface: gains demonstrably accrue to regulated_animal_use_industries, which monetize the social license; welfare organizations collect a toll on the traffic (audit fees, donations) rather than the cargo itself, so they are beneficiaries without being the receipt seat. Fixing cost is prohibitive for the seats that could fix it: raising standards to eliminate the residual suffering triggers relocation arbitrage, food-price effects, and concentrated industry opposition against diffuse benefits.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the industry seat the arrangement is a favorable bargain: real compliance costs repaid many times over in license and market access, with an arbitrage exit no other seat holds. From the welfare-organization seat it is mission and revenue simultaneously - the organization has become the framework's administrator, and its identity_lock means exit (pivoting to a use-critical program) would dissolve institutional purpose and donor base; if that identity frame broke, the seat's classification would shift materially. From the animals' structural position the arrangement is the permitted residue itself. The usual remedy of coalition power among powerless payers is unavailable here in the strongest sense: the paying class lacks agency entirely, so representation is structurally delegated to proxies with independent agendas - which is why the exclusion of abolitionist voices matters more than token exclusion usually does. Consumers sit near symmetric, buying comfort and paying premiums. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. Farmed and research animals are declared victims with trapped exit and zero power: they sit at the full-target pole, and their trapping amplifies effective extraction since no mobility damps it. Regulated industries are declared beneficiaries but bear real compliance costs, placing them above the pure-beneficiary pole yet still net-subsidized once license value is counted. Welfare organizations derive near the beneficiary pole from their declaration, but their operational dependence on the industries they certify (audit revenue, cooperative standard-setting) and their stake in the framework's perpetuation pull them toward symmetric relative to a passive beneficiary; this is noted qualitatively rather than forced through an override, since the derivation from declarations plus exit data captures the ordering. Consumers are near symmetric by construction of their dual role. No directionality_overrides are authored: the derivation chain produces the right ordering from the structural declarations, and no two same-power agents require differentiation the atoms cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the framework as pure rope ignores the victim class and the legitimacy rents - the coordination story is real but not the whole story, and denying the extraction half would launder the lawful suffering residue. Reading it as pure snare ignores the genuine collective-action function (race-to-bottom prevention) and the documented suffering reductions the early statutes and standards delivered - overclaiming extraction would erase the reason the framework was built and the good it still does. The genealogy interview locates the mandatrophy question precisely: the founding problem (gratuitous nineteenth-century cruelty) has largely receded as stated, while the framework persists with its center of gravity shifted toward legitimation of scaled use - hence founding_problem_status 'contested' rather than 'dead', since the parties genuinely dispute whether today's enforcement targets are continuations of the founding problem or new problems the framework grew to cover. The mismatch consumer reads status x verdict; contested x world_rearranges correctly flags neither zombie nor healthy mandate but a live dispute about which the corpus should keep data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_epsilon,
    'This constraint is one reading (welfare_reading) of the animal_moral_status kernel; epsilon is authored for the standing regulated-use arrangement by this reading''s lights. What would the sibling readings change structurally?',
    'Compile the sibling stories (animal_moral_status__property_reading, animal_moral_status__abolitionist_reading) and compare per-seat classifications over the identical referent: the property reading authors epsilon near zero (no independent standing, nothing extracted from non-rightsholders), the abolitionist reading authors epsilon near one (all use violates rights-bearing individuals).',
    'Classification is indexical to the reading: the same statute-set computes as rope-flavored, tangled_rope, or snare depending on which reading''s victim set and premise structure is loaded. Cross-reading comparison is valid only at the referent level; merging epsilon across readings is a category error the corpus must not commit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_epsilon, conceptual, 'Committer structure: one of three readings of the animal moral status kernel; the disagreement lives in the standing question, not in empirical facts about suffering.').

omega_variable(
    residual_suffering_measurement,
    'How much suffering actually persists within lawful ''humane'' practice - the quantity this reading''s epsilon is a judgment about?',
    'Independent welfare-outcome auditing decoupled from certification schemes: behavioral indicator studies, stocking-density outcome data, transport and slaughter monitoring published outside the assurance industry.',
    'Lower measured residual suffering pushes the arrangement toward rope (coordination dominating); higher measured residue pushes toward snare (license purchase dominating). The current 0.55 is a reading-indexed judgment, not a settled measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_suffering_measurement, empirical, 'The empirical basis of the residual-suffering judgment underlying epsilon.').

omega_variable(
    legitimation_vs_protection_drift,
    'Is the framework''s dominant present function suffering-reduction or use-legitimation - does certification increase total consumption by relieving moral discomfort enough to offset the per-unit welfare gains?',
    'Consumption elasticity studies around certification labels; longitudinal accounting comparing aggregate animal-years lived under certified systems against welfare-gain per animal.',
    'If legitimation dominates, effective extraction is higher than the authored epsilon suggests (the framework enables more suffering than it prevents) and theater_ratio understates the dysfunction; if protection dominates, the rope-side reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimation_vs_protection_drift, empirical, 'Whether the certification layer nets out protective or enabling at the aggregate level.').

omega_variable(
    welfare_org_capture_direction,
    'Are welfare organizations independent guardians of the standards or captured co-administrators - does audit-revenue dependence and cooperative standard-setting bend their enforcement and code-position behavior?',
    'Funding-flow disclosure for major certification bodies; coding of organizational positions on standard-tightening proposals against industry positions; revolving-personnel tracking between certifiers and certified firms.',
    'Confirmed capture pulls the welfare-organization seat toward symmetric or target-adjacent directionality and strengthens a snare-flavored reading of the certification layer specifically; confirmed independence supports the tangled_rope claim as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_org_capture_direction, empirical, 'Direction and degree of capture in the certifier seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(anim_tr_t60, animal_moral_status__welfare_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(anim_be_t60, animal_moral_status__welfare_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__welfare_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(anim_su_t60, animal_moral_status__welfare_reading, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'animal moral status' decomposes into three structurally distinct constraints, one per reading of the kernel. All three share a single referent - the standing instrumental-use arrangement - but author different epsilon by their own lights: the property reading authors epsilon near zero (animals lack independent standing, so the arrangement extracts nothing from rightsholders-by-definition), this welfare reading authors epsilon at 0.55 (lawful residual suffering weighs), and the abolitionist reading authors epsilon near one (all use violates rights-bearing individuals regardless of care standards). Each story carries its own claimed_type, metrics, stakeholders, and axioms; none hedges across readings. Structural ordering: the welfare reading sits between its siblings - it inherits the property baseline (use is lawful) and generates the sentience-recognition pressure that progressively erodes that baseline, while the abolitionist reading rejects the welfare settlement outright. Sibling constraint_ids follow the kernel__reading convention assumed at generation; verify against sibling files at compile time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
