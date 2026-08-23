% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status — Ownership-Derived Considerability Reading
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   The standing arrangement is the comprehensive legal classification of
 *   nonhuman animals as chattel property across agriculture, research,
 *   entertainment, and companionship: considerability is channeled
 *   exclusively through ownership, valuation proceeds solely in economic
 *   terms, and anti-cruelty statutes function as protection of owner asset
 *   value rather than as representation of animal interests. This file
 *   instantiates ONE reading of the animal_status_kernel — the
 *   property_reading — and is ε-invariant within itself: one referent (the
 *   standing instrumental-use arrangement), one ε, one beneficiary/victim
 *   structure. The sibling readings (welfare_reading, abolitionist_reading)
 *   are separate constraints in separate files, linked via
 *   network.affects_constraints; they are not described, hedged, or averaged
 *   here. KEY AGENTS (by structural relationship): - animal_property_owners:
 *   Primary collector (powerful/arbitrage) — receives all transferred value -
 *   animal_agribusiness_industry: Institutional collector and enforcement
 *   sponsor (institutional/arbitrage) - state_legislatures_and_courts: Agenda
 *   setter administering the classification (institutional/constrained) -
 *   slaughterhouse_workers: Concentrated human cost-bearer at the execution
 *   interface (powerless/constrained) -
 *   rural_neighbors_of_intensive_operations: Externalized-cost bearer
 *   (moderate/constrained) - meat_consumers: Incidental beneficiary, partial
 *   indirect payer (moderate/constrained) - farmed_animals: The governed
 *   population itself — seated as EXCLUDED, not as payer, which is the
 *   reading's constitutive move (powerless/trapped) -
 *   animal_advocacy_movement: Contesting party denied standing
 *   (moderate/identity_locked) - legal_philosophy_community: Analytical
 *   observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - animal_property_owners: Primary collector (powerful/arbitrage) — receives everything the arrangement transfers
 *   - animal_agribusiness_industry: Institutional collector and enforcement sponsor (institutional/arbitrage) — funds the classification's legislative defense
 *   - state_legislatures_and_courts: Agenda setter (institutional/constrained) — administers and adjudicates the classification
 *   - slaughterhouse_workers: Concentrated human cost-bearer (powerless/constrained) — performs and absorbs the execution costs
 *   - rural_neighbors_of_intensive_operations: Externalized-cost bearer (moderate/constrained) — absorbs environmental and property-value losses
 *   - meat_consumers: Incidental beneficiary and indirect payer (moderate/constrained) — receives cheap products, carries diffuse externalities
 *   - farmed_animals: The governed population, seated as EXCLUDED (powerless/trapped) — the reading's defining act is leaving them off the payer ledger
 *   - animal_advocacy_movement: Standing-denied contestant (moderate/identity_locked) — organizes the resistance the classification absorbs
 *   - legal_philosophy_community: Analytical observer (analytical/analytical) — articulates the rival positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.74).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, mountain).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status — Ownership-Derived Considerability Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).
domain_priors:emerges_naturally(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '9f9741ef-586e-4b74-b391-679e960ba735').
narrative_ontology:cs_kernel_codification('9f9741ef-586e-4b74-b391-679e960ba735', formalized).
narrative_ontology:cs_authority_grounding('9f9741ef-586e-4b74-b391-679e960ba735', extraction).
narrative_ontology:cs_interpretation_layer_present('9f9741ef-586e-4b74-b391-679e960ba735').
narrative_ontology:cs_reading_relation('9f9741ef-586e-4b74-b391-679e960ba735', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('9f9741ef-586e-4b74-b391-679e960ba735', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('9f9741ef-586e-4b74-b391-679e960ba735', foundational, considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('9f9741ef-586e-4b74-b391-679e960ba735', considerability_derives_from_ownership, conventional).
narrative_ontology:cs_axiom('9f9741ef-586e-4b74-b391-679e960ba735', foundational, economic_value_exhaustive_of_relevant_value).
narrative_ontology:cs_axiom_status(economic_value_exhaustive_of_relevant_value, holdable).
narrative_ontology:cs_axiom_grounding('9f9741ef-586e-4b74-b391-679e960ba735', economic_value_exhaustive_of_relevant_value, instrumental).
narrative_ontology:cs_axiom('9f9741ef-586e-4b74-b391-679e960ba735', secondary, anticruelty_statute_protects_owner_asset_value).
narrative_ontology:cs_axiom_status(anticruelty_statute_protects_owner_asset_value, holdable).
narrative_ontology:cs_axiom_grounding('9f9741ef-586e-4b74-b391-679e960ba735', anticruelty_statute_protects_owner_asset_value, conventional).
narrative_ontology:cs_reference_frame('9f9741ef-586e-4b74-b391-679e960ba735', common_law_chattel_baseline).
narrative_ontology:cs_drift_state('9f9741ef-586e-4b74-b391-679e960ba735', contemporary_animal_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f9741ef-586e-4b74-b391-679e960ba735', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_property_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_agribusiness_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, meat_consumers).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, slaughterhouse_workers).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, rural_neighbors_of_intensive_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, meat_consumers).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, owner_sovereignty_over_chattels).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, market_price_as_exclusive_valuation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own animals as titled assets. Collect everything the animals produce — meat, milk, eggs, fiber, offspring, labor — and decide unilaterally what happens to the animals' bodies, reproduction, and deaths. Can sell, liquidate, switch species, relocate operations, or convert holdings into other assets; nothing about the arrangement attaches to them personally. Their costs are ordinary input costs, priced and recoverable.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_property_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Operates vertically integrated production, processing, and retail across the animal-product economy. Funds legislative defense of the property classification, circulates model statutes (including measures restricting undercover documentation of facility conditions), and supplies the economic expertise that hearings rely on. Collects processing margins layered on top of owner returns; diversified enough to shift capital between species, regions, and product lines.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_agribusiness_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, animal_agribusiness_industry, agenda_setter).

% Maintain and adjudicate the classification: statutes define animals as personal property, courts process ownership disputes, award damages at market price, and hear cruelty prosecutions brought by owners or the state rather than by any representative of the animals. Abandoning the classification would require rebuilding the asset-registration and secured-lending framework built on it; institutional continuity depends on treating the classification as settled.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, state_legislatures_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Perform the killing the arrangement requires at industrial line speeds. Carry elevated rates of physical injury and documented psychological harm; wages sit near local floors because labor supply is captive to rural geographies. Leaving means unemployment in thin local markets; internal reporting of conditions invites dismissal.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, slaughterhouse_workers, payer,
    powerless, immediate, constrained, regional).

% Live beside concentrated facilities: airborne emissions, water contamination, property-value decline, unresolved health complaints. Compensation flows through nuisance frameworks that price harm at market rates or deny relief outright under right-to-farm statutes. Organizing draws retaliatory use of the same statutes that shield facility operations; relocation is possible but costly and leaves others exposed.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, rural_neighbors_of_intensive_operations, payer,
    moderate, biographical, constrained, regional).

% Receive abundant, inexpensive animal products made possible by the arrangement's cost structure, and pay indirectly through dietary-health burdens and environmental externalities not reflected at purchase. Habit, culture, price, and food infrastructure bind consumption patterns; individual exit through dietary change is possible but socially frictional and leaves the production system untouched.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, meat_consumers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, meat_consumers, payer).

% Are the population the arrangement allocates: bred by selection programs, confined by housing systems, transported, and slaughtered on schedules set by owners. Hold no capacity to appear before any court, agency, or market — no standing, no representation, no forum accepts their presence. Their interests affect outcomes only where an owner's economic interest or a statute protecting owner asset value happens to coincide with them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, farmed_animals, excluded,
    powerless, biographical, trapped, global).

% Organizations and volunteers who contest the classification through litigation, ballot initiatives, investigation and publication, and consumer campaigns. Denied standing to represent the animals they describe; documentation efforts meet statutory countermeasures; litigation reaches settlements that leave the classification intact. Commitment is career-defining for core members, whose professional and communal identities are built around the contest.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_advocacy_movement, excluded,
    moderate, generational, identity_locked, global).

% Scholars of law and moral philosophy who map the classification's history, compare jurisdictions, and articulate the rival positions on animal status. Neither collects nor pays under the arrangement; their publications supply the vocabulary in which the competing positions on animal standing are argued.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_philosophy_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, animal_property_owners).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the asset-governance problem for commercially valuable living beings: establishes clear, transferable, collateralizable title over animals, resolves possession disputes, and standardizes the commercial interface between humans and the animals they use.
% TRANSFER_FUNCTION: Moves the full productive output of animal bodies — meat, milk, eggs, fiber, labor, offspring — together with the animals' bodily liberty and lives themselves, from the animals to their registered owners; distributes the arrangement's externalized costs onto processing workers and neighboring communities; and reserves all decision-making standing to owners.
% ABSENT_VOICES: The governed class itself. The animals whose bodies and output the arrangement allocates are structurally unable to appear in any forum: they hold no standing, cannot retain counsel, and are 'represented' only by parties with opposite interests — their owners. Under either sibling reading they would be the primary speakers in this conversation. Secondary absences: processing workers in regulatory consultations dominated by industry representation.
% DISAPPEARANCE_RATIONALE: Overnight removal would force wholesale legal reclassification of every farm, laboratory, zoo, and companion animal; credit markets built on herd collateral would freeze; protein supply chains would halt pending new legal categories; and millions of dependent enterprises would fail or transform. Few arrangements short of money itself are more load-bearing.
% FOUNDING_PROBLEM: Settled agrarian economies needed secure, enforceable, transferable claims over high-value mobile assets that could not speak or contract; classifying animals as chattels gave lenders collateral, gave herders defensible possession, and folded living assets into the general property system.
% FOUNDING_PROBLEM_CORROBORATION: Commercial lending practice (herd collateral under secured-transactions law) and legal-historical scholarship attest that the ownership-security function is real and ongoing. By contrast, the moral rider — that considerability itself derives from ownership and that economic value exhausts relevance — is attested by no source outside the benefiting parties: industry submissions and owner associations assert it, and no independent legal-historical or philosophical source corroborates it. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status_kernel__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status_kernel__property_reading),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is mountain because THIS reading presents the classification as natural order — persons own things, animals are things, economic value is the measure of things — a summit assertion, not a description; the false-summit signature exists precisely to test such claims when identifiable beneficiaries are present, and beneficiaries are declared here. The metrics describe the arrangement's actual operation: extractiveness 0.88 because the reading's own axioms strip out every countervailing consideration (no welfare floor, no interest-balancing, no non-economic value), so the measured transfer is total and undamped — the manifest's expected delta ('extractiveness high because no countervailing moral constraint on use'). Suppression 0.74 reflects the enforcement architecture: standing denials, documentation bans, right-to-farm shields, and enforcement asymmetry between activists and standard practices. Theater 0.44 is the thickening performative layer — anti-cruelty prosecutions staged as moral solicitude while functionally policing asset damage, and humane labeling priced as market premium — sitting atop a still-real title-and-collateral function, hence well below piton range. Accessibility_collapse 0.62: within the frame, alternatives collapse completely for the governed class (no forum, no standing, anywhere), but the frame itself faces live rivals in sibling jurisdictions and discourses, holding the composite below natural-law levels. Resistance 0.58 is organized, funded, and persistent yet so far institutionally contained. The temporal series run on ONE shared grid (t = 0,15,30,45,60,75 for all three tracked metrics); the suppression_requirement series is authored because this story specifically traces enforcement-capacity build-out — standing doctrine hardening, documentation bans spreading, line speeds rising — not mere extraction drift; a flat-suppression alternative would have dropped the series, not misaligned the grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the owner seat the arrangement is ordinary commerce: assets, inputs, outputs, arbitrage-grade exit — effective extraction damps toward subsidy. From the worker seat it is concentrated bodily and psychological cost with constrained exit — amplified. From the neighbor seat it is uncompensated externality priced at market rates or denied relief outright. The consumer seat nets a genuine benefit against diffuse unpaid costs. The farmed-animal seat computes NOTHING — no directionality, no chi — because the reading grants it no seat; the engine's inability to amplify extraction onto the principal cost-bearing population is not an oversight but the reading working exactly as designed, and the undercount relative to the sibling files is quantified only by cross-file comparison (see omega victim_set_boundary_fiat).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: owners and industry sit at the beneficiary pole (low d, further pushed by arbitrage-grade exit); consumers sit near-symmetric (real benefit, diffuse indirect payment). Victim declarations seat the workers and neighbors near the target pole. Farmed animals carry NO directionality value in this file: they appear only in the excluded role, which is commentary-grade and feeds no derivation — the deliberate structural signature of this reading, recorded as an omega rather than corrected by an override. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct d for every seated agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — secure, transferable claims over high-value living assets — is live: industrial animal agriculture still runs on collateralized herd finance and title registration, so this is not a mandate outliving its function and mandatrophy_resolved is not declared. But the arrangement persists on a live coordination core PLUS a moral rider — considerability-from-ownership, economic-value-exhaustive — that no corroborating source outside the benefiting parties attests. The classification apparatus prevents mislabeling in both directions: the FSM path forces the reading's naturalness claim through testing rather than accepting its self-description, while the live founding problem prevents the whole arrangement from being written off as pure extraction with no coordination content. Watch item: if cultivated-protein substitution decouples the security function from animal assets, the coordination core strands first and the residual becomes theatrical maintenance — the rising theater series is the early indicator of exactly that trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_property_status,
    'Is the classification of animals as property a structural feature of the moral and legal universe — a natural-order fact this reading merely recognizes — or a constructed allocation whose ''naturality'' is asserted by identifiable beneficiaries?',
    'Cross-jurisdiction comparison: jurisdictions that have amended constitutions or statutes to recognize animal sentience (Germany''s dignity clause, New Zealand''s sentience amendments, rights-of-nature provisions applied to species) demonstrate the classification moving with enactment rather than with discovery. If the ''fact'' tracks legislation, it is constructed.',
    'Resolves the false-summit question: a constructed status converts the mountain claim into an enforced allocation with identifiable beneficiaries, routing the story through the FSM override path rather than certifying natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_property_status, empirical, 'Whether animal property status is discovered structure or enacted allocation.').

omega_variable(
    victim_set_boundary_fiat,
    'Does this reading''s exclusion of animals from the victim set measure a genuine absence of harmed parties, or does it manufacture the absence by definitional fiat?',
    'Kernel-family comparison: seat the same biological population as payers in the sibling files and diff the computed classifications. If extraction appears only when the boundary moves, the boundary — not the arrangement — carried the result.',
    'High: the entire victim structure of this story rests on that boundary. Resolving it determines whether the arrangement contains a class of cost-bearers this reading cannot register, and whether the computed per-seat picture systematically understates total extraction relative to the sibling files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_boundary_fiat, conceptual, 'Whether the empty animal side of the ledger is finding or artifact of the reading''s own definitions.').

omega_variable(
    epsilon_referent_sharing,
    'This reading authors its extractiveness over the same standing arrangement as the welfare and abolitionist readings; what exactly differs across the three files — the referent, the counting, or the endorsement?',
    'Structural audit of the kernel family: confirm the referent is identical (the standing instrumental-use arrangement) and locate the divergence in what each reading counts as cost and whom it seats. The disagreement lives in the victim-set boundary and the value-monism axiom, not in any empirical fact.',
    'Confirms reading-indexed epsilon over a fixed referent: cross-reading value differences are principled structural data, enabling family-level meta-analysis rather than averaging or reconciliation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_sharing, conceptual, 'Location of the inter-reading disagreement: referent versus counting versus endorsement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of alternatives to property status structural (legal standing barriers, documentation bans, enforcement asymmetry) or internalized (population-wide absorption of the frame as common sense)?',
    'Post-reform trajectory: in jurisdictions that adopt sentience recognition, track whether challenge behavior rises once structural barriers drop. Persistent quiescence after barrier removal indicates a substantial internalized component.',
    'If substantially internalized, repealing structural barriers under-delivers: the effective suppression the arrangement enjoys exceeds the structural measure authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between legal architecture and absorbed common sense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status_kernel__property_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t45, animal_status_kernel__property_reading, theater_ratio, 45, 0.36).
narrative_ontology:measurement_basis(anim_tr_t45, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status_kernel__property_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(anim_tr_t60, observed).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.44).
narrative_ontology:measurement_basis(anim_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t15, animal_status_kernel__property_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t45, animal_status_kernel__property_reading, base_extractiveness, 45, 0.81).
narrative_ontology:measurement_basis(anim_be_t45, observed).
narrative_ontology:measurement(anim_be_t60, animal_status_kernel__property_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement_basis(anim_be_t60, observed).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.88).
narrative_ontology:measurement_basis(anim_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t15, animal_status_kernel__property_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t45, animal_status_kernel__property_reading, suppression_requirement, 45, 0.67).
narrative_ontology:measurement_basis(anim_su_t45, observed).
narrative_ontology:measurement(anim_su_t60, animal_status_kernel__property_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(anim_su_t60, observed).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.74).
narrative_ontology:measurement_basis(anim_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the status of animals.' The single natural-language concept decomposes into three ε-stable readings sharing one referent (the standing instrumental-use arrangement) with divergent reading-indexed ε: property_reading (this file, ε 0.88, undamped because its axioms remove every countervailing consideration), welfare_reading (intermediate ε: welfare duties partially constrain and partially legitimize the same transfers), abolitionist_reading (highest ε: the entire transfer structure is categorically illegitimate). Upstream/downstream structure: this reading is the historical substrate — welfare statutes are drafted as regulations OF property, inheriting its categories — so it structurally influences the welfare reading's drafting form while logically foreclosing its premise; the abolitionist reading is defined reactively against both. Family links are declared bidirectionally via affects_constraints; cross-reading ε differences are routed to omega variables, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
