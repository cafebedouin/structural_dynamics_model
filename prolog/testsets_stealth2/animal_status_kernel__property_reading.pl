% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Animal Property Status - Ownership-Derivative Considerability Reading
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This story instantiates the property_reading of the animal_status_kernel:
 *   animals are chattel property, moral considerability derives wholly from
 *   ownership rights, and economic value is the only relevant measure. The
 *   referent arrangement under contest is the standing global regime in which
 *   owned animals' entire productive and bodily value - labor, offspring,
 *   milk, eggs, and the animals themselves - is appropriated by their
 *   titleholders, bounded only by anti-cruelty statutes that this reading
 *   understands as protecting asset value rather than animal interests. Per
 *   the kernel-reading rules, the file generates this one reading cleanly:
 *   the sibling readings (welfare_reading, abolitionist_reading) are separate
 *   constraints linked through the network block, and the committer structure
 *   is routed to omega variables. Claim and metrics are authored
 *   independently: the reading claims mountain - it presents animal property
 *   status as a civilizational constant rather than a policy choice
 *   (emerges_naturally true) - while the metrics describe near-total
 *   appropriation of the governed class, a legally closed category, and
 *   actively maintained boundaries. Because a mountain claim carries declared
 *   beneficiaries, the false-summit signature is armed and a dedicated omega
 *   documents the natural-law-versus-constructed ambiguity. The
 *   extractiveness value is authored in the reading's own economic frame:
 *   transfer of the governed class's value to titleholders is effectively
 *   complete, with reciprocity limited to upkeep that preserves asset worth.
 *   The reading's axioms nonetheless deny the governed class standing to be
 *   victimized, so the victim-set is deliberately empty - that emptiness is
 *   the reading's constitutive normative act, not a descriptive finding, and
 *   it is the principal structural delta separating this file from its
 *   siblings.
 *
 * KEY AGENTS:
 *   - property_law_institutions: agenda-setter (institutional/constrained) - defines and adjudicates title over animals
 *   - anti_cruelty_enforcement_agencies: enforcement administrator (institutional/constrained) - polices the asset-value floor
 *   - commercial_livestock_enterprises: primary beneficiary (powerful/arbitrage) - captures the bulk of appropriated output
 *   - animal_research_facilities: secondary beneficiary (organized/constrained) - consumes animals as experimental instruments
 *   - household_animal_owners: diffuse beneficiary (moderate/mobile) - holds everyday title
 *   - consumers_of_animal_products: demand-side beneficiary (organized/mobile) - funds the arrangement at checkout
 *   - veterinarians: dual-positioned participant (moderate/constrained) - paid by the frame, ethically burdened inside it
 *   - owned_animals: the governed class (powerless/trapped) - classified as non-parties by this reading; recorded with agent=false
 *   - animal_advocacy_movements: excluded voice (organized/constrained) - disputes the premise from outside the frame
 *   - moral_status_theorists: analytical observer (analytical/analytical) - maps the reading against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.6).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, mountain).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status - Ownership-Derivative Considerability Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).
domain_priors:emerges_naturally(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '0de00327-86c4-4408-8ec6-e400b6ac5712').
narrative_ontology:cs_kernel_codification('0de00327-86c4-4408-8ec6-e400b6ac5712', formalized).
narrative_ontology:cs_authority_grounding('0de00327-86c4-4408-8ec6-e400b6ac5712', lineage).
narrative_ontology:cs_interpretation_layer_present('0de00327-86c4-4408-8ec6-e400b6ac5712').
narrative_ontology:cs_reading_relation('0de00327-86c4-4408-8ec6-e400b6ac5712', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('0de00327-86c4-4408-8ec6-e400b6ac5712', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('0de00327-86c4-4408-8ec6-e400b6ac5712', foundational, economic_value_exclusive_measure).
narrative_ontology:cs_axiom_status(economic_value_exclusive_measure, holdable).
narrative_ontology:cs_axiom_grounding('0de00327-86c4-4408-8ec6-e400b6ac5712', economic_value_exclusive_measure, instrumental).
narrative_ontology:cs_axiom('0de00327-86c4-4408-8ec6-e400b6ac5712', foundational, ownership_derives_considerability).
narrative_ontology:cs_axiom_status(ownership_derives_considerability, holdable).
narrative_ontology:cs_axiom_grounding('0de00327-86c4-4408-8ec6-e400b6ac5712', ownership_derives_considerability, conventional).
narrative_ontology:cs_reference_frame('0de00327-86c4-4408-8ec6-e400b6ac5712', owner_sovereign_title_regime).
narrative_ontology:cs_drift_state('0de00327-86c4-4408-8ec6-e400b6ac5712', contemporary, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('0de00327-86c4-4408-8ec6-e400b6ac5712', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, commercial_livestock_enterprises).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_research_facilities).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, household_animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, veterinarians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, veterinarians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures define animals as chattel under property codes; courts adjudicate possession, sale, and use disputes under those headings. They maintain the registry, contract, and tort machinery that makes title over living animals tradable and inheritable. Amending the category is within their power but would unsettle credit, agricultural finance, and research instruments built on it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, property_law_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Inspect and prosecute under anti-cruelty and welfare statutes. The statutes they administer intervene where treatment would depress the animal's market value or shock public sensibility - starvation, torture, abandonment - and set housing and slaughter standards for commercially held animals. Caseload and penalty schedules are calibrated to commercial practice; they do not adjudicate whether the underlying ownership is itself permissible.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, anti_cruelty_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Breed, raise, and process the majority of owned animals. They set stocking densities, growth rates, breeding cycles, and slaughter timing to maximize margin, and capture the entire output - meat, milk, eggs, offspring, hides - as revenue. Capital moves freely across jurisdictions and species, so any jurisdiction tightening terms loses the operation to a looser one; this mobility disciplines regulation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, commercial_livestock_enterprises, beneficiary,
    powerful, biographical, arbitrage, global).

% Purchase, breed, and use animals as experimental instruments under institutional protocols. Funding pipelines, regulatory approvals, and scientific training all assume animal models; protocol review constrains procedure detail but never questions the owning-and-using frame itself. Substituting non-animal methods requires capital and validation timelines they do not control.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_research_facilities, beneficiary,
    organized, biographical, constrained, global).

% Keep companion and hobby animals under ordinary title: they decide diet, confinement, breeding, medical spend, and end-of-life, bounded mainly by anti-cruelty floors. They receive companionship, labor, or status from the animal and may sell, gift, or surrender it. Exit is easy in form - stop owning - and the arrangement costs them little.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, household_animal_owners, beneficiary,
    moderate, biographical, mobile, local).

% Buy meat, dairy, eggs, leather, and animal-tested products at prices made possible by the production economics above. Individual purchase decisions are trivially reversible; collective demand is the revenue signal that funds the enterprise seat. Most engage the arrangement only at the checkout.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, consumers_of_animal_products, beneficiary,
    organized, immediate, mobile, global).

% Earn their livelihood treating owned animals under owner consent: they may not treat without the owner's authorization and must euthanize or withhold care on owner instruction. Their training carries a healing ethic that collides daily with acting as the asset's maintenance contractor; licensing binds them to the owner-consent frame, and refusing it means leaving clinical practice.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, veterinarians, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, veterinarians, payer).

% The class the arrangement governs: bred on schedule, confined at set densities, moved, milked, bled, bred again, and killed when output declines or purpose ends. Every term of their lives is set by owner economics. The reading instantiated here classifies them as objects of title rather than parties, so they hold no seat in its proceedings; this entry records their position for completeness.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, owned_animals, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__property_reading, owned_animals).

% Organizations and campaigns that dispute the premise that ownership settles the moral question. They litigate personhood cases, run undercover documentation, and lobby for standing reforms. Inside the frame this story instantiates, their submissions register as sentiment or economic interference rather than as claims from a party; several jurisdictions have answered their documentation with disclosure bans rather than hearings.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_advocacy_movements, excluded,
    organized, generational, constrained, global).

% Philosophers and legal theorists who map the positions in the animal-status dispute - property, welfare, abolition - and test each for coherence. They hold no stake in the arrangement's revenues and can endorse, amend, or reject the ownership-derivative account on the merits.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, moral_status_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, commercial_livestock_enterprises).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unambiguous, enforceable title over living animals, making long-cycle investments secure: breeding programs span years, herds serve as loan collateral, and trade, inheritance, and insurance all price against registered ownership. The frame solves the coordination problem of who may use an animal and capture its outputs across millions of decentralized transactions.
% TRANSFER_FUNCTION: Moves the entire productive and bodily value of owned animals - labor, offspring, milk, eggs, wool, and the animals themselves - from the animals to their titleholders, valued solely in economic terms. It also moves moral consideration itself: any regard for an animal is routed through its owner's interests rather than accorded to the animal in its own right.
% ABSENT_VOICES: The governed animals themselves - the reading classifies them as objects of title, so the parties most affected hold no seat and their interests enter only as owner-reported costs. Advocacy organizations that would speak to the missing seat are received as sentiment or economic threat; in several jurisdictions their documentation is met with disclosure bans. They stand outside the frame this story instantiates, which is precisely where the sibling readings locate the dispute.
% DISAPPEARANCE_RATIONALE: Food supply, biomedical research, veterinary practice, and rural credit are all organized around titled animals. Overnight removal of the ownership frame would force simultaneous reconstruction of production, pricing, insurance, lending, and research methodology - the world rearranges massively rather than continuing as before.
% FOUNDING_PROBLEM: Secure, enforceable title over valuable animate assets: early agrarian economies needed unambiguous answers to who may use an animal, capture its output, breed it, and pass it on, so that multi-year breeding cycles, herd collateral, trade, and inheritance could be relied upon.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship on livestock title and agricultural-economics analyses of collateral and trade functions corroborate the founding problem from outside the benefiting parties; welfare-statute drafters likewise presuppose the ownership substrate they regulate. Animal advocacy organizations attest the problem's reality while disputing that it justifies the arrangement's present scope - corroboration of the problem, contestation of its sufficiency.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.88: measured in the arrangement's own accounting frame, the transfer is near-total - breeding, gestation, lactation, offspring, work, and slaughter timing are all set by owner margin, and the animal's residual claim is limited to upkeep that protects asset value. Suppression 0.60: the legal category is closed (an animal cannot be granted standing inside this frame; personhood filings fail for lack of a cognizable party), and deviation is policed where it threatens asset value or disclosure, but social exits - sanctuary keeping, refusal to purchase - remain open, so suppression is substantial without being total. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and spatial scope downstream. Theater ratio 0.28: humane-washing, welfare labeling, and ceremonial cruelty prosecutions perform concern while leaving the appropriation untouched, and the performative share grows with public scrutiny (see measurement series). Accessibility collapse 0.72: within the frame, alternatives collapse almost completely - every interaction with an owned animal routes through title, and no transactional alternative exists - but the category is legislatively revisable in principle, which keeps it below the near-total collapse of physical law. Resistance 0.40: sustained advocacy, personhood litigation, undercover documentation, and direct action meet the arrangement continuously; the response has been enforcement hardening rather than concession. The measurement series share one eight-point grid; all three tracked metrics rise monotonically over the industrialization interval, with no oscillation requiring cycle modeling. Rising base_extractiveness on a mountain claim is expected to trip the extraction-accumulation abductive trigger; that is the intended historical record, not a tuning artifact.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From property_law_institutions the arrangement is neutral infrastructure: title security enabling credit, trade, and inheritance. From commercial_livestock_enterprises it is the profit substrate itself. From household_animal_owners it is an unremarkable background category costing little. From veterinarians it is a daily collision between a healing ethic and owner-sovereign instruction - income from the frame, moral cost inside it. From animal_advocacy_movements the same structure is the injustice under protest. And owned_animals - which this reading classifies as objects of title rather than parties - hold no perceptual seat at all; that erasure is not an oversight in the data but the reading's own move, and it is exactly what the sibling readings contest. The engine computes per-seat classifications from the structural data; the authored mountain claim does not adjudicate among these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the enterprise, research, household-owner, consumer, and veterinarian seats near the beneficiary end of the directionality range: each collects something from the arrangement without bearing its costs in any comparable measure. Veterinarians are the one genuinely dual-positioned seat (beneficiary income, payer-like ethical and professional cost), flagged via secondary_role rather than a directionality override - an override keyed to their power atom would also sweep household owners, whose position is unambiguously beneficiary-side. No victims are declared: the reading's constitutive axiom denies the governed class standing, so the target side of the derivation is intentionally vacant, and owned_animals is recorded with agent=false so the class contributes no directionality term. This is the structural signature of the property reading - high authored extraction over an emptied victim-set - and the principal axis along which the welfare reading (partial victim-set restored) and the abolitionist reading (full standing) diverge. Global spatial scope amplifies effective extraction through verification difficulty; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - secure, enforceable title over animate assets so that multi-year breeding cycles, herd collateral, trade, and inheritance can be priced - remains live and is performed at scale, so the mandate has not outlived its function and mandatrophy is not resolved. The classification hazard runs in both directions. Reading the arrangement as pure extraction would erase the genuine title-coordination service that billions of transactions still rely on; accepting the mountain claim at face value would naturalize a constructed category that concentrates enormous value on identifiable beneficiary seats. The false-summit checkpoint is the designed arbiter of that second hazard: beneficiaries are declared, the naturality assertion is flagged, and the omega variable records what evidence would settle the question. The engine, not the claim, decides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_status,
    'Is the property status of animals a natural feature of settled human society, or a constructed legal-moral arrangement whose persistence serves identifiable beneficiaries?',
    'Comparative historical and anthropological analysis of animal-relations regimes (sacred-animal traditions, commons custody arrangements, recent limited personhood grants) together with legislative-lobby tracing of who funds the category''s defense.',
    'If constructed, the mountain claim fails and the false-summit signature reclassifies toward a hybrid coordination/extraction reading; if natural, the claim survives with the declared beneficiaries explained as incidental rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_status, empirical, 'Natural-law versus constructed-origin ambiguity of animal property status (false-summit checkpoint).').

omega_variable(
    kernel_reading_contest_location,
    'This story is one reading (property_reading) of the animal_status_kernel; what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Authoring the sibling stories (welfare_reading, abolitionist_reading) against the same referent arrangement and comparing victim-set declarations, sources of considerability, and computed per-seat types.',
    'welfare_reading restores a partial victim-set (sentient sufferers) and constrains use; abolitionist_reading removes property status outright and makes all use impermissible. Either switch rewrites beneficiaries, victims, and classification wholesale while the referent arrangement stays fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: which kernel, which reading, what the siblings would change.').

omega_variable(
    cruelty_statute_function,
    'Do anti-cruelty statutes protect animal interests or owner property value?',
    'Statute-design analysis (whose consent triggers prosecution, how penalties calibrate to asset depreciation) plus enforcement-outcome data across jurisdictions.',
    'If statutes track animal interests, the reading''s no-countervailing-constraint premise weakens and effective extraction falls below the authored profile; if they track asset value, extraction stands near-total and the reading''s self-description is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cruelty_statute_function, empirical, 'Function of the anti-cruelty layer inside the property frame.').

omega_variable(
    suppression_consensus_vs_enforcement,
    'Is the arrangement''s stability carried by internalized acceptance of the property frame or by active enforcement against deviation?',
    'Compare jurisdictions and periods differing in enforcement intensity (disclosure-ban adoption, biosecurity policing) while holding public-sentiment surveys constant; examine post-repeal trajectories where disclosure bans lapse.',
    'If enforcement-carried, suppression is structural and responsive to legal change; if consensus-carried, suppression persists after enforcement relaxes and the arrangement is more entrenched than its enforcement budget suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_consensus_vs_enforcement, empirical, 'Structural versus internalized maintenance of the property frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__property_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__property_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status_kernel__property_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(anim_tr_t60, observed).
narrative_ontology:measurement(anim_tr_t70, animal_status_kernel__property_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(anim_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__property_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__property_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_status_kernel__property_reading, base_extractiveness, 60, 0.87).
narrative_ontology:measurement_basis(anim_be_t60, observed).
narrative_ontology:measurement(anim_be_t70, animal_status_kernel__property_reading, base_extractiveness, 70, 0.88).
narrative_ontology:measurement_basis(anim_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__property_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__property_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_status_kernel__property_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(anim_su_t60, observed).
narrative_ontology:measurement(anim_su_t70, animal_status_kernel__property_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(anim_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the moral status of animals' decomposes into three structurally distinct constraints sharing one kernel: this property reading (victim-set empty by axiom, considerability ownership-derivative, extractiveness authored high as total appropriation in the reading's own economic frame), the welfare reading (partial victim-set of sentient sufferers, use constrained by welfare obligations), and the abolitionist reading (animals as rights-holders, property status itself the violation). Same referent arrangement, reading-indexed epsilon per OQ-26; each story links the other two here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
