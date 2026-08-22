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
 *   human_readable: Instrumental-Use Regime over Animals (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global regime in which
 *   animals are held as legal property and used instrumentally — for food,
 *   fiber, research, and entertainment — under a lattice of property law,
 *   subsidy structures, and welfare statutes. This file instantiates ONE
 *   reading of the contested animal_status kernel: the abolitionist reading,
 *   on which animals are rights-holders with inherent value and no
 *   instrumental use is permissible. Per the kernel-reading epsilon rule, the
 *   epsilon authored here is for the STANDING ARRANGEMENT as the abolitionist
 *   reading assesses it — not for the rights-respecting arrangement the
 *   reading would install, which would score near zero by construction. On
 *   this reading the welfare-regulation layer functions as legitimation
 *   rather than protection: standards largely codify industry practice, and
 *   the enforcement machinery defends the use relationship itself
 *   (facility-access restrictions, property recovery of rescued animals).
 *   This story is one member of a three-story constraint family; the
 *   welfare_reading and property_reading siblings are separate files with
 *   their own victim sets, epsilon values, and classifications, linked
 *   through network.affects_constraints. The claim/metric gap is deliberate:
 *   claimed_type states this reading's structural verdict; the metrics state
 *   what the arrangement's operation looks like from that seat; the engine
 *   computes per-seat classifications independently.
 *
 * KEY AGENTS:
 *   - - farmed_animals: Primary target (powerless/trapped) — bears the arrangement's full costs with zero exit at any life stage
 *   - - laboratory_animals: Secondary target (powerless/trapped) — protocol-determined conditions, no representative with veto power
 *   - - animal_agribusiness: Primary beneficiary and de facto rule-setter (powerful/arbitrage) — collects product revenue and subsidies; capital can pivot
 *   - - biomedical_research_establishment: Secondary beneficiary (institutional/constrained) — locked in by validation-pathway dependence on animal data
 *   - - animal_product_consumers: Diffuse beneficiary carrying indirect costs (moderate/mobile) — individually mobile exit, socially frictional
 *   - - livestock_policy_apparatus: Administrator (institutional/constrained) — writes and enforces the rules under structural industry dependence
 *   - - animal_advocacy_organizations: Excluded challenger (organized/constrained) — voice outside the renewal rooms; evidence-gathering restricted
 *   - - animal_ethics_theorists: Analytical observer (analytical/analytical) — articulates the inherent-value argument; no material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.93).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Instrumental-Use Regime over Animals (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '31ee325a-7566-4b41-a08f-f54bb85ff6b6').
narrative_ontology:cs_kernel_codification('31ee325a-7566-4b41-a08f-f54bb85ff6b6', distributed).
narrative_ontology:cs_authority_grounding('31ee325a-7566-4b41-a08f-f54bb85ff6b6', distributed).
narrative_ontology:cs_reading_relation('31ee325a-7566-4b41-a08f-f54bb85ff6b6', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('31ee325a-7566-4b41-a08f-f54bb85ff6b6', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('31ee325a-7566-4b41-a08f-f54bb85ff6b6', foundational, sentience_suffices_for_inherent_value).
narrative_ontology:cs_axiom_status(sentience_suffices_for_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('31ee325a-7566-4b41-a08f-f54bb85ff6b6', sentience_suffices_for_inherent_value, deontological).
narrative_ontology:cs_axiom('31ee325a-7566-4b41-a08f-f54bb85ff6b6', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('31ee325a-7566-4b41-a08f-f54bb85ff6b6', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_reference_frame('31ee325a-7566-4b41-a08f-f54bb85ff6b6', equal_inherent_value_regime).
narrative_ontology:cs_drift_state('31ee325a-7566-4b41-a08f-f54bb85ff6b6', contemporary_global_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('31ee325a-7566-4b41-a08f-f54bb85ff6b6', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agribusiness).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_product_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, housed, transported, and killed on schedules set entirely by titleholders. Their labor, reproductive output, bodies, and lives are the goods the arrangement delivers. There is no point in their lives at which leaving is possible: escape is physically prevented, and recovery of escaped animals is treated as lawful retrieval of property.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Are bred to order for experimental protocols. Exposure conditions, procedures, and endpoints are fixed by approval chains in which they have no participant and no representative with veto power. Removal from a protocol occurs only when the protocol ends or the animal is no longer useful to it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Owns and processes the large majority of farmed animals and collects the product revenue and a substantial share of public subsidy flows. Shapes the operative rules through lobbying, structural information dependence, and legislative access. Capital is diversified enough to pivot toward non-animal protein ventures if margins shift, so exit from the arrangement is available in ways it is not for the animals.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agribusiness, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_agribusiness, agenda_setter).

% Depends on animal models that regulatory validation pathways require before human trials. Collects research funding, publication output, and patentable findings built on animal data. Exit is limited because replacing animal data with validated alternatives requires regulatory acceptance that the establishment itself must petition for.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, constrained, global).

% Receive inexpensive, abundant animal products sustained by subsidy and established food infrastructure. Carry indirect costs: health burdens associated with high consumption, environmental externalities, and — on the moral accounting this reading applies — the cost of participating in the arrangement. Individually, changing diet is feasible; socially, habit, price signals, and food environments create friction.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_product_consumers, payer).

% Legislatures, agriculture departments, and inspection bodies that write and administer the property and welfare statutes composing the arrangement. Structurally dependent on the regulated industry for technical information, personnel circulation, and political support, which narrows the range of rules it will entertain. Administers enforcement, including statutes restricting investigation of facilities.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, livestock_policy_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Campaign for legal-personhood reforms and for ending use categories outright. Hold voice in public discourse and courts but not in the agricultural and research committees where the arrangement is periodically renewed. Operate through investigation, litigation, ballot initiatives, and market pressure; facility-access restrictions raise the cost of their evidence-gathering.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Philosophers and legal theorists who articulate the inherent-value argument and audit the arrangement's moral structure. Hold no material stake in its continuation or removal; their contribution is argument, genealogy, and critique.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_ethics_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_agribusiness).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes animals as ownable, transferable, financeable assets and coordinates mass provision of animal-derived goods: property and contract law secure title and collateral; integrated breeding, housing, transport, and processing infrastructure delivers predictable volumes to retailers; welfare statutes supply uniform operating rules across jurisdictions; breeding colonies supply standardized research material to laboratories.
% TRANSFER_FUNCTION: Moves animals' labor, reproductive output, bodies, and lives to titleholders and onward to purchasers of animal products; moves public funds to producers through subsidies; moves research material from breeding facilities to laboratories; moves decision-making authority over animals' conditions entirely to human titleholders and administrators.
% ABSENT_VOICES: The animals whose status the arrangement allocates cannot appear in any forum that decides it; their interests enter only through proxies, and the proxy bodies (welfare councils, institutional ethics committees) are staffed predominantly by permitted users of animals. Advocacy organizations are present in public discourse but absent from the legislative and funding rooms where the arrangement is renewed.
% DISAPPEARANCE_RATIONALE: Global food systems, medical research pipelines, rural land use, and commodity trade would reorganize around non-animal protein and non-animal research methods; breeding cycles covering tens of billions of animals would cease; property law would shed an entire asset class; subsidy and insurance structures built around livestock would unwind.
% FOUNDING_PROBLEM: Before synthetic alternatives existed, domesticated animals were the dependable source of protein, fiber, motive power, and research material, and owners needed enforceable title over valuable, mobile, living assets.
% FOUNDING_PROBLEM_CORROBORATION: FAO food-security statistics and nutrition science — sources outside the benefiting parties — corroborate that the underlying provisioning problem was and is real. Independent lifecycle-assessment and food-technology literature attests that non-animal routes increasingly meet the same needs. No source outside the benefiting parties attests that instrumental use of animals specifically remains necessary, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.93 because the referent is the standing arrangement as this reading assesses it: the entirety of animals' labor, reproductive capacity, bodies, and lives is appropriated, with the endpoint (killing at a small fraction of natural lifespan) unchanged across all use categories. Suppression is 0.88 as a raw, unscaled structural property: physical confinement, breeding into dependency, legal property status, and enforcement directed at rescuers and investigators. Theater_ratio is 0.62: the welfare apparatus — audits, humane labels, welfare statutes — performs protection while, on this reading, codifying practices that would be prosecuted if performed on companion animals; the extraction machinery itself is brutally functional, which caps the ratio below piton territory. Accessibility_collapse is 0.45: plant-based and cultivated alternatives persist and are growing, so understanding the arrangement does not collapse the alternative set — the collapse is partial and enforced at the margins (subsidy asymmetry, procurement rules). Resistance is 0.50: an organized advocacy sector, litigation, ballot measures, and dietary defection are real but historically outmatched by the concentration of beneficiary power. The temporal series run on one shared grid (interval 0-80 maps to roughly 1945-2025): intensification (confinement systems, genetic selection, throughput) drives base_extractiveness upward; the legitimation layer expands faster than protection, driving theater_ratio past 0.5 (Goodhart drift); and the enforcement ratchet — inspection bureaucracies maturing into facility-access restrictions and investigation statutes — drives suppression_requirement upward. All three trajectories are authored at every shared time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute radically different types from identical structural data. From the farmed_animals and laboratory_animals seats the arrangement is total appropriation with zero exit — nothing resembling coordination is experienced from inside it. From the animal_agribusiness seat the same structure is ordinary commerce it helped design, with arbitrage-grade exit that further insulates it. From the livestock_policy_apparatus seat it is administration of a lawful asset class. From the consumer seat it is a background provisioning system with frictions. The engine computes these per-seat classifications from power, exit, and declared position; this story's claimed_type records only the authoring seat's structural verdict and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: farmed_animals and laboratory_animals sit at the full-target end (d near 1.0), amplified by trapped exit — no arbitrage, no mobility, identity of the arrangement with their entire existence. animal_agribusiness sits near the beneficiary end (d near 0.0), pushed further by arbitrage-grade exit. biomedical_research_establishment is beneficiary-positioned but its constrained exit keeps it from the extreme. animal_product_consumers derive low d from their beneficiary role with an upward drag from the secondary payer position and the indirect costs they carry. livestock_policy_apparatus derives a mid-range d: formally an administrator, structurally dependent on the regulated industry, which the derivation reads as partial alignment with the beneficiary pole. The excluded and observer seats contribute to the absent-voices record rather than to chi. No directionality overrides are authored: the structural data (roles, power atoms, exit options) is sufficient, and the coarse per-power-atom override surface would misapply across seats sharing an atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — provisioning protein, fiber, and research material from animals before alternatives existed, and securing title over living assets — is answered by alternatives the arrangement's own beneficiaries have incentives to slow-walk. The R5 mismatch consumer reads founding_problem_status=contested together with disappearance_verdict=world_rearranges: the world depends on the arrangement, the problem it was built for is disputed as solved, and the arrangement persists — the zombie/capture configuration the mismatch flag exists to catch. mandatrophy_resolved is authored true as a reading-indexed judgment: on this reading the mandate has been overtaken by substitutable technology. Classification discipline prevents the inverse error as well: a welfare-seat analysis of the same arrangement would find genuine coordination (uniform operating rules, inspection infrastructure) and compute rope or tangled_rope; this story's structural data — victims with zero exit, a legitimation layer whose share of activity rises past half, enforcement aimed at investigators rather than at harm — is what distinguishes the snare verdict from the coordination verdict. The engine owns that computation; the divergence between seats and between readings is the datum the corpus exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_status_kernel_reading_indexicality,
    'This story instantiates only the abolitionist reading of the animal_status kernel; which reading governs a jurisdiction''s actual arrangement changes the victim set and epsilon wholesale — what exactly would each sibling reading alter?',
    'Comparative generation and cross-reading divergence analysis of the sibling stories (animal_status__welfare_reading, animal_status__property_reading), joined against which reading each jurisdiction''s law and court practice actually encodes.',
    'Under the property_reading, animals exit the victim set entirely and epsilon collapses toward human-side costs only (roughly 0.2); under the welfare_reading, epsilon drops to the gap between statutory floors and sentient-interest thresholds (roughly 0.5); the engine-computed foreclosure edges among the three readings differ accordingly. This file''s high epsilon is valid only under the abolitionist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_status_kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings; this file is the abolitionist instantiation and its classification is indexical to that reading.').

omega_variable(
    welfare_layer_function,
    'Is the welfare-regulation layer pure legitimation sustaining the use relationship, or does it deliver net suffering reduction that would survive scrutiny from outside the benefiting parties?',
    'Controlled comparison of outcomes across jurisdictions and periods with and without binding welfare mandates; audit of whether adopted standards track prevailing industry practice or exceed it; injury and welfare-outcome data gathered by parties with no stake in continued use.',
    'If the layer is legitimation, the snare verdict holds and welfare reform is counterproductive by the reading''s own lights; if it delivers real, durable reduction, the arrangement carries a genuine coordination component the reading discounts, moving the computed type toward tangled_rope even under this seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_layer_function, empirical, 'Whether the welfare apparatus constrains harm or merely licenses it.').

omega_variable(
    consumer_suppression_mechanism,
    'Is continued consumer participation driven by structural dependency (price, availability, food environments, subsidy-shaped markets) or by internalized rationalization that persists after structural barriers fall?',
    'Post-barrier trajectory: purchasing behavior in markets where plant-based and cultivated alternatives reach price and quality parity; if consumption persists at parity, the residual is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure — participants carry the arrangement''s pull with them after exit becomes available — and the consumer seat''s mobility is overstated, raising its computed target-side extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_suppression_mechanism, empirical, 'Structural versus internalized component of consumer-side persistence.').

omega_variable(
    substitution_trajectory,
    'How fast do non-animal substitutes close the cost-and-function gap, and does the arrangement therefore persist by necessity or by protection?',
    'Price-parity tracking for precision-fermented and cultivated products; subsidy-incidence and procurement-rule analysis showing where the arrangement is shielded from substitution rather than outcompeted.',
    'Rapid parity implies persistence is enforcement-dependent, supporting the snare verdict; slow parity leaves a residual necessity component that would support tangled_rope elements even under this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_trajectory, empirical, 'Substitution rate as the test of necessity versus protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.49).
narrative_ontology:measurement(anim_tr_t60, animal_status__abolitionist_reading, theater_ratio, 60, 0.54).
narrative_ontology:measurement(anim_tr_t70, animal_status__abolitionist_reading, theater_ratio, 70, 0.59).
narrative_ontology:measurement(anim_tr_t80, animal_status__abolitionist_reading, theater_ratio, 80, 0.62).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(anim_be_t60, animal_status__abolitionist_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(anim_be_t70, animal_status__abolitionist_reading, base_extractiveness, 70, 0.91).
narrative_ontology:measurement(anim_be_t80, animal_status__abolitionist_reading, base_extractiveness, 80, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(anim_su_t60, animal_status__abolitionist_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(anim_su_t70, animal_status__abolitionist_reading, suppression_requirement, 70, 0.83).
narrative_ontology:measurement(anim_su_t80, animal_status__abolitionist_reading, suppression_requirement, 80, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'animal status' conflates three structurally distinct claims that yield different victim sets, different epsilon values, and different classifications. This file (abolitionist_reading) authors epsilon 0.93 for the standing instrumental-use arrangement; the welfare_reading sibling authors a lower epsilon over the same referent (use constrained, not prohibited); the property_reading sibling removes animals from the victim set entirely. The upstream/downstream structure runs from property_reading (the legal baseline the other readings contest) through welfare_reading (the compromise layer) to abolitionist_reading (the strict reading whose axioms logically eliminate both siblings within any single normative framework). All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
