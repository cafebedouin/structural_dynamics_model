% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare-Constrained Instrumental Use of Sentient Animals
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The standing arrangement under contest: animals are legally and ethically
 *   recognized as sentient beings whose interests constrain human use, while
 *   instrumental use remains broadly licensed through a lattice of welfare
 *   statutes, codes of practice, and exemption structures (standard-practice
 *   exemptions in agriculture, institutional scientific-use licenses in
 *   research, religious slaughter carve-outs). This file instantiates ONE
 *   reading of the contested kernel animal_status — the welfare_reading — and
 *   describes no other reading inside the constraint; the abolitionist and
 *   property readings are separate linked stories. The epsilon referent is
 *   the existing instrumental-use-with-welfare-protections arrangement,
 *   assessed by the welfare reading's own lights: the reading endorses
 *   constrained use in principle, so the measured wronging is the gap between
 *   the arrangement's balancing ideal and its actual operation, concentrated
 *   where exemptions override interests without individualized justification.
 *   Interval mapping: t=0 corresponds to approximately 1964 (the
 *   exposé-and-inquiry moment that launched modern welfare legislation), t=60
 *   to the present. Claim and metrics are authored independently: the claimed
 *   type states what I believe is structurally true, the metrics what I
 *   believe is descriptively true, and the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): - farmed_animals: primary target (powerless/trapped) —
 *   bears the arrangement's costs at population scale, no voice, no exit -
 *   laboratory_animals: primary target (powerless/trapped) — bears costs
 *   under scientific-use licenses - intensive_livestock_operators: primary
 *   beneficiary and co-agenda-setter (institutional/arbitrage) — collects
 *   avoided welfare costs at scale, drafts code baselines -
 *   biomedical_research_institutions: secondary beneficiary
 *   (institutional/constrained) — collects research legitimacy -
 *   consumers_of_animal_products: diffuse beneficiary with incidental
 *   cost-bearing (organized/mobile) - animal_welfare_regulators:
 *   agenda-setter (institutional/constrained) — administers the arrangement,
 *   mildly dependent on it - religious_slaughter_exemption_holders:
 *   identity-locked beneficiary (organized/identity_locked) -
 *   welfare_certification_veterinarians: fee-collecting administrator
 *   (moderate/mobile) - animal_advocacy_organizations: excluded challenger
 *   (organized/constrained) — presses the boundaries from outside the
 *   drafting rooms - applied_ethics_legal_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.65).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare-Constrained Instrumental Use of Sentient Animals").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'fff115ce-8daf-44f7-a359-d3cc62cb94ae').
narrative_ontology:cs_kernel_codification('fff115ce-8daf-44f7-a359-d3cc62cb94ae', distributed).
narrative_ontology:cs_authority_grounding('fff115ce-8daf-44f7-a359-d3cc62cb94ae', distributed).
narrative_ontology:cs_reading_relation('fff115ce-8daf-44f7-a359-d3cc62cb94ae', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fff115ce-8daf-44f7-a359-d3cc62cb94ae', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('fff115ce-8daf-44f7-a359-d3cc62cb94ae', foundational, sentience_grounds_constrainable_interests).
narrative_ontology:cs_axiom_status(sentience_grounds_constrainable_interests, holdable).
narrative_ontology:cs_axiom_grounding('fff115ce-8daf-44f7-a359-d3cc62cb94ae', sentience_grounds_constrainable_interests, deontological).
narrative_ontology:cs_axiom('fff115ce-8daf-44f7-a359-d3cc62cb94ae', secondary, proportional_balancing_permits_necessary_use).
narrative_ontology:cs_axiom_status(proportional_balancing_permits_necessary_use, holdable).
narrative_ontology:cs_axiom_grounding('fff115ce-8daf-44f7-a359-d3cc62cb94ae', proportional_balancing_permits_necessary_use, instrumental).
narrative_ontology:cs_reference_frame('fff115ce-8daf-44f7-a359-d3cc62cb94ae', sentient_interest_balancing_settlement).
narrative_ontology:cs_drift_state('fff115ce-8daf-44f7-a359-d3cc62cb94ae', contemporary_post_sentience_science_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fff115ce-8daf-44f7-a359-d3cc62cb94ae', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, intensive_livestock_operators).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, religious_slaughter_exemption_holders).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_certification_veterinarians).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__welfare_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, confined, transported, and processed at a scale of tens of billions of lifecycles per year under standards written around normal husbandry practice. Their interests — space, social contact, freedom from pain and fear — are voiced only by inspectors, auditors, and advocates who do not experience them. Every path available to them runs through the system, and their lifespans are scheduled to production cycles.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Live inside procedures approved in advance under broad institutional scientific licenses. Before approval, their expected suffering is weighed by committees whose members face institutional and career pressures; after approval, daily conditions are governed by facility custom and staff workload. There is no route out except completion of the protocol.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, continental).

% Operate the production systems that house the large majority of land-animal biomass under licenses that exempt standard practice from case-by-case scrutiny. They supply much of the technical language for welfare codes, sell under welfare-certified labels, and can shift species, geography, or automation when rules tighten in any one jurisdiction.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, intensive_livestock_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, intensive_livestock_operators, agenda_setter).

% Hold institutional licenses permitting invasive procedures under committee oversight. Animal models are embedded in drug-approval pathways and grant structures; validated non-animal methods exist and grow, but switching within grant and patent horizons carries costs institutions rarely absorb voluntarily.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Buy products whose prices embed whatever welfare cost the production system avoided, and receive moral reassurance through labeling and certification schemes. Individually they can shift toward plant-based alternatives; collectively their demand sets the scale of the entire arrangement, and they carry the health and pandemic externalities its density generates.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, consumers_of_animal_products, payer).

% Draft welfare codes, inspect facilities, and prosecute egregious cases. Code-writing proceeds in consultation with the regulated industries, and technical baselines frequently adopt industry-normal practice as the compliance floor. Agency staffing and budgets presuppose the arrangement continuing much as it is.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Practice ritual slaughter and religious animal use under statutory exemptions carved out of general welfare requirements. The practices are constitutive of community identity; proposals to narrow the exemptions are experienced as attacks on the community itself, so adjusting practice is not a live option from inside, whatever external pressure accumulates.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, religious_slaughter_exemption_holders, beneficiary,
    organized, generational, identity_locked, national).

% Perform the audits and sign the certificates that move product through welfare-labeled channels. Fees arrive per audit and career advancement rewards throughput and client retention; the criteria they apply are largely written around industry-normal practice, so signing is routine and refusal is exceptional.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_certification_veterinarians, beneficiary,
    moderate, biographical, mobile, regional).

% Investigate facilities, litigate, campaign, and run ballot initiatives to widen the protected class and narrow the exemptions. They hold consultative seats in some code processes but no vote in the rooms where exemption language is drafted; their leverage runs through publicity, elections, and market pressure the arrangement tolerates but does not institutionalize.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Analyze the arrangement from outside its administration: mapping whose interests count, stress-testing the coherence of necessity and proportionality doctrines, and publishing critiques that circulate through courts, legislatures, and curricula without carrying any enforcement power of their own.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, applied_ethics_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, intensive_livestock_operators).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem on two levels. Among producers: without shared welfare floors, care-cost competition drives a race to the bottom, so statutory minimums standardize the floor everyone can plan against. Among humans generally: the arrangement codifies the settled consensus that gratuitous cruelty is wrongful while licensing necessary use, giving consumers, researchers, and producers a shared, defensible boundary for when animal interests yield to human purposes.
% TRANSFER_FUNCTION: Moves decision authority over animal lives and bodies from the animals themselves (who cannot hold or transfer it) to human institutions; moves avoided welfare costs from the governed class to operators through exemption structures; moves moral reassurance to consumers through certification; moves research legitimacy to institutions through scientific-use licenses; moves audit fees to certifying professionals.
% ABSENT_VOICES: The governed class itself has no seat anywhere in the arrangement — animals appear only through proxies who do not experience the interests at stake, and the proxy seats are filled by people whose budgets, clients, and careers sit inside the arrangement. Smallholder and subsistence producers are also effectively absent: compliance regimes are drafted around industrial scale, pricing traditional keepers out of legality without anyone representing their situation in the code rooms.
% DISAPPEARANCE_RATIONALE: Overnight removal forces one of two massive rearrangements: reversion toward unrestricted object-treatment (welfare prosecutions cease, standards lapse, certification markets evaporate, advocacy refocuses on recreating basic protections) or abrupt prohibition (the food system, biomedical research pipelines, and tens of billions of annual animal lifecycles reorganize around substitutes that do not yet exist at scale). Either branch reorganizes agriculture, science, law, and retail simultaneously — nothing about the current world survives the removal intact.
% FOUNDING_PROBLEM: Reconcile the continuing human use of animals with the emerging recognition of their sentience: when intensification exposed the gap between how animals were treated and what was known about their capacities (the early-1960s exposé moment that produced the first modern welfare inquiries), the arrangement was built to keep use lawful while making interests constrain it.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the government inquiry convened after the 1964 exposé corroborates the founding problem historically; national sentience-recognition statutes enacted over industry objection corroborate that the reconciliation task is treated as unfinished; peer-reviewed welfare-science literature and court decisions acknowledging sentience while upholding use corroborate the live-balancing reading. Industry bodies attest the problem's liveness self-interestedly, and theorists outside the arrangement altogether attest it is differently constituted — either already answered in principle (and now managed as legitimacy maintenance) or unsolvable by balancing at all. No single attester outside the benefiting set speaks for the arrangement's own account.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the arrangement's protective content is real — gratuitous-cruelty prohibitions bite at the margins, prosecutions occur, some measurable standards improved over the interval — but the exemption lattice overrides interests wholesale precisely where volume concentrates, and the early-interval gains (visible as the dip from 0.38 to 0.34) were eroded as intensification scaled faster than individually justified protections did. Suppression 0.65: the governed class has zero voice and zero exit by construction, and the proxy layer that speaks for them is staffed from inside the arrangement; enforcement coercion on the regulated is moderate and grew with the inspection machinery (traced by the suppression_requirement series, which is authored because enforcement-capacity build-up is a real dynamic of this interval, not a static picture). Theater_ratio 0.42 and rising: certification, labeling, and audit activity grow faster than independently verified welfare gains — reassurance output decoupling from protective output is the signature of the exemption era. Accessibility_collapse 0.32: alternatives (plant-based agriculture, validated non-animal methods) visibly persist and scale; the arrangement does not collapse them, it merely declines to accelerate them. Resistance 0.52: sustained pressure from below (investigations, litigation, ballot initiatives) and from above (industry resistance to tightening) — the arrangement is defended and attacked continuously. All three tracked metrics run on one shared seven-point grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute radically different types from identical structural data. From the farmed_animals and laboratory_animals positions the arrangement governs their entire lives, offers no compensating benefit they can access, and admits no exit — a near-total constriction experienced without consent. From the operator and consumer positions the same structure is a workable license with manageable compliance friction and a clear conscience attached. From the regulator position it is a functioning balance they administer in good faith. The engine derives this divergence from directionality and exit asymmetry; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared victims (farmed_animals, laboratory_animals) derive directionality near the full-target end: powerless power combined with trapped exit amplifies effective extraction to its ceiling. Declared beneficiaries derive low directionality scaled by exit: operators (arbitrage-grade exit) sit nearest the beneficiary end; research institutions (constrained exit) sit slightly less comfortably; consumers (mobile exit, diffuse indirect costs) sit near-symmetric with a beneficiary tilt. The religious seat's identity lock keeps its directionality low — the constraint subsidizes a practice its holders cannot adjust even under pressure. No directionality_overrides were authored: the override surface keys on power atom alone, and an institutional-atom override intended to capture the regulator seat's mild dependence would misapply to the institutional beneficiaries sharing that atom. The regulator-capture residual is routed to a dedicated omega instead, where the correction belongs until the override surface can express per-agent targeting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling use with recognized sentience) is authored as contested, and the disappearance verdict is world_rearranges — the mismatch consumer flags only dead-plus-rearranges, so no dead-mandate flag fires here, correctly: the reconciliation task plausibly remains live as long as use continues at scale. The zombie-drift vector is nonetheless visible in the temporal record: theater_ratio climbing monotonically toward 0.42 while extractiveness creeps back up after its early-interval dip is the signature of a mandate drifting toward legitimacy management. If exemption coverage reaches near-totality (the exemption_coverage_share omega), the founding problem flips toward dead while the arrangement persists — at that point the arrangement is maintained because dismantling it is prohibitive (fixing_cost) and its gains accrue to a named seat (gain_flow), which is the capture configuration, not the inertial one. mandatrophy_resolved is deliberately not declared: the mandate has not outlived its function beyond contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_status_kernel_reading_position,
    'This constraint is one reading (welfare_reading) of the contested kernel animal_status. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative analysis across the linked sibling stories: the abolitionist_reading places ALL instrumental use in the victim set (driving its epsilon far above this reading''s); the property_reading removes animals from the victim set entirely for owned-animal use (driving its epsilon near zero for the use arrangement). The disagreement is located in the moral-status premise: whether sentience grounds constrainable interests, inherent rights precluding use, or no independent standing at all.',
    'If the abolitionist premise prevails, this arrangement''s entire beneficiary structure converts into a victim structure and the classification migrates sharply toward pure extraction; if the property premise prevails, the victim set empties and the arrangement collapses into ordinary property law with negligible measured wronging. This story''s epsilon of 0.45 is valid ONLY under the welfare reading''s own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_status_kernel_reading_position, conceptual, 'Committer-frame routing: this file instantiates only the welfare_reading of kernel animal_status; sibling readings are separate constraints with different victim sets and epsilon values.').

omega_variable(
    exemption_coverage_share,
    'What fraction of the confined-animal population actually sits under standard-practice and scientific exemptions rather than under individually justified welfare protections?',
    'Jurisdiction-by-jurisdiction audit of statutory exemption scopes against census data on animal numbers by production system and research use; measure biomass covered by case-by-case justification versus blanket exemption.',
    'If exemptions cover the large majority of biomass, the arrangement''s operative content is the exemption structure and its legitimation function dominates its protective function, pushing effective extraction well above the authored 0.45 and the classification toward the snare boundary; if exemptions cover a minority, the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_coverage_share, empirical, 'Whether the exemption structures are the exception or the rule in operative coverage.').

omega_variable(
    sentience_boundary_scope,
    'Which taxa fall inside the protected class as sentience science expands — decapods, cephalopods, insects, fish — and does the arrangement''s protection extend with the science?',
    'Track statutory and regulatory recognition against the accumulating comparative cognition and nociception literature; compare taxa recognized by science against taxa covered by welfare statutes and exemption schedules.',
    'Each expansion of the scientifically sentient class that the arrangement fails to absorb enlarges the unprotected victim set and raises effective extraction without any change in the arrangement''s text; failure to absorb is evidence that the constraint protects categories, not interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_boundary_scope, empirical, 'Whether the protected-class boundary tracks the sentience evidence or lags it indefinitely.').

omega_variable(
    proxy_representation_fidelity,
    'Can proxy-based representation of animal interests (by regulators, auditors, veterinarians, advocates) track the actual interests at stake, or does the proxy layer systematically filter them?',
    'Compare welfare outcomes under proxy-administered standards against outcomes under mechanisms that bypass proxies (direct behavioral indicators, camera-based monitoring, citizen suit provisions); look for systematic divergence between what proxies certify and what independent measurement shows.',
    'If proxies filter systematically, the arrangement''s suppression is deeper than its enforcement statistics suggest — the governed class is silenced twice, once by lacking voice and once by having its voice intermediated — and the authored suppression of 0.65 understates the structural condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_fidelity, conceptual, 'Whether interest-representation through human proxies preserves or distorts the interests represented.').

omega_variable(
    administrator_capture_residual,
    'Does the canonical fallback for institutional administrator seats misstate the regulator position here, given that welfare codes are substantially drafted around industry-normal practice and agency budgets depend on the arrangement continuing?',
    'Code-provenance study: trace authorship of exemption clauses and baseline standards; measure revolving-door flows between agencies and the regulated industries; compare agency-enforced baselines against independently recommended ones.',
    'If capture is material, effective extraction on the administered population is understated because the administrator seat''s directionality sits nearer the beneficiary end than a neutral-administrator fallback would place it. No directionality override was authored because the override surface keys on power atom alone and would misapply to institutional beneficiaries sharing that atom; this omega carries the correction instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrator_capture_residual, conceptual, 'Residual mild-capture question on the administrator seat that the power-atom-keyed override surface cannot express.').

omega_variable(
    transitional_vs_steady_state,
    'Is the welfare-constrained-use arrangement a transitional settlement that sunsets as non-animal alternatives mature, or a steady-state design intended to persist indefinitely?',
    'Track substitution rates (plant-based and cultivated protein share, non-animal research method validation) against the arrangement''s own revision cycle: a settlement that tightens its constraints as alternatives scale behaves transitionally; one that holds its exemptions fixed regardless behaves as steady state.',
    'If transitional, the arrangement carries an undeclared sunset obligation and its persistence without a sunset clause becomes a mandatrophy finding; if steady state, the arrangement''s defenders must justify permanence rather than transition, which changes the burden the arrangement must meet.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transitional_vs_steady_state, preference, 'Whether the arrangement''s own logic commits it to eventual obsolescence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(anim_tr_t60, animal_status__welfare_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(anim_be_t60, animal_status__welfare_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement(anim_su_t60, animal_status__welfare_reading, suppression_requirement, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'animal status' decomposes into three structurally distinct claims per the epsilon-invariance principle. The property_reading (historical baseline, upstream) declares no victims for owned-animal use and authors near-zero epsilon for the use arrangement; this welfare_reading (middle) places animals in the victim set for gratuitous and exemption-covered harm only, epsilon 0.45; the abolitionist_reading (downstream contest) places all instrumental use in the victim set and authors high epsilon. Each is a separate file with its own beneficiaries, victims, and classification; they are linked here because the property regime's persistence is cited as evidence in welfare debates and the welfare settlement's existence is cited as bad faith in abolitionist arguments. Sibling constraint_ids follow the kernel-prefix convention animal_status__<reading_id>.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
