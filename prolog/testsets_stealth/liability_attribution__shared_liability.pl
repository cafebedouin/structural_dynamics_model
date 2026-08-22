% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Value-Chain Shared Liability Allocation (Causal Contribution and Control)
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   Under the shared-liability allocation, legal responsibility for harms
 *   arising from multi-actor technology value chains is apportioned across
 *   every contributing actor (model developers, fine-tuners, integrators,
 *   deployers) in proportion to causal contribution and practical control
 *   over the risk that produced the harm. The arrangement solves a real
 *   collective problem: when causation is diffuse, single-seat liability
 *   leaves victims facing judgment-proof defendants and leaves no actor with
 *   a care incentive proportional to its risk contribution. It also generates
 *   its own cost structure: every chain actor must document processes deeply
 *   enough for contribution to be assessed, insure an allocated share, and
 *   litigate apportionment whenever shares are contested; an insurance and
 *   indemnification market emerges as the risk-pricing layer, and contractual
 *   clauses redistribute opacity and defense burdens along the chain. The
 *   expected structural delta of this reading is visible in the stakeholder
 *   set: both developers and deployers sit in the victim set, coordination
 *   costs are higher than any single-seat alternative, the insurance market
 *   is a named party, and the opacity burden travels through contracts. The
 *   claim/metric split is deliberate: claimed_type tangled_rope is my
 *   structural judgment that genuine coordination (compensation, deterrence,
 *   insurability) and asymmetric extraction (compliance, premium, and
 *   litigation costs borne by the paying chain, with margin captured at the
 *   insurance layer) coexist in one enforced structure, while the metrics are
 *   authored descriptively of the arrangement's actual operation. This file
 *   instantiates the shared_liability reading of the liability_attribution
 *   kernel; the developer-primary and deployer-primary readings are separate
 *   constraints linked via network.affects_constraints, each with its own
 *   concentrated epsilon and victim set by construction.
 *
 * KEY AGENTS:
 *   - frontier_model_developers: primary target (powerful/constrained) — carries allocated upstream liability shares, funds compliance and insurance, retains agenda-shaping influence over the allocation formula
 *   - enterprise_deployers: primary target (institutional/constrained) — carries deployment-side shares and contractual indemnity obligations, reprices its share through vendor and contract structure
 *   - small_ai_vendors: target (moderate/trapped) — fixed compliance and insurance costs are regressive at thin margins; full exit means leaving the market
 *   - open_source_contributors: target (powerless/constrained) — exposed without contractual protection or insurance capacity; cannot retract released artifacts
 *   - injured_third_parties: primary beneficiary (moderate/trapped) — the compensated class; viable claims only through aggregation
 *   - liability_insurers: beneficiary and receipt seat (institutional/arbitrage) — intermediates the allocated risk, collects premiums, retains margin
 *   - plaintiff_bar: beneficiary (organized/mobile) — collects contingent fees on aggregated apportioned claims
 *   - technology_regulators: agenda setter (institutional/constrained) — writes allocation statutes, duties, and market-access conditions
 *   - courts: agenda setter (institutional/constrained) — operationalizes contribution-and-control apportionment case by case
 *   - technology_policy_analysts: analytical observer (analytical/analytical) — outside view on whether apportionment tracks contribution or litigation resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.58).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.5).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Value-Chain Shared Liability Allocation (Causal Contribution and Control)").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'ded63a01-5ac9-4fc3-8767-c73815eb5bdd').
narrative_ontology:cs_kernel_codification('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', formalized).
narrative_ontology:cs_authority_grounding('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', lineage).
narrative_ontology:cs_interpretation_layer_present('ded63a01-5ac9-4fc3-8767-c73815eb5bdd').
narrative_ontology:cs_reading_relation('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', liability_attribution__developer_liability, influences).
narrative_ontology:cs_axiom('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', foundational, liability_apportioned_by_control_and_causation).
narrative_ontology:cs_axiom_status(liability_apportioned_by_control_and_causation, holdable).
narrative_ontology:cs_axiom_grounding('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', liability_apportioned_by_control_and_causation, deontological).
narrative_ontology:cs_axiom('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', foundational, victim_recourse_independent_of_single_seat_solvency).
narrative_ontology:cs_axiom_status(victim_recourse_independent_of_single_seat_solvency, holdable).
narrative_ontology:cs_axiom_grounding('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', victim_recourse_independent_of_single_seat_solvency, deontological).
narrative_ontology:cs_reference_frame('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', proportional_apportionment_baseline).
narrative_ontology:cs_drift_state('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', partial_adoption_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ded63a01-5ac9-4fc3-8767-c73815eb5bdd', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, injured_third_parties).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, plaintiff_bar).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, frontier_model_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, enterprise_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, small_ai_vendors).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, open_source_contributors).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, loss_spreading_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and release the large models at the head of the value chain. Under the allocation rules they carry a share of liability for downstream harms, priced through insurance premiums, reserve requirements, and indemnity obligations, and they must document training and evaluation processes well enough for their contribution to be assessed. They lobby intensively over allocation formulas and fund the compliance infrastructure the rules require. Exit would mean withdrawing from regulated markets or relocating development to permissive jurisdictions, both costly given where their customers and compute sit.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, frontier_model_developers, payer,
    powerful, biographical, constrained, global).

% Integrate models into products and services and face deployment-side liability for monitoring, disclosure, and intervention failures. They carry deployment-side liability shares and negotiate indemnity clauses upstream and downstream, passing risk through contracts where their bargaining power allows. They cannot easily exit, since their offerings increasingly depend on the capability, but they can shift vendors, jurisdictions, or contract structures to reprice their share.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, enterprise_deployers, payer,
    institutional, biographical, constrained, global).

% Build narrow products on top of foundation models with thin margins and no in-house legal or actuarial capacity. Fixed compliance costs such as documentation, insurance minimums, and contractual review consume a proportionally larger share of revenue than for large firms, and insurers price them as a class rather than on individual merit. Leaving the market is the only full exit, and their sunk investment in the technology stack makes that costly.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_ai_vendors, payer,
    moderate, biographical, trapped, national).

% Release model weights, code, and datasets without charge and without contracts. They cannot participate in the contractual allocation of opacity and indemnity burdens, hold no insurance, and face personal or unincorporated exposure when their artifacts appear in harm chains. They can stop contributing, but cannot retract released artifacts, and community standing gives the exit real cost.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, open_source_contributors, payer,
    powerless, biographical, constrained, global).

% People harmed by deployed systems: screened out by a model, defrauded through an agentic workflow, injured by an automated process. The allocation rules are what stand between them and bearing the loss alone: they provide a set of solvent defendants and a procedure for apportioning among them. Individual claims are only viable aggregated, and they cannot exit the legal system that processes their claims.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, injured_third_parties, beneficiary,
    moderate, biographical, trapped, national).

% Underwrite the allocated risk: they price liability shares into premiums, impose loss-prevention requirements on insureds, defend and settle claims, and retain the margin between premiums collected and losses paid. Determinate shares are what make the risk insurable at all. They can reprice, tighten terms, or withdraw from lines of business, making them the most mobile seat in the arrangement.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Contingency-fee counsel who assemble and prosecute the aggregated claims the allocation rules make viable. Their income is a percentage of recoveries, so their caseload and fee stream scale with the volume and size of apportioned claims. They can shift practice areas if the claims dry up.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, plaintiff_bar, beneficiary,
    organized, biographical, mobile, national).

% Write the allocation statutes and delegated rules: liability shares, documentation duties, and market-access conditions tied to compliance. They gain mandate, staff, and enforcement relevance from the arrangement's existence and are constrained by legislative direction, budget, and jurisdictional reach. They cannot exit their enforcement obligations but can deprioritize them.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, technology_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Operationalize the formula: assess causal contribution and control in individual cases, apportion shares among defendants, and build the doctrine future allocation follows. Their authority is case-by-case and precedent-bound; they cannot decline the attribution questions the rules route to them, and their dockets absorb the litigation volume the allocation generates.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, courts, agenda_setter,
    institutional, generational, constrained, national).

% Academic and think-tank researchers who model the allocation's incentive effects, audit its outcomes, and propose reforms. They bear none of its costs and collect none of its flows; their seat is the outside view on whether apportionment tracks contribution or litigation resources.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, technology_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the diffuse-causation compensation problem for multi-actor technology harms: when a harm emerges from a chain no single actor controls (foundation model, fine-tuner, integrator, deployer), apportioned liability is the allocation that gives every actor some care incentive and gives victims a solvent set of defendants. It also coordinates risk pricing: the insurance market can pool the risk only once liability shares are legally determinate.
% TRANSFER_FUNCTION: Moves money from technology value-chain actors to injured third parties through judgments and settlements, and to the intermediaries processing the transfer through insurer premiums and margins and plaintiff and defense counsel fees. It also moves documentation and transparency effort from value-chain actors to regulators and courts as the evidentiary price of attribution.
% ABSENT_VOICES: Open-source contributors have no seat in the contractual allocation that distributes opacity and indemnity burdens, though they bear exposure under the rules. Future victims not yet harmed are represented only derivatively. Small deployers in non-adopting jurisdictions have no voice in rules that reach them through market-access conditions. End customers bear passed-through compliance costs without a seat in apportionment disputes.
% DISAPPEARANCE_RATIONALE: If shared-liability apportionment vanished overnight, pending multi-defendant claims would collapse into unsatisfiable single-defendant suits, the liability-insurance market built on determinate shares would reprice or withdraw, contractual indemnity chains would lose their anchor, and safety-relevant behavior currently priced through allocated shares would be re-externalized. Compensation flows, insurance contracts, and allocation doctrine would all rearrange within one litigation cycle.
% FOUNDING_PROBLEM: As technology value chains fragmented, harm causation became diffuse: no single actor controlled enough of the chain to bear primary liability, judgment-proof defendants absorbed judgments that outran their assets, and victims of multi-actor harms went uncompensated while no actor faced a care incentive proportional to its contribution to the risk.
% FOUNDING_PROBLEM_CORROBORATION: Paying parties concede the problem is real even while contesting the allocation: developer and deployer trade-association legislative testimony acknowledges the attribution gap. Court opinions in single-seat jurisdictions document unsatisfied judgments and dismissed claims against judgment-proof defendants. Regulatory impact assessments for liability reform quantify uncompensated-harm rates. Law-and-economics scholarship outside the beneficiary set corroborates both the problem's existence and its persistence. No corroborating source outside the arrangement claims the problem is solved.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.58: the arrangement's costs are real and rising — insurance premium load, indemnification cascades, documentation burden, multi-party apportionment litigation — but a large fraction of what moves is compensation to injured parties and priced deterrence, which is the coordination function working. Suppression 0.50: enforcement is coercive (courts, market-access conditions) but alternatives are not foreclosed the way an extraction-dominant structure forecloses them; the main suppression channel is chilling — deferred releases, avoided product categories, declining open-source contribution — which is ambiguous between functional deterrence and deadweight loss (see omega deterrence_vs_chilling_ambiguity). Theater 0.35 and rising: documentation and compliance activity is partly functional evidence-gathering for attribution and partly ritual that no longer improves safety or attribution accuracy. Accessibility_collapse 0.45: single-seat and first-party-insurance alternatives remain live in other jurisdictions and in ongoing legislative debate. Resistance 0.60: sustained industry lobbying, jurisdictional arbitrage, and contract-structure fights are the arrangement's daily opposition. All three tracked series share one grid (2016-2046, seven points); the 2016-2026 points are observed (proposal, first statutes, early case law) and 2031-2046 are projected continuation of the hardening trajectory. Rising base_extractiveness over a stable coordination core is the expected tangled-rope drift: rent layers (insurance margin, compliance industry) accreting on a functional core, with theater growth marking the documentation ritualization that would signal atrophy if it continues.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is structural rather than perceptual error. From the insurer seat the arrangement is a priced product line whose determinate shares make risk insurable: low personal extraction, high coordination. From the injured-party seat it is a compensation guarantee: near-zero extraction, high coordination. From the small-vendor and open-source seats the same clauses are a regressive fixed cost with no compensating benefit: extraction near full. From the regulator and court seats it is a deterrence instrument they administer. Frontier developers occupy a hybrid: full-rate payers on paper, partial agenda-setters in practice, which is why their effective position sits below the small-vendor rate despite nominally similar shares. Coalition note: the powerless open-source class has low coalition potential — heterogeneous, volunteer, unorganized — so its numerical size does not translate into resistance the way an organized class's would.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: injured_third_parties, liability_insurers, and plaintiff_bar sit near the beneficiary end (d low), with insurers' arbitrage exit damping their effective exposure further, while injured parties' trapped exit keeps them short of the full beneficiary position — they pay in litigation time and unrecovered losses even when they win. Victim declarations drive the target end: frontier_model_developers, enterprise_deployers, small_ai_vendors, and open_source_contributors sit near the full-target end (d high), amplified for the trapped and the powerless — small vendors and open-source contributors have no exit and no contractual protection, so their effective extraction approaches the full rate — while powerful developers' lobbying and contract bargaining power damps theirs. Regulators and courts are agenda setters: they administer the arrangement rather than collect from it, sitting near symmetric. Scope amplification applies at the global scope of the paying chain, where verifying contribution across jurisdictions is hardest; suppression remains the raw authored scalar, unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both directions of misreading. A pure-extraction reading would erase the functions that actually operate: money reaches injured parties, insurance makes the risk financeable, and allocated shares change development and deployment behavior. A pure-coordination reading would erase the asymmetries: premium margin accrues to the insurance layer, compliance costs are regressive against small actors, and contractual allocation pushes opacity burdens onto parties who never signed. Mandatrophy is not resolved: the founding problem (diffuse causation leaving victims without a solvent defendant and actors without proportional care incentives) is live and corroborated from outside the beneficiary set. The monitored failure mode is the theater trajectory: if documentation compliance continues ritualizing without improving attribution or safety, the coordination half atrophies while the extraction half persists and the arrangement drifts toward the piton cell — an administrator that could change it, a cost-to-fix that exceeds what it bears, mostly performance. The rising theater_ratio series is the early indicator; the prohibitive fixing_cost reflects that the arrangement is load-bearing today, which is exactly what makes later atrophy hard to repair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_underdetermination,
    'This constraint is the shared_liability reading of the liability_attribution kernel; the disagreement with the developer_liability and deployer_liability siblings is located in which structural element — the seat of causal origin of the capability, the seat of practical control over deployment risk, or the proportionality formula this reading adopts?',
    'Comparative institutional analysis across jurisdictions that adopted different readings: trace which attribution factor (origin, control, capacity) actually drives outcomes in shared-liability courts, and whether outcomes converge on one seat despite nominal sharing.',
    'If apportionment outcomes empirically concentrate on one seat, the shared reading collapses toward that sibling: the victim set contracts to one class, coordination costs fall, the insurance market thins to a single-class product, and opacity burdens recenter on the designated seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation_underdetermination, conceptual, 'Which structural element of the liability kernel the sibling disagreement turns on.').

omega_variable(
    attribution_metric_operationalization,
    'Can causal contribution and control be operationalized precisely enough that apportionment tracks the statutory formula rather than the parties'' litigation resources?',
    'Empirical study of apportionment outcomes against party litigation budgets and technical-evidence quality: if awarded shares correlate with legal spending rather than measured contribution, allocation is litigation-determined.',
    'If litigation resources drive shares, effective extraction shifts from principled apportionment to legal-resource asymmetry: small actors bear disproportionate effective rates, and the arrangement''s coordination claim weakens toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_metric_operationalization, empirical, 'Whether apportionment tracks the statutory formula or party resources.').

omega_variable(
    insurer_intermediary_effect,
    'Does the insurance and indemnification layer that emerges under shared liability reduce net cost (risk pooling, loss-prevention expertise) or amplify it (margin capture, shadow-regulatory compliance demands, class-based pricing of small actors)?',
    'Compare loss ratios and premium loading in shared-liability lines against comparable single-seat liability lines; audit insurer-imposed compliance requirements for safety-relevant versus liability-shifting content.',
    'If the intermediary layer amplifies cost, the insurance margin grows as a rent layer and the arrangement slides toward the snare cell; if it reduces net cost, the coordination reading strengthens and epsilon is partly the price of financeability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurer_intermediary_effect, empirical, 'Net effect of the insurance intermediary layer on extraction.').

omega_variable(
    deterrence_vs_chilling_ambiguity,
    'How much of the measured suppression is functional deterrence (discouraging negligent development and deployment) versus deadweight chilling (discouraging beneficial deployment, open-source contribution, and market entry)?',
    'Natural experiments from allocation reforms: measure entry rates, open-source release rates, and deployment of safety-improving features before and after changes, distinguishing activity reduction in high-risk segments from low-risk segments.',
    'If most suppression is functional deterrence, the coordination function is stronger than the suppression scalar suggests; if deadweight chilling dominates, effective suppression exceeds the authored value and the rope component erodes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_vs_chilling_ambiguity, conceptual, 'Functional-deterrence versus deadweight-chilling component of suppression.').

omega_variable(
    noncontracting_party_exposure,
    'Does contractual allocation of opacity and indemnification burdens systematically exclude parties outside the contracting chain (open-source contributors, downstream users of free artifacts), concentrating uncompensated exposure on the least-insured class?',
    'Trace license enforceability of indemnity and opacity clauses against non-signing contributors; survey litigation outcomes involving open-source components in shared-liability claims.',
    'If exclusion is systematic, the effective victim set is larger than authored, the powerless class''s effective extraction approaches the full target rate, and the arrangement''s fairness premise — apportionment by contribution — is contradicted by its own allocation mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noncontracting_party_exposure, empirical, 'Whether contractual allocation excludes non-signing contributors from protection while leaving them exposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 2016, 2046).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t2016, liability_attribution__shared_liability, theater_ratio, 2016, 0.16).
narrative_ontology:measurement_basis(liab_tr_t2016, observed).
narrative_ontology:measurement(liab_tr_t2021, liability_attribution__shared_liability, theater_ratio, 2021, 0.2).
narrative_ontology:measurement_basis(liab_tr_t2021, observed).
narrative_ontology:measurement(liab_tr_t2026, liability_attribution__shared_liability, theater_ratio, 2026, 0.24).
narrative_ontology:measurement_basis(liab_tr_t2026, observed).
narrative_ontology:measurement(liab_tr_t2031, liability_attribution__shared_liability, theater_ratio, 2031, 0.28).
narrative_ontology:measurement_basis(liab_tr_t2031, projected).
narrative_ontology:measurement(liab_tr_t2036, liability_attribution__shared_liability, theater_ratio, 2036, 0.31).
narrative_ontology:measurement_basis(liab_tr_t2036, projected).
narrative_ontology:measurement(liab_tr_t2041, liability_attribution__shared_liability, theater_ratio, 2041, 0.33).
narrative_ontology:measurement_basis(liab_tr_t2041, projected).
narrative_ontology:measurement(liab_tr_t2046, liability_attribution__shared_liability, theater_ratio, 2046, 0.35).
narrative_ontology:measurement_basis(liab_tr_t2046, projected).

% Extraction over time
narrative_ontology:measurement(liab_be_t2016, liability_attribution__shared_liability, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement_basis(liab_be_t2016, observed).
narrative_ontology:measurement(liab_be_t2021, liability_attribution__shared_liability, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement_basis(liab_be_t2021, observed).
narrative_ontology:measurement(liab_be_t2026, liability_attribution__shared_liability, base_extractiveness, 2026, 0.5).
narrative_ontology:measurement_basis(liab_be_t2026, observed).
narrative_ontology:measurement(liab_be_t2031, liability_attribution__shared_liability, base_extractiveness, 2031, 0.54).
narrative_ontology:measurement_basis(liab_be_t2031, projected).
narrative_ontology:measurement(liab_be_t2036, liability_attribution__shared_liability, base_extractiveness, 2036, 0.56).
narrative_ontology:measurement_basis(liab_be_t2036, projected).
narrative_ontology:measurement(liab_be_t2041, liability_attribution__shared_liability, base_extractiveness, 2041, 0.57).
narrative_ontology:measurement_basis(liab_be_t2041, projected).
narrative_ontology:measurement(liab_be_t2046, liability_attribution__shared_liability, base_extractiveness, 2046, 0.58).
narrative_ontology:measurement_basis(liab_be_t2046, projected).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t2016, liability_attribution__shared_liability, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement_basis(liab_su_t2016, observed).
narrative_ontology:measurement(liab_su_t2021, liability_attribution__shared_liability, suppression_requirement, 2021, 0.35).
narrative_ontology:measurement_basis(liab_su_t2021, observed).
narrative_ontology:measurement(liab_su_t2026, liability_attribution__shared_liability, suppression_requirement, 2026, 0.39).
narrative_ontology:measurement_basis(liab_su_t2026, observed).
narrative_ontology:measurement(liab_su_t2031, liability_attribution__shared_liability, suppression_requirement, 2031, 0.43).
narrative_ontology:measurement_basis(liab_su_t2031, projected).
narrative_ontology:measurement(liab_su_t2036, liability_attribution__shared_liability, suppression_requirement, 2036, 0.46).
narrative_ontology:measurement_basis(liab_su_t2036, projected).
narrative_ontology:measurement(liab_su_t2041, liability_attribution__shared_liability, suppression_requirement, 2041, 0.48).
narrative_ontology:measurement_basis(liab_su_t2041, projected).
narrative_ontology:measurement(liab_su_t2046, liability_attribution__shared_liability, suppression_requirement, 2046, 0.5).
narrative_ontology:measurement_basis(liab_su_t2046, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'technology/AI liability' per the epsilon-invariance principle: the label covers three structurally distinct allocation claims, each with its own epsilon, victim set, and enforcement profile. liability_attribution__developer_liability concentrates costs on capability creators; liability_attribution__deployer_liability concentrates them on deployment-context controllers; this file (shared_liability) distributes costs across the whole paying chain and adds machinery the single-seat readings lack — apportionment doctrine and the insurance/indemnification intermediation layer. The shared reading is upstream in machinery terms: its contribution-assessment and control-test toolkit is what single-seat regimes borrow when their attribution fails, and the insurance market built on determinate shares changes the cost structure of any single-seat alternative. Same kernel, different constraints — linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
