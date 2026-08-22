% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe-as-Necessary-Selector Doctrine (Event-Driven Competence Regime)
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   In safety-critical industries — nuclear operations, aviation, chemical
 *   processing, emergency services — a durable doctrine holds that only
 *   actual catastrophes supply the selection pressure (chaos, mortality
 *   salience, organizational trauma) that keeps catastrophe-handling
 *   competence real. Where the doctrine governs, it shapes budgets (drill and
 *   simulator spending held down as 'false confidence'), authority (veterans
 *   with first-person event experience outrank simulation credentials), and
 *   learning policy (post-event investigation treated as the genuine training
 *   mechanism). The doctrine encodes a real epistemic core — the
 *   rehearsal-reality gap is empirically persistent — and simultaneously
 *   concentrates savings with budget holders while diffusing mortality risk
 *   across workers and publics. This file instantiates ONE reading of the
 *   kernel catastrophe_avoidance_retention (reading:
 *   catastrophe_as_necessary_selector); the sibling readings
 *   simulation_as_proxy_catastrophe and hybrid_near_miss_learning are
 *   separate constraint stories with their own epsilon and are not averaged
 *   into this one. Per the kernel-reading epsilon rule, the referent is the
 *   standing event-driven regime this doctrine governs, assessed by this
 *   reading's own lights: the reading prices catastrophic casualties as
 *   necessary tuition rather than theft, so authored epsilon is moderate-low
 *   even though the structural declarations support substantially higher
 *   per-seat extraction for payer seats. That divergence is the datum the
 *   engine computes. KEY AGENTS (by structural relationship): -
 *   executive_budget_holders: Agenda-setter and primary collector
 *   (powerful/mobile) — controls drill budgets, captures the savings the
 *   doctrine justifies - veteran_incident_operators: Status beneficiary with
 *   residual payer exposure (moderate/identity_locked) — scar-tissue
 *   authority, frontline mortality risk - frontline_operators: Primary target
 *   (powerless/constrained) — bears rehearsed-but-unready risk -
 *   new_hire_operators: Secondary target (powerless/trapped) — inherits the
 *   thinned training pipeline - downstream_public: Diffuse target
 *   (powerless/trapped) — bears catastrophic tail risk -
 *   safety_science_community: Analytical observer (analytical/analytical) —
 *   compiles the counter-evidence the regime must dismiss -
 *   near_miss_reporters: Excluded voice (powerless/constrained) — produces
 *   the evidence stream the doctrine classes as insufficiently real
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.3).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.61).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe-as-Necessary-Selector Doctrine (Event-Driven Competence Regime)").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'f313d57a-b66b-4498-9b4e-ca67a83519c9').
narrative_ontology:cs_kernel_codification('f313d57a-b66b-4498-9b4e-ca67a83519c9', distributed).
narrative_ontology:cs_authority_grounding('f313d57a-b66b-4498-9b4e-ca67a83519c9', lineage).
narrative_ontology:cs_interpretation_layer_present('f313d57a-b66b-4498-9b4e-ca67a83519c9').
narrative_ontology:cs_reading_relation('f313d57a-b66b-4498-9b4e-ca67a83519c9', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('f313d57a-b66b-4498-9b4e-ca67a83519c9', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('f313d57a-b66b-4498-9b4e-ca67a83519c9', foundational, embodied_trauma_necessity).
narrative_ontology:cs_axiom_status(embodied_trauma_necessity, holdable).
narrative_ontology:cs_axiom_grounding('f313d57a-b66b-4498-9b4e-ca67a83519c9', embodied_trauma_necessity, empirically_contingent).
narrative_ontology:cs_axiom('f313d57a-b66b-4498-9b4e-ca67a83519c9', secondary, peacetime_decay_inevitability).
narrative_ontology:cs_axiom_status(peacetime_decay_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('f313d57a-b66b-4498-9b4e-ca67a83519c9', peacetime_decay_inevitability, empirically_contingent).
narrative_ontology:cs_reference_frame('f313d57a-b66b-4498-9b4e-ca67a83519c9', catastrophic_selection_regime).
narrative_ontology:cs_drift_state('f313d57a-b66b-4498-9b4e-ca67a83519c9', contemporary_simulation_maturity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f313d57a-b66b-4498-9b4e-ca67a83519c9', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, new_hire_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, downstream_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_operators).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, rehearsal_reality_gap_thesis).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_forgetting_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set training and preparedness budgets and decide how much synthetic rehearsal to fund. The doctrine that only real catastrophes build competence gives them a principled reason to hold drill spending down and to treat post-event investigation as the real training mechanism. Savings land in their operating lines during their tenure; accountability for the next event lands on whoever is present when it arrives, which is usually not them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders, agenda_setter,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders, beneficiary).

% Hold authority grounded in having been present at real catastrophic events. Their scar-tissue experience is the regime's scarcest credential, and the doctrine is what keeps it scarce: if simulation counted fully, their status premium would deflate. They also staff the front line when events occur, so they bear the same mortality risk their authority helps justify. Leaving would mean abandoning the professional identity their experience constitutes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_operators, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_operators, payer).

% Work the plants, cockpits, wards, and rigs where catastrophic events actually arrive. They receive less rehearsal than the doctrine's critics recommend and inherit the residual risk when drilled responses meet real chaos. Changing employers does not escape the regime because the doctrine is industry-wide; refusing the risk is not a live option for someone with a mortgage and a specialty.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators, payer,
    powerless, biographical, constrained, regional).

% Enter the profession after the doctrine has already shaped their training pipeline: thin simulation hours, deference to veteran war stories, and the message that real competence comes only from real events. They have sunk education and identity into the field, so leaving means writing off the investment; staying means waiting for the event that will finally teach them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, new_hire_operators, payer,
    powerless, immediate, trapped, regional).

% Live downwind, downstream, and underneath the facilities operated under this regime. They are assured the industry learns from every event, and they bear the tail risk when the learning arrives too late. Relocating away from hazard zones is possible in principle and prohibitive in practice for most households.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, downstream_public, payer,
    powerless, generational, trapped, regional).

% Researchers in high-reliability organization theory, resilience engineering, and transfer-of-training studies. They compile the evidence that near-miss reporting, foreign-incident analysis, and high-fidelity simulation do move competence, and they publish the critiques the doctrine's holders must dismiss to keep the regime intact. They hold no operational authority over budgets or rosters.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_science_community, observer,
    analytical, civilizational, analytical, global).

% Line workers who file near-miss reports — the data stream the doctrine classifies as insufficiently real to confer competence. Their reports feed databases that budget reviews cite as evidence training works, and their careers can suffer when reports implicate popular veterans. They are inside the system producing its most contested evidence and outside the rooms where the doctrine is reaffirmed.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, near_miss_reporters, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational attention on the gap between rehearsed and real conditions: it warns that passing a drill is not proof of field readiness, that organizations forget faster than they document, and that synthetic success can manufacture confidence that collapses on contact with chaos.
% TRANSFER_FUNCTION: Moves preparedness resources away from synthetic rehearsal toward post-event reconstruction: budget savings flow to operating lines and the executives who control them; authority and status flow to veterans holding first-person catastrophe experience; residual mortality and tail risk flow to frontline workers, new hires, and downstream publics.
% ABSENT_VOICES: The victims of the next catastrophe are absent by construction — the regime treats their arrival as its tuition. Near-miss reporters are present in the data but absent from the rooms where the doctrine is reaffirmed; safety-science researchers publish outside operational decision cycles; retired operators who watched drills work carry testimony the regime has no slot for.
% DISAPPEARANCE_RATIONALE: Training budgets would be re-contested immediately: simulation and near-miss programs would gain the funds the doctrine currently deflects, veteran authority premised on scar-tissue scarcity would deflate, and the industry's risk distribution would shift from episodic tuition toward continuous preparedness investment. The regime's beneficiaries would lose their justificatory framework overnight, and post-event investigation would compete with — rather than displace — synthetic training as the learning mechanism.
% FOUNDING_PROBLEM: Early industrial, aviation, and military history repeatedly showed organizations failing at exactly the tasks they had rehearsed: drilled responses disintegrated under real chaos, plans collapsed on contact, and certified competence masked fatal unreadiness. The doctrine was built to answer that record — to stop organizations mistaking rehearsal for readiness.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the naturalistic-decision-making and transfer-of-training literatures, by accident-investigation reports documenting drilled-response collapse under real conditions, and by practitioner memoirs across aviation, firefighting, and emergency medicine. No corroborating source outside the doctrine's holders attests the stronger claim that catastrophe is the ONLY sufficient teacher — that extension rests on the doctrine's own authority and is precisely what the sibling readings contest.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).
:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.30) is reading-indexed over the fixed referent: through this reading's own lights the regime's casualties are necessary tuition, not extraction — but the reading's own statement of its mechanism ('mortality salience, organizational trauma') concedes the human cost it prices as input, so epsilon is not zero. Suppression (0.61) is authored as a raw structural property, unscaled by power or scope: the regime must actively dismiss accumulating transfer-effectiveness evidence, marginalize near-miss programs, and defend budget lines — structural-discursive coercion rather than physical force, with an internalized component on the identity-locked veteran seat. Theater ratio (0.42): post-event 'lessons learned' rituals, anniversary reviews, and commemorative investigations increasingly substitute for operational change, and the series shows that substitution growing. Accessibility collapse (0.35): the alternatives — simulation, near-miss systems — remain visible and partially funded; the doctrine degrades rather than eliminates them. Resistance (0.60): the HRO and resilience-engineering literatures, simulator advocates, and near-miss program champions contest the doctrine continuously. Claim and metrics are independent: claimed_type tangled_rope is my structural judgment (genuine epistemic core + asymmetric transfer + active enforcement); the metrics describe actual operation. Temporal series run on one shared grid (t=0..30, roughly three decades of the modern HRO era) with every tracked metric authored at every point. Base extractiveness rises slowly — as peer industries accumulated simulation evidence, the regime's opportunity cost grew. Suppression_requirement rises because enforcement INTENSIFIED: the doctrine's dismissal machinery had progressively more counter-evidence to absorb as transfer studies matured — an enforcement ratchet, not decay. Theater rises as ritualized learning displaced change. The series smooths the regime's episodic rhythm (crisis -> reaffirmation -> decay -> crisis) into trend; the episodic spike structure is itself part of the operating pattern, with each real event briefly restoring the doctrine's authority. Coalition note: the powerless payer seats could in principle coalition (unions, public-interest groups, cross-site operator networks), but the doctrine fragments them — each site is taught to wait for its own tuition, and near-miss data that would federate the sites is exactly what the regime classes as insufficiently real.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (frontline operators, new hires, downstream public) should compute substantially extractive types: they bear the regime's residual risk with constrained or trapped exit. The agenda-setter seat should compute a coordination type it administers: from the budget holder's position the doctrine is disciplined refusal to fund false confidence. The veteran seat is structurally split — an identity-locked beneficiary whose status capital depends on the doctrine and who nonetheless stands on the front line when events arrive — which is why it carries a secondary payer role and a directionality override. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. The sharpest gap is between the reading's own seat (which authors epsilon at 0.30, seeing tuition) and the payer seats' computed positions (which experience the same arrangement as enforced risk transfer) — that gap is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. Executive budget holders sit near the beneficiary end (low d): they collect the savings and control the enforcement agenda, with mobile exit letting them leave before the deferred costs arrive. Frontline operators, new hires, and the downstream public sit near the full-target end (high d): they bear the transferred mortality and tail risk, and trapped or constrained exit keeps them there. Safety-science observers take the analytical seat. The single override corrects the veteran seat: the derivation chain reading the beneficiary declaration plus identity_locked exit would place veteran_incident_operators near full subsidy (d around 0.15), but they also staff the front line during the events their authority presupposes — the secondary payer role is structural, not rhetorical — so the override sets d to 0.35 for the moderate power atom, which in this story only they occupy. Receipt is not benefit: veteran_incident_operators benefit (status rents) and are listed as beneficiaries, but the extracted resources themselves — the deflected preparedness funding — demonstrably accrue to the executive budget line, which is why gain_flow names executive_budget_holders. Fixing is prohibitive for whoever could fix it: rebuilding credible high-fidelity competence infrastructure is capital-intensive with probabilistic, deferred payoffs that exceed the fixer's private benefit, which is precisely why the regime persists despite published critique.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric errors. Reading the doctrine as pure snare would erase its genuine coordination function: the rehearsal-reality gap is real, drilled confidence does sometimes collapse on contact, and the doctrine's warning has saved organizations from simulator-induced complacency. Reading it as rope would erase the asymmetric transfer: savings concentrate with budget holders while mortality risk diffuses across workers and publics through the same structure that delivers the warning. The victim declarations keep the extraction half visible; the vindicated propositions (rehearsal_reality_gap_thesis, organizational_forgetting_law) keep the coordination half visible without granting them rent-collecting status. On mandatrophy proper: the founding problem (rehearsed competence masking unreadiness) is live and independently corroborated, so no mandatrophy_resolved declaration is authored — the regime is not yet performing a dead mandate. The piton risk lies elsewhere and is visible in the theater series: if transfer-effectiveness evidence continues to accumulate and the doctrine's enforcement becomes purely ceremonial dismissal, the arrangement drifts toward theatrical maintenance of a position its holders no longer argue for — the omega variables transfer_effectiveness_boundary and veteran_identity_lock_direction are the tripwires for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Does first-person catastrophic experience provide a selection pressure (chaos, mortality salience, organizational trauma) that synthetic and vicarious signals cannot replicate, or do the sibling readings'' mechanisms close the same competence gap?',
    'Longitudinal cross-organization studies comparing competence retention under simulation-heavy, near-miss-rich, and event-driven regimes at matched event-free durations, controlling for base rates and hazard profile.',
    'If the sibling mechanisms suffice, this reading''s regime loses its justification and its extraction becomes pure rent; if this reading holds, the sibling regimes are false-confidence machines. The sibling stories'' classifications flip accordingly; this story is one reading of kernel catastrophe_avoidance_retention and does not average over them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is the catastrophe_as_necessary_selector reading; the disagreement with simulation_as_proxy_catastrophe and hybrid_near_miss_learning is located at the sufficiency boundary of synthetic and vicarious signals.').

omega_variable(
    transfer_effectiveness_boundary,
    'How much real-condition competence does high-fidelity simulation actually transfer, and is there a fidelity ceiling below which drill-produced confidence is structurally false?',
    'Transfer-of-training meta-analyses and prospective cohort studies comparing field performance of drill-trained versus event-experienced operators under genuine emergencies.',
    'High measured transfer collapses this reading''s foundational premise and completes the axiom_overriding drift; low transfer vindicates the doctrine and shifts extraction attribution toward the simulation-and-certification complex the doctrine condemns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_effectiveness_boundary, empirical, 'The empirical hinge on which this reading''s core axiom stands or falls.').

omega_variable(
    naturality_of_decay,
    'Is competence decay without catastrophic selection a law of organizational nature, or a produced effect of memory practices, staffing rotation, and documentation choices that the doctrine''s inevitability framing conceals?',
    'Compare decay curves across organizations differing in deliberate retention countermeasures (rotation design, deliberate-practice schedules, externalized procedure memory) at equal event-free durations.',
    'If decay is constructed, the doctrine''s natural-law rhetoric is doing justificatory work for budget avoidance and the arrangement moves toward constructed-extraction readings; if lawful, part of the measured cost is irreducible and the coordination core strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_decay, conceptual, 'Whether the ''inevitable decay'' premise is discovered or manufactured.').

omega_variable(
    survivorship_bias_in_selection_narrative,
    'Does the doctrine''s evidence base suffer survivorship bias — counting the catastrophes that taught lessons and forgetting the ones that only killed?',
    'Denominator analysis: catalog all catastrophic events in the industrial record and classify outcomes as competence-building versus purely destructive; test whether the teaching fraction exceeds what unselected learning would predict.',
    'If most catastrophes destroy without teaching, the selection-pressure mechanism is largely mythical, the regime''s tuition buys little, and the arrangement slides toward snare-flavored extraction riding a real but thin epistemic core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_selection_narrative, empirical, 'Whether catastrophic ''tuition'' actually purchases competence at the claimed rate.').

omega_variable(
    veteran_identity_lock_direction,
    'Is veteran resistance to simulation credit epistemic (a genuine fidelity-gap concern) or identity-protective (defense of scar-tissue status capital)?',
    'Test whether veteran operators update on blinded transfer-effectiveness data; measure status-premium sensitivity to proposals granting simulation credentials parity with event experience.',
    'If identity-protective, the veteran seat''s effective directionality shifts toward target-of-its-own-doctrine and the regime''s enforcement is substantially self-serving; if epistemic, part of the measured suppression reflects warranted skepticism rather than enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(veteran_identity_lock_direction, conceptual, 'Identity-lock composition of the regime''s most authoritative defenders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 5, 0.28).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.31).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 15, 0.34).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.37).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 25, 0.29).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, information_standard).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% The colloquial thesis 'catastrophes maintain competence' decomposes into three structurally distinct claims about WHICH signals suffice to select for readiness: first-person catastrophe (this story), high-fidelity simulation (catastrophe_avoidance_retention__simulation_as_proxy_catastrophe), and distributed near-miss/foreign-incident learning (catastrophe_avoidance_retention__hybrid_near_miss_learning). Each claim carries its own epsilon, beneficiary structure, and failure mode per the epsilon-invariance decomposition rule. This reading is the upstream, historically grounded position — anchored in the rehearsal-reality record — and its dismissal of synthetic signals is the legitimacy condition the sibling readings must overcome; the upstream claim is routinely cited as evidence against the downstream ones.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
