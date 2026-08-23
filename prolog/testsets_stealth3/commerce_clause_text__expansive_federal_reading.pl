% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause — Expansive Federal Reading (Substantial Aggregate Effects Doctrine)
 *   domain: political/legal — constitutional law, federalism, commerce regulation
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the commerce_clause_text kernel:
 *   the expansive federal reading, under which interstate commerce
 *   encompasses any economic activity whose absence, aggregated across all
 *   actors, would substantially affect national markets. The arrangement
 *   under assessment is the standing doctrine built on that reading (Jones &
 *   Laughlin Steel 1937, Wickard 1942, Heart of Atlanta 1964, reaffirmed at
 *   Raich 2005) together with the federal apparatus it licenses. Epsilon is
 *   authored for THAT standing arrangement, as this reading's own lights
 *   assess it — never for the arrangements the narrow or limited sibling
 *   readings would create; those are separate constraints in separate files
 *   linked through network.affects_constraints. The claim/metric split is
 *   deliberate and unreconciled: claimed_type records the structure I believe
 *   true of this arrangement (a genuine national-market coordination core
 *   wrapped around an asymmetric, actively enforced transfer of rule-making
 *   authority), while the metrics describe observed operation. Where the
 *   engine computes a different type from any seat, that divergence is data,
 *   not error. KEY AGENTS (by structural relationship): - us_supreme_court:
 *   Agenda-setter (institutional / constrained) — authors, administers, and
 *   absorbs drift in the reading - federal_administrative_agencies: Primary
 *   beneficiary (institutional / arbitrage) — jurisdiction and budgets scale
 *   with the perimeter; the seat the gains accrue to -
 *   congressional_majorities: Beneficiary with secondary agenda-setting
 *   (institutional / arbitrage) — legislate under and extend the reading -
 *   national_labor_civil_rights_coalitions: Beneficiary (organized /
 *   constrained) — national programs ride on the perimeter -
 *   multistate_commerce_firms: Dual-positioned beneficiary/payer (powerful /
 *   arbitrage) - national_electorate_workers_consumers: Diffuse beneficiary
 *   (moderate / constrained) - state_legislatures: Primary payer (organized /
 *   constrained) — bear regulatory subordination -
 *   intrastate_small_producers: Payer (powerless / trapped) — swept in by
 *   aggregation logic - local_municipal_governments: Excluded and paying
 *   (powerless / trapped) — no independent seat in the settlement -
 *   constitutional_law_academy: Analytical observer (analytical / analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.62).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.66).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause — Expansive Federal Reading (Substantial Aggregate Effects Doctrine)").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "political/legal — constitutional law, federalism, commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'b0bc449a-807c-4de1-b3f8-3d041b34b78e').
narrative_ontology:cs_kernel_codification('b0bc449a-807c-4de1-b3f8-3d041b34b78e', fixed_text).
narrative_ontology:cs_authority_grounding('b0bc449a-807c-4de1-b3f8-3d041b34b78e', extraction).
narrative_ontology:cs_interpretation_layer_present('b0bc449a-807c-4de1-b3f8-3d041b34b78e').
narrative_ontology:cs_reading_relation('b0bc449a-807c-4de1-b3f8-3d041b34b78e', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0bc449a-807c-4de1-b3f8-3d041b34b78e', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('b0bc449a-807c-4de1-b3f8-3d041b34b78e', foundational, aggregate_effects_define_commerce_perimeter).
narrative_ontology:cs_axiom_status(aggregate_effects_define_commerce_perimeter, holdable).
narrative_ontology:cs_axiom_grounding('b0bc449a-807c-4de1-b3f8-3d041b34b78e', aggregate_effects_define_commerce_perimeter, instrumental).
narrative_ontology:cs_axiom('b0bc449a-807c-4de1-b3f8-3d041b34b78e', secondary, national_standards_override_conflicting_state_economic_rules).
narrative_ontology:cs_axiom_status(national_standards_override_conflicting_state_economic_rules, holdable).
narrative_ontology:cs_axiom_grounding('b0bc449a-807c-4de1-b3f8-3d041b34b78e', national_standards_override_conflicting_state_economic_rules, conventional).
narrative_ontology:cs_reference_frame('b0bc449a-807c-4de1-b3f8-3d041b34b78e', wickard_expansive_settlement).
narrative_ontology:cs_drift_state('b0bc449a-807c-4de1-b3f8-3d041b34b78e', contemporary_post_lopez_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('b0bc449a-807c-4de1-b3f8-3d041b34b78e', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, congressional_majorities).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_labor_civil_rights_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, multistate_commerce_firms).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_electorate_workers_consumers).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, intrastate_small_producers).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_municipal_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, multistate_commerce_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the expansive reading in the 1937-1942 decisions and has administered it since: deciding which congressional statutes fall inside the substantial-effects perimeter, trimming occasional outliers (a statute regulating gun possession near schools, a civil-remedy provision), and reaffirming the core (home-consumed wheat, home-cultivated medical marijuana). Its interpretations absorb challenges without reopening the underlying settlement. Its institutional standing now rests on being the body that defines where national economic regulation ends; stepping out of that role would mean disavowing nearly nine decades of its own work.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, us_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Pass statutes on wages, hours, union relations, environmental discharge, and discrimination by invoking the aggregate-effects rationale. Each majority thereby obtains the ability to legislate for the entire national economy without negotiating separately with dozens of statehouses. Majorities turn over quickly, so the authority they collect is short-horizon; the statutes they initiate are also the tests that extend or contract the reading's future reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, congressional_majorities, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, congressional_majorities, agenda_setter).

% Receive jurisdiction, budgets, and staffing proportional to the area the reading opens: bodies that certify bargaining units, set wage floors, and issue permits for facilities whose goods may never cross a state line. Because an activity's cumulative footprint can always be argued to touch the national market, new regulatory programs can be routed through the reading — the agencies' growth path runs along doctrinal breadth, and their organizational existence presupposes the perimeter.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary,
    institutional, biographical, arbitrage, national).

% Organized nationally after decades in which state-by-state campaigns failed: capital relocated to hostile jurisdictions and local ordinances fell to court challenges. National bargaining law, wage floors, and public-accommodations guarantees all stand on the reading. Returning to purely state-level strategy would forfeit hard-won national standards, so these organizations defend the perimeter even where particular applications disappoint them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_labor_civil_rights_coalitions, beneficiary,
    organized, generational, constrained, national).

% Operate in every state and face one federal rulebook instead of fifty. Uniformity lowers compliance engineering and prevents hostile-state escalation spirals. The price: federal compliance obligations on operations that would otherwise be purely local matters, and loss of the option to shop for lenient state regimes. Firms hedge — supporting federal preemption in some fields while lobbying for exemptions in others.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, multistate_commerce_firms, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, multistate_commerce_firms, payer).

% Receive portable protections that travel with jobs across state lines: a national wage floor, workplace safety standards, and anti-discrimination guarantees no state can undercut to attract employers. They also lose the ability to set different economic rules for themselves through their state capitols, and uniform standards sometimes fit their regions poorly. Their voice arrives mainly through national elections rather than through the state legislatures that once set these rules.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_electorate_workers_consumers, beneficiary,
    moderate, biographical, constrained, national).

% Retain police powers on paper but find the economic core of those powers occupied: whenever a state standard touches activity that feeds the national market, federal law displaces it or shadows it. States administer federally designed programs under funding conditions, coordinate resistance through multistate coalitions and lawsuits, and negotiate waivers. They cannot leave the arrangement; their leverage is procedural rather than structural.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_legislatures, payer,
    organized, biographical, constrained, regional).

% Grow, manufacture, or serve entirely within one state yet fall inside federal jurisdiction because their output, pooled with everyone else's, moves the national figure. The canonical case is a farmer's home-consumed wheat crop counting against national marketing quotas. Nothing they do individually escapes the pooling logic; their only exits are shrinking their operations or leaving production altogether.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, intrastate_small_producers, payer,
    powerless, biographical, trapped, local).

% Cities, counties, and towns hold no independent seat in the federal framework — they are legally creatures of their states. When federal standards occupy an economic field, local ordinances on rent, scheduling, or local hiring fall with them, and municipal preferences reach Washington only if a state chooses to carry them. They bear the flattening of local variation without a direct channel to contest it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_municipal_governments, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, local_municipal_governments, payer).

% Scholars and commentators who map the doctrine's boundaries, document its evolution, and supply the arguments each faction deploys. They hold no enforcement power; their influence runs through clerkships, briefs, and nomination debates. Their distance from every seat lets them see the whole settlement — its coordination achievements and its subordination costs — at once.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates economic activity whose effects cross state lines: supplies one enforceable rule-set for national markets, internalizes cross-border externalities such as pollution and wage undercutting, and removes the incentive for states to compete by lowering labor, safety, and consumer standards.
% TRANSFER_FUNCTION: Moves regulatory authority — with the budgets, staffing, and policy discretion attached to it — from state governments and local communities to Congress, federal agencies, and the judiciary that administers the perimeter; secondarily moves compliance costs onto producers whose activity is swept in by aggregation logic.
% ABSENT_VOICES: Municipal and county governments: constitutionally creatures of their states, they hold no independent place in the federal-state settlement, yet their ordinances and local variations bear the preemption. Their objections surface only when a state government elects to carry them. Also absent: residents of regions whose economies fit national standards poorly, who can register dissent chiefly through the same national electoral channel that produced the standards.
% DISAPPEARANCE_RATIONALE: Overnight removal would invalidate the constitutional footing beneath most twentieth-century federal economic and civil-rights regulation: agencies would lose jurisdiction wholesale, running programs would collapse, multistate firms would confront fifty conflicting regimes, and states would regain formal authority they lack the fiscal and technical capacity to exercise against national externalities. Re-consolidation through a new constitutional settlement would take decades.
% FOUNDING_PROBLEM: A nationalizing industrial economy governed state-by-state: capital crossed borders while labor, wage, and safety rules stopped at them, producing races to the bottom, exported externalities, and repeated failure of local reform (child-labor laws struck down, union drives broken by relocation threats). The New Deal Court consolidated this reading so the national government could govern the market that actually existed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: state governors and legislatures themselves petitioned Congress for federal wage-hour floors in the 1930s precisely because unilateral state action invited undercutting (Fair Labor Standards Act hearing records); economic historians of the Progressive Era independently document the interstate-competition dynamics; and contemporary state officials implicitly confirm the founding problem whenever they request federal preemption of fields they once regulated themselves.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the arrangement transfers regulatory authority and its attached rents from roughly fifty subnational rule-makers to one national apparatus, and agency jurisdiction demonstrably tracks doctrinal breadth; it stays below pure-extraction territory because a large share of the transfer purchases real solutions to cross-border collective-action problems (externality export, race-to-the-bottom dynamics, patchwork compliance costs) that no single state can solve alone. Suppression 0.66: persistence depends on active machinery — supremacy and preemption, funding conditions, and a court that polices the boundary in both directions; state-level alternatives survive only in concurrent fields inside a federal shadow. Suppression is authored as a raw structural property and is deliberately left unscaled here; directionality and scope amplification belong to the engine. Theater_ratio 0.34 and rising across the interval: federalism-deference rhetoric ('traditional areas of state concern') has grown considerably faster than any actual devolution — a performative overlay thickening on a stable centralized core. Accessibility_collapse 0.50: alternative readings have NOT collapsed — the Lopez/Morrison line demonstrates a narrower reading can still win cases, so full understanding of the doctrine leaves contest open. Resistance 0.60: continuous organized counter-pressure from state coalitions, federalism scholarship, and recurring congressional proposals. All three series share one time grid (1937/1964/1980/1995/2005/2025). The 1995 dip in extractiveness and enforcement intensity is a perturbation from court-composition shift, not an oscillatory cycle: the recovery by 2005 reveals an underlying ratchet, and the oscillation is not itself an extraction mechanism. Institutional identity fusion operates at two seats: the court's self-concept is bound to custodianship of the settlement, and the agencies' organizational identities presuppose the perimeter their jurisdiction lives inside — breaking either frame is the realistic precondition for migration toward a sibling reading.
 *
 * PERSPECTIVAL GAP:
 *   Payer seats and beneficiary seats should classify the same doctrine oppositely, and the engine's per-seat computation should surface that divergence rather than average it away. From the federal administrative seat the arrangement is infrastructure: the reading made national labor law, civil-rights enforcement, and environmental permitting possible, and its breadth reads as capacity. From the state-legislature and intrastate-producer seats the identical text operates as enforced subordination: their rule-making space contracts by an aggregation logic no vote of theirs can reach, and exit is procedural at best. The corporate seat splits internally — uniformity is a service purchased and a leash worn simultaneously. The excluded municipal seat experiences the arrangement as something decided entirely over its head. None of these perceptions is authored as truth; each follows from that seat's position in the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation chain. federal_administrative_agencies and congressional_majorities sit nearest the beneficiary pole (derived d roughly 0.05-0.15): the perimeter is their jurisdiction and payroll, and their arbitrage-grade flexibility lets them route new programs through it. national_labor_civil_rights_coalitions derive similarly low (about 0.15), with constrained exit deepening attachment. state_legislatures derive high (roughly 0.8) as declared cost-bearers whose exit is confined to procedure; intrastate_small_producers derive highest (roughly 0.95, trapped — no productive act escapes the pooling test). Two explicit overrides correct derivable blind spots, each targeting the sole stakeholder carrying that power atom. First, multistate_commerce_firms: the derivation reads the beneficiary declaration and lands near 0.1, but the authored situation includes substantial payment — federal overhead on otherwise-local operations and surrendered forum-shopping — so the powerful seat is overridden to d=0.32. Second, national_electorate_workers_consumers: declared beneficiary derives near 0.1, but they cede local self-government over economic rules, a real cost, so the moderate seat is overridden to d=0.30. local_municipal_governments combine exclusion with payment: their exclusion is itself the structural fact (no independent seat), and their directionality follows the victim-side derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-by-state failure against a nationalizing industrial economy — remains live: cross-border externalities have widened since 1937, not narrowed, so founding_problem_status='live' combined with disappearance_verdict='world_rearranges' is internally consistent and no zombie/mismatch flag should fire. Mandatrophy discipline cuts both ways in this domain. Labeling the entire arrangement pure predation erases a genuine coordination achievement: national wage floors and civil-rights public-accommodations enforcement could not have been assembled state-by-state, and the historical record (states petitioning for federal floors to stop mutual undercutting) confirms the founding problem from outside the beneficiary set. Labeling it pure coordination erases the documented, actively enforced transfer of rule-making authority away from the seats closest to local variation, with identifiable cost-bearers and a concentrated receiving seat. The tangled_rope claim holds both halves simultaneously; the temporal series marks the boundary to watch — theater rising while extractiveness plateaus is the signature that would indicate the coordination half decaying into performance while the transfer machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is ONE reading of the commerce_clause_text kernel (expansive_federal_reading). How would instantiating a sibling reading change the structural facts — and therefore the classification?',
    'Author the sibling stories (commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading) and diff their beneficiary/victim sets and epsilon values against this file; the deltas localize the disagreement.',
    'Under the narrow sibling, federal administrative agencies lose jurisdiction wholesale and this file''s beneficiary structure largely evaporates; under the limited sibling, a jurisdictional-nexus requirement restores state autonomy in noneconomic fields and shrinks the victim set. Every metric and role in this file is valid only inside the expansive reading''s perimeter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: all authored structure is reading-relative to the expansive federal instantiation of the commerce-clause kernel.').

omega_variable(
    substantial_effects_elasticity,
    'Is the reading''s ''substantial aggregate effects'' criterion a bounded test or an effectively unlimited one — does Wickard-style aggregation sweep every productive activity into the perimeter?',
    'Systematic coding of all substantial-effects findings since 1937: the distribution of activities held to qualify, and whether any economic activity has definitively failed the test once Congress asserted it.',
    'If the test is elastic, the perimeter tracks congressional appetite rather than any principled limit and the arrangement''s transfer function is effectively unbounded; if bounded, the reading is a generous but genuine coordination rule with a real outer edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effects_elasticity, empirical, 'Whether the reading''s defining criterion has a stable outer boundary.').

omega_variable(
    uniformity_net_benefit_for_multistate_firms,
    'Are multistate commercial firms net beneficiaries of uniform national regulation, or net cost-bearers once federal compliance overhead and surrendered regulatory forum-shopping are counted?',
    'Firm-level compliance-cost studies comparing a single federal regime against a fifty-state patchwork; lobbying-pattern analysis of whether firms seek more or less federal preemption across sectors.',
    'If net losers, the directionality override on the powerful seat is wrong and the beneficiary structure narrows to public-sector and movement actors, sharpening the asymmetry; if net winners, uniformity is a genuine coordination payoff that strengthens the coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_net_benefit_for_multistate_firms, empirical, 'Net position of the dual-positioned corporate seat under the expansive perimeter.').

omega_variable(
    doctrine_personnel_dependence,
    'Does the reading''s persistence rest on structural entrenchment (precedent, institutional dependence) or on Supreme Court composition — would an originalist majority migrate the kernel to the narrow sibling reading?',
    'Track appointment waves against subsequent substantial-effects jurisprudence; use the Lopez-Morrison-Raich-NFIB sequence as a natural experiment in composition-driven drift.',
    'If personnel-dependent, the constraint''s stability is contingent and its classification can flip with appointments without any change in the underlying arrangement; if entrenched, precedent inertia and institutional dependence dominate composition and the arrangement is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_personnel_dependence, empirical, 'Entrenchment-versus-personnel basis of the reading''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__expansive_federal_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_text__expansive_federal_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__expansive_federal_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__expansive_federal_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__expansive_federal_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.44).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1964, 0.58).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1980, 0.64).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1995, 0.59).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1964, 0.72).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% Colloquial usage treats 'the Commerce Clause' as one constraint, but the label decomposes into three structurally distinct claims (epsilon-invariance): this expansive reading (broad perimeter; mixed coordination and enforced transfer), the originalist narrow reading (trade crossing state borders and instrumentalities of movement only), and the substantial-effects limited reading (aggregate-effects logic qualified by jurisdictional nexus and non-pretext requirements). Each warrants its own epsilon, beneficiary/victim sets, and classification. Genealogically the expansive reading's holdings constitute the doctrinal terrain within which the limited reading argues (hence the influences edge), while the narrow reading coexists as a rival framework held by different juristic factions. All three files link through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, powerful, 0.32).
constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
