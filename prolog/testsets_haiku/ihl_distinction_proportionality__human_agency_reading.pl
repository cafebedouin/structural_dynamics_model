% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction/Proportionality: Human Agency Reading
 *   domain: international/humanitarian/military_ethics
 *
 * SUMMARY:
 *   The international humanitarian law distinction and proportionality
 *   obligations sit at the intersection of military necessity, civilian
 *   protection, and technological change. This is ONE READING of a contested
 *   kernel: the Martens Clause and IHL's core principles. The human-agency
 *   reading asserts that IHL's distinction and proportionality duties require
 *   irreducible human moral judgment at the moment of lethal force
 *   application — machines cannot be delegated the power to make life-death
 *   decisions because accountability, moral agency, and compliance with the
 *   humanity principle are constitutive of IHL, not separable from it. This
 *   reading suppresses fully autonomous targeting systems categorically and
 *   authorizes only human-supervised autonomy. Two sibling readings contest
 *   this: the outcomes-based reading (IHL obligations bind outcomes, not
 *   means — technology-neutral compliance is possible if autonomous systems
 *   demonstrably achieve parity or superiority in distinction/proportionality
 *   performance) and the categorical-prohibition reading (Martens Clause
 *   principles of humanity and public conscience prohibit autonomous weapons
 *   per se, independent of technical performance — crossing the threshold of
 *   machine-decided killing violates human dignity intrinsically). The
 *   human-agency reading sits between these: it is not a categorical ban
 *   (machines can assist and augment human judgment) nor a pure outcomes test
 *   (process matters because human accountability is constitutive of IHL).
 *   The constraint extracts from military operational efficiency and weapons
 *   developers by imposing process requirements that raise cost and slow
 *   deployment. It benefits IHL interpretive authorities (especially the
 *   ICRC) by maintaining their centrality in defining what IHL permits.
 *   Civilian protection advocates benefit by anchoring their position in a
 *   process rule that is harder to game than outcomes tests. The measurement
 *   series tracks rising extractiveness and theater over the interval — as
 *   military organizations adapt to the constraint by developing nominal
 *   human-review procedures that preserve supervisory authority in form while
 *   delegating decision authority in practice (theater rising), and as the
 *   interpretive burden on the ICRC grows (suppression_requirement rising),
 *   the effective cost of maintaining this reading increases.
 *
 * KEY AGENTS:
 *   - ICRC and IHL interpretive authorities — maintain centrality by defining what the Martens Clause requires
 *   - Armed forces with autonomy investments — bear operational cost of human-supervision requirements
 *   - Weapons developers — face design constraints and market foreclosure from autonomous-targeting restrictions
 *   - Civilian protection advocates — benefit from process rule anchoring their normative position
 *   - State parties to Geneva Conventions — benefit from clear rule, constrained by treaty obligation
 *   - Competing armed forces — excluded from interpretive process, incentivized to reinterpret or bypass
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction/Proportionality: Human Agency Reading").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international/humanitarian/military_ethics").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'e52a7058-9ae6-4e83-a625-ad3727dcc22e').
narrative_ontology:cs_kernel_codification('e52a7058-9ae6-4e83-a625-ad3727dcc22e', fixed_text).
narrative_ontology:cs_authority_grounding('e52a7058-9ae6-4e83-a625-ad3727dcc22e', lineage).
narrative_ontology:cs_interpretation_layer_present('e52a7058-9ae6-4e83-a625-ad3727dcc22e').
narrative_ontology:cs_reading_relation('e52a7058-9ae6-4e83-a625-ad3727dcc22e', ihl_distinction_proportionality__outcomes_based_reading, influences).
narrative_ontology:cs_reading_relation('e52a7058-9ae6-4e83-a625-ad3727dcc22e', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_axiom('e52a7058-9ae6-4e83-a625-ad3727dcc22e', foundational, human_judgment_constitutive_of_ihl_compliance).
narrative_ontology:cs_axiom_status(human_judgment_constitutive_of_ihl_compliance, holdable).
narrative_ontology:cs_axiom_grounding('e52a7058-9ae6-4e83-a625-ad3727dcc22e', human_judgment_constitutive_of_ihl_compliance, deontological).
narrative_ontology:cs_axiom('e52a7058-9ae6-4e83-a625-ad3727dcc22e', foundational, moral_accountability_inseparable_from_targeting_decision).
narrative_ontology:cs_axiom_status(moral_accountability_inseparable_from_targeting_decision, holdable).
narrative_ontology:cs_axiom_grounding('e52a7058-9ae6-4e83-a625-ad3727dcc22e', moral_accountability_inseparable_from_targeting_decision, deontological).
narrative_ontology:cs_reference_frame('e52a7058-9ae6-4e83-a625-ad3727dcc22e', martens_clause_human_judgment_constitutive).
narrative_ontology:cs_drift_state('e52a7058-9ae6-4e83-a625-ad3727dcc22e', contemporary_autonomous_weapons_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e52a7058-9ae6-4e83-a625-ad3727dcc22e', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, armed_forces_with_autonomy_investments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, weapons_developers).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_protection_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, state_parties_to_geneva_conventions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Principally the International Committee of the Red Cross and its advisory bodies, but also national military legal counsel and treaty-state parties. They interpret the Martens Clause and IHL's distinction/proportionality obligations as mandating human judgment at the moment of lethal force application. They maintain that delegating targeting decisions to machines violates the human dignity principle embedded in IHL. Their authority derives from lineage (the Geneva Conventions and their Commentaries) and from practice (generations of IHL jurisprudence and state custom). They benefit from remaining the authoritative seat for what IHL permits; if outcomes-based interpretation prevails, their role recedes to monitoring technical performance rather than policing process.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Military organizations that have invested in autonomous weapon systems research, development, and deployment. They argue that human-in-the-loop requirements are operationally expensive, slow decision cycles below lethal-force timescales, and prevent legitimate military advantage in competitive environments. They bear the compliance cost of maintaining human decision authority over every targeting decision, which means delaying or abandoning systems that could be faster and cheaper if fully autonomous. Their exit is constrained by state sovereignty (they cannot simply ignore IHL obligations), though they can lobby for reinterpretation or technical workarounds (e.g., automated recommendations with nominal human review).
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, armed_forces_with_autonomy_investments, payer,
    institutional, biographical, constrained, global).

% Defense contractors and technology companies designing and building weapons systems. They carry extraction through design constraints (must architect systems for human supervisory authority even when the technical capability for full autonomy exists, which raises R&D costs and time-to-market). They also carry indirect extraction through market foreclosure: systems they could sell into autonomous-weapons markets are barred by this interpretation. Conversely, they collect some benefit if the human-in-the-loop constraint becomes a competitive moat (if they excel at human-machine interfaces and rivals do not, the constraint favors them).
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, weapons_developers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, weapons_developers, beneficiary).

% NGOs, academic networks, and policy advocates focused on protecting civilians in armed conflict. They argue that requiring human judgment at the targeting moment preserves accountability and moral responsibility — a machine cannot be held accountable for a proportionality error, but a human can. They benefit from the human-agency reading because it aligns with their advocacy position that IHL's humanitarian principles are non-negotiable and technology must conform to them, not replace them. Their exit is mobile: if outcomes-based reinterpretation prevails, they can shift to demanding outcome-auditing regimes, but their core narrative loses its normative force.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_protection_advocates, beneficiary,
    organized, generational, mobile, global).

% Nations that have ratified the Geneva Conventions and their protocols. They benefit from a clear IHL rule that prohibits fully autonomous weapons — it provides legal certainty and prevents an arms-control race to the bottom. They also carry extraction in the form of operational constraint and competitive disadvantage if rival states reinterpret the rule more permissively. Their exit is constrained by treaty obligation and by domestic law; they cannot simply ignore IHL without diplomatic and legal cost, though they can lobby for reinterpretation at treaty conferences.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, state_parties_to_geneva_conventions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, state_parties_to_geneva_conventions, payer).

% Military organizations in rival states that adopt different interpretations of IHL or operate outside the treaty regime. They would benefit from less restrictive interpretations of IHL (or ignoring it altogether) that permit faster autonomous systems. They are structurally excluded from the IHL interpretive process by state sovereignty and institutional fragmentation; they are not invited to the treaty negotiations or IHL commentary development.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, competing_armed_forces, excluded,
    institutional, immediate, trapped, global).

% Academic lawyers, philosophers, and policy analysts studying international humanitarian law. They analyze whether the human-agency reading is structurally sound, whether it can be maintained as technology advances, and whether it produces consistent outcomes across different conflict scenarios. They take no direct position in the constraint structure but can provide expertise that influences how the constraint's ambiguities resolve.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_researcher_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state practice and IHL interpretation around a shared principle: that human moral judgment must remain irreplaceable in targeting decisions. This solves a collective-action problem that would otherwise emerge if states raced to develop autonomous weapons with minimal human oversight — the coordination function is preventing a strategic arms-race-to-the-bottom in lethal autonomy.
% TRANSFER_FUNCTION: Transfers operational efficiency cost (slower decision cycles, higher computational overhead, design constraint) from the interpretive community (which gets to maintain centrality in defining IHL's meaning) to military organizations and weapons developers (who must architect systems for human supervisory authority and cannot deploy fully autonomous targeting).
% ABSENT_VOICES: Competing armed forces and non-state actors operating outside the Geneva Convention framework have no seat at the IHL interpretive table. Weapons-development engineers focused on optimization rather than compliance are excluded from the normativity-setting process. The voices of military officers who argue operational necessity would demand faster autonomy are present in military organizations but typically not as independent parties to the treaty process.
% DISAPPEARANCE_RATIONALE: If the human-agency reading vanished overnight and no interpretive authority maintained it, military organizations would rapidly deploy fully autonomous targeting systems (if technically feasible), operational efficiency would spike, civilian risk models would shift to whatever humans programmed them to be, and IHL interpretive authority would migrate toward outcomes-based review rather than process mandate. The strategic and humanitarian landscape would reorganize.
% FOUNDING_PROBLEM: Early concerns about autonomous weapon systems (post-2010) noted that delegating life-death decisions to machines could evade IHL accountability, lower firing thresholds by removing human hesitation, and create unpredictable lethal outcomes. The Martens Clause principle of humanity and public conscience was invoked to argue that some decisions are categorically human — not because humans are infallible, but because human moral agency and accountability are intrinsic to the principle.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC attests the founding problem is live and worsening as autonomy capabilities advance. Civilian protection advocates and most IHL scholars corroborate that the accountability gap is real. Competing military organizations and outcomes-based IHL researchers attest the problem is overstated — human operators commit proportionality violations regularly, and the accountability gap is not unique to machines. Independent technical audits (e.g., from MIT Media Lab, UC Berkeley) document that current 'human-in-the-loop' systems often reduce to nominal human review — the human reviews are theater, not real decision authority.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (high but not maximal) because the constraint does impose real operational costs (slower decision cycles, architectural constraints) that military organizations and developers must absorb. It is not maximal because the constraint has genuine coordination content: it prevents an arms-control race that would be worse for civilian protection. Suppression is high (0.72) because maintaining the human-agency reading requires active enforcement: military organizations are incentivized to develop nominal human-review procedures (theater), to lobby for reinterpretation, and to deploy systems that push the boundary of what 'human-supervised' means. Theater is moderate (0.28) because the constraint's enforcement increasingly relies on procedural theater — declaring that a human 'reviewed' a targeting recommendation that was algorithmically determined, or maintaining supervisory access that is never actually exercised in real-time. Accessibility collapse is high (0.78) because once the human-agency reading is institutionalized in treaty language and IHL commentary, alternatives (outcomes-based interpretation, categorical prohibition) become harder to reach without formal treaty amendment or state reinterpretation — states cannot simply ignore IHL, and the interpretive authority sits at the ICRC. Resistance is moderate (0.61) because military organizations mount real opposition (demanding outcomes-based testing, lobbying for research exceptions, developing workarounds) but lack the institutional legitimacy to simply override the ICRC's reading. The measuring grid is one shared across all three metrics at all time points (2026-06-12 grid alignment rule): every metric is authored at t=0,5,10,15,20,25. The series shows extractiveness stabilizing (observations to t=10, projections after) as the military adapts to the constraint and theater rises — the operational cost is front-loaded; suppression_requirement continuing to rise as the need to suppress workarounds intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The IHL interpretive authorities and the agenda-setter seat (the ICRC) should compute as beneficiary-aligned or even beneficiary-favorable: they get to define the meaning of IHL, maintain authority, and see their normative framework institutionalized. The armed-forces and weapons-developer seats should compute as heavily target-aligned: they bear the compliance cost, face market restrictions, and have constrained exit (they cannot simply leave the treaty regime). Civilian protection advocates compute as beneficiary-aligned because the process rule protects accountability and human agency, which is their core normative claim. This divergence arises from the structural data: beneficiaries (ICRC, civilian advocates) are the seats that get to adjudicate what IHL means; victims (military, developers) are the seats that must comply or absorb the cost of reinterpreting. The engine computes this from the declared roles and power atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the ICRC and IHL interpretive authorities is near 0.0 (full beneficiary): they collect the benefit of centrality and interpretive authority; they face minimal compliance cost (they are the interpreters, not the constrained parties). Directionality for armed forces and weapons developers is high (0.7–0.9, full target): they bear the operational cost, face market restrictions, and have constrained exit (state sovereignty binds them; they cannot simply leave the framework without diplomatic cost). Civilian protection advocates are near 0.2–0.3 (slight beneficiary): they benefit from the process rule but carry some cost if it creates military inefficiency that leads to civilian harm in other ways (a slower military response could cause collateral damage in some scenarios). State parties are near 0.5 (symmetric): they benefit from the clear rule and competitive fairness (no state gains advantage from looser interpretation if all are bound), but they carry the compliance burden and the operational friction. No directionality overrides are needed here; the derivation from beneficiary/victim + power + exit naturally produces the right d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The human-agency reading faces a mandatrophy risk: the founding problem (delegating life-death decisions to machines violates accountability and human agency) was live at t=0 (early 2010s, when autonomous systems were emerging). The reading's founding mandate was to preserve human judgment and accountability as technology advanced. However, the measurement series and field observation suggest the mandate is partially dead: military organizations have adapted by developing nominal human-review procedures (theater rising to 0.28), which technically preserve the form of human authority while delegating practical decision authority to machines. The constraint is enforced increasingly through procedure and theater rather than through real human moral judgment. At the same time, the theater rise correlates with suppression_requirement rising (now 0.72), which indicates the ICRC and interpretive authorities are expending more effort to maintain the fiction that supervision is real. The mandatrophy is not complete (the constraint still suppresses genuinely autonomous systems) but the drift toward performative compliance is visible. If this trend continues, the constraint risks becoming a Piton — a former Tangled Rope maintained by theatrical enforcement and interpretive authority, but with the underlying coordination function (preventing arms-control race) intact and the underlying extraction function (maintaining ICRC centrality) degraded as military workarounds become standard practice. The R5 genealogy check (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags this mandatrophy trajectory: the independent technical audits corroborate that human review is often theater, while the ICRC continues to insist the founding problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_naturality_vs_constructed,
    'Is the human-agency constraint a natural reflection of what IHL''s founding principles require (a discovered norm), or is it a constructed interpretation grounded in institutional and normative interests of the IHL interpretive community?',
    'Historical comparative analysis: study IHL''s application to previous technologies (aircraft, missiles, drones) to see whether human-agency requirements were consistently insisted upon or emerged contingently with autonomous weapons. Textual analysis of the Geneva Conventions'' originating documents to determine whether human judgment was explicitly mandated or is a later interpretation.',
    'If discovered norm: the constraint''s authority is higher, and reinterpretation toward outcomes-based compliance is illegitimate. If constructed: the constraint is subject to legitimate reinterpretation by states and the interpretive community, reducing its suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_naturality_vs_constructed, empirical, 'Whether human-agency requirement is foundational to IHL or is a later institutional interpretation.').

omega_variable(
    theater_vs_real_supervision,
    'What fraction of current human review of autonomous targeting recommendations represents real moral judgment versus procedural theater?',
    'Observational audit of military targeting processes: measure how often human reviewers reject algorithmic recommendations, how much time they spend on each review, how often their review changes the targeting decision, and whether they have real-time access to updated information.',
    'If theater is very high (>0.5), the constraint is degraded to Piton status — it persists through institutional inertia and ICRC authority rather than through real enforcement. If theater is low (<0.2), the constraint retains real suppressive force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_vs_real_supervision, empirical, 'Measurement of whether human supervision is substantive or performative.').

omega_variable(
    outcomes_equivalence_empirical,
    'Can autonomous systems demonstrably achieve distinction and proportionality performance equal to or exceeding human operators in relevant combat scenarios?',
    'Controlled trials comparing human and autonomous targeting decisions on standardized scenarios (urban, complex civilian/combatant mixtures, low-information situations). Independent technical evaluation commissioned by non-military research institutions.',
    'If outcomes are equivalent: the outcomes-based sibling reading gains legitimacy and the human-agency reading''s suppressive force diminishes (states could argue that the process requirement is satisfied if outcomes are equivalent). If humans demonstrably outperform on distinction/proportionality: the human-agency reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcomes_equivalence_empirical, empirical, 'Whether autonomous systems can match human distinction/proportionality performance.').

omega_variable(
    accountability_gap_mitigation,
    'Can meaningful accountability be assigned to autonomous weapons systems (through developers, commanders, or deployed software engineers) in a way that substitutes for human decision authority?',
    'Legal analysis of accountability chains in current military command structures and weapons development. Case study of incidents where autonomous or semi-autonomous weapons caused civilian harm and attempt to assign responsibility.',
    'If accountability cannot be established: the human-agency constraint remains justified on grounds of preserving moral responsibility. If accountability can be clearly established through institutional means: the constraint''s justification weakens (humans can still be held accountable even if they supervise rather than decide).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_gap_mitigation, conceptual, 'Whether accountability can substitute for decision authority in satisfying IHL principles.').

omega_variable(
    reading_logical_relationship,
    'Does the human-agency reading logically foreclose the outcomes-based reading, or can a single framework hold both simultaneously?',
    'Formal logical analysis: if a state were to adopt the human-agency reading while also measuring outcomes-equivalence compliance, would it face a logical contradiction or simply a dual compliance gate?',
    'If foreclosure obtains: the readings are truly incompatible frameworks; one must prevail. If not: the readings can coexist as different compliance pathways, and the engine should classify the relationship as coexists_with rather than forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_relationship, conceptual, 'Logical compatibility of the human-agency and outcomes-based readings within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_dph_tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ihl_dph_tr_t0, observed).
narrative_ontology:measurement(ihl_dph_tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(ihl_dph_tr_t5, observed).
narrative_ontology:measurement(ihl_dph_tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(ihl_dph_tr_t10, observed).
narrative_ontology:measurement(ihl_dph_tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(ihl_dph_tr_t15, projected).
narrative_ontology:measurement(ihl_dph_tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(ihl_dph_tr_t20, projected).
narrative_ontology:measurement(ihl_dph_tr_t25, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(ihl_dph_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ihl_dph_be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(ihl_dph_be_t0, observed).
narrative_ontology:measurement(ihl_dph_be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(ihl_dph_be_t5, observed).
narrative_ontology:measurement(ihl_dph_be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(ihl_dph_be_t10, observed).
narrative_ontology:measurement(ihl_dph_be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ihl_dph_be_t15, projected).
narrative_ontology:measurement(ihl_dph_be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ihl_dph_be_t20, projected).
narrative_ontology:measurement(ihl_dph_be_t25, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ihl_dph_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl_dph_su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(ihl_dph_su_t0, observed).
narrative_ontology:measurement(ihl_dph_su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(ihl_dph_su_t5, observed).
narrative_ontology:measurement(ihl_dph_su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(ihl_dph_su_t10, observed).
narrative_ontology:measurement(ihl_dph_su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ihl_dph_su_t15, projected).
narrative_ontology:measurement(ihl_dph_su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ihl_dph_su_t20, projected).
narrative_ontology:measurement(ihl_dph_su_t25, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ihl_dph_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, martens_clause_operationalization).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, accountability_in_autonomous_weapons).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel. The human-agency reading asserts that process (human judgment at the targeting moment) is constitutive of IHL compliance. Sibling readings — outcomes-based and categorical-prohibition — contest what IHL compliance entails. All three stories share a referent (the standing practice of IHL interpretation regarding autonomous weapons) but author different ε values: human-agency reading ε=0.68 (substantial extraction from military efficiency, high suppression); outcomes-based reading ε should be lower (process-neutral, smaller operational cost); categorical-prohibition reading ε should be higher (categorical rejection is more extractive). They are linked by network.affects_constraints because each sibling's adoption changes the legitimacy conditions and institutional pressure on the others. The human-agency reading INFLUENCES the outcomes-based reading by maintaining process gate, and INFLUENCES the categorical-prohibition reading by accepting a middle path.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
