% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test (Aggregate Substantial Effects Reading)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the commerce_clause_scope kernel:
 *   the broad-effects reading, under which commerce includes any economic
 *   activity that substantially affects interstate commerce when aggregated
 *   across all similarly situated actors, 'regulate' includes prohibition and
 *   comprehensive displacement of state law, and federal power therefore
 *   reaches purely intrastate activity with cumulative national economic
 *   impact. Established in Wickard v. Filburn (1942), deployed at scale in
 *   the Civil Rights Act cases (Heart of Atlanta Motel, Katzenbach v.
 *   McClung, 1964), and reaffirmed against a state-authorized contrary scheme
 *   in Gonzales v. Raich (2005), this reading produces an expansive effective
 *   victim set — virtually all economic activity is potentially reachable —
 *   while retaining a genuine coordination function (single national market,
 *   uniform commercial rules, national nondiscrimination enforcement). The ε
 *   referent is the standing broad-effects arrangement as operated 1942–2026,
 *   assessed by this reading's own lights: the reading disputes the normative
 *   valence of the transfer, not the magnitudes — authority, compliance cost,
 *   and policy-setting access demonstrably move from state and local seats to
 *   the federal center, and ε records that movement. Claim and metrics are
 *   authored independently: claimed_type tangled_rope states my structural
 *   belief (real coordination function PLUS asymmetric transfer PLUS active
 *   enforcement); the metrics state what I believe descriptively true of the
 *   arrangement's operation. The sibling readings (narrow_originalist,
 *   intermediate_channels) are separate constraints with their own ε, victim
 *   sets, and classifications; they are linked via
 *   network.affects_constraints and are not averaged into this file. KEY
 *   AGENTS (by structural relationship): - federal_regulatory_agencies:
 *   agenda-setter and principal collector of expanded jurisdiction
 *   (institutional / identity_locked) - congressional_majorities:
 *   agenda-setter (institutional / constrained) — enacts the statutes whose
 *   reach the reading sustains - federal_courts: agenda-setter and
 *   boundary-keeper (institutional / constrained) — defines, polices, and
 *   periodically widens the doctrine - national_interest_groups: primary
 *   beneficiary (organized / mobile) — converts fifty-state campaigns into
 *   one national enactment - civil_rights_enforcement_coalitions: primary
 *   beneficiary (organized / mobile) — the public-accommodations victories
 *   rest on this reading - national_business_enterprises: dual-positioned
 *   beneficiary-payer (powerful / arbitrage) — buys uniformity, pays
 *   compliance - state_governments: primary target (institutional / trapped)
 *   — police powers subsumed, recourse limited to litigation -
 *   local_economic_actors: primary target (powerless / trapped) — purely
 *   intrastate activity reached by aggregation - state_policy_innovators:
 *   target (moderate / constrained) — experimental programs displaced by
 *   federal occupation of the field - municipal_governments: excluded voice
 *   (moderate / trapped) — preempted ordinances, no standing -
 *   constitutional_law_scholars: analytical observer (analytical /
 *   analytical)
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: agenda-setter and principal collector of expanded jurisdiction (institutional / identity_locked) — administers the rules the reading makes available; mission-fused with the mandate
 *   - congressional_majorities: agenda-setter (institutional / constrained) — electoral incentives favor national action over federal restraint
 *   - federal_courts: agenda-setter and boundary-keeper (institutional / constrained) — announces limits and then absorbs them back into the framework
 *   - national_interest_groups: primary beneficiary (organized / mobile) — uniform national policy won once instead of fifty times
 *   - civil_rights_enforcement_coalitions: primary beneficiary (organized / mobile) — national nondiscrimination enforcement exists only inside this reading's scope
 *   - national_business_enterprises: dual-positioned beneficiary-payer (powerful / arbitrage) — purchases uniformity, pays the compliance bill, retains forum-shopping leverage
 *   - state_governments: primary target (institutional / trapped) — residual police powers shrink on each aggregate-effects finding; exit closed
 *   - local_economic_actors: primary target (powerless / trapped) — home-consumption-scale activity reached by classwide aggregation
 *   - state_policy_innovators: target (moderate / constrained) — laboratory-of-democracy pilots terminated by federal occupation of the field
 *   - municipal_governments: excluded voice (moderate / trapped) — preempted locally, no standing to object
 *   - constitutional_law_scholars: analytical observer (analytical / analytical) — documents the announced-limits versus practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.66).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.62).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.66).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test (Aggregate Substantial Effects Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "legal/constitutional").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'c4323f3d-82bf-4b47-a359-10ade12f426d').
narrative_ontology:cs_kernel_codification('c4323f3d-82bf-4b47-a359-10ade12f426d', fixed_text).
narrative_ontology:cs_authority_grounding('c4323f3d-82bf-4b47-a359-10ade12f426d', lineage).
narrative_ontology:cs_interpretation_layer_present('c4323f3d-82bf-4b47-a359-10ade12f426d').
narrative_ontology:cs_reading_relation('c4323f3d-82bf-4b47-a359-10ade12f426d', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('c4323f3d-82bf-4b47-a359-10ade12f426d', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('c4323f3d-82bf-4b47-a359-10ade12f426d', foundational, aggregate_effects_confer_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_effects_confer_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c4323f3d-82bf-4b47-a359-10ade12f426d', aggregate_effects_confer_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('c4323f3d-82bf-4b47-a359-10ade12f426d', foundational, regulate_includes_prohibition).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('c4323f3d-82bf-4b47-a359-10ade12f426d', regulate_includes_prohibition, conventional).
narrative_ontology:cs_axiom('c4323f3d-82bf-4b47-a359-10ade12f426d', secondary, tenth_amendment_no_independent_limit).
narrative_ontology:cs_axiom_status(tenth_amendment_no_independent_limit, holdable).
narrative_ontology:cs_axiom_grounding('c4323f3d-82bf-4b47-a359-10ade12f426d', tenth_amendment_no_independent_limit, conventional).
narrative_ontology:cs_reference_frame('c4323f3d-82bf-4b47-a359-10ade12f426d', plenary_national_economic_authority).
narrative_ontology:cs_drift_state('c4323f3d-82bf-4b47-a359-10ade12f426d', contemporary_post_raich_settlement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c4323f3d-82bf-4b47-a359-10ade12f426d', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_business_enterprises).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_policy_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, national_business_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the rules the broad reading makes available: air and water standards, workplace safety, drug approval, financial conduct. Each successful assertion of the power adds jurisdiction, staff, and budget. Agency missions are written in the language of federal regulatory mandates; an agency asked to hand the power back would be surrendering what it is. Exit is not a thing an agency does — its identity and its work have become the same thing.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary).

% Enact statutes under the aggregate-effects power: national labor law, environmental statutes, civil rights acts, criminal provisions reaching local conduct. Electoral cycles reward visible national action; deference to state authority rewards nothing measurable. Members can vote against federal reach but rarely do, because constituents ask Washington to solve problems the states have not solved.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, congressional_majorities, agenda_setter,
    institutional, biographical, constrained, national).

% Define where the power stops. Announced limits — a jurisdictional element here, a non-economic activity carve-out there — have been stated and then largely absorbed back into the aggregate-effects framework. The bench polices the boundary it drew and periodically redraws it; its authority rests on maintaining a coherent line of precedent, which cuts against abrupt contraction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_courts, observer).

% Campaign for uniform national rules — labor standards, environmental floors, consumer protection — because winning once in Washington beats winning fifty times in state capitols. What flows to them is durable nationwide policy; what they forgo is the ability to tailor demands to local conditions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Won the public-accommodations cases of 1964 on this reading: motels and restaurants serving interstate travelers were reached through the aggregated effects of local discriminatory practices. Their policy inheritance — enforceable national nondiscrimination in commerce — exists only inside this reading's scope, which makes them its most morally invested defenders.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions, beneficiary,
    organized, generational, mobile, national).

% Operate across state lines and prefer one federal standard to fifty divergent ones; uniformity lowers compliance-design costs and simplifies market entry. At the same time they pay the compliance bill itself, and scale lets large firms absorb costs in ways that disadvantage smaller rivals. They can relocate operations, select favorable forums, and lobby for tailored carve-outs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_business_enterprises, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, national_business_enterprises, payer).

% Hold residual police powers over health, safety, morals, and welfare that shrink whenever Congress identifies an aggregate economic effect behind a subject. They retain the machinery of governance but increasingly administer federal programs under federal conditions. They cannot leave the union, and the amendment threshold for restoring authority is effectively unreachable; their recourse is litigation, which they bring in coalitions and usually lose.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, trapped, regional).

% Farmers growing feed for their own animals, patients growing medicine for their own use, small holders whose purely local activity becomes regulable once everyone like them is counted together. They never entered any market across state lines; the arithmetic of aggregation reaches them anyway. Relocation does not help — the counting follows them anywhere in the country.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_actors, payer,
    powerless, biographical, trapped, local).

% Legislatures and agencies piloting novel approaches — cannabis legalization, new gun-safety schemes, data-privacy regimes — find the field already occupied by federal statutes enacted under the same power. Experimentation continues only in gaps Congress has left open; the pilot program ends when the federal rule arrives.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_policy_innovators, payer,
    moderate, biographical, constrained, regional).

% Cities and counties whose local ordinances are displaced by federal law. They typically lack standing to challenge preemption and have no seat where federal statutes are drafted; their objections surface, when they surface at all, as amicus briefs in other parties' litigation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, municipal_governments, excluded,
    moderate, generational, trapped, local).

% Map the doctrine's boundaries, document the gap between announced limits and actual practice, and supply the theoretical vocabulary in which the competing readings argue. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective-action problems among the states that no state can solve alone: interstate trade barriers and tolls, races to the bottom in labor and environmental standards, cross-border pollution and spillover externalities, and free-rider problems in national markets. It supplies a single set of commercial rules for a continental economy and, historically, supplied the only workable vehicle for national civil-rights enforcement against locally entrenched discrimination.
% TRANSFER_FUNCTION: Moves regulatory decision-making authority from state capitals and local communities to the federal government; moves compliance costs onto local and intrastate economic actors whose activity is reached by aggregation; and moves policy-setting access and durable-rule rents to national organizations with the resources to litigate and lobby in Washington.
% ABSENT_VOICES: Municipal governments whose ordinances are preempted — they usually lack standing and are absent from the drafting process. Citizens who value state-level policy diversity have no procedural seat; their preference surfaces only indirectly through state officials who themselves litigate from a losing position. State-sovereignty advocates are heard, but systematically as unsuccessful litigants rather than as participants in the rule-making that displaces them.
% DISAPPEARANCE_RATIONALE: If the broad reading vanished overnight, the constitutional foundation of a large share of the federal administrative state — national labor law, environmental statutes, food and drug regulation, financial oversight, federal criminal provisions reaching local conduct — would collapse or require immediate re-grounding in other powers. Public-accommodations nondiscrimination would revert to state-by-state enforcement. National market rules would fragment toward fifty regimes, and every seat in the structure would renegotiate its position within months.
% FOUNDING_PROBLEM: The failure of the Articles of Confederation: states erected tariffs and trade barriers against one another, issued competing currencies, and could not coordinate debt service or a common market. The Philadelphia Convention's central economic problem was giving a national government power over commerce among the states; the New Deal crisis later added a second founding problem — managing a genuinely national industrial economy through depression and war.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated well outside the current beneficiary set: the documented record of 1780s state trade barriers and tariff wars (Madison's convention notes, contemporaneous state statutes, British consular reports), economic histories of the antebellum and interwar periods, and the concession of originalist scholars themselves that interstate trade friction was real — they dispute the solution's scope, not the problem's existence. Modern corroboration comes from cross-border externality literature produced by economists with no stake in federal institutional budgets.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66 at interval end) is substantial but not maximal: the arrangement genuinely solves interstate coordination problems, yet each exercise of the power transfers binding authority away from state and local seats and reaches conduct (home-consumed wheat, home-grown medicine) that crosses no state line. Suppression (0.62) is a raw structural property, unscaled by power or scope: states cannot exit the union, the amendment threshold is practically closed, and preemption operates automatically under the Supremacy Clause; it is tempered by the persistence of state litigation, cooperative-federalism bargaining, and the Court's occasional announced limits. Theater ratio (0.36) reflects the growing share of doctrinal activity that is performative — the 'substantial effects,' jurisdictional-element, and non-economic-activity limits announced in Lopez and Morrison function largely as rhetoric absorbed back by Raich, while the real work is done by an undefended presumption of federal reach. Accessibility collapse (0.52): once the reading is understood, state alternatives persist only in gaps Congress leaves; they are partly, not wholly, collapsed. Resistance (0.62): continuous and real — multi-state litigation coalitions, Raich itself, sanctuary and legalization movements operating in deliberate defiance of federal reach — but structurally losing. The measurement series run on ONE shared time grid (1942, 1958, 1968, 1976, 1995, 2005, 2012, 2026) with every tracked metric authored at every point. The trajectory is a ratchet with pauses, not a cycle: steady expansion 1942–1976, a brief contraction signal at Lopez/Morrison (1995–2000) visible as dips in suppression and extractiveness, re-expansion at Raich (2005), a second pause at NFIB (2012, where the individual mandate failed under the Commerce Clause specifically), and resumed drift since. The oscillation is not intermittent reinforcement — it tracks judicial-composition events, not a strategic escalation cycle. Endpoint values match the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The federal seats and the state/local seats experience the same instrument as opposite things. From the federal seats, the arrangement is nation-building coordination they built, staffed, and defend: agencies see mandates, Congress sees solved constituent problems, courts see a coherent precedent line. From the state and local seats, the same structure operates as subsumption: police powers that shrink on each aggregate-effects finding, pilot programs that die on federal arrival, home-scale activity regulated by arithmetic. The sharpest internal divergence is the courts' own: they announce limits (Lopez, Morrison, the NFIB commerce-clause holding) whose practical effect they then largely neutralize (Raich), so the boundary-keeping seat alternates between the two experiences within a single decade. The identity-lock dynamic on the agency seat is institutional fusion: decades of statutory mandates have made the agencies' organizational self-concept and the broad reading mutually constitutive; if the identity frame broke — if agencies could conceive of their function surviving a narrower power — the enforcement coalition defending the reading would fracture from inside.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality (subsidy-side): federal_regulatory_agencies (also agenda-setters — they run the machinery and collect jurisdiction, budget, and personnel), national_interest_groups and civil_rights_enforcement_coalitions (collect durable national policy without running the enforcement apparatus), and national_business_enterprises (buy uniformity). Declared victims derive high directionality (target-side): state_governments (trapped — no secession, no amendment path, litigation-only recourse pushes them toward the full-target end), local_economic_actors (powerless and trapped — aggregation reaches them wherever they are), and state_policy_innovators (constrained — experimentation permitted only in federal gaps). One override is authored: power_atom 'powerful' to d=0.45. The structural derivation would flatten national_business_enterprises toward the beneficiary end on the strength of their beneficiary declaration, but their situation is genuinely dual — they pay the compliance bill and their scale advantages reshape markets against smaller rivals — placing them near symmetric rather than subsidy-side. The override corrects exactly this flattening; no other seat shares the powerful atom, so the correction cannot misfire elsewhere. Federal courts and state governments share the institutional atom, so no per-atom override can differentiate them; courts are left to the engine's fallback for undeclared agents, and their boundary-keeping position is documented here in commentary rather than forced through an override that would also move the states.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interstate trade friction, later joined by national economic management — is live, and the arrangement's disappearance would rearrange the world, so the R5 mismatch flag (dead-status-plus-rearrangement) does not fire: this is not a zombie mandate. The tangled_rope classification earns its keep by blocking two symmetrical misreads. Reading the arrangement as pure extraction (snare) erases the coordination function that is real and load-bearing: a continental economy genuinely cannot run on fifty incompatible commercial codes, and the civil-rights enforcement legacy is a moral asset the structure actually delivered. Reading it as pure coordination (rope) erases the asymmetry that is equally real: the same statute that integrates the market transfers binding authority from every state capital to Washington and reaches the farmer's home-consumed wheat through nobody's consent. The theater ratio matters diagnostically here: the announced limiting principles perform constraint while the operative rule is near-plenary reach, which is precisely the signature of a coordination structure carrying an extraction payload it does not officially acknowledge. Mandatrophy is not resolved and should not be declared — the mandate and the function still correspond; what is contested is the breadth, which is the kernel contest itself, routed to the omega variables below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the broad-effects reading the correct instantiation of the commerce_clause_scope kernel, or do the sibling readings (narrow_originalist, intermediate_channels) better capture the founding commitment?',
    'Sustained doctrinal development under successive judicial appointments, or a constitutional-theoretic account that ranks the readings by fidelity to the text''s original public meaning and subsequent interpretive tradition.',
    'Under narrow_originalist the victim set collapses to cross-border traders and the federal transfer from state sovereignty largely disappears; under intermediate_channels aggregation survives only for economic activity with jurisdictional elements, shrinking the victim set substantially and lowering effective extraction from every state and local seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: which reading of the commerce kernel this constraint instantiates, and what the sibling readings would change structurally.').

omega_variable(
    aggregation_limiting_principle,
    'Does the aggregation doctrine have a principled stopping point, or does cumulative-effects reasoning extend federal power to every economic activity without limit?',
    'The litigation record: a sustained line of cases where aggregation fails for genuinely economic activity, or economic analysis identifying classes of activity with no classwide market effect.',
    'If unbounded, the victim set is effectively all economic activity and effective extraction trends toward the ceiling the structure permits; a principled cutoff would bound the victim set and bend the long-run extractiveness trajectory downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_limiting_principle, empirical, 'Whether the Wickard-to-Raich trajectory reflects a bounded principle or an unbounded one.').

omega_variable(
    political_safeguards_adequacy,
    'Do the political safeguards of federalism — the states'' representation in Congress — adequately protect state interests, such that the structural extraction measure overstates the net harm to the state seat?',
    'Comparative tracking of state-interest defeats versus protections across Congresses; roll-call analysis of preemption legislation and of the state-locality lobbying channel.',
    'If the safeguards work, effective extraction from the state seat is materially lower than the structural measure suggests and the arrangement sits closer to the coordination end; if they fail, the state seat is effectively voiceless and the measured extraction stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_safeguards_adequacy, empirical, 'Whether process-based protection of states substitutes for structural limits on federal reach.').

omega_variable(
    civil_rights_entanglement,
    'How much of the broad reading''s legitimacy and durability depends on its civil-rights enforcement history, and can any candidate narrowing preserve the Heart of Atlanta outcomes?',
    'Counterfactual doctrinal analysis: test each proposed limiting principle against the 1964 public-accommodations cases; survey whether narrowed readings sustain them.',
    'If narrowing sacrifices civil-rights enforcement, reform coalitions fracture and the broad reading persists regardless of its extraction profile; if the outcomes are preservable under intermediate_channels, the sibling reading becomes politically viable and this reading''s dominance is contingent rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_entanglement, conceptual, 'The moral-asset entanglement that stabilizes the broad reading against doctrinal contraction.').

omega_variable(
    settlement_stability,
    'Is the post-Raich settlement stable, or will continued appointment drift produce another contraction-and-rebound cycle?',
    'Track certiorari grants and panel composition in commerce-clause challenges across successive Court terms.',
    'Determines whether the 2026 endpoint measurements represent a steady state or a waypoint; a renewed contraction cycle would date any type transition differently and would shift the theater-ratio trajectory (announced limits performing constraint) in the opposite direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settlement_stability, empirical, 'Forward-looking stability of the current doctrinal equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1942, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_scope__broad_effects_test, theater_ratio, 1942, 0.15).
narrative_ontology:measurement(comm_tr_t1958, commerce_clause_scope__broad_effects_test, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(comm_tr_t1968, commerce_clause_scope__broad_effects_test, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(comm_tr_t1976, commerce_clause_scope__broad_effects_test, theater_ratio, 1976, 0.26).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__broad_effects_test, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_scope__broad_effects_test, theater_ratio, 2026, 0.36).

% Extraction over time
narrative_ontology:measurement(comm_be_t1942, commerce_clause_scope__broad_effects_test, base_extractiveness, 1942, 0.45).
narrative_ontology:measurement(comm_be_t1958, commerce_clause_scope__broad_effects_test, base_extractiveness, 1958, 0.5).
narrative_ontology:measurement(comm_be_t1968, commerce_clause_scope__broad_effects_test, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(comm_be_t1976, commerce_clause_scope__broad_effects_test, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__broad_effects_test, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_scope__broad_effects_test, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1942, commerce_clause_scope__broad_effects_test, suppression_requirement, 1942, 0.4).
narrative_ontology:measurement(comm_su_t1958, commerce_clause_scope__broad_effects_test, suppression_requirement, 1958, 0.48).
narrative_ontology:measurement(comm_su_t1968, commerce_clause_scope__broad_effects_test, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(comm_su_t1976, commerce_clause_scope__broad_effects_test, suppression_requirement, 1976, 0.56).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__broad_effects_test, suppression_requirement, 2012, 0.59).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_scope__broad_effects_test, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% Colloquial reference to 'the Commerce Clause' names one provision but decomposes into at least three structurally distinct constraints (epsilon-invariance decomposition): this broad-effects reading (expansive victim set, high sovereignty transfer, epsilon ~0.66), narrow_originalist (cross-border trade only, regulate-as-facilitate semantics, negligible extraction from intrastate actors), and intermediate_channels (three recognized categories with limiting principles, intermediate victim set). Their epsilon values differ widely because they are different constraints, not one constraint viewed from angles. The upstream/downstream structure runs FROM this reading: its precedents (Wickard, Raich) are the operative law that both siblings propose to replace, so this story's network edges point at both siblings. Each sibling file must carry reciprocal edges and its own decomposition note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
