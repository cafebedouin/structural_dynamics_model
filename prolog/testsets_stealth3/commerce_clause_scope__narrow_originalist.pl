% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope — Narrow Originalist Reading (Trade-Crossing-Lines Facilitation)
 *   domain: constitutional law/federalism
 *
 * SUMMARY:
 *   The narrow originalist reading of the Commerce Clause allocates
 *   regulatory authority between the federal government and the states:
 *   Congress may remove state-imposed barriers to trade crossing state lines
 *   and keep the commercial rules governing that trade uniform, and may do
 *   nothing else — 'regulate' means make regular, not restrict, and commerce
 *   means trade that crosses state lines. The arrangement this story is about
 *   is that allocation as the reading claims it; epsilon is authored from the
 *   reading's own lights over that fixed referent, so the reading's own
 *   assessment (low extraction from state sovereignty, a genuine
 *   trade-facilitation function) is recorded alongside the structural fact of
 *   who bears the arrangement's costs. The claim/metric gap is deliberate and
 *   is the datum: the reading claims rope, while the metrics describe an
 *   arrangement whose operative life peaked in the Lochner era, was displaced
 *   by the broad reading after 1937, and is now maintained substantially
 *   theatrically with partial revival gestures (Lopez, Morrison) re-narrowed
 *   by Raich. This story is one member of the commerce_clause_scope
 *   constraint family; the sibling readings are separate stories linked by
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setter (institutional/analytical) — the only
 *   seat that can adopt or discard the reading - congress: primary federal
 *   seat bound by the limit (institutional/constrained) - state_governments:
 *   primary beneficiary of retained jurisdiction (institutional/constrained)
 *   - local_businesses: beneficiary of federal-regulatory immunity
 *   (moderate/constrained) - interstate_commerce_firms: dual-positioned —
 *   uniform interstate rules gained, local patchwork borne (powerful/mobile)
 *   - national_labor_market_workers /
 *   civil_rights_claimants_in_recalcitrant_states /
 *   pollution_exposure_communities: cost-bearing constituencies
 *   (moderate-to-powerless / constrained-to-trapped) - federalism_scholars:
 *   analytical observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter (institutional/analytical) — decides which reading governs; the only seat that can adopt or discard the narrow allocation
 *   - congress: primary federal seat bound by the limit (institutional/constrained) — loses jurisdiction over intrastate and noncommercial activity
 *   - state_governments: primary beneficiary (institutional/constrained) — retain regulatory authority over intrastate economic life
 *   - local_businesses: beneficiary (moderate/constrained) — exempt from federal regulation of local activity
 *   - interstate_commerce_firms: dual-positioned beneficiary/payer (powerful/mobile) — uniform rules for interstate trade, patchwork for local operations
 *   - national_labor_market_workers, civil_rights_claimants_in_recalcitrant_states, pollution_exposure_communities: cost-bearing constituencies (moderate-to-powerless / constrained-to-trapped) — lose federal protections the reading places beyond Congress's power
 *   - federal_regulatory_agencies: institutional payer (institutional/constrained) — lose caseload, budget, and jurisdiction
 *   - federalism_scholars: analytical observer — produces the interpretive arguments; collects nothing, bears nothing
 *   - local_harm_victims: excluded seat (powerless/trapped) — harmed by purely local noncommercial activity, with no federal champion and no seat in the allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.18).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.22).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.18).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope — Narrow Originalist Reading (Trade-Crossing-Lines Facilitation)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '06547a65-d588-4e3d-9d62-00084fe5cb04').
narrative_ontology:cs_kernel_codification('06547a65-d588-4e3d-9d62-00084fe5cb04', fixed_text).
narrative_ontology:cs_authority_grounding('06547a65-d588-4e3d-9d62-00084fe5cb04', lineage).
narrative_ontology:cs_interpretation_layer_present('06547a65-d588-4e3d-9d62-00084fe5cb04').
narrative_ontology:cs_reading_relation('06547a65-d588-4e3d-9d62-00084fe5cb04', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('06547a65-d588-4e3d-9d62-00084fe5cb04', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('06547a65-d588-4e3d-9d62-00084fe5cb04', foundational, commerce_denotes_cross_border_trade).
narrative_ontology:cs_axiom_status(commerce_denotes_cross_border_trade, holdable).
narrative_ontology:cs_axiom_grounding('06547a65-d588-4e3d-9d62-00084fe5cb04', commerce_denotes_cross_border_trade, empirically_contingent).
narrative_ontology:cs_axiom('06547a65-d588-4e3d-9d62-00084fe5cb04', foundational, regulate_means_facilitate_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('06547a65-d588-4e3d-9d62-00084fe5cb04', regulate_means_facilitate_not_restrict, empirically_contingent).
narrative_ontology:cs_axiom('06547a65-d588-4e3d-9d62-00084fe5cb04', secondary, intrastate_economic_activity_reserved_to_states).
narrative_ontology:cs_axiom_status(intrastate_economic_activity_reserved_to_states, holdable).
narrative_ontology:cs_axiom_grounding('06547a65-d588-4e3d-9d62-00084fe5cb04', intrastate_economic_activity_reserved_to_states, conventional).
narrative_ontology:cs_reference_frame('06547a65-d588-4e3d-9d62-00084fe5cb04', founding_trade_facilitation_bargain).
narrative_ontology:cs_drift_state('06547a65-d588-4e3d-9d62-00084fe5cb04', contemporary_doctrine, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('06547a65-d588-4e3d-9d62-00084fe5cb04', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_commerce_firms).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_labor_market_workers).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, pollution_exposure_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, congress).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, interstate_commerce_firms).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, original_public_meaning_method).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, federalism_laboratories_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court decides what the Commerce Clause requires: which federal statutes fall within Congress's commerce power and which exceed it. Its majority doctrine since 1937 has upheld comprehensive federal regulation of economic activity; a persistent line of opinions and dissents argues the Clause reaches only trade crossing state lines and that 'regulate' means making trade regular rather than restricting or prohibiting activity. The Court's composition determines whether the narrow allocation governs; no other institution can adopt or discard it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enacts federal statutes relying on the commerce power — labor standards, environmental rules, civil rights protections, criminal statutes. Under this reading, statutes regulating purely local or noncommercial activity exceed its authority and fall. Members face constituent demand for federal action in exactly the domains the reading closes off; the institution cannot exit the constitutional allocation and must litigate for every expansion of its jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, payer,
    institutional, biographical, constrained, national).

% Retain primary regulatory authority over economic activity within their borders — labor conditions, local commerce, land use, professional licensing. Under this reading no federal statute displaces their choices for intrastate activity, and they compete with one another over regulatory packages. They cannot leave the union, and their retained autonomy exists only so long as the courts hold the line they favor.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, constrained, regional).

% Businesses operating within a single state answer only to their state's rules for their local activity and carry no federal commercial-regulation surface for it. They hold a lighter compliance burden than national competitors, though they inherit whatever regulatory package their own state chooses to impose and cannot shop jurisdictions without relocating.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, constrained, local).

% Firms whose trade crosses state lines get a single federal authority that keeps commercial rules uniform for the interstate component of their business — the original function of the Clause. Their operations inside each state remain subject to that state's distinct regulatory regime, so they navigate up to fifty regulatory environments for the local components and lobby both Congress and the states accordingly.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_commerce_firms, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, interstate_commerce_firms, payer).

% Workers employed by national employers lose federal labor standards — wages, hours, collective bargaining protections — for any employment the reading places outside Congress's commerce power, falling back on whatever their state provides. They can change jobs within the labor market but cannot exit the allocation that decides which protections exist.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_labor_market_workers, payer,
    moderate, biographical, constrained, national).

% People facing discrimination by local businesses — hotels, restaurants, theaters — lose the federal public-accommodations and anti-discrimination framework for establishments the reading places beyond Congress's power, and must rely on state law in the very jurisdictions where discrimination is most entrenched. Their protected characteristics are not portable; moving states is the only exit and it is costly.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states, payer,
    powerless, biographical, trapped, local).

% Communities living near industrial sources of pollution lose federal environmental regulation for any emission source the reading classifies as purely local or noncommercial, relying on state environmental agencies whose capacity and willingness vary widely. Harms accumulate over decades; relocating away from a pollution source is possible for some residents and impossible for others.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, pollution_exposure_communities, payer,
    powerless, generational, constrained, local).

% Agencies administering federal labor, environmental, and civil rights programs lose jurisdiction over the activities the reading removes from Congress's power — with it go their caseloads, budgets, and staffing. The institution cannot recover the jurisdiction on its own; it depends on Congress's authority and on the courts' willingness to uphold it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Constitutional historians, law professors, and originalist and living-constitutionalist theorists produce the interpretive arguments the Court draws on. They hold seats in the debate that decides the reading but collect nothing from its operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federalism_scholars, observer,
    analytical, generational, analytical, national).

% People harmed by purely local, noncommercial activity — violence, hazards, private mistreatment by noncommercial actors — have no federal legislative champion under this reading and no seat in the federalism allocation that decides their protection; they enter the constitutional conversation only as litigants in cases they rarely choose and cannot afford.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_harm_victims, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate-trade collective-action problem: absent a single authority, states erect tariffs, discriminatory duties, and retaliatory barriers against one another's goods — the Articles-of-Confederation trade war. The arrangement empowers one legislature to remove state-imposed barriers to trade crossing state lines and to keep the commercial rules governing that trade uniform, and leaves everything else to the states.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction over economic activity from the federal government to state governments, and correspondingly moves the burden of providing labor, civil-rights, and environmental protections from federal institutions to state institutions — and, where states decline to provide them, to the affected individuals themselves. It also moves compliance costs: local businesses shed the federal regulatory surface that national firms carry.
% ABSENT_VOICES: The protection-constituencies — workers in national labor markets, civil-rights claimants in recalcitrant states, pollution-exposed communities — hold no seat in the allocation that decides their protection; they reach the constitutional conversation only through Congress, whose authority the reading removes, and through litigation they rarely can afford. Historically, the founding-era public whose assent the reading reconstructs excluded enslaved people, women, and the propertyless — that 'public meaning' was never theirs to give, and their descendants inherit the allocation without having been parties to it.
% DISAPPEARANCE_RATIONALE: If the narrow allocation disappeared overnight — if the broad reading simply took the Clause's field — federal regulatory power would expand to every economic activity with aggregate national effects, thousands of state regulatory choices over intrastate life would be displaced by federal ones, and the compliance surface of every multi-state business would reorganize around a single federal regulator. The federal-state balance is load-bearing: whichever reading governs, the other reading's world does not survive its replacement.
% FOUNDING_PROBLEM: Under the Articles of Confederation, Congress had no power over interstate trade; states imposed tariffs and discriminatory duties on one another's goods, retaliated in kind, and the resulting trade war threatened both the union and the national economy. The Commerce Clause was written to give one authority the power to make regular the trade among the states — clear state barriers, keep the rules uniform.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the reading's beneficiary set: the Articles-era trade war is documented in Madison's convention notes and the Federalist papers and is conceded by broad-reading scholars across the interpretive camps; the continuing docket of state-protectionism cases under the dormant Commerce Clause shows the founding problem recurring whenever state power over commerce goes unchecked. No seat inside the arrangement attests it alone.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored from the reading's own lights over the standing arrangement the story is about — the narrow allocation itself — so it is low (0.18): the reading classifies its constraint as trade facilitation, not extraction from state sovereignty, and treats the costs imposed on protection-constituencies as activities Congress was never authorized to reach; the ambiguity is carried by omega cost_allocation_vs_extraction. Suppression is authored as a raw structural property of the arrangement — the active force needed to hold the allocation against congressional practice — and is not scaled by power or scope (the engine scales only extractiveness); at interval end it is low (0.22) because the reading's enforcement machinery is largely dismantled — rational-basis review and Raich leave it little operative force — but requires_active_enforcement is true because without active judicial enforcement against Congress the allocation does not hold, as 1937–1995 demonstrated. Theater (0.50) is the honest description of the reading's current operation: originalist scholarship, confirmation-hearing invocations, and occasional symbolic majorities maintain the position while the operative doctrine is broad. Accessibility collapse is low (0.25): the sibling readings remain fully available — indeed dominant — so understanding this constraint collapses no alternatives. Resistance is high (0.85): Congress, the agencies, most of the academy, and the standing doctrine all resist the reading's adoption. The measurement series share one grid (1789–2026): base_extractiveness traces the arrangement's cost-imposition (Lochner-era peak when the reading actively struck protective laws, trough during displacement, partial recovery with Lopez/Morrison); theater_ratio traces its displacement into rhetorical maintenance; suppression_requirement traces its enforcement capacity (Lochner peak, post-1937 collapse, partial 1995 rebuild, Raich re-narrowing). The 1995 rise and 2026 fall is a small revival cycle, not noise: revival pressure and re-displacement alternate, and the cycle itself is part of how the position stays publicly alive without governing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the judiciary's and state_governments' seats the arrangement is constitutional fidelity and retained self-government — a coordination device with its costs priced in at the founding. From congress's and the agencies' seats it is a jurisdictional loss to be litigated around, imposed by a counter-majoritarian institution. From the cost-bearing constituencies' seats it is abandonment: the federal backstop removed precisely where state majorities are least likely to replace it. Nothing in the authored claim adjudicates among these; the engine computes a type per seat from power, exit, and role, and the divergence between the reading's rope claim and the payer seats' computed types is the measurement the corpus exists to take. Coalition note: the powerless payer seats (claimants, pollution communities) are individually trapped but not structurally incapable of coalition — state-level electoral coalitions and interstate compacts are the exits the reading leaves open, and their viability is probed from the other side by omega state_protectionism_recurrence.
 *
 * DIRECTIONALITY LOGIC:
 *   state_governments and local_businesses sit at the beneficiary end (d near 0): the arrangement subsidizes them with regulatory jurisdiction and federal-regulatory immunity. interstate_commerce_firms are genuinely dual-positioned — uniform rules for the interstate component pull them toward beneficiary, patchwork costs for local operations push toward target; their secondary_role records this and their mobility keeps effective extraction modest. congress and the federal agencies sit at the target end with institutional power and no exit: the arrangement removes jurisdiction they currently exercise. The trapped constituencies (civil-rights claimants, local-harm victims) sit nearest full target — no exit, no replacement protection — while mobile national workers sit somewhat lower. No directionality overrides are authored: the derivation from beneficiary/victim declarations plus exit options captures each seat, and an override keyed to a power atom would misplace the other seats sharing that atom (institutional spans judiciary, congress, states, and agencies, which hold opposite structural relationships to the arrangement).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state barriers to interstate trade — is live, so the arrangement is not a mandate that has outlived its function; what has happened is contest-loss: the reading lost the doctrinal contest in 1937 and persists as a maintained interpretive position with partial revival gestures. That distinction matters for classification: a mandatrophy reading would mislabel the arrangement as a dead mandate administered theatrically, when in fact its function is intact, its enforcement is defeated, and its theater is the theater of a political-intellectual movement rather than of an administrator performing a dead function. mandatrophy_resolved is therefore not declared, and the R5 mismatch consumer should read founding_problem_status=live together with the elevated theater and high resistance: a live founding problem, a displaced operative constraint, and an organized revival movement — not a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the narrow_originalist reading of the commerce_clause_scope kernel; what would the sibling readings (broad_effects_test, intermediate_channels) change structurally if they governed instead?',
    'Comparative classification across the three reading-stories: the victim set under broad_effects_test expands to every economic activity reachable by aggregation; under intermediate_channels non-economic activity with a jurisdictional element escapes; under narrow_originalist only trade crossing state lines is federal.',
    'The beneficiary/victim structure, epsilon, and per-seat classifications all shift with the reading; cross-reading comparison is the measurement, and no single reading''s classification adjudicates the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the commerce_clause_scope kernel this constraint instantiates and what siblings would change.').

omega_variable(
    founding_semantics_of_commerce,
    'Did ''commerce'' in founding-era public meaning denote only trade and carriage across state lines, or the broader field of economic intercourse including production and gainful activity?',
    'Founding-era corpus linguistics, dictionary studies, and usage analysis of the ratification debates and contemporaneous texts.',
    'If the broader meaning is right, the reading''s foundational semantic axiom fails and its constraint collapses toward intermediate_channels; if the narrow meaning holds, the broad reading loses its textual anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_semantics_of_commerce, empirical, 'Whether the founding-era meaning of ''commerce'' supports the narrow reading''s core semantic premise.').

omega_variable(
    regulate_etymology_claim,
    'Does founding usage of ''regulate'' support ''make regular/facilitate'' to the exclusion of restriction and prohibition, or did the term include governing and banning?',
    'Corpus analysis of eighteenth-century legal and political usage, including contemporaneous regulatory statutes that restricted or prohibited conduct.',
    'If ''regulate'' includes restriction, the reading''s facilitation limit falls and federal prohibition of articles in trade — and more — enters the reading''s own scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulate_etymology_claim, empirical, 'Whether the ''make regular, not restrict'' etymological claim survives founding-era usage evidence.').

omega_variable(
    cost_allocation_vs_extraction,
    'Are the costs the reading imposes on protection-constituencies — lost federal labor, civil-rights, and environmental coverage — extraction by the arrangement, or the constitutional allocation working as designed?',
    'Per-seat classification comparison: the same structural data classifies differently from the state_governments seat and the civil_rights_claimants seat; the divergence is the datum.',
    'If the costs count as extraction, the constraint computes toward tangled_rope or snare from the payer seats; if allocation, it holds as rope from the reading''s own seat — the corpus records both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_allocation_vs_extraction, conceptual, 'Seat-indexed ambiguity over whether the reading''s cost profile is extraction or allocation.').

omega_variable(
    state_protectionism_recurrence,
    'If the reading governed, would state-imposed barriers to interstate trade recur faster than federal removal could police them, re-creating the founding problem inside the reading''s own terms?',
    'Historical natural experiment: the Articles-era trade war under unconstrained state power; the modern dormant Commerce Clause caseload as a measure of latent state protectionism.',
    'If recurrence outpaces policing, the reading''s coordination function degrades under its own operation and its rope character becomes unstable; if policing suffices, the founding problem stays live and managed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_protectionism_recurrence, empirical, 'Whether the reading''s limit on federal power re-creates the trade-barrier problem it was built to solve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_scope__narrow_originalist, theater_ratio, 1789, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1789, observed).
narrative_ontology:measurement(comm_tr_t1820, commerce_clause_scope__narrow_originalist, theater_ratio, 1820, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1820, observed).
narrative_ontology:measurement(comm_tr_t1860, commerce_clause_scope__narrow_originalist, theater_ratio, 1860, 0.12).
narrative_ontology:measurement_basis(comm_tr_t1860, observed).
narrative_ontology:measurement(comm_tr_t1905, commerce_clause_scope__narrow_originalist, theater_ratio, 1905, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1905, observed).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.35).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_scope__narrow_originalist, theater_ratio, 1964, 0.65).
narrative_ontology:measurement_basis(comm_tr_t1964, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.45).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_scope__narrow_originalist, theater_ratio, 2026, 0.5).
narrative_ontology:measurement_basis(comm_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_scope__narrow_originalist, base_extractiveness, 1789, 0.12).
narrative_ontology:measurement_basis(comm_be_t1789, observed).
narrative_ontology:measurement(comm_be_t1820, commerce_clause_scope__narrow_originalist, base_extractiveness, 1820, 0.15).
narrative_ontology:measurement_basis(comm_be_t1820, observed).
narrative_ontology:measurement(comm_be_t1860, commerce_clause_scope__narrow_originalist, base_extractiveness, 1860, 0.17).
narrative_ontology:measurement_basis(comm_be_t1860, observed).
narrative_ontology:measurement(comm_be_t1905, commerce_clause_scope__narrow_originalist, base_extractiveness, 1905, 0.22).
narrative_ontology:measurement_basis(comm_be_t1905, observed).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.2).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_scope__narrow_originalist, base_extractiveness, 1964, 0.12).
narrative_ontology:measurement_basis(comm_be_t1964, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_scope__narrow_originalist, base_extractiveness, 2026, 0.18).
narrative_ontology:measurement_basis(comm_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_scope__narrow_originalist, suppression_requirement, 1789, 0.15).
narrative_ontology:measurement_basis(comm_su_t1789, observed).
narrative_ontology:measurement(comm_su_t1820, commerce_clause_scope__narrow_originalist, suppression_requirement, 1820, 0.18).
narrative_ontology:measurement_basis(comm_su_t1820, observed).
narrative_ontology:measurement(comm_su_t1860, commerce_clause_scope__narrow_originalist, suppression_requirement, 1860, 0.22).
narrative_ontology:measurement_basis(comm_su_t1860, observed).
narrative_ontology:measurement(comm_su_t1905, commerce_clause_scope__narrow_originalist, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement_basis(comm_su_t1905, observed).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_scope__narrow_originalist, suppression_requirement, 1964, 0.1).
narrative_ontology:measurement_basis(comm_su_t1964, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_scope__narrow_originalist, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(comm_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, intermediate_channels).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' covers three structurally distinct allocations of federal power, decomposed per the epsilon-invariance principle into three stories: broad_effects_test (victim set: all economic activity reachable by aggregation; high federal reach), intermediate_channels (three-prong power with limiting principles; non-economic activity with a jurisdictional element escapes), and narrow_originalist (this story: only trade crossing state lines is federal; high state autonomy). The readings disagree on the semantic content of 'commerce' and 'regulate' in the fixed text, so within any single interpretive framework this reading's premises exclude the siblings' — the foreclosure edges run from this story to both siblings, and the sibling stories author their own edges in return. The empirical dependency runs the other way: the broad reading's doctrine is the standing arrangement this reading contests, and the historical-linguistic evidence this reading depends on is the same evidence the siblings contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
