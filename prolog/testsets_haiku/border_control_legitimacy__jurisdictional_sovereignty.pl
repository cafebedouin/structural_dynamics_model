% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy via Jurisdictional Sovereignty
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'border_control_legitimacy': the jurisdictional_sovereignty reading. It
 *   holds that sovereignty is jurisdictional authority (power to regulate
 *   rights and obligations within territory) but does NOT necessarily include
 *   absolute border closure authority. Legitimacy requires balancing
 *   protection obligations to residents with labor needs and public consent.
 *   This reading acknowledges dual victim sets — excluded migrants AND
 *   displaced citizen workers — and constrains enforcement through
 *   proportionality and necessity tests. The constraint enters legitimacy
 *   crisis when enforcement becomes cruel (violating basic rights of excluded
 *   migrants) OR when admission policies undermine public consent (displacing
 *   citizen workers without mitigation). This reading coexists with two
 *   sibling readings: freedom_of_movement_primary (which denies that
 *   sovereignty entails closure authority at all, treating movement as
 *   fundamental right) and sovereignty_primary (which asserts absolute
 *   discretion to exclude). The three readings are live positions held by
 *   different institutional and political actors; none forecloses the others
 *   within a single framework, though they compete for institutional
 *   authority.
 *
 * KEY AGENTS:
 *   - State institutional order: agenda-setter, maintains border enforcement as necessary for social contract legitimacy
 *   - Excluded migrants: powerless victims, bear immediate cost of closure and lack voice in policy
 *   - Displaced citizen workers: moderate power, dual role (beneficiaries of state protection, victims of wage/employment pressure from both closure and admission)
 *   - Labor market gatekeepers: organized, beneficiaries of labor scarcity created by closure
 *   - Welfare provision state: institutional agenda-setter, claims closure is fiscally necessary
 *   - International human rights bodies: institutional observers, monitor enforcement for proportionality
 *   - Pro-migration advocates: moderate power, excluded from enforcement apparatus
 *   - State security apparatus: institutional agenda-setter, administers enforcement machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.68).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.72).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy via Jurisdictional Sovereignty").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '9eddeae2-99f1-4d8b-9b21-a823548c4598').
narrative_ontology:cs_kernel_codification('9eddeae2-99f1-4d8b-9b21-a823548c4598', fixed_text).
narrative_ontology:cs_authority_grounding('9eddeae2-99f1-4d8b-9b21-a823548c4598', lineage).
narrative_ontology:cs_interpretation_layer_present('9eddeae2-99f1-4d8b-9b21-a823548c4598').
narrative_ontology:cs_reading_relation('9eddeae2-99f1-4d8b-9b21-a823548c4598', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_reading_relation('9eddeae2-99f1-4d8b-9b21-a823548c4598', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('9eddeae2-99f1-4d8b-9b21-a823548c4598', foundational, jurisdiction_authority_non_absolute_closure).
narrative_ontology:cs_axiom_status(jurisdiction_authority_non_absolute_closure, holdable).
narrative_ontology:cs_axiom_grounding('9eddeae2-99f1-4d8b-9b21-a823548c4598', jurisdiction_authority_non_absolute_closure, deontological).
narrative_ontology:cs_axiom('9eddeae2-99f1-4d8b-9b21-a823548c4598', foundational, legitimacy_requires_dual_balance).
narrative_ontology:cs_axiom_status(legitimacy_requires_dual_balance, holdable).
narrative_ontology:cs_axiom_grounding('9eddeae2-99f1-4d8b-9b21-a823548c4598', legitimacy_requires_dual_balance, instrumental).
narrative_ontology:cs_reference_frame('9eddeae2-99f1-4d8b-9b21-a823548c4598', westphalian_sovereign_territoriality).
narrative_ontology:cs_drift_state('9eddeae2-99f1-4d8b-9b21-a823548c4598', contemporary_mobility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9eddeae2-99f1-4d8b-9b21-a823548c4598', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_institutional_order).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeepers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, welfare_provision_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises jurisdictional authority over rights and obligations within territory. Claims legitimacy through balance: protecting citizens' labor market, welfare provision, and collective self-determination while acknowledging some obligation to migrants and refugee populations. Defends border enforcement as necessary to maintain the administrative capacity to fulfill protection obligations to residents. Faces dual legitimacy pressure: accused of cruelty toward excluded migrants and of insufficiently protecting citizen workers from wage suppression and labor market displacement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_institutional_order, agenda_setter,
    institutional, generational, analytical, national).

% Seeks entry for work, family reunion, or refuge but is denied under the state's border closure authority. Bears the direct cost of exclusion: separation from family, inability to access labor markets, risk of deportation, detention, or forced return to danger. No formal political voice in the jurisdiction's deliberations; presence advocates for border opening but is structurally silenced by exclusion itself. Exit option is trapped: remaining outside the jurisdiction means continued deprivation; all alternative jurisdictions maintain similar closure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens whose labor-market standing, wage levels, or employment security is claimed to depend on restricting migrant entry. May benefit from state protection (welfare, public goods, legal standing) while bearing the cost of potentially reduced wages, increased job competition, or downward labor-market pressure attributed to migration. Legitimacy pressure points: if borders stay closed, state struggles to fund public services; if borders open, workers fear wage suppression. Their voice in border policy is proxied through electoral/welfare claims, making their actual position ambiguous between beneficiary and victim.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_workers, beneficiary).

% Employers, unions, or professional licensing bodies that benefit from border closure's labor-scarcity effect. Restrict supply of workers to maintain wage premiums, negotiating leverage, or professional status. They advocate closure as protecting citizen workers from exploitation but benefit directly from restricted supply. Their exit option is mobile: they can relocate production, outsource labor, or reduce hours if the constraint loosens; the constraint serves their interests but does not trap them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeepers, beneficiary,
    organized, biographical, mobile, national).

% Must fund education, healthcare, housing, and income support for residents. Claims border closure is necessary to make welfare provision fiscally sustainable and prevent free-rider effects. Faces competing pressures: excluding migrants reduces fiscal burden but may shrink the tax base and labor supply; admitting migrants increases demand but can expand revenue and fill labor shortages. Legitimacy depends on balancing fiscal health with moral obligations to vulnerable populations (both citizens and non-citizens).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, welfare_provision_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, welfare_provision_state, payer).

% Evaluate state border enforcement against conventions on freedom of movement, non-refoulement, and human dignity. Recognize state jurisdictional authority but dispute whether that authority includes absolute closure. Monitor for proportionality and necessity: whether exclusion is the minimum required to achieve legitimate aims, or whether it exceeds that threshold. Hold the state accountable when enforcement becomes cruel or when the state's protection obligations are invoked to mask pure rent-seeking.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Argue that freedom of movement is a fundamental right and that sovereignty does not entail closure authority. Advocate for open borders or substantially more permissive entry regimes. Are present in political discourse but structurally excluded from the border enforcement apparatus itself; their voice carries limited weight in administrative and enforcement decision-making.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, pro_migration_advocates, excluded,
    moderate, biographical, constrained, national).

% Administers border enforcement, immigration detention, and deportation machinery. Has institutional interest in maintaining border closure authority and control mechanisms. Expands enforcement infrastructure and may conflate security concerns with labor-market gatekeeping to justify resource allocation. Operates under stated legitimacy constraints (proportionality, necessity, human rights compliance) but has material interest in escalating enforcement when questioned.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_security_apparatus, agenda_setter,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeepers).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a recognized, regularized system for who belongs to a political community and has standing to claim its protections and share its commons. Solves the coordination problem: without border authority, no state can maintain the social contract, welfare provision, or collective decision-making structures that give residents standing to govern together. Distinguishes citizens (with full political and economic rights) from non-citizens (with limited or no standing).
% TRANSFER_FUNCTION: Transfers labor-market exclusivity and economic opportunity from globally mobile people to resident citizens and labor-market gatekeepers, in exchange for state-provided public goods (education, healthcare, welfare, security) and political membership. The state collects rents on labor scarcity by limiting entry; citizen workers receive partial rents and protections; excluded migrants lose access entirely; gatekeepers capture outsized rents by maintaining labor supply restriction.
% ABSENT_VOICES: Excluded migrants cannot participate in the border policy deliberations that determine whether they can enter, despite bearing the full cost of exclusion. Their absence from decision-making is structural — enforcement excludes them before they can have voice. Future citizen workers (not yet born, not yet migrants) are also absent from policy deliberations.
% DISAPPEARANCE_RATIONALE: If border control legitimacy vanished overnight — if states lost all jurisdictional authority to regulate entry and exit — the social contract would require renegotiation. Public welfare provision would face fiscal crisis or expansion depending on admission policy; labor markets would reorganize around global rather than territorial supply; political membership and democratic self-determination would require redefinition (how does a polity govern if anyone can enter to participate in governance). The constraint is not a natural fact; it depends on active state enforcement and legitimacy belief.
% FOUNDING_PROBLEM: Early national states required mechanisms to distinguish members (who share in commons, have claims on protection, participate in collective decision-making) from non-members (who do not share in commons and cannot claim protections as a right). Without such distinction, no stable social contract is possible; no state can fund welfare provision if it cannot limit the claimant population; no democracy can function if the electorate is undefined.
% FOUNDING_PROBLEM_CORROBORATION: The state institutional order and welfare-state apparatus attest the founding problem is live and border closure is necessary. International human rights bodies and pro-migration advocates attest the founding problem has been substantially solved (most people stay in place voluntarily; global governance structures can manage migration without absolute closure) and border closure now persists as extraction and exclusion rather than as response to founding necessity. Economic analysis from outside benefiting parties (immigration economists, OECD policy research) shows that moderate, non-absolute border control can achieve coordination while reducing extraction. No corroboration exists from excluded migrants themselves — their absence is structural to the constraint.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading produces a Tangled Rope classification because it satisfies all three gates: (1) coordination function: establishing political membership and jurisdiction is genuine coordination (solves the founding problem of defining the polity), (2) asymmetric extraction: labor-market gatekeepers and organized state interests capture rents from labor scarcity while powerless excluded migrants bear the full cost, and (3) active enforcement: the constraint persists only through enforcement machinery (detention, deportation, border patrol, visa systems) that would collapse without continuous operation. Extractiveness is moderate-high (0.68) because the constraint transfers significant economic opportunity but still permits *some* regulated entry and maintains a veneer of humanitarian protection. Suppression is high (0.72) because excluded migrants have no meaningful alternative — they are trapped outside the jurisdiction and lack voice in policy. Theater is moderate (0.41) because security/protection language genuinely describes some enforcement function (preventing crime, protecting public health), but a growing share of enforcement activity (through the interval) serves pure labor-gatekeeping and rent-extraction rather than stated safety functions. Measurements show suppression requirement rising steeply from t=0 to t=15 (as enforcement infrastructure hardens and political pressure from labor gatekeepers increases), then plateauing at t=20+ (enforcement reaches saturation). Extractiveness also rises but plateaus, suggesting the constraint hits equilibrium when political resistance from pro-migration advocates and international pressure matches the gatekeepers' rent-capture interests. Theater rises continuously, indicating increasing performative justification-work relative to functional output — the 'protection obligations' language expands even as enforcement becomes more purely selective-exclusionary.
 *
 * PERSPECTIVAL GAP:
 *   The state institutional order and labor gatekeepers should perceive this as legitimate coordination (membership definition, welfare capacity); excluded migrants and international observers should perceive it as pure extraction and cruelty. The engine computes these divergent seat classifications from the structural data: the agenda-setter and beneficiary seats derive low d and perceive coordination function; the powerless victim seats derive high d and perceive extraction and suppression. The displaced citizen workers sit in the most ambiguous position: they are told they benefit (protection from competition) but may also be victims (if wage suppression occurs despite closure, or if public services degrade from fiscal constraints). The welfare provision state faces genuine tension: it is simultaneously agenda-setter (enforcing closure) and victim (fiscal pressure from either closure or open admission).
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants have directionality d approaching 1.0 (full target): they are powerless, trapped, have no exit, and bear the direct cost of exclusion. Labor gatekeepers have d approaching 0.0 (beneficiary): they are organized, mobile, and capture substantial rents from scarcity. The state institutional order sits near 0.3–0.4 (mild beneficiary): it collects legitimacy and stability from membership definition but pays a cost in enforcing the apparatus and in legitimacy crisis when enforcement becomes cruel. Displaced citizen workers are the most difficult to locate: they are told they benefit (protection from low-wage pressure) but often face genuine wage/employment pressure regardless, and they pay through reduced labor-market flexibility and public service constraints. Their d sits near 0.5–0.6 (slightly toward target): they have moderate power and constrained exit but some political voice, which gives them slightly more leverage than the excluded migrants. International human rights bodies are observers (d = analytical) — they have high power but no direct stake in the economic transfer; their directionality is analytical rather than situated.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy tension by acknowledging that border closure authority was FOUNDED to solve a genuine coordination problem (defining membership and enabling welfare provision) but that founding problem has substantially shifted. Modern nation-states can maintain welfare and governance without absolute closure — the empirical claim is contested but increasingly defended in economic literature. The constraint persists because gatekeepers benefit from it and because the state's legitimacy narrative (protection obligations) provides cover for pure rent-extraction. The reading avoids conflating 'this is coordination' (false) with 'this is pure extraction' (also false) by declaring both: the membership-definition function is real but has decoupled from the closure mechanism. A state could define membership and fund welfare provision while admitting more migrants on controlled terms. The extractive layer — the rents captured by labor gatekeepers and the exclusion of vulnerable people — is analytically separable from the coordination layer. This Tangled Rope classification captures that separability: there IS genuine coordination, but it is yoked to and masked by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of membership definition and welfare-capacity protection actually been solved or substantially mitigated, or does it remain live?',
    'Comparative institutional analysis: examine welfare states that have maintained fiscal solvency and democratic governance while admitting substantially more migrants than restrictive states. Test whether membership definition persists without absolute closure (e.g., through tiered citizenship, work permits, or residency categories). Look for empirical evidence of welfare degradation or governance failure when admission rates rise.',
    'If the founding problem is substantially solved, the constraint''s extraction layer becomes dominant and the tangled_rope classification shifts toward snare. If the founding problem remains live, the rope component is genuine and extraction is the secondary feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether state legitimacy and welfare capacity actually depend on border closure or whether alternative membership definitions suffice.').

omega_variable(
    dual_victim_asymmetry,
    'Do excluded migrants and displaced citizen workers constitute genuinely dual victims of the same constraint, or are they victims of different constraints that are structurally linked?',
    'Decompose the mechanisms: closing borders harms excluded migrants directly; but does it actually help displaced citizen workers, or does it only appear to help while labor gatekeepers capture the actual rents? Test through natural experiments: when borders become more permissive, do citizen workers'' wages fall (supporting victim status) or do they stabilize/rise (suggesting they were not the real victims)?',
    'If they are genuinely dual victims of the same constraint, the tangled_rope classification holds. If displaced workers are victims of a separate constraint (labor-market gatekeeping) that is merely LINKED to border closure but not identical, then this constraint should be decomposed into two stories per ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_asymmetry, empirical, 'Whether the measured extractiveness reflects a single constraint with dual victims or multiple constraints with confounded effects.').

omega_variable(
    legitimacy_crisis_simultaneity,
    'Can a state experience legitimacy crisis from both directions simultaneously — cruelty to migrants AND displacement of workers — or does it face a trade-off where addressing one necessarily worsens the other?',
    'Institutional history of border-policy reform: when states liberalize admission to address migrant-rights concerns, do citizen-worker displacement concerns automatically intensify, forcing reclosure? Or can policymakers decouple admission policy from labor-market effects through wage support, retraining, or productivity gains?',
    'If the crisis is genuinely simultaneous and policy trade-offs are unavoidable, the constraint is more fragile and more likely to enter mandatrophy. If a decoupling mechanism exists (admission need not harm workers), then the constraint could be reformed to be less extractive without collapsing coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_crisis_simultaneity, conceptual, 'Whether the dual legitimacy pressures are structurally entangled or whether they can be independently managed.').

omega_variable(
    suppression_internalization_vs_structure,
    'Is the measured suppression (0.72) attributable primarily to structural barriers (legal, administrative, geographic) or to internalized beliefs that borders are natural and exclusion is deserved?',
    'Post-exit cohort studies: survey migrants who successfully entered after initial exclusion; measure whether their sense of exclusion-legitimacy persists after removal of structural barriers. If suppression persists and deepens post-entry (''forever outsider'' internalization), the constraint''s internalized component is substantial.',
    'If suppression is mostly structural, removal of legal barriers would rapidly reduce it. If suppression is mostly internalized, removal of barriers alone would not break the constraint; identity-fusion and shame-based mechanisms would require additional intervention (e.g., integration policy, social narrative change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structure, empirical, 'Mechanism by which suppression operates on excluded migrants: external barriers vs. internal conviction.').

omega_variable(
    kernel_reading_sibling_foreclosure,
    'Does the jurisdictional_sovereignty reading''s core premise (sovereignty is jurisdiction but not necessarily closure) logically foreclose either sibling reading, or do all three remain live?',
    'Logical analysis: the sovereignty_primary reading claims closure authority IS constitutive of sovereignty; the jurisdictional_sovereignty reading claims jurisdiction does NOT necessarily entail closure. These are NOT logically contradictory in a single framework if ''closure authority'' is treated as contingent rather than essential. However, if ''constitutive'' means ''logically entailed'', then the readings do foreclose each other. The freedom_of_movement_primary reading takes movement as fundamental right and denies sovereignty entails closure — this may coexist with jurisdictional_sovereignty if jurisdiction is treated as compatible with movement rights.',
    'If the readings are logically incompatible, the committer-axis should resolve (through law, referendum, or institutional dominance) toward one reading. If they are compatible, institutional pluralism permits coexistence at different policy levels (federal/regional, domestic/international). This affects the terminal attractor: incompatibility predicts institutional conflict; compatibility predicts stable hybrid arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_foreclosure, conceptual, 'Whether the three readings of border_control_legitimacy are logically mutually exclusive or whether they can coexist in hybrid institutional arrangements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.22).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.28).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.33).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.38).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.4).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.41).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.18).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeeping_rents).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, welfare_state_fiscal_capacity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel border_control_legitimacy. The freedom_of_movement_primary reading denies that sovereignty entails closure authority and treats movement as fundamental right. The sovereignty_primary reading asserts closure authority is constitutive of sovereignty. These three readings are linked via network.affects_constraints: the jurisdictional_sovereignty reading (this file) holds that closure is contingent on legitimacy balance, which directly influences the sibling readings' plausibility. Each reading has its own constraint_id and its own ε, beneficiary/victim structure, and classification; they are NOT observations of one constraint from different angles, but rather THREE structurally distinct constraints instantiated from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
