% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure and Labor Market Protection (Member Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   This constraint instantiates the MEMBER SOVEREIGNTY PRIMARY reading of
 *   the contested federation_membership_obligations kernel. The kernel
 *   itself—how member states balance free movement rights against welfare
 *   closure authority—admits three structurally distinct readings. This
 *   reading asserts that national welfare states retain the primary authority
 *   to set closure rules, conditional only on labor market protection and
 *   fiscal sustainability concerns. Mobile workers from other member states
 *   can move and work freely but face waiting periods and contribution
 *   thresholds before accessing the receiving state's welfare. The constraint
 *   is CLAIMED as tangled_rope (genuine coordination function for labor
 *   market stabilization + asymmetric extraction from mobile workers) while
 *   authored metrics describe substantial active enforcement and rising
 *   theater (the justification frames shift toward pure sovereignty and away
 *   from functional necessity as time passes). The measurement series tracks
 *   extraction accumulation (t=0 to t=15) and plateau (t=15 to t=35),
 *   suggesting the constraint has reached a stable enforced equilibrium.
 *
 * KEY AGENTS:
 *   - Member state legislature: Sets and enforces closure rules; retains veto authority over welfare eligibility. Institutional power; constrained exit (federation membership).
 *   - Native labor constituencies: Benefit from prioritized welfare access and protected labor standards. Organized power; mobile exit (can move to other members, but strongest claims remain at home).
 *   - Mobile workers from other members: Pay taxes but face waiting periods and contribution thresholds before welfare access. Moderate power; constrained exit (wage premium tempers return to origin state).
 *   - Transnational service providers: Deploy workers cross-border and absorb compliance costs from closure rules. Powerful; arbitrage exit (can relocate operations but cannot ignore the internal market).
 *   - Receiving state courts: Adjudicate disputes between mobile workers and closure authority. Institutional power; constrained but observed seat.
 *   - Federation court and commission: Enforce integration rules and challenge excessive closure. Institutional power; sit between integration pressure and sovereignty pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.71).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure and Labor Market Protection (Member Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '94109627-a76f-42e3-a12b-82a67e1b5424').
narrative_ontology:cs_kernel_codification('94109627-a76f-42e3-a12b-82a67e1b5424', formalized).
narrative_ontology:cs_authority_grounding('94109627-a76f-42e3-a12b-82a67e1b5424', lineage).
narrative_ontology:cs_interpretation_layer_present('94109627-a76f-42e3-a12b-82a67e1b5424').
narrative_ontology:cs_reading_relation('94109627-a76f-42e3-a12b-82a67e1b5424', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('94109627-a76f-42e3-a12b-82a67e1b5424', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('94109627-a76f-42e3-a12b-82a67e1b5424', foundational, member_state_welfare_primacy).
narrative_ontology:cs_axiom_status(member_state_welfare_primacy, holdable).
narrative_ontology:cs_axiom_grounding('94109627-a76f-42e3-a12b-82a67e1b5424', member_state_welfare_primacy, deontological).
narrative_ontology:cs_axiom('94109627-a76f-42e3-a12b-82a67e1b5424', secondary, fiscal_sustainability_closure_justified).
narrative_ontology:cs_axiom_status(fiscal_sustainability_closure_justified, holdable).
narrative_ontology:cs_axiom_grounding('94109627-a76f-42e3-a12b-82a67e1b5424', fiscal_sustainability_closure_justified, empirically_contingent).
narrative_ontology:cs_reference_frame('94109627-a76f-42e3-a12b-82a67e1b5424', national_welfare_state_sovereignty).
narrative_ontology:cs_drift_state('94109627-a76f-42e3-a12b-82a67e1b5424', contemporary_labor_mobility_acceleration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('94109627-a76f-42e3-a12b-82a67e1b5424', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, native_labor_constituencies).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_members).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, transnational_service_providers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, national_democratic_legitimacy).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_state_fiscal_sustainability).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, labor_market_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal conditions for welfare access and labor market participation within the member state. Retains authority to define residence requirements, contribution thresholds, and eligibility waiting periods for non-citizens. Justifies closure as protection of fiscal sustainability and democratic legitimacy—voters authorize welfare through taxation and expect welfare to flow primarily to citizens who have participated in the tax base.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Receive prioritized access to unemployment insurance, family benefits, and healthcare funded by progressive taxation. Benefit from wage floors and labor standard enforcement that the state sustains partly by limiting inflow of cheaper labor from other member states. Can exit by relocating to other member states but retain strongest claim on receiving state's welfare at home.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, native_labor_constituencies, beneficiary,
    organized, biographical, mobile, national).

% Migrate to seek employment and higher wages but face multi-year waiting periods before accessing the receiving state's welfare system—even after paying taxes and contributions. Pay into systems they cannot immediately draw from. Can exit by returning to origin state, but that foregoes wage premium and career advancement. Face administrative barriers (language, credential recognition, residency bureaucracy) that increase the effective cost of establishing claims.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_members, payer,
    moderate, biographical, constrained, global).

% Operate cross-border service delivery (legal, accounting, construction, healthcare staffing) and deploy workers across member states on assignment. Bear the cost of compliance with each member's welfare exclusion rules—must maintain separate insurance pools, limited-duration posting arrangements, and repatriation clauses. Can arbitrage between states (locating operations where rules are lighter) but cannot ignore closure entirely given the scale of the internal market.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, transnational_service_providers, payer,
    powerful, biographical, arbitrage, continental).

% Adjudicate disputes between mobile workers and the member state over welfare access, residency, and labor standards. Must interpret federal treaty language (free movement, non-discrimination) against national legislation (waiting periods, contribution thresholds). Courts become the arena where member sovereignty and integration pressures collide; their rulings either narrow or widen the closure space.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_courts, observer,
    institutional, generational, constrained, regional).

% Interprets federation treaties and constitutional texts governing free movement and welfare. Reviews member state closure rules for compatibility with federation law. Sits between integration pressure (treaties grant free movement rights) and sovereignty pressure (members retain welfare authority). Rulings directly reshape what constitutes permissible closure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, federation_court, observer,
    institutional, generational, analytical, continental).

% Enforces federation rules on non-discrimination and free movement. Can initiate infringement procedures against member states for excessive welfare closure. Lacks direct enforcement power but can reputationally and legally pressure members toward opening. Reflects integration priority; tension with member sovereignty sits at the institutional level.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, federation_commission, observer,
    institutional, generational, analytical, continental).

% Have workers who migrate out and remit wages home. Experience fiscal drain (tax revenue from emigrants, responsibility for unskilled workers left behind) while receiving states capture wage-premium surplus. Could advocate for reciprocal welfare access or labor-mobility fees but are structurally outside the receiving state's decision-making on closure rules. Excluded from the constraint's enforcement mechanism despite bearing consequences.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, origin_state_governments, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, native_labor_constituencies).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes labor markets and fiscal sustainability within member states by allowing them to set welfare access rules that protect domestic workforce participation and prevent over-subscription of insurance pools. Enables member states to fund generous welfare systems without facing unlimited claims from workers who have not contributed to the tax base. Solves a genuine collective-action problem: without closure authority, states face a race to the bottom in welfare generosity as mobile workers concentrate in high-benefit jurisdictions.
% TRANSFER_FUNCTION: Transfers welfare access and labor market priority from mobile workers to native constituencies. Mobile workers pay taxes into a system they cannot immediately draw from; native workers receive prioritized unemployment, family, and healthcare benefits. The transfer occurs across national boundaries—workers from lower-wage origin states subsidize higher-wage receiving states' welfare systems without reciprocal benefit for a defined period.
% ABSENT_VOICES: Origin state governments, which lose workers and remittance capacity, are excluded from the rules that govern the receiving state's welfare closure. Workers in origin states—left behind as emigrants depart—have no seat at the table where receiving states set closure rules. Receiving state employers who seek cheaper labor (and would profit from open access) are not organized as a stakeholder and are generally aligned with the closure authority rather than opposed to it.
% DISAPPEARANCE_RATIONALE: If member states lost closure authority overnight, welfare systems would face immediate over-subscription; benefit levels would compress or eligibility would tighten; labor cost structures would shift as wage competition intensified; the fiscal capacity to fund generous welfare would erode. Member states would reorganize political-economic arrangements, possibly by exiting the federation, fragmenting welfare systems into contribution-based tiers, or establishing federal (rather than member-based) welfare to manage pooled risk.
% FOUNDING_PROBLEM: Post-war federations needed to balance free movement (economic integration benefit) against member state fiscal capacity to fund welfare and protect labor standards. Member states built welfare on the assumption that citizens—who participate in shared tax bases and electoral legitimacy—would be the primary beneficiaries. Opening welfare access to all mobile persons without reciprocal contribution would undermine the fiscal model and electoral support for generous benefits.
% FOUNDING_PROBLEM_CORROBORATION: Member state legislatures and executives attest the founding problem is live and urgent: fiscal pressures from migration and welfare demand are rising. Federation courts and commissions attest the problem has shifted: labor mobility has become efficient enough that closure now blocks beneficial economic reallocation. Origin state governments and mobility-rights advocates attest the founding problem is partly solved and the closure now extracts unfair gains for receiving states. Academic research on fiscal federalism (Pierson, Rodrik, Streeck) outside the directly benefiting parties documents the tension but does not resolve whether closure remains justified.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because mobile workers pay into a system they cannot immediately draw from, while native constituencies receive prioritized benefits. The extraction is not maximal because mobile workers do eventually gain access after contribution periods, and the constraint acknowledges some coordination function (labor market stabilization). Suppression is elevated (0.71) because persistence depends on actively excluding mobile workers and transnational providers—member states maintain borders, credential barriers, residency rules, and administrative gatekeeping to hold the constraint. Theater rises from t=0 (0.32) to t=20 (0.43) and plateaus, indicating that justifications are shifting: early framing emphasizes genuine coordination (welfare sustainability, labor market protection) while later activity defends closure as pure sovereignty. This pattern (rising theater as a constraint ages) is a diagnostic signal of a tangled_rope shifting toward snare-like operation. Accessibility collapse is moderate-high (0.68) because alternatives for mobile workers are real but costly—they can stay in origin states or move to less restrictive members, but receiving state's wage premium and labor market depth create strong path dependency. Resistance is moderate (0.59) because native constituencies and member legislatures strongly support closure (they benefit) while mobile workers and transnational providers resist (they bear costs), and federation-level actors (courts, commission) generate countervailing pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (member state legislature) and the payer seat (mobile workers) should compute radically differently. From the legislature's position the constraint is a necessary protection of fiscal sustainability and democratic legitimacy—genuine coordination that they alone have authority to set. From the mobile worker seat it is enforced extraction: they contribute taxes but are denied benefits while native workers (less productive, equal contributors) receive full access. The engine computes this divergence from directionality: legislators sit near beneficiary (d ~ 0.2–0.35), mobile workers sit near target (d ~ 0.75–0.85). Courts sit in between: they must balance both frames, which produces the classification uncertainty and the role of judicial rulings as the constraint's destabilization point.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures are the clear beneficiaries (they retain authority, collect political support from native constituencies, avoid welfare over-subscription). Native labor constituencies benefit from prioritized access and protected labor standards (beneficiary role; d ~ 0.2). Mobile workers are clear targets: they pay taxes, face waiting periods, and have constrained exit (payer role; d ~ 0.78). Transnational service providers are secondary targets (they bear compliance costs) but have more exit flexibility through arbitrage (d ~ 0.62). Federation courts and commission observers sit at the contested boundary (d ~ 0.5), where the constraint's legitimacy is directly in question. No directionality overrides are needed; the structural data (beneficiary/victim declarations + power + exit) produces the correct directionality for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This is NOT a case of atrophied function masked by inertia. The founding problem (balancing free movement with welfare sustainability) is genuinely live: labor migration has accelerated over the interval, fiscal pressures on welfare have intensified, and member states face real trade-offs between opening access and maintaining benefit generosity. The constraint persists because active enforcement (border controls, eligibility verification, credential gatekeeping) continues to be justified by the founding problem. However, the rising theater_ratio (0.32 → 0.44 over the interval) signals that justification is shifting: early theater focuses on labor market stabilization and fiscal necessity; later theater emphasizes national democratic sovereignty as an end in itself, disconnected from whether closure is functionally necessary for the coordination problem. This shift suggests the constraint is transitioning from tangled_rope (coordination + extraction) toward snare-like operation (extraction + sovereignty theater). The T17 abductive trigger (mountain_extraction_accumulation) does not fire here because extractiveness plateaus at t=15 and does not continue rising—the constraint has reached equilibrium, not accumulation. The mandate has NOT outlived its function; the function has shifted from coordination (fiscal protection) toward pure sovereignty (member authority as value in itself), which is a change in the legitimacy frame rather than mandatrophy in the classical sense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_necessity_vs_distributional_choice,
    'Is the measured extraction (0.62) a necessary cost of maintaining fiscal sustainability and labor market protection (coordination function), or is it a distributional choice by member states to protect native worker rents and exclude lower-wage competition?',
    'Comparative analysis of member states with tight and loose closure rules, controlling for welfare generosity, tax rates, and labor market outcomes. If tighter closure correlates with higher welfare spending and stable labor markets while looser closure does not reduce welfare or destabilize labor markets, the necessity claim is weakened. Counterfactual modeling: what happens to welfare fiscal capacity if closure is removed?',
    'If closure is functionally necessary, the constraint is tangled_rope (coordination + necessary extraction cost). If closure is distributive choice disconnected from fiscal necessity, the constraint reclassifies toward snare (extraction + justification theater). This omega directly determines whether the constraint is reclassified in light of new evidence about fiscal mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_necessity_vs_distributional_choice, empirical, 'Whether measured extraction is a functional necessity or a distributional choice.').

omega_variable(
    federation_primacy_vs_member_sovereignty,
    'Is the constraint''s legitimacy grounded in member state authority over welfare (this reading''s core premise) or in federation law establishing free movement as a superior right that member closure must subordinate to?',
    'Federation court rulings clarifying the hierarchy: does the court treat member welfare authority as primary (constrained only by non-discrimination rules) or does it treat free movement as foundational (member closure permissible only where it meets strict necessity tests)? Historical trajectory of judicial interpretation: are courts narrowing or widening closure space over time?',
    'If the court establishes member sovereignty primacy, the constraint''s classification and enforcement stabilize. If the court establishes integration primacy, the constraint faces reclassification toward rope (coordination without asymmetric extraction for citizenship-based reasons) or toward piton (atrophied authority masked by inertia while federation law supersedes). This omega documents the contested reading at the institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_primacy_vs_member_sovereignty, conceptual, 'Which institutional authority—member state or federation—holds ultimate welfare closure authority.').

omega_variable(
    suppression_mechanism_structural_vs_identity,
    'Is the measured suppression (0.71) structural (borders, credential gatekeeping, bureaucratic barriers enforce closure regardless of mobile worker preferences) or partly internalized (mobile workers internalize the closure narrative as legitimate member authority, reducing active resistance)?',
    'Post-exit suppression trajectory: if mobile workers who leave for less restrictive members later return to the restrictive member state despite lower-barrier alternatives available, suppression is internalized. Lived-experience interviews with mobile workers and origin-state emigrants about whether they challenge the closure rules or accept them as legitimate. Resistance measurement: do organized migrant communities actively contest closure, or do they accept waiting periods and contribution thresholds as normal?',
    'If suppression is structural, the constraint''s enforcement requires active border and administrative machinery; if it persists, maintenance costs are high. If suppression is internalized, mobile workers carry the closure logic with them; the constraint''s inertia is higher. This affects whether the constraint''s persistence can be explained by inertia (piton drift) versus active institutional maintenance (tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_identity, empirical, 'Whether suppression is structural barriers or internalized legitimacy acceptance.').

omega_variable(
    reading_contest_foreclosure_test,
    'Can a single federation framework simultaneously hold the member_sovereignty_primary and integration_primary readings, or do they logically foreclose each other?',
    'Institutional analysis of federation constitutional structure: does the treaty text assign welfare authority to members or to the federation? Do federation courts treat this as a zero-sum hierarchy (one reading forecloses the other) or as a balanced two-level system where both readings have valid domains? Historical record: have attempts to reconcile these readings produced stable doctrine or ongoing reinterpretation?',
    'If the readings foreclose each other, the constraint''s legitimacy depends entirely on which reading the federation''s institutional authorities adopt—high classification volatility. If they coexist through domain separation (members control eligibility timing and thresholds; federation controls discrimination rules), the constraint stabilizes as a compromise form. This omega documents the institutional instability of the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_test, conceptual, 'Whether member sovereignty and integration primacy readings can coexist in one framework or logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 35, 0.44).
narrative_ontology:measurement_basis(fede_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(fede_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fede_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__member_sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% The federation_membership_obligations kernel admits three structurally distinct readings. This story models the member_sovereignty_primary reading: national welfare states retain closure authority; free movement is conditional on labor market protection and welfare sustainability. The integration_primary reading asserts free movement overrides member closure; selective_solidarity asserts welfare access is tied to contribution history rather than citizenship. All three readings are instances of the same contested kernel but have different ε values (structural extractiveness), different beneficiary/victim structures, and different classification outcomes. Links via affects_constraints enable contamination analysis: if member_sovereignty reading's institutional support erodes, the other readings gain traction. No single reading can be removed without the other two reshaping the federation's constitutional equilibrium.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
