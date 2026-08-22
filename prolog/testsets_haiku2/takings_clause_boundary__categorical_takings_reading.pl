% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Rule: Per Se Standards and Penn Central Balancing
 *   domain: constitutional/property_rights
 *
 * SUMMARY:
 *   The takings clause of the Fifth Amendment states that private property
 *   shall not 'be taken for public use without just compensation.' This
 *   constraint represents ONE READING of how that clause applies to
 *   regulatory restrictions on property use. The categorical reading holds
 *   that permanent physical occupations and regulations that eliminate all
 *   economically beneficial use are per se takings (automatic compensation);
 *   all other regulations are evaluated using the three-factor Penn Central
 *   test (balancing public purpose, economic impact, and interference with
 *   investment-backed expectations). This reading was crystallized in Loretto
 *   v. Teleprompter (1982) and Lucas v. South Carolina Coastal Council
 *   (1992). It competes with the physical_appropriation_reading (which
 *   restricts per se status to direct physical seizures, leaving permanent
 *   occupations to Penn Central balancing) and the regulatory_takings_reading
 *   (which applies flexible balancing to all regulations without categorical
 *   exceptions). This constraint story captures the categorical reading's
 *   structure, beneficiaries, and extractive effects as currently
 *   instantiated in federal property law.
 *
 * KEY AGENTS:
 *   - property_owners_with_categorical_claims: powerful institutional position, gain predictability through bright-line rules at extremes
 *   - regulatory_agencies: institutional position, bear compensation costs and litigation risk from categorical triggering
 *   - public_interest_coalitions: organized position, constrained by the constraint's entrenchment of property expectations
 *   - courts: institutional position, set and administer the categorical/Penn Central framework
 *   - marginal property owners: moderate position, sit in Penn Central balancing zone with uncertain outcomes
 *   - legislative bodies: institutional but excluded from doctrine authorship, trapped by constitutional hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.41).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Rule: Per Se Standards and Penn Central Balancing").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'e1f8b3d2-d26c-4a99-a13b-b99c58dbba89').
narrative_ontology:cs_kernel_codification('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', fixed_text).
narrative_ontology:cs_authority_grounding('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', lineage).
narrative_ontology:cs_interpretation_layer_present('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89').
narrative_ontology:cs_reading_relation('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', foundational, categorical_per_se_bright_lines_required).
narrative_ontology:cs_axiom_status(categorical_per_se_bright_lines_required, holdable).
narrative_ontology:cs_axiom_grounding('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', categorical_per_se_bright_lines_required, instrumental).
narrative_ontology:cs_axiom('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', foundational, penn_central_balancing_preserves_regulatory_flexibility).
narrative_ontology:cs_axiom_status(penn_central_balancing_preserves_regulatory_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', penn_central_balancing_preserves_regulatory_flexibility, instrumental).
narrative_ontology:cs_reference_frame('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', takings_doctrine_with_predictable_extremes).
narrative_ontology:cs_drift_state('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', contemporary_regulatory_state_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1f8b3d2-d26c-4a99-a13b-b99c58dbba89', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_with_categorical_claims).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, public_interest_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, marginal_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, marginal_property_owners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, takings_clause_limits_regulatory_power).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, bright_line_rules_reduce_litigation_uncertainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landowners whose property experiences permanent physical occupation or total economic loss gain automatic compensation through the categorical rule, bypassing the uncertainty of Penn Central balancing. They benefit from predictable bright-line protection at the poles (certain takings) while maintaining the flexibility to argue Penn Central balancing in middle cases. Their exit is arbitrage: they can litigate to establish categorical status or accept diminished value.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_with_categorical_claims, beneficiary,
    powerful, generational, arbitrage, national).

% Environmental, zoning, and safety regulators must budget for compensation when regulations trigger categorical takings (permanent occupations like conservation easements, or total economic deprivation like wetland prohibitions on developable land). They bear the direct fiscal cost of compensation and face litigation risk in borderline cases where categorical status is contested. Their exit is constrained: they cannot abandon environmental or safety mandates without legislative change.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Environmental groups, housing advocates, and public health organizations that depend on regulation to internalize externalities (pollution, sprawl, public health risks) bear the political and fiscal cost when broad categorical rules trigger compensation obligations. They argue that categorical takings rules entrench property expectations against public goods and shift environmental costs to taxpayers. Their exit is constrained by dependence on the regulatory system itself.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, public_interest_coalitions, payer,
    organized, biographical, constrained, national).

% Small landowners in borderline cases (partial economic loss, intermittent occupations, mixed use restrictions) sit in the Penn Central balancing zone where categorical rules do not apply and case-by-case adjudication governs. They benefit from the possibility of compensation but face uncertainty, litigation costs, and the likelihood that three-factor balancing denies relief. Their exit is constrained by locality and capital limitations.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, marginal_property_owners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, marginal_property_owners, payer).

% Federal courts (especially the Supreme Court in Loretto, Lucas, and related cases) author and enforce the categorical/Penn Central framework. They set the boundaries of what counts as 'permanent physical occupation' and 'total economic loss,' and they administer the three-factor Penn Central test for middle cases. Their role is to stabilize expectations while preserving flexibility; their exit is analytical—they can only reframe doctrine through new decisions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts_applying_takings_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% Congress and state legislatures are structurally excluded from authoring takings doctrine within this reading frame (constitutional courts hold that authority). Legislatures could expand or contract compensation entitlements, but doing so would require amending the constitutional text or overriding judicial interpretation—both extremely difficult. They are trapped by the constitutional hierarchy.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legislative_bodies, excluded,
    institutional, generational, trapped, national).

% Scholars, economists, and policy analysts observe the constraint's operation and measure its effects on regulatory capacity, property value expectations, and litigation costs. They produce the data on which reform debates turn but do not directly collect from or bear costs through the constraint.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, property_owners_with_categorical_claims).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, judicially administrable boundary between compensable takings and non-compensable regulations by establishing categorical rules (per se) for extreme cases (permanent occupations, total value loss) and a structured three-factor test (Penn Central) for middle cases. Coordinates property owner expectations with regulatory capacity by separating cases where compensation is certain from cases where it is contextual.
% TRANSFER_FUNCTION: Moves compensation liability from property owners to public treasuries (federal and state) in categorical cases; in Penn Central cases, the transfer depends on case-by-case balancing of public vs. private interest. The constraint shapes which costs are internalized to regulators vs. socialized or borne by property owners.
% ABSENT_VOICES: Legislative bodies are structurally excluded from authoring the boundary itself (constitutional courts hold that authority). Property owners without resources to litigate are absent from the cases that set doctrine. Public interest groups argue they are muted by the constraint's bias toward property expectations over regulatory flexibility; they can file amicus briefs but cannot set the frame.
% DISAPPEARANCE_RATIONALE: If the categorical/Penn Central framework disappeared overnight, regulatory agencies would face radical uncertainty: they would not know which regulations trigger automatic compensation claims. Property owners would lose the bright-line protections at the poles and face uniform contextual balancing everywhere. Litigation would increase as both sides tested new boundaries. Public treasuries would need to recalibrate compensation budgets. The distribution of risk between property owners and the public would shift substantially.
% FOUNDING_PROBLEM: Early takings jurisprudence applied a single ad-hoc balancing test (weighing public benefit against private loss) to all regulations, leaving both property owners and regulators uncertain about when compensation was owed. Property owners faced unpredictable liability; regulators faced unpredictable claims. The categorical rule was designed to create bright-line certainty at the extremes (permanent occupations always compensable, total deprivation almost always compensable) while preserving regulatory flexibility in middle cases through contextual balancing.
% FOUNDING_PROBLEM_CORROBORATION: The categorical takings reading (this reading's proponents—property rights advocates, conservative judges) attests the founding problem is live: regulatory uncertainty remains for marginal cases and bright lines remain necessary. Environmental advocates and public-interest scholars attest the founding problem is substantially solved and the constraint now serves to entrench property expectations against environmental protection. The Supreme Court's own opinions (Lucas, Loretto) acknowledge both the coordination function and the extraction problem—the problem is authoritatively contested within the judiciary itself.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 (midpoint of the interval, 2024) because the constraint creates a dual structure: categorical bright lines at the poles that reliably protect some property owners (automatic compensation), combined with Penn Central balancing in middle cases that systematically favors property owner framing over regulatory justification. The Penn Central test—which requires courts to weigh public purpose against private loss—structurally advantages property interests because it foregrounds 'investment-backed expectations' and treats regulatory cost-shifting to public treasuries as relevant. Suppression is lower (0.41) because the constraint operates through judicial decision-making (courts publish reasoned opinions) rather than through coercive administrative machinery; property owners can litigate and sometimes prevail; regulatory agencies must comply with court orders but have formal due process. Theater is moderate (0.28) because the bright-line categorical rules perform a real coordination function (they do reduce litigation uncertainty), but over the interval an increasing share of judicial activity is devoted to policing the boundary between categorical and contextual cases (drawing the line around 'permanent' occupation, 'total' economic loss) rather than administering the rules themselves. Theater rises from 1978 (0.12) to 2014 (0.28) and plateaus, indicating stable maintenance of boundary-policing performance. Accessibility collapse at 0.58 reflects that alternatives to the categorical/Penn Central framework (pure ad-hoc balancing, pure property-owner bias, pure regulatory supremacy) all remain theoretically available but are politically entrenched against; property owners and regulators both have exit options (litigate, lobby for legislative change, relocate) but face high barriers. Resistance is high (0.72) because the takings clause itself is constitutional text that cannot be erased, and both property-owner and public-interest coalitions mount active resistance through litigation, legislative campaigns, and scholarship challenging the doctrine's application.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (regulatory agencies, public-interest coalitions) sees a snare masquerading as a rope: they experience the categorical rules as unpredictable triggers for unbudgeted compensation, and they experience Penn Central as a test that structurally favors property-owner framing. From their perspective, extraction is high, suppression is strong (they cannot opt out of regulation or stop paying compensation without legislative change), and the coordination story is a cover. The beneficiary seat (property owners, especially institutional ones) sees a rope: they experience categorical certainty at the poles as genuine coordination they depend on, and they experience Penn Central as a reasonable balancing of interests. The court seat (agenda-setter) sees neutrality and doctrine. The marginal-property-owner seat sees uncertainty and mixed blessing: the constraint helps in strong cases (permanent occupation, total loss) but leaves them vulnerable in weak cases. Mandatrophy is not present (function is not atrophied, it is actively maintained and litigated), but extraction accumulation is present (the range of what counts as 'permanent' or 'total' has expanded over time, triggering compensation more often). The classified type divergence should be: property-owner seat → rope, regulatory-agency seat → snare or tangled_rope, court seat → rope, marginal-owner seat → tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners with categorical claims sit at d ≈ 0.2 (full beneficiary pole): they collect the bright-line protection and the Penn Central framing that favors their interests; they have arbitrage-grade exit (can litigate, sell, relocate investment); they hold powerful institutional position. Regulatory agencies sit at d ≈ 0.8 (full target pole): they pay compensation when categorical triggers, they bear litigation risk, they face constrained exit (cannot abandon mandates without legislative change). Public-interest coalitions sit at d ≈ 0.7 (target-leaning): they do not directly pay compensation but bear the political and opportunity cost (environmental goods go unfunded, regulations face heightened scrutiny). Courts sit near symmetric (d ≈ 0.5) because they administer the constraint neutrally from a doctrine perspective, though they structurally favor the property-owner frame. Marginal property owners sit at d ≈ 0.55 (slightly target-leaning): they benefit from the possibility of compensation but face uncertainty and litigation cost, so the constraint's actual benefit to them is lower than to institutional property owners. Legislative bodies are excluded (not a stakeholder in the constraint's operation, so d is undefined).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory uncertainty in takings jurisprudence) remains live in the Penn Central zone, but the categorical rule has substantially solved it at the poles. The constraint could be classified as a rope (genuine coordination on takings boundaries) or a snare (extraction riding on the coordination). The classification hinges on whether the Penn Central test is itself part of the coordination function or a site of asymmetric extraction. The authored extraction at 0.62 reflects the current empirical state: the constraint does coordinate (bright lines are real and reduce litigation at extremes), but it also extracts (Penn Central balancing systematically favors property interests, shifts costs to public treasuries, and entrenches property expectations against regulatory need). The tangled_rope claim captures this dual structure: there is genuine coordination at the poles (categorical rules), and there is asymmetric extraction in the Penn Central zone (regulatory agencies are targets, property owners are beneficiaries, courts frame the test to advantage property owners). The measurement series shows base_extractiveness rising from 0.45 (1978, shortly after Loretto established categorical rules) to 0.62 (2024, after decades of Penn Central case law clarifying boundaries and property-owner-friendly factors). The rise reflects that as the doctrine matured, practitioners learned to frame cases to trigger categorical status, and courts increasingly accommodated property-owner arguments within Penn Central balancing. Theater_ratio rises from 0.12 to 0.28 and plateaus, indicating that after ~2005 the doctrinal work shifted from developing new categorical/balancing principles to policing boundaries and performing consistency, not deepening the coordination function itself. This pattern is consistent with mandatrophy (founding function solved, now maintained theatrically), but the constraint remains actively enforced because property owners press claims and regulators must budget for compensation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_boundary_drift,
    'Over time, has the set of regulations that count as ''permanent physical occupation'' or ''total economic loss'' expanded, contracting, or remained stable?',
    'Docket analysis of takings cases 1980–2024: measure the proportion of cases classified as per se takings, and the range of regulatory contexts they cover. Expansion would indicate the categorical rules are capturing more cases; contraction would indicate the definition has tightened.',
    'If categorical scope expanded, then effective extraction has risen even if the doctrine itself is unchanged—more regulations trigger automatic compensation. This would support mandatrophy-via-capture (the boundary policing has become cover for broadening the takings category). If the definition tightened, the constraint remains stable in scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_boundary_drift, empirical, 'Whether the categorical rules have expanded or contracted in practice over the interval.').

omega_variable(
    penn_central_factor_weighting,
    'Within Penn Central balancing, have courts systematically weighted the three factors (public purpose, economic impact, investment-backed expectations) in a stable ratio, or has the weighting shifted over time?',
    'Regression analysis of Penn Central outcomes 1978–2024: code each case for factor-by-factor holdings and compute the relative weight courts assign to each factor over decades. Stable weighting supports neutrality; shifting weights indicate doctrine has moved.',
    'If investment-backed-expectations factor has gained weight relative to public-purpose factor, the constraint has become more property-owner-favoring over time, supporting extraction accumulation. If weights have held stable, Penn Central has remained balanced in principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(penn_central_factor_weighting, empirical, 'Whether Penn Central balancing has drifted toward property-owner framing.').

omega_variable(
    regulatory_chilling_effect_scope,
    'To what extent do regulatory agencies avoid or water down environmental, zoning, and safety regulations specifically because they trigger takings exposure under the categorical/Penn Central framework?',
    'Comparative policy study: jurisdictions with different takings regimes (some with categorical rules, some without; some with stronger Penn Central protections for property owners, some with weaker) and measure adoption and stringency of environmental/zoning/safety regulations. Chilling effect presence would show in lower stringency in high-takings-risk jurisdictions.',
    'Strong chilling effect would demonstrate extraction from regulatory agencies is real and substantial—they forgo public goods because they cannot afford compensation. Weak or absent chilling effect would indicate the constraint''s extractive impact is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chilling_effect_scope, empirical, 'Whether takings exposure actually chills regulatory innovation.').

omega_variable(
    categorical_vs_sibling_readings_structural_position,
    'This reading (categorical: per se at poles + Penn Central in middle) coexists with the physical_appropriation_reading (per se only for direct seizures) and the regulatory_takings_reading (flexible balancing everywhere). Where do the sibling readings place the boundary between compensable and non-compensable, and how do those boundaries'' positions differ structurally?',
    'Doctrinal mapping: for each sibling reading, identify the canonical case law and the explicit or implicit boundary rule. Map the boundaries onto a spectrum of regulatory intensity (from direct appropriation to total deprivation to partial harm). Compare the beneficiary/victim sets each reading creates.',
    'If the categorical reading''s boundary sits between the siblings'' boundaries (balancing), then it is the middle-ground compromise. If it sits at an extreme, it favors one set of property owners over another. This clarifies whether the categorical reading is integrative (tangled rope attempting to hold coordination and extraction in tension) or partisan (a snare for one seat).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_sibling_readings_structural_position, conceptual, 'The categorical reading''s boundary position relative to sibling readings'' boundaries.').

omega_variable(
    compensation_realization_gap,
    'Among property owners who win takings claims (categorical or Penn Central), what proportion actually receive compensation, and at what delay?',
    'Case outcomes database: track takings judgments from verdict to final payment (or settlement). Measure win-to-compensation realization ratio and average payment delay. High realization and short delays support the constraint as genuine coordination; low realization or long delays support extraction (the promise of compensation is not reliably fulfilled).',
    'If property owners win takings claims but face years of litigation and failed collections, the extractive burden is lower than the nominal doctrine suggests—promises of compensation that are not realized are not extraction. If realization is high and prompt, the constraint genuinely transfers wealth from public treasuries to property owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_realization_gap, empirical, 'Whether takings compensation is actually realized after judgment.').

omega_variable(
    kernel_contest_reading_allocation,
    'This reading (categorical) instantiates ONE interpretation of the takings clause kernel. The sibling readings (physical_appropriation, regulatory_takings) instantiate alternative interpretations. How are these readings currently allocated across jurisdictions, courts, and periods? Is one reading dominant, or do all three coexist as live options?',
    'Jurisprudential mapping: identify which courts, circuits, and periods adopt which reading. Current state (2024): the categorical reading dominates U.S. Supreme Court and most federal circuits; the physical_appropriation reading has pockets of state-court adoption; the regulatory_takings reading remains a minority scholarly position with growing political support. The allocation reflects the institutional authority (Supreme Court > circuits > state courts > scholarship).',
    'If the categorical reading is institutionally dominant and the others are marginal, this reading has a structural advantage in setting default expectations. The other readings coexist as alternatives available for advocacy but face higher barriers to adoption. This is what ''coexists_with'' means in the reading_relations frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_allocation, empirical, 'The institutional allocation of the three takings readings across U.S. jurisdictions and courts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement_basis(taki_tr_t1978, observed).
narrative_ontology:measurement(taki_tr_t1987, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1987, 0.16).
narrative_ontology:measurement_basis(taki_tr_t1987, observed).
narrative_ontology:measurement(taki_tr_t1996, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1996, 0.21).
narrative_ontology:measurement_basis(taki_tr_t1996, observed).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement_basis(taki_tr_t2005, observed).
narrative_ontology:measurement(taki_tr_t2014, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement_basis(taki_tr_t2014, observed).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(taki_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement_basis(taki_be_t1978, observed).
narrative_ontology:measurement(taki_be_t1987, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1987, 0.52).
narrative_ontology:measurement_basis(taki_be_t1987, observed).
narrative_ontology:measurement(taki_be_t1996, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1996, 0.58).
narrative_ontology:measurement_basis(taki_be_t1996, observed).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(taki_be_t2005, observed).
narrative_ontology:measurement(taki_be_t2014, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement_basis(taki_be_t2014, observed).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(taki_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement_basis(taki_su_t1978, observed).
narrative_ontology:measurement(taki_su_t1987, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1987, 0.37).
narrative_ontology:measurement_basis(taki_su_t1987, observed).
narrative_ontology:measurement(taki_su_t1996, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1996, 0.39).
narrative_ontology:measurement_basis(taki_su_t1996, observed).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(taki_su_t2005, observed).
narrative_ontology:measurement(taki_su_t2014, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2014, 0.41).
narrative_ontology:measurement_basis(taki_su_t2014, observed).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.41).
narrative_ontology:measurement_basis(taki_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, environmental_regulation_permitting_system).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, land_use_planning_and_zoning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested takings-clause kernel. The categorical_takings_reading establishes per se compensation for permanent physical occupations and total economic deprivation, plus Penn Central balancing for middle cases. The physical_appropriation_reading restricts per se status to direct seizures. The regulatory_takings_reading applies flexible balancing to all regulations. All three stories share the same constitutional text (Fifth Amendment takings clause) but instantiate structurally distinct constraints with different beneficiary/victim sets and extraction profiles. The categorical reading's distinctive contribution is the attempt to stabilize expectations through bright-line rules while preserving regulatory flexibility—a middle-ground position between the stricter physical_appropriation reading (which favors regulatory capacity) and the more expansive regulatory_takings reading (which favors property owners). The three readings form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
