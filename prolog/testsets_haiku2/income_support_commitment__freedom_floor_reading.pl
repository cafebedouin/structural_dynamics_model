% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint embodies one reading of the income-support-commitment
 *   kernel: unconditional income as enabling autonomy, dignity, and genuine
 *   labor market exit capacity. The reading frames the constraint as a
 *   coordination solution to the collective problem of funding a baseline
 *   floor sufficient for autonomy without stigmatizing means-testing. It
 *   declares no victims (universality eliminates the victim set that
 *   means-testing creates); beneficiaries are those whose exit capacity or
 *   autonomy is enabled: caregivers, precarious workers, abuse survivors, and
 *   risk-taking entrepreneurs. The constraint is CLAIMED as rope (genuine
 *   coordination, minimal coercion, participant beneficiaries) and the
 *   authored metrics describe low extractiveness (0.18), minimal suppression
 *   (0.12), and low theater (0.08) — consistent with the claim. The engine
 *   will measure whether the computed type from structural data aligns with
 *   this claim; divergence would indicate misclassification or misalignment
 *   in the reading's own terms.
 *
 * KEY AGENTS:
 *   - Caregivers (predominantly women): powered by unconditional income to choose care work as legitimate labor rather than economic desperation
 *   - Precarious workers: enabled to refuse exploitative work and negotiate from a position of autonomy rather than desperation
 *   - Abuse survivors: financially decoupled from abusive partners or situations
 *   - Artists and entrepreneurs: runway for risk-taking and experimentation without market-driven premature commercialization
 *   - Taxpayers and employers: bear the fiscal and structural cost; benefit from labor market with genuine alternatives
 *   - State apparatus: agenda-setter and administrator; holds the commitment to universality and unconditional delivery
 *   - Exclusionary movements: structurally excluded from this reading's design; would argue for citizenship conditionality, work requirements, or means-testing efficiency if seated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '2708d1fc-af94-4477-8b7b-ddcaecf6f8cd').
narrative_ontology:cs_kernel_codification('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', distributed).
narrative_ontology:cs_authority_grounding('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', distributed).
narrative_ontology:cs_reading_relation('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', foundational, universality_necessary_for_dignity).
narrative_ontology:cs_axiom_status(universality_necessary_for_dignity, holdable).
narrative_ontology:cs_axiom_grounding('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', universality_necessary_for_dignity, deontological).
narrative_ontology:cs_axiom('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', foundational, exit_capacity_enables_genuine_choice).
narrative_ontology:cs_axiom_status(exit_capacity_enables_genuine_choice, holdable).
narrative_ontology:cs_axiom_grounding('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', exit_capacity_enables_genuine_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', autonomy_enabling_income_floor).
narrative_ontology:cs_drift_state('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', contemporary_welfare_state_targeting_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2708d1fc-af94-4477-8b7b-ddcaecf6f8cd', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, general_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals (predominantly women) engaged in unpaid care work — raising children, elder care, community support. Without income support, caregiving forces economic dependence on wage-earner partners or state means-tested welfare. Unconditional income enables choosing care work as legitimate labor and negotiating household economics from a position of autonomy rather than desperation.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, generational, identity_locked, national).

% Gig workers, seasonal laborers, and low-wage workers whose employment is unstable or abusive. Unconditional income provides a floor below which wages need not fall and enables refusal of exploitative working conditions. Without it, desperation drives acceptance of any terms.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Individuals in domestic violence, economic abuse, or coercive control relationships. Unconditional income breaks the financial lock that keeps them trapped: they can leave abusive partners or situations without facing destitution. Means-tested welfare visible to abusers erodes this function.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, trapped, national).

% Creative workers, startup founders, and risk-taking entrepreneurs whose early-stage work generates no income. Unconditional income decouples survival from immediate market success, enabling longer runway for experimentation and risk-taking. Without it, market pressure forces premature commercialization or abandonment.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, generational, mobile, national).

% Wage-earners and asset-holders whose taxes fund the income support transfer. They bear the fiscal cost of the arrangement and must coordinate to sustain funding via progressive taxation. The constraint requires a political commitment to universal coverage without means-testing stigma.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).

% Firms benefit from the constraint by gaining access to workers with genuine alternatives to exploitative work — the floor constrains their wage-setting power only downward. They also bear part of the tax cost. The net structural position differs by firm: low-wage employers lose power; high-wage firms can absorb the shift.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, employers, payer).

% Enacts and administers the unconditional income mechanism. Bears the burden of tax collection and distribution infrastructure. Can modulate eligibility, rate, and funding — the constraint exists only through sustained state commitment.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Political actors arguing for citizenship-based conditionality, means-testing, or work requirements who are not seated at the core of this reading's design. Their exclusion from the 'freedom floor' design means they would argue for tighter targeting if admitted to the conversation, but are structured out by the universality commitment.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, exclusionary_movements, excluded,
    moderate, biographical, constrained, national).

% Researchers and economists evaluating the constraint's effects on labor supply, autonomy, and exit capacity. They gather evidence on whether the arrangement achieves its stated function and how it compares to alternative designs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of funding a baseline income floor sufficient to enable genuine labor market exit, caregiving, risk-taking, and autonomy for all participants, without stigmatizing means-testing or creating perverse incentives. Coordinates tax base, distribution mechanism, and social legitimacy of unconditional support.
% TRANSFER_FUNCTION: Moves income from higher-earning, lower-care-burden participants to lower-earning and care-burdened participants — and from present workers to future creators and risk-takers. The transfer is universal (no means test) to eliminate surveillance and stigma; the funding mechanism is progressive taxation.
% ABSENT_VOICES: Citizenship-restricting movements (arguing for work requirements or immigration conditionality), employer wage-maximization advocates (who would oppose the labor-market-exit capacity), and traditional means-testing efficiency advocates (who would argue for targeting) are structurally excluded from this reading's design. Their objections would center on work incentives, fiscal cost, and deservingness — grounds this reading's design deliberately sidesteps.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, caregivers would re-enter forced dependency or low-wage work, precarious workers would face heightened exploitation pressure, abuse survivors would lose an exit mechanism, and entrepreneurs would reduce risk-taking. Wage floors would soften, bargaining power would shift sharply toward employers, and the labor market would reorganize around scarcity-driven desperation rather than genuine choice.
% FOUNDING_PROBLEM: Pre-industrial and industrial labor markets structured work as the only path to survival, and caregiving, creativity, and risk-taking as luxuries or privileges. Modern economies have sufficient productive capacity to decouple survival from participation in any specific labor market. The founding problem is: how do you enable autonomy, dignity, and genuine choice in a high-productivity economy without leaving any participant vulnerable to coercion?
% FOUNDING_PROBLEM_CORROBORATION: Labor economists (Piketty, Standing), feminist theorists (Fraser, Pateman), and autonomy-focused policy researchers (Van Parijs, Raventós) outside the welfare-state apparatus attest the problem remains live: labor markets continue to use subsistence pressure as the discipline mechanism, caregiving remains economically penalized, and exit capacity remains scarce for powerless agents. Historical records show that wherever unconditional income has been piloted or implemented (Finland 2017–2018, Kenya GiveDirectly trials, Canada MINCOME), documented increases in bargaining power and exit capacity, reduction in abuse entrapment, and maintenance or increase in productivity support the founding problem's persistence and the arrangement's function.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint solves a genuine collective-action problem (funding a universal floor) without creating asymmetric coercion or forced transfers. The 'transfer' is from higher-earning to lower-earning and care-burdened participants, but this is coordinated redistribution, not extraction — the beneficiaries gain autonomy and exit capacity, not subsistence at the cost of dignity. Suppression is minimal (0.12) because there is no enforced compliance mechanism: the constraint's persistence depends on political commitment to universality and tax base stability, not on active coercion of defectors. Theater is low (0.08) because the mechanism is structurally simple: taxation, distribution, no means-testing gate or surveillance. The slight rise in extractiveness from t=0 to t=40 reflects modest fiscal pressure as aging and care demands increase, not a shift toward extraction — the constraint remains fundamentally coordinative. The single shared time grid ensures every metric is authored at every examined point, with basis tagged 'observed' for historically grounded measurements from pilot programs and implemented systems (Finland, Canada, Kenya trials).
 *
 * PERSPECTIVAL GAP:
 *   The reading assumes structural alignment across seats: caregivers, precarious workers, and risk-taking entrepreneurs all benefit from the floor; taxpayers absorb the cost as a coordinated commitment to mutual autonomy. But from the seat of low-wage employers (particularly in service sectors that rely on desperation-driven labor), the constraint operates as an enforced floor that constrains wage-setting power — they experience it as extraction. From the seat of wealth-concentrating asset holders, the progressive taxation that funds it is experienced as asymmetric redistribution. However, within this reading's framework, these are not seats at the table: the reading deliberately structures the design around autonomy rather than employer wage-maximization or asset concentration. The engine's per-seat computation should surface the employers' divergent d value (higher d = more target-like) relative to the beneficiary seats, making this perspectival gap explicit in the classified output.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (caregivers, precarious workers, abuse survivors, artists) all have low power, constrained or identity-locked exit options, and substantial spatial scope — they gain the most from the floor. Their directionality d should be near the beneficiary end (0.1–0.3): the constraint subsidizes their autonomy and exit capacity. Taxpayers bear a real cost but gain coordination benefit (labor market with genuine alternatives, reduced desperation-driven crime and social fragmentation, dignity for caregivers); their d sits near symmetric (0.4–0.6). Employers as a group sit near symmetric or slight-payer: they lose wage-setting power on the low end but gain access to a labor force with genuine alternatives (lower turnover, higher morale, higher productivity); larger firms can absorb the cost better than small low-wage employers, so d varies within the organized group. State apparatus is analytical (exit_options: analytical) and its d is not computed from beneficiary/victim structure — it administers the arrangement. Exclusionary movements have constrained exit (they cannot overturn the commitment without political struggle) and would position themselves as payers if seated, but they are excluded by design, so their d is not computed here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to enable autonomy in a high-productivity economy) remains live, and the arrangement's function (enabling exit capacity, autonomy, dignity) is actively deployed — there is no mandatrophy. However, the reading must address a critical omega: whether political commitment to universality can hold under fiscal pressure or identity-based objections. If universality erodes into means-testing or work-conditionality, the constraint would mutate from rope (universal coordination) to snare or tangled rope (extraction from the means-tested poor). The reading's type is robust to this transformation only if the underlying commitment holds. Mandatrophy would arise if the founding problem became genuinely obsolete (i.e., if labor markets were restructured to eliminate coercion without income support) or if the state capacity to fund the floor collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_efficiency_tradeoff,
    'Can unconditional, universal income support be sustained politically and fiscally, or does cost pressure inevitably drive the system toward means-testing and conditionality?',
    'Longitudinal implementation tracking (Finland 2017–2018, Kenya GiveDirectly long-term, proposed Canadian programs): observe whether political commitment to universality holds under fiscal pressure, demographic aging, and opposition from targeting advocates.',
    'If universality erodes, the constraint mutates from rope (universal coordination) to snare (means-tested extraction). The reading''s core axiomatic claim (universality as dignity preservation) becomes untenable within this reading''s own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_efficiency_tradeoff, empirical, 'Whether political commitment to universality is sustainable or yields to means-testing pressure.').

omega_variable(
    labor_supply_behavioral_response,
    'Does unconditional income reduce labor force participation in ways that undermine the productive capacity to fund the floor itself, or does it enable productivity increases through education, health, and risk-taking that sustain the tax base?',
    'Experimental and quasi-experimental evidence from unconditional income pilots: measure labor force participation, skill investment, entrepreneurship, and overall productivity vs. counterfactual. Compare sectors and demographics.',
    'If labor supply contracts sharply, the fiscal sustainability of the floor degrades and the constraint becomes extractive (funding becomes zero-sum, payers lose coordination benefit). If productivity increases or stabilizes, the reading''s viability strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_behavioral_response, empirical, 'Whether unconditional income enables or undermines the productive base needed to sustain funding.').

omega_variable(
    suppression_of_exit_coercion_mechanisms,
    'Is suppression of coercive labor market mechanisms (employer desperation-leverage, abuser financial trapping) a structural property of the income floor, or is it eroded by internalized labor-market discipline and cultural norms around deservingness?',
    'Post-exit and transgenerational tracking: measure whether abuse survivors and precarious workers maintain autonomy gains after leaving the constraint; assess whether second generation raised with floor internalizes different labor-market expectations or reproduces desperation-driven compliance.',
    'If suppression of coercion is structural and durable, the autonomy gains hold across generations; if eroded by cultural internalization, the constraint loses its function for future cohorts and reverts to performative universality (theater_ratio rises).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_exit_coercion_mechanisms, empirical, 'Whether the floor''s suppression of coercive mechanisms is structurally durable or culturally vulnerable.').

omega_variable(
    employer_capture_under_low_wage_pressure,
    'Do employers in low-wage sectors (service, care, agriculture) capture the floor through political pressure to lower the rate, or does the floor hold as a genuine constraint on wage-setting?',
    'Comparative political economy: track employer lobby effectiveness in implemented unconditional-income jurisdictions; observe whether rate settings hold at autonomy-enabling levels or erode under employer pressure.',
    'If captured, the floor loses functional teeth as a labor-market exit mechanism and the constraint becomes performative (theater_ratio rises, beneficiaries gain symbolic dignity but not actual autonomy). If held, employer capture reveals the constraint as snare (extraction from low-wage employers constrained from cutting wages).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_capture_under_low_wage_pressure, empirical, 'Whether the income floor is politically sustainable against low-wage employer pressure.').

omega_variable(
    citizenship_vs_universal_boundary,
    'Is the ''unconditional'' commitment genuinely universal or conditioned on citizenship, legal residency, or other inclusion criteria that reconstruct a victim set outside the listed beneficiaries?',
    'Policy text analysis and implementation audit: determine whether the stated commitment is truly unconditional or bounded by membership criteria. Assess political pressure toward citizenship-conditionality.',
    'If universal commitment holds (no membership gates), no victim set is created and the reading''s claim stands. If conditioned on citizenship or status, a victim set (undocumented migrants, asylum seekers) re-emerges and the reading mischaracterizes the constraint as victimless.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citizenship_vs_universal_boundary, empirical, 'Whether the reading''s universality commitment is genuinely unconditional or bounded by membership criteria.').

omega_variable(
    alternative_framing_interdependence_vs_autonomy,
    'Is the reading''s axiomatic claim (that unconditional income enables autonomy and dignity) compatible with a simultaneous commitment to interdependence and collective care? Or does the autonomy frame inadvertently elevate individual independence over relational reciprocity, licensing a different kind of extraction?',
    'Feminist and communitarian theory engagement: articulate whether the reading''s autonomy and dignity framing foreclose non-individualist framings of care and interdependence. Examine whether implementation treats care work as a legitimate labor form or as a leisure choice to be left out of economic accounting.',
    'If the autonomy frame forecloses interdependence language, the reading may inadvertently stigmatize care (creating a victim set of caregivers treated as welfare recipients rather than legitimate workers) even while claiming to enable caregiving. This would indicate a reading-internal contradiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_framing_interdependence_vs_autonomy, conceptual, 'Whether the reading''s autonomy framing is compatible with genuine valuation of interdependent care work or constitutes a different form of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__freedom_floor_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__freedom_floor_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__freedom_floor_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(inco_tr_t32, income_support_commitment__freedom_floor_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__freedom_floor_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__freedom_floor_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__freedom_floor_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(inco_be_t32, income_support_commitment__freedom_floor_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three structurally distinct constraint stories, each a different reading of the same policy commitment: (1) freedom_floor_reading (this file) frames unconditional income as enabling autonomy and exit capacity; (2) dependency_trap_reading frames it as creating work-disincentive and state dependence; (3) targeting_efficiency_reading frames it as fiscal efficiency trade-off between coverage and cost. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and types because they operate under different axioms about the standing arrangement's referent and effects. The freedom_floor_reading's ε (0.18) is low because the reading construes the arrangement as genuine coordination with no inherent extraction. The dependency_trap_reading's ε would be higher (extraction from productive workers to fund disincentive) and the targeting_efficiency_reading's ε would be medium (transfer asymmetry justified by efficiency, but creating victim set of means-tested poor). These are not measurements of the same constraint from different angles — they are different constraints grounded in the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
