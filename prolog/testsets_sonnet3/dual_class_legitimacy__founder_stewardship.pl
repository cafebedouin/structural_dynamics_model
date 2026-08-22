% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure as Founder Fiduciary Stewardship
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This story instantiates the founder_stewardship reading of the
 *   dual_class_legitimacy kernel: dual-class share structures at IPO, in
 *   which a founder retains super-voting Class B shares while selling Class A
 *   shares to public investors, are read here as a genuine coordination
 *   mechanism that insulates long-horizon mission execution from short-term
 *   market pressure, with Class A holders benefiting indirectly through
 *   eventual mission success. This is one reading among three siblings
 *   sharing the same underlying kernel (the legitimacy of concentrated
 *   founder voting power): disclosure_consent locates legitimacy in informed
 *   consent at time of purchase rather than in control parity or stewardship
 *   outcomes, and minority_extraction reads the identical structural facts as
 *   capital-proportional entitlement violated by voting disproportion. All
 *   three share the same standing arrangement (existing dual-class
 *   structures) but author different ε, different beneficiary/victim sets,
 *   and different classifications from that shared referent, per the
 *   reading-indexed ε rule.
 *
 * KEY AGENTS:
 *   - founder_ceo: agenda_setter/beneficiary (institutional/arbitrage) - sets strategy, captures upside, insulated from override
 *   - class_a_shareholders: beneficiary/payer (organized/mobile) - benefit if mission succeeds, minimal voting leverage
 *   - class_a_shareholders_dissenting: payer (organized/constrained) - object to specific decisions, structurally unable to prevail
 *   - public_pension_fund_investors: payer (moderate/constrained) - index-mandated holders with no practical exit
 *   - activist_investors: excluded (powerful/trapped) - the class this mechanism specifically forecloses
 *   - securities_regulators: observer (institutional/analytical) - evaluate disclosure sufficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.55).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure as Founder Fiduciary Stewardship").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '42ba6b8c-0b6a-4e42-8629-d20c226b68b1').
narrative_ontology:cs_kernel_codification('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', formalized).
narrative_ontology:cs_authority_grounding('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', practice).
narrative_ontology:cs_interpretation_layer_present('42ba6b8c-0b6a-4e42-8629-d20c226b68b1').
narrative_ontology:cs_reading_relation('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', foundational, founder_discretion_serves_all_shareholders_over_long_horizon).
narrative_ontology:cs_axiom_status(founder_discretion_serves_all_shareholders_over_long_horizon, holdable).
narrative_ontology:cs_axiom_grounding('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', founder_discretion_serves_all_shareholders_over_long_horizon, empirically_contingent).
narrative_ontology:cs_axiom('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', secondary, control_concentration_is_coordination_not_entitlement_violation).
narrative_ontology:cs_axiom_status(control_concentration_is_coordination_not_entitlement_violation, holdable).
narrative_ontology:cs_axiom_grounding('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', control_concentration_is_coordination_not_entitlement_violation, instrumental).
narrative_ontology:cs_reference_frame('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', founder_mission_protection_period).
narrative_ontology:cs_drift_state('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', post_ipo_maturity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42ba6b8c-0b6a-4e42-8629-d20c226b68b1', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_employees).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_shareholders_dissenting).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, public_pension_fund_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_ceo).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, founder_led_firms_outperform_over_long_horizons).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, mission_continuity_requires_insulation_from_quarterly_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B super-voting shares carrying 10x or 20x the voting weight of Class A shares while owning a minority of total equity. Sets strategic direction, controls board composition, and can override any Class A shareholder vote on matters short of a handful of statutorily reserved actions. Frames the structure as insulating the company's mission from market pressure that would otherwise force premature profit extraction. Personally captures upside from both equity appreciation and the discretion to pursue long-horizon bets without shareholder veto.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_ceo, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founder_ceo, beneficiary).

% Purchase publicly traded low-vote shares knowing the control structure at time of investment; benefit if the founder's long-horizon bets pay off, but hold negligible voting leverage to change course, replace management, or block self-dealing transactions. Can sell shares (liquid public market) but cannot exit the governance structure while remaining invested; disclosure at IPO means the arrangement was knowable in advance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_a_shareholders, payer).

% A subset of Class A holders who conclude the founder's strategic choices (dilutive acquisitions, executive compensation, related-party transactions) destroy value; they can vote against management at the margins but their votes are structurally incapable of prevailing given the super-voting share weight. Their only real remedy is selling at a depressed price or litigating breach of fiduciary duty, a high-cost and low-success-rate path.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders_dissenting, payer,
    organized, biographical, constrained, global).

% Index-tracking and passive pension funds are structurally required to hold shares of index-included dual-class companies regardless of governance objections, since divestment would create tracking error against fiduciary benchmarks. They bear governance risk without meaningful voice, and their exit option is effectively foreclosed by their own mandate to track the index rather than by the company.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, public_pension_fund_investors, payer,
    moderate, generational, constrained, national).

% Employees whose equity compensation and career trajectory benefit from the company pursuing multi-year technical or mission-driven bets that would be difficult to sustain under quarterly activist pressure; they can leave for other employers but have no direct governance voice either way.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_horizon_employees, beneficiary,
    moderate, biographical, mobile, national).

% Would ordinarily accumulate stakes and press for board seats, spinoffs, or buybacks to unlock value; the super-voting structure makes a proxy contest mathematically futile regardless of accumulated stake, so this class of check on management is foreclosed by design rather than by market outcome.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_investors, excluded,
    powerful, biographical, trapped, global).

% Evaluate whether disclosure at IPO satisfies investor-protection obligations and whether listing standards should impose sunset provisions on super-voting structures; can shift exchange listing rules but have historically deferred to disclosure-based legitimacy over structural parity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_ceo).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A founder with unusually deep domain knowledge or mission commitment is insulated from short-term market discipline that would otherwise force premature harvesting of long-horizon investments (R&D, market-building, platform bets), enabling coordination around a multi-year strategy that a dispersed, liquidity-driven shareholder base could not commit to holding.
% TRANSFER_FUNCTION: Moves governance control from capital-proportional voting (one share, one vote) to founder-proportional voting (super-voting shares), and moves the associated decision rights and any resulting value capture or destruction from the general shareholder base toward the founder's discretion.
% ABSENT_VOICES: Activist investors and would-be board challengers are structurally excluded from the mechanism that would ordinarily let them contest strategy regardless of how large a stake they accumulate; passive pension fund investors are present as capital but have no practical voice given index-tracking constraints.
% DISAPPEARANCE_RATIONALE: If the dual-class structure vanished overnight, the founder's ability to resist activist pressure and pursue long-horizon bets would disappear, and proponents argue the company would drift toward short-term optimization; critics argue governance would simply become capital-proportional and any subsequent underperformance would reflect the founder's actual (rather than presumed) value-add being tested by ordinary market discipline. Which world is 'the real one' is exactly what is contested between the readings of this kernel.
% FOUNDING_PROBLEM: Newly public companies with founder-led technical or mission-driven strategies faced the risk that public market shareholders, prioritizing near-term returns, would force premature pivots, cost-cutting, or sale of the company before long-horizon value could be realized.
% FOUNDING_PROBLEM_CORROBORATION: Some institutional investors and governance researchers outside the founder's circle corroborate that certain founder-led firms (frequently cited: technology and biotech ventures with multi-year R&D horizons) have produced outcomes consistent with the stewardship story. Other outside observers — proxy advisory firms, several major public pension funds, and academic corporate-governance scholars — corroborate the opposing view that the mechanism persists well past any plausible mission-protection need and functions primarily to entrench management from accountability; no consensus corroboration exists from a party outside the founder's own beneficiary set that unambiguously supports the stewardship framing as the dominant explanation.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end, rising from 0.28) because under the stewardship reading the founder's discretion is presumed to be exercised, on average, in ways that increase aggregate value even where it reduces minority voice — the coordination function is real and non-trivial, not merely cover. Suppression is authored moderately high (0.55) because the mechanism does actively foreclose activist and dissenting-shareholder recourse regardless of how the founder actually performs; that foreclosure is structural (statutory/charter-based super-voting rights) and does not depend on whether the founder is in fact a good steward. Theater ratio stays low (0.2) because the coordination function is genuinely operative, not primarily performative, under this reading. Suppression is authored flat-to-rising, reflecting hardening of super-voting protections over successive charter amendments and IPO structuring practice, not a metric scaled by power or scope — per the ε-invariance and unscaled-suppression rules.
 *
 * PERSPECTIVAL GAP:
 *   The founder's seat and the dissenting Class A / pension fund seats compute structurally differently from identical facts: from the founder's seat, the arrangement is textbook coordination — a mission-critical actor protected from a collective-action problem (dispersed shareholders unable to commit to a long horizon). From the constrained payer seats, the identical super-voting structure is an entrenchment device that happens to coincide with good outcomes in cases the founder's defenders cite, and coincide with poor outcomes in cases they do not. The engine computes each seat's type from the declared power/exit/scope data; this story does not adjudicate which seat is 'right' — it authors the founder_stewardship reading's structural facts honestly, including facts (activist exclusion, structural suppression) that a purely favorable telling would omit.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_ceo sits at the clear beneficiary end: institutional power, arbitrage-grade exit (can sell down over time, retains control throughout), direct capture of both mission-success upside and governance discretion. Class_a_shareholders sit closer to symmetric-to-beneficiary: they knowingly bought into the structure and profit if the bet pays off, but lack proportional control, so their d is pulled toward the target end by the voting-power asymmetry even as the beneficiary declaration pulls it back. Dissenting Class A holders and pension funds are declared victims because their situation is defined by inability to convert their capital stake into governance leverage regardless of outcome — trapped or constrained exit locks their directionality toward the target end. Activist investors are excluded rather than victimized in the beneficiary/victim sense — they are foreclosed from the game entirely rather than paying into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading resists collapsing into either 'pure coordination, no extraction' or 'pure extraction dressed as coordination' by requiring both a genuine coordination function (declared beneficiaries, non-trivial mission-continuity value) and requiring active enforcement plus a named victim class (dissenting shareholders, pension funds) — the tangled_rope gates. This prevents the story from claiming Rope status (which would deny that anyone pays) while also refusing to claim Snare status (which would deny any genuine coordination benefit exists). Whether the founding problem (market impatience with long-horizon bets) remains live is marked contested rather than resolved in either direction, because corroboration exists on both sides but not from a party outside the founder's own beneficiary set unambiguously endorsing the stewardship framing as dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_reading_committer_structure,
    'Is the founder_stewardship reading a description of what the dual-class mechanism actually does on average across founder-led firms, or is it a legitimating narrative that happens to be true in the specific high-profile cases most commonly cited in its defense?',
    'Longitudinal comparison of dual-class firm performance and minority-shareholder outcomes against single-class comparables, controlling for founder quality and industry, across multiple market cycles rather than a single bull-market cohort.',
    'If the stewardship pattern holds broadly, this reading''s beneficiary declaration for class_a_shareholders and long_horizon_employees is empirically grounded; if the pattern is driven by a small number of survivorship-biased success stories, the reading functions closer to the minority_extraction sibling''s account with a favorable label attached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_reading_committer_structure, empirical, 'Whether the stewardship narrative describes an actual average pattern or a survivorship-biased legitimating story.').

omega_variable(
    sibling_reading_foreclosure_location,
    'Where exactly does the founder_stewardship reading''s core premise diverge from minority_extraction''s — is it a disagreement about facts (does concentration actually produce better outcomes) or a disagreement about entitlement (is proportional governance a right regardless of outcomes)?',
    'None fully resolves this — it is the structural location of the kernel contest itself. Partial resolution: separating the empirical outcome question (addressed by the omega above) from the entitlement question, which is normative and not resolvable by performance data alone.',
    'If the disagreement is purely empirical, better data could in principle converge the readings; if it is genuinely normative (capital-proportional entitlement as a value independent of outcomes), the readings remain permanently coexisting rather than resolvable, which supports the coexists_with relation declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_location, conceptual, 'Whether the kernel contest between stewardship and extraction readings is empirical or normative in character.').

omega_variable(
    sunset_clause_absence,
    'Does the absence of a mandatory sunset or time/ownership-decay provision on super-voting rights in most dual-class structures undermine the stewardship reading''s own logic, which is premised on protecting a specific transitional mission-execution period rather than permanent entrenchment?',
    'Comparison of firms with sunset provisions (time-based or founder-ownership-threshold-based) against those without, on long-run minority shareholder outcomes and founder departure/succession events.',
    'If firms with sunset provisions show materially better minority outcomes without sacrificing the claimed mission-protection benefit, this would suggest the stewardship reading, taken seriously on its own terms, should require has_sunset_clause — and its absence in most real structures is evidence the mechanism has drifted from stewardship toward entrenchment even under this reading''s own logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_absence, conceptual, 'Whether the stewardship reading''s own internal logic implies a sunset requirement that most real-world structures lack.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__founder_stewardship, theater_ratio, 4, 0.12).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__founder_stewardship, theater_ratio, 8, 0.14).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.16).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__founder_stewardship, theater_ratio, 16, 0.18).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__founder_stewardship, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__founder_stewardship, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__founder_stewardship, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dual_su_t4, dual_class_legitimacy__founder_stewardship, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__founder_stewardship, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__founder_stewardship, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dual_class_legitimacy kernel, all describing the same standing arrangement (public dual-class share structures) but authoring different ε, beneficiary/victim structures, and classifications. founder_stewardship (this story, tangled_rope) coexists with minority_extraction (likely snare-leaning, capital-proportional entitlement violated) and disclosure_consent (likely rope-leaning, legitimacy grounded in informed consent rather than outcomes). Network edges link all three; no single file should be read as 'the' dual-class constraint — each is a distinct instantiation per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
