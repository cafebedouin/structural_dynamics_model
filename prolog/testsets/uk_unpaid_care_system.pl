% ============================================================================
% CONSTRAINT STORY: uk_unpaid_care_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_unpaid_care_system, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_unpaid_care_system
 *   human_readable: The UK's Reliance on Unpaid Carers for Social and Healthcare
 *   domain: economic/social
 *
 * SUMMARY:
 *   The UK's social care system has become structurally dependent on unpaid
 *   family labor provided by approximately 5.3 million carers, typically
 *   family members supporting elderly, disabled, or chronically ill
 *   relatives. This constraint represents a systematic extraction of labor,
 *   time, and health from carers without compensation or genuine alternative
 *   options. Over three decades, as the state reduced funding for social care
 *   and shifted responsibility toward families, the extractiveness has
 *   increased from low-moderate (0.35 in 1992, when NHS-funded care was more
 *   available) to high-moderate (0.58 in 2023, after sustained austerity and
 *   privatization). The theater ratio has risen proportionally, indicating
 *   increasing performative activity: carer support charities, government
 *   rhetoric about 'valuing carers,' and tokenistic Carer's Allowance
 *   (£70.15/week, below minimum wage equivalent) constitute performative
 *   recognition of a system that fundamentally extracts unpaid labor. The
 *   constraint operates through suppression: lack of affordable care
 *   alternatives, moral obligation internalized through family kinship norms,
 *   and economic vulnerability of carers (particularly women) who cannot exit
 *   without abandoning care recipients to crisis. The suppression is
 *   particularly high (0.72) because carers face multiple barriers: financial
 *   dependence on earnings from the care recipient's pension or their own
 *   reduced work, social stigma around 'institutionalizing' relatives,
 *   inadequate respite care, and health deterioration from care work that
 *   removes them from labor markets. The system exhibits snare
 *   characteristics at all observable levels: the family carer is trapped,
 *   the care recipient is trapped, and the arrangement is sustained by
 *   institutional arbitrage rather than genuine coordination.
 *
 * KEY AGENTS:
 *   - Unpaid Family Carers: Primary victims (powerless/trapped) — bear extraction through lost wages, foregone careers, health deterioration, social isolation, and moral obligation with no exit
 *   - Care Recipients (Elderly, Disabled, Ill): Primary victims (powerless/trapped) — dependent on unpaid family labor due to insufficient public care provision; quality of life constrained by carer availability and burnout
 *   - NHS and Government Budget: Primary beneficiaries (institutional/arbitrage) — avoid £billions in care costs by externalizing care provision to unpaid labor; benefit from the extraction without directly enforcing it
 *   - Private Care Sector and Care Agencies: Secondary beneficiaries (organized/constrained) — benefit from suppressed labor costs in the surrounding ecosystem; can charge high prices for paid care because unpaid alternatives remain unavailable
 *   - Carer Support Organizations (Carers UK, Age UK): Ambiguous actors (moderate/constrained) — provide coordination and support but are structurally dependent on the unpaid care system continuing; benefit from perpetuating the constraint
 *   - Analytical Observer: Global structural analysis — reveals the UK's system as a particular instance of civilizational extraction of unpaid female labor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_unpaid_care_system, 0.58).
domain_priors:suppression_score(uk_unpaid_care_system, 0.72).
domain_priors:theater_ratio(uk_unpaid_care_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_unpaid_care_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_unpaid_care_system, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(uk_unpaid_care_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_unpaid_care_system, snare).
narrative_ontology:human_readable(uk_unpaid_care_system, "The UK's Reliance on Unpaid Carers for Social and Healthcare").
narrative_ontology:topic_domain(uk_unpaid_care_system, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, nhs_government_budget).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, social_care_providers).
narrative_ontology:constraint_victim(uk_unpaid_care_system, unpaid_family_carers).
narrative_ontology:constraint_victim(uk_unpaid_care_system, care_recipients_quality_of_life).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNPAID FAMILY CARER (SNARE) — Structurally trapped. Exit options are minimal: abandoning care triggers immediate crisis for the care recipient, social stigma, and potential legal liability. The carer bears extraction through lost wages (opportunity cost), foregone career development, health deterioration from stress and physical demands, and social isolation. No meaningful alternative coordination mechanism exists. The constraint extracts through moral obligation and lack of viable exit.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE RECIPIENTS WITH LIMITED AGENCY (SNARE) — Elderly, disabled, or seriously ill individuals are trapped: they cannot exit the system by accessing sufficient paid care (due to cost and limited availability). They are dependent on unpaid family labor, often constrained to suboptimal living conditions and isolated by carer burnout. The suppression operates through lack of resource alternatives and vulnerability.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: NHS AND GOVERNMENT BUDGET (ROPE) — Institutional beneficiary with arbitrage options. The unpaid care system enables them to avoid £billions in care costs. They experience the constraint as coordination: unpaid carers provide essential social infrastructure that allows the health system to function with lower direct expenditure. From this perspective, the system coordinates care delivery without direct state extraction of labor — the moral obligation on families is not framed as coercion but as legitimate social interdependence. Net beneficiary with stable arbitrage.
constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVATE CARE SECTOR AND CARE AGENCIES (ROPE) — Organized beneficiaries with constrained exit. The unpaid care system creates a boundary condition: the most dependent care (complex medical needs, 24/7 support) increasingly flows toward private markets or NHS-funded care packages, but routine personal care and emotional labor remain unpaid. Private providers benefit from the suppression of labor costs in the surrounding ecosystem — they can set higher prices for paid care because unpaid alternatives are not fully substitutable. They coordinate market segments rather than extract from carers directly.
constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CARER SUPPORT ORGANIZATIONS (TANGLED ROPE) — Moderate power with constrained exit. These organizations (Carers UK, Age UK, Macmillan) provide advocacy, support, and partial services that reduce carer isolation and provide some access to respite care. However, they are structurally dependent on the unpaid care system continuing: their funding and mission derive from the existence of millions of carers in crisis. They have genuine coordination function (support networks, information provision) but also benefit from perpetuating the constraint rather than replacing it with fully funded professional care. They coordinate support but do not fully extract from carers — instead, they channel resources to sustain the unpaid system.
constraint_indexing:constraint_classification(uk_unpaid_care_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE, GLOBAL PERSPECTIVE) — From a civilizational/global analysis, the UK's unpaid care system is a particularly severe form of a global structural constraint: high-income societies have systematized the extraction of unpaid female labor through the 'family care model.' The UK's system is not exceptional — it is the global norm for wealthy nations, revealing that the suppression operates at the level of gender, kinship structure, and economic organization. This observer sees the constraint as a civilizational snare: entire economies depend on the systematic extraction of unpaid work from women and marginalized groups, with alternatives (universal paid care) coded as economically impossible. The extractiveness and suppression are higher at this observational distance — the theater decreases (the structural mechanism becomes clearer), while the true extraction intensifies.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_unpaid_care_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_unpaid_care_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(uk_unpaid_care_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The unpaid carer bears significant extraction through opportunity cost (forgone wages, estimated at £13,000-17,000 annually for a full-time carer), health costs (stress, physical injury, elevated mortality risk), and social costs (isolation, reduced marriage/fertility prospects). The extraction is not as severe as a pure debt trap (which would reach 0.75+) because some carers maintain partial employment and some receive state support. However, the extraction has intensified over the 31-year interval as public care provision declined and austerity shifted costs to families. Suppression (0.72): Very high. Carers face systematic barriers to exit: (1) Lack of affordable alternatives — waiting lists for state-funded social care exceed 1 million people; privately funded care costs £15,000-50,000 annually; (2) Moral and kinship obligation — abandoning a parent or disabled child triggers social stigma and personal guilt; (3) Economic vulnerability — carers typically lose earnings, making them financially dependent on the care recipient's benefits or savings; (4) Inadequate respite care — statutory respite provision is sparse and insufficient; (5) Legal liability — some carers face pressure from family members or implicit threat of elder abuse prosecution if they 'fail' to provide care. Theater ratio (0.65): Moderate-high. The system maintains legitimacy through performative recognition: Carer's Allowance is paid (at below-minimum-wage equivalent of £0.75/hour), carer support organizations are funded, and government rhetoric emphasizes 'valuing carers' while cutting care budgets. This theater substitutes for actual structural change — the focus shifts from care system redesign to 'supporting' carers within an extractive system. The theater has increased over time as the extraction has become more visible and politically sensitive; performative gestures have intensified to manage public dissent.
 *
 * PERSPECTIVAL GAP:
 *   The family carer sees a snare — trapped, extracting, with no exit. The care recipient sees a snare — vulnerable and dependent. The NHS sees rope — legitimate coordination of care that enables budgetary sustainability. The private sector sees rope — stable market conditions with high pricing power. Carer support organizations see tangled rope — they coordinate genuine support but also perpetuate the system. The global observer sees a civilizational snare — the entire high-income welfare model depends on extracting unpaid female labor, and the 'family care model' is an ideological cover for this extraction. The perspectival gap reflects fundamental conflict of interest: those who benefit (government, NHS, private providers) do not experience or acknowledge extraction; those who bear it (carers, care recipients) lack power to reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for unpaid carers (d ≈ 0.90): Powerless agents with trapped exit experience maximum extraction. The derived d is high because they are victims with no meaningful alternatives. Directionality for care recipients (d ≈ 0.85): Powerless with trapped exit, but slightly lower than carers because some receive partial state support and NHS services (reducing pure extraction to them). Directionality for NHS/government (d ≈ 0.10): Institutional beneficiaries with arbitrage options. They derive enormous benefits (£billions in avoided care costs) while maintaining plausible deniability about extraction (the moral obligation on families is not state-imposed). Directionality for private care sector (d ≈ 0.25): Organized beneficiaries with constrained exit — they benefit from the suppressed labor costs in the surrounding ecosystem but do not directly control carers. Directionality for carer support organizations (d ≈ 0.45): Moderate agents with constrained exit. They genuinely help carers but are incentivized to sustain rather than transform the system. The engine will derive these d values from the beneficiary/victim declarations and exit options; the high-extraction perspectives will classify as snare, while the beneficiary perspectives will classify as rope or scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as snare at the carer and care recipient levels, and the analysis confirms this is not a misclassification of coordination as extraction. The snare classification is robust to mandatrophy testing: (1) No coordination benefit accrues to carers — they receive no economic return, no career development, and no social capital that would justify the constraint as a coordination mechanism. (2) Suppression is high (0.72) and active — lack of alternatives is not accidental but structural and maintained through policy choices (privatization of care, austerity, immigration restrictions on care workers). (3) The constraint persists not through genuine consent but through internalized obligation and lack of exit. (4) The beneficiary perspectives (NHS, government, private sector) genuinely experience coordination — they see the system as solving a legitimate care provision problem. This is not a miscommunication but a fundamental conflict: the constraint is snare for victims and rope for beneficiaries simultaneously. This asymmetry is the defining feature of snares that avoid mandatrophy through hidden extraction. The system also exhibits theater elements (0.65) that sustain legitimacy: Carer's Allowance, carer support charities, and government rhetoric about 'valuing carers' all function as performative recognition that substitutes for structural change. The theater has increased over time as the extraction has become more visible, indicating Goodhart drift — the focus has shifted from care system adequacy to 'supporting' carers within an extractive system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_care_models_cost,
    'What is the true cost of replacing unpaid family care with professionally funded social care across the UK, and is it genuinely unaffordable or merely politically undesirable?',
    'Comparative cost analysis with Nordic countries (Denmark, Sweden) that have implemented universal social care; actuarial modeling of full replacement scenarios; analysis of national wealth and tax revenue capacity',
    'If truly unaffordable within UK wealth: constraint becomes a legitimate economic mountain (temporary scarcity). If politically undesirable but affordable: constraint remains a snare (extraction disguised as necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_care_models_cost, empirical, 'Whether universal paid care is economically feasible or merely politically rejected').

omega_variable(
    carer_agency_and_choice,
    'To what extent do unpaid carers actively choose informal caregiving as their preferred arrangement, versus being trapped by lack of alternatives?',
    'Longitudinal surveys of carer satisfaction and counterfactual preferences; qualitative interviews exploring exit barriers; policy experiments with universal child and elder care provision in specific local authorities',
    'If genuine preference: constraint becomes more rope-like (coordination with consent). If result of constrained choice: constraint remains snare (extraction through absence of alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carer_agency_and_choice, empirical, 'Whether unpaid care is chosen or coerced by lack of alternatives').

omega_variable(
    gender_dynamics_in_care_assignment,
    'Are the gendered patterns in unpaid care (women bearing 60% of heavy care burden) a reflection of genuine role preference or structural coercion through earnings inequality and social expectations?',
    'Analysis of care distribution changes when spousal earnings are equal; cross-cultural comparison of gender ratios in societies with different labor market structures; longitudinal tracking of career penalty asymmetry',
    'If preference: gender-neutral snare (same extraction for all). If coercion: reveals higher extraction from women through intersecting constraints (wage gap + care obligation). Classification remains snare but mandatrophy worsens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_dynamics_in_care_assignment, conceptual, 'Whether gendered care burden reflects preference or structural coercion').

omega_variable(
    national_identity_and_family_obligation,
    'Does the UK''s cultural emphasis on family responsibility for care (versus state responsibility) constitute a genuine value coherent with demographics, or is it an ideological cover for extraction avoidance?',
    'Historical analysis of how family care norms changed with welfare state expansion and retrenchment; cross-national comparison of cultural values and care system design; analysis of public opinion shifts when alternative models are made visible',
    'If genuine cultural value: constraint is partially self-sustaining through belief (piton or scaffold elements). If ideological cover: constraint is pure extraction rationalized through naturalized norms (pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_identity_and_family_obligation, conceptual, 'Whether family care norms reflect genuine values or ideological extraction cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_unpaid_care_system, 1992, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(upc_tr_t1992, uk_unpaid_care_system, theater_ratio, 1992, 0.4).
narrative_ontology:measurement(upc_tr_t2005, uk_unpaid_care_system, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(upc_tr_t2023, uk_unpaid_care_system, theater_ratio, 2023, 0.65).

% Extraction over time
narrative_ontology:measurement(upc_be_t1992, uk_unpaid_care_system, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(upc_be_t2005, uk_unpaid_care_system, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(upc_be_t2023, uk_unpaid_care_system, base_extractiveness, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_unpaid_care_system, resource_allocation).
narrative_ontology:affects_constraint(uk_unpaid_care_system, nhs_social_care_funding_gap).
narrative_ontology:affects_constraint(uk_unpaid_care_system, uk_female_labor_market_exit).
narrative_ontology:affects_constraint(uk_unpaid_care_system, care_home_quality_crisis).

% DUAL FORMULATION NOTE:
% The unpaid care system decomposes into three interdependent constraints: (1) The direct extraction of unpaid labor from family carers (uk_unpaid_care_system, ε=0.58, snare). (2) The systemic underfunding of NHS and local authority social care (nhs_social_care_funding_gap, ε≈0.52, tangled rope between state and care recipients). (3) The gendered labor market exit of women carers (uk_female_labor_market_exit, ε≈0.55, snare in labor market dynamics). These three constraints are mutually reinforcing: underfunding increases family care burden, family care burden traps women in unpaid work and out of labor markets, and labor market exit reduces women's bargaining power in family care negotiations. Each story has distinct ε and beneficiary/victim dynamics, but they form a constraint family linked by structural causation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_unpaid_care_system, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
