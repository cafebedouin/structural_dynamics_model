% ============================================================================
% CONSTRAINT STORY: institutional_selection_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_selection_bias, []).

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
 *   constraint_id: institutional_selection_bias
 *   human_readable: Institutional Selection Bias
 *   domain: institutional_governance/organizational_dynamics
 *
 * SUMMARY:
 *   Institutional selection bias operates at the intersection of coordination
 *   and extraction. Institutions require mechanisms to select among
 *   candidates for positions, roles, and participation — a genuine
 *   coordination problem. But these mechanisms are designed, enforced, and
 *   interpreted by incumbents who benefit from homogeneity and have
 *   incentives to frame their in-group preference as neutral meritocracy. The
 *   constraint exhibits all six DR types depending on the observer's
 *   structural position. Structurally excluded populations experience pure
 *   extraction (snare) with no exit mechanism. Reform coalitions organize
 *   around the asymmetry and benefit from institutional accountability crises
 *   (tangled rope). Incumbent leadership experiences the bias as legitimate
 *   coordination and role definition (rope). Regulatory mandates create
 *   temporary pressure for change with explicit sunset logic (scaffold).
 *   Formal selection procedures appear objective but function primarily as
 *   theater masking informal network decisions (piton). Analytical observers
 *   risk naturalizing selection bias as an inevitable feature of cognition
 *   (false mountain). The constraint's extractiveness has increased over the
 *   interval as institutional complexity has grown — more sophisticated
 *   selection machinery produces more sophisticated screening while
 *   maintaining plausible deniability about systemic exclusion.
 *
 * KEY AGENTS:
 *   - Structurally Excluded Populations: Primary victims (powerless/trapped) — lack social capital, educational pathways, or demographic markers that institutions privilege; no exit mechanism within institutional context
 *   - Incumbent Leadership: Primary beneficiary (institutional/arbitrage) — captures career security, cultural continuity, and reduced accountability through in-group preference
 *   - Reform Coalitions: Secondary actors (organized/constrained) — organized groups pushing for transparency and diversification; benefit from dysfunction exposure while bearing institutional resistance costs
 *   - Regulatory Bodies: Organized actors (organized/constrained) — mandate diversity reporting and anti-discrimination audits; create temporary enforcement pressure with decay cycles
 *   - Selection Committee Members: Individual institutional actors (moderate/constrained) — experience selection procedures as legitimate coordination while being constrained by informal norms and group dynamics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_selection_bias, 0.58).
domain_priors:suppression_score(institutional_selection_bias, 0.65).
domain_priors:theater_ratio(institutional_selection_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_selection_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_selection_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_selection_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_selection_bias, tangled_rope).
narrative_ontology:human_readable(institutional_selection_bias, "Institutional Selection Bias").
narrative_ontology:topic_domain(institutional_selection_bias, "institutional_governance/organizational_dynamics").

domain_priors:requires_active_enforcement(institutional_selection_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_selection_bias, incumbent_leadership).
narrative_ontology:constraint_beneficiary(institutional_selection_bias, institutional_continuity).
narrative_ontology:constraint_victim(institutional_selection_bias, excluded_populations).
narrative_ontology:constraint_victim(institutional_selection_bias, institutional_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY EXCLUDED (SNARE) — Populations excluded by selection bias have no mechanism for institutional redress. Selection criteria are framed as neutral (meritocratic, objective, evidence-based) but systematically filter out agents who lack the social capital, educational pathway, or demographic markers that institutions privilege. Exit is impossible within the institutional context; alternatives are typically resource-constrained. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_selection_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITIONS (TANGLED ROPE) — Organized groups pushing for transparency, diversification, and selection bias audits benefit from coordination around shared goals (more equitable access) while bearing costs of institutional resistance. They also benefit from institutional dysfunction being exposed (career opportunities in diversity consulting, media attention, funding for research). High suppression via institutional closure, reputational risk, but they have organizational capacity and partial exit options (changing organizations, shifting focus).
constraint_indexing:constraint_classification(institutional_selection_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: INCUMBENT LEADERSHIP (ROPE) — Benefits from selection bias through in-group preference, cultural continuity, and reduced accountability pressure. Experiences selection mechanisms as legitimate coordination: defining who 'belongs' in the institution, maintaining institutional identity, screening for 'fit'. Low or negative experienced extraction — the constraint subsidizes this agent's position.
constraint_indexing:constraint_classification(institutional_selection_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY MANDATES (SCAFFOLD) — Legal requirements for diversity reporting, blind resume review, and anti-discrimination audits create temporary coordination mechanisms with explicit sunset logic. Institutions adopt these mechanisms under compliance pressure but often without genuine commitment. If enforcement wanes, compliance rates drop sharply. The sunset is endogenous to regulatory attention cycles.
constraint_indexing:constraint_classification(institutional_selection_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL SELECTION PROCEDURES (PITON) — Documented recruitment and promotion protocols (job descriptions, interview panels, evaluation rubrics) are substantially performative. The theater of 'objective selection' masks how informal networks, cultural fit assessments, and implicit bias actually determine outcomes. Procedures persist through institutional inertia despite low predictive validity for institutional success. Theater ratio: 0.68 — a substantial fraction of the selection apparatus is ritual maintenance rather than actual decision-making.
constraint_indexing:constraint_classification(institutional_selection_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN FALSE SUMMIT) — A naive analytical view might naturalize selection bias as an inevitable feature of human cognition: 'preference for similarity,' 'homophily,' 'in-group bias' are described as hardwired evolutionary adaptations. From this perspective, selection bias appears immutable and universal. However, the structural data reveals this as naturalization of a contingent institutional choice: institutions design selection mechanisms and can redesign them. The mountain classification is a false summit.
constraint_indexing:constraint_classification(institutional_selection_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_selection_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_selection_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_selection_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_selection_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_selection_bias, TR),
    TR >= 0.70.

:- end_tests(institutional_selection_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional selection bias extracts significant resources from excluded populations in the form of career denial, opportunity cost, and accumulated wealth/credential gaps. But the extraction is not total — some excluded individuals do access institutions through alternative pathways (connections, elite performance signaling, institutional diversity initiatives). The value reflects that the mechanism is strong but not absolute. Suppression (0.65): High. Multiple barriers prevent exit: excluded populations cannot easily access alternative institutions if dominant pathways are closed; formal appeals procedures are typically weak and captured by incumbent interests; social isolation from networks that enable alternative pathways; reputational risk of challenging selection outcomes. Theater ratio (0.68): High and increasing. The formalization of selection procedures (rubrics, documented criteria, interview protocols) serves primarily to legitimize decisions that were actually made through informal networks and cultural fit assessments. As institutions come under scrutiny, they invest more in selection theater — blind resume screening, structured interviews, diversity committees — often without changing actual decision mechanisms. The theater has increased over the interval as criticism of selection bias has increased institutional defensive behavior.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent leadership's rope perspective and the excluded population's snare perspective are irreconcilable observations of the same structural mechanism. The leadership genuinely experiences selection as coordination — they are solving the problem of role definition and institutional continuity. The excluded population genuinely experiences extraction — they are locked out of opportunity through no fault of their performance. The gap is not epistemic disagreement but structural — different agents have different exit options and different relationships to the extraction flow. The reform coalition's tangled rope perspective is strategic: they identify both the coordination function (institutions do need selection) and the asymmetric extraction (but selection could be redesigned to reduce bias), creating leverage for negotiation. The regulatory mandate perspective adds sunset logic: constraints can be reformed through external pressure, but only while that pressure persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents with arbitrage options derive low or negative directionality (d ≈ 0.15): they benefit from the constraint and can exercise exit by moving to alternative institutions where selection bias favors them. Excluded populations with trapped exit (d ≈ 0.95) experience maximum extraction and cannot exercise alternative pathways. Reform coalitions with constrained exit but partial agency (d ≈ 0.60) experience moderate extraction but also benefit from dysfunction exposure. Regulatory bodies with enforcement power but time-limited attention (d ≈ 0.45) experience the constraint as a problem they can partially solve. The sigmoid function maps these directionalities to experienced extractiveness, producing the perspectival gap: beneficiaries see low chi (coordination), victims see high chi (extraction), and analytical observers risk missing the structural asymmetry entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the risk of conflating 'selection bias is inevitable' (a claim about human cognition) with 'this specific institutional mechanism is necessary' (a claim about institutional design). The false mountain perspective naturalizes what is actually a contingent institutional choice. The resolution is to distinguish: (1) Some degree of preference-for-similarity in human cognition may be difficult to eliminate (a true constraint on individual psychology); (2) Institutional design can drastically reduce this bias's impact through mechanism changes (blind evaluation, diverse committees, transparent criteria). The constraint's true structure is tangled rope: genuine coordination function (selection must happen) overlaid with asymmetric extraction (mechanism is designed to benefit incumbents). The mandatrophy is resolved by recognizing that the coordination problem could be solved through multiple mechanisms, and the choice to use bias-amplifying mechanisms is a design decision, not an inevitable outcome of human nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_criteria_ostensible_vs_actual,
    'What is the causal relationship between stated selection criteria and actual institutional outcomes? Are stated criteria determining decisions, or are they post-hoc rationalization for decisions made through informal networks?',
    'Audit studies using matched resumes with demographic variation; correlation analysis of stated criteria vs. actual hiring/promotion decisions; exit interviews with rejected candidates and interview training analysis',
    'If stated criteria drive decisions: selection bias is a calibration problem (criteria need revision). If stated criteria are post-hoc rationalization: selection bias is an enforcement problem (informal networks override formal rules), and the constraint has higher effective suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selection_criteria_ostensible_vs_actual, empirical, 'Causal relationship between stated and actual selection criteria').

omega_variable(
    exclusion_mechanism_systemic_vs_individual,
    'Is the observed pattern (exclusion of specific populations) produced by systemic institutional design, accumulated individual biases within formal procedures, or identity-locked cultural norms that defenders perceive as natural ''fit''?',
    'Decomposition of bias sources: compare outcomes from blind evaluation (protocol compliance tests) vs. non-blind evaluation; analyze selection committee composition and implicit association test data; track how ''fit'' language appears in decision documentation',
    'If systemic: the constraint is designed exclusion (higher malice, higher intentional suppression). If individual: bias remediation through training should reduce bias (lower structural suppression). If identity-locked cultural norms: incumbent leadership cannot perceive bias without identity frame shift (suppression is partly internalized for beneficiaries, fully structural for victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_systemic_vs_individual, empirical, 'Source of exclusion mechanism: systemic design, individual bias, or cultural norms').

omega_variable(
    diversity_benefits_realization,
    'Do institutions that diversify their selection actually realize the purported coordination benefits (better decision-making, broader perspective, reduced groupthink), or do they encounter enforcement barriers that prevent diverse cohorts from exercising their perspectives?',
    'Longitudinal studies of organizations that successfully diversify vs. those that hire diverse candidates but experience high attrition/marginalization; measurement of decision quality and perspective diversity before/after selection changes; exit rates and retention satisfaction by demographic group',
    'If benefits realized: diversity is coordination (supports tangled_rope and scaffold classifications). If benefits blocked by enforcement barriers: diversity initiatives fail to change the actual coordination mechanism, and the constraint remains a snare for excluded populations even if selection criteria formally change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefits_realization, empirical, 'Whether institutional diversification realizes coordination benefits').

omega_variable(
    regulatory_enforcement_decay,
    'Do institutional compliance rates with diversity and anti-bias mandates persist after enforcement pressure relaxes, or do they revert to baseline selection patterns?',
    'Time-series analysis of hiring/promotion diversity before, during, and after regulatory scrutiny cycles; comparison of compliance rates between high-enforcement and low-enforcement jurisdictions; measurement of institutional investment in selection infrastructure (training, blind review systems, audit capacity)',
    'If compliance persists: selection bias constraint has internalized enforcement mechanisms (institutions have adopted new norms). If compliance reverts: scaffold perspective is accurate — regulatory mandates created temporary constraint, and sunset is endogenous to enforcement attention cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_decay, empirical, 'Whether regulatory enforcement effects persist after oversight relaxes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_selection_bias, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insb_tr_t0, institutional_selection_bias, theater_ratio, 0, 0.55).
narrative_ontology:measurement(insb_tr_t3, institutional_selection_bias, theater_ratio, 3, 0.62).
narrative_ontology:measurement(insb_tr_t6, institutional_selection_bias, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(insb_be_t0, institutional_selection_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(insb_be_t3, institutional_selection_bias, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(insb_be_t6, institutional_selection_bias, base_extractiveness, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_selection_bias, identity_coordination).
narrative_ontology:affects_constraint(institutional_selection_bias, glass_ceiling_dynamics).
narrative_ontology:affects_constraint(institutional_selection_bias, cultural_fit_gatekeeping).
narrative_ontology:affects_constraint(institutional_selection_bias, credentialing_system_bias).

% DUAL FORMULATION NOTE:
% Institutional selection bias is the overarching constraint. It affects three downstream constraints: glass ceiling dynamics (how selection bias compounds over career trajectories), cultural fit gatekeeping (how informal norms reinforce selection criteria), and credentialing system bias (how upstream educational barriers interact with institutional selection). Each downstream constraint has its own ε value reflecting specific mechanisms. Selection bias itself (ε=0.58) represents the institutional-level coordination-with-extraction hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_selection_bias, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
