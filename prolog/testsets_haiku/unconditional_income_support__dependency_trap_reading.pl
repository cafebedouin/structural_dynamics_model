% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency-Trap Subsidy
 *   domain: political_economy/welfare_state
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency-trap reading of
 *   unconditional income support (UBI): a framing in which UBI appears as a
 *   universal, dignified transfer but functions as a snare that extracts from
 *   the working poor (via elimination of targeted programs worth more than
 *   the flat payment) and transfers upward to middle/upper classes who
 *   receive the payment without offsetting loss. The reading contest is
 *   structured around fundamentally different causal stories and
 *   beneficiary/victim maps. The dependency-trap reading emphasizes
 *   employment disincentives (−1 to −3.2% labor supply reduction in pilots),
 *   crowd-out of targeted programs, and regressive redistribution despite the
 *   universal framing. The freedom-floor reading emphasizes labor-market
 *   autonomy and dignified access. The universality-paradox reading notes the
 *   policy's cross-ideological appeal masks incompatible implementation paths
 *   (means-tested UBI, negative income tax, versus true unconditional cash)
 *   that converge on fiscal outcomes but diverge on moral framing. This JSON
 *   instantiates ONLY the dependency-trap reading as a clean, ε-invariant
 *   constraint; the other readings are sibling constraints, not alternatives
 *   within this one.
 *
 * KEY AGENTS:
 *   - working_poor_targeted_program_dependents: Victims. Currently receive means-tested programs calibrated to need (housing, food, childcare); face catastrophic loss if replaced by flat UBI below their current benefit threshold.
 *   - middle_upper_class_non_needy: Beneficiaries. Receive the UBI without offsetting loss of other benefits; have rich exit options and can arbitrage the transfer.
 *   - ubi_advocates_ideological: Beneficiary/agenda-setter. Gain political capital and ideological vindication from the policy; set the frame as 'universal' and 'dignified,' occluding the regressive redistribution within it.
 *   - taxpayers_broad_base: Payers. Bear the $1.4 trillion net annual cost; organized but diffuse and politically constrained.
 *   - labor_market_participants_marginal: Payers (via employment reduction). Show 1–3.2% labor-force exit in pilots; lose income and career continuity despite the framing of autonomy.
 *   - competing_welfare_reform_advocates: Excluded. Job-guarantee, expanded-healthcare, and targeted-subsidy proponents are structurally excluded from the policy frame because the universality frame dismisses them as paternalistic.
 *   - affected_working_poor_political_voice: Excluded. The poorest have weak voice in the debate despite being most affected; debate is dominated by upper-class advocates and academic economists.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.71).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency-Trap Subsidy").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/welfare_state").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '4d19c521-73e6-443e-bc6a-938a5c09bc71').
narrative_ontology:cs_kernel_codification('4d19c521-73e6-443e-bc6a-938a5c09bc71', distributed).
narrative_ontology:cs_authority_grounding('4d19c521-73e6-443e-bc6a-938a5c09bc71', extraction).
narrative_ontology:cs_reading_relation('4d19c521-73e6-443e-bc6a-938a5c09bc71', unconditional_income_support__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('4d19c521-73e6-443e-bc6a-938a5c09bc71', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('4d19c521-73e6-443e-bc6a-938a5c09bc71', foundational, universality_as_inherent_harm_to_poor).
narrative_ontology:cs_axiom_status(universality_as_inherent_harm_to_poor, holdable).
narrative_ontology:cs_axiom_grounding('4d19c521-73e6-443e-bc6a-938a5c09bc71', universality_as_inherent_harm_to_poor, empirically_contingent).
narrative_ontology:cs_axiom('4d19c521-73e6-443e-bc6a-938a5c09bc71', foundational, unconditionality_enables_labor_extraction).
narrative_ontology:cs_axiom_status(unconditionality_enables_labor_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4d19c521-73e6-443e-bc6a-938a5c09bc71', unconditionality_enables_labor_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('4d19c521-73e6-443e-bc6a-938a5c09bc71', universal_welfare_dignity).
narrative_ontology:cs_drift_state('4d19c521-73e6-443e-bc6a-938a5c09bc71', contemporary_employment_effect_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d19c521-73e6-443e-bc6a-938a5c09bc71', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_non_needy).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates_ideological).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_targeted_program_dependents).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers_broad_base).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, program_administrators_and_caseworkers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, policy_economists_and_researchers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, labor_market_participants_marginal).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, program_administrators_and_caseworkers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently depend on means-tested programs (housing vouchers, food assistance, childcare subsidies) calibrated to their specific needs and income level, worth $8,000–$15,000 annually. Face elimination or substantial reduction of these programs if UBI replaces them. A flat UBI of $10,000–$12,000 does not account for family size, rent burden, disability, or other needs targeted programs address. Cannot exit the labor market entirely without losing all income. Their exit options are constrained to: survive on inadequate flat payment, continue working at low wages, or face destitution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_targeted_program_dependents, payer,
    powerless, biographical, trapped, national).

% Receive the same UBI payment as all residents ($10,000–$12,000 annually) despite having no need. Household income from other sources (wages, capital, investments) is sufficient to cover all needs and discretionary spending. The UBI represents a pure transfer—additional income without offsetting benefit loss or obligation. Can use the transfer to save, invest, fund education, or consume. Their exit options are rich: they can work or not, relocate, take risk on entrepreneurship, or pursue non-market activities without any risk of deprivation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_non_needy, beneficiary,
    powerful, generational, arbitrage, national).

% Set and advance the UBI policy agenda through research institutions, policy organizations, political advocacy, and media. Gain professional reputation, funding, and ideological vindication from UBI's adoption and expansion. Frame UBI as universal, dignified, and the solution to welfare inefficiency and labor-market coercion. Control the policy narrative by defining universality as intrinsically good, marginalizing competing frames (job guarantee, expanded childcare, targeted subsidies) as paternalistic or insufficient. Their exit from the agenda-setting role is costless—if UBI fails, they can reframe the failure as under-funding or external factors; if it succeeds, they claim credit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates_ideological, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, ubi_advocates_ideological, agenda_setter).

% Fund the UBI through tax increases or budget reallocation. The net cost (after elimination of some programs and behavioral adjustments) is estimated at $1.4 trillion annually. Are broadly organized but politically diffuse—no individual taxpayer can see their specific contribution or its effect. Have constrained exit: tax avoidance is illegal and costly; relocation is expensive; political organizing to reverse the policy is slow and uncertain. Their interest in cost control is real but politically weak relative to the concentrated interests of beneficiaries and advocates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers_broad_base, payer,
    organized, biographical, constrained, national).

% Are at the margin of labor-force participation: students, second earners, workers nearing retirement, or gig workers. UBI provides an income floor that makes exiting the labor market more feasible. Pilot studies show 1.0–3.2% reduction in employment in this group. They gain leisure and reduced labor-market stress but lose income, career continuity, and job-specific skills. Many would prefer access to targeted job training, childcare, or subsidized healthcare that would keep them in the labor force at higher productivity and income.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_participants_marginal, payer,
    moderate, biographical, constrained, national).

% Administer means-tested welfare programs: income verification, eligibility determination, casework services, fraud detection. Under UBI, many administrative roles are eliminated because the program is unconditional—no eligibility checking, no case work, no means testing. Some administrators are repositioned into UBI distribution systems (simpler work); others lose employment. Their situation is mixed: those who transition to UBI administration benefit; those who lose jobs without transition are harmed. Overall, the constraint reduces casework and professional social-work opportunities.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, program_administrators_and_caseworkers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, program_administrators_and_caseworkers, payer).

% Gain research funding, publication opportunities, professional visibility, and career advancement from studying UBI effects. Some researchers have direct organizational or financial ties to UBI advocates. Career incentives are strongly aligned with UBI's continued salience in the policy domain. Have mobile exit options (other research topics, other sectors) but benefit from UBI's prominence in academic and policy funding. Their interests in UBI's expansion are real, though some researchers are motivated primarily by the scientific interest in the policy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, policy_economists_and_researchers, beneficiary,
    organized, biographical, mobile, national).

% Advocate for alternative reforms: targeted job guarantees, expanded childcare and healthcare, wage subsidies, expanded earned-income tax credit, or negative income taxes. These alternatives are structurally excluded from the UBI policy frame because the universality narrative dominates—alternatives are dismissed as insufficiently universal or paternalistic. Would argue their approaches are more cost-effective and better tailored to working-poor needs; their voices are systematically marginalized in the policy debate dominated by UBI universalists.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, competing_welfare_reform_advocates, excluded,
    organized, biographical, constrained, national).

% The working poor themselves have weak political voice in the UBI debate. The policy conversation is dominated by upper-class advocates (philosophers, economists, policy entrepreneurs) and academic researchers. Working-poor perspectives on what specific supports they actually need—targeted programs, job access, childcare, healthcare—are rarely represented in high-level policy debates. Their exclusion from the conversation is structural: they lack the education, time, organizational resources, or media access to participate in think-tank discussions, academic conferences, or policy advocacy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_political_representation, excluded,
    powerless, biographical, trapped, national).

% Conduct empirical research on employment effects of UBI in pilots and natural experiments. Their findings vary widely (−0.5% to −3.2% depending on study design, sample, and definitions). Report their findings transparently; the policy persists regardless of evidence because the policy frame is ideological (universality is intrinsically dignified) rather than empirically contingent on employment outcomes. Serve an analytical function but have limited power to constrain the policy trajectory.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_supply_researchers, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_class_non_needy).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies welfare administration by replacing multiple means-tested programs with a single, unconditional transfer to all residents. Eliminates need for income verification, eligibility determination, casework services, and fraud investigation. Reduces administrative overhead and the stigma associated with welfare receipt by providing the same payment to all.
% TRANSFER_FUNCTION: Moves $1.4 trillion annually from broad-based taxpayers to all residents regardless of need. The distribution appears universal (everyone gets the same payment) but is functionally regressive in the slice most affected: working poor lose targeted programs worth more than the flat UBI they receive; middle and upper classes receive the flat UBI without any offsetting loss of benefits.
% ABSENT_VOICES: The working poor themselves are absent from the policy debate, which is conducted by upper-class UBI advocates, academic economists, and think-tank policy entrepreneurs. Their actual needs—specific supports for housing, food, childcare, healthcare, disabilities—are not represented because the debate frame dismisses targeting as paternalistic. Competing welfare-reform advocates (job-guarantee, expanded-childcare, wage-subsidy proponents) are structurally excluded from the universality frame.
% DISAPPEARANCE_RATIONALE: If UBI were eliminated overnight: targeted means-tested programs would need to be reinstated (or the working poor would face sharp benefit loss); middle/upper classes would lose the universal transfer; taxpayers would recover $1.4 trillion in net annual tax burden; labor-force participation would likely rebound by 1–3% as the employment-reduction effect reversed; welfare administration would revert to means-tested verification and casework systems. The political economy of welfare would reorganize around targeted-versus-universal frames, with competing reforms (job guarantee, expanded healthcare, negative income tax) re-entering serious consideration.
% FOUNDING_PROBLEM: Welfare bureaucracy is inefficient (high administrative cost relative to benefits delivered), stigmatizing (discourages receipt), and creates perverse incentives (welfare cliffs that trap recipients in poverty); labor-market participation is coercive (those without independent means must work even when non-market activities—education, caregiving, rest—would be socially valuable); poverty persists despite substantial transfer programs because targeting is imperfect, administrative barriers are high, and the programs do not address structural inequality.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates attest the founding problem is live and that unconditional universality is the solution. Welfare economists and working-poor advocates contest the diagnosis: they attest that targeted programs, despite administrative friction, serve actual needs better than flat cash; they argue that employment reduction shown in pilots is harm, not freedom; they argue that universality to the non-needy is inefficient and regressive. Labor economists (AEI, IZA meta-analyses) document employment reduction of 1–3.2% in pilots, contradicting the freedom narrative. No credible source outside the UBI advocacy community attests that unconditional universality is the optimal solution to the founding problem; many contest it.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and rising (0.65→0.78 over the interval) because the policy is fundamentally asymmetric: it appears universal but delivers regressive redistribution. The working poor face a clear extraction: loss of targeted programs worth $8k–$15k annually, replaced by $10k–$12k flat UBI. The gap is not compensated by universality—universality benefits those with surplus income (middle/upper classes) who can arbitrage the transfer. Suppression is high (0.71) because the policy persists despite evidence of employment harm (−1 to −3.2% labor supply reduction) and distributional harm to the poorest. The suppression mechanism is not overt coercion but rather frame control: the universality narrative dominates policy discourse, and competing voices (targeted-reform advocates, working-poor political representation) are structurally excluded. Theater is moderate-to-high (0.42): the policy is marketed as dignified and universal (the theatrical framing), but actual operation is targeted extraction (the functional reality). The upward drift in theater_ratio (0.25→0.42) reflects growing disjunction between the stated justification (dignity, simplicity, labor autonomy) and observed effects (regressive redistribution, employment reduction, working-poor harm). Accessibility collapse is moderate (0.63): alternatives (targeted programs, job guarantees, negative income taxes) remain conceptually available but are politically foreclosed by the universality frame. Resistance is substantial (0.59): labor economists publish critical meta-analyses, welfare advocates contest the policy, and working-poor communities experience the harm directly—but resistance is institutionally weak (poor political voice, researchers with career incentives aligned with UBI) and the policy advances anyway.
 *
 * PERSPECTIVAL GAP:
 *   The seated gaps between agenda-setter/beneficiary and payer/victim should be stark. From the UBI advocate's seat, the constraint is emancipatory—it simplifies bureaucracy, eliminates stigma, and honors dignity through universality. From the working-poor seat, the constraint is catastrophic—it eliminates programs they depend on and offers less in return. From the taxpayer seat, the constraint is a regressive transfer ($1.4 trillion annually from broad base to all residents, disproportionately benefiting the non-needy). The engine should compute snare from the working-poor and taxpayer seats (high extraction), while the agenda-setter seat might compute rope or even mountain (coordination framing) if evaluated in isolation from the distributional harm. This seat divergence is structural, not an error: the constraint IS experienced differently by different seats because the universality framing obscures the distributional asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Working poor are high-d (trapped, powerless, identity-locked to labor participation and welfare dependence): they face extraction through program loss and cannot exit. UBI advocates are low-d (powerful, institutional, arbitrage exit): they gain political capital and ideological vindication; if the policy fails, they reframe and move on. Middle/upper classes are very-low-d (beneficiaries without offsetting loss; powerful, arbitrage exit). Taxpayers are moderate-d (organized but diffuse; constrained exit via tax policy). Marginal labor-force participants are moderate-d (show employment reduction, lose income, but have some choice at the margin). No directionality override is needed—the structural derivation from beneficiary/victim + power + exit produces the right directionality ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare inefficiency, labor-market coercion, administrative burden) is CONTESTED in status because it is identified by the UBI advocates but denied or reframed by welfare economists and the working poor themselves. Working-poor dependents on targeted programs do not attest that the founding problem is 'live and unsolved'—they attest that targeted programs, despite bureaucratic friction, serve their needs better than a flat transfer would. The policy persists despite this contestation because: (1) the universality framing is ideologically appealing to a broad political coalition, (2) the regressive distributional effects are occluded by the universal framing, (3) beneficiaries (middle/upper class, UBI advocates) have concentrated political power, and (4) victims (working poor) have weak political voice. This is not mandatrophy in the classic sense (a mandate that has outlived its function); rather, it is a policy whose stated mandate (solve welfare inefficiency through universality) is contested, and whose actual function (regressive redistribution, labor-supply reduction) is neither acknowledged nor addressed by the policy frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_effect_interpretation,
    'Do employment reductions of 1–3.2% in UBI pilots represent voluntary exit (freedom to not work, autonomy gain) or labor-market extraction (inability to sustain prior income/career trajectory)?',
    'Qualitative follow-up studies of pilot participants: do those who exit employment report increased well-being and reduced coercion, or financial stress and lost career continuity? Post-pilot re-employment rates and wage trajectories for exit cohorts.',
    'If exit is voluntary and welfare-increasing, the constraint''s extractiveness is overstated and the freedom-floor reading gains ground. If exit is forced by inadequate UBI and leads to lost income/career, extractiveness is correctly measured and dependency-trap reading holds. The distinction hinges on whether the employment reduction reflects autonomy (freedom reading) or deprivation (snare reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_interpretation, empirical, 'Whether employment reduction is autonomy-driven or extraction-driven.').

omega_variable(
    targeted_program_replacement_scope,
    'What is the actual proportion of current UBI proposals that explicitly eliminate (versus supplement) targeted means-tested programs?',
    'Policy text review of major UBI proposals (current legislation, pilot designs, advocacy documents): explicit budget scoring showing program elimination, supplementation, or no specification.',
    'If most proposals supplement rather than replace targeted programs, the crowd-out extraction is avoided and the constraint''s victim group shrinks. If most eliminate or substantially reduce targeted programs, the working-poor extraction is structural to the policy frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_program_replacement_scope, empirical, 'Whether UBI is replacement or supplementation in actual policy designs.').

omega_variable(
    universality_as_cover_story,
    'Is the universality framing (UBI for all, regardless of need) the primary justification for the policy, or is it a secondary framing that obscures the actual targeting (implicit targeting toward those with weak labor-market position or political power to demand it)?',
    'Textual analysis of UBI advocacy: do advocates emphasize universality as intrinsically dignified (suggesting the framing is primary), or do they emphasize practical benefits (simplicity, avoiding welfare traps, autonomy) that happen to be universal in form? Political-economy analysis: which constituencies gain, and do the gains justify the universality framing?',
    'If universality is the primary justification (intrinsically dignified), the constraint''s actual regressive distributional effects constitute a false-summit candidate—a constraint that claims mountain (natural, universal, dignified) but operates as snare (extracts from working poor, transfers to non-needy). If universality is secondary (a practical frame for simplicity), the policy should be evaluated on its empirical effects, not on the dignity of the universal form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_as_cover_story, conceptual, 'Whether universality is intrinsic justification or instrumental cover story.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.71) structural (political exclusion of working-poor and competing advocates from the policy frame) or internalized (working poor and others accept the universality framing despite harm, believing dignity is intrinsically valuable)?',
    'Post-implementation survey of affected populations: do those who lose targeted programs view UBI as compensating them fairly (suggesting internalized acceptance of universality narrative) or as a net harm (suggesting structural suppression of their voices in design)? Political-history analysis: were working-poor and welfare advocates included in policy design, or excluded?',
    'If suppression is structural, the constraint persists through frame control (universality narrative, exclusion of dissenting voices). If internalized, the constraint persists through belief in the dignified universal form despite material harm. Both mechanisms support snare classification, but the remedies differ: structural suppression requires inclusion of excluded voices; internalized suppression requires post-exit perspective to break the frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural exclusion or internalized acceptance of universality narrative.').

omega_variable(
    kernel_reading_contest_structure,
    'The three readings (dependency-trap, freedom-floor, universality-paradox) are structured around fundamentally different causal models and normative commitments. Does the contest between them admit of empirical resolution, or do they remain incommensurable framings held by different ideological coalitions?',
    'Head-to-head policy experiment with three treatment arms: (1) UBI as unconditional cash (freedom-floor design), (2) UBI with explicit targeting to working poor via clawback (dependency-trap-aware design), (3) means-tested negative income tax (competing reform design). Measure employment, well-being, distributional outcomes, and political coalition stability across three years.',
    'If the three arms produce detectably different outcomes on material dimensions (employment, poverty reduction, cost per poor household served), the readings may be empirically distinguishable. If outcomes converge despite the framing differences, the readings are incommensurable—they reflect different normative commitments (dignity via universality, autonomy via unconditionality, efficiency via targeting) that cannot be settled by data. The constraint''s type and extractiveness may vary across readings not because of measurement ambiguity but because of genuine structural differences in how the policy is implemented.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the three kernel readings are empirically distinguishable or incommensurable framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(unco_tr_t16, observed).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(unco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.69).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.77).
narrative_ontology:measurement_basis(unco_be_t16, observed).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(unco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(unco_su_t0, observed).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement_basis(unco_su_t4, observed).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(unco_su_t8, observed).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(unco_su_t12, observed).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(unco_su_t16, observed).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(unco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, welfare_state_targeting_efficiency).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, labor_supply_incentive_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unconditional_income_support kernel. All three readings share the same policy object (cash transfers to all residents) but diverge on extractiveness, beneficiary/victim structure, and causal mechanisms. The dependency-trap reading (this constraint) emphasizes employment disincentives and regressive redistribution. The freedom-floor reading emphasizes autonomy and dignity. The universality-paradox reading notes the policy's cross-ideological appeal masks incompatible implementation paths. The three readings are structurally related through the kernel: each is a plausible interpretation of the same stabilized political commitment, but they generate different constraints with different types and extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
