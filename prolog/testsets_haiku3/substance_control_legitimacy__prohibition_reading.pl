% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Substance Prohibition via Criminalization (Harm Prevention Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading instantiates one normative framework for state
 *   authority over substance use: substances are inherently harmful; moral
 *   duty requires prevention through criminalization and abstinence mandates.
 *   This reading positions substance users as targets requiring coercive
 *   intervention, law enforcement as moral agents, and criminalization as the
 *   legitimate expression of public health authority. The reading entered the
 *   kernel contest as the default policy in most jurisdictions (1920-2010
 *   consensus) but is now empirically contested by harm-reduction and
 *   legalization readings instantiated as alternative constraint stories.
 *   This story authors the prohibition reading's structural claims about who
 *   benefits (enforcement apparatus, carceral system, black market
 *   operators), who bears costs (users, marginalized communities, families),
 *   and how the arrangement persists (active enforcement, identity-lock,
 *   theater masking extraction as harm prevention). The constraint's measured
 *   extractiveness (0.81) and theater (0.62) reflect the growing divergence
 *   between prohibition's stated harm-prevention function and its actual
 *   operation as carceral and wealth extraction. The founding problem (that
 *   criminalization is necessary) is assessed as empirically dead by public
 *   health consensus while remaining politically live for law-enforcement and
 *   political-gatekeeper beneficiaries.
 *
 * KEY AGENTS:
 *   - substance_users: targets of criminalization; identity-locked; powerless; bear carceral extraction and legal barriers
 *   - marginalized_communities: disproportionately policed; geographic/economic trap; generational wealth and health extraction
 *   - law_enforcement_apparatus: institutional beneficiary; funds, legitimacy, and gatekeeping power from enforcement; has arbitrage to alternative legitimacy models (terrorism, organized crime)
 *   - carceral_administration: institutional beneficiary; receives operational budgets from drug offense populations
 *   - political_gatekeepers: beneficiary; derives electoral constituencies and police-power expansion authority from prohibition rhetoric
 *   - harm_reduction_advocates: excluded; would argue for decriminalization but structurally barred from drug policy formation
 *   - public_health_authorities: observer; measure harm outcomes but lack authority over criminal justice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Prohibition via Criminalization (Harm Prevention Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '1b362d3a-80c7-47d6-834c-438db4c07816').
narrative_ontology:cs_kernel_codification('1b362d3a-80c7-47d6-834c-438db4c07816', formalized).
narrative_ontology:cs_authority_grounding('1b362d3a-80c7-47d6-834c-438db4c07816', extraction).
narrative_ontology:cs_interpretation_layer_present('1b362d3a-80c7-47d6-834c-438db4c07816').
narrative_ontology:cs_reading_relation('1b362d3a-80c7-47d6-834c-438db4c07816', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b362d3a-80c7-47d6-834c-438db4c07816', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('1b362d3a-80c7-47d6-834c-438db4c07816', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('1b362d3a-80c7-47d6-834c-438db4c07816', substance_use_inherently_harmful, deontological).
narrative_ontology:cs_axiom('1b362d3a-80c7-47d6-834c-438db4c07816', foundational, criminalization_necessary_for_prevention).
narrative_ontology:cs_axiom_status(criminalization_necessary_for_prevention, overridden).
narrative_ontology:cs_axiom_grounding('1b362d3a-80c7-47d6-834c-438db4c07816', criminalization_necessary_for_prevention, empirically_contingent).
narrative_ontology:cs_reference_frame('1b362d3a-80c7-47d6-834c-438db4c07816', moral_prohibition_necessity).
narrative_ontology:cs_drift_state('1b362d3a-80c7-47d6-834c-438db4c07816', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b362d3a-80c7-47d6-834c-438db4c07816', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, treatment_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, carceral_administration).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, political_gatekeepers).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, black_market_operators).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_paternalistic_authority).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, moral_harm_doctrine).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, abstinence_as_cardinal_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalization frames substance use as moral failing and legal violation. Users face criminal records, incarceration, family separation, employment barriers, and social stigma. The criminal penalty structure produces carceral extraction (fines, detention) and secondary extraction (loss of custody, housing, employment eligibility). Exit from 'substance user' identity is medically and socially difficult; exit from criminalization requires either abstinence (identity death) or flight to jurisdiction without prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, identity_locked, national).

% Enforcement is concentrated in low-income, racialized neighborhoods via police deployment decisions and prosecutorial discretion. Marginalized communities bear disproportionate incarceration, family dissolution, and wealth extraction through fines and restitution. Geographic and economic barriers make relocation infeasible; legal challenge capacity is systematically constrained by prior criminal records and resource limits.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, trapped, national).

% Bear cascading costs: loss of household income, childcare burden, visitation travel, commissary expenses, bail and legal fees. Children of incarcerated parents face developmental harm, educational disruption, and intergenerational transmission of poverty and criminalization risk. Economic alternatives (informal work, migration) are constrained by state surveillance and data systems.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated, payer,
    powerless, generational, constrained, national).

% Derives operational funding, institutional legitimacy, personnel justification, and political power from drug enforcement. Drug crimes are low-evidentiary-bar arrests that generate statistics, conviction rates, and asset seizure revenue. Enforcement discretion over who gets arrested and charged produces localized gatekeeping power over marginalized populations. Transition to non-criminalization would require institutional reorganization and compete with other police legitimacy claims.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus, agenda_setter).

% Drug-offense convictions fill prison and jail capacity, sustaining budgets, staffing, and operational contracts. Private and public correctional systems extract value (commissary mark-up, phone call fees, prison labor). Decriminalization would reduce the inmate population and associated appropriations. Addiction services contracts and probation administration extract surveillance revenue.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, carceral_administration, beneficiary,
    institutional, generational, arbitrage, national).

% Criminalization feeds demand for court-mandated and incarceration-alternative treatment programs. Funding flows from criminal justice budgets; referrals originate from arrest and sentencing. Harm-reduction or legalization models would require different service models and funding streams, creating business uncertainty for treatment providers dependent on criminal referral pathways.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, treatment_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Prohibition offers a simple, delegitimizing frame for political rivals ('soft on drugs'). Prohibition rhetoric permits broad surveillance and police power expansion with minimal legal scrutiny. Maintains a constituency (law-and-order voters) through symbolic enforcement. Decriminalization invites challenge from this constituency and requires reframing of public safety authority.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, political_gatekeepers, beneficiary,
    institutional, generational, arbitrage, national).

% Criminalization eliminates regulated competition and protects monopolistic rents through supply restriction. Black market violence and territorial disputes are externalities of prohibition; operators benefit from the monopoly but bear enforcement risk. Exit requires legalization, which would eliminate the rent.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_operators, beneficiary,
    organized, biographical, trapped, national).

% Public health professionals, social workers, and medical providers argue criminalization increases harm (overdose risk, infection, delayed treatment-seeking, family dissolution). They would advocate for decriminalization and public-health-led responses if policy forums were open; they are structurally excluded from law-enforcement-dominated drug policy formation and often criminally charged for providing evidence-based harm reduction (needle exchange, medication-assisted treatment).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% Civil liberties, drug policy reform, and libertarian organizations argue autonomy and market regulation are more effective than criminalization. They have policy influence in some jurisdictions and epistemic resources but are excluded from federal drug policy formation and face organized opposition from law-enforcement and treatment-industry interests.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, legalization_advocates, excluded,
    powerful, biographical, constrained, national).

% Measure overdose mortality, infection rates, and treatment access as health outcomes. Prohibited from certain evidence-based interventions by criminal law; conduct epidemiological analysis and advocate policy changes but have limited authority over criminal justice allocation. Sit in analysis and testimony role rather than agenda-setting.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents supply and normalization of substances deemed inherently harmful by a public moral consensus; maintains a unified national policy boundary that prevents jurisdictional regulatory arbitrage (users migrating to permissive zones).
% TRANSFER_FUNCTION: Extracts labor (incarceration, probation supervision, treatment mandates) and wealth (fines, asset seizure, commissary fees) from substance users and their families; redirects enforcement and treatment budgets to law enforcement and carceral administration; restricts supply to benefit black market operators and maintain police gatekeeping authority.
% ABSENT_VOICES: Harm-reduction advocates and medical professionals are structurally excluded from drug policy formation, which is dominated by law-enforcement and political gatekeeping voices. Substance users themselves have no institutional voice in the policy process — they are the object of policy, not participants in it.
% DISAPPEARANCE_RATIONALE: If criminalization vanished, law enforcement would face immediate need for institutional reorganization and budget reallocation; carceral populations would shrink; black market operators would face legalized competition; treatment systems would shift from court-mandated to voluntary models; users would face reduced identity-lock and legal barriers; incarceration-related wealth extraction would collapse. The world would rearrange around substance use as a public health matter and consumption as a regulated or autonomous choice, depending on the alternative framework.
% FOUNDING_PROBLEM: Recreational and self-medicating substance use existed before prohibition; the founding problem is not substance use itself but rather the claim that moral duty and effective public health require criminalization as the control mechanism rather than alternatives (regulation, taxation, medical treatment, harm reduction).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (that criminalization is the most effective or necessary harm-prevention mechanism) is contradicted by 50+ years of epidemiological research, international natural experiments (Portugal decriminalization study, Swiss heroin-assisted treatment, Dutch harm reduction), and public health consensus documented in reports from WHO, American Medical Association, National Academies of Sciences, and the Global Commission on Drug Policy. Law enforcement and carceral interests attest the founding problem remains live; public health and harm-reduction communities attest it is dead and that prohibition increases harm. The empirical record from non-prohibitionist jurisdictions corroborates the harm-reduction reading.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because criminalization produces multiple extraction streams: direct (fines, asset seizure, incarceration costs borne by families), indirect (loss of employment, housing, custody eligibility, intergenerational poverty), and opportunity (black market monopoly rents). The measurement series shows slow rise from 0.62 to 0.81 as enforcement techniques intensify (prosecution of treatment barriers, asset seizure expansion, mandatory minimum sentencing) while the founding problem (necessity of criminalization) erodes empirically. Suppression is very high (0.87) because exit is structurally and identity-locked: users cannot simply choose to no longer use substances (addiction is a medical condition); cannot leave the jurisdiction easily (criminal records, poverty, family ties, surveillance systems); and face internalized shame/moral judgment reinforcing legal penalty. Theater rises from 0.45 to 0.62 as the harm-prevention narrative becomes increasingly unmoored from actual outcomes: enforcement becomes more intensive while overdose mortality, untreated addiction, and family dissolution all worsen under prohibition — the theater is the maintenance of the harm-prevention frame despite contradicting evidence. The constraint is classified as tangled_rope (genuine coordination function of preventing normalization + asymmetric extraction through criminalization) from the prohibition reading's internal logic, but measured metrics suggest drift toward snare as the founding problem validity deteriorates.
 *
 * PERSPECTIVAL GAP:
 *   From law enforcement's perspective, criminalization is legitimate coordination preventing normalization and maintaining public order; from users' and marginalized communities' perspective, criminalization is coercive wealth and liberty extraction justified by false necessity claims. The divergence is not empirical disagreement but structural difference in beneficiary/victim position: those who extract rents (enforcement apparatus, carceral administration) perceive genuine coordination; those who bear costs (users, families) perceive extraction. The constraint's classification should differ per seat: agenda-setter seats (law enforcement, carceral administration) compute toward rope or tangled_rope; payer seats (users, marginalized communities) compute toward snare. The identity-lock on users (inability to exit the substance-use identity, even if they exit the jurisdiction or decriminalization regime) differentiates them from economically constrained payers — their exit is not merely costly but identity-dissolving.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users derive d → 1.0 (full target) from: victims list (criminalization is designed to restrict their behavior), identity_locked exit (they cannot choose to not be users; decriminalization does not restore behavioral freedom, only legal freedom), and powerless power atom (no institutional capacity to resist or negotiate the constraint's terms). Law enforcement derives d → 0.0 (full beneficiary) from: beneficiary list (receives operational funding, institutional legitimacy, gatekeeping authority, discretionary power over marginalized communities), institutional power atom (can set enforcement priorities and policy outcomes), and arbitrage exit (can shift institutional focus to other crime domains). Marginalized communities derive d → 0.95 from: victims list, trapped exit (geographic and economic barriers to relocation; surveillance systems tracking prior criminal records across jurisdictions), and generational time horizon (costs propagate across family lines). Black market operators derive d → 0.1 (near-beneficiary) from: monopoly rents created by supply restriction, but trapped exit within prohibition (if prohibited is removed, their business collapses). No directionality override is needed; the structural data drives the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is assessed as empirically dead: the claim that criminalization is necessary or most effective for harm prevention is contradicted by 50+ years of research and by natural experiments (Portugal decriminalization, Swiss medical heroin, Dutch harm reduction, Uruguay legalization). Yet the constraint persists at high extractiveness (0.81) and high theater (0.62), maintained by institutional inertia and concentrated beneficiary power (law enforcement, carceral administration, political gatekeepers) who have structural interest in continuation. Mandatrophy is resolved by: recognizing that the constraint's persistence is not justified by its founding problem, but by the inversion of cost-benefit for the beneficiary seats; the constraint can be classified as extractive (snare) rather than coordination (tangled_rope) once the founding problem invalidation is acknowledged. The measurement series and founding_problem_status field document this: the constraint persists as pure extraction masked by therapeutic/harm-prevention theater, not as genuinely necessary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_founding_problem_status,
    'Is criminalization still the most evidence-backed harm-prevention mechanism for substance use, or has the founding problem (that criminalization is necessary) been empirically invalidated by harm-reduction and legalization experiments?',
    'Systematic comparison of harm outcomes (overdose mortality, infection rates, treatment access, community health, incarceration impact) across prohibitionist, harm-reduction, and legalization jurisdictions; WHO and public health consensus assessments.',
    'If prohibition is empirically invalidated as a founding problem, the constraint reclassifies from tangled_rope (coordination + asymmetric extraction) to pure snare (extraction justified by false necessity claim). The prohibition reading itself becomes incoherent as a live policy justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_founding_problem_status, empirical, 'Whether the constraint''s founding problem (criminalization is necessary/most effective) remains valid or is empirically dead.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression of substance users structurally enforced (carceral apparatus, criminal records, surveillance) or substantially internalized (shame, identity fusion with addiction, acceptance of moral judgment)?',
    'Post-decriminalization tracking: if users continue to avoid treatment and employment despite removal of criminal penalty, suppression is internalized; if they rapidly access services and re-enter labor markets, suppression was primarily structural.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than the 0.87 structural measure suggests — the target carries criminalization internalization with them after legal penalty removal. This would require extended harm-reduction and identity-repair interventions, not just decriminalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether suppression of users is structural (enforcement apparatus) or internalized (identity/shame).').

omega_variable(
    kernel_committer_reading_boundary,
    'Is this constraint a genuine reading of a contested kernel (prohibition is one way to interpret state authority over substance harm), or is it a doctrine that claims monopoly on harm-prevention and excludes alternative readings by design?',
    'Examine whether the prohibition reading acknowledges rival readings as coherent alternatives that differ on normative premises, or claims they are empirically or morally incoherent. If the former, genuine kernel reading; if the latter, the constraint is not a reading but a doctrine asserting monopoly.',
    'If monopoly doctrine rather than genuine reading, the committer-frame structure (rules 1-4) is a misclassification and the constraint should be reauthored as a pure snare with false-naturalization pretense (no rival readings, no committer axioms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_reading_boundary, conceptual, 'Whether this is a genuine reading of a contested kernel or a doctrine claiming exclusivity.').

omega_variable(
    black_market_violence_externality,
    'Is black market violence (territorial disputes, trafficking violence, gang wars) a necessary externality of criminalization, or an avoidable side effect of specific enforcement strategies?',
    'Comparison of violence outcomes across jurisdictions with different enforcement intensity and supply-management strategies; natural experiments from countries shifting enforcement strategies while remaining prohibitionist.',
    'If violence is avoidable within prohibition, the extraction structure is constrained to carceral costs; if inherent to monopoly creation by prohibition, violence is part of the extractive burden borne by communities where black markets operate. Current model assumes inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_externality, empirical, 'Whether black market violence is inherent to criminalization or avoidable via different enforcement approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__prohibition_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__prohibition_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__prohibition_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__prohibition_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__prohibition_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__prohibition_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__prohibition_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__prohibition_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__prohibition_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement_basis(subs_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the substance_control_legitimacy kernel. The prohibition_reading instantiates state authority via criminalization and moral duty to prevent use; harm_reduction_reading instantiates state authority via public health minimization of use harms; legalization_reading instantiates state authority via autonomy respect and third-party harm prevention. Each reading has its own ε (prohibition: 0.81, referent = criminalization arrangement; harm_reduction: lower ε, referent = public-health-led arrangement; legalization: even lower ε, referent = regulated-market arrangement). The readings are not perspectives on one constraint but separate constraints with distinct beneficiary/victim structures, founding problems, and empirical status. All three are live readings in public discourse; no single reading forecloses another within a unified policy framework, though any two are mutually exclusive for a single jurisdiction's policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
