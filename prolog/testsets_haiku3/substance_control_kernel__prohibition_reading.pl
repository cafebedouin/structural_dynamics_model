% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Use as Criminal Moral Transgression
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the prohibition reading of the
 *   contested substance-control kernel: the claim that substance use is moral
 *   transgression requiring state punishment to protect social order. Under
 *   this reading, users are criminals (not patients), enforcement apparatus
 *   is the primary beneficiary, and the constraint's persistence is justified
 *   by threat inflation and moral absolutes. This reading competes in real
 *   discourse against harm-reduction and legalization readings, each grounded
 *   in different axioms about substance use and state authority. The ε value
 *   (0.81) is high because the constraint extracts substantially from users
 *   (criminalization, incarceration, permanent records, identity-locking)
 *   while transferring institutional power and budget to enforcement and
 *   correctional sectors. The measurement series track extraction
 *   accumulation over 40 time units (modeling the historical trend since War
 *   on Drugs escalation ~1980–2020), showing rising extractiveness,
 *   plateauing theater ratio (sustained performative maintenance at ~0.47),
 *   and rising suppression requirement (enforcement infrastructure
 *   hardening). The coercion grid distinguishes levels: individual-level
 *   suppression is the highest (direct criminalization, arrest risk, family
 *   separation); structural-level suppression is lower but still high (the
 *   constraint as institutional arrangement is contested and faces organized
 *   pushback). The claim/metric independence is maintained: the constraint is
 *   CLAIMED as necessary social protection (the prohibition reading's own
 *   framing) while the metrics describe substantially extractive,
 *   violence-producing operation.
 *
 * KEY AGENTS:
 *   - substance_users: criminalized victims bearing identity-lock and incarceration
 *   - law_enforcement_apparatus: primary institutional beneficiary, sets enforcement priorities and budget justification
 *   - correctional_industry: secondary beneficiary, sustained by incarceration demand
 *   - low_income_communities: class-level victims bearing disproportionate enforcement
 *   - families_of_incarcerated: economically and socially devastated by constraint operation
 *   - public_health_professionals: excluded parties who would propose alternative coordination (harm reduction, treatment)
 *   - legislators: agenda-setters who could dismantle constraint but face political cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Criminal Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '8798f125-d71c-4d6c-949e-46d7af5e4a92').
narrative_ontology:cs_kernel_codification('8798f125-d71c-4d6c-949e-46d7af5e4a92', formalized).
narrative_ontology:cs_authority_grounding('8798f125-d71c-4d6c-949e-46d7af5e4a92', extraction).
narrative_ontology:cs_interpretation_layer_present('8798f125-d71c-4d6c-949e-46d7af5e4a92').
narrative_ontology:cs_reading_relation('8798f125-d71c-4d6c-949e-46d7af5e4a92', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('8798f125-d71c-4d6c-949e-46d7af5e4a92', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('8798f125-d71c-4d6c-949e-46d7af5e4a92', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('8798f125-d71c-4d6c-949e-46d7af5e4a92', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('8798f125-d71c-4d6c-949e-46d7af5e4a92', foundational, criminal_punishment_necessary_for_social_order).
narrative_ontology:cs_axiom_status(criminal_punishment_necessary_for_social_order, holdable).
narrative_ontology:cs_axiom_grounding('8798f125-d71c-4d6c-949e-46d7af5e4a92', criminal_punishment_necessary_for_social_order, deontological).
narrative_ontology:cs_reference_frame('8798f125-d71c-4d6c-949e-46d7af5e4a92', moral_transgression_deterrence_framework).
narrative_ontology:cs_drift_state('8798f125-d71c-4d6c-949e-46d7af5e4a92', contemporary_evidence_era_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8798f125-d71c-4d6c-949e-46d7af5e4a92', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, correctional_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, drug_enforcement_bureaucracy).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, low_income_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, families_of_incarcerated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalized for addiction, charged with moral transgression, subject to arrest, incarceration, and permanent criminal records that bar employment and housing. Exit from substance use is treated as the only legitimate exit from the constraint; substance users' own testimony about use patterns, harm reduction efficacy, or gradual cessation is systematically excluded from policy discussion. Their identity as 'criminals' rather than 'people with medical conditions' is the primary enforcement mechanism.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, biographical, identity_locked, national).

% Sets arrest priorities, prosecutes users and dealers, justifies escalating enforcement budgets and technologies by pointing to drug availability and street use. Collects resources (budget, personnel, equipment, municipal authority) that flow from drug-war framing. Has institutional incentive to expand enforcement scope and demonstrate activity; the constraint's persistence directly sustains the apparatus's scale and authority.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Both state and private correctional systems receive sustained demand (incarceration for drug offenses) that justifies expansion, new facilities, and ongoing contracts. Prison populations are managed and occupied; drug offense convictions provide steady caseload. The constraint keeps beds filled and budgets justified.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, correctional_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Dedicated agencies (DEA, international narcotics enforcement) derive their statutory mandate, personnel, and budget authority from the prohibition framework. Threat inflation (portraying drug use as an existential social danger) justifies continued operation and expansion. The constraint's dismantling would eliminate these agencies' primary purpose.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, drug_enforcement_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).

% Disproportionately subject to drug enforcement (surveillance, policing, arrests) despite similar or lower per-capita use rates than affluent communities. Face concentrated incarceration of working-age adults, destabilizing family structure and economic opportunity. Lack political power to resist or redirect enforcement priorities.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, low_income_communities, payer,
    organized, biographical, constrained, national).

% Bear economic and social costs of incarceration: lost income, kinship rupture, intergenerational trauma, stigma. Children grow up in households marked by parental incarceration; the constraint operates across generations. Their perspective is structurally excluded from drug policy formation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, families_of_incarcerated, payer,
    powerless, biographical, trapped, national).

% Would recommend evidence-based treatment and harm reduction (medication-assisted therapy, needle exchange, overdose prevention sites) as primary interventions. Are systematically excluded from drug policy leadership; prohibition framing treats their expertise as secondary to criminal justice framing. Their alternative coordination function is not admitted to policy space.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_professionals, excluded,
    institutional, generational, constrained, national).

% Authorized to revise or dismantle the constraint; politically incentivized to continue it ('tough on drugs' remains a vote-winning position despite policy reversal in peer democracies). Receive lobbying pressure from law enforcement and correctional interests; hear from substance users and public health professionals only when organized opposition reaches critical threshold. The constraint persists because exit is politically costlier than maintenance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, legislators_and_policy_makers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, legislators_and_policy_makers, observer).

% Non-agent entity: prohibition creates the economic foundation for criminal drug distribution networks. Black market violence, turf warfare, and supply-chain predation are externalities of the constraint, not organized actors, but they structure the enforcement justification ('we must keep fighting to control the chaos prohibition created').
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, illicit_drug_markets, beneficiary,
    powerful, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__prohibition_reading, illicit_drug_markets).

% Non-agent entity: the axiom that substance use is moral transgression (not illness, not choice) is vindicated by the constraint's operation and enforcement. The doctrine collects no rents but is institutionally sustained as the justification for enforcement; the constraint carries it forward as legitimate framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_absolutism_doctrine, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__prohibition_reading, moral_absolutism_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None: this reading claims no coordination function. The constraint claims social order protection via punishment, but punishment is not a coordination mechanism (multiple parties solving a collective-action problem). The counter-coordination function—the alternative this reading rejects—would be harm reduction (coordinating health services and overdose prevention across multiple actors). This reading's claim is that harm reduction is insufficient and punishment must be added; but the constraint's own operation does not coordinate harm reduction, it displaces it.
% TRANSFER_FUNCTION: Transfers criminal liability, incarceration risk, and permanent identity stigma from state to users and families. Transfers institutional budget, enforcement authority, and personnel positions from other sectors to law-enforcement and correctional sectors. Transfers labor (state must invest in enforcement machinery, courts, prisons) from other state services. In aggregate: moves resources, authority, and identity-classification toward punishment apparatus and away from health and social services.
% ABSENT_VOICES: Substance users themselves are structurally excluded — treated as criminals, not as experts on their own condition or exit barriers. Public health professionals are excluded from drug policy formation, despite having evidence-based treatment protocols. Families of incarcerated people are excluded despite bearing massive costs. Communities experiencing disproportionate enforcement are excluded from priority-setting. The parties the constraint would benefit (harm-reduction advocates, legalization supporters) are present in broader policy discourse but not seated in drug enforcement governance.
% DISAPPEARANCE_RATIONALE: If the prohibition constraint disappeared: law enforcement and correctional agencies would contract substantially or be eliminated; millions of criminal records would be expunged or cease being obstacles to employment/housing; users would exit toward treatment rather than criminal prosecution; public health systems would reorient toward evidence-based interventions; black market violence would decline as supply moved to regulated channels; families would recover incarcerated members and their income; stigma would shift from criminal transgression to treatable condition. The world rearranges because the constraint sustains an enormous institutional and carceral apparatus that would either collapse or transform.
% FOUNDING_PROBLEM: Substance use was framed in early 20th century policy (U.S. and internationally via treaties like the Single Convention on Narcotic Drugs) as moral transgression and social contagion requiring criminal prohibition to prevent spread of addiction and social decay. The prohibition constraint was built to solve the perceived threat of use-as-contagion via deterrent punishment.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is empirically dead: peer-reviewed public health literature (JAMA, Lancet, Addiction journals, NIH consensus panels) establishes that increased criminal enforcement severity does NOT reduce use prevalence; rather, jurisdictions with the highest incarceration for drug offenses continue to show high or rising use rates. Jurisdictions that decriminalized or legalized (Portugal 2001–present, Switzerland harm-reduction pilots, Canada cannabis legalization, parts of Australia) show reduced use prevalence, reduced overdose deaths, and reduced crime. These are corroborating sources outside the enforcement beneficiary set (peer-reviewed research community, public health agencies, comparative government data). The founding problem (moral transgression / contagion requiring punishment) has been falsified in the evidence base; the constraint persists anyway because enforcement beneficiaries have captured the policy apparatus and can afford to deny the evidence.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the constraint transfers authority and resources from health/social sectors to punishment sectors, while users and families bear expanding costs (criminal records, incarceration, permanent stigmatization, economic exclusion). Suppression is the highest component (0.88) because the constraint's persistence depends entirely on active, coercive enforcement — users would exit toward treatment if exit were not criminalized; the constraint requires continuous legal machinery to hold. Theater ratio is moderate-to-high (0.47) because enforcement rhetoric (threat inflation, drug-war statistics) sustains the constraint's legitimacy despite the founding problem being objectively dead (peer democracies with legalization show reduced use and reduced harm; prohibition-intensive jurisdictions show persistent or rising use). The measurement series plateau late in the interval (extractiveness flattens at ~0.81 after 30 time units, theater stays at 0.47) because the constraint has reached institutional equilibrium — enforcement apparatus is fully built out, beneficiary interests are entrenched, and no internal pressure toward change is sufficient to alter it. Resistance is rising through the interval (0.34→0.38 at individual level, 0.52→0.61 at organizational level, 0.58→0.68 at class level) because public awareness of prohibition's failure accumulates; organizational resistance (public health bodies, advocacy coalitions) is gaining relative to individual-level resistance (scattered users) because the constraint's ineffectiveness becomes undeniable in aggregate. The coercion grid shows individual-level accessibility collapse near ceiling (0.92–0.93) because substance users as individuals have almost no exit routes (criminalization, treatment-coercion, or continued use-under-threat are the only options); structural-level collapse is lower (0.58→0.62) because the overall system acknowledges that legalization exists as a counterfactual alternative in other jurisdictions — the constraint is contestable at the system level even though individual users face near-total constraint.
 *
 * PERSPECTIVAL GAP:
 *   Law enforcement and correctional agents experience the constraint as legitimate institutional authority protecting social order; users experience it as coercive extraction and identity-assault. The engine derives this perspectival gap from structural data: agenda-setters (law_enforcement_apparatus) with institutional power and arbitrage exits derive low effective extraction (beneficiary-positioned); payers (substance_users) with powerless status and identity-locked exits derive high effective extraction (target-positioned). The gap should be substantial and visible in per-seat type classification: the constraint appears as institutional coordination (maintaining order) from the enforcement seat, but as pure snare (extraction via criminalization) from the user seat. This divergence is the measurement the corpus exists to detect — the authored claim (prohibition = social protection) and the authored metrics (high extraction, high suppression, dead founding problem) generate exactly this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary positioning: law_enforcement_apparatus (d ≈ 0.1, near beneficiary extreme) — institutional power, generational time horizon, arbitrage exits (can expand or contract enforcement), direct institutional collection of budget/authority. Correctional_industry (d ≈ 0.15) — similar structural position, dependent on sustained prisoner flow. Target positioning: substance_users (d ≈ 0.95, near target extreme) — powerless status, identity-locked exit (cessation is framed as the only legitimate exit, but framing that as 'exit from the constraint' rather than 'exit from substance use' is the constraint's core violence), biological addiction makes exit costly and identity-fusion makes it psychologically unavailable. Families_of_incarcerated (d ≈ 0.88) — trapped exit (cannot recover incarcerated relatives without constraint dismantling), powerless status. Low_income_communities (d ≈ 0.82) — organized power (collective agency) but constrained exits (geographically concentrated enforcement, political marginalization). Class-level directionality differs from individual because organization provides some exit alternative (collective advocacy, political mobilization) even though structural position remains trapped. Public_health_professionals are excluded, not coordinated beneficiaries — their directionality toward the constraint is not computed (observer position), but if forced to assign: d ≈ 0.85 (constrained exit from the healthcare system, forced to work within prohibition-structured funding and legal constraints, but professional authority keeps them near organized rather than powerless).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding problem (substance use as moral contagion requiring criminal deterrent) is dead — use patterns do not respond to increased enforcement severity, and comparative jurisdictions show legalization/decriminalization reduces use prevalence and harm. Yet the constraint persists with high extractiveness (0.81) and rising theater ratio because enforcement beneficiaries (law enforcement, correctional systems) have institutional incentives to expand the constraint rather than admit its failure. The mandatrophy manifests as widening gap between founding problem status (dead — refuted in literature, contradicted by peer-nation evidence) and disappearance verdict (world_rearranges — the constraint structurally matters because it sustains enforcement institutions). This gap is flagged by the R5 genealogy interview: founding_problem_status=dead + disappearance_verdict=world_rearranges is precisely the snare/zombie signature the mandatrophy framework detects. The theater ratio rising from 0.35 to 0.47 (while extractiveness plateaus) indicates enforcement activity increasingly becomes theatrical maintenance (drug-war statistics, high-profile prosecutions, media campaigns) rather than functional problem-solving (use reduction, harm minimization). The constraint's classification as snare is justified by mandatrophy: it extracts from users and families, requires active enforcement to prevent alternatives (harm reduction, treatment, legalization), and persists not because it solves the problem it claims to solve but because its beneficiaries have captured the policy apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'What proportion of the measured suppression (0.88) is structural coercion (legal barriers, enforcement apparatus, arrest risk) versus internalized (users'' self-perception as criminal, identity-fusion with transgressor label, intergenerational trauma that perpetuates compliance)?',
    'Quasi-experimental analysis from jurisdictions that decriminalized: if suppression-like behaviors (non-disclosure, avoidance of services, silence) persist after the legal apparatus is removed, that signals high internalization. If behaviors rapidly shift after decriminalization, that signals suppression was primarily structural.',
    'If largely structural, constraint removal would rapidly shift user behavior and outcomes. If largely internalized, constraint removal would require sustained intervention to break identity-fusion and intergenerational patterns — a longer transition time and higher fixing cost than structural-only suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of this constraint is externally imposed or self-perpetuating through identity and trauma.').

omega_variable(
    founding_problem_extinction_vs_deflation,
    'Is the founding problem (substance use as moral threat requiring criminal deterrent) objectively extinct in observable reality, or does it persist as a organizing axiom for enforcement beneficiaries despite being falsified in the evidence base?',
    'Track enforcement rhetoric and institutional commitment over time: if agencies explicitly acknowledge the problem is dead but continue enforcement anyway (''because stopping would cause institutional collapse''), that''s zombie constraint. If agencies continue to assert the problem is live despite contradictory data, that''s denial/internalizing the axiom as non-empirical (deontological) rather than empirical.',
    'If zombie (dead problem, continued extraction), the constraint belongs in piton-grade analysis: what''s left is institutional theater and inertia. If axiom-internalization (the ''moral transgression'' claim is treated as truth-independent), the constraint is foreclosed by empirical overriding of its deontological premise — but deontological axioms don''t foreclose to empirical evidence by the schema''s rules, so this remains a live structural tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_extinction_vs_deflation, empirical, 'Whether the constraint''s founding mandate has been empirically falsified or institutionally denied.').

omega_variable(
    identity_lock_nature_and_reversibility,
    'Substance users are authored as identity_locked; is the lock primarily the criminal record itself (structural: employers won''t hire, housing won''t rent) or the self-concept internalization (''I am a criminal/addict'')? Are they equally reversible?',
    'Longitudinal analysis from expungement/record-sealing policies: if structural lock is removed (record sealed, legal barriers lifted), do users'' self-concept and social reintegration follow rapidly? If not, the identity-lock is predominantly internalized and would require additional intervention (therapy, restorative justice, community reintegration programs).',
    'If lock is primarily structural, constraint removal (decriminalization) would largely solve the exit problem. If lock is primarily internalized, constraint removal would be necessary but insufficient — users would need additional support to reverse identity-fusion and reintegrate. This affects the estimated fixing_cost (cheap if only decriminalization needed, prohibitive if also requires mental-health infrastructure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_nature_and_reversibility, empirical, 'Whether identity-locking of substance users is structurally enforced or self-perpetuating through trauma and internalized stigma.').

omega_variable(
    black_market_violence_as_constraint_externality_vs_independent,
    'The measurement series describe rising extractiveness over the 40-time-unit interval; is this rising extractiveness a property of the constraint itself, or is it measuring the constraint-plus-its-black-market-externality as one thing?',
    'Decompose the extractiveness metric into two readings: (1) extractiveness from criminalization alone (enforcement budget, incarceration, records, stigma), and (2) extractiveness from black-market violence produced by prohibition (supply-chain murders, turf warfare, contaminated supply, overdose deaths from inconsistent potency). If (1) is stable and (2) is rising, the apparent constraint-level extraction is actually measuring how much worse the externality becomes over time, not how much more the prohibition-constraint itself extracts.',
    'This affects the reading''s relationship to sibling readings: if most measured extractiveness (0.81) is actually black-market violence (an externality), then the harm_reduction and legalization readings'' case for opening the market becomes stronger — they reduce (2) while possibly increasing (1) slightly. If extractiveness is primarily from criminalization (1), then legalization''s case rests on accepting the loss of moral-transgression framing, not on empirical harm reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_as_constraint_externality_vs_independent, conceptual, 'Whether measured constraint extractiveness includes black-market externalities or only direct enforcement extractiveness.').

omega_variable(
    axiom_moral_transgression_temporal_holdability,
    'The prohibition_reading grounds itself in the axiom that substance use IS moral transgression (not illness, not victimless choice). Is this axiom still genuinely holdable in contemporary discourse, or has it been functionally overridden by medical framing even within institutions that maintain prohibition-like practices?',
    'Audit medical and legal institutions'' own documentation: do courts, medical examiners, public health agencies, and treatment facilities continue to use language consistent with ''moral transgression'' (willful wrongdoing, culpability, deserved punishment)? Or do they use medical language (disorder, addiction, disease, treatment need) while enforcing criminal consequences — a split between stated and implemented axioms?',
    'If axiom is overridden in practice (courts acknowledge addiction while sentencing for addiction, medical systems acknowledge substance-use disorder while criminal system punishes as transgression), the prohibition_reading''s foundational axiom is incoherent within its own institutions — the constraint is operating under a different, hidden axiom (perhaps: ''substance use is a social-control problem requiring enforcement regardless of health status''). This affects the reading''s stability and foreclosure relations to siblings: an overridden axiom loses logical force but may persist as institutional theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_moral_transgression_temporal_holdability, conceptual, 'Whether the moral-transgression axiom remains coherent in contemporary institutional practice or has been functionally overridden by medical framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__prohibition_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__prohibition_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__prohibition_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.47).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__prohibition_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__prohibition_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__prohibition_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__prohibition_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__prohibition_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__prohibition_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.88).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(subs_grid_01, substance_control_kernel__prohibition_reading, accessibility_collapse(class), 0, 0.76).
narrative_ontology:measurement(subs_grid_02, substance_control_kernel__prohibition_reading, accessibility_collapse(class), 40, 0.78).
narrative_ontology:measurement(subs_grid_03, substance_control_kernel__prohibition_reading, accessibility_collapse(individual), 0, 0.92).
narrative_ontology:measurement(subs_grid_04, substance_control_kernel__prohibition_reading, accessibility_collapse(individual), 40, 0.93).
narrative_ontology:measurement(subs_grid_05, substance_control_kernel__prohibition_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(subs_grid_06, substance_control_kernel__prohibition_reading, accessibility_collapse(organizational), 40, 0.71).
narrative_ontology:measurement(subs_grid_07, substance_control_kernel__prohibition_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(subs_grid_08, substance_control_kernel__prohibition_reading, accessibility_collapse(structural), 40, 0.62).
narrative_ontology:measurement(subs_grid_09, substance_control_kernel__prohibition_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(subs_grid_10, substance_control_kernel__prohibition_reading, resistance(class), 40, 0.68).
narrative_ontology:measurement(subs_grid_11, substance_control_kernel__prohibition_reading, resistance(individual), 0, 0.34).
narrative_ontology:measurement(subs_grid_12, substance_control_kernel__prohibition_reading, resistance(individual), 40, 0.38).
narrative_ontology:measurement(subs_grid_13, substance_control_kernel__prohibition_reading, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(subs_grid_14, substance_control_kernel__prohibition_reading, resistance(organizational), 40, 0.61).
narrative_ontology:measurement(subs_grid_15, substance_control_kernel__prohibition_reading, resistance(structural), 0, 0.41).
narrative_ontology:measurement(subs_grid_16, substance_control_kernel__prohibition_reading, resistance(structural), 40, 0.45).
narrative_ontology:measurement(subs_grid_17, substance_control_kernel__prohibition_reading, stakes_inflation(class), 0, 0.73).
narrative_ontology:measurement(subs_grid_18, substance_control_kernel__prohibition_reading, stakes_inflation(class), 40, 0.75).
narrative_ontology:measurement(subs_grid_19, substance_control_kernel__prohibition_reading, stakes_inflation(individual), 0, 0.89).
narrative_ontology:measurement(subs_grid_20, substance_control_kernel__prohibition_reading, stakes_inflation(individual), 40, 0.91).
narrative_ontology:measurement(subs_grid_21, substance_control_kernel__prohibition_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(subs_grid_22, substance_control_kernel__prohibition_reading, stakes_inflation(organizational), 40, 0.54).
narrative_ontology:measurement(subs_grid_23, substance_control_kernel__prohibition_reading, stakes_inflation(structural), 0, 0.41).
narrative_ontology:measurement(subs_grid_24, substance_control_kernel__prohibition_reading, stakes_inflation(structural), 40, 0.43).
narrative_ontology:measurement(subs_grid_25, substance_control_kernel__prohibition_reading, suppression(class), 0, 0.79).
narrative_ontology:measurement(subs_grid_26, substance_control_kernel__prohibition_reading, suppression(class), 40, 0.81).
narrative_ontology:measurement(subs_grid_27, substance_control_kernel__prohibition_reading, suppression(individual), 0, 0.84).
narrative_ontology:measurement(subs_grid_28, substance_control_kernel__prohibition_reading, suppression(individual), 40, 0.86).
narrative_ontology:measurement(subs_grid_29, substance_control_kernel__prohibition_reading, suppression(organizational), 0, 0.71).
narrative_ontology:measurement(subs_grid_30, substance_control_kernel__prohibition_reading, suppression(organizational), 40, 0.73).
narrative_ontology:measurement(subs_grid_31, substance_control_kernel__prohibition_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(subs_grid_32, substance_control_kernel__prohibition_reading, suppression(structural), 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__prohibition_reading, 0.22).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three structurally distinct constraint stories: (1) prohibition_reading (this file) grounds legitimacy in moral-transgression axiom and punishment authority; (2) harm_reduction_reading grounds legitimacy in health/pragmatism and treats substance use as chronic condition; (3) legalization_reading grounds legitimacy in liberty/externality-capture and treats use as individual choice. The ε values differ substantially: prohibition_reading has high extraction (0.81) because it criminates the primary agent (users); harm_reduction_reading has moderate extraction because it coordinates health services but may still constrain individual choice; legalization_reading has low extraction because it permits use subject only to externality-capture mechanisms (tax, driving-under-influence law). The beneficiary/victim structures are inverted: prohibition extraction flows to enforcement/correctional; harm_reduction extraction (if any) would flow to medical/service institutions; legalization would eliminate the extraction-to-enforcement pathway entirely. The three readings are not alternative measurements of a neutral fact — they are competing normative framings that institutions and parties actively advocate for in real political struggle. Each reading produces a different constraint; each constraint has different structural properties; the struggle between readings is visible in divergent per-seat type classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
