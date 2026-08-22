% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Criminalization of Drug Possession to Protect Third Parties (Prohibition Reading)
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the PROHIBITION READING of the contested
 *   kernel 'substance_control_authority' — the framework in which the state's
 *   legitimate role is to criminalize drug use and possession in order to
 *   protect third parties from drug-related crime and social disorder. Users
 *   of criminalized drugs enter the victim set through the mechanism of
 *   enforcement itself; protection of third parties (communities experiencing
 *   drug-driven crime, overdose deaths, disorder) is the stated coordination
 *   function. The reading is contested against two sibling readings: the
 *   HARM-REDUCTION reading (which accepts drug use while minimizing health
 *   harms through public health interventions) and the LEGALIZATION reading
 *   (which regulates drug markets as legal commerce). This story models the
 *   prohibition reading as a TANGLED ROPE: genuine coordination (establishing
 *   market disruption and deterrence as shared goods for communities)
 *   combined with asymmetric extraction (incarceration and collateral
 *   consequences concentrated on users and low-income communities). The
 *   claim/metric gap is structural: the prohibition reading frames the
 *   constraint as protecting third parties (coordination narrative), while
 *   the authored metrics describe high extraction, rising suppression
 *   requirement (indicating enforcement intensification), and rising theater
 *   ratio (indicating an increasing gap between publicly stated function and
 *   actual enforcement focus). The engine's computation of this divergence —
 *   claimed rope, computed tangled rope or snare, depending on directionality
 *   — is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - State enforcement apparatus (institutional power, agenda-setter) — sets criminalization policy, controls enforcement prioritization, receives institutional benefits from drug-war infrastructure
 *   - Drug users criminalized (powerless, trapped exit) — face criminal penalties, incarceration, collateral consequences
 *   - Incarcerated drug offenders (powerless, trapped exit) — concentrated extraction victims bearing liberty loss, family separation, economic devastation
 *   - Low-income communities over-policed (powerless, trapped exit) — disproportionately subject to enforcement; experience racial disparities in arrest and incarceration
 *   - Property crime victims and disorder-affected communities (moderate power, constrained exit) — nominally protected by market disruption and user incapacitation; also subject to intensified policing that creates community degradation
 *   - Law enforcement institutions (institutional power, beneficiary/agenda-setter) — benefit from budget expansion, personnel justification, civil asset forfeiture, surveillance infrastructure expansion
 *   - Harm-reduction advocates and medical authorities (moderate/institutional power, excluded) — would claim criminalization increases harms; structurally excluded from policy process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Criminalization of Drug Possession to Protect Third Parties (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '9a11e6c1-37c5-4bcc-8555-2e8d08bd863b').
narrative_ontology:cs_kernel_codification('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', formalized).
narrative_ontology:cs_authority_grounding('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', extraction).
narrative_ontology:cs_interpretation_layer_present('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b').
narrative_ontology:cs_reading_relation('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', foundational, criminalization_deters_drug_use).
narrative_ontology:cs_axiom_status(criminalization_deters_drug_use, holdable).
narrative_ontology:cs_axiom_grounding('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', criminalization_deters_drug_use, empirically_contingent).
narrative_ontology:cs_axiom('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', foundational, incapacitation_through_incarceration_protects_third_parties).
narrative_ontology:cs_axiom_status(incapacitation_through_incarceration_protects_third_parties, holdable).
narrative_ontology:cs_axiom_grounding('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', incapacitation_through_incarceration_protects_third_parties, instrumental).
narrative_ontology:cs_reference_frame('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', criminal_deterrence_doctrine).
narrative_ontology:cs_drift_state('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', opioid_crisis_era_2010_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a11e6c1-37c5-4bcc-8555-2e8d08bd863b', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, property_crime_victims).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, disorder_affected_communities).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_institutions).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users_criminalized).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, incarcerated_drug_offenders).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_communities_over_policed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets criminalization policy, controls drug scheduling, prosecutes drug possession, administers incarceration and parole systems. Justifies enforcement as deterrence, market disruption, and protection of third parties from drug-driven crime and disorder. Receives budgetary expansion, institutional legitimacy, civil asset forfeiture revenue, and personnel justification from the drug war infrastructure. Maintains monopoly over enforcement authority and controls prioritization of which drugs and which users to criminalize.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Face criminal penalties for possession and use, including arrest, prosecution, incarceration, and collateral consequences (employment barriers, housing discrimination, loss of parental rights, voting disenfranchisement, educational exclusion). Their drug use is typically driven by poverty, trauma, disability, or addiction; criminalization does not address underlying causes. Their exit options are not genuinely available: geographical relocation may temporarily reduce arrest risk but does not eliminate exposure; stopping use is constrained by addiction and by lack of treatment access in criminalized contexts; using privately entails ongoing criminal liability regardless of behavior. The constraint operates entirely at their expense.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users_criminalized, payer,
    powerless, biographical, trapped, national).

% Bear the concentrated extraction mechanism: loss of liberty, institutional violence exposure, family separation, economic devastation, and permanent social marking via criminal record. Their incarceration is justified as protecting third parties, but they themselves are third parties whose protection is deemed secondary to the deterrent and incapacitative value of their imprisonment. During incarceration, exit options are non-existent. Post-release, collateral consequences (employment barriers, housing discrimination, surveillance monitoring) perpetuate extraction indefinitely.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, incarcerated_drug_offenders, payer,
    powerless, biographical, trapped, national).

% Bear the enforcement burden disproportionately. Racial minorities, particularly Black and Latino communities, experience drug-related arrest and incarceration at rates 5–10 times higher than white communities despite similar or lower drug use prevalence. These communities pay through criminalization of their own members, intensified police presence, police violence risk, community degradation, surveillance infrastructure concentrated in their neighborhoods, lost economic productivity, and intergenerational incarceration. Exit is geographically constrained by poverty and housing discrimination. The enforcement apparatus concentrates enforcement in these communities partly because open-air markets are more visible in low-income areas, partly because police discretion is exercised against minoritized populations.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_communities_over_policed, payer,
    powerless, generational, trapped, local).

% Nominally benefit from deterrence that is supposed to reduce property crime associated with drug use and market activity. The criminalization authority is partly justified as protecting their safety and property from theft and burglary. However, their benefit is mixed and indirect: they experience protection via incapacitation of criminalized users, but also incur costs of intensified policing, police violence risk in their own communities, and the diffuse social costs of mass incarceration. Their exit options are constrained by geography and housing costs; they cannot easily relocate to communities with lower crime and lower policing.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, property_crime_victims, beneficiary,
    moderate, biographical, constrained, local).

% Experience open-air drug markets, associated public intoxication, public drug use and associated paraphernalia, and overdose deaths. Criminalization is framed as protecting them through market disruption and user incapacitation. However, enforcement is implemented through intensified police presence concentrated in their communities, which criminalizes their own residents and creates social distrust and community degradation. The nominal protection from market disruption is offset by the actual harm of criminalization of their neighbors and family members, creating a dual burden: nominal benefit, concentrated extraction cost.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, disorder_affected_communities, beneficiary,
    organized, biographical, constrained, local).

% Police departments, prosecutors' offices, and correctional agencies benefit from drug criminalization through budget allocation, personnel expansion, civil asset forfeiture revenue, surveillance infrastructure expansion, and institutional legitimacy. They coordinate with the state enforcement apparatus and have both authority and financial incentive to expand criminalization scope. Police departments use drug arrests as high-volume enforcement metrics to justify budget increases and staffing; prosecutors build career advancement on drug conviction numbers; correctional agencies justify facility expansion and staffing through incarceration rates driven by drug offenses (25–30% of incarcerated persons in the US are in prison for drug-only offenses).
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_institutions, agenda_setter).

% Are structurally excluded from policy authority because harm-reduction interventions (syringe exchange, medication-assisted treatment, supervised consumption sites, naloxone access) are criminalized or severely restricted under the prohibition framework. They would argue that criminalization increases harms (overdose deaths rising 5x in US 1999–2020, disease transmission risk from contaminated supplies, community distrust of institutions preventing treatment-seeking). Their voice is marginalized in the policy process despite epidemiological evidence supporting their claims.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% Scientific evidence from addiction medicine, epidemiology, and public health demonstrates that criminalization does not reduce drug use and amplifies harms. The American Medical Association, World Health Organization, American Public Health Association, and national public health agencies attest that drug addiction is a health condition requiring medical treatment, not primarily a criminal matter. They observe but are constrained in policy influence by the criminalization framework's institutional dominance and the enforcement apparatus's vested interests.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, medical_and_public_health_authorities, observer,
    institutional, generational, constrained, national).

% Jurisdictions adopting harm-reduction (Portugal decriminalized in 2001) or legalization frameworks (Canada, parts of Europe) experience different outcomes: lower overdose deaths, reduced incarceration, improved treatment access, but are internationally constrained by drug treaties (UN Convention on Narcotic Drugs, 1961) that mandate criminalization. The prohibition reading's framework is enforced through international legal instruments that make alternative readings formally illegal in treaty-signatory nations. Trapped between domestic evidence suggesting alternatives work better and international treaty obligations mandating prohibition.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, competing_jurisdictions, excluded,
    institutional, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, law_enforcement_institutions).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes criminal law as the primary tool for managing drug use and markets; coordinates state enforcement institutions around a unified drug control mission; creates deterrent threat and incapacitation mechanisms to disrupt open-air drug markets and reduce user visibility in public spaces; establishes shared state authority over drug scheduling, enforcement prioritization, and incarceration decisions.
% TRANSFER_FUNCTION: Transfers liberty, economic opportunity, family relationships, and social standing from drug users and low-income communities (via arrest, prosecution, incarceration, collateral consequences, and disproportionate policing) to law enforcement institutions (budgetary expansion, personnel justification, civil asset forfeiture revenue, institutional legitimacy) and nominally to communities protected from drug-driven crime (though the protection is diffuse and uncertain while the enforcement burden concentrates on the payer communities themselves).
% ABSENT_VOICES: Harm-reduction practitioners, people with lived addiction experience, medical and public health authorities, civil rights advocates documenting racial disparities in enforcement, incarcerated persons and their families, and competing-jurisdiction policymakers practicing decriminalization or legalization are structurally excluded from the policy-setting process. Their objection is that criminalization increases harms (overdose, disease, incarceration trauma, community degradation) and that alternatives (treatment, harm reduction, regulated markets) are more effective for the stated goal (protecting third parties) and for the primary victims (people struggling with addiction). They are kept out by the framework itself, which treats drug use as a crime problem rather than a health problem and criminalizes their very participation in alternative solutions (harm reduction is criminalized in most jurisdictions under prohibition).
% DISAPPEARANCE_RATIONALE: If the criminalization authority vanished overnight, drug markets would reorganize (toward legalization, decriminalization, or harm-reduction frameworks administered by health rather than criminal-justice authorities). Incarceration would cease, criminal enforcement would redirect, and the extraction mechanisms (arrest, prosecution, incarceration, collateral consequences) would stop. The absence of criminalization would structurally transform the drug market, law enforcement institutions and funding, the status of current and future drug users, and the role of incarcerated populations in the labor and social system.
% FOUNDING_PROBLEM: Drug use creates public disorder (open-air drug markets, public intoxication, visible drug use and paraphernalia in shared spaces) and endangers third parties through drug-driven property crime and associated violence. Non-users experience threat, property loss, and community degradation associated with visible drug markets. Criminalization was adopted to deter use, disrupt markets, incapacitate users via incarceration, and protect non-users through legal threat and market disruption.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and law-and-order constituencies attest the founding problem is live and criminalization is necessary. However, medical authorities (American Medical Association, World Health Organization), epidemiological research (50+ years of prevalence data), and harm-reduction jurisdictions (Portugal, Switzerland, Canada) attesting from outside the enforcement beneficiary set report that: (1) drug use persists under criminalization at similar or rising rates; (2) property crime and public disorder blamed on drugs are often better explained by poverty and inequality; (3) criminalization itself creates harms (overdose deaths rising due to supply unpredictability, incarceration trauma, community distrust of institutions) that exceed harms it prevents; (4) alternative frameworks (decriminalization with treatment, harm reduction, legalization with regulation) achieve better outcomes for public safety, public health, and third-party protection. The empirical dispute over whether criminalization actually reduces drug use, disorder, or crime — the persistence of the founding problem — is one of the central contested facts in public policy.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and rising over the 1971–2024 span (from 0.45 to 0.81) because incarceration rates, collateral consequence scope, and carceral infrastructure have expanded dramatically while the founding problem (drug use, disorder) has not been solved by criminalization. The extraction rises not as a price for a coordination function that works but as institutional expansion of the enforcement apparatus itself — a rent-seeking dynamic. Suppression is very high (0.87) because persistence of the constraint depends entirely on active enforcement: absent police activity, incarceration, and collateral-consequence regimes, criminalization would not persist. Suppression requirement rises sharply 1971–2000 (from 0.68 to 0.84), indicating that the enforcement apparatus had to be built and hardened during the drug war escalation; post-2000 it plateaus, indicating the enforcement machinery has stabilized at a high level. Theater ratio rises consistently (0.35 to 0.62) because an increasing share of enforcement targets low-level possession and low-level dealers, not major traffickers — the publicly stated mission (protecting from major drug-driven crime) diverges from the actual distribution of enforcement (quantity-of-arrests metrics that incentivize low-level charges). The measured rising theater ratio models the growing rhetorical gap between the stated goal (protecting third parties) and the actual enforcement pattern (mass low-level criminalization). All measurements are authored on a single shared time grid (1971, 1985, 2000, 2010, 2020, 2024) so every metric has values at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The state enforcement apparatus and law-and-order constituencies, in their seat, perceive the constraint as a genuine coordination tool: an authority to establish and maintain deterrence, disrupt markets, and protect communities. From the powerless and trapped seats (criminalized users, low-income over-policed communities, incarcerated persons), the same constraint appears as enforcement apparatus expansion and extraction. From the moderate-power beneficiary seats (property-crime victims, disorder-affected communities), the constraint offers nominal protection via incapacitation but at the cost of intensified policing that stigmatizes and degrades their own communities. From the excluded observer seats (harm-reduction practitioners, medical authorities), the constraint's entire logic is defeated by empirical reality: criminalization does not reduce drug use, increases harms via overdose and disease transmission risk, and trades one set of harms (drug-driven crime) for another set (incarceration trauma, community distrust, intergenerational poverty). The engine should compute dramatically different types from different seats: the enforcement apparatus seat may compute as rope (coordination with net benefits from their position); the powerless and incarcerated seats compute as snare (pure extraction with trapped exits); the disorder-affected moderate seats compute as tangled rope (some coordination benefit mixed with disproportionate enforcement burden).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by stakeholder power and exit options. The state enforcement apparatus (institutional, arbitrage exit) has d near 0.0 (full beneficiary): they set the agenda, receive institutional expansion and legitimacy, control enforcement prioritization, and can exit into other enforcement domains. Drug users criminalized (powerless, trapped exit) have d near 1.0 (full target): criminalization itself is the extraction mechanism; they cannot exit legally or geographically without continuing exposure; the constraint operates entirely at their expense. Incarcerated drug offenders have d = 1.0 (perfect target): they are the extraction mechanism concentrated as incarceration. Low-income over-policed communities (powerless, trapped exit) have d ≈ 0.85–0.95 (strong target): they experience disproportionate enforcement and community degradation; exit is geographically constrained by poverty. Property-crime victims and disorder-affected communities (moderate power, constrained exit) have d ≈ 0.5–0.65 (mixed beneficiary/payer): they receive nominal protection but also incur policing burden; their exit is constrained by geography. Law-enforcement institutions (institutional, arbitrage exit) have d near 0.0–0.2 (beneficiary to ambiguous beneficiary): they collect expansion, funding, civil asset forfeiture; some institutional actors (prosecutors) may compute closer to 0.3 if they perceive reputational cost from mass incarceration. Harm-reduction advocates and medical authorities are excluded, not seated, so directionality does not apply to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate was to protect third parties from drug-related crime and social disorder through deterrence and market disruption. The founding_problem_status = contested because empirical research increasingly shows that criminalization has not achieved meaningful reduction in drug use (prevalence stable or rising), that drug-driven crime is often better explained by poverty than by drug markets themselves, and that criminalization's own harms (overdose risk from unpredictable supply, incarceration trauma, community distrust) are substantial. The theater ratio rise (0.35 to 0.62) indicates functional drift: an increasing share of enforcement is low-level quantity-of-arrests policing (incentivized by arrest metrics) rather than targeted market disruption. The disappearance_verdict = world_rearranges indicates the constraint is not a natural fact — it is sustained by active choice and enforcement, not by necessity. The combination (mandated to solve a now-disputed problem, enforced through mechanisms showing increasing functional drift, persists through pure enforcement not through problem-solving) indicates mandatrophy: the original mandate has outlived its credibility and its effectiveness, but the constraint persists because the enforcement institutions have become self-sustaining. The extraction (rising over 53 years) and suppression (high and stable) are consistent with a snare dressed in rope language: the coordination function is contested/dissolved, but the extraction mechanism is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_truth,
    'Does criminalization actually reduce drug use prevalence, or does it merely displace use geographically and shift consumption patterns without reducing net consumption?',
    'Cross-national comparison of drug prevalence trends in criminalized vs. decriminalized jurisdictions controlling for socioeconomic factors; time-series analysis of US drug prevalence before and after major enforcement escalations (1980s drug war, 2010s opioid enforcement).',
    'If criminalization does not reduce prevalence, the founding problem (drug use) persists despite the constraint, and the constraint''s function becomes maintenance of authority and extraction rather than problem-solving. Classification shifts from rope (solves coordination problem) toward snare (persists by extraction despite problem unsolved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_truth, empirical, 'Whether criminalization achieves its stated deterrent goal of reducing drug use.').

omega_variable(
    third_party_protection_mechanism,
    'Is the protection of third parties from drug-driven crime and disorder best achieved through criminalization of users, through treatment and harm reduction, through addressing poverty and inequality, or through some combination? Is criminalization a necessary condition for third-party protection?',
    'Comparative policy analysis across jurisdictions with different drug policy frameworks; longitudinal analysis of crime, overdose, public health, and social outcomes under different approaches; controlled policy experiments (e.g., supervised consumption sites, medication-assisted treatment expansion in otherwise criminalized contexts).',
    'If third-party protection is achievable or more effective under alternative frameworks, the criminalization constraint loses its sole justification. The ''protection'' benefit concentrates in the moderate-power beneficiary seats and may vanish or invert (if alternatives provide better protection at lower social cost). The constraint recomputes as pure extraction with nominal coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_protection_mechanism, empirical, 'Whether criminalization is necessary or effective for protecting third parties from drug-related harms.').

omega_variable(
    racial_disparity_mechanism,
    'Are racial disparities in drug enforcement (arrest, prosecution, incarceration rates 5–10x higher for Black/Latino communities despite similar prevalence) a product of enforcement discretion and bias, or do they reflect different rates of drug market activity (open-air markets more prevalent in low-income communities)?',
    'Analysis of arrest rates for drug use with vs. without market-structural controls; audit studies testing identical behavior across racial groups in police interactions; historical analysis of drug-schedule decisions (cocaine vs. crack disparities, opioid crisis response disparities).',
    'If disparities are primarily enforcement bias rather than market-structural differences, the constraint functions as a race-targeted extraction mechanism regardless of stated intent. Directionality for low-income and minority communities shifts further toward target (d approaching 1.0), and suppression mechanisms shift from legal penalty (prison) toward surveillance and control as primary functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_mechanism, empirical, 'Whether racial disparities in drug enforcement reflect bias or market structure.').

omega_variable(
    founding_problem_decomposition,
    'The stated founding problem (drug use, disorder, crime) is actually three claims: that drug use exists, that it causes crime and disorder, and that criminalization reduces all three. Which parts are true? If drug use persists but crime/disorder attributed to it are actually driven by poverty and inequality, is the founding problem solved or merely misdiagnosed?',
    'Econometric analysis decomposing drug-driven crime from poverty-driven crime; time-series analysis of drug use, incarceration, and crime rates under different policy regimes; longitudinal community studies tracking residents'' perception of disorder vs. actual crime rates.',
    'If the founding problem is primarily poverty-driven disorder falsely attributed to drugs, criminalization has solved the wrong problem and the actual coordination function collapses. The constraint becomes pure extraction framed in false-problem language. Classification reverts to snare with false-summit features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_decomposition, empirical, 'Whether the constraint solves its stated founding problem or misattributes different social harms to drug use.').

omega_variable(
    suppression_internalization_ambiguity,
    'The measured suppression (0.87) reflects active enforcement machinery (police, courts, prisons). But for drug users in over-policed communities, is suppression sustained by external enforcement or by internalized fear/shame that persists even absent enforcement activity? Do users exit when enforcement is removed, or do they remain trapped by internalized constraints?',
    'Post-decriminalization trajectory studies in jurisdictions that removed criminalization (Portugal, Switzerland, parts of Canada): do users remain isolated from treatment and community? Do they exhibit behavioral change only in enforcement presence, or do beliefs/shame perpetuate the constraint after formal enforcement removal?',
    'If suppression is primarily internalized (identity-fusion, shame, belief in criminality), the constraint becomes structurally more durable — removing enforcement does not remove the constraint because the target population has absorbed it. If suppression is primarily external, decriminalization paired with treatment and social reintegration would substantially reduce suppression. This affects whether the constraint can be reformed (external suppression, reformable) or whether cultural reparation is necessary (internalized suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether drug criminalization''s suppression mechanism is structural/external or internalized.').

omega_variable(
    kernel_reading_boundary,
    'Is the prohibition reading a live, internally coherent framework, or has it been substantially undermined by accumulating countervailing evidence (rising overdose deaths, stable prevalence, racial disparities, failed policy experiments) such that it is now a framework held primarily by institutional beneficiaries rather than by reasoned commitment to its core premises?',
    'Genealogical analysis of policy justifications over time: do prohibition advocates still claim deterrence works, or have they shifted to incapacitation and retribution as primary justifications (indicating the founding problem claim has been abandoned)? Survey data on whether law enforcement and policy communities still endorse the founding problem or acknowledge it as unsolved.',
    'If the prohibition reading''s core premises have been substantially undermined within its own tradition (law enforcement acknowledges deterrence doesn''t work but continues enforcement anyway), the reading reverts to pure authority-extraction dressed in outdated language. This would place it in the ''overridden'' status for axioms, indicating the reading''s own tradition has abandoned its founding logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the prohibition reading''s core premises remain coherent or have been abandoned by its practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1971, substance_control_authority__prohibition_reading, theater_ratio, 1971, 0.35).
narrative_ontology:measurement_basis(subs_tr_t1971, observed).
narrative_ontology:measurement(subs_tr_t1985, substance_control_authority__prohibition_reading, theater_ratio, 1985, 0.42).
narrative_ontology:measurement_basis(subs_tr_t1985, observed).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(subs_tr_t2000, observed).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement_basis(subs_tr_t2010, observed).
narrative_ontology:measurement(subs_tr_t2020, substance_control_authority__prohibition_reading, theater_ratio, 2020, 0.61).
narrative_ontology:measurement_basis(subs_tr_t2020, observed).
narrative_ontology:measurement(subs_tr_t2024, substance_control_authority__prohibition_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(subs_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t1971, substance_control_authority__prohibition_reading, base_extractiveness, 1971, 0.45).
narrative_ontology:measurement_basis(subs_be_t1971, observed).
narrative_ontology:measurement(subs_be_t1985, substance_control_authority__prohibition_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement_basis(subs_be_t1985, observed).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement_basis(subs_be_t2000, observed).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement_basis(subs_be_t2010, observed).
narrative_ontology:measurement(subs_be_t2020, substance_control_authority__prohibition_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement_basis(subs_be_t2020, observed).
narrative_ontology:measurement(subs_be_t2024, substance_control_authority__prohibition_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(subs_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1971, substance_control_authority__prohibition_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement_basis(subs_su_t1971, observed).
narrative_ontology:measurement(subs_su_t1985, substance_control_authority__prohibition_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement_basis(subs_su_t1985, observed).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement_basis(subs_su_t2000, observed).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement_basis(subs_su_t2010, observed).
narrative_ontology:measurement(subs_su_t2020, substance_control_authority__prohibition_reading, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement_basis(subs_su_t2020, observed).
narrative_ontology:measurement(subs_su_t2024, substance_control_authority__prohibition_reading, suppression_requirement, 2024, 0.87).
narrative_ontology:measurement_basis(subs_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, racial_disparity_in_criminal_enforcement).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, incarceration_industrial_complex).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, collateral_consequences_of_criminalization).

% DUAL FORMULATION NOTE:
% The substance_control_authority kernel decomposes into three constraint stories, each instantiating a different reading of the state's legitimate role. This story (prohibition_reading) models criminalization as deterrence/protection. The harm_reduction_reading models accepting drug use while minimizing health harms; the legalization_reading models regulation as legal commerce. Each reading has its own epsilon, its own beneficiary/victim structure, and its own type classification. The three stories are linked via network.affects_constraints to represent their structural relationships as competing framings of the same kernel. The prohibition reading influences (but does not foreclose) the other two by establishing criminalization as the institutional baseline that competing readings must work against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
