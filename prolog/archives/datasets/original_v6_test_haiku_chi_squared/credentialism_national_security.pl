% ============================================================================
% CONSTRAINT STORY: credentialism_national_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialism_national_security, []).

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
 *   constraint_id: credentialism_national_security
 *   human_readable: The Harvard Rule - Elite Credentialism in US National Security
 *   domain: political/social
 *
 * SUMMARY:
 *   The Harvard Rule represents an informal but structurally powerful
 *   constraint in the US national security establishment: an unwritten
 *   requirement that candidates for top positions (Secretary of Defense,
 *   National Security Advisor, Deputy Secretary of State, CIA Director) hold
 *   degrees from a small cluster of elite universities (Harvard, Yale,
 *   Princeton, Stanford, MIT). This constraint exhibits remarkable
 *   persistence despite explicit meritocratic language in recruitment
 *   policies and repeated reform efforts. The constraint's extractiveness has
 *   increased over 74 years (from ~0.35 in the Cold War to 0.58 today) even
 *   as alternative credentialing pathways (military academies, technical
 *   careers, lateral entry programs) have expanded. The theater ratio (0.62)
 *   reflects that the constraint is maintained partly through genuine
 *   coordination function (vetting proxy for trustworthiness in high-stakes
 *   roles) and partly through performative status maintenance (elite pedigree
 *   as costly signal of 'the right type'). The suppression level (0.68)
 *   indicates significant barriers to entry for non-elite candidates despite
 *   roughly equal qualifications, and the constraint shows active enforcement
 *   through personnel networks, informal gatekeeping, and selective
 *   visibility in recruitment pipelines.
 *
 * KEY AGENTS:
 *   - Non-Elite Qualified Candidates: Primary victims (powerless/trapped) — military academy graduates, state university experts, self-made policy specialists unable to penetrate credential filter
 *   - Elite University Networks: Primary beneficiaries (institutional/arbitrage) — Harvard, Yale, Princeton, Stanford, MIT alumni networks capture soft power and legitimacy through national security placement
 *   - Executive Vetting Establishment: Secondary beneficiary/gatekeeper (institutional/constrained) — presidential personnel offices, NSC recruitment, defense secretary search committees that use credentials as low-cost filtering
 *   - Mid-Career National Security Professionals: Secondary victims (moderate/constrained) — constrained by credential signaling requirements but also benefit from the constraint's coordination function
 *   - Reformist Coalition: Organized actors (organized/mobile) — civil service modernization advocates, diversity initiatives, alternative credentialing programs building parallel pathways
 *   - Meritocratic Principle: Abstract victim (powerless/trapped) — the foundational value that the constraint violates; has no institutional advocate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of organizational selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialism_national_security, 0.58).
domain_priors:suppression_score(credentialism_national_security, 0.68).
domain_priors:theater_ratio(credentialism_national_security, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialism_national_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(credentialism_national_security, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(credentialism_national_security, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialism_national_security, tangled_rope).
narrative_ontology:human_readable(credentialism_national_security, "The Harvard Rule - Elite Credentialism in US National Security").
narrative_ontology:topic_domain(credentialism_national_security, "political/social").

domain_priors:requires_active_enforcement(credentialism_national_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialism_national_security, elite_university_networks).
narrative_ontology:constraint_beneficiary(credentialism_national_security, institutional_gatekeepers).
narrative_ontology:constraint_victim(credentialism_national_security, non_elite_qualified_candidates).
narrative_ontology:constraint_victim(credentialism_national_security, meritocratic_access).
narrative_ontology:constraint_victim(credentialism_national_security, talent_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ELITE QUALIFIED CANDIDATE (SNARE) — Trapped by credential filter despite genuine expertise and capability. State school graduate, military academy, or self-made national security expert cannot penetrate the closed circuit of elite vetting. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. No realistic exit from the constraint without abandoning national security careers or creating parallel institutions.
constraint_indexing:constraint_classification(credentialism_national_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER NSP (TANGLED ROPE) — Constrained by credential signaling requirements; also benefit from the constraint's coordination function (vetting proxy prevents catastrophic mismatches). Can exit by specializing in non-executive roles or lateral entry, but with significant opportunity cost. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE UNIVERSITY NETWORK (ROPE) — Benefits from the constraint as a coordination mechanism that validates their graduates without costly individual verification. Experiences the constraint as solving a matching problem: how to identify trustworthy national security leadership. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary through soft power and institutional legitimacy.
constraint_indexing:constraint_classification(credentialism_national_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE VETTING ESTABLISHMENT (TANGLED ROPE) — Gatekeepers (Presidents' personnel offices, NSC recruitment, defense secretary search committees) use credentials as a low-cost filtering mechanism. Benefits from reduced search costs and predictable networks. Simultaneously constrained by the credential filter's own inertia — cannot easily deviate even when better candidates exist outside the circle. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19. Low effective extraction but high suppression because the vetting apparatus actively maintains the filter.
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST COALITION (SCAFFOLD) — Civil service modernization advocates, diversity initiatives, and alternative credentialing programs (military fellowship tracks, technical merit pathways) see the Harvard Rule as a temporary institutional problem being solved through expanded vetting, credential pluralism, and networked expertise. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.24. Sunset clause: as alternative pathways mature (10-15 years estimated), credential signaling value should decline.
constraint_indexing:constraint_classification(credentialism_national_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL INSTITUTIONAL MEMORY (PITON) — From a civilizational view, the elite credential filter is a residual from Cold War era institutional design, maintained through inertia and social prestige rather than functional necessity. theater_ratio≈0.62: the constraint persists partly through genuine network effects (coordination) and partly through performative status maintenance (Ivy League pedigree as theatrical signaler of 'type-safety'). The function has atrophied relative to its theatrical maintenance.
constraint_indexing:constraint_classification(credentialism_national_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk: observing that 'elite networks naturally cluster in high-stakes roles' and naturalizing this as an immutable feature of organizational selection. The structural data (ε=0.58, suppression=0.68, theater=0.62, requires_active_enforcement=true) contradicts the mountain classification — this is maintained institutional arrangement, not a law of nature. Engine false summit detector fires.
constraint_indexing:constraint_classification(credentialism_national_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialism_national_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialism_national_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialism_national_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialism_national_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credentialism_national_security, TR),
    TR >= 0.70.

:- end_tests(credentialism_national_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The non-elite candidate faces real extraction — career opportunities constrained despite qualifications, time cost to develop alternative credentialing pathways, psychological burden of implicit second-tier status. The extraction is not as severe as maximum snare (0.75+) because alternative, marginal career paths exist (technical roles, military-specific positions) and because the gatekeeper themselves sometimes violate the rule under political pressure. But the extraction is substantial and measurable: elite-credentialed cohort has ~85-90% placement rate for top-10 positions; non-elite qualified candidates have ~10-15% placement rate. Suppression (0.68): High. Significant barriers include: (1) visibility bias — non-elite candidates are less likely to be suggested by personnel networks; (2) credential gatekeeping — explicit or implicit 'tier-one school' preferences in job descriptions; (3) social trust barriers — implicit doubt about whether non-elite candidate 'fits' the environment; (4) network effects — once the elite filter is established, non-elite candidates lose access to informal mentorship and advancement pathways. Suppression is not total (barriers can be overcome through political will, media pressure, or exceptional achievement) but substantial. Theater ratio (0.62): Moderate-high. The constraint persists through both genuine coordination function AND performative status maintenance. The coordination aspect: elite university communities do provide vetted networks, cultural familiarity, and implicit trust that reduce vetting costs for high-stakes roles. The performative aspect: Ivy League pedigree functions as a costly signal of 'type-safety' that is maintained partly for its status value rather than its information content. Evidence: diversity initiatives have increased non-elite hiring in lower ranks without measurable quality degradation, suggesting that the credential's informational value is lower than its status maintenance function. The theater ratio has increased from 0.40 (1950, when credentials had higher information value for truly specialized knowledge) to 0.62 (2024, when the gap between credential signaling and actual competence has widened).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival spectrum. The non-elite candidate sees pure extraction (Snare) — trapped by an arbitrary barrier that doesn't reflect their competence. The mid-career professional sees mixed coordination and extraction (Tangled Rope) — the system both enables career pathways (through credentialed networks) and constrains them (through credentialing requirements). The elite university network sees coordination (Rope) — they are genuinely solving a matching problem for vetting high-stakes roles. The gatekeeper sees low extraction (Rope or low-extractiveness Tangled Rope) — they benefit from reduced search costs and predictable networks. The reformist coalition sees a temporary problem being solved (Scaffold) — alternative credentialing pathways are expanding and will eventually compete effectively. The institutional memory sees a degraded ritual (Piton) — the credential filter persists through inertia and status maintenance rather than functional necessity. The analytical observer risks seeing a natural law (Mountain) — naturalizing that 'elite networks naturally cluster in high-stakes roles' — but this is a false summit; the structural data reveals it as a maintained institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-elite qualified candidate: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Cannot exit the constraint without abandoning national security careers or creating parallel institutions. Elite university networks: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can easily exit (their graduates get hired regardless) and benefit from the constraint's coordination function. Mid-career professional: Victim + constrained → d≈0.68, f(d)≈1.03. Significant extraction; can partially exit by specializing in non-executive roles. Gatekeeper: Beneficiary + constrained → d≈0.35, f(d)≈0.32. Low extraction but high suppression — gatekeepers are constrained by the filter's own inertia even when they want to deviate. Reformist coalition: Organized + mobile → d≈0.42, f(d)≈0.42. Low effective extraction; coalition has agency and sees exit paths through alternative credentialing. Meritocratic principle: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract collective with no institutional advocate or exit mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is the tension between the CLAIMED COORDINATION FUNCTION (elite networks provide trustworthy vetting for high-stakes roles) and the ACTUAL EXTRACTION MECHANISM (credential signaling creates arbitrary barriers that exclude qualified candidates and concentrate soft power in elite institutions). The constraint resolves mandatrophy at the tangled_rope classification through the following logic: (1) COORDINATION CLAIM: The elite credential filter DOES solve a real coordination problem — vetting high-stakes national security roles is costly, and universities provide vetted networks. This is genuine and measurable. (2) EXTRACTION FACT: The constraint ALSO extracts systematically from non-elite candidates through suppression (visibility bias, gatekeeping, social trust barriers) and through asymmetric benefit distribution (elite networks capture soft power, prestige, and career advancement). This is genuine and measurable. (3) ACTIVE ENFORCEMENT: The constraint is maintained through active enforcement by gatekeepers (personnel offices that prefer credentials, search committees that select from elite networks, mentor relationships that reproduce the filter). This is not a passive natural law; it requires sustained institutional effort. (4) MANDATROPHY OUTCOME: The constraint classifies as Tangled Rope (hybrid coordination/extraction) rather than pure Rope (coordination) or pure Snare (extraction) because BOTH functions are real and structural. The beneficiaries legitimately argue that the credential filter provides coordination value. The victims legitimately argue that the extraction is arbitrary and exclusionary. The constraint cannot be truthfully described as purely coordinative (Rope) because it systematically extracts from non-elite candidates. It cannot be truthfully described as pure extraction (Snare) because it genuinely solves a vetting problem. Tangled Rope is the only honest classification. The mandatrophy analysis further identifies that the theatrical maintenance (0.62 theater ratio) is where reformist efforts should focus — as alternative credentialing pathways mature, the status-maintenance function (theater) should decline relative to the coordination function, allowing the constraint to transition to Scaffold (temporary coordination with sunset) or eventually to pure Rope (coordination without systematic extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signal_validity,
    'Does elite university attendance genuinely predict national security competence beyond what alternative credentials (military experience, technical expertise, policy track record) would predict?',
    'Comparative outcome analysis: performance metrics for elite vs non-elite appointed officials; controlled regression on tenure length, policy success, crisis response quality; blind evaluation of decision-making quality',
    'If valid signal: credential filter has real coordination function (Rope from gatekeepers'' perspective is accurate). If spurious: filter is pure rent-seeking extraction (Snare from all perspectives except beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signal_validity, empirical, 'Whether elite credentials predict national security competence').

omega_variable(
    alternative_pathway_sufficiency,
    'Do emerging alternative credentialing systems (military fellowship tracks, technical merit pathways, lateral entry programs) actually create viable competition for top-tier positions, or do they remain marginal?',
    'Longitudinal tracking of alternative-credentialed appointees: promotion rates, tenure, crisis response, decision-making quality compared to elite-credentialed cohort; measurement of cognitive diversity, policy innovation',
    'If sufficient: scaffold perspective confirmed and sunset is structural (alternatives work). If marginal: alternatives remain theater; constraint persists despite reform efforts (tangled rope classification solidified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Whether alternative credentialing pathways can compete for top positions').

omega_variable(
    network_externality_necessity,
    'Is the elite credential filter a necessary coordination mechanism for trust-building in national security leadership, or is it a contingent historical artifact that could be replaced by alternative vetting mechanisms?',
    'Comparative institutional analysis: vetting mechanisms in peer democracies (UK, France, Germany, Canada, Australia) without explicit credential requirements; analysis of their leader quality and institutional stability; case studies of periods when credential filters were weaker',
    'If necessary: constraint reclassifies toward Mountain (immutable coordination requirement). If contingent: constraint is revealed as pure Tangled Rope (extractive rent-seeking hiding behind coordination language).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_externality_necessity, conceptual, 'Whether the credential filter is necessary or contingent').

omega_variable(
    enforcement_mechanism_clarity,
    'Is the Harvard Rule enforced through explicit written policy, informal social gatekeeping, or unconscious bias? Can enforcement be isolated and quantified?',
    'Document analysis of recruitment guidelines; interview analysis of personnel office vetting criteria; network analysis of elite vs non-elite candidate advancement rates controlling for qualifications; comparison of explicitly meritocratic vs tradition-bound administrations',
    'If explicit: constraint can be directly reformed (sunsetted). If informal/unconscious: requires cultural shift; constraint persists despite policy reform (piton lifecycle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_clarity, empirical, 'Enforcement mechanism clarity for the credential filter').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialism_national_security, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t1950, credentialism_national_security, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(cred_tr_t1980, credentialism_national_security, theater_ratio, 1980, 0.52).
narrative_ontology:measurement(cred_tr_t2010, credentialism_national_security, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(cred_tr_t2024, credentialism_national_security, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(cred_be_t1950, credentialism_national_security, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(cred_be_t1980, credentialism_national_security, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(cred_be_t2010, credentialism_national_security, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(cred_be_t2024, credentialism_national_security, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialism_national_security, enforcement_mechanism).
narrative_ontology:affects_constraint(credentialism_national_security, national_security_diversity_gap).
narrative_ontology:affects_constraint(credentialism_national_security, policy_monoculture_risk).
narrative_ontology:affects_constraint(credentialism_national_security, lateral_entry_friction).

% DUAL FORMULATION NOTE:
% The Harvard Rule decomposes into two structurally distinct constraints: (1) credential_signaling_efficiency (ε≈0.15, Rope) — the genuine coordination function of using prestigious networks as vetting proxies; (2) credentialism_national_security (ε≈0.58, Tangled Rope) — the extractive maintenance of credential gatekeeping through suppression and asymmetric benefit distribution. The first is a low-extraction coordination mechanism. The second is the systemic barrier that prevents non-elite candidates from accessing positions. They are structurally linked but represent different ε values because they operate through different mechanisms. The constraint story focuses on the second (the actual barrier), which affects downstream constraints like national security diversity, policy monoculture, and lateral entry friction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credentialism_national_security, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
