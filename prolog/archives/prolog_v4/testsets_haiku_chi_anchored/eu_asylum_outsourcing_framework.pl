% ============================================================================
% CONSTRAINT STORY: eu_asylum_outsourcing_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_asylum_outsourcing_framework, []).

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
 *   constraint_id: eu_asylum_outsourcing_framework
 *   human_readable: EU Framework for Outsourcing Asylum Procedures to Third Countries
 *   domain: political/migration/international_law
 *
 * SUMMARY:
 *   The EU framework for outsourcing asylum procedures to third countries
 *   represents a structural transformation in how EU member states manage
 *   migration policy. By establishing legal authority to process asylum
 *   applications outside EU territory and jurisdiction, the framework creates
 *   a two-tier protection system: applicants processed within the EU retain
 *   access to European Court of Human Rights remedies and EU asylum law
 *   protections; applicants processed in third countries operate under
 *   host-country legal regimes with weaker safeguards and limited EU
 *   oversight. This constraint exhibits the structural signature of a snare:
 *   high extractiveness (0.68) reflecting that rejected applicants have no
 *   formal appeal route to EU courts, suppression (0.72) reflecting barriers
 *   to legal representation and information access in third countries, and
 *   theater (0.65) reflecting compliance performance metrics that measure
 *   processing speed rather than protection quality. The framework was
 *   politically framed as burden-sharing and coordination (facilitating EU
 *   member state cooperation on border security), but the structural data
 *   reveals asymmetric extraction: costs are borne by asylum seekers and
 *   international protection norms, while benefits concentrate in EU member
 *   state administrations and domestic political capital gained from reduced
 *   irregular migration. The constraint demonstrates mandatrophy resolution
 *   by distinguishing the coordination narrative (shared responsibility for
 *   refugee flows) from the extraction structure (transferring asylum
 *   processing to jurisdictions with lower protection standards). The theater
 *   ratio (0.65) indicates that compliance monitoring focuses on procedural
 *   performance (applications processed per month, rejection rates) rather
 *   than substantive protection outcomes (correctness of status
 *   determinations, refoulement avoidance). This creates theatrical
 *   compliance: high processing throughput demonstrating 'efficiency' masks
 *   systematic underprotection.
 *
 * KEY AGENTS:
 *   - Asylum seekers in third countries: Primary victim (powerless/trapped) — subject to accelerated processing without access to EU appellate remedies; trapped in host country without legal pathway to EU territory
 *   - International refugee protection regime: Structural victim (powerless/trapped) — abstract collective (1951 Convention principles, non-refoulement norm, right to fair hearing) cannot organize or exit; bears cost of systematic norm erosion
 *   - EU member states & border control administrations: Primary beneficiary (institutional/arbitrage) — gains political capital from reduced irregular migration to EU territory; transfers administrative burden and risk to third countries
 *   - Third-country host governments: Institutional victim-beneficiary hybrid (moderate/constrained) — receives EU aid/trade access (benefits) but assumes responsibility for asylum determinations under weaker legal regimes and bears security/demographic risks
 *   - EU human rights advocacy organizations: Organized challenger (organized/constrained) — maintains legal and advocacy campaigns against framework through courts and parliament; constrained by limited enforcement power over third-country actions
 *   - International legal reform movement: Organized alternative advocate (organized/mobile) — UNHCR, international NGOs, sympathetic states advancing in-EU processing and regional burden-sharing as sunset alternative to outsourcing model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_asylum_outsourcing_framework, 0.68).
domain_priors:suppression_score(eu_asylum_outsourcing_framework, 0.72).
domain_priors:theater_ratio(eu_asylum_outsourcing_framework, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, extractiveness, 0.68).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_asylum_outsourcing_framework, snare).
narrative_ontology:human_readable(eu_asylum_outsourcing_framework, "EU Framework for Outsourcing Asylum Procedures to Third Countries").
narrative_ontology:topic_domain(eu_asylum_outsourcing_framework, "political/migration/international_law").

domain_priors:requires_active_enforcement(eu_asylum_outsourcing_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, border_control_administrations).
narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, asylum_seekers_in_third_countries).
narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, international_refugee_protection_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER IN THIRD COUNTRY (SNARE) — Trapped in host country without access to EU territory or formal appeal channels. Bears full cost of accelerated processing, limited legal representation, and exposure to deportation under third-country agreements with EU member states. d≈0.96, f(d)≈1.42, σ=1.2 → χ≈1.16. Maximum extraction: no exit, no alternatives, full vulnerability.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL REFUGEE PROTECTION NORMS (SNARE) — The 1951 Refugee Convention principle of non-refoulement and right to asylum hearing is systematically undermined by outsourcing procedures. Abstract collective obligation (cannot organize, cannot exit) bears cost of norm erosion. d≈0.94, f(d)≈1.40, σ=1.0 → χ≈0.95. High extraction: system designed to bypass rights protections.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: EU MEMBER STATES & BORDER CONTROL ADMINISTRATIONS (ROPE) — Primary beneficiary. Experiences outsourcing as coordination mechanism: reduces internal asylum backlog, transfers administrative burden, creates buffer against domestic political pressure from anti-immigration constituencies. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.07. Net beneficiary. Framed as burden-sharing and resource efficiency (coordination narrative).
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THIRD-COUNTRY HOST GOVERNMENTS (TANGLED ROPE) — Constrained by economic dependence on EU aid/trade; receive financial compensation for hosting processing centers. Also benefit from EU technical assistance and legitimacy transfer. But bear costs of managing asylum populations, security risks, and international legal liability if refoulement occurs. d≈0.58, f(d)≈0.78, σ=0.9 → χ≈0.44. Hybrid: coordination (resource provision) + extraction (constraint on sovereignty).
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EU HUMAN RIGHTS ADVOCACY ORGANIZATIONS (PITON) — Organized actors formally challenging the framework through European Court of Human Rights, parliamentary inquiries, and media campaigns. But substantive enforcement is degraded: framework persists despite legal challenges, EU courts have limited enforcement power over third-country actions, and political will prioritizes security/border control over rights. Theater_ratio≈0.65 (procedural/legal theater vs actual protection). d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.47. Advocacy ritual maintained despite limited functional impact.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Observer might argue that border control and asylum processing are inherent properties of sovereign states; outsourcing is merely efficient administration of an immutable constraint. However, base properties (ε=0.68, suppression=0.72, theater=0.65) contradict mountain classification. This is a false summit: the constraint is wholly contingent on EU legal/political choices, not an inevitable feature of state sovereignty. Engine detects false natural law framing.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERNATIONAL LEGAL REFORM MOVEMENT (SCAFFOLD) — Global coalition (UNHCR, international NGOs, sympathetic states) advancing alternative frameworks: regional burden-sharing, in-EU processing with protection standards, refugee resettlement pathways. Sees outsourcing framework as transitional problem with sunset: as international norms strengthen and refugee advocacy gains political power, the outsourcing model becomes untenable. d≈0.42, f(d)≈0.41, σ=1.2 → χ≈0.20. Low effective extraction because mobile organized agents see exit path and alternative institutional design.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_asylum_outsourcing_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_asylum_outsourcing_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_asylum_outsourcing_framework, TR),
    TR >= 0.70.

:- end_tests(eu_asylum_outsourcing_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The framework systematically extracts asylum seekers' access to EU legal protections by relocating their status determination to third countries with weaker legal regimes and limited appeal mechanisms. The extraction is not absolute (some applicants eventually reach EU territory and obtain review), but the default pathway is designed to prevent EU entry and appellate access. The measurement trajectory (0.42 → 0.68 over 6 time units) reflects the framework's operational deployment: early phases involved pilot programs and bilateral agreements with weaker enforcement; later phases show full operationalization with higher rejection rates and lower appeal success. Suppression (0.72): High. Multiple suppression mechanisms: (1) information asymmetry — applicants in third countries have limited access to asylum law, precedents, and legal representation; (2) institutional capacity — third-country processing centers are staffed with personnel trained under different legal standards; (3) coercive context — applicants are physically outside EU territory and cannot access EU courts; (4) procedural barriers — no automatic EU appeal mechanism; (5) diplomatic vulnerability — third-country partners have incentive to maintain high rejection rates to justify EU aid/partnership. Theater ratio (0.65): Moderate-high. Compliance is measured through administrative metrics (processing speed, rejection rates, cost per application) rather than substantive protection outcomes (correctness of status determinations, alignment with 1951 Convention interpretation). EU member states report processing efficiency; third-country partners report security cooperation; human rights organizations report legal violations. The same constraint generates contradictory performance narratives depending on which metrics dominate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The EU member state perspective (Rope) frames the constraint as burden-sharing and coordination — reducing EU internal asylum backlogs, distributing responsibility across regions, enhancing border security through third-country partnerships. The framework is presented as efficient administration of an inherent state function. The asylum seeker perspective (Snare) experiences the same constraint as extraction: accelerated processing without legal representation, rejection without appeal, physical confinement to third country, no pathway to EU territory. The international protection regime perspective (Snare) sees the constraint as norm erosion: systematic undermining of non-refoulement, fair hearing, and asylum access principles. The third-country government perspective (Tangled Rope) experiences mixed extraction and coordination: EU funding and technical assistance (coordination benefit) coupled with political constraint (EU pressure to maintain high rejection rates). The human rights advocacy perspective (Piton) observes that legal challenges and procedural reviews persist, but substantive enforcement is degraded — the framework persists despite European Court of Human Rights criticism because political will prioritizes border control. The analytical observer (false Mountain) risks naturalizing the constraint as an inevitable feature of state sovereignty, but the structural data reveals contingency: outsourcing is a political choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers in third countries: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. International protection regime: Victim + trapped → d≈0.94, f(d)≈1.40. Maximum extraction (abstract collective). EU member states: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Third-country governments: Victim + constrained (economically dependent, but receive compensation) → d≈0.58, f(d)≈0.78. Significant extraction mediated by aid flows. EU human rights orgs: Organized + constrained (legal remedies exist but have low functional impact) → d≈0.50, f(d)≈0.65. Mixed experience: advocacy process vs limited enforcement. International legal reform movement: Organized + mobile (able to advocate alternatives, see path to sunset) → d≈0.42, f(d)≈0.41. Low effective extraction because organized agents have agency and perceive exit route.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The framework resolves mandatrophy by precisely distinguishing coordination function (burden-sharing narrative) from asymmetric extraction (protection access gap). The EU member state perspective genuinely experiences coordination: they are solving a legitimate collective action problem (managing transnational refugee flows) through institutional design. However, the coordination is achieved through asymmetric extraction: costs are transferred to powerless agents (asylum seekers, international norms) who cannot participate in the coordination game. This is the definition of Snare, not Rope. The mandatrophy would arise if the framework were classified as Rope (pure coordination with minimal extraction) based on the burden-sharing narrative alone. But the base properties (ε=0.68, suppression=0.72) reveal that extractiveness significantly exceeds pure coordination thresholds. The framework is not 'coordination that happens to have side effects' — it is 'extraction that is organized through coordination mechanisms.' The theater ratio (0.65) confirms this: procedural compliance (coordination metrics like processing speed) masks substantive extraction (protection access denial). By declaring beneficiaries (EU member states) and victims (asylum seekers, protection norms) explicitly, the JSON structure forces the distinction: Rope requires minimal extraction; this framework has massive extraction; therefore it is Snare. The false natural law perspective (Mountain) is defanged by the same structural clarity: outsourcing is contingent on EU legal choices, not inevitable state function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refoulement_verification_gap,
    'Can EU member states or third-country partners verify compliance with non-refoulement obligations in outsourced asylum procedures without direct access to applicant testimony and appeal processes?',
    'Independent monitoring of third-country processing centers; cross-reference deportation rates against applicant demographics; analysis of appeal success rates in third countries vs EU-processed cases',
    'If verification fails: constraint is snare (extraction masked as legal compliance). If verification succeeds: constraint could degrade to rope (coordination with protection safeguards).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refoulement_verification_gap, empirical, 'Whether non-refoulement compliance can be verified in third-country processing').

omega_variable(
    sovereign_legitimacy_transfer,
    'Does the EU framework legitimize or delegitimize third-country governments that enforce asylum rejections? Does legitimacy transfer change those governments'' willingness to cooperate on refugee protection?',
    'Analysis of third-country domestic political narratives; correlation between EU partnership and hardening of asylum policies; interviews with third-country officials on framing of EU collaboration',
    'If legitimacy empowers hardline factions: framework increases refoulement risk globally (strengthens snare). If legitimacy enables moderate reformers: framework could enable protection improvements (softens snare toward tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_legitimacy_transfer, conceptual, 'Whether EU partnership legitimizes hardline asylum policies in third countries').

omega_variable(
    supply_chain_circularity,
    'Does outsourcing asylum processing to third countries create feedback loops where rejected applicants reinforce the refugee populations those same third countries host, increasing pressure for mass deportation?',
    'Longitudinal tracking of refugee population dynamics in third-country hosts; correlation analysis between EU rejections and detention/deportation rates in host countries; interviews with humanitarian organizations on displacement cycles',
    'If circularity confirmed: framework is inherently unstable and extractive (strong snare). If linear/stable: framework could be sustainable as tangled rope with compensation mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_circularity, empirical, 'Whether outsourcing creates destabilizing feedback in third-country refugee populations').

omega_variable(
    mandate_interpretation_divergence,
    'Do third-country governments interpret EU asylum law requirements (credibility assessment, persecution grounds) identically to EU courts, or does interpretive divergence create systematic rejection bias?',
    'Comparative analysis of asylum grant rates for identical case profiles in EU vs third-country processing; expert review of third-country interpretation standards; legal harmonization assessments',
    'If systematic divergence confirmed: framework encodes extraction through legal variability (snare). If harmonized interpretation: framework reduces to coordination problem (rope/tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretation_divergence, empirical, 'Whether third-country legal interpretation diverges from EU asylum law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_asylum_outsourcing_framework, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euasylum_tr_t0, eu_asylum_outsourcing_framework, theater_ratio, 0, 0.48).
narrative_ontology:measurement(euasylum_tr_t3, eu_asylum_outsourcing_framework, theater_ratio, 3, 0.58).
narrative_ontology:measurement(euasylum_tr_t6, eu_asylum_outsourcing_framework, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(euasylum_be_t0, eu_asylum_outsourcing_framework, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(euasylum_be_t3, eu_asylum_outsourcing_framework, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(euasylum_be_t6, eu_asylum_outsourcing_framework, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_asylum_outsourcing_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, dublin_regulation_enforcement).
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, safe_third_country_doctrine).
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, eu_border_externalization).

% DUAL FORMULATION NOTE:
% The EU asylum outsourcing framework is downstream of the Dublin regulation (responsibility for asylum application processing in first EU entry state) and upstream of bilateral safe third country agreements. The framework represents a qualitative expansion of the externalization strategy: from Dublin's geographic allocation (first EU state processes) to territorial outsourcing (processing outside EU entirely). Distinct ε values: Dublin regulation is coordination-heavy (ε≈0.35); outsourcing framework is extraction-heavy (ε≈0.68). The family relationship reflects increasing severity of externalization over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_asylum_outsourcing_framework, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
