% ============================================================================
% CONSTRAINT STORY: hhs_fetal_tissue_research_ban_2019
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hhs_fetal_tissue_research_ban_2019, []).

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
 *   constraint_id: hhs_fetal_tissue_research_ban_2019
 *   human_readable: 2019 HHS Ban on Fetal Tissue Research Funding
 *   domain: political_economy/biomedical_research/regulatory
 *
 * SUMMARY:
 *   The June 2019 HHS ban on federal funding for fetal tissue research
 *   represents a constraint that simultaneously exhibits pure extraction
 *   (from the perspective of dependent researchers and patients), political
 *   theater (from the regulatory system's own perspective), coordination
 *   failure (from the research sector's perspective), and temporary
 *   coordination challenge with technological sunset (from the alternative
 *   research technology coalition's perspective). The ban was issued by HHS
 *   Secretary Alex Azar under political pressure from anti-abortion
 *   coalitions and the Trump administration, formalizing and extending prior
 *   de facto restrictions on fetal tissue research funding that had existed
 *   since the 1988 moratorium. The constraint creates asymmetric costs:
 *   researchers lose funding access and career mobility is restricted;
 *   patients lose access to regenerative medicine therapies in development;
 *   institutions lose research capacity and competitive standing. The
 *   beneficiary appears to be the anti-abortion coalition, but this
 *   relationship is sustained by coercive enforcement rather than genuine
 *   coordination. The ban's extractiveness is elevated by the fact that it
 *   suppresses a specific research modality (fetal tissue) while allowing
 *   ideologically preferred alternatives (iPSCs) to develop without
 *   equivalent restriction—creating a regulatory advantage for alternative
 *   technologies that may be less scientifically mature. The theater ratio is
 *   moderate (0.55) because the ban formalizes prior practice and performs
 *   political signaling (demonstrating commitment to anti-abortion
 *   constituencies) while actual research suppression occurs through existing
 *   funding mechanisms.
 *
 * KEY AGENTS:
 *   - Disease Patient Populations: Primary victims (powerless/trapped) — dependent on regenerative medicine breakthroughs; no alternative pathway; cannot influence policy
 *   - Regenerative Medicine Researchers: Secondary victims (moderate/constrained) — face career restriction and funding loss; can migrate internationally but at high cost
 *   - Anti-Abortion Coalition: Institutional beneficiaries (powerful/mobile) — exercise regulatory capture; benefit from policy concessions without bearing research costs
 *   - Research Institutions (Universities, Medical Centers): Mixed position (organized/constrained) — bear compliance costs but receive coordination benefit from unified regulatory clarity
 *   - Alternative Research Technology Coalition: Organized actors (organized/mobile) — benefit from technology-driven exit pathway; see ban as temporary constraint with sunset
 *   - HHS Regulatory System: Institutional maintainer (institutional/arbitrage) — enforces ban through performative theater; sees own process as degraded
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing political choice as immutable ethical principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, 0.62).
domain_priors:suppression_score(hhs_fetal_tissue_research_ban_2019, 0.68).
domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, extractiveness, 0.62).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hhs_fetal_tissue_research_ban_2019, snare).
narrative_ontology:human_readable(hhs_fetal_tissue_research_ban_2019, "2019 HHS Ban on Fetal Tissue Research Funding").
narrative_ontology:topic_domain(hhs_fetal_tissue_research_ban_2019, "political_economy/biomedical_research/regulatory").

domain_priors:requires_active_enforcement(hhs_fetal_tissue_research_ban_2019).

% --- Structural relationships ---
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, regenerative_medicine_researchers).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, disease_patient_populations).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, fetal_tissue_research_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISEASE PATIENT POPULATIONS (SNARE) — Trapped within the regulatory boundary with no exit. Patients dependent on regenerative medicine breakthroughs enabled by fetal tissue research have no alternative pathway and cannot influence policy. Bear full cost of research restriction through delayed therapeutic development. Maximum extraction: denied access to potentially beneficial treatments.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGENERATIVE MEDICINE RESEARCHERS (SNARE) — Constrained but not trapped. Can relocate to other nations (Canada, UK, EU maintain fetal tissue research programs), but relocation costs are high: loss of institutional affiliation, NIH funding ineligibility, emigration barriers. Career development severely compromised within U.S. system. High extraction with constrained exit.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-ABORTION COALITION BENEFICIARIES (SNARE) — Powerful institutional actors (religious organizations, anti-abortion advocacy groups, right-to-life coalitions) benefit from the ban through regulatory capture of HHS policy. Mobile exit options: can shift focus to state-level restrictions, clinical trial bans, private funding restrictions. For this perspective, the ban functions as successful rent-seeking — extracting policy concessions from the research sector without bearing costs. High power, mobile exit, but perversely classified as snare because the underlying beneficiary relationship is contingent on coercive enforcement, not genuine coordination.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL RESEARCH SECTOR (TANGLED ROPE) — Universities, medical centers, and research institutions face mixed incentives. The ban extracts compliance costs (loss of NIH funding, institutional research restrictions) but also provides coordination benefit: unified regulatory clarity, simplified compliance infrastructure, protection from political attacks on institutional legitimacy. Organized actors can negotiate exemptions (some institutions maintain embryonic stem cell research via alternative funding). Constrained exit: cannot easily relocate institutional infrastructure abroad. Mixed extraction and coordination.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE RESEARCH TECHNOLOGY COALITION (SCAFFOLD) — Organized actors promoting induced pluripotent stem cells (iPSCs), organoids, and computational modeling as fetal tissue alternatives experience the ban as temporary coordination failure with sunset logic. iPSC technology maturation (2006 onwards) makes fetal tissue progressively less critical. The ban accelerates investment in alternatives through policy-driven innovation. Organized exit options through technology development. Suppression is real but declining as technology improves. Sunset occurs when iPSCs achieve functional parity with fetal tissue for all major research applications (estimated 5-10 years).
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HHS REGULATORY SYSTEM (PITON) — The ban itself is largely performative theater. HHS already prohibited federal funding for fetal tissue research de facto through prior restrictions (1988 moratorium, 1993 reinstatement conditional frameworks). The 2019 ban formalizes existing practice with heightened rhetoric and enforcement theater. The regulatory apparatus sees its own process as degraded: the ban's primary function is political signaling rather than functional research governance. Maintained through institutional inertia and political coalition pressure, not genuine research oversight. Theater ratio (0.55) reflects the gap between performative policy announcement and actual research impact (alternative pathways already existed).
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — A risk perspective: some actors frame the ban as emerging from immutable ethical principles (sanctity of life, protection of embryos) that are universal natural law, not contingent political choice. This perspective falsely treats a contested political decision as a natural law limit. The engine's false summit detector will identify this as naturalization of what is structurally a political extraction mechanism enforced through regulatory capture.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hhs_fetal_tissue_research_ban_2019_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, TR),
    TR >= 0.70.

:- end_tests(hhs_fetal_tissue_research_ban_2019_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. The ban extracts research capacity and therapeutic opportunity from the research community and patient populations. However, the extraction is not maximal because: (1) alternative technologies (iPSCs) provide partial substitution pathways that reduce true suppression, (2) international arbitrage is available (researchers can relocate, though at significant cost), and (3) institutional workarounds exist (non-federal funding streams, indirect methodologies). The measurement trajectory (0.48 → 0.62) reflects increasing effective extraction as researchers discover the depth of the restriction and iPSC alternatives prove slower to mature than initially hoped. Suppression (0.68): High. The ban eliminates federal funding pathways (the primary source for U.S. biomedical research), creates reputational costs for institutions engaging in the research, and induces compliance theater. However, suppression is not total because private funding and international collaboration remain available. Theater ratio (0.55): Moderate. The ban performs political signaling (demonstrating executive commitment to anti-abortion constituencies) and formalizes prior practice, but it also creates genuine restrictions on federal funding flows. The theater element emerges from the fact that de facto fetal tissue restrictions existed since 1988 — the 2019 ban's novelty is primarily rhetorical formalization and enforcement theater, not substantive policy change.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. Disease patients and researchers see pure extraction (Snare) — the ban suppresses beneficial research with no offsetting benefit to their position. The anti-abortion coalition sees coordination success (Rope or better) — the ban realizes their policy objectives with minimal cost to their interests. Research institutions see mixed coordination and extraction (Tangled Rope) — they bear compliance costs but gain regulatory clarity and institutional protection. The alternative technology coalition sees temporary coordination failure with sunset (Scaffold) — the ban accelerates iPSC development, which will eventually make fetal tissue obsolete, rendering the ban irrelevant. The HHS regulatory system sees its own process as degraded ritual (Piton) — peer review of research funding has been replaced by political vetting; the regulatory apparatus recognizes its own loss of functional capacity. The analytical observer risks seeing immutable ethical principle (Mountain) — natural law framing of embryo protection — but structural analysis reveals this as naturalization of contingent political choice. The perspectival gap is fundamental: from the victim's position, this is catastrophic suppression; from the beneficiary's position, it is routine policy success; from the institutional position, it is manageable coordination framework; from the technological innovation position, it is temporary market condition; from the regulatory position, it is performative theater; from the philosophical position, it risks becoming false natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives, revealing the constraint's asymmetric extraction structure. Disease patient populations (powerless/trapped) derive maximum d ≈ 0.95, experiencing the full weight of suppression with zero exit options — they cannot choose to conduct research, cannot migrate, cannot opt out of the constraint. Regenerative medicine researchers (moderate/constrained) derive d ≈ 0.70, experiencing high extraction but with constrained exit to international venues. Anti-abortion coalition beneficiaries derive d ≈ 0.15 (they are beneficiaries with mobile exit), experiencing the constraint as low-cost coordination: they mobilized to influence policy, achieved their objective, and can shift focus if political conditions change. Research institutions (organized/constrained) derive d ≈ 0.55, experiencing mixed extraction (compliance costs, funding loss) and coordination benefit (regulatory clarity, unified policy landscape). The analytical observer derives d ≈ 0.72 under the assumption that observation of regulatory capture entails partial victimization (being forced to acknowledge the extraction mechanism). The directionality pipeline feeds into f(d) and thence to χ = ε × f(d) × σ(S), producing different experienced extractiveness for each agent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through perspectival decomposition. The question 'Is this a Snare or Rope?' has no single answer — it is a Snare from the victim's perspective (powerless/trapped), a Rope from the beneficiary's perspective (powerful/mobile), Tangled Rope from the institutional perspective, Scaffold from the technological coalition, Piton from the regulatory system. The mandatrophy is not 'what is the true type?' but 'whose perspective are we adopting?' The falsity in the analytical observer's mountain classification is critical: the constraint is not an immutable law of ethics but a contingent political choice enforced through regulatory capture. The presheaf of perspectives reveals that the beneficiary has successfully naturalized a political extraction mechanism by framing it as ethical necessity. Mandatrophy resolution confirms: (1) the primary classification (Snare) is correct from the victim's structural position, (2) the alternative classifications are legitimate from other positions but do not erase the extraction from the victim's position, (3) the mountain classification is a false summit — the constraint is politics and regulation, not natural law, and (4) the sustainability of the constraint depends on the beneficiary's ability to maintain enforcement and the victims' inability to exit or organize counter-coalitions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ipscs_functional_parity,
    'Do induced pluripotent stem cells achieve functional parity with fetal tissue for all critical regenerative medicine applications?',
    'Longitudinal tracking of therapeutic development timelines: iPSC-derived treatments vs fetal tissue-derived treatments; comparative efficacy data; clinical trial success rates',
    'If parity achieved: ban becomes effectively obsolete through technology substitution (scaffold sunset realized). If parity unachievable: fetal tissue remains uniquely valuable and ban perpetuates permanent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ipscs_functional_parity, empirical, 'Whether iPSCs achieve functional parity with fetal tissue in regenerative medicine').

omega_variable(
    international_research_migration,
    'What fraction of high-value fetal tissue research projects migrated to international venues (Canada, UK, EU) following the 2019 ban?',
    'Publication venue analysis: longitudinal tracking of fetal tissue research authorship by institutional affiliation; grant funding flows to international institutions; researcher visa/emigration data',
    'If high migration (>30%): ban extracts U.S. research capacity but doesn''t suppress research globally (global commons perspective). If low migration (<10%): ban successfully suppresses research even through international arbitrage (extraction is genuinely coercive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_research_migration, empirical, 'Degree of research migration to international jurisdictions').

omega_variable(
    therapeutic_delay_quantification,
    'Can therapeutic development delays caused by the ban be quantified independently from other factors (COVID-19 disruption, funding cycles, technical barriers)?',
    'Comparative analysis: disease-specific therapeutic timeline projections (pre-ban); actual development timelines (post-ban); counterfactual modeling using disease domains without fetal tissue dependence',
    'If delays quantifiable: victim extraction can be measured precisely (time cost to patient populations). If delays confounded with other factors: victim cost remains uncertain, enabling political denial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_delay_quantification, empirical, 'Quantifiability of therapeutic delays attributable to the ban').

omega_variable(
    anti_abortion_coalition_unity,
    'Is the anti-abortion coalition unified on the fetal tissue ban, or does it contain internal factions with different objectives (embryo protection vs research suppression vs political signaling)?',
    'Institutional analysis of coalition positions: tracking internal disputes, statement evolution, tactical divergence on enforcement mechanisms',
    'If genuinely unified: the ban reflects stable coalition preferences. If factionally divided: the ban may be unstable political equilibrium vulnerable to defection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_abortion_coalition_unity, conceptual, 'Coherence and unity of the anti-abortion coalition backing the ban').

omega_variable(
    enforcement_efficacy,
    'Does HHS enforcement of the fetal tissue funding restriction actually prevent federal funding flows to fetal tissue research, or do institutional workarounds (indirect funding, non-federal revenue sources, definitional gaming) substantially circumvent the ban?',
    'Audit of actual research funding flows: NIH grant abstracts pre/post-ban; institutional financial disclosure; detection of definitional ambiguities in ''fetal tissue'' (e.g., secondary analysis of tissue, donated tissue classifications)',
    'If enforcement is effective: ban actually suppresses research (Snare classification holds). If enforcement leaky: ban is primarily performative theater (Piton classification stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy, empirical, 'Actual enforcement efficacy of the fetal tissue funding restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hhs_fetal_tissue_research_ban_2019, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hhs_ftb_tr_t0, hhs_fetal_tissue_research_ban_2019, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hhs_ftb_tr_t3, hhs_fetal_tissue_research_ban_2019, theater_ratio, 3, 0.48).
narrative_ontology:measurement(hhs_ftb_tr_t6, hhs_fetal_tissue_research_ban_2019, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(hhs_ftb_be_t0, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hhs_ftb_be_t3, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(hhs_ftb_be_t6, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hhs_fetal_tissue_research_ban_2019, enforcement_mechanism).
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, stem_cell_research_funding_restrictions).
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, embryonic_research_regulatory_capture).

% DUAL FORMULATION NOTE:
% The 2019 HHS ban decomposes into two structurally distinct constraints: (1) the de facto restriction mechanism (existing since 1988, formalizing in 2019, ε ≈ 0.48, lower epistemic confidence), and (2) the regulatory capture and enforcement theater (the political choice to formalize and amplify the restriction, ε ≈ 0.62, higher epistemic confidence that the process is extractive). The measurements track the progression from low-intensity restriction to high-intensity formalized ban. Upstream constraint: stem_cell_research_funding_restrictions (the general category of biomedical research restrictions that include fetal tissue bans). Downstream constraints: embryonic_research_regulatory_capture (how political coalitions have captured embryo protection policy), patient_therapeutic_access_suppression (how restriction mechanisms delay therapeutic development).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hhs_fetal_tissue_research_ban_2019, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
