% ============================================================================
% CONSTRAINT STORY: biosecurity_norm_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biosecurity_norm_formation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biosecurity_norm_formation
 *   human_readable: Biosecurity Norm Formation as Coordination and Extraction
 *   domain: biosecurity/governance/institutional
 *
 * SUMMARY:
 *   Biosecurity norm formation emerged post-2001 and accelerated
 *   post-COVID-19 as a global governance mechanism to reduce pandemic and
 *   bioweapon risks. The structural constraint appears as legitimate
 *   coordination (reducing shared biological hazards) from institutional
 *   perspectives but as extraction and surveillance from powerless researcher
 *   perspectives. The constraint exhibits tangled rope dynamics: genuine
 *   coordination function (shared interest in pandemic risk reduction)
 *   combined with asymmetric enforcement where developed nations set norms
 *   and developing nation researchers bear compliance costs. Theater ratio
 *   has increased as academic publishing institutions maintain performative
 *   biosecurity review processes (journals assessing dual-use risk despite
 *   lacking security expertise) while actual risk evaluation occurs offline
 *   in security agencies. The constraint's tension reflects an underlying
 *   legitimacy gap: developing nations experience biosecurity norms as
 *   hegemonic control; developed nations experience them as necessary commons
 *   governance. Norm enforcement divergence — discretionary enforcement for
 *   developed-nation research, strict enforcement elsewhere — reveals
 *   extractive machinery beneath coordination framing.
 *
 * KEY AGENTS:
 *   - Developed Nation Biosecurity Institutions: Primary beneficiaries (institutional/arbitrage) — set norms, control enforcement discretion, maintain research autonomy
 *   - Developing Nation Researchers: Primary victims (powerless/trapped) — dependent on developed-nation approval for publications, funding, partnerships; asymmetric compliance burden
 *   - Dual-Use Field Researchers: Secondary victims (moderate/constrained) — benefit from reduced pandemic risk but bear publication delays, material access restrictions, compliance overhead
 *   - Biosecurity Capacity-Building Coalition: Organized agents (organized/mobile) — NGOs and international bodies building research infrastructure with sunset logic (capacity targets)
 *   - Academic Publishing System: Institutional gatekeeper (institutional/arbitrage) — maintains performative biosecurity review ritual with low functional security capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional inequalities as inherent research capacity differences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biosecurity_norm_formation, 0.58).
domain_priors:suppression_score(biosecurity_norm_formation, 0.65).
domain_priors:theater_ratio(biosecurity_norm_formation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biosecurity_norm_formation, extractiveness, 0.58).
narrative_ontology:constraint_metric(biosecurity_norm_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(biosecurity_norm_formation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biosecurity_norm_formation, tangled_rope).
narrative_ontology:human_readable(biosecurity_norm_formation, "Biosecurity Norm Formation as Coordination and Extraction").
narrative_ontology:topic_domain(biosecurity_norm_formation, "biosecurity/governance/institutional").

domain_priors:requires_active_enforcement(biosecurity_norm_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biosecurity_norm_formation, institutional_biosecurity_leadership).
narrative_ontology:constraint_beneficiary(biosecurity_norm_formation, developed_nation_governments).
narrative_ontology:constraint_victim(biosecurity_norm_formation, biological_research_autonomy).
narrative_ontology:constraint_victim(biosecurity_norm_formation, developing_nation_researchers).
narrative_ontology:constraint_victim(biosecurity_norm_formation, dual_use_fields).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED RESEARCHER (SNARE) — Lacks political voice to negotiate biosecurity norms. Faces complete dependency on norm-setting countries for research funding, publication access, and training partnerships. Exit is impossible: not participating means research isolation and career exclusion. Maximum extraction experienced through asymmetric access, compliance burdens, and capacity constraints.
constraint_indexing:constraint_classification(biosecurity_norm_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DUAL-USE RESEARCHER (TANGLED ROPE) — Benefits from biosecurity norms that reduce pandemic risk and enhance research credibility. Also bears costs: compliance overhead, publication delays, access restrictions to materials and data. Mixed experience — genuine coordination function (shared risk reduction) alongside asymmetric extraction (compliance burden concentrated on individual researchers).
constraint_indexing:constraint_classification(biosecurity_norm_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPED NATION INSTITUTIONS (ROPE) — Set and enforce norms; experience pure coordination benefit. Can arbitrage by implementing stricter standards domestically while advocating looser international rules. Low extraction cost relative to benefit — control the norm-setting process itself. Net beneficiary from institutional position.
constraint_indexing:constraint_classification(biosecurity_norm_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPACITY-BUILDING COALITION (SCAFFOLD) — NGOs and international bodies (ABSA, CDC, Wellcome Trust) building biosecurity infrastructure globally. See current norm asymmetry as temporary — generational investment in research infrastructure, training, and funding in developing nations will create symmetric access and reduce extractive dynamics. Sunset logic: as global research capacity equalizes, asymmetric norm enforcement becomes less tenable. Organized agents with exit paths (can shift funding and partnership focus) and explicit sunset (capacity targets, timeline-bound initiatives).
constraint_indexing:constraint_classification(biosecurity_norm_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC PUBLISHING (PITON) — Biosecurity review in journals is largely performative theater. Editors and reviewers assess manuscript novelty and risk framing but lack domain expertise to verify actual dual-use hazard levels. The review ritual persists through institutional inertia — journals maintain biosecurity assessment procedures despite low functional verification capacity. Theater ratio high because actual risk evaluation happens in security agencies offline, not in peer review. Publishing system sees its own biosecurity role as degraded (necessary appearance but limited real function).
constraint_indexing:constraint_classification(biosecurity_norm_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational scope, biological research knowledge asymmetries appear natural and unchangeable: some nations will always have superior research capacity, equipment access, and security infrastructure. Dual-use hazards are inherent to biology as a field — cannot be negotiated away. This perspective risks naturalizing contingent institutional inequalities (unequal research infrastructure, historical capacity gaps) as immutable features of the scientific landscape. Engine flags as false summit: the apparent 'naturalness' is actually institutional and historically contingent.
constraint_indexing:constraint_classification(biosecurity_norm_formation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biosecurity_norm_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biosecurity_norm_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biosecurity_norm_formation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biosecurity_norm_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biosecurity_norm_formation, TR),
    TR >= 0.70.

:- end_tests(biosecurity_norm_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint captures genuine value asymmetry: developing nation researchers lose publication opportunities, face longer review timelines, and absorb compliance costs. Developed-nation researchers retain discretionary enforcement exemptions. The extraction is real but not total — some dual-use knowledge is genuinely restricted (gain-of-function variants, weaponizable pathogens), justifying some coordination costs. The intermediate value reflects that much of the extraction is channeled through institutional dependency rather than direct coercion. Suppression (0.65): High. Developing nation researchers face multiple exit barriers: no alternative publication venues with equivalent prestige, research funding concentrated in developed nations, training partnerships requiring normative compliance, career advancement dependent on international visibility. Exit costs are severe but not total — some regional research networks exist. Theater ratio (0.68): High. Academic publishing biosecurity review is substantially performative. Journal editors and peer reviewers assess dual-use risk primarily through narrative risk framing (does the paper 'sound dangerous'?) rather than through substantive security evaluation. Actual security assessment happens offline in government agencies and classified channels. The theatrical nature has increased as dual-use definitions have expanded — more publications require biosecurity review despite reviewers' limited capacity to evaluate actual hazards. Academic publishing maintains the theater because the alternative (removing biosecurity assessment) appears riskier from a liability perspective, even though it serves limited security function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence based on structural position. Developed-nation institutions see rope (coordination solving genuine collective action problem of pandemic risk). Capacity-building coalitions see scaffold with sunset (temporary asymmetry being rectified through infrastructure investment). Publishing systems see piton (maintaining degraded ritual for institutional legitimacy). Dual-use researchers see tangled rope (genuine coordination benefit mixed with significant extraction costs). Developing-nation researchers see snare (trapped in compliance with no exit, bearing costs of asymmetric norm enforcement). The analytical observer at civilizational scope risks seeing mountain (natural research capacity inequality) but the structural data reveals this as false summit — the inequality is institutional and historically contingent, not natural. The perspectival gap is largest between the beneficiary (institutional/arbitrage) and the victim (powerless/trapped) positions: the same norm enforcement appears as legitimate governance to one and as coercive control to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to norm enforcement. Developed-nation institutions are beneficiaries with arbitrage options (d ≈ 0.10) — they control norm-setting and can exempt their own research. Developing-nation researchers are victims with trapped exit (d ≈ 0.92) — they cannot exit norm compliance without career exclusion. Dual-use researchers are mixed (d ≈ 0.60) — they experience coordination benefits (reduced pandemic risk) and extraction costs (compliance burden) roughly equally. The capacity-building coalition (d ≈ 0.40) experiences constrained exit with mobile options (can shift funding allocation). The publishing system (d ≈ 0.15) benefits from norm maintenance through institutional necessity (must appear to assess security risk). The analytical observer (d ≈ 0.72) is attempting to observe the system from outside but risks naturalizing the extraction as inherent research asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing how coordination and extraction are empirically inseparable in norm formation. The genuinely coordinated element (pandemic risk reduction) cannot be disaggregated from the asymmetric enforcement (which nations can exempt their research). The mandatrophy appears as a false choice between 'this is pure coordination' and 'this is pure extraction' — the constraint is structurally tangled rope because both functions are real and both emerge from the same enforcement mechanism. The classification prevents mislabeling the constraint as rope (which would hide the extraction) or as snare (which would deny the genuine coordination function). The resolution is to measure from multiple perspectives and note that the constraint's classification type IS sensitive to observer position in ways that mandate the indexed tuple. The theater ratio increase (0.42→0.68) over the interval indicates Goodhart drift: as compliance burden increased, performative theater increased as a substitute for genuine security function, suggesting the constraint may be trending toward snare rather than remaining tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_legitimacy_asymmetry,
    'Are biosecurity norms perceived as legitimate global commons governance or as hegemonic control by developed nations?',
    'Survey analysis of norm acceptance across national research communities; comparison of stated norm rationale vs. experienced norm burden by nation and research field',
    'If legitimacy high: norms function as rope (coordination). If legitimacy low: norms function as snare (extraction masquerading as governance). Legitimacy gap correlates with extraction rate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_legitimacy_asymmetry, empirical, 'Whether biosecurity norms are perceived as legitimate governance or hegemonic extraction').

omega_variable(
    dual_use_definition_divergence,
    'Which research areas are classified as dual-use: narrow (gain-of-function variants), moderate (pathogen characterization), or expansive (all microbiology)?',
    'Textual analysis of biosecurity guidance documents from different nations; identification of classification boundaries and their justifications',
    'If definition expands: more researchers constrained, suppression increases, more snare dynamics. If definition stable: permits more rope-like coordination on clear boundaries. Definition divergence itself is an extraction mechanism (targets cannot coordinate when rules are unclear).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_definition_divergence, empirical, 'Variability in dual-use research definition across nations and institutions').

omega_variable(
    capacity_building_effectiveness,
    'Do international biosecurity capacity-building programs actually increase research autonomy in developing nations, or do they deepen institutional dependency on developed-nation standards and oversight?',
    'Longitudinal analysis of research independence, funding diversification, and norm-setting voice in developed-vs-developing nations over 20-year span',
    'If effective: scaffold sunset is real — temporary constraint on path to symmetry. If ineffective: capacity building is performative; developing nations remain trapped despite infrastructure investment. Theater itself may be the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_building_effectiveness, empirical, 'Whether capacity-building programs increase or deepen research dependency').

omega_variable(
    publication_gatekeeping_asymmetry,
    'Do biosecurity review processes delay or reject research publications disproportionately from researchers in developing nations, non-English-speaking researchers, or non-Western institutional affiliations?',
    'Analysis of publication timelines, rejection rates, and required revisions by author geography and institution type; comparison of security review stringency across journals and regions',
    'If yes: biosecurity review is an extraction and surveillance mechanism targeting specific populations. If neutral: review functions as genuine coordination. Asymmetry correlates with piton dynamics (gatekeeping theater masquerading as security).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_gatekeeping_asymmetry, empirical, 'Whether biosecurity publication review exhibits geographic or institutional bias').

omega_variable(
    norm_enforcement_divergence,
    'Are biosecurity norms enforced symmetrically across nations, or do developed nations maintain enforcement discretion for their own researchers while imposing strict enforcement on others?',
    'Case analysis of sanctioned dual-use research publications, institutional penalties, and access restrictions across developed vs developing nations',
    'If enforcement symmetric: norms are legitimate coordination constraints. If divergent: norms are extractive machinery where enforcement targets powerless agents selectively. Divergence reveals snare underlying the rope framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(norm_enforcement_divergence, empirical, 'Symmetry of biosecurity norm enforcement across developed and developing nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biosecurity_norm_formation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biosec_tr_t0, biosecurity_norm_formation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(biosec_tr_t5, biosecurity_norm_formation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(biosec_tr_t10, biosecurity_norm_formation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(biosec_be_t0, biosecurity_norm_formation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biosec_be_t5, biosecurity_norm_formation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(biosec_be_t10, biosecurity_norm_formation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biosecurity_norm_formation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biosecurity_norm_formation, 0.12).
narrative_ontology:affects_constraint(biosecurity_norm_formation, gain_of_function_research_governance).
narrative_ontology:affects_constraint(biosecurity_norm_formation, international_research_funding_asymmetry).
narrative_ontology:affects_constraint(biosecurity_norm_formation, pathogen_surveillance_access).

% DUAL FORMULATION NOTE:
% Biosecurity norm formation is downstream of specific dual-use research domains (gain-of-function, pathogen characterization) but represents a distinct structural constraint at the governance level. Upstream constraints have extractiveness values reflecting specific research risks; this constraint has extractiveness reflecting the institutional inequality in norm-setting power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biosecurity_norm_formation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
