% ============================================================================
% CONSTRAINT STORY: chemical_regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chemical_regulatory_capture, []).

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
 *   constraint_id: chemical_regulatory_capture
 *   human_readable: Chemical Industry Regulatory Capture
 *   domain: industrial_regulation/environmental_governance
 *
 * SUMMARY:
 *   Chemical industry regulatory capture represents a structural constraint
 *   in which the regulated industry systematically influences the regulatory
 *   agencies ostensibly designed to constrain it. This is not market
 *   distortion or corruption in the conventional sense—it is an institutional
 *   capture mechanism operating through multiple structural channels: (1)
 *   industry funding and commissioning of safety studies that drive
 *   regulatory standards; (2) technical complexity in chemical hazard
 *   assessment that creates information asymmetries favoring incumbent firms
 *   with resources to generate data; (3) revolving-door employment between
 *   industry and regulatory agencies creating personnel continuity and
 *   cultural alignment; (4) direct participation in standard-setting
 *   committees where industry shapes the technical definitions of 'safe'; (5)
 *   regulatory arbitrage threat (relocation to less-regulated jurisdictions)
 *   that creates political pressure on agencies to maintain 'competitive'
 *   standards; (6) asymmetric litigation capacity allowing firms to challenge
 *   enforcement actions through extended legal proceedings. The constraint
 *   exhibits all six classification types from different structural
 *   positions, making it a diagnostic exemplar for regulatory failure modes.
 *   The extractiveness has accumulated over two decades (0.35 → 0.58),
 *   theater has increased (0.45 → 0.64) as procedural legitimacy (advisory
 *   boards, comment periods, transparency initiatives) has grown without
 *   corresponding decision-making power, and suppression requirements have
 *   intensified (0.52 → 0.68) as information asymmetries and enforcement
 *   barriers have solidified.
 *
 * KEY AGENTS:
 *   - Exposed Communities: Powerless/trapped (national scope) — bear health costs of biased standards; no exit or meaningful agency
 *   - Environmental Protection Function: Powerless/constrained (global scope) — regulatory mandate becomes instrumentalized; standards drift upward; testing protocols favor industry assumptions
 *   - Regulatory Agency: Institutional/constrained (national scope) — primary coordinator of legitimate safety standards but systematically biased toward industry through budget dependency, revolving-door hiring, technical resource asymmetries, legal challenge capacity
 *   - Incumbent Chemical Firms: Institutional/arbitrage (national scope) — primary beneficiaries; experience the system as coordination mechanism; arbitrage exit through production relocation or jurisdiction shopping
 *   - Non-Incumbent Manufacturers: Moderate/constrained (national scope) — experience mixed coordination (standardized frameworks reduce chaos) and extraction (incumbent advantage through biased standards and regulatory barriers-to-entry)
 *   - Environmental Advocacy Organizations: Organized/mobile (global scope) — perceive degraded institution (theater without decision-making power); retain mobile exit options (litigation, media, jurisdictional forum-shifting) but face resource and information constraints
 *   - Analytical Observer: Analytical/analytical (global scope) — risks naturalizing contingent institutional arrangements as inevitable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chemical_regulatory_capture, 0.58).
domain_priors:suppression_score(chemical_regulatory_capture, 0.68).
domain_priors:theater_ratio(chemical_regulatory_capture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chemical_regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(chemical_regulatory_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(chemical_regulatory_capture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chemical_regulatory_capture, tangled_rope).
narrative_ontology:human_readable(chemical_regulatory_capture, "Chemical Industry Regulatory Capture").
narrative_ontology:topic_domain(chemical_regulatory_capture, "industrial_regulation/environmental_governance").

domain_priors:requires_active_enforcement(chemical_regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chemical_regulatory_capture, incumbent_chemical_firms).
narrative_ontology:constraint_beneficiary(chemical_regulatory_capture, regulatory_agency_staff).
narrative_ontology:constraint_victim(chemical_regulatory_capture, environmental_protection).
narrative_ontology:constraint_victim(chemical_regulatory_capture, public_health).
narrative_ontology:constraint_victim(chemical_regulatory_capture, regulatory_integrity).
narrative_ontology:constraint_victim(chemical_regulatory_capture, competing_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED COMMUNITIES (SNARE) — Communities near chemical facilities cannot exit the exposure zone without relocating. Suppression operates through information asymmetry (studies commissioned by industry), economic dependency on facility employment, and political marginalization. No alternatives provided; no meaningful participation in safety decisions. Maximum extraction.
constraint_indexing:constraint_classification(chemical_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENVIRONMENTAL PROTECTION FUNCTION (SNARE) — The regulatory mandate cannot exit the capture; the function itself becomes instrumentalized. Standards drift upward in permissible exposure limits (regulatory creep), testing protocols incorporate industry-favorable assumptions, and negative findings are excluded from decision-making. The commons bears cost; beneficiary captures value.
constraint_indexing:constraint_classification(chemical_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (TANGLED ROPE) — The agency coordinates legitimate safety communication (rope function) but is systematically biased toward industry positions through budget dependency, revolving-door hiring, and technical resource asymmetries. Enforcement capacity is constrained by industry legal challenges and political pressure. The agency genuinely solves coordination problems (communication of standards) while simultaneously extracting from the public (biased standards).
constraint_indexing:constraint_classification(chemical_regulatory_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CHEMICAL FIRMS (ROPE) — From the incumbent's perspective, the regulatory system is primarily a coordination mechanism: standardized safety protocols reduce litigation risk, industry participation in standard-setting ensures technical feasibility and cost-efficiency, and clear regulatory pathways enable investment planning. The extraction (biased standards) is invisible to this perspective — it appears as legitimate accommodation of industrial realities. Arbitrage exit option reflects ability to relocate production or influence jurisdiction shopping.
constraint_indexing:constraint_classification(chemical_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL ADVOCACY ORGANIZATIONS (PITON) — Environmental NGOs perceive the regulatory system as a degraded institution maintained by theater (public comment periods, scientific advisory boards) while actual decision-making occurs in industry-captured channels. The organizations retain mobile exit options (public litigation, media campaigns, jurisdictional forum-shifting to state/local/international bodies) and can see the constraint clearly, but their ability to disrupt the capture is limited by information asymmetries and resource disparities. Classification reflects theater ratio exceeding chi.
constraint_indexing:constraint_classification(chemical_regulatory_capture, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-INCUMBENT MANUFACTURERS (TANGLED ROPE) — Smaller or specialized chemical firms experience both coordination function (standardized safety frameworks reduce compliance chaos) and extraction (incumbent advantage through industry-favorable standards, regulatory barriers-to-entry, and technical standards favoring dominant process technologies). Exit options are constrained by regulatory compliance costs and incumbent lobbying power, but not eliminated — some firms arbitrage by targeting underregulated jurisdictions or specialized markets.
constraint_indexing:constraint_classification(chemical_regulatory_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, regulatory capture might be naturalized as an inevitable economic law: firms with high compliance costs will always seek regulatory influence; information asymmetries always favor incumbents with resources to generate technical data; concentrated regulatory authority always creates an attractive target for concentrated economic interests. This perspective risks treating contingent institutional arrangements as immutable natural law. The structural data (identifiable beneficiaries, measurable extraction, alternative regulatory models) contradicts the mountain classification — this is a false summit.
constraint_indexing:constraint_classification(chemical_regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chemical_regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chemical_regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chemical_regulatory_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chemical_regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chemical_regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(chemical_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, representing measurable asymmetric benefit to incumbent firms through biased standards while the public bears environmental/health costs. The value reflects that the extraction is real but not total—alternative jurisdictions exist, litigation can challenge regulations, and some oversight mechanisms function. Suppression (0.68): High. Multiple mechanisms prevent exit and alternatives: exposed communities are geographically trapped; regulatory integrity is captured by information asymmetries; competing firms face incumbent-favorable standards; the public lacks technical capacity to counter industry-generated safety data; enforcement is resource-constrained and legally vulnerable. The measurement trajectory (rising over 20 years) reflects enforcement machinery strengthening—each challenge defeated, each precedent set makes future enforcement more difficult. Theater ratio (0.64): Moderate-high. Regulatory procedures (public comment periods, scientific advisory boards, environmental impact assessments, transparency initiatives) create perceived legitimacy and opportunity for participation, but actual decision-making authority concentrates in industry-captured channels. The rising trajectory (0.45 → 0.64) shows theater increasing even as actual capture deepens—procedural legitimacy substitutes for substantive independence. This is the diagnostic signature of Piton from the advocacy organization's perspective: the ritual persists through institutional inertia despite low functional capacity to constrain industry behavior.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Incumbent chemical firms perceive a coordination mechanism—regulatory standards ensure technical feasibility, reduce litigation risk, enable investment planning—Rope classification is their genuine experience. The regulatory agency genuinely solves coordination problems (communicating safety standards) while being systematically biased toward industry—Tangled Rope is structurally accurate. Exposed communities experience pure extraction with no exit—Snare. Environmental organizations see performative procedures without decision-making power—Piton. The civilizational analytical observer risks the false natural law perspective (Mountain: 'firms will always influence regulators because information asymmetries are inevitable'). The perspectival gap is not a measurement problem—it is the structural reality of the constraint. All perspectives are defensible from within their own structural position. The gap reveals that regulatory capture is not a distortion of an otherwise pure system but an intentional structural outcome of institutional design: agencies designed with industry participation, funded through mechanisms dependent on regulated industries, staffed through revolving-door arrangements, and constrained by litigation capacity asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d) varies dramatically by agent perspective, producing the full range of classifications. Incumbent firms as beneficiaries with arbitrage exit options experience d ≈ 0.15–0.25 (low extraction from their perspective—the system is coordination). Non-incumbent firms as partial victims with constrained options experience d ≈ 0.55–0.65 (moderate extraction). Exposed communities as trapped victims experience d ≈ 0.90+ (maximum extraction). The regulatory agency itself is split: as a primary coordinator it experiences lower d; as a captured institution it experiences higher d. The analytical observer at civilizational scope risks d ≈ 0.50 (symmetric) by naturalizing the capture as inevitable. The engine's directionality derivation from beneficiary/victim declarations and exit options produces these variations automatically—no override needed, as the structural data is sufficiently differentiated. The perspectival gap (Rope for beneficiaries, Tangled Rope for agencies, Snare for trapped victims, Piton for observers) reveals that the constraint is experienced completely differently depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by differentiating between the coordination function the regulatory system legitimately provides (preventing chaos, standardizing safety communication) and the extraction mechanism that layered on top (industry influence over which standards are adopted). The Tangled Rope classification is exactly correct: genuine coordination coexists with asymmetric extraction. The false natural law perspective (Mountain) attempts to dissolve the mandatrophy by claiming that regulatory capture is inevitable—an immutable property of regulation. This is false. Alternative institutional designs (independent funding, civil-service protection, reduced industry participation in standard-setting, asymmetric litigation remedies) have been attempted in other jurisdictions and domains. The question is not whether capture is possible but whether specific institutional choices perpetuate it. The constraint is contingent, not natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    industry_funding_independence,
    'Are safety studies funded by regulated firms structurally distinguishable from independent research in their conclusions, or is the apparent bias a genuine reflection of technical feasibility?',
    'Meta-analysis of safety conclusions: comparative effect sizes for industry-funded vs. independently-funded studies on identical chemical hazards; blinded review of methodological quality; replication attempts of industry-sponsored safety studies by non-sponsored labs',
    'If studies show systematic bias: extractiveness rises to 0.72+ (snare territory). If no systematic bias detected: extractiveness drops to 0.35–0.40 (rope territory) — the capture is coordination, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_funding_independence, empirical, 'Whether industry-funded safety studies show systematic bias vs. genuine technical insight').

omega_variable(
    regulatory_arbitrage_exit,
    'Does the threat of production relocation to less-regulated jurisdictions constitute genuine exit capacity (making regulatory agencies'' constraint structural but not coercive) or manufactured threat (extraction via political pressure)?',
    'Historical relocation data: correlation between regulatory tightening and actual facility moves; cost-benefit analysis of relocation vs. compliance investment; credibility assessment of relocation threats by ex-regulators and industry sources',
    'If relocation is genuine exit: firms experience constrained-level, not trapped-level barriers; agencies face genuine coordination pressure, not pure capture. If threats are manufactured: extraction mechanism is strengthened (firms extract by threatening exit they would not execute); suppression is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_exit, empirical, 'Whether production relocation is genuine exit or political threat').

omega_variable(
    alternative_regulatory_models,
    'Do regulatory jurisdictions with stronger independence structures (civil-service protection, dedicated funding, reduced industry participation in standard-setting) achieve materially better environmental/health outcomes, or do they impose economically unsustainable compliance burdens?',
    'Comparative regulatory effectiveness: health outcomes in high-independence vs. captured jurisdictions; cost-benefit analysis of alternative regulatory designs; longitudinal tracking of compliance cost vs. environmental/health improvement',
    'If alternative models show better outcomes at lower cost: capture is contingent institutional failure (Tangled Rope classification confirmed). If alternatives impose severe economic costs with marginal health gains: capture might represent rough cost-benefit balance (recalibrates suppression and extractiveness downward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_regulatory_models, empirical, 'Whether alternative regulatory models achieve better outcomes').

omega_variable(
    revolving_door_mechanism,
    'Does the revolving door between industry and regulatory agencies (ex-regulators joining firms, ex-industry scientists joining agencies) constitute capture through personnel continuity or legitimate expertise transfer?',
    'Career-path analysis: do ex-regulators advocate for positions that protect their former/future employers? Do regulatory decisions shift visibly after industry-affiliated officials join? Do agency positions on technical standards change after key staff rotate to industry?',
    'If capture mechanism confirmed: suppression rises (institutional inertia prevents detection of bias). If expertise transfer verified: suppression may be lower than measured (the constraint is coordination plus modest asymmetry, not capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_mechanism, empirical, 'Whether revolving-door rotation constitutes capture or expertise transfer').

omega_variable(
    false_natural_law_naturalization,
    'Is regulatory capture being naturalized as inevitable (mountain perspective) when it is actually a contingent outcome of specific institutional design choices (tangled_rope)?',
    'Historical analysis: what regulatory designs existed in different eras and jurisdictions? Are current capture patterns present in periods/places with different institutional structures? What would need to change institutionally to reduce capture severity?',
    'Confirms false-summit diagnosis: the mountain perspective is analytically interesting but structurally misleading. The constraint is a designed institutional outcome, not a natural law. Remediation becomes possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_natural_law_naturalization, conceptual, 'Whether regulatory capture is naturalized inevitability or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chemical_regulatory_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chem_cap_tr_t0, chemical_regulatory_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(chem_cap_tr_t10, chemical_regulatory_capture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(chem_cap_tr_t20, chemical_regulatory_capture, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(chem_cap_be_t0, chemical_regulatory_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chem_cap_be_t10, chemical_regulatory_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(chem_cap_be_t20, chemical_regulatory_capture, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(chem_cap_su_t0, chemical_regulatory_capture, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(chem_cap_su_t10, chemical_regulatory_capture, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(chem_cap_su_t20, chemical_regulatory_capture, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chemical_regulatory_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(chemical_regulatory_capture, chemical_hazard_information_asymmetry).
narrative_ontology:affects_constraint(chemical_regulatory_capture, environmental_damage_liability_erosion).
narrative_ontology:affects_constraint(chemical_regulatory_capture, incumbent_firm_barrier_to_entry).

% DUAL FORMULATION NOTE:
% Chemical regulatory capture is upstream of three distinct constraints: (1) information asymmetry in chemical hazard assessment (which determines the data available for regulatory decisions); (2) liability erosion (which determines whether firms bear costs of environmental damage); (3) barrier-to-entry for competing firms (which determines market concentration and incumbent power). This story models the capture mechanism itself; the three downstream constraints model specific extractive outcomes enabled by the capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chemical_regulatory_capture, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
