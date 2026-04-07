% ============================================================================
% CONSTRAINT STORY: supplement_industry_regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supplement_industry_regulatory_capture, []).

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
 *   constraint_id: supplement_industry_regulatory_capture
 *   human_readable: Supplement Industry Regulatory Capture
 *   domain: healthcare_policy/regulatory_affairs
 *
 * SUMMARY:
 *   The supplement industry regulatory capture operates at the intersection
 *   of public health, economic interest, and institutional legitimacy. The
 *   Dietary Supplement Health and Education Act (DSHEA, 1994) created a
 *   permissive regulatory framework where supplements can make
 *   'structure-function claims' without pre-market efficacy evidence, unlike
 *   pharmaceuticals. Over the subsequent 30 years, the supplement industry (a
 *   ~$40 billion US market) has captured the FDA's definitional authority,
 *   congressional attention, and scientific discourse through revolving-door
 *   staffing, industry-funded research, and litigation threats. The
 *   constraint exhibits classic regulatory capture: the regulated industry
 *   (supplement manufacturers) has captured the regulator (FDA), extracted
 *   the agency's authority, and converted it into a tool for legitimizing
 *   permissive marketing while suppressing independent verification. However,
 *   the structure is not pure extraction (Snare) because the industry
 *   genuinely needs some coordination (standardization, liability frameworks,
 *   market legitimacy) that DSHEA provides. This hybrid character — real
 *   coordination function embedded within asymmetric extraction — defines the
 *   Tangled Rope classification. The theater ratio has increased over the
 *   interval as DSHEA enforcement has become increasingly performative: the
 *   FDA conducts warning letters and enforcement actions at rates far below
 *   what the universe of violators suggests, creating theatrical compliance
 *   theater that legitimizes the framework without enforcing it.
 *
 * KEY AGENTS:
 *   - Supplement Manufacturers: Primary beneficiary (institutional/arbitrage) — capture weak oversight; extract authority from FDA; access market without efficacy evidence cost
 *   - Consumer Health & Safety: Primary victim (powerless/trapped) — face unverified claims; cannot exit information asymmetry; bear risk of adulterated products and ineffective treatments
 *   - Healthcare Practitioners: Secondary victim (powerless/trapped) — undermined authority; patients resist clinical guidance; professional credibility extracted by unregulated competitors
 *   - FDA Regulators: Mixed victim-beneficiary (moderate/constrained) — genuinely need industry coordination but are captured; career advancement incentives favor non-confrontation; suppressed by resource constraints and political pressure
 *   - Congressional Dietary Supplement Caucus: Institutional actor (institutional/constrained) — explicitly captured by industry funding and staffing; maintains performative legitimacy of DSHEA
 *   - Consumer Advocacy Organizations: Organized victims (organized/constrained) — coordinate on safety standards but suppressed by litigation costs and resource asymmetry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory dysfunction as inherent to supplement 'complexity'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supplement_industry_regulatory_capture, 0.58).
domain_priors:suppression_score(supplement_industry_regulatory_capture, 0.65).
domain_priors:theater_ratio(supplement_industry_regulatory_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supplement_industry_regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(supplement_industry_regulatory_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supplement_industry_regulatory_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supplement_industry_regulatory_capture, tangled_rope).
narrative_ontology:human_readable(supplement_industry_regulatory_capture, "Supplement Industry Regulatory Capture").
narrative_ontology:topic_domain(supplement_industry_regulatory_capture, "healthcare_policy/regulatory_affairs").

domain_priors:requires_active_enforcement(supplement_industry_regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supplement_industry_regulatory_capture, supplement_manufacturers).
narrative_ontology:constraint_beneficiary(supplement_industry_regulatory_capture, industry_trade_associations).
narrative_ontology:constraint_victim(supplement_industry_regulatory_capture, consumer_health_safety).
narrative_ontology:constraint_victim(supplement_industry_regulatory_capture, regulatory_agency_independence).
narrative_ontology:constraint_victim(supplement_industry_regulatory_capture, evidence_based_medicine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER (SNARE) — Consumers face maximal extraction with no exit. Marketing claims routinely exceed evidence; adverse event reporting is fragmented; ingredient quality/dosing is unverified. The consumer cannot evaluate supplement safety or efficacy — the constraint extracts confidence and money with minimal transparency. Trapped by information asymmetry and lack of accessible verification.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTHCARE PRACTITIONERS (SNARE) — Physicians, nurses, and evidence-based practitioners face extraction through undermined authority. Patients arrive with unverified supplement claims and resist clinical guidance. The constraint extracts credibility from the medical profession while insulating supplements from clinical accountability. Trapped by regulatory asymmetry — medicines face rigorous evaluation, supplements do not.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FDA REGULATOR (TANGLED ROPE) — The FDA experiences genuine coordination needs (industry does want some standardization to avoid catastrophic liability; unsafe products damage the entire market) alongside asymmetric extraction (industry has captured the agency's authority structure and definition of oversight scope). The agency experiences suppression through litigation risk, resource constraints, and political pressure. Constrained by career incentives and institutional dependencies — regulators who aggressively challenge industry face retaliation through congressional defunding or legal action.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPPLEMENT MANUFACTURERS (ROPE) — Experience the constraint as genuine coordination: industry standardization, liability protection, and market legitimacy are real benefits. Manufacturers also benefit from extraction — weak enforcement of substantiation requirements and liberal marketing rules allow efficient cost-cutting and differentiation without evidence investment. Maximum beneficiary position.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL DIETARY SUPPLEMENT CAUCUS (PITON) — Maintains the Dietary Supplement Health and Education Act (DSHEA) framework through institutional inertia despite degraded functional legitimacy. The caucus is explicitly captured (funded by industry, staffed by former industry lawyers) and now serves primarily to block FDA action. The arrangement persists through legislative theater — routine reauthorizations, performative hearings with sympathetic witnesses — rather than through active problem-solving. Suppression is high but declining as the dysfunctionality becomes visible.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSUMER ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents (Consumers Union, Public Citizen, medical societies) coordinate on safety standards and transparency while experiencing extraction through legal/political barriers to reform. These organizations provide genuine coordination (pressure for standardized labeling, adverse event tracking) but face suppression via litigation costs and industry counter-messaging. Constrained by resource asymmetry — industry can outspend advocacy groups in legislative and litigation arenas.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a universal perspective, one might perceive supplement efficacy claims as inherently difficult to verify (cognitive science, placebo effect, individual variation are all 'natural' barriers). This perspective naturalizes the contingent regulatory capture as an immutable feature of how supplements 'work.' However, the structural data contradicts the mountain classification — the false summit reveals that the regulatory framework is not a law of nature but a captured institution.
constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supplement_industry_regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supplement_industry_regulatory_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supplement_industry_regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supplement_industry_regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(supplement_industry_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. The constraint extracts significant value from consumers (unverified claims, adulterated products, ineffectiveness) and from practitioners (authority degradation). The value is not higher because some regulation exists (adverse event reporting, some enforcement) and industry does coordinate on basic standards that prevent catastrophic failures. The trajectory from 0.35 to 0.58 reflects increasing capture over time — early DSHEA had more genuine balance; current regime is heavily skewed toward industry. Suppression (0.65): High. Consumers face structural barriers to verification (no accessible safety data, marketing dominance, cost of independent testing). Practitioners face professional isolation (patients distrust clinical guidance). Regulators face political pressure and resource starvation. The suppression is primarily structural (external barriers) rather than identity-based, though some consumers may experience identity lock as 'natural supplement users.' Theater ratio (0.68): Moderate-high. DSHEA compliance appears to be enforcement (warning letters, recalls) but is actually performed theater — enforcement is rare relative to the scale of potential violations. Congressional hearings on supplement safety are performative — industry-friendly witnesses dominate, and legislation never tightens standards despite evidence of problems.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) in DSHEA's standardization function. Consumers and practitioners see pure extraction (Snare) in unverified claims and authority undermining. The FDA sees mixed coordination and capture (Tangled Rope). The congressional caucus sees performative legitimacy (Piton). The analytical observer risks seeing a natural law (Mountain) about supplement complexity; the structural data reveals the capture as contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Manufacturers are beneficiaries with arbitrage-level exit capacity (can shift products across regulatory regimes, can engage in litigation) → low d. Consumers are victims with trapped-level exit capacity (information asymmetry, cost barriers, no regulatory appeal mechanism) → high d. FDA regulators are mixed (some beneficiary functions through career advancement in captured institutions; some victim functions through suppressed agency) → moderate d, constrained by institutional dependency rather than trapped or arbitrage mobility. The directionality overrides are needed for the congressional caucus: nominally institutional power but functionally captured (d should be ~0.15 to reflect captured beneficiary status, not 0.00 for pure beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the classification ambiguity by recognizing that regulatory capture is inherently a Tangled Rope — it requires genuine coordination (industry-regulator alignment, standardization, market legitimacy) to function as an extraction mechanism. If the industry and FDA were purely opposed, extraction would fail. The coordination function (industry gets market access; FDA gets institutional stability) is the glue that holds the extraction apparatus together. The Snare perspectives (consumers, practitioners) are accurate assessments of their structural experience, but the system-level classification is Tangled Rope because the beneficiary (industry) and the regulator (FDA) are coordinating on the framework that enables extraction. The Piton classification (congressional caucus) reflects the degradation of legislative function — the caucus has become purely performative theater maintaining institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_vs_marketing_boundary,
    'Where is the legitimate boundary between marketing communication and efficacy claim? Can a supplement claim to ''support bone health'' without clinical trial evidence?',
    'Comparative regulatory analysis: FDA drug standards (efficacy must be proven before marketing) vs DSHEA supplement standards (structure-function claims are permitted pre-evidence). Meta-analysis of whether permissive structure-function claims systematically exceed the evidence base.',
    'If boundary is at marketing only: much current supplement labeling is compliant, extraction is overstated. If boundary requires substantiation: most supplement manufacturers violate standards, extraction is worse than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_vs_marketing_boundary, conceptual, 'Boundary between legitimate structure-function claims and unsupported efficacy claims').

omega_variable(
    adverse_event_detection_adequacy,
    'Is the current adverse event reporting system (VAERS-equivalent for supplements) sufficient to detect safety problems, or does it systemically undercount?',
    'Comparison of adverse event reports for supplements vs pharmaceuticals in same therapeutic class. Analysis of cases where supplements had delayed safety actions despite available adverse event signals. Audit of consumer knowledge of and participation in adverse event reporting.',
    'If adequate: suppression is overstated; consumers have some exit path through safety reporting. If inadequate: extraction is worse than measured; safety signals are systematically hidden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adverse_event_detection_adequacy, empirical, 'Adequacy of adverse event detection for supplement safety').

omega_variable(
    industry_capture_timeline,
    'At what point did the supplement industry shift from advocacy for market access to active regulatory capture? When did industry funding of science and policy become dominant over independent verification?',
    'Historical analysis of DSHEA legislative history (1994) vs contemporary oversight mechanisms. Tracking of industry funding of supplement efficacy research. Analysis of FDA staffing and revolving-door patterns.',
    'If capture is recent (post-2010): the scaffold perspective (sunset through reform) may be realistic. If capture is structural since DSHEA (1994): the snare and piton perspectives are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_timeline, empirical, 'Timeline and mechanisms of industry capture of supplement regulation').

omega_variable(
    consumer_identity_lock,
    'To what extent do consumers self-identify as ''natural product users'' in ways that prevent evaluating individual supplements on merit? Is the binding mechanism structural (cost to exit) or identity-based (cannot imagine self outside this category)?',
    'Survey and ethnographic analysis of supplement users'' stated reasons for continued use post-adverse event or efficacy doubt. Analysis of switching costs (do users try alternatives easily?) vs identity fusion (do users explicitly frame supplements as core to their health identity?).',
    'If structural barriers dominate: suppression is primarily external (cost, availability); identity_locked exit is not applicable. If identity fusion dominates: suppression is internalized; the constraint is stronger than structural measures suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_identity_lock, conceptual, 'Whether consumer binding is structural cost or identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supplement_industry_regulatory_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supp_tr_t0, supplement_industry_regulatory_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(supp_tr_t5, supplement_industry_regulatory_capture, theater_ratio, 5, 0.6).
narrative_ontology:measurement(supp_tr_t10, supplement_industry_regulatory_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(supp_be_t0, supplement_industry_regulatory_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(supp_be_t5, supplement_industry_regulatory_capture, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(supp_be_t10, supplement_industry_regulatory_capture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supplement_industry_regulatory_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supplement_industry_regulatory_capture, 0.12).
narrative_ontology:affects_constraint(supplement_industry_regulatory_capture, pharmaceutical_efficacy_standards).
narrative_ontology:affects_constraint(supplement_industry_regulatory_capture, consumer_information_asymmetry).
narrative_ontology:affects_constraint(supplement_industry_regulatory_capture, healthcare_practitioner_authority).

% DUAL FORMULATION NOTE:
% The supplement industry regulatory capture can be decomposed into multiple structurally distinct constraints: (1) DSHEA structure-function claim permissiveness (ε~0.42, policy capture), (2) adverse event reporting inadequacy (ε~0.55, information asymmetry), (3) manufacturer quality control opacity (ε~0.52, verification bottleneck). This story treats the capture mechanism holistically; decomposition into separate ε values per claim would enable more precise analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supplement_industry_regulatory_capture, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
