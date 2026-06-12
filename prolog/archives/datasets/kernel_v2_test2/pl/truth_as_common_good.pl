% ============================================================================
% CONSTRAINT STORY: truth_as_common_good
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_truth_as_common_good, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: truth_as_common_good
 *   human_readable: Truth as Common Good in Digital Ecosystems
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   The constraint 'truth as common good' operates in digital ecosystems
 *   where algorithmic curation prioritizes engagement over veracity, creating
 *   a structural tension between platform business models (attention
 *   monetization) and democratic epistemic requirements (shared reality,
 *   verifiable claims, correction mechanisms). The constraint exhibits
 *   tangled_rope structure: genuine coordination functions exist (platforms
 *   do distribute information globally, enable fact-checking networks,
 *   provide transparency tools) alongside substantial extraction (epistemic
 *   pollution, manipulation vulnerability, concentration of narrative power).
 *   The coordination story is real but insufficient to justify the extraction
 *   level. This constraint is downstream of digital_power_concentration (the
 *   upstream snare that creates the structural conditions for epistemic
 *   extraction) and is a reading of the contested kernel 'human dignity in AI
 *   governance' — specifically, the Magisterial integralist reading that
 *   grounds truth's common-good status in theological anthropology.
 *
 * KEY AGENTS:
 *   - Democratic Publics: Primary victim (powerless/trapped) — bears full cost of epistemic pollution and manipulation; cannot exit algorithmically curated environments
 *   - Journalists and Fact-Checkers: Secondary victim (moderate/constrained) — resource asymmetry, platform control of distribution, career risk in challenging powerful actors
 *   - Platforms Monetizing Attention: Primary beneficiary (institutional/arbitrage) — captures revenue from engagement regardless of veracity; experiences moderation as coordination
 *   - Political Actors Manipulating Opinion: Secondary beneficiary (powerful/mobile) — low-cost opinion manipulation, micro-targeted disinformation, can exit to alternative channels
 *   - Disinformation Entrepreneurs: Tertiary beneficiary (moderate/mobile) — monetize viral falsehoods, exploit platform algorithms
 *   - Reform Coalition: Organized agents (organized/constrained) — civil society, researchers, some regulators working to restore epistemic commons; mixed experience of coordination and extraction
 *   - Regulatory Bodies: Institutional actors (institutional/constrained) — face genuine coordination problems but also experience extraction through capture and information asymmetry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible duality of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(truth_as_common_good, 0.58).
domain_priors:suppression_score(truth_as_common_good, 0.62).
domain_priors:theater_ratio(truth_as_common_good, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(truth_as_common_good, extractiveness, 0.58).
narrative_ontology:constraint_metric(truth_as_common_good, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(truth_as_common_good, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(truth_as_common_good, tangled_rope).
narrative_ontology:human_readable(truth_as_common_good, "Truth as Common Good in Digital Ecosystems").
narrative_ontology:topic_domain(truth_as_common_good, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(truth_as_common_good).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(truth_as_common_good, '730ecb53-c9c9-4781-b80c-e17fea01dc9d').
narrative_ontology:cs_kernel_codification('730ecb53-c9c9-4781-b80c-e17fea01dc9d', formalized).
narrative_ontology:cs_authority_grounding('730ecb53-c9c9-4781-b80c-e17fea01dc9d', lineage).
narrative_ontology:cs_interpretation_layer_present('730ecb53-c9c9-4781-b80c-e17fea01dc9d').
narrative_ontology:cs_reading_relation('730ecb53-c9c9-4781-b80c-e17fea01dc9d', truth_as_common_good__secular_humanist_reading, coexists_with).
narrative_ontology:cs_reading_relation('730ecb53-c9c9-4781-b80c-e17fea01dc9d', truth_as_common_good__techno_optimist_reading, forecloses).
narrative_ontology:cs_reading_relation('730ecb53-c9c9-4781-b80c-e17fea01dc9d', truth_as_common_good__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('730ecb53-c9c9-4781-b80c-e17fea01dc9d', foundational, truth_as_intrinsic_good).
narrative_ontology:cs_axiom_status(truth_as_intrinsic_good, holdable).
narrative_ontology:cs_axiom_grounding('730ecb53-c9c9-4781-b80c-e17fea01dc9d', truth_as_intrinsic_good, deontological).
narrative_ontology:cs_axiom('730ecb53-c9c9-4781-b80c-e17fea01dc9d', foundational, epistemic_pollution_violates_dignity).
narrative_ontology:cs_axiom_status(epistemic_pollution_violates_dignity, holdable).
narrative_ontology:cs_axiom_grounding('730ecb53-c9c9-4781-b80c-e17fea01dc9d', epistemic_pollution_violates_dignity, deontological).
narrative_ontology:cs_axiom('730ecb53-c9c9-4781-b80c-e17fea01dc9d', secondary, platforms_must_serve_common_good).
narrative_ontology:cs_axiom_status(platforms_must_serve_common_good, holdable).
narrative_ontology:cs_axiom_grounding('730ecb53-c9c9-4781-b80c-e17fea01dc9d', platforms_must_serve_common_good, conventional).
narrative_ontology:cs_reference_frame('730ecb53-c9c9-4781-b80c-e17fea01dc9d', imago_dei_anthropology).
narrative_ontology:cs_drift_state('730ecb53-c9c9-4781-b80c-e17fea01dc9d', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('730ecb53-c9c9-4781-b80c-e17fea01dc9d', '2026-06-08T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(truth_as_common_good, platforms_monetizing_attention).
narrative_ontology:constraint_beneficiary(truth_as_common_good, political_actors_manipulating_opinion).
narrative_ontology:constraint_beneficiary(truth_as_common_good, disinformation_entrepreneurs).
narrative_ontology:constraint_victim(truth_as_common_good, democratic_publics).
narrative_ontology:constraint_victim(truth_as_common_good, vulnerable_populations_targeted_by_manipulation).
narrative_ontology:constraint_victim(truth_as_common_good, educators).
narrative_ontology:constraint_victim(truth_as_common_good, journalists).
narrative_ontology:constraint_victim(truth_as_common_good, fact_checking_organizations).
narrative_ontology:constraint_vindicates(truth_as_common_good, engagement_maximization_doctrine).
narrative_ontology:constraint_vindicates(truth_as_common_good, marketplace_of_ideas_self_correction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary citizens navigating algorithmically curated information environments. Cannot distinguish reliable from unreliable sources without significant effort. Vulnerable to micro-targeted manipulation. Exit would require abandoning digital communication, which is socially and economically prohibitive. Bears full cost of epistemic pollution: degraded discourse, manipulation, erosion of shared reality.
narrative_ontology:constraint_stakeholder(truth_as_common_good, democratic_publics, payer,
    powerless, immediate, trapped, global).

% Professional fact-checkers and investigative journalists working to verify claims and correct falsehoods. Face resource asymmetry: disinformation spreads faster than correction, platforms control distribution algorithms, corrections often fail to reach those who saw the original falsehood. Career risk in challenging powerful actors. Can exit journalism but at significant professional cost.
narrative_ontology:constraint_stakeholder(truth_as_common_good, journalists, payer,
    moderate, biographical, constrained, national).

% Social media platforms, search engines, and content aggregators that monetize user attention through advertising. Engagement-maximizing algorithms prioritize emotionally charged content regardless of veracity. Content moderation is partly functional, partly performative compliance theater. Can shift between jurisdictions and business models with relative ease. Captures revenue from engagement; bears minimal cost from epistemic harms.
narrative_ontology:constraint_stakeholder(truth_as_common_good, platforms_monetizing_attention, beneficiary,
    institutional, immediate, arbitrage, global).

% Political campaigns, state actors, and advocacy organizations using micro-targeted disinformation and algorithmic amplification to shape public opinion. Low-cost manipulation compared to traditional media. Can exit to alternative platforms or traditional channels if one platform becomes inhospitable. Benefits from epistemic pollution and filter bubbles.
narrative_ontology:constraint_stakeholder(truth_as_common_good, political_actors_manipulating_opinion, beneficiary,
    powerful, immediate, mobile, national).

% Individuals and organizations monetizing viral falsehoods through ad revenue, affiliate marketing, or political patronage. Exploit platform algorithms that reward engagement over veracity. Can shift between platforms and content types. Low barriers to entry and exit.
narrative_ontology:constraint_stakeholder(truth_as_common_good, disinformation_entrepreneurs, beneficiary,
    moderate, immediate, mobile, global).

% Civil society organizations, academic researchers, and some regulators working to restore epistemic commons through transparency mandates, algorithmic accountability, and platform governance reform. Benefits from coordination mechanisms (research networks, policy forums) but also bears costs of platform resistance, regulatory capture, and resource constraints. Mixed experience: genuine coordination alongside extraction.
narrative_ontology:constraint_stakeholder(truth_as_common_good, reform_coalition, agenda_setter,
    organized, generational, constrained, global).

% Government agencies tasked with content governance and platform regulation. Face genuine coordination problems (defining harmful content, balancing speech rights, managing cross-border flows) but also experience extraction through regulatory capture, revolving-door employment, and asymmetric information. Platforms often know more about their own systems than regulators do. Can set rules but enforcement is constrained by platform cooperation and legal challenges.
narrative_ontology:constraint_stakeholder(truth_as_common_good, regulatory_bodies, agenda_setter,
    institutional, biographical, constrained, continental).

% Independent verification organizations partnering with platforms to label false content. Provide genuine epistemic service but are structurally disadvantaged: corrections propagate slower than falsehoods, platforms control whether and how fact-checks are displayed, and fact-checkers bear reputational risk when platforms ignore their findings. Resource-constrained relative to the scale of misinformation.
narrative_ontology:constraint_stakeholder(truth_as_common_good, fact_checking_organizations, payer,
    moderate, biographical, constrained, global).

% Teachers and educational institutions working to build media literacy and critical thinking skills. Face uphill battle against algorithmically curated environments that reward emotional engagement over epistemic rigor. Educational interventions are slow and resource-intensive; epistemic pollution is fast and cheap. Cannot exit the problem but can only mitigate it.
narrative_ontology:constraint_stakeholder(truth_as_common_good, educators, payer,
    moderate, generational, constrained, national).

% Demographic groups disproportionately targeted by disinformation campaigns: elderly users less familiar with digital literacy, linguistic minorities receiving less fact-checking coverage, politically polarized communities in filter bubbles. Structurally disadvantaged by lower digital literacy, language barriers, or social isolation. Cannot exit without severe social costs.
narrative_ontology:constraint_stakeholder(truth_as_common_good, vulnerable_populations_targeted_by_manipulation, payer,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms solve genuine information distribution problems: enabling global communication, reducing transaction costs for knowledge sharing, connecting fact-checkers with content, providing transparency tools for researchers. Reform coalitions coordinate verification networks and policy development.
% TRANSFER_FUNCTION: Attention and engagement flow from users to platforms (monetized through advertising). Narrative power flows from distributed sources to platforms (algorithmic curation concentrates control over what is seen). Epistemic costs (degraded discourse, manipulation vulnerability) flow from platforms and manipulators to democratic publics.
% ABSENT_VOICES: Those without digital access (the digitally excluded), those in authoritarian regimes where platforms are censored or controlled, future generations who will inherit degraded epistemic commons, and non-human stakeholders (the epistemic commons itself as an abstract good). These voices would object to the current arrangement but are not in the conversation due to structural exclusion, geographic barriers, temporal displacement, or ontological category (abstract goods have no advocate).
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight — if platforms prioritized veracity over engagement, if manipulation became prohibitively expensive, if epistemic commons were restored — the world would rearrange substantially. Political campaigns would shift strategies, platform business models would change, fact-checking organizations would face different resource constraints, and democratic publics would experience different information environments. The arrangements of multiple stakeholders depend on the current epistemic extraction regime.
% FOUNDING_PROBLEM: The founding problem was information distribution at scale: how to enable global communication, connect dispersed knowledge, and reduce transaction costs for information sharing in a digitally networked world. Platforms were built to solve this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (information distribution at scale) remains live and is corroborated by multiple sources outside the beneficiary set: academic researchers studying network effects and information diffusion, civil society organizations working on digital inclusion, and international bodies like UNESCO documenting the digital divide. However, the SOLUTION to that problem (engagement-maximizing algorithms, attention monetization, centralized curation) is contested. Reform coalitions argue that alternative architectures (chronological feeds, user-controlled algorithms, federated networks) could solve the founding problem without the current extraction level. The founding problem is live; the current arrangement is not the only solution.
narrative_ontology:disappearance_verdict(truth_as_common_good, world_rearranges).
narrative_ontology:founding_problem_status(truth_as_common_good, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC PUBLICS (SNARE) — Trapped in algorithmically curated information environments with no practical exit. Cannot distinguish reliable from unreliable sources without significant cognitive overhead. Bears full cost of epistemic pollution: degraded democratic discourse, vulnerability to manipulation, erosion of shared reality. Maximum experienced extraction.
constraint_indexing:constraint_classification(truth_as_common_good, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JOURNALISTS AND FACT-CHECKERS (SNARE) — Constrained by resource asymmetry: disinformation spreads faster than correction, platforms control distribution, and correction often fails to reach those who saw the original falsehood. Career risk in challenging powerful actors. Some agency but structurally disadvantaged. High extraction.
constraint_indexing:constraint_classification(truth_as_common_good, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORMS MONETIZING ATTENTION (ROPE) — Benefits from engagement-maximizing algorithms regardless of veracity. Experiences content moderation as coordination: managing advertiser risk, regulatory pressure, and user retention. Net beneficiary. Extraction flows toward this agent.
constraint_indexing:constraint_classification(truth_as_common_good, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLITICAL ACTORS MANIPULATING OPINION (ROPE) — Benefits from low-cost opinion manipulation and micro-targeted disinformation. Sees the digital ecosystem as a coordination tool for message amplification. Can exit to alternative platforms or traditional media. Low experienced extraction.
constraint_indexing:constraint_classification(truth_as_common_good, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED REFORM ADVOCATES (TANGLED ROPE) — Civil society organizations, academic researchers, and some regulators working to restore epistemic commons. Benefits from coordination mechanisms (research networks, policy forums, transparency mandates) but also bears costs of platform resistance, regulatory capture, and resource constraints. Mixed experience: genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(truth_as_common_good, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY BODIES (TANGLED ROPE) — Institutional actors tasked with content governance face genuine coordination problems (defining harmful content, balancing speech rights, managing cross-border flows) but also experience extraction through regulatory capture, revolving-door dynamics, and asymmetric information. The regulator both coordinates and is constrained by the platforms it regulates.
constraint_indexing:constraint_classification(truth_as_common_good, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination function (platforms do solve real information distribution problems, enable global communication, reduce transaction costs for knowledge sharing) and substantial extraction (epistemic pollution, democratic degradation, concentration of narrative power). The coordination story is real but insufficient to justify the extraction level. Tangled Rope classification reflects this irreducible duality.
constraint_indexing:constraint_classification(truth_as_common_good, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(truth_as_common_good_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(truth_as_common_good, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(truth_as_common_good, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(truth_as_common_good, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(truth_as_common_good_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Platforms capture revenue from engagement-maximizing algorithms that systematically amplify emotionally charged and often false content. Political actors exploit low-cost manipulation. Democratic publics bear costs of degraded discourse and vulnerability to manipulation. The extraction is not total (some genuine information distribution occurs) but is significant and rising over the interval. Suppression (0.62): Moderate-high. Barriers to epistemic correction include algorithmic amplification of falsehoods, platform control of distribution, resource asymmetry between disinformation and correction, filter bubbles, and the cognitive overhead required to verify claims. Exit options are limited: democratic publics cannot practically leave digital ecosystems without severe social and economic costs. Suppression has increased over the interval as platforms have matured and network effects have locked in users. Theater ratio (0.48): Moderate. Content moderation is partly functional (some harmful content is removed, some fact-checking occurs) but also substantially performative (moderation policies are inconsistently enforced, transparency reports obscure more than they reveal, appeals processes are opaque, and platforms resist structural changes that would reduce engagement). Theater has increased as regulatory pressure has grown but platforms have responded with compliance theater rather than structural reform.
 *
 * PERSPECTIVAL GAP:
 *   Democratic publics experience pure extraction (snare): trapped in manipulated information environments with no exit. Journalists and fact-checkers also experience snare: structurally disadvantaged by resource asymmetry and platform control. Platforms and political manipulators experience coordination (rope): the system works for them, enabling monetization and influence at low cost. Reform coalitions and regulators experience tangled_rope: genuine coordination problems (defining harmful content, balancing rights, managing cross-border flows) coexist with extraction (capture, information asymmetry, resistance from platforms). The analytical observer sees tangled_rope at the civilizational scale: the coordination function is real (global information distribution, reduced transaction costs) but insufficient to justify the extraction level (epistemic pollution, democratic degradation). The perspectival gap is structural: beneficiaries see coordination, victims see extraction, and those in between see both.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic publics are full victims: d approaches 1.0 (full target). Trapped exit options and powerless status amplify experienced extraction to maximum. Journalists and fact-checkers are victims with some agency: d is high (~0.75) but not maximal due to moderate power and constrained (not trapped) exit. Platforms are full beneficiaries: d approaches 0.0 (full beneficiary). Arbitrage exit options and institutional power dampen experienced extraction to negative (subsidy). Political manipulators are beneficiaries: d is low (~0.15) due to powerful status and mobile exit. Reform coalitions are mixed: d is moderate (~0.45) reflecting genuine coordination function alongside extraction. Regulators are also mixed but more constrained: d is moderate-high (~0.55) due to capture dynamics. The analytical observer's d is derived from the structural data and reflects the irreducible duality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled_rope is the correct classification when both coordination and extraction are structurally irreducible. The coordination function is real: platforms do solve genuine information distribution problems, fact-checking networks do coordinate verification, transparency mandates do enable some accountability. But the extraction is also real and substantial: epistemic pollution degrades democratic discourse, manipulation concentrates political power, and platforms capture rents from engagement regardless of veracity. The constraint is not a snare (pure extraction) because the coordination function is genuine and benefits some agents. It is not a rope (pure coordination) because the extraction is substantial and harms identifiable victims. It is tangled_rope because both are true simultaneously and neither can be reduced to the other. The perspectival gap confirms this: beneficiaries see rope, victims see snare, and the analytical observer sees the irreducible entanglement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_of_ideas_empirical_status,
    'Does the ''marketplace of ideas'' self-correction mechanism function in algorithmically curated environments, or is it an empirically falsified premise that platforms invoke as cover?',
    'Longitudinal studies of correction propagation vs. misinformation spread; A/B testing of algorithmic interventions; comparative analysis of epistemic outcomes across platform architectures (chronological vs. engagement-maximizing feeds)',
    'If self-correction functions: lower extractiveness, platforms are genuine coordination mechanisms. If falsified: higher extractiveness, marketplace-of-ideas framing is theatrical cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marketplace_of_ideas_empirical_status, empirical, 'Whether marketplace-of-ideas self-correction operates in algorithmic environments').

omega_variable(
    content_moderation_theater_vs_function,
    'What proportion of platform content moderation is functional verification vs. performative compliance theater?',
    'Leaked internal documents, whistleblower testimony, error rate analysis, comparison of moderation outcomes across jurisdictions with different regulatory pressure',
    'High theater ratio supports piton classification from some perspectives; low theater ratio supports tangled_rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_moderation_theater_vs_function, empirical, 'Functional vs. theatrical content moderation ratio').

omega_variable(
    regulatory_capture_depth,
    'Are regulatory bodies genuinely constrained by platforms'' structural power, or do they retain sufficient autonomy to impose meaningful constraints?',
    'Analysis of regulatory outcomes vs. stated goals; revolving-door employment patterns; comparison of regulatory stringency across jurisdictions with different political economies',
    'Deep capture: regulators are victims, not coordinators. Shallow capture: regulators are tangled_rope agents with mixed experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of regulatory capture by platforms').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''human dignity as imago Dei'' or ''truth as common good''? The Magisterial reading grounds truth''s common-good status in theological anthropology (persons are truth-seeking by nature, made in God''s image). The secular humanist reading grounds it in democratic theory (shared epistemic commons enables self-governance). Both claim the same constraint but from different kernels.',
    'Conceptual analysis of which framing better predicts the constraint''s structural features: beneficiary/victim distribution, enforcement mechanisms, resistance patterns. If theological grounding is load-bearing, the Magisterial kernel is correct. If democratic-theory grounding suffices, the secular kernel is correct.',
    'Magisterial kernel: authority_grounding is lineage, interpretation_layer_present is true, axioms include imago_dei_anthropology. Secular kernel: authority_grounding is distributed, no interpretation layer, axioms include democratic_self_governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which kernel this constraint is a reading of').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(truth_as_common_good, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(truth_cg_tr_t0, truth_as_common_good, theater_ratio, 0, 0.3).
narrative_ontology:measurement(truth_cg_tr_t3, truth_as_common_good, theater_ratio, 3, 0.38).
narrative_ontology:measurement(truth_cg_tr_t6, truth_as_common_good, theater_ratio, 6, 0.44).
narrative_ontology:measurement(truth_cg_tr_t9, truth_as_common_good, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(truth_cg_be_t0, truth_as_common_good, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(truth_cg_be_t3, truth_as_common_good, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(truth_cg_be_t6, truth_as_common_good, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(truth_cg_be_t9, truth_as_common_good, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(truth_cg_su_t0, truth_as_common_good, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(truth_cg_su_t3, truth_as_common_good, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(truth_cg_su_t6, truth_as_common_good, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(truth_cg_su_t9, truth_as_common_good, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(truth_as_common_good, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of digital_power_concentration (the upstream snare that creates structural conditions for epistemic extraction). The upstream constraint has its own extractiveness reflecting platform monopoly power; this constraint has its own extractiveness reflecting the specific epistemic harms of engagement-maximizing algorithms. They are distinct but causally linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(truth_as_common_good, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
