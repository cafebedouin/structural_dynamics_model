% ============================================================================
% CONSTRAINT STORY: sk_ai_act_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_ai_act_2026, []).

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
 *   constraint_id: sk_ai_act_2026
 *   human_readable: South Korea's Proposed AI Industry Promotion Act (2026)
 *   domain: technological/political
 *
 * SUMMARY:
 *   South Korea's Proposed AI Industry Promotion Act (2026) exemplifies the
 *   tension between coordinated industrial policy and asymmetric data
 *   extraction. The act creates broad exemptions from existing privacy law
 *   (PIPA) and copyright statute, allowing AI developers to collect personal
 *   data and use copyrighted works for training without consent, under the
 *   justification that South Korean developers cannot compete globally with
 *   restricted data access. The constraint reveals itself differently from
 *   eight structural positions: domestic AI developers see coordination
 *   (Rope), citizens see extraction (Snare), organized civil society sees a
 *   mixed system (Tangled Rope), international platforms see arbitrage
 *   opportunity (Tangled Rope), existing enforcement machinery sees its own
 *   degradation (Piton), sunset advocates see temporary bridge (Scaffold),
 *   and the analytical observer risks naturalizing competitive necessity as
 *   immutable law (false Mountain). The core structural data shows
 *   moderate-high extractiveness (0.58) and suppression (0.68), with rising
 *   theater ratio (0.40→0.55), indicating that enforcement machinery is
 *   becoming increasingly performative as exemptions expand.
 *
 * KEY AGENTS:
 *   - Data Subjects (Citizens): Powerless/trapped — cannot exit national jurisdiction or opt out of collection; bear full cost of privacy loss
 *   - Copyright Holders and Content Creators: Powerless/trapped — cannot prevent their work from being used in training; no compensation mechanism; face suppressed legal remedies
 *   - Domestic AI Developers (Samsung, Naver, Kakao, startups): Institutional/arbitrage beneficiaries — capture legal safe harbor, training data access, competitive boost; can arbitrage global regulatory differences
 *   - South Korean Government Innovation Agencies: Institutional/arbitrage beneficiaries — advance national AI strategy; control narrative around competitive necessity
 *   - Organized Civil Society (Privacy NGOs, labor unions, tech workers): Organized/constrained — can mobilize politically but face institutional opposition; experience mixed coordination benefit (national tech advancement) and extraction cost (citizen data loss)
 *   - International Tech Companies (Meta, Google, OpenAI): Powerful/mobile — gain access to South Korean data; face regulatory arbitrage opportunities but also international pressure from home regulators
 *   - Existing Privacy and Copyright Enforcement: Institutional/arbitrage — formal structures persist but functional capacity degraded for AI cases; theater ratio rises
 *   - Regulatory Sunset Advocates: Organized/constrained — see exemption as temporary; expect new AI-specific governance frameworks to replace blanket exemptions within 3-5 years
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_ai_act_2026, 0.58).
domain_priors:suppression_score(sk_ai_act_2026, 0.68).
domain_priors:theater_ratio(sk_ai_act_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_ai_act_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(sk_ai_act_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sk_ai_act_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_ai_act_2026, tangled_rope).
narrative_ontology:human_readable(sk_ai_act_2026, "South Korea's Proposed AI Industry Promotion Act (2026)").
narrative_ontology:topic_domain(sk_ai_act_2026, "technological/political").

domain_priors:requires_active_enforcement(sk_ai_act_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, domestic_ai_developers).
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, south_korean_government_innovation_agencies).
narrative_ontology:constraint_victim(sk_ai_act_2026, data_subjects_citizens).
narrative_ontology:constraint_victim(sk_ai_act_2026, copyright_holders).
narrative_ontology:constraint_victim(sk_ai_act_2026, content_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Citizens cannot opt out of data collection for 'AI research' purposes under the exemption framework. No meaningful consent mechanism; cannot exit national jurisdiction without migration. Personal data extracted without compensation or control. Maximum extraction from trapped position.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COPYRIGHT HOLDERS/CONTENT CREATORS (SNARE) — Individual artists, writers, photographers cannot prevent their work from being used in training datasets under the exemption. Suppression is high: legal remedy is unavailable; collective organization faces government backing for AI developers. No exit path except leaving South Korea.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC AI DEVELOPERS (ROPE) — Primary beneficiaries experience the act as pure coordination: legal safe harbor solves collective action problem (all developers needed for ecosystem to compete globally). Extraction runs toward this group. Can arbitrage compliance costs and regulatory advantages globally. Net benefit.
constraint_indexing:constraint_classification(sk_ai_act_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL SOCIETY COALITION / TECH WORKERS (TANGLED ROPE) — Organized resistance groups (privacy advocates, labor unions) see both coordination benefit (South Korean tech sector needs competitive boost) AND asymmetric extraction (citizens pay the cost). Can mobilize politically but face institutional opposition. Constrained exit through domestic political action; some international alliance options. Mixed experience.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL TECH COMPANIES (TANGLED ROPE) — Global platforms (Meta, Google, OpenAI) gain training data from South Korean sources but face pressure from home-country regulators (EU, US) to comply with stricter standards. Experience mixed extraction (data value) and coordination benefit (South Korean data access). Mobile exit: can shift operations between jurisdictions based on regulatory arbitrage.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVACY/COPYRIGHT ENFORCEMENT (PITON) — Existing statutory frameworks (PIPA, copyright law, trade secret law) are formally intact but functionally degraded by selective exemptions for AI development. Enforcement machinery persists performatively; real enforcement has atrophied for AI-related data use. Institutional inertia maintains the framework despite reduced function. Theater ratio elevated because enforcement rituals continue for non-AI contexts while being suspended for AI.
constraint_indexing:constraint_classification(sk_ai_act_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY SUNSET ADVOCATES (SCAFFOLD) — Progressive advocates see the exemption as temporary bridge: accelerate AI development now, establish new governance frameworks later (AI-specific privacy law, creator compensation schemes). Extraction is tolerated because the constraint has built-in review mechanisms and planned sunset. Constrained exit through legislative re-engagement in 3-5 years.
constraint_indexing:constraint_classification(sk_ai_act_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / COMPETITIVE NECESSITY (MOUNTAIN) — From a civilizational perspective, some AI developers frame data access as a natural law of competitive advantage: without training data, South Korea cannot compete with Chinese and US models. The exemption appears as an inevitable response to structural necessity. Engine will flag this as a false summit: competitive necessity is a contextual claim about geopolitical incentives, not an immutable physical/logical law.
constraint_indexing:constraint_classification(sk_ai_act_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_ai_act_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_ai_act_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_ai_act_2026, TR),
    TR >= 0.70.

:- end_tests(sk_ai_act_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The act creates moderate-high extraction by conferring asymmetric benefit. AI developers gain unrestricted access to training data without compensation; citizens lose privacy rights without consent; creators lose compensation without approval. The extractiveness is moderate rather than severe (0.70+) because the constraint is framed as temporary (sunset clauses under discussion) and because South Korea has genuine global competitive pressure from China and the US. The extraction is real (data confiscation) but contextually defensible as industrial policy. Suppression (0.68): Significant. Citizens have no legal remedy for privacy violations under the exemption; creators cannot enforce copyright for training use; traditional enforcement mechanisms are suspended selectively for AI cases. But suppression is not total (0.80+) because: (1) enforcement for non-AI privacy violations continues; (2) organized groups retain political mobilization capacity; (3) international regulatory pressure exists. Theater ratio (0.55): Moderate and rising. The exemption is framed as a 'legal safe harbor' and 'research exemption,' which creates performative language around what is actually data extraction. Regulatory guidance and enforcement announcements maintain the theater that the system is fair and bounded, while actual implementation is expansive. Theater ratio rises from 0.40 to 0.55 over the interval as more AI companies claim exemptions and enforcement guidance expands the exemption's scope. Claimed type (Tangled Rope): The act is neither pure coordination (Rope) nor pure extraction (Snare). It solves a genuine collective action problem (individual developers cannot compete globally without data access; coordinated exemption enables ecosystem development) AND creates asymmetric extraction (citizens and creators bear the cost). Requires active enforcement: true, because the exemption framework must be actively maintained and expanded as new AI use cases emerge.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a wide perspectival gap because the structural benefit and cost are distributed asymmetrically across power levels. Institutional beneficiaries (government, major AI firms) perceive coordination and competitive necessity (Rope). Powerless victims (data subjects, individual creators) perceive pure extraction (Snare). Organized groups perceive the hybrid (Tangled Rope: coordination benefit for national tech, extraction cost for citizen privacy). International actors perceive arbitrage opportunity with regulatory risk (Tangled Rope). The existing enforcement system perceives its own degradation (Piton). Sunset advocates perceive a temporary bridge (Scaffold). The analytical observer risks seeing competitive necessity as natural law (false Mountain). The perspectival gap is structurally stable unless: (1) enforcement collapses entirely (regulatory capture), shifting more perspectives to Snare; (2) sunset mechanisms are activated and new governance replaces exemptions, shifting perspectives toward Scaffold; or (3) international retaliation closes the arbitrage exit for developers, shifting institutional perspective toward Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from the structural relationships: Domestic AI developers are beneficiaries with arbitrage exit options (can move operations globally if regulation tightens) → derive d ≈ 0.05-0.15 (low d, negative effective extraction for this group). Citizens are victims with trapped exit (cannot exit South Korea without migration; cannot reject data collection) → derive d ≈ 0.95 (high d, maximum experienced extraction). Organized civil society are constrained victims with mobilization capacity → derive d ≈ 0.60-0.70 (moderate-high). International tech companies are beneficiaries with mobile exit (can adjust presence based on regulatory arbitrage) → derive d ≈ 0.25-0.35 (low-moderate). The government agencies are institutional beneficiaries → derive d ≈ 0.05 (beneficiary status). The directionality pipeline produces differentiated chi values for each perspective, explaining why the same base properties generate Rope for developers, Snare for citizens, Tangled Rope for organized opposition, and Piton for enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy — the threat of mislabeling coordination as extraction (or vice versa) — is resolved by separating the genuine coordination function (South Korean developers need data access to compete globally; exemption solves collective action problem) from the asymmetric extraction (citizens and creators bear the privacy/compensation cost). The act is neither pure Rope (coordination only) nor pure Snare (extraction only). It is Tangled Rope because: (1) it solves a real coordination problem (all developers benefit from exemption relative to individual compliance costs); (2) it creates asymmetric extraction (citizens and creators subsidize the benefit). The Tangled Rope classification prevents the false narrative that the act is 'merely' coordination (benign) or 'merely' extraction (purely exploitative). It is both. The classification is stable across most perspectives except where institutional actors risk naturalizing the competitive necessity as an immutable law (false Mountain) or where the constraint is viewed as a deliberate sunset bridge (Scaffold). The resolved mandatrophy reveals that South Korean industrial policy is genuinely pursuing coordination — the national AI ecosystem is better off with exemption than without — but at the structural cost of imposing extraction on citizens and creators who cannot exit or organize effectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_use_boundary_definition,
    'What constitutes ''AI research'' under the exemption? Are commercial training datasets for product development covered, or only academic research?',
    'Legislative text analysis and regulatory guidance interpretation; tracking of actual data use cases approved under exemption framework',
    'If broad definition (includes commercial): extractiveness rises to 0.75+ (Snare from citizen perspective confirmed). If narrow (academic only): extractiveness drops to 0.30-0.35 (Scaffold from sunset perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_use_boundary_definition, conceptual, 'Boundary between exempt research vs. commercial AI development').

omega_variable(
    enforcement_likelihood_actual,
    'Will government enforcement of remaining privacy/copyright rules persist for politically connected AI developers, or will exemption effectively become de facto regulatory capture?',
    'Tracking enforcement actions against major AI firms over 24-36 month period; comparison of enforcement intensity before and after act passage; analysis of government conflict-of-interest patterns',
    'If enforcement persists: suppression remains at 0.68, tangled rope classification stable. If enforcement collapses: suppression rises to 0.80+, shifts to pure Snare from more perspectives (regulatory capture confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_likelihood_actual, empirical, 'Whether government will enforce privacy/copyright rules against connected AI developers').

omega_variable(
    international_regulatory_retaliation,
    'Will EU or US impose retaliatory trade/regulatory pressure on South Korean AI developers for violating their citizens'' data rights?',
    'Monitoring EU GDPR enforcement actions against South Korean firms; US FTC scrutiny of South Korean tech acquisitions; bilateral trade discussions',
    'If retaliation occurs: constrains actual benefit to South Korean developers (arbitrage exit closes), elevates effective extraction to 0.70+. Shifts institutional beneficiary perspective toward Snare. If no retaliation: South Korean developers capture asymmetric advantage, extraction remains at 0.58, tangled rope stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_retaliation, empirical, 'International regulatory response to AI data exemption').

omega_variable(
    creator_compensation_mechanism_viability,
    'Can a new creator compensation scheme (licensing, pooled royalties, data trusts) be designed and implemented before the initial exemption expires?',
    'Assessment of compensation model proposals; international precedents (Japan''s data broker framework, EU digital services regulations); implementation timeline analysis',
    'If viable: Scaffold sunset logic holds; creators experience temporary extraction. If not viable: exemption becomes structural/permanent, extractiveness stabilizes at 0.58+ for creators, tangled rope persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_compensation_mechanism_viability, conceptual, 'Feasibility of creator compensation frameworks to replace exemption').

omega_variable(
    citizen_resistance_mobilization,
    'Will organized citizen resistance (privacy advocacy, labor unions, opposition parties) achieve legislative reversal or substantive amendment before 3-5 year sunset?',
    'Tracking of public opposition campaigns, legislative amendment proposals, court challenges; sentiment analysis of civic engagement; polling on public support for exemption',
    'If mobilization succeeds: constraint may be dismantled or narrowed (scaffold perspective confirmed). If mobilization fails: citizens'' trapped exit option is confirmed, snare classification hardened for perspective 1.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_resistance_mobilization, empirical, 'Likelihood of citizen-driven legislative reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_ai_act_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sk_ai_tr_t0, sk_ai_act_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sk_ai_tr_t6, sk_ai_act_2026, theater_ratio, 6, 0.48).
narrative_ontology:measurement(sk_ai_tr_t12, sk_ai_act_2026, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(sk_ai_be_t0, sk_ai_act_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sk_ai_be_t6, sk_ai_act_2026, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(sk_ai_be_t12, sk_ai_act_2026, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_ai_act_2026, resource_allocation).
narrative_ontology:affects_constraint(sk_ai_act_2026, global_ai_training_data_asymmetry).
narrative_ontology:affects_constraint(sk_ai_act_2026, eu_ai_act_regulatory_divergence).
narrative_ontology:affects_constraint(sk_ai_act_2026, platform_copyright_liability_shift).

% DUAL FORMULATION NOTE:
% The AI Industry Promotion Act is a specific instantiation of a broader constraint family around data access and training data asymmetry. This story models the South Korean legislative framework at extractiveness 0.58 (moderate-high). The upstream constraint 'global_ai_training_data_asymmetry' (ε ≈ 0.72, Snare) models the global-level power imbalance in data access; the South Korean act is a policy response that partially mitigates South Korean developers' structural disadvantage but externalizes the cost to citizens. The downstream constraint 'eu_ai_act_regulatory_divergence' models the collision between South Korean exemptions and EU regulatory frameworks, creating arbitrage and retaliation risks. The family structure clarifies that the South Korean act is not an isolated policy but a tactical move in a larger game of regulatory arbitrage over training data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sk_ai_act_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
