% ============================================================================
% CONSTRAINT STORY: private_power_vs_subsidiarity_common_good
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_power_vs_subsidiarity_common_good, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: private_power_vs_subsidiarity_common_good
 *   human_readable: Private Power Concentration vs. Subsidiarity and Common Good in AI Governance
 *   domain: political_theology/technology_ethics/catholic_social_teaching
 *
 * SUMMARY:
 *   The concentration of AI infrastructure ownership in transnational tech
 *   oligopolies creates a structural violation of Catholic Social Teaching's
 *   subsidiarity principle and common good doctrine. Subsidiarity requires
 *   that decisions be made at the most local competent level, with higher
 *   levels providing support rather than substitution. The current constraint
 *   inverts this: platform owners make unilateral decisions about algorithmic
 *   curation, data use, content moderation, and labor conditions that affect
 *   billions, bypassing democratic accountability at national and local
 *   levels. The common good — understood in CST as the sum of social
 *   conditions enabling each person and group to reach fulfillment — is
 *   subordinated to shareholder value maximization. This is not a technology
 *   problem but a governance problem: the same AI capabilities could be
 *   organized through cooperative ownership, public infrastructure, or
 *   stakeholder governance models that preserve subsidiarity's participatory
 *   logic. The constraint exhibits rising extraction (0.45 → 0.68 over 25
 *   years) as platform lock-in deepens, rising suppression (0.38 → 0.72) as
 *   alternatives are foreclosed through network effects and regulatory
 *   capture, and rising theater (0.25 → 0.58) as corporate 'AI ethics'
 *   initiatives substitute for binding democratic governance. The magisterial
 *   analytical perspective classifies this as tangled_rope: genuine
 *   coordination function (technology enables human connection and
 *   knowledge-sharing) coexists with asymmetric extraction (private capture
 *   of socially-produced value, erosion of democratic sovereignty). The
 *   powerless perspective experiences pure snare: trapped in platform
 *   dependency with no exit.
 *
 * KEY AGENTS:
 *   - Tech Oligopolies (institutional/arbitrage): Primary beneficiaries — capture data rents, algorithmic control, and regulatory arbitrage across jurisdictions; experience constraint as coordination enabling profit maximization
 *   - Local Communities and Workers (powerless/trapped): Primary victims — subjected to algorithmic management, data extraction, and decision-making processes they cannot influence or exit; bear full cost of platform power concentration
 *   - Nation-States (moderate/constrained): Secondary victims — sovereignty eroded by regulatory arbitrage and lobbying asymmetry; can attempt regulation but face capital flight threats and technical complexity barriers
 *   - Civil Society Organizations (organized/constrained): Mixed position — benefit from platform coordination tools while experiencing extraction through algorithmic suppression and attention capture; funding dependencies limit autonomy
 *   - Digital Rights Coalitions (organized/mobile): Organized agents building alternative infrastructure (federated protocols, cooperative platforms, data trusts); see current concentration as temporary with viable exit path through institutional alternatives
 *   - Catholic Social Teaching Framework (analytical/analytical): Magisterial position recognizing technology's dual nature (gift and temptation) and diagnosing structural violation of subsidiarity and common good; classifies as tangled_rope requiring active enforcement to correct market concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_power_vs_subsidiarity_common_good, 0.68).
domain_priors:suppression_score(private_power_vs_subsidiarity_common_good, 0.72).
domain_priors:theater_ratio(private_power_vs_subsidiarity_common_good, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_power_vs_subsidiarity_common_good, extractiveness, 0.68).
narrative_ontology:constraint_metric(private_power_vs_subsidiarity_common_good, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(private_power_vs_subsidiarity_common_good, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_power_vs_subsidiarity_common_good, snare).
narrative_ontology:human_readable(private_power_vs_subsidiarity_common_good, "Private Power Concentration vs. Subsidiarity and Common Good in AI Governance").
narrative_ontology:topic_domain(private_power_vs_subsidiarity_common_good, "political_theology/technology_ethics/catholic_social_teaching").

domain_priors:requires_active_enforcement(private_power_vs_subsidiarity_common_good).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(private_power_vs_subsidiarity_common_good, '9010163d-0e9c-42ca-bbb3-7967511a25d0').
narrative_ontology:cs_kernel_codification('9010163d-0e9c-42ca-bbb3-7967511a25d0', formalized).
narrative_ontology:cs_authority_grounding('9010163d-0e9c-42ca-bbb3-7967511a25d0', lineage).
narrative_ontology:cs_interpretation_layer_present('9010163d-0e9c-42ca-bbb3-7967511a25d0').
narrative_ontology:cs_created_at('9010163d-0e9c-42ca-bbb3-7967511a25d0', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_power_vs_subsidiarity_common_good, tech_oligopolies).
narrative_ontology:constraint_beneficiary(private_power_vs_subsidiarity_common_good, platform_shareholders).
narrative_ontology:constraint_beneficiary(private_power_vs_subsidiarity_common_good, ai_infrastructure_owners).
narrative_ontology:constraint_victim(private_power_vs_subsidiarity_common_good, local_communities).
narrative_ontology:constraint_victim(private_power_vs_subsidiarity_common_good, nation_states).
narrative_ontology:constraint_victim(private_power_vs_subsidiarity_common_good, intermediary_civil_society_orgs).
narrative_ontology:constraint_victim(private_power_vs_subsidiarity_common_good, workers_displaced_by_automation).
narrative_ontology:constraint_victim(private_power_vs_subsidiarity_common_good, global_south_data_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL COMMUNITIES AND WORKERS (SNARE) — Trapped by platform dependency, algorithmic management, and data extraction with no meaningful exit. Cannot opt out of surveillance capitalism infrastructure that mediates employment, commerce, and civic participation. Maximum extraction: labor value captured, decision-making authority stripped, no alternative infrastructure available.
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATION-STATES (SNARE) — Constrained by regulatory arbitrage, lobbying power asymmetry, and technical complexity barriers. Can attempt regulation but face capital flight threats, platform market power, and capture of standard-setting bodies. Substantial extraction: sovereignty eroded, tax base hollowed, democratic accountability bypassed.
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL SOCIETY ORGS (TANGLED ROPE) — Organized but constrained by funding dependencies and platform mediation of organizing capacity. Benefit from coordination tools (social media for mobilization) while simultaneously experiencing extraction (algorithmic suppression, data harvesting, attention capture). Mixed coordination and extraction: the tools that enable organizing are owned by the extractors.
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: TECH OLIGOPOLIES (ROPE) — Primary beneficiaries with arbitrage-grade exit across jurisdictions. Experience the constraint as pure coordination: standards enable interoperability, network effects create value, regulatory fragmentation allows forum-shopping. Extraction flows toward this agent, not away. Effective extraction is negative (subsidy).
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL RIGHTS COALITIONS (SCAFFOLD) — Organized agents building alternative infrastructure (federated protocols, cooperative platforms, open-source AI models, data trusts). See current concentration as temporary coordination failure with sunset logic: GDPR, Digital Markets Act, cooperative platform movement, and municipal broadband represent transitional mechanisms toward democratized tech governance. Moderate extraction because coalition has agency and sees viable exit path through institutional alternatives.
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CATHOLIC SOCIAL TEACHING ANALYTICAL (TANGLED ROPE) — From the magisterial analytical position, the constraint exhibits both genuine coordination function (technology enables human flourishing, communication, knowledge-sharing) AND asymmetric extraction that violates subsidiarity (decision-making concentrated at global corporate level, bypassing local/national democratic participation) and common good (private profit prioritized over universal access and human dignity). The teaching recognizes technology's dual nature: gift and temptation. Requires active enforcement to maintain because market forces alone concentrate power upward, contradicting subsidiarity's participatory logic.
constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_power_vs_subsidiarity_common_good_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_power_vs_subsidiarity_common_good, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_power_vs_subsidiarity_common_good, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(private_power_vs_subsidiarity_common_good_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Tech oligopolies capture the majority of value created through network effects, data accumulation, and algorithmic optimization, while workers, users, and communities bear costs (precarity, surveillance, democratic erosion) without proportional benefit. The value is not 0.85+ (maximum snare) because some genuine coordination and innovation occurs — the platforms do enable real communication and knowledge-sharing. But the asymmetry is severe: shareholders capture rents from socially-produced data and network effects that subsidiarity logic would allocate to contributing communities. Suppression (0.72): High. Alternatives are systematically foreclosed through network effects (switching costs), intellectual property barriers (proprietary algorithms), regulatory capture (lobbying against interoperability mandates), and capital requirements (AI infrastructure costs). Exit options exist in theory (cooperative platforms, municipal broadband, open-source models) but face coordination problems and resource asymmetries that make them non-viable for most actors. The suppression is structural rather than merely economic. Theater ratio (0.58): Moderate-high. Corporate 'AI ethics' initiatives, stakeholder advisory boards, and transparency reports are substantially performative — they create appearance of accountability without binding constraints on extraction. Real governance would require enforceable rights (data portability, algorithmic transparency, worker co-determination) and ownership restructuring (cooperative models, public infrastructure, antitrust breakup). The theater has increased over the interval as 'responsible AI' discourse has proliferated while ownership concentration has deepened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position. Tech oligopolies with arbitrage exit see pure coordination (rope): platforms solve real problems, create value, and enable innovation. Their effective extraction is negative — they are net beneficiaries. Local communities and workers with trapped exit see pure extraction (snare): algorithmic management strips autonomy, data harvesting captures value without consent, platform dependency eliminates alternatives. Their effective extraction is maximum. Nation-states with constrained exit see snare: sovereignty eroded, tax base hollowed, democratic accountability bypassed, but some regulatory capacity remains. Civil society organizations experience tangled_rope: the tools that enable organizing (social media for mobilization) are owned by the extractors (algorithmic suppression, attention capture). Digital rights coalitions see scaffold: current concentration is temporary, alternatives are being built, sunset logic applies. The Catholic Social Teaching analytical framework sees tangled_rope at the civilizational level: genuine coordination function coexists with structural violation of subsidiarity and common good, requiring active enforcement (regulation, ownership restructuring) to correct market concentration that contradicts participatory logic. The gap between the oligopoly's rope and the community's snare is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Tech oligopolies are primary beneficiaries: they own the infrastructure, capture the rents, and have arbitrage exit across jurisdictions. The engine derives d ≈ 0.0-0.15 (full beneficiary) from beneficiary status + institutional power + arbitrage exit, producing negative effective extraction (subsidy). Local communities and workers are primary victims: they generate the data, perform the labor, and bear the costs (precarity, surveillance, autonomy loss) with no exit. The engine derives d ≈ 0.85-1.0 (full target) from victim status + powerless position + trapped exit, producing maximum effective extraction. Nation-states are secondary victims with moderate power and constrained exit: d ≈ 0.60-0.75, producing high but not maximum extraction. Civil society organizations are mixed: listed as victims but with organized power and constrained exit, d ≈ 0.50-0.65, producing moderate extraction — they experience both coordination (platform tools enable organizing) and extraction (algorithmic suppression, funding dependencies). Digital rights coalitions are organized with mobile exit and see themselves as building alternatives: d ≈ 0.30-0.45, producing low-moderate extraction — they have agency and exit paths. The analytical perspective (CST framework) is not a beneficiary or victim in the material sense but occupies the analytical position that sees the full structure: genuine coordination function coexisting with asymmetric extraction that violates subsidiarity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that classification depends on structural position, not on discovering a single 'true' type. From the oligopoly's seat, the constraint is rope: platforms coordinate communication, enable commerce, and create value through network effects. This is their genuine experience — extraction flows toward them, not away. From the community's seat, the constraint is snare: platform dependency is inescapable, algorithmic control strips autonomy, and democratic participation is bypassed. This is also genuine — they are trapped with no exit. From the CST analytical seat, the constraint is tangled_rope: technology's coordination function (gift) coexists with ownership concentration that violates subsidiarity and common good (temptation). All three classifications are structurally valid measurements from their respective observation sites. The mandatrophy is not 'which type is correct?' but 'which perspective reveals which aspect of the structure?' The presheaf over the observation site IS the answer. The divergence between claimed_type (snare, from the victim's perspective) and the analytical classification (tangled_rope, recognizing dual nature) is not an error — it is the diagnostic signal that the constraint operates differently for different agents, and that the 'pure extraction' framing from below coexists with 'mixed coordination-extraction' framing from the analytical position that sees both the genuine coordination function and the structural violation of participatory logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidiarity_scale_threshold,
    'At what scale does technological infrastructure become inherently non-subsidiary — requiring coordination above the local level vs. representing illegitimate concentration?',
    'Historical analysis of successful cooperative/municipal ownership of infrastructure (electricity grids, water systems, internet provision); identification of technical vs. political barriers to decentralization',
    'If threshold is low (local/regional): current concentration is pure extraction, violates subsidiarity categorically. If threshold is high (continental/global): some concentration is legitimate coordination, making the constraint tangled_rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_scale_threshold, empirical, 'Scale threshold for legitimate vs. illegitimate technological coordination').

omega_variable(
    market_vs_political_construction,
    'Is platform concentration a natural market outcome (network effects, economies of scale) or a politically constructed monopoly (IP law, regulatory capture, state subsidy of R&D)?',
    'Counterfactual policy analysis: would alternative IP regimes, antitrust enforcement, or public infrastructure investment have produced different ownership structures? Quantify role of state subsidy (DARPA funding, university research, tax incentives) in creating private platform value.',
    'If natural: mountain from some perspectives (inherent to technology). If constructed: snare from more perspectives (political choice masquerading as necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_vs_political_construction, empirical, 'Whether platform concentration is market-natural or politically constructed').

omega_variable(
    cooperative_platform_viability,
    'Can cooperative/municipal ownership models achieve technical performance and user adoption comparable to investor-owned platforms, or do they face insurmountable coordination problems?',
    'Comparative analysis of cooperative platforms (Mastodon, platform.coop members, municipal broadband) vs. investor-owned equivalents on metrics of reliability, feature development, user growth, and financial sustainability',
    'If viable: scaffold perspective confirmed — alternatives exist and sunset is real. If unviable: current concentration may be coordination necessity (rope from more perspectives) rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_platform_viability, empirical, 'Technical and economic viability of cooperative platform alternatives').

omega_variable(
    common_good_operationalization,
    'How is ''common good'' operationalized in technology governance — who defines it, through what process, and how are trade-offs adjudicated when universal access conflicts with innovation incentives?',
    'Comparative institutional analysis: participatory technology assessment models (Danish consensus conferences, citizen assemblies on AI), stakeholder governance structures, and their actual influence on platform design vs. shareholder primacy',
    'If operationalizable through democratic process: snare classification strengthened (current structure bypasses available mechanisms). If inherently contested with no resolution procedure: the constraint may be conceptually under-determined (preference omega rather than empirical).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_good_operationalization, conceptual, 'Operationalization and adjudication mechanisms for common good in tech governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_power_vs_subsidiarity_common_good, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(privpwr_theater_2000, private_power_vs_subsidiarity_common_good, theater_ratio, 0, 0.25).
narrative_ontology:measurement(privpwr_theater_2005, private_power_vs_subsidiarity_common_good, theater_ratio, 5, 0.32).
narrative_ontology:measurement(privpwr_theater_2010, private_power_vs_subsidiarity_common_good, theater_ratio, 10, 0.42).
narrative_ontology:measurement(privpwr_theater_2015, private_power_vs_subsidiarity_common_good, theater_ratio, 15, 0.5).
narrative_ontology:measurement(privpwr_theater_2020, private_power_vs_subsidiarity_common_good, theater_ratio, 20, 0.55).
narrative_ontology:measurement(privpwr_theater_2025, private_power_vs_subsidiarity_common_good, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(privpwr_extract_2000, private_power_vs_subsidiarity_common_good, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(privpwr_extract_2005, private_power_vs_subsidiarity_common_good, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(privpwr_extract_2010, private_power_vs_subsidiarity_common_good, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(privpwr_extract_2015, private_power_vs_subsidiarity_common_good, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(privpwr_extract_2020, private_power_vs_subsidiarity_common_good, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(privpwr_extract_2025, private_power_vs_subsidiarity_common_good, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(privpwr_suppress_2000, private_power_vs_subsidiarity_common_good, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(privpwr_suppress_2005, private_power_vs_subsidiarity_common_good, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(privpwr_suppress_2010, private_power_vs_subsidiarity_common_good, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(privpwr_suppress_2015, private_power_vs_subsidiarity_common_good, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(privpwr_suppress_2020, private_power_vs_subsidiarity_common_good, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(privpwr_suppress_2025, private_power_vs_subsidiarity_common_good, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_power_vs_subsidiarity_common_good, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_dignity (the broader paradigm that treats efficiency and control as primary values, subordinating human dignity). The upstream constraint establishes the ideological framework; this constraint instantiates it in the specific domain of AI infrastructure ownership and governance. The two constraints have different ε values: the upstream paradigm is more diffuse and ideological (tangled_rope with moderate extraction), while this ownership concentration constraint is more materially extractive (snare from victim perspectives, tangled_rope from analytical). They are linked but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
