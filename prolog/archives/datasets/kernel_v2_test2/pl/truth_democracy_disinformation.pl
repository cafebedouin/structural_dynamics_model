% ============================================================================
% CONSTRAINT STORY: truth_democracy_disinformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_truth_democracy_disinformation, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: truth_democracy_disinformation
 *   human_readable: AI-Generated Disinformation Erosion of Democratic Epistemic Commons
 *   domain: political_theology/technology_ethics/information_integrity
 *
 * SUMMARY:
 *   The AI-generated disinformation constraint represents the collision
 *   between generative AI capabilities (text, image, video synthesis at
 *   scale) and the epistemic foundations required for democratic
 *   deliberation. Catholic Social Teaching frames this as a crisis of human
 *   dignity: when citizens cannot distinguish truth from fabrication, they
 *   lose the capacity for informed consent that grounds democratic legitimacy
 *   and personal autonomy. The encyclical Antiqua et Nova positions this
 *   within the broader 'technocratic paradigm' critique — the reduction of
 *   truth to instrumental utility and information to commodity. The
 *   constraint exhibits rising extraction over the 2016-2024 interval as
 *   generative AI capabilities outpaced detection and regulatory responses.
 *   Platform companies and authoritarian regimes are primary beneficiaries,
 *   extracting attention, data, and geopolitical advantage. Democratic
 *   publics and marginalized communities are primary victims, bearing
 *   epistemic costs without exit options. The constraint's theater_ratio
 *   (0.58) reflects substantial performative content: platform content
 *   moderation policies, fact-checking partnerships, and voluntary AI ethics
 *   commitments perform accountability while preserving engagement-maximizing
 *   algorithms that amplify disinformation. The suppression_requirement
 *   trajectory shows enforcement intensification: as public awareness grows,
 *   platforms and state actors invest more heavily in suppressing
 *   alternatives (public digital infrastructure, mandatory content
 *   provenance, platform liability reforms) through lobbying, regulatory
 *   capture, and forum shopping.
 *
 * KEY AGENTS:
 *   - Democratic Publics: Primary victim (powerless/trapped) — cannot exit platform-mediated information ecosystems; bear full epistemic cost of synthetic media saturation
 *   - Marginalized Communities: Primary victim (powerless/identity_locked) — targeted by identity-specific disinformation campaigns; cognitively trapped by synthetic media exploiting existing vulnerabilities
 *   - Journalists/Fact-Checkers: Secondary victim (moderate/constrained) — resource asymmetry (verification expensive, generation cheap); benefit from verification ecosystem but bear extraction through burnout and platform dependency
 *   - Platform Companies: Primary beneficiary (institutional/arbitrage) — extract advertising revenue and data from disinformation-amplifying engagement; arbitrage regulatory jurisdictions
 *   - Authoritarian Regimes: Primary beneficiary (institutional/arbitrage) — weaponize epistemic chaos in adversary democracies while suppressing it domestically
 *   - Disinformation Entrepreneurs: Primary beneficiary (powerful/mobile) — monetize synthetic media generation and bot networks; mobile across jurisdictions
 *   - EU Digital Services Regulators: Organized actor (organized/constrained) — building transitional frameworks (DSA, AI Act) with sunset logic; constrained by jurisdictional limits
 *   - Catholic Magisterium: Institutional actor (institutional/constrained) — provides moral framework but lacks enforcement power; benefits from vindication of technocratic paradigm warnings but bears extraction as framework is ignored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(truth_democracy_disinformation, 0.68).
domain_priors:suppression_score(truth_democracy_disinformation, 0.72).
domain_priors:theater_ratio(truth_democracy_disinformation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(truth_democracy_disinformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(truth_democracy_disinformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(truth_democracy_disinformation, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(truth_democracy_disinformation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(truth_democracy_disinformation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(truth_democracy_disinformation, snare).
narrative_ontology:human_readable(truth_democracy_disinformation, "AI-Generated Disinformation Erosion of Democratic Epistemic Commons").
narrative_ontology:topic_domain(truth_democracy_disinformation, "political_theology/technology_ethics/information_integrity").

domain_priors:requires_active_enforcement(truth_democracy_disinformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(truth_democracy_disinformation, '84daadd0-4349-478f-8195-5a1e204fab9f').
narrative_ontology:cs_kernel_codification('84daadd0-4349-478f-8195-5a1e204fab9f', formalized).
narrative_ontology:cs_authority_grounding('84daadd0-4349-478f-8195-5a1e204fab9f', lineage).
narrative_ontology:cs_interpretation_layer_present('84daadd0-4349-478f-8195-5a1e204fab9f').
narrative_ontology:cs_created_at('84daadd0-4349-478f-8195-5a1e204fab9f', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(truth_democracy_disinformation, platform_companies).
narrative_ontology:constraint_beneficiary(truth_democracy_disinformation, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(truth_democracy_disinformation, disinformation_entrepreneurs).
narrative_ontology:constraint_victim(truth_democracy_disinformation, democratic_publics_vulnerable_to_manipulation).
narrative_ontology:constraint_victim(truth_democracy_disinformation, marginalized_communities_targeted_by_synthetic_media).
narrative_ontology:constraint_victim(truth_democracy_disinformation, journalists_fact_checkers).
narrative_ontology:constraint_vindicates(truth_democracy_disinformation, technocratic_paradigm_dominance).
narrative_ontology:constraint_vindicates(truth_democracy_disinformation, information_as_commodity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens in democratic societies who consume information through platform-mediated channels. Cannot distinguish AI-generated synthetic media from authentic content without technical expertise. Trapped in information ecosystems optimized for engagement over truth. Bear epistemic costs (inability to form informed political judgments, erosion of trust in institutions, vulnerability to manipulation) without compensation. No exit from platform-mediated public sphere without losing access to civic participation.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, democratic_publics_vulnerable_to_manipulation, payer,
    powerless, immediate, trapped, global).

% Communities defined by race, religion, immigration status, or other identity markers who are disproportionately targeted by synthetic media campaigns. Identity-locked because disinformation exploits existing social identities — exit would require abandoning the identity itself. Structurally mobile in principle but cognitively trapped by identity-targeted deepfakes, fabricated evidence, and coordinated harassment campaigns. Bear compounded extraction: epistemic harm plus identity-based violence.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, marginalized_communities_targeted_by_synthetic_media, payer,
    powerless, biographical, identity_locked, national).

% Professional journalists and fact-checking organizations who verify claims and debunk disinformation. Constrained by resource asymmetry: verification is expensive and time-consuming, generation is cheap and instant. Benefit from verification ecosystem (professional networks, forensic tools, institutional support) but also bear extraction (burnout from volume of false claims, harassment from disinformation networks, platform dependency for distribution). Can exit the profession but at high career cost.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, journalists_fact_checkers, payer,
    moderate, biographical, constrained, national).

% Social media platforms and content distribution companies (Meta, X/Twitter, YouTube, TikTok) that host and algorithmically amplify user-generated content including disinformation. Primary beneficiaries: extract advertising revenue and user data from engagement-maximizing algorithms that amplify disinformation. Arbitrage exit: can relocate operations, forum-shop regulatory jurisdictions, externalize moderation costs to contractors. Experience the constraint as coordination: content moderation theater satisfies regulators while preserving core business model.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, platform_companies, beneficiary,
    institutional, immediate, arbitrage, global).

% State actors (Russia, China, Iran, others) that deploy AI-generated disinformation to destabilize adversary democracies while suppressing it domestically. Beneficiaries of epistemic chaos in target countries: undermines democratic legitimacy, polarizes populations, erodes trust in institutions. Arbitrage exit: can deploy disinformation across borders while maintaining domestic information control. Experience the constraint as coordination tool: synthetic media enables narrative control and geopolitical influence at scale.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, authoritarian_regimes, beneficiary,
    institutional, biographical, arbitrage, national).

% Commercial actors who monetize synthetic media generation: bot networks, deepfake-for-hire services, coordinated inauthentic behavior campaigns. Extract revenue from clients (political campaigns, corporate interests, state actors) who purchase disinformation services. Mobile across jurisdictions: can relocate operations to avoid enforcement. Experience the constraint as market opportunity: AI tools lower production costs and increase scale.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, disinformation_entrepreneurs, beneficiary,
    powerful, immediate, mobile, global).

% European Union regulatory bodies implementing Digital Services Act and AI Act provisions for content authenticity and platform accountability. Organized institutional actors building transitional frameworks: mandatory content provenance markers, platform liability for amplified disinformation, algorithmic transparency requirements. Constrained by jurisdictional limits (cannot regulate platforms headquartered outside EU) and enforcement capacity (limited resources relative to platform legal teams). See the constraint as temporary coordination failure with sunset logic: regulatory frameworks will restore epistemic commons.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, eu_digital_services_regulators, agenda_setter,
    organized, generational, constrained, continental).

% Catholic teaching authority (Pope, councils, tradition) providing moral framework for AI governance through encyclicals and social doctrine. Institutional actor with constrained exit: cannot abandon teaching mission but lacks enforcement power over secular technology governance. Benefits from the constraint insofar as epistemic crisis vindicates CST warnings about technocratic paradigm reducing truth to utility. But also bears extraction: disinformation undermines the Church's own communicative authority and pastoral mission. Provides moral framework (human dignity, common good, solidarity) but framework is often ignored by those with power to act.
narrative_ontology:constraint_stakeholder(truth_democracy_disinformation, catholic_magisterium, observer,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates information distribution at planetary scale, enabling rapid dissemination of news, cultural content, and civic discourse across geographic and linguistic boundaries. Platforms provide infrastructure for public communication that would be prohibitively expensive for individuals or small organizations to build independently.
% TRANSFER_FUNCTION: The arrangement transfers attention (user engagement time), data (behavioral profiles, social graphs, content preferences), and advertising revenue from users to platform companies. It transfers epistemic authority from traditional gatekeepers (journalists, editors, institutions) to algorithmic curation systems. It transfers geopolitical influence from stable democracies to authoritarian regimes deploying disinformation campaigns.
% ABSENT_VOICES: Democratic publics whose epistemic commons is being eroded are present in the conversation but structurally powerless — their objections are registered (surveys show declining trust in information sources) but not determinative of platform design or regulatory outcomes. Marginalized communities targeted by synthetic media are often absent from policy debates entirely — their vulnerability is discussed by advocates but they rarely have direct voice in governance decisions. Future generations who will inherit degraded epistemic infrastructure are entirely absent. The Catholic Church and other religious institutions are present as moral witnesses but lack enforcement power.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight — if AI-generated disinformation became technically impossible or platforms were legally prohibited from amplifying it — multiple arrangements would rearrange. Platform business models would shift from engagement-maximization to other revenue sources (subscriptions, public funding, cooperative ownership). Authoritarian regimes would lose a key geopolitical influence tool and would need to develop alternative methods. Disinformation entrepreneurs would lose their market. Journalists and fact-checkers would face different work (less debunking, more investigative reporting). Democratic publics would regain capacity for informed deliberation, though trust restoration would take time. The world would rearrange because the constraint is not a natural fact but an engineered system that multiple actors depend on for their current arrangements.
% FOUNDING_PROBLEM: The founding problem was genuine: how to enable global information sharing and public discourse at scale in the internet age, when traditional gatekeepers (newspapers, broadcasters) could not scale to meet demand and had their own biases and exclusions. Early internet optimism held that distributed, user-generated content would democratize information access and empower marginalized voices. Platforms were built to solve the coordination problem of connecting billions of people for communication and information exchange.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies claim the founding problem remains live: billions of people still need infrastructure for global communication, and platforms provide it. Civil society critics (EFF, Access Now, Mozilla) acknowledge the coordination need but argue the founding problem has been solved in a way that created new, worse problems (surveillance capitalism, algorithmic amplification of extremism, epistemic crisis). The Catholic Magisterium (Antiqua et Nova encyclical) argues the founding problem was misconceived: treating information as commodity rather than epistemic commons serving human dignity was the original error, and the current crisis is the predictable result. EU regulators take a middle position: the coordination need is real but the current implementation is extractive and requires structural reform. The status is contested because parties disagree on whether the original problem (global communication infrastructure) is separable from the current implementation (engagement-maximizing algorithms that amplify disinformation).
narrative_ontology:disappearance_verdict(truth_democracy_disinformation, world_rearranges).
narrative_ontology:founding_problem_status(truth_democracy_disinformation, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC PUBLICS (SNARE) — Trapped in information ecosystems designed for engagement over truth. Cannot distinguish synthetic from authentic content without technical expertise. No exit from platform-mediated public sphere. Bears full epistemic cost of disinformation while platforms extract attention and data. Maximum experienced extraction.
constraint_indexing:constraint_classification(truth_democracy_disinformation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Identity-locked because targeted disinformation exploits existing social identities (race, religion, immigration status). Structurally mobile in principle but cognitively trapped by identity-targeted synthetic media campaigns. Disproportionate victims of deepfake harassment and fabricated evidence. Extraction compounds existing vulnerabilities.
constraint_indexing:constraint_classification(truth_democracy_disinformation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: JOURNALISTS/FACT-CHECKERS (TANGLED ROPE) — Constrained by resource asymmetry: verification is expensive, generation is cheap. Benefit from the verification ecosystem (professional networks, forensic tools) but also bear extraction (burnout, harassment, platform dependency). Mixed coordination and extraction: the system needs their labor but does not adequately resource it.
constraint_indexing:constraint_classification(truth_democracy_disinformation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM COMPANIES (ROPE) — Primary beneficiaries. Arbitrage exit: can relocate operations, forum-shop regulations, externalize moderation costs. Experience the constraint as coordination: content moderation theater satisfies regulators while preserving engagement-maximizing algorithms. Net beneficiaries of the disinformation economy through advertising revenue and data extraction.
constraint_indexing:constraint_classification(truth_democracy_disinformation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTHORITARIAN REGIMES (ROPE) — Beneficiaries of epistemic chaos in adversary democracies. Arbitrage exit: can deploy disinformation across borders while suppressing it domestically. Experience the constraint as coordination tool: synthetic media enables narrative control at scale. Low effective extraction because they are extractors, not targets.
constraint_indexing:constraint_classification(truth_democracy_disinformation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EU REGULATORS (SCAFFOLD) — Organized institutional actors building transitional frameworks (DSA, AI Act content provenance requirements). See the constraint as temporary coordination failure with sunset logic: mandatory content authenticity markers, platform liability, and algorithmic transparency will restore epistemic commons. Constrained by jurisdictional limits and enforcement capacity but have agency and see exit path.
constraint_indexing:constraint_classification(truth_democracy_disinformation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: MAGISTERIAL AUTHORITY (TANGLED ROPE) — Institutional actor with constrained exit (cannot abandon teaching mission but lacks enforcement power over secular tech governance). Benefits from the constraint insofar as epistemic crisis vindicates CST warnings about technocratic paradigm, but also bears extraction: disinformation undermines the Church's own communicative authority and pastoral mission. Mixed coordination (provides moral framework) and extraction (framework is ignored by those with power to act).
constraint_indexing:constraint_classification(truth_democracy_disinformation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From civilizational/universal perspective, the constraint is pure extraction masquerading as coordination. The 'marketplace of ideas' framing naturalizes what is actually an engineered epistemic crisis. Platforms claim to coordinate public discourse while systematically amplifying disinformation for profit. Regulatory theater (content moderation policies, fact-checking partnerships) performs accountability without structural change. Alternatives (public digital infrastructure, content authenticity standards, platform liability) are suppressed through lobbying and regulatory capture. Genuine coordination function is negligible; extraction is substantial and asymmetric.
constraint_indexing:constraint_classification(truth_democracy_disinformation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(truth_democracy_disinformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(truth_democracy_disinformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(truth_democracy_disinformation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(truth_democracy_disinformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(truth_democracy_disinformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platform companies extract attention and data by amplifying disinformation for engagement. Authoritarian regimes extract geopolitical advantage by destabilizing adversary democracies. Disinformation entrepreneurs extract revenue from synthetic media generation. Democratic publics and marginalized communities bear the epistemic costs without compensation. The extraction is asymmetric and substantial. Suppression (0.72): High. Alternatives to the current information ecosystem (public digital infrastructure, mandatory content authenticity standards, platform liability for amplified disinformation) are actively suppressed through lobbying, regulatory capture, and jurisdictional arbitrage. Citizens cannot exit platform-mediated public spheres without losing access to civic participation. The suppression has intensified as regulatory pressure has grown — platforms invest more heavily in preserving the extractive core while performing compliance. Theater ratio (0.58): Moderate-high. Content moderation policies are substantially performative: platforms announce high-profile takedowns and fact-checking partnerships while algorithmic amplification of disinformation continues. Voluntary AI ethics commitments perform accountability without binding enforcement. The theater has increased as public awareness has grown, requiring more sophisticated performance. Accessibility collapse (0.35): Low-moderate. Alternatives remain partially accessible: public broadcasting, nonprofit journalism, open-source verification tools, regulatory frameworks in some jurisdictions. The collapse is incomplete because the constraint is contested — civil society, some regulators, and the Magisterium actively resist naturalization. Resistance (0.62): Moderate-high. Significant active resistance from journalists, fact-checkers, civil society organizations, EU regulators, and religious institutions including the Catholic Church. The resistance is real but asymmetrically resourced compared to platform lobbying power.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Platform companies and authoritarian regimes see coordination (Rope) — they are solving the 'problem' of information distribution and narrative control at scale, and they are net beneficiaries. EU regulators see a temporary coordination failure with a sunset (Scaffold) — transitional frameworks will restore epistemic commons. Journalists and the Magisterium see mixed coordination and extraction (Tangled Rope) — the system needs their labor (verification, moral framework) but does not adequately resource it and often ignores it. Democratic publics and marginalized communities see pure extraction (Snare) — they are trapped in information ecosystems designed to exploit them, with no exit and no voice. The analytical observer sees the 'marketplace of ideas' framing as naturalization of an engineered crisis (Snare with false coordination cover). The gap between the platform/regime Rope perspective and the public Snare perspective is the core measurement: beneficiaries experience coordination while victims experience extraction from the same structural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Platform companies and authoritarian regimes are primary beneficiaries with arbitrage exit options — they collect from the constraint (advertising revenue, data, geopolitical advantage) and can relocate operations to avoid regulation. Their directionality is low (near 0.0), producing negative or near-zero effective extraction (they experience subsidy, not cost). Democratic publics are primary victims with trapped exit options — they bear epistemic costs, cannot distinguish synthetic from authentic content without expertise, and cannot exit platform-mediated public spheres without losing civic participation. Their directionality is high (near 1.0), producing maximum effective extraction. Marginalized communities are victims with identity_locked exit — structurally mobile in principle but cognitively trapped by identity-targeted synthetic media. Their directionality is high, amplified by the identity lock. Journalists and fact-checkers are secondary victims with constrained exit — they face resource asymmetry and platform dependency but have some agency. Their directionality is moderate (0.4-0.6), producing moderate effective extraction. The Magisterium is an institutional actor with constrained exit — cannot abandon teaching mission but lacks enforcement power. Its directionality is moderate, reflecting mixed benefit (vindication of warnings) and extraction (framework ignored). EU regulators are organized actors with constrained exit — building frameworks but limited by jurisdiction. Their directionality is lower (0.3-0.4) because they have agency and see an exit path, producing lower effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the same structural arrangement (AI-generated disinformation in platform-mediated information ecosystems) is simultaneously coordination for beneficiaries and extraction for victims. Platform companies genuinely coordinate information distribution at scale — that is a real function. Authoritarian regimes genuinely coordinate narrative control — that is a real function from their perspective. But the coordination is asymmetric: it serves the coordinators' interests while extracting from democratic publics who bear epistemic costs without consent or compensation. The Tangled Rope classification at the analytical level captures this: there is a genuine coordination function (information distribution) AND asymmetric extraction (epistemic harm to publics, suppression of alternatives). The Snare classification from the victim perspective is also correct — they experience pure extraction because they do not benefit from the coordination and cannot exit. The Scaffold classification from the EU regulator perspective is a structural hypothesis — if transitional frameworks succeed in restoring epistemic commons, the constraint will sunset. The Rope classification from the beneficiary perspective is their genuine experience — they are net beneficiaries. All classifications are true from their respective perspectives. The mandatrophy is resolved by recognizing that 'coordination vs. extraction' is not a binary property of the constraint but a perspectival measurement that varies with the observer's structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_authenticity_sufficiency,
    'Do technical content authenticity standards (C2PA, cryptographic provenance) constitute sufficient epistemic repair, or do they merely shift the attack surface?',
    'Longitudinal tracking of synthetic media detection rates and adversarial evasion techniques post-standard deployment; measurement of public trust restoration in jurisdictions with mandatory provenance vs. without',
    'If sufficient: scaffold perspective confirmed, regulatory sunset is real. If insufficient: technical standards are themselves theater, and the snare persists under new cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_authenticity_sufficiency, empirical, 'Whether technical authenticity standards provide genuine epistemic repair').

omega_variable(
    platform_liability_threshold,
    'At what threshold of platform liability for amplified disinformation does the business model become non-viable, forcing structural change vs. mere compliance theater?',
    'Comparative analysis of platform behavior under different liability regimes (Section 230 immunity vs. DSA conditional immunity vs. publisher liability); correlation between liability exposure and algorithmic transparency',
    'If threshold is low: current regulatory approaches are sufficient (scaffold). If threshold is high: platforms will perform compliance while preserving extractive core (snare persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_liability_threshold, empirical, 'Liability threshold required to force structural vs. theatrical platform change').

omega_variable(
    magisterial_authority_secular_reach,
    'Does Catholic Social Teaching possess structural authority over secular AI governance, or is its role limited to moral witness without enforcement power?',
    'Historical analysis of CST influence on technology policy (precedents: bioethics, labor rights, environmental regulation); measurement of encyclical citation in legislative debates and regulatory frameworks',
    'If structural authority exists: Magisterium is a coordination actor (rope/scaffold). If limited to witness: Magisterium is itself a victim of the constraint (tangled_rope/snare) — its framework is vindicated by crisis but ignored by those with power to act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_secular_reach, conceptual, 'Whether CST has structural vs. merely moral authority in secular tech governance').

omega_variable(
    epistemic_commons_restoration_timeline,
    'Is the erosion of shared epistemic foundations reversible within a generation, or has the damage crossed an irreversible threshold?',
    'Measurement of cross-partisan agreement on basic factual claims over time; longitudinal trust in institutions surveys; generational cohort analysis of media literacy and epistemic resilience',
    'If reversible: scaffold logic holds, transitional frameworks can restore commons. If irreversible: the constraint is not a coordination failure but a permanent extraction mechanism (snare with no exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_restoration_timeline, empirical, 'Whether epistemic commons erosion is reversible within generational timeframe').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(truth_democracy_disinformation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(truth_dem_theater_2016, truth_democracy_disinformation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(truth_dem_theater_2018, truth_democracy_disinformation, theater_ratio, 2, 0.45).
narrative_ontology:measurement(truth_dem_theater_2020, truth_democracy_disinformation, theater_ratio, 4, 0.52).
narrative_ontology:measurement(truth_dem_theater_2022, truth_democracy_disinformation, theater_ratio, 6, 0.56).
narrative_ontology:measurement(truth_dem_theater_2024, truth_democracy_disinformation, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(truth_dem_extract_2016, truth_democracy_disinformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(truth_dem_extract_2018, truth_democracy_disinformation, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(truth_dem_extract_2020, truth_democracy_disinformation, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(truth_dem_extract_2022, truth_democracy_disinformation, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(truth_dem_extract_2024, truth_democracy_disinformation, base_extractiveness, 8, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(truth_dem_suppress_2016, truth_democracy_disinformation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(truth_dem_suppress_2020, truth_democracy_disinformation, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(truth_dem_suppress_2024, truth_democracy_disinformation, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(truth_democracy_disinformation, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_primacy. The upstream constraint establishes the broader structural tension between instrumental rationality and human dignity in technological systems. The disinformation constraint is one specific instantiation: when information is treated as commodity (technocratic paradigm) rather than as epistemic commons serving human dignity, the result is engineered epistemic crisis. The upstream constraint has its own extractiveness reflecting the general pattern; this constraint has its own extractiveness reflecting the specific disinformation mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(truth_democracy_disinformation, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
