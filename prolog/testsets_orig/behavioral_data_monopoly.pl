% ============================================================================
% CONSTRAINT STORY: behavioral_data_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_data_monopoly, []).

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
 *   constraint_id: behavioral_data_monopoly
 *   human_readable: Behavioral Data Monopoly and Digital Extraction
 *   domain: technology/economics/social
 *
 * SUMMARY:
 *   The behavioral data monopoly represents one of the most consequential
 *   institutional constraints of the 21st century. Digital platforms (Meta,
 *   Google, TikTok, Amazon, and their ecosystem) operate a system in which
 *   user behavior generates extraordinarily valuable data that flows
 *   asymmetrically to platform operators and their downstream beneficiaries
 *   in ad-tech and intelligence markets. Users experience this as either
 *   consensual (trading data for services), normal (behavioral surveillance
 *   has become culturally naturalized), or invisible (the scope of data
 *   collection exceeds user awareness). The constraint exhibits exceptionally
 *   high extractiveness (0.68) and suppression (0.72) because exit is
 *   structurally costly (network effects, switching costs, social
 *   integration), information asymmetry is institutionally maintained (opaque
 *   terms of service, complex algorithmic systems), and alternatives remain
 *   systematically underfunded relative to dominant platforms. The theater
 *   ratio (0.55) reflects that data protection regulations (GDPR, CCPA)
 *   create the appearance of user control without fundamentally altering the
 *   extraction mechanism — users can request data access and deletion, but
 *   cannot prevent collection without leaving the platform entirely. The
 *   constraint's timeline shows acceleration: extractiveness grew from 0.35
 *   to 0.68 over 15 years as platform penetration deepened, machine learning
 *   scaled, and behavioral data value increased. Theater ratio grew from 0.30
 *   to 0.55 as regulatory frameworks created compliance rituals that
 *   substituted for structural change.
 *
 * KEY AGENTS:
 *   - Users (powerless/trapped): Primary victims — provide continuous behavioral data without compensation or genuine consent; face network-effect lock-in and high switching costs
 *   - Digital native youth (powerless/identity-locked): Secondary victim cohort — identity formation occurs within platforms; exit structurally possible but perceptually impossible because self-concept fused with digital footprint
 *   - Small business dependents (moderate/constrained): Tertiary victims — merchants and creators dependent on platform distribution; subject to algorithmic changes and terms modifications
 *   - Platform operators (institutional/arbitrage): Primary beneficiaries — Meta, Google, TikTok, Amazon extract behavioral data and monetize through ad targeting, predictive modeling, and market intelligence
 *   - Ad-tech and intelligence sectors (institutional/arbitrage): Secondary beneficiaries — purchase and utilize behavioral models derived from platform data
 *   - Privacy-conscious coalition (organized/constrained): Mixed position — benefits from platform coordination but extracted from; has regulatory leverage and agency
 *   - Data protection regulators (institutional/constrained): Performative actors — maintain regulatory theater (GDPR/CCPA compliance) without structural transformation
 *   - Analytical observer: Detects that the constraint naturalizes institutional capture of informational commons
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_data_monopoly, 0.68).
domain_priors:suppression_score(behavioral_data_monopoly, 0.72).
domain_priors:theater_ratio(behavioral_data_monopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_data_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(behavioral_data_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(behavioral_data_monopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_data_monopoly, snare).
narrative_ontology:human_readable(behavioral_data_monopoly, "Behavioral Data Monopoly and Digital Extraction").
narrative_ontology:topic_domain(behavioral_data_monopoly, "technology/economics/social").

domain_priors:requires_active_enforcement(behavioral_data_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_data_monopoly, platform_operators).
narrative_ontology:constraint_victim(behavioral_data_monopoly, users).
narrative_ontology:constraint_victim(behavioral_data_monopoly, privacy_commons).
narrative_ontology:constraint_victim(behavioral_data_monopoly, alternative_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGITALLY TRAPPED USER (SNARE) — Users cannot exit the platform ecosystem without bearing massive social and economic costs. Network effects, habit formation, employer/government integration, and switching costs create structural entrapment. Behavioral data extraction is continuous and non-negotiable. Maximum suppression through information asymmetry, terms-of-service opacity, and normalized surveillance.
constraint_indexing:constraint_classification(behavioral_data_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIGITAL NATIVE YOUTH (SNARE with identity lock) — For users socialized entirely within platform ecosystems, identity formation itself occurs within spaces of data extraction. Exit is structurally possible at low material cost but identity-locked — self-conception fused with platform participation. Users may be structurally mobile but perceptually trapped because their identity IS their digital footprint and social graph. Suppression functions internalized: users self-censor and self-disclose to platform norms.
constraint_indexing:constraint_classification(behavioral_data_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL BUSINESS PLATFORM DEPENDENCE (SNARE) — Merchants, creators, and small businesses dependent on platform distribution channels face high costs of exit. Platform can modify algorithms, pricing, content policies, and data terms unilaterally. Behavioral data flows from both business owner and customers to the platform beneficiary. Suppression through dependency — alternative distribution networks remain immature.
constraint_indexing:constraint_classification(behavioral_data_monopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY-CONSCIOUS COALITION (TANGLED ROPE) — Organized privacy advocates, privacy-by-design communities, and regulatory bodies experience the constraint as hybrid. The coalition benefits from coordination norms that platforms provide (connection, discovery, communication), but these same platforms extract behavioral data asymmetrically. Coalition has agency and regulatory leverage; can negotiate rather than be commanded. Classification reflects both genuine coordination function (connection enabling) and asymmetric extraction (data harvesting).
constraint_indexing:constraint_classification(behavioral_data_monopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PLATFORM OPERATOR (ROPE) — The beneficiary institution sees the behavioral data constraint as pure coordination: the system enables user connection, content discovery, algorithmic optimization, and business model efficiency. From this perspective, behavioral data extraction is necessary coordination infrastructure, not coercion. Platform operators benefit from the constraint and experience it as a self-justifying system.
constraint_indexing:constraint_classification(behavioral_data_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY DATA REGULATION (PITON) — GDPR, CCPA, and similar regulations appear as performative theater: users obtain data access and deletion rights, but the fundamental extraction mechanism persists because users cannot prevent data collection without exiting entirely. Regulatory compliance becomes a checkbox ritual; the core constraint remains unchanged. Theater ratio high because regulation creates appearance of control without structural change. Regulation is institutionally inert — maintained because alternatives have not emerged, not because it effectively controls extraction.
constraint_indexing:constraint_classification(behavioral_data_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational scope, behavioral data extraction represents asymmetric institutional capture of the informational commons. Users collectively generate behavioral data (a public good) but receive no compensation and lose informational self-determination. The constraint exhibits all hallmarks of snare: high suppression through complexity and normalization, high extraction toward platform operators, minimal coordination benefit proportional to cost, and dependence on suppressing alternatives (privacy-preserving technologies remain underfunded).
constraint_indexing:constraint_classification(behavioral_data_monopoly, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_data_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_data_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_data_monopoly, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_data_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_data_monopoly, TR),
    TR >= 0.70.

:- end_tests(behavioral_data_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accelerating. Platform operators extract behavioral data continuously and monetize it through ad targeting, algorithmic ranking, and market intelligence. Users receive services (connection, content, convenience) but the value asymmetry is extreme — platform market capitalizations ($1-3 trillion) are built on this data extraction while users receive no compensation and bear information privacy costs. The extractiveness value reflects: (1) the magnitude of data flow (continuous behavioral logging at scale), (2) the asymmetry of benefit (concentrated to platform operators and ad-tech), (3) the irreversibility (data cannot be unextracted), and (4) the opacity (users cannot perceive the full scope). Suppression (0.72): Very high. Multiple suppression mechanisms operate simultaneously: (1) Structural — network effects and switching costs make exit economically costly; (2) Informational — terms of service are intentionally complex and opaque; algorithmic systems are blackboxes; data collection scope exceeds user awareness; (3) Psychological — behavioral addiction design; normalization of surveillance in user culture; (4) Regulatory — data protection laws create appearance of control while permitting extraction; (5) Competitive — alternative platforms remain systematically underfunded and technically immature. No single mechanism is dominant; the constraint is over-determined. Theater ratio (0.55): Moderate-high. Regulatory compliance (GDPR cookie notices, CCPA data access portals) is substantially theater — compliance creates user perception of control while collection mechanisms remain unchanged. However, the theater is not complete — some users do change behavior in response to privacy notices, and some platform design changes occurred in response to regulation (though primarily on the surfaces users see, not the data extraction apparatus). Theater ratio lower than pure pitons (0.70+) because some genuine functional change occurred, but high enough to indicate that regulation has not fundamentally altered the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The 4-perspective gap between the trapped user's snare (d=0.95) and the beneficiary's rope (d=0.15) is the primary diagnostic signal that behavioral data extraction is extractive rather than coordinative. If this were genuine coordination, perspectives would cluster around tangled rope with narrower gaps. The fact that the same structural mechanism produces snare from the victim's position and rope from the beneficiary's position reveals that the 'coordination' language (users trade data for services) is a cover narrative that naturalizes extraction. The identity-locked youth perspective adds a crucial signal: structural mobility masked by cognitive capture. This youth can technically exit (no insurmountable material barrier) but cannot perceive exit because their identity is constituted through platform participation. The piton classification of regulatory theater is the second diagnostic signal — regulations that create appearance of control without structural change are evidence that the constraint is fundamentally extractive, not coordinative. Genuine coordination mechanisms do not require performative regulation because the equilibrium is self-reinforcing.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are beneficiaries with arbitrage exit: they can instantly change the constraint's terms, implement new extraction mechanisms, or abandon it if alternatives became profitable. They experience the constraint as enabling rather than coercive. Users are victims with trapped exit: switching to alternative platforms incurs network-effect costs (losing social graph), social costs (isolation from peers), and opportunity costs (missing communication). The directionality asymmetry is structural, not perspectival — it reflects real differences in capacity to change the constraint. Small businesses face constrained exit: they could theoretically build their own distribution channels or move to competitor platforms, but algorithmic dominance by incumbent platforms makes alternative channels uncompetitive. Privacy advocates have constrained exit but organized power: they can negotiate regulatory changes and create pressure on platform design, making them partially able to shape the constraint rather than just endure it. Regulators are an intermediate actor — they have institutional power to enforce rules but constrained real impact because the underlying extraction mechanism persists regardless of compliance theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The behavioral data monopoly at extractiveness 0.68 exceeds the mandatrophy threshold (ε > 0.70 requires resolution). The resolution is articulated through the perspectival structure: the constraint is demonstrably a SNARE, not a false coordination mechanism, because (1) the beneficiary and victim perspectives classify identically (both snare) despite different power levels, (2) the piton perspective shows regulatory theater rather than functional constraint, (3) the omega variables identify multiple mechanisms (consent ambiguity, alternative platform viability, regulatory capture) that would reduce extractiveness if resolved, and (4) the network analysis reveals downstream beneficiaries (ad-tech, intelligence markets) creating dependency chains that prevent exit. The mandatrophy is resolved by accepting that behavioral data extraction is a genuine snare mechanism, not a coordination problem awaiting the right regulatory fix. The false natural law narrative is that 'this is just how digital platforms work' — the analytical perspective reveals this is a contingent institutional arrangement dependent on network effects and regulatory capture, not an immutable feature of information technology. The platform operator's rope classification is a false equilibrium — it reflects the beneficiary's self-justifying perspective, not the objective structural relationship. Accepting snare classification is the mandatrophy resolution: acknowledging that suppression is structural (not just preferences), that extraction is asymmetric (not fair trade), and that alternatives are suppressed (not nonviable), enabling policy response rather than resignation to false naturalness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_ambiguity,
    'Is behavioral data extraction consensual surveillance (users voluntarily trade data for services) or coercive extraction masked by fictional consent (terms-of-service theater)?',
    'Empirical measure of genuine informed consent: proportion of users who understand data collection scope; analysis of switching behavior if consent requirements were made explicit and revocable; study of platform design patterns that obscure consent.',
    'If truly consensual: classification shifts from Snare toward Tangled Rope with high beneficiary-victim coordination. If coercive: Snare classification confirmed and suppression value rises toward 0.85. If masked by design (dark patterns in consent flow): suppression remains high and ''consensual'' framing is revealed as false natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_ambiguity, empirical, 'Whether behavioral data extraction represents consensual exchange or coercive extraction masked by terms-of-service theater').

omega_variable(
    alternative_viable_platform_feasibility,
    'Are privacy-respecting, decentralized, or open-source platform alternatives technically viable and economically scalable, or is network effect dominance an immutable constraint on competition?',
    'Technical audit of federated/decentralized platforms (Mastodon, Signal, Matrix); user adoption trajectories; analysis of whether switching costs are organizational/social (changeable) or fundamental to distributed architecture.',
    'If viable alternatives exist: suppression score is artificially high (users self-select into trapping platforms despite better options); classification shifts downward. If alternatives are technically infeasible: suppression reflects real structural constraint; Snare classification and high suppression confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_viable_platform_feasibility, empirical, 'Whether privacy-respecting platform alternatives are technically viable and economically scalable').

omega_variable(
    beneficiary_identity_ambiguity,
    'Are the true beneficiaries the platform operators themselves, or the downstream ad-tech/intelligence community that extracts predictive models and targeting systems from the behavioral data?',
    'Value-chain analysis of data monetization flows; identification of where primary extraction margins accumulate; study of data-broker ecosystems and intelligence-market structures built on platform behavioral data.',
    'If platform operators are direct beneficiaries: current snare classification stands. If real beneficiaries are ad-tech and intelligence sectors using platform data: platform operators are intermediate extractors (higher d value, different perspective classification). This would require decomposition into upstream (behavioral_data_collection) and downstream (predictive_model_extraction) constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'Whether platform operators or downstream ad-tech/intelligence community are the primary beneficiaries').

omega_variable(
    identity_lock_generational_persistence,
    'Is identity-lock binding for digital natives merely a cohort effect (will diminish as alternative social infrastructure matures) or a civilizational shift in how human identity forms?',
    'Longitudinal analysis of users who exit platform ecosystems: do they reconstitute identity outside platforms, or do they experience persistent identity fragmentation? Comparative study across cultures with lower platform penetration.',
    'If cohort effect: identity-locked classification applies only to currently young populations; future cohorts may experience platforms as high-cost but exitable constraints. If civilizational shift: identity-lock becomes stable over generations and strengthens extraction mechanism through normalized self-disclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_generational_persistence, conceptual, 'Whether digital native identity-lock is cohort-specific or a civilizational shift').

omega_variable(
    regulatory_capture_of_data_law,
    'Do data protection regulations (GDPR, CCPA) represent genuine constraints on platform extraction, or are they themselves captured by platform operators who engineered compliance theater?',
    'Analysis of regulation crafting: platform industry participation in GDPR drafting; study of CCPA enforcement outcomes; comparison of regulation intent vs actual user data protection achieved; examination of technical architecture changes platforms made in response to regulation.',
    'If regulations are effective: theater_ratio should be lower, and piton classification shifts toward tangled rope. If regulations are captured theater: theater_ratio is high (confirmed 0.55+), piton classification stands, and regulatory compliance becomes evidence of snare durability, not constraint success.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_of_data_law, empirical, 'Whether data protection regulations represent genuine constraints or platform-captured compliance theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_data_monopoly, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_tr_t0, behavioral_data_monopoly, theater_ratio, 0, 0.3).
narrative_ontology:measurement(behav_tr_t5, behavioral_data_monopoly, theater_ratio, 5, 0.42).
narrative_ontology:measurement(behav_tr_t10, behavioral_data_monopoly, theater_ratio, 10, 0.55).
narrative_ontology:measurement(behav_tr_t15, behavioral_data_monopoly, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(behav_be_t0, behavioral_data_monopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(behav_be_t5, behavioral_data_monopoly, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(behav_be_t10, behavioral_data_monopoly, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(behav_be_t15, behavioral_data_monopoly, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_data_monopoly, resource_allocation).
narrative_ontology:affects_constraint(behavioral_data_monopoly, algorithmic_opacity_in_content_ranking).
narrative_ontology:affects_constraint(behavioral_data_monopoly, ad_tech_targeting_extraction).
narrative_ontology:affects_constraint(behavioral_data_monopoly, network_effects_platform_lock).
narrative_ontology:affects_constraint(behavioral_data_monopoly, digital_identity_formation).

% DUAL FORMULATION NOTE:
% The behavioral data monopoly represents a cluster of structurally distinct constraints that can be decomposed: (1) behavioral_data_collection (the raw extraction of user behavior logs), (2) algorithmic_prediction_and_profiling (deriving predictive models and psychological profiles from the data), and (3) market_intelligence_accumulation (using profiles for competitive advantage and information markets). The present story models the entire pipeline from user behavior to platform profit. Upstream constraints address individual mechanisms; downstream constraints address market effects of concentrated behavioral knowledge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_data_monopoly, analytical, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
