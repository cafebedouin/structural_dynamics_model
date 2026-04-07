% ============================================================================
% CONSTRAINT STORY: data_extraction_surveillance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_extraction_surveillance, []).

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
 *   constraint_id: data_extraction_surveillance
 *   human_readable: Data Extraction Through Ubiquitous Surveillance
 *   domain: technology/political_economy
 *
 * SUMMARY:
 *   Data extraction surveillance represents a structural transformation in
 *   how power operates. The constraint binds individuals into systems of
 *   continuous behavioral monitoring where their actions, preferences, social
 *   connections, and psychological states are recorded, analyzed, and traded
 *   as commodities or used for predictive control. The extraction mechanism
 *   is distributed across thousands of collection points (smartphones,
 *   websites, IoT devices, advertising networks, state security systems)
 *   making it difficult to identify any single point of resistance. The
 *   suppression is extreme: dependency on digital services creates trapped
 *   exit conditions, while internalized narratives normalize surveillance as
 *   inevitable. The constraint exhibits different structural forms depending
 *   on the observer's position: for individuals it is a snare with no
 *   functional exit; for platforms it is a coordination mechanism solving the
 *   problem of connecting advertisers to audiences; for organized privacy
 *   advocates it is a tangled rope where institutional interests create mixed
 *   incentives; for states it enables both domestic coordination and
 *   international extraction; for regulators it becomes a performative ritual
 *   (piton); and from a civilizational analytical view it risks appearing as
 *   an immutable natural law of digital systems. The measurements show
 *   accelerating extractiveness (0.35→0.68 over 20 units) alongside rising
 *   theater (0.32→0.58), indicating that as extraction deepens, the
 *   regulatory and privacy-theater responses increase in proportion, creating
 *   the illusion of control without substantive constraint.
 *
 * KEY AGENTS:
 *   - Data Subjects (Individuals): Powerless/trapped primary victims — cannot exit surveillance infrastructure without abandoning digital participation; bear full extraction cost through behavioral control and data commodification.
 *   - Surveillance-Native Generation: Powerless/identity_locked secondary victims — socialized entirely within surveillance capitalism; identity constituted through connectivity norms making exit unthinkable; structurally mobile but cognitively trapped.
 *   - Platform Operators (Tech Companies): Institutional/arbitrage primary beneficiaries — extract behavioral data and sell predictive power; experience surveillance as coordination mechanism solving advertising efficiency problem; highest power with lowest extraction perceived.
 *   - State Security Apparatus: Powerful/arbitrage institutional actor — uses surveillance for domestic coordination and international extraction; experiences tangled rope (mixed coordination/extraction) because security functions are genuine but asymmetrically benefit security apparatus over monitored populations.
 *   - Data Protection Advocates/Regulators: Moderate/constrained secondary beneficiaries — professional existence depends on surveillance crisis; experience tangled rope because institutional interests are partially misaligned with actual solution; regulatory frameworks create theater without constraint.
 *   - Decentralization/Privacy-Tech Coalition: Organized/constrained exit-pathway builders — see genuine sunset through federated protocols and privacy-preserving infrastructure; building alternatives but constrained by resources and network-effect barriers.
 *   - Analytical Observer: Civilizational view at risk of false summit — sees surveillance as immutable property of digital infrastructure rather than contingent institutional choice.
 *   - Regulatory Framework (GDPR, CCPA): Institutional/constrained piton — privacy regulations persist as performative theater; actual behavioral control minimal; exists through institutional inertia and legitimacy maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_extraction_surveillance, 0.68).
domain_priors:suppression_score(data_extraction_surveillance, 0.72).
domain_priors:theater_ratio(data_extraction_surveillance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_extraction_surveillance, extractiveness, 0.68).
narrative_ontology:constraint_metric(data_extraction_surveillance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(data_extraction_surveillance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_extraction_surveillance, snare).
narrative_ontology:human_readable(data_extraction_surveillance, "Data Extraction Through Ubiquitous Surveillance").
narrative_ontology:topic_domain(data_extraction_surveillance, "technology/political_economy").

domain_priors:requires_active_enforcement(data_extraction_surveillance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_extraction_surveillance, platform_operators).
narrative_ontology:constraint_beneficiary(data_extraction_surveillance, advertising_brokers).
narrative_ontology:constraint_beneficiary(data_extraction_surveillance, state_security_apparatus).
narrative_ontology:constraint_victim(data_extraction_surveillance, data_subjects).
narrative_ontology:constraint_victim(data_extraction_surveillance, behavioral_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual user cannot meaningfully opt out of surveillance infrastructure without abandoning economic participation, professional necessity, and social contact. Exit is theoretically possible but practically impossible — trapped by dependency on digital services with no functional alternative. Suppression is extreme: pervasive monitoring, algorithmic invisibility, terms-of-service opacity, and the distributed nature of extraction across thousands of collection points prevent organized resistance. Maximum experienced extraction.
constraint_indexing:constraint_classification(data_extraction_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For those socialized entirely within surveillance capitalism, surveillance infrastructure becomes constitutive of identity. The ability to imagine life without continuous monitoring, data collection, and algorithmic mediation is cognitively unavailable. Exit would require rejecting the identity frames (digital nativity, connectedness, visibility as status) that structure self-concept. Structurally mobile but identity-trapped. Theater maintains the lock through normalizing narratives (privacy is dead, you have nothing to hide, connection is inevitable).
constraint_indexing:constraint_classification(data_extraction_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% Data protection advocates experience the constraint as both coordination and extraction. They benefit from the surveillance infrastructure they critique — their organizations exist because of the problem, their funding comes from crisis awareness, their influence derives from being the named expert. They also bear significant extraction: career risk, regulatory capture by platforms, resource constraints that prevent meaningful enforcement. Mixed but asymmetric: benefits accrue from problem persistence; extraction costs come from attempting solution.
constraint_indexing:constraint_classification(data_extraction_surveillance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% For platform operators, data extraction surveillance is core coordination infrastructure. It solves the advertising matching problem, enables personalization, provides risk management signals, and creates competitive advantage through algorithmic prediction. From this perspective, the constraint is seen as solving a genuine coordination challenge: connecting advertisers with relevant audiences across billions of users. The extraction is reframed as the price of the service (free platform access in exchange for behavioral data). Net beneficiary experiencing rope-level coordination.
constraint_indexing:constraint_classification(data_extraction_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% States experience data surveillance as both coordination mechanism and extraction tool. Domestically, surveillance enables coordination of security responses, crime prevention, and administrative efficiency. Internationally, surveillance of foreign populations is pure extraction with minimal coordination component. States with arbitrage options (ability to withdraw from international data agreements, build domestic platforms, maintain internal security capability) experience this as tangled rope rather than pure snare — they have genuine exit options and benefit from having the surveillance infrastructure available. Powerful position + arbitrage options + beneficiary status → tangled rope.
constraint_indexing:constraint_classification(data_extraction_surveillance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Privacy regulations exist as degraded surveillance controls. GDPR, CCPA, and similar frameworks are largely performative: compliance theaters (cookie banners, privacy policies nobody reads) that create the appearance of control without substantive constraint on data extraction. The regulations persist through institutional inertia and political legitimacy maintenance, but their actual functional verification of data practices is minimal. High theater ratio reflects that the ritual of consent and transparency has replaced meaningful governance. The extraction continues beneath the regulatory performance.
constraint_indexing:constraint_classification(data_extraction_surveillance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational analytical perspective, pervasive data extraction might appear as an immutable feature of digital infrastructure: surveillance is inherent to networked computation, extraction follows inevitably from information asymmetry, behavioral analysis is unavoidable in any system with feedback loops. This perspective risks naturalizing what is actually a contingent institutional arrangement — the choice to build extractive surveillance rather than privacy-preserving alternatives. The engine's false summit detector will flag this perspective as misclassifying contingent power dynamics as natural law.
constraint_indexing:constraint_classification(data_extraction_surveillance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Organized agents (privacy-focused technologists, decentralization advocates, open-source communities) see data extraction surveillance as a temporary problem with a sunset: federated protocols, end-to-end encryption, local-first software, and privacy-preserving computation are building alternative infrastructures that bypass the centralized surveillance model. These movements see genuine exit pathways and have some agency in building them, though resource constraints are significant. The sunset logic depends on whether these alternatives can reach network-effect scale before centralized platforms entrench further.
constraint_indexing:constraint_classification(data_extraction_surveillance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_extraction_surveillance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_extraction_surveillance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_extraction_surveillance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_extraction_surveillance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_extraction_surveillance, TR),
    TR >= 0.70.

:- end_tests(data_extraction_surveillance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the degree to which behavioral data is extracted from data subjects without commensurate compensation or genuine consent. The measurement progression (0.35→0.68) shows accumulation of extraction mechanisms: initial platforms (2010s) extracted primary behavioral data; mobile expansion (2015+) added location and device-level extraction; contemporary systems (2020+) layer in social graph extraction, biometric inference, and predictive behavioral control. The 0.68 value reflects that extraction has reached maximal intensity short of complete behavioral determinism. Suppression (0.72): Very high. Multiple suppression mechanisms operate: technical (encryption/anonymization barriers making exit technically impossible without compromising digital functionality); legal (terms of service make refusal-of-use contracts legally binding); social (professional and economic necessity creates dependency); cognitive (normalization narratives reduce perception of extraction). Theater (0.58): Moderate-high. The 0.58 value reflects that privacy regulations, consent dialogs, and transparency policies create significant theater — users see notifications, privacy policies exist, regulatory frameworks are visible — but actual constraint on extraction is minimal. GDPR compliance produces cookie banners and data deletion options that have marginal effect on actual behavioral extraction. Theater increased from 0.32 to 0.58 as regulators responded to political pressure with performative rather than functional controls.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a profound perspectival gap between victims and beneficiaries. Data subjects perceive pure extraction (snare) with no coordination benefit — they cannot see how surveillance serves their interests, only how it constrains their autonomy. Platform operators perceive pure coordination (rope) — connecting advertisers to relevant audiences is a genuine problem and surveillance is the solution. The gap is not in disagreement about facts but in fundamental incommensurability of experienced relationship to the constraint. Scaffold and piton perspectives represent partially organized responses: privacy advocates see temporary problem (scaffold) while regulatory systems perform temporary response (piton) without addressing the structure. The identity_locked perspective reveals how suppression operates not just materially but cognitively: those socialized into surveillance normalization have lost the capacity to imagine non-surveilled digital participation. The state perspective demonstrates how the same constraint can function as coordination mechanism (domestic security) and extraction mechanism (international espionage) simultaneously. The false mountain perspective shows how civilizational analysis risks naturalizing what is a contingent power arrangement — surveillance is not inherent to digital systems but a specific design choice by those who benefit from extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from the agent's structural relationship to extraction flows. Data subjects are full victims with trapped exit: d≈0.95, f(d)≈1.42, maximum experienced extraction despite moderate base extractiveness. Platform operators are net beneficiaries with arbitrage options: d≈0.05, f(d)≈-0.12, experiencing minimal or negative extraction despite high base extractiveness because they are the extractors. States have mixed relationships: domestic citizens are victims (high d) while foreign populations are pure extraction targets (d≈1.0); the state's analytical position is beneficiary (d≈0.10) but with embedded extraction from subjects. Data protection advocates experience moderate d≈0.55 because they are both victims (trapped in professional dependence on surveillance crisis) and partial beneficiaries (derive institutional status from problem). The decentralization coalition has lower d because they have organizational agency and exit pathways even if constrained by resources. Regulatory frameworks experience d≈0.35 (captured institutions with some autonomy but fundamentally dependent on platform cooperation) producing piton classification. The analytical observer's d≈0.72 produces the mountain-like classification that the false summit detector flags as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by showing that the snare classification persists across all practically available perspectives for data subjects and remains the dominant classification from the powerless position even as organized and institutional perspectives show alternative types. The constraint cannot be reframed as pure coordination because the asymmetry between extractors and subjects is fundamental: platforms do not offer surveillance to users as a benefit but extract it despite users' stated preferences. Regulatory theater might superficially suggest scaffold dynamics (sunset through better governance) but the measurements show theater rising (0.32→0.58) while actual extraction continues rising (0.35→0.68), indicating that regulatory responses are not reducing extraction but performing solution while it continues. The mandatrophy resolves as: 'This is a snare for those who bear the costs. It appears as rope or tangled rope for those who benefit. The gap itself is the structural feature that makes the classification unambiguous — the constraint exists because extraction continues beneath regulatory theater.' The high extractiveness (0.68) combined with persistent suppression (0.72) and rising theater (0.58) confirms snare classification. If the constraint were genuinely a scaffold with sunset, extractiveness would be declining or theater would be declining toward functional constraint; neither is occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_behavioral_suppression,
    'Is suppression primarily structural (technical/legal barriers to exit) or behavioral (normalized internalization of surveillance as inevitable)?',
    'Comparative analysis of exit rates between populations exposed to privacy advocacy versus control populations; longitudinal tracking of surveillance acceptance after explicit availability of exit mechanisms; measurement of willingness-to-pay for privacy across demographics with varying exposure to privacy frames.',
    'If primarily structural: suppression metric (0.72) is accurate and conservative. If primarily behavioral: actual suppression is lower than measured (users could exit if they truly wanted to), suggesting the identity_locked exit option applies more broadly than currently classified. If mixed: impacts the omega variable for internalization vs externalization of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_behavioral_suppression, empirical, 'Whether suppression is structural (technical barriers) or behavioral (normalized acceptance)').

omega_variable(
    decentralization_feasibility,
    'Can privacy-preserving decentralized alternatives achieve network-effect scale before centralized surveillance platforms entrench further, or is the scaffold sunset aspirational rather than structural?',
    'Historical comparison of alternative infrastructure adoption rates (email migration from centralized to federated, messaging app adoption cycles); analysis of whether privacy-tech projects show adoption acceleration or stagnation; measurement of actual privacy-preserving infrastructure usage rates.',
    'If feasible with realistic timeline: scaffold perspective is correct and the constraint has a genuine exit path. If not feasible: scaffold is aspirational and the constraint remains snare for all practical timeframes. Classification stays snare but scaffolding expectations should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_feasibility, empirical, 'Whether decentralized privacy-preserving alternatives can scale before centralized extraction entrenches').

omega_variable(
    identity_lock_generational_depth,
    'For populations socialized entirely within surveillance capitalism, is the identity lock permanent or can it be broken through explicit cognitive reframing?',
    'Longitudinal studies of surveillance-native populations exposed to strong alternative narratives; measurement of acceptance rates for privacy-preserving tools after explicit education about alternatives; analysis of whether generational cohort effects show increasing or decreasing acceptance of surveillance normalization.',
    'If permanent: entire generation is effectively trapped despite structural mobility (identity_locked → mountain-like immutability at biographical timescale). If breakable: identity lock is a temporary perceptual filter rather than a structural constraint; populations can exit if exposed to alternative frames. Affects whether the constraint''s extractiveness is structural or depends on maintained ideological work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_generational_depth, empirical, 'Whether identity lock in surveillance-native populations is permanent or reversible').

omega_variable(
    state_surveillance_extraction_asymmetry,
    'Does state surveillance of foreign populations constitute structural extraction from those populations, or is it a separate constraint from domestic platform surveillance?',
    'Decomposition analysis: whether surveillance''s effect on non-citizens differs fundamentally from effect on citizens; whether state surveillance and platform surveillance are coupled or independent; measurement of extraction flows in each direction (state extracts from citizens, state+platform extract from non-citizens).',
    'If integrated: the constraint is genuinely tangled rope at state level (mixed coordination domestic + extraction international). If separate: state surveillance should be decomposed into a distinct story with higher extractiveness. Current analysis assumes integration; separation would split the state perspective into multiple constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_surveillance_extraction_asymmetry, conceptual, 'Whether state surveillance of foreign populations is integrated with or separate from platform surveillance extraction').

omega_variable(
    regulatory_capture_of_privacy_advocates,
    'Do privacy advocacy organizations and regulators experience the constraint as tangled rope because their institutional existence depends on problem persistence, creating perverse incentives against meaningful solution?',
    'Organizational analysis: funding dependence on surveillance crisis awareness; career incentive alignment with continued problem; measurement of how advocacy priorities shift when extraction mechanisms are actually addressed vs when they persist; comparison of advocacy effectiveness across funded vs unfunded organizations.',
    'If captured: the moderate power perspective experiences tangled rope not as genuine mixed extraction but as a principal-agent problem where advocates'' incentives are misaligned with actual victims. This would lower moderate agents'' classification on the snare→tangled rope spectrum and raise the analytical perspective''s credibility in the false summit detection. If not captured: advocates are genuinely constrained and their mixed experience is authentic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_privacy_advocates, empirical, 'Whether privacy advocacy organizations are captured by surveillance crisis dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_extraction_surveillance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(data_surv_tr_t0, data_extraction_surveillance, theater_ratio, 0, 0.32).
narrative_ontology:measurement(data_surv_tr_t5, data_extraction_surveillance, theater_ratio, 5, 0.45).
narrative_ontology:measurement(data_surv_tr_t10, data_extraction_surveillance, theater_ratio, 10, 0.58).
narrative_ontology:measurement(data_surv_tr_t15, data_extraction_surveillance, theater_ratio, 15, 0.58).
narrative_ontology:measurement(data_surv_tr_t20, data_extraction_surveillance, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(data_surv_be_t0, data_extraction_surveillance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(data_surv_be_t5, data_extraction_surveillance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(data_surv_be_t10, data_extraction_surveillance, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(data_surv_be_t15, data_extraction_surveillance, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(data_surv_be_t20, data_extraction_surveillance, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_extraction_surveillance, resource_allocation).
narrative_ontology:affects_constraint(data_extraction_surveillance, algorithmic_prediction_coupling).
narrative_ontology:affects_constraint(data_extraction_surveillance, behavioral_autonomy_degradation).
narrative_ontology:affects_constraint(data_extraction_surveillance, attention_extraction_economy).

% DUAL FORMULATION NOTE:
% Data extraction surveillance decomposes into multiple structurally distinct constraints. The behavioral data extraction (current story) has ε≈0.68 and operates through economic incentives and technical dependency. Algorithmic prediction coupling (ε≈0.55) operates through inference mechanisms that extract behavioral patterns even from incomplete data. Attention extraction (ε≈0.62) operates through deliberate engagement manipulation. These are linked constraints where successful surveillance extraction enables prediction coupling which enables attention manipulation; decomposition clarifies that addressing one does not resolve the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_extraction_surveillance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
