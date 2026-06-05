% ============================================================================
% CONSTRAINT STORY: bureaucratic_discretion_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_discretion_capture, []).

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
 *   constraint_id: bureaucratic_discretion_capture
 *   human_readable: Bureaucratic Discretion Capture
 *   domain: political_economy/regulatory_capture
 *
 * SUMMARY:
 *   Bureaucratic discretion capture occurs when regulatory agencies develop
 *   stable, informal relationships with regulated industries that shape how
 *   enforcement discretion is applied. Unlike statutory capture (where
 *   legislation directly favors industry), discretion capture operates
 *   through the gap between written rules and enforcement practice. The
 *   constraint is tangled because genuine coordination does exist — complex
 *   markets require expert judgment and adaptive rule application — but this
 *   coordination function is systematically asymmetric: industry gets
 *   preferential interpretation of ambiguous rules, while public
 *   constituencies and competitors face strict enforcement. The theater ratio
 *   (0.68) reflects that formal regulatory codes remain performatively
 *   important (legitimacy requires appearing neutral) while actual
 *   rule-making occurs in agency discretion and informal consultation. The
 *   constraint demonstrates all six DR types depending on structural
 *   position: powerless agents (excluded competitors, public interest)
 *   experience pure extraction (snare); established firms experience
 *   coordination benefits (rope); agencies and senior bureaucrats capture
 *   career benefits (rope); mid-career regulators are identity-locked to a
 *   system they cannot perceive as extractive (tangled_rope); organized
 *   reformers see a temporary failure with sunset mechanisms (scaffold); and
 *   the analytical observer risks naturalizing institutional contingency as
 *   immutable bureaucratic necessity (false mountain). The extractiveness has
 *   increased over the interval as agency discretion has expanded (more
 *   complex regulations, more exceptions, wider inspector judgment) without
 *   corresponding transparency or constraint mechanisms.
 *
 * KEY AGENTS:
 *   - Excluded Competitors: Primary victim (powerless/trapped) — new market entrants facing discretionary barriers; cannot exit regulatory regime or appeal enforcement decisions
 *   - Public Interest Constituencies: Primary victim (powerless/trapped) — abstract collective (consumers, workers, environment) that cannot organize defense; bear costs through reduced competition, higher prices, worse safety
 *   - Incumbent Industry: Primary beneficiary (institutional/arbitrage) — largest firms shape rule interpretation through lobbying; capture discretion through informal relationships; can arbitrage across jurisdictions or shift political influence
 *   - Senior Bureaucrats: Secondary beneficiary (institutional/arbitrage) — agency leadership captures postgovernment employment prospects and industry relationships; direct beneficiary of discretion extraction flow
 *   - Mid-Career Regulators: Mixed actor (moderate/identity_locked) — professional identity fused with regulatory system; cannot perceive extraction mechanism because identity locked to 'being a regulator'; constrained exit through career path dependence
 *   - Mid-Tier Firms: Mixed actor (moderate/constrained) — benefit from competitor exclusion through regulatory barriers but constrained by uncertainty in discretionary enforcement; experience both coordination and extraction
 *   - Reform Coalition: Organized resistance (organized/constrained) — transparency advocates, oversight bodies, structural reformers building alternative institutional pathways; see discretion capture as temporary with sunset through automation and sunshine laws
 *   - Formal Rule System: Institutional artifact (institutional/arbitrage) — written regulatory code maintains legitimacy theater while actual rule-making occurs in discretionary channels; performative function rather than operative function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_discretion_capture, 0.58).
domain_priors:suppression_score(bureaucratic_discretion_capture, 0.65).
domain_priors:theater_ratio(bureaucratic_discretion_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_discretion_capture, tangled_rope).
narrative_ontology:human_readable(bureaucratic_discretion_capture, "Bureaucratic Discretion Capture").
narrative_ontology:topic_domain(bureaucratic_discretion_capture, "political_economy/regulatory_capture").

domain_priors:requires_active_enforcement(bureaucratic_discretion_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_discretion_capture, regulated_industry).
narrative_ontology:constraint_beneficiary(bureaucratic_discretion_capture, senior_bureaucrats).
narrative_ontology:constraint_victim(bureaucratic_discretion_capture, public_interest_constituencies).
narrative_ontology:constraint_victim(bureaucratic_discretion_capture, competitive_market_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED COMPETITOR (SNARE) — New market entrants and small competitors face regulatory barriers designed through discretionary enforcement. No exit from the regulatory regime; no meaningful appeal process; no alternative market access. Experiences maximum extraction through differential rule application.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC INTEREST (SNARE) — Consumer welfare, environmental protection, worker safety — abstract constituencies that cannot organize collective action or exit from regulated markets. Bear costs of regulatory capture through worse service, higher prices, reduced safety. Maximal structural extraction with no agency or escape.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-TIER FIRM (TANGLED ROPE) — Established firms benefit from regulatory barriers that exclude smaller competitors, but also constrained by uncertainty in discretionary enforcement. Experience both coordination function (standardized compliance reduces competition on non-quality factors) and asymmetric extraction (larger rivals get preferential interpretation of rules). Can exit through merger or regulatory appeal, but at significant cost.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT INDUSTRY (ROPE) — Largest firms benefit from discretionary enforcement that disadvantages competitors; participate in rule-drafting through agency consultation. Experience the regulatory apparatus as a coordination mechanism that protects market structure while delegating to agency discretion which rules to enforce strictly. Can arbitrage by shifting operations across jurisdictions or influencing rule interpretation directly.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SENIOR BUREAUCRAT (ROPE) — Agency leadership captures career benefits through discretionary enforcement: reputational alignment with regulated industry, postgovernment employment opportunities, resource flows to favored enforcement priorities. Experience discretion as a coordination tool (unclear rules require experienced judgment) while being direct beneficiary of the extraction flow. Arbitrage exit through revolving door.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MID-CAREER REGULATOR (TANGLED ROPE) — Career advancement depends on working within the capture relationship; professional identity fused with regulatory system that is itself captured. Sees coordination function (complex rules require expert interpretation) but cannot escape identity lock to see the extractive mechanism clearly. Could exit through external employment, but identity-locked to 'being a regulator' makes exit unthinkable from within frame.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: REFORM COALITION (SCAFFOLD) — Advocacy organizations, transparency advocates, and structural reformers see bureaucratic discretion capture as a temporary institutional failure with a sunset: sunshine laws, algorithmic decision-making, mandatory rule justification, and removal of discretion. Low effective extraction because organized agents (NGOs, oversight bodies, legislative reform) have clear exit pathways and are actively building institutional alternatives.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: FORMAL RULE SYSTEM (PITON) — Written regulatory code appears neutral and comprehensive, but is substantially performative: the published rules are reference theater while actual enforcement runs through discretionary channels and informal relationships. The code persists (maintains legitimacy) while real rule-making occurs in agency discretion. High theater ratio indicates degraded function — the formal rule system is largely inert.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, delegation of enforcement discretion to agencies is inherent to complex regulation: no statutory code can specify enforcement for all contingencies, and practical administration requires expert judgment. This perspective risks naturalizing discretion as an immutable feature of bureaucratic governance. However, the base properties contradict the mountain classification — the engine will identify this as a false summit, revealing that the 'inherent to bureaucracy' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_discretion_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_discretion_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_discretion_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_discretion_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_discretion_capture, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_discretion_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Discretionary enforcement creates measurable market disadvantages for competitors and public constituencies — barrier to entry, reduced competition, sustained pricing power for incumbents. However, not maximal (not 0.72+) because some discretion reflects genuine complexity coordination rather than pure capture. The interval measurement shows extractiveness increasing from 0.42 to 0.58 as agency discretion expanded without constraint mechanisms. Suppression (0.65): High. Multiple barriers prevent exit or challenge: regulatory appeal processes are slow and expensive, market exit means abandoning operations in regulated jurisdiction, public constituencies lack standing and organization. Suppression increasing as regulatory complexity grows. Theater ratio (0.68): High. Formal regulatory codes appear comprehensive and neutral; enforcement operates through discretionary channels that are invisible in published decisions; regulatory theater maintains legitimacy for system where real rule-making occurs informally. Theater ratio increasing as gap between code and practice widens — more rules published, more discretion required to interpret them, more performative theater needed to sustain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals the mechanism of capture. Incumbent industry sees rope (coordination) because they benefit; excluded competitors see snare (extraction) because they are targeted; mid-career regulators see tangled_rope with identity lock because they are inside the system and cannot see extraction from within their professional frame; reformers see scaffold because they have organized resistance and clear exit pathways. The gap is not observational disagreement — it is structural. The constraint actually IS rope to the incumbent industry (they genuinely coordinate through it). It actually IS snare to excluded competitors (they genuinely experience extraction). The analytical observer's mountain classification is a false summit: it naturalizes what the base properties reveal as contingent institutional arrangement. The perspectival gap is the diagnostic signature that this is institutional capture, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect beneficiary/victim structure and exit capacity. Excluded competitors are victims with trapped exit — maximum directionality (0.92) toward extraction. Incumbent industry are beneficiaries with arbitrage exit — minimum directionality (0.08) toward extraction, meaning extraction flows away from them. Public interest constituencies are abstract victims with no exit capacity — maximum directionality (0.98). Senior bureaucrats benefit from the capture through postgovernment employment and career alignment — low directionality (0.10). Mid-career regulators experience mixed signals: they benefit from discretion as a tool (coordination function) but are constrained by identity lock and career dependence — moderate directionality (0.55). The directional asymmetry is what makes this tangled_rope rather than rope: beneficiaries (incumbent, bureaucrats) experience low extraction flow, while victims (competitors, public) experience high extraction flow, from the same constraint structure. The institutional asymmetry is the tangling mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   REGULATORY CAPTURE CLUSTER: This constraint resolves mandatrophy by showing how a single regulatory apparatus can simultaneously coordinate (for beneficiaries) and extract (from victims). The mandatrophy is not 'is this extraction or coordination?' but 'for whom?' Incumbent industry genuinely sees rope — discretion enables adaptive coordination that benefits them. Excluded competitors genuinely see snare — same discretion enables barriers that extract from them. The analytical observer risks resolving the ambiguity by claiming both coordination and extraction are illusions of perspective — 'really it's immutable bureaucratic necessity' (mountain). But the base properties contradict this: increasing extractiveness over time, increasing theater ratio, structural beneficiary/victim declaration, high suppression. The mountain classification is a false summit. The true classification is tangled_rope: genuine coordination function (discretion handles complexity) + asymmetric extraction (discretion serves beneficiaries). The mandatrophy resolves by keeping both aspects and measuring the asymmetry. The reform coalition's scaffold perspective adds the critical dimension: discretion capture is solvable through institutional redesign (algorithmic decision-making, sunshine laws, mandatory rule justification). The constraint is not naturally immutable — it persists through institutional choices that can be changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_necessity_threshold,
    'What fraction of regulatory enforcement genuinely requires discretionary judgment versus what fraction could be mechanized through rule-based decision procedures?',
    'Comparative analysis of agency enforcement data; identification of decisions that consistently correlate with industry lobbying vs decisions that follow clear statutory criteria; pilot automated enforcement systems',
    'If threshold < 20%: most discretion is capture mechanism (pure extraction). If threshold > 60%: significant portion of discretion reflects genuine complexity coordination. Shifts classification toward rope or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_necessity_threshold, empirical, 'Proportion of enforcement decisions requiring genuine discretion').

omega_variable(
    industry_influence_measurement,
    'What is the causal pathway from regulated industry lobbying to enforcement discretion outcomes? Is influence direct (quid pro quo) or indirect (alignment of preferences)?',
    'Regulatory event studies correlating industry input timing with enforcement decisions; longitudinal career tracking of regulators and postgovernment employment; analysis of agency enforcement priority shifts after industry consultation',
    'If direct quid pro quo: captures definition as snare (intentional corruption). If indirect preference alignment: captures definition as tangled_rope (coordination with asymmetric benefit). If negligible: classification reverts toward rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_influence_measurement, empirical, 'Causal mechanism of industry influence on discretionary enforcement').

omega_variable(
    identity_lock_depth,
    'For mid-career regulators, what proportion of inability to exit regulatory capture is due to material career barriers (locked pension, no external job market) versus identity fusion (professional self-concept constituted through the regulatory role)?',
    'Career history analysis of regulators who exit; interviews on exit drivers; measurement of reemployment probability and salary replacement after exit; analysis of regulatory behavior among those with and without postgovernment employment options',
    'If mostly material barriers: should reclassify as trapped or constrained rather than identity_locked. If mostly identity fusion: identity_locked classification confirmed and suggests deeper institutional lock-in mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Material vs identity-based barriers to regulator exit').

omega_variable(
    theater_ratio_causation,
    'Does the high theater ratio (0.68) reflect genuine enforcement gap or adaptive compliance theater (industry performing compliance to formal rules while maintaining capture through discretionary means)?',
    'Comparison of formal compliance rates vs actual market outcomes; analysis of enforcement narratives vs enforcement actions; industry survey on compliance burden perception vs actual regulatory pressure',
    'If enforcement gap: theater reflects failed rule system (piton classification confirmed). If adaptive theater: industry is performing compliance to maintain legitimacy while capture operates separately (extraction mechanism sophisticated enough to sustain theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_causation, empirical, 'Whether theater ratio reflects enforcement gap or adaptive compliance performance').

omega_variable(
    sunset_mechanism_feasibility,
    'Are algorithmic decision-making, sunshine laws, and mandatory rule justification actually sufficient to eliminate discretion capture, or do they merely create new layers of discretion (appeals processes, exception procedures, algorithmic auditing)?',
    'Longitudinal study of agencies that implemented transparency and automation; measurement of appeal rate and override rate; analysis of whether structured discretion replaces unstructured discretion or genuinely reduces it',
    'If sufficient: scaffold classification and sunset logic confirmed. If creates new discretion layers: reform tools trade visible capture for hidden capture (extraction becomes harder to detect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_feasibility, empirical, 'Whether transparency/automation reforms eliminate or relocate discretion capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_discretion_capture, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burcap_tr_t0, bureaucratic_discretion_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(burcap_tr_t8, bureaucratic_discretion_capture, theater_ratio, 8, 0.6).
narrative_ontology:measurement(burcap_tr_t16, bureaucratic_discretion_capture, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(burcap_be_t0, bureaucratic_discretion_capture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(burcap_be_t8, bureaucratic_discretion_capture, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(burcap_be_t16, bureaucratic_discretion_capture, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_discretion_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, regulatory_capture_general).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, barrier_to_entry_discretion).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, agency_expertise_lock_in).

% DUAL FORMULATION NOTE:
% Bureaucratic discretion capture is downstream of regulatory capture (when statutory law favors industry) but represents a distinct structural mechanism operating through enforcement practice rather than rule design. The three constraints form a family: statutory capture (written rules favor industry) feeds discretion capture (enforcement runs through agency discretion) which reinforces barrier_to_entry (competitors face discretionary obstacles). Each has different ε and different sunset mechanisms. Discretion capture is the intermediate mechanism that converts statutory advantage into practical market control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_discretion_capture, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
