% ============================================================================
% CONSTRAINT STORY: postman_survival_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_postman_survival_protocol, []).

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
 *   constraint_id: postman_survival_protocol
 *   human_readable: Postman's Protocol for Informational Hygiene
 *   domain: social/technological
 *
 * SUMMARY:
 *   Neil Postman's protocol for informational hygiene—articulated most fully
 *   in 'Amusing Ourselves to Death' and 'Technopoly'—proposes a set of
 *   defensive practices for individuals and communities to maintain
 *   epistemological agency in the face of technological systems designed to
 *   capture attention, manipulate behavior, and reduce citizens to consumers.
 *   The constraint operates at the intersection of individual autonomy and
 *   institutional power: technology companies extract value through
 *   behavioral prediction and attention capture, yet they simultaneously
 *   provide genuine coordination services (connectivity, distributed
 *   information access). Postman's protocol is the tension between these
 *   functions. It manifests as a coordination mechanism (Rope: community
 *   literacy practices, analog refuges, intentional technology use) but faces
 *   increasing institutional encroachment (Tangled Rope: libraries and
 *   schools adopting the protocol while dependent on technology company
 *   partnerships) and theatrical performance (Piton: 'digital wellness'
 *   features that obscure rather than solve extraction). The constraint
 *   exhibits high perspectival variance: the individual consumer caught in
 *   algorithmic feeds sees a Snare; the cultural literacy coalition sees pure
 *   Rope; platform companies see a Tangled Rope where they both benefit and
 *   face risk; legacy media see their own degradation (Piton); and the
 *   civilizational observer sees a coordination mechanism for preserving
 *   human agency across a technological transition.
 *
 * KEY AGENTS:
 *   - Individual Information Consumers: Primary victims (powerless/trapped) — ensnared in attention-capture systems with minimal exit options; protocol is their primary defense
 *   - Cultural Literacy Communities: Primary beneficiaries (institutional/arbitrage) — teachers, librarians, independent media, civic organizations that benefit from increased cultural authority and trust as alternatives to algorithmic feeds
 *   - Public Library System: Secondary actor (organized/constrained) — institutional coordinator providing analog refuge and literacy infrastructure; victim of budget constraints and technology company licensing restrictions
 *   - Legacy Media Institutions: Secondary actor (institutional/arbitrage) — theatrically maintained through subscription nostalgia while losing their original agenda-setting function to algorithms
 *   - Technology Platform Companies: Institutional extractor (powerful/mobile) — extract attention and behavioral data; simultaneously provide coordination services; threatened by critical awareness of manipulation
 *   - Regulatory Bodies: Potential future actor — emergence of technology regulation (EU DMA, potential US legislation) could shift platforms toward transparent, less extractive design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postman_survival_protocol, 0.35).
domain_priors:suppression_score(postman_survival_protocol, 0.48).
domain_priors:theater_ratio(postman_survival_protocol, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postman_survival_protocol, extractiveness, 0.35).
narrative_ontology:constraint_metric(postman_survival_protocol, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(postman_survival_protocol, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(postman_survival_protocol, rope).
narrative_ontology:human_readable(postman_survival_protocol, "Postman's Protocol for Informational Hygiene").
narrative_ontology:topic_domain(postman_survival_protocol, "social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(postman_survival_protocol, individual_information_autonomy).
narrative_ontology:constraint_beneficiary(postman_survival_protocol, cultural_literacy_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS CONSUMER (SNARE) — Individual citizens ensnared in algorithmic feeds, notifications, and attention-capture systems designed by institutional media and tech companies. Exit options are minimal: digital participation is now mandatory for employment, social connection, and civic engagement. The protocol is their only defense, but it requires constant active resistance against designed persuasion. d≈0.90, f(d)≈1.38, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(postman_survival_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CULTURAL LITERACY COALITION (ROPE) — Teachers, librarians, independent media organizations, and civic institutions that recognize Postman's protocol as a coordination mechanism for protecting epistemological commons. They benefit from the protocol's adoption through increased cultural authority and public trust in alternatives to algorithmic feeds. The protocol itself is pure coordination: sharing critical media literacy, building reading groups, establishing information-free zones (analog media, face-to-face discourse). d≈0.10, f(d)≈-0.04, σ=1.0 → χ≈-0.01. Negative effective extraction: net beneficiary.
constraint_indexing:constraint_classification(postman_survival_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC LIBRARY SYSTEM (TANGLED ROPE) — Libraries are simultaneously coordination partners (providing analog refuge, cultural literacy infrastructure) and victims of informational extraction (competing against free algorithmic feeds, budget-constrained, losing patrons). The protocol benefits them through renewed mission clarity but requires enforcing boundaries against tech company incursion (WiFi contracts, ebook licensing restrictions, surveillance of patron data). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(postman_survival_protocol, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGACY MEDIA INSTITUTIONS (PITON) — Newspapers, broadcast networks, and publishing houses that once coordinated information flow but now operate as degraded rituals. The protocol theoretically supports their continued relevance (print journalism as 'curated information'), but they are theatrically maintained through nostalgia and subscription models while their primary function (agenda-setting, fact-checking at scale) has been displaced by algorithms. theater_ratio=0.62 reflects that traditional editorial gatekeeping is now largely performative — the audience has fragmented and attention flows through social platforms regardless of editorial decisions. d≈0.02, f(d)≈-0.19, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(postman_survival_protocol, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGY PLATFORM COMPANIES (TANGLED ROPE) — Meta, Google, TikTok extract value through attention capture and behavioral prediction (ε≈0.35 base extraction from users) while simultaneously providing a genuine coordination service: connecting distributed people, enabling real-time information access, reducing friction in social discovery. The protocol threatens their extraction mechanism (critical awareness of algorithmic manipulation reduces attention capture), but platforms themselves have begun adopting 'wellness' features (screen time limits, algorithmic transparency) suggesting they recognize the coordination legitimacy of the constraint. They are simultaneously beneficiaries (of informational asymmetries they exploit) and victims (of cultural backlash and potential regulation). d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(postman_survival_protocol, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL OBSERVER (ROPE) — From the longest timescale, the protocol is a civilizational coordination mechanism for preserving human agency across a technological transition. Societies adopting informational hygiene practices (media literacy, analog alternatives, intentional technology use) are solving a collective action problem: how to capture the productivity benefits of digital systems while preventing the depersonalization and behavior manipulation that threaten autonomy. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(postman_survival_protocol, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postman_survival_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(postman_survival_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postman_survival_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(postman_survival_protocol, TR),
    TR >= 0.70.

:- end_tests(postman_survival_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, increasing over the measurement interval from 0.18 to 0.35. Base extraction reflects the value captured by technology companies through attention harvesting, behavioral prediction, and advertising-driven business models. The protocol's adoption has not decreased underlying extraction but has increased awareness of it—extraction becomes more visible and thus more psychologically significant even if the structural mechanism remains. The rising trajectory reflects the intensification of algorithmic manipulation (more sophisticated attention-capture, deeper integration into daily life) and the corresponding increase in required defensive effort. Suppression (0.48): Moderate-high. Significant barriers to protocol adoption include technological inevitability framing ('digital is the future'), network effects (social pressure to remain on platforms), employment requirements (digital literacy demanded by employers), and the designed friction of stepping away from convenience. However, suppression is not total—communities can and do adopt practices, and analog alternatives exist and function. Theater ratio (0.62): Moderately high. Rising from 0.35 to 0.62 over the interval reflects increasing performativity. Technology companies now adopt 'wellness' features (screen time limits, algorithmic transparency dashboards) that appear to address the protocol's concerns while preserving the underlying extraction mechanism. Educational institutions teach 'digital citizenship' while increasing surveillance of student data. Libraries promote media literacy while negotiating licensing restrictions with tech companies. The theater reflects institutional co-optation: the protocol's language is adopted while its intent (reducing extraction) is neutralized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival variance across power asymmetries. The powerless individual consumer sees a Snare (trapped in systems designed to manipulate them). The institutional beneficiary (teachers, librarians) sees pure Rope (coordination mechanism for cultural authority). The public library system sees Tangled Rope (simultaneously enabling cultural literacy and victim of technology company licensing). Legacy media see Piton (their editorial role degraded but theatrically maintained). Technology platforms see Tangled Rope (extraction mechanism threatened by awareness but also coordination service they provide). The civilizational observer sees Rope (adaptive coordination for preserving human agency). The perspectival gaps reflect genuine structural differences: the beneficiary's experience of coordination is not an illusion—libraries do provide real literacy services—while the victim's experience of extraction is equally real. The mandatrophy resolution lies in recognizing that both are simultaneous: the protocol IS coordination (libraries, literacy communities) AND the underlying technological extraction continues unabated. The protocol's strategic function is not to eliminate extraction but to make it visible and to preserve individual agency within extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual consumers: Victim + trapped → d≈0.90, f(d)≈1.38. Near-maximum extraction: digital participation is mandatory, exit is costly, manipulation is designed-in. Cultural literacy communities: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.04. Net beneficiary: they gain cultural authority and institutional purpose from the protocol's adoption. Public libraries: Mixed victim/beneficiary + constrained → d≈0.55, f(d)≈0.75. Constrained because they depend on public funding and technology company partnerships; they are both coordinators and partially captured. Tech platforms: Powerful actor with mixed extraction/coordination + mobile → d≈0.45, f(d)≈0.45. They can exit regulatory pressure through geographic arbitrage but are also threatened by cultural backlash. Legacy media: Institutional/arbitrage but degraded → d≈0.02, f(d)≈-0.19. Their d remains low because they continue to have exit options (paywalls, subscriber loyalty) even as their function degrades. Civilizational observer: d≈0.72, f(d)≈1.15. Observer position emphasizing the constraint's role in preserving species-level human agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the protocol is simultaneously genuine coordination AND fails to eliminate extraction. The Rope classification (primary: perspectives 2 and 6) reflects that the protocol genuinely coordinates communities around informational autonomy. The Snare classification (perspective 1) reflects that individual consumers remain trapped despite the protocol's existence. The Tangled Rope classifications (perspectives 3 and 5) reflect institutional actors who benefit from coordination language while perpetuating extraction (public libraries gain mission clarity while becoming tech company partners; platforms adopt wellness language while maintaining attention-capture mechanics). The Piton classification (perspective 4) reflects that legacy media gatekeeping is now largely theater. The classification plurality is not a failure of the framework—it is the accurate representation of a constraint that IS genuinely coordinating AND genuinely extractive depending on structural position. The mandatrophy resolves through recognition that informational hygiene practices are adaptive mechanisms for living within extraction, not mechanisms for eliminating it. The protocol is Rope (coordination) because it builds real communities and literacy. It is also Snare (extraction) because the underlying technological infrastructure continues to manipulate even protocol-adopters. It is Tangled Rope because institutional adoption often co-opts the protocol for different extraction purposes. This multiplicity reflects the actual structure of technology governance: there is no single 'correct' classification because the constraint operates as simultaneous coordination and extraction depending on which institutional actor's experience you center.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_contingency,
    'Is informational overload and attention capture a necessary feature of digital technology, or a contingent design choice that could be engineered differently?',
    'Comparative analysis of platform designs (TikTok engagement vs. Wikipedia experience); examination of historical evidence from early web (pre-algorithmic feeds); controlled experiments comparing algorithmic vs. curated feeds on user agency metrics',
    'If necessary: Postman''s protocol is an adaptive coping mechanism (Rope: pure coordination). If contingent: platform design is extractive (Snare/Tangled Rope) and could be reformed through regulation or market pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_contingency, empirical, 'Whether attention-capture design is inherent to digital technology or contingent on choices').

omega_variable(
    protocol_effectiveness_measurability,
    'Can informational hygiene practices be measured as increasing individual autonomy, or are they merely coping theater that leaves underlying structural extraction intact?',
    'Longitudinal studies of protocol adopters: compare attention patterns, decision-making autonomy, exposure to manipulative content, life satisfaction, and susceptibility to social engineering across protocol practitioners vs. controls. Measure whether protocol adopters maintain independence from algorithmic recommendations or merely become consciously aware of their dependence.',
    'If effective: protocol represents genuine Rope coordination and should be institutionalized in education/public libraries. If ineffective: protocol is Piton (performative coping) that legitimizes individual blame while leaving extraction intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protocol_effectiveness_measurability, empirical, 'Whether informational hygiene practices measurably increase autonomy or constitute theater').

omega_variable(
    scalability_of_analog_alternatives,
    'Can analog information systems (print, face-to-face discourse, local community networks) actually scale to replace the coordination functions provided by digital platforms, or do they require technological infrastructure that reintroduces the original problem?',
    'Case studies of large-scale analog information coordination (historical cities, monastic knowledge networks, contemporary non-digital economies); analysis of costs, latency, and coverage compared to digital alternatives. Test whether localized digital networks (municipal broadband with algorithmic transparency) can provide coverage without extractive attention capture.',
    'If analog can scale: protocol is viable Rope coordination. If it cannot: protocol is Scaffold (temporary transition mechanism) requiring technological reform rather than technology abandonment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_of_analog_alternatives, empirical, 'Whether analog alternatives can scale to replace digital platform coordination').

omega_variable(
    regulatory_capture_of_protocol,
    'Will institutions adopting the protocol co-opt it for their own extraction? (e.g., schools teaching ''digital citizenship'' while normalizing surveillance, libraries promoting ''media literacy'' with funding from tech companies)',
    'Track institutional adoption of Postman-derived frameworks in education, libraries, policy; measure whether adopting institutions maintain independence from technology companies or become gatekeepers of approved informational norms. Examine whether ''digital wellness'' features deployed by platforms reduce extraction or merely obscure it.',
    'If captured: protocol becomes Piton (degraded ritual). If institutions maintain independence: protocol remains Rope coordination with real autonomy gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_protocol, empirical, 'Whether institutional adoption co-opts or preserves the protocol''s autonomy function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(postman_survival_protocol, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(postman_tr_t0, postman_survival_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(postman_tr_t10, postman_survival_protocol, theater_ratio, 10, 0.5).
narrative_ontology:measurement(postman_tr_t20, postman_survival_protocol, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(postman_be_t0, postman_survival_protocol, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(postman_be_t10, postman_survival_protocol, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(postman_be_t20, postman_survival_protocol, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(postman_survival_protocol, information_standard).
narrative_ontology:affects_constraint(postman_survival_protocol, algorithmic_attention_capture).
narrative_ontology:affects_constraint(postman_survival_protocol, platform_epistemic_asymmetry).
narrative_ontology:affects_constraint(postman_survival_protocol, digital_literacy_gatekeeping).

% DUAL FORMULATION NOTE:
% Postman's protocol is a meta-constraint on information flow and individual autonomy. It is upstream of specific technology platform mechanics (attention capture, recommendation algorithms, behavioral prediction) and downstream of more fundamental constraints on human attention and cognitive capacity. Decomposing the constraint family: (1) biological_attention_scarcity (Mountain: humans have fixed attentional bandwidth) → (2) algorithmic_attention_capture (Tangled Rope: systems designed to exploit this constraint) → (3) postman_survival_protocol (Rope: cultural response coordinating defensive practices). Each story in the family has distinct ε values reflecting empirical status and structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(postman_survival_protocol, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
