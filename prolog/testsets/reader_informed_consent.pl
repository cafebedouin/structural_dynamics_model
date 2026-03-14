% ============================================================================
% CONSTRAINT STORY: reader_informed_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reader_informed_consent, []).

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
 *   constraint_id: reader_informed_consent
 *   human_readable: Reader Informed Consent in Information Systems
 *   domain: information_systems/cognitive_autonomy/epistemic_integrity
 *
 * SUMMARY:
 *   Reader informed consent in digital information systems represents the
 *   structural tension between the coordination benefit of information access
 *   and the extraction mechanism of attention capture and behavioral data
 *   harvesting. The constraint operates at the intersection of epistemic
 *   autonomy (the reader's cognitive right to self-determination) and
 *   platform economics (the business model dependence on attention
 *   extraction). The system exhibits high suppression through cognitive
 *   capture mechanisms (algorithmic curation, personalization), platform
 *   lock-in (network effects, data portability friction), and epistemic
 *   isolation (filter bubbles, algorithmic amplification). The extractiveness
 *   has grown monotonically over the interval as platforms have moved from
 *   information distribution toward behavioral prediction and personalized
 *   manipulation. The theater ratio has risen as consent mechanisms (cookie
 *   banners, privacy policies) have become more visible while remaining
 *   functionally ineffective — readers click through without comprehension,
 *   and alternatives to data extraction are rarely presented as genuine
 *   options. The constraint decomposes into multiple structurally distinct
 *   claims: (1) whether readers can make autonomous choices given information
 *   asymmetry (epistemological), (2) whether platform transparency can be
 *   achieved (technical), (3) whether exit costs are material enough to
 *   eliminate choice (economic), (4) whether coordination and extraction can
 *   be decoupled (architectural). The Tangled Rope classification at the
 *   primary level reflects genuine coordination function (information access)
 *   combined with asymmetric extraction (attention/data harvesting). The
 *   Scaffold perspective from the digital rights movement represents real
 *   structural change: GDPR, DMA, alternative platforms, and privacy
 *   technologies are creating exit pathways that did not exist at time 0. The
 *   Piton perspective reflects the performative nature of current consent
 *   mechanisms. The false mountain at the civilizational level reveals the
 *   naturalization trap — treating information asymmetry and attention
 *   scarcity as immutable features of communication rather than as contingent
 *   design choices.
 *
 * KEY AGENTS:
 *   - Readers (Powerless/Trapped): Primary victims — cognitive autonomy is captured through asymmetric information, algorithmic manipulation, and platform lock-in. No meaningful exit exists without abandoning information systems.
 *   - Conscious Readers (Moderate/Constrained): Secondary agent — readers with media literacy and critical capacity experience mixed coordination-extraction; have some agency through selective consumption but face real costs to platform switching.
 *   - Information Publishers (Institutional/Arbitrage): Primary beneficiary — capture audience reach and engagement value. Experience constraint as enabling coordination. Maximal exit optionality.
 *   - Platform Operators (Institutional/Constrained): Dual institutional perspective — operate under regulatory constraint while structurally dependent on behavioral data extraction. Higher d than publishers (they extract; publishers coordinate). Constrained by regulation and reputation risk.
 *   - Digital Rights Movement (Organized/Constrained): Organized beneficiary of alternative architecture — GDPR, DMA, platform alternatives, user control mechanisms. Perceive genuine sunset clause through regulatory framework and technology maturation.
 *   - Consent Ritual Infrastructure (Institutional/Arbitrage): Performative mechanism — cookie banners, privacy policies persist through legal requirement and inertia. Theater maintains appearance of consent while extraction mechanisms continue unchanged.
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective risks naturalizing contingent design as law of information systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reader_informed_consent, 0.58).
domain_priors:suppression_score(reader_informed_consent, 0.65).
domain_priors:theater_ratio(reader_informed_consent, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reader_informed_consent, extractiveness, 0.58).
narrative_ontology:constraint_metric(reader_informed_consent, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reader_informed_consent, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reader_informed_consent, tangled_rope).
narrative_ontology:human_readable(reader_informed_consent, "Reader Informed Consent in Information Systems").
narrative_ontology:topic_domain(reader_informed_consent, "information_systems/cognitive_autonomy/epistemic_integrity").

domain_priors:requires_active_enforcement(reader_informed_consent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reader_informed_consent, information_publishers).
narrative_ontology:constraint_beneficiary(reader_informed_consent, attention_economy_operators).
narrative_ontology:constraint_victim(reader_informed_consent, readers_cognitive_autonomy).
narrative_ontology:constraint_victim(reader_informed_consent, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE READER (SNARE) — Reader autonomy is trapped. Immersed in information ecosystems designed to extract attention and shape cognition without explicit consent or transparent mechanisms. Exit requires abandoning information access entirely — no meaningful alternative pathway exists. High suppression: cognitive capture mechanisms, algorithmic opacity, platform lock-in, epistemic isolation from alternative framings. Extraction flow is unidirectional: publishers extract attention, engagement, behavioral data, cognitive attention span.
constraint_indexing:constraint_classification(reader_informed_consent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSCIOUS READER / ORGANIZED RESISTANCE (TANGLED ROPE) — Readers with media literacy and critical framing capacity experience the constraint as mixed. They benefit from genuine coordination (information access, knowledge commons, networked inquiry) AND bear extraction costs (attention manipulation, cognitive labor of filtering noise, value extraction from their engagement data). Constrained exit: can reduce exposure but cannot fully exit information systems without material cost to social participation and economic opportunity. Some agency through selective consumption and platform switching.
constraint_indexing:constraint_classification(reader_informed_consent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMATION PUBLISHER (ROPE) — Experiences the constraint as pure coordination. Publishing platforms solve the genuine problem of finding audience and distributing content. Readers benefit from access; publishers benefit from audience reach. The constraint enables mutual benefit. Net beneficiary with maximal exit optionality — can change platforms, audiences, distribution mechanisms. Extraction runs toward publisher (positive directionality).
constraint_indexing:constraint_classification(reader_informed_consent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS & TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized agents (privacy advocates, transparency initiatives, digital literacy programs, alternative platforms) perceive the constraint as a temporary coordination failure with architectural solutions. GDPR, platform transparency reports, algorithmic auditing, and user-controlled data portability are creating exit pathways. The constraint has a sunset: as regulatory frameworks mature and alternative platforms mature, reader consent becomes structurally enforceable rather than performative. High suppression is tolerated because the organized coalition has agency and perceives a genuine exit window.
constraint_indexing:constraint_classification(reader_informed_consent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENT RITUAL INFRASTRUCTURE (PITON) — Cookie banners, privacy policies, and consent pop-ups are largely theatrical: readers do not genuinely comprehend what they are consenting to, alternatives are rarely presented, and opting out carries hidden costs (feature degradation, paywall activation, tracking via other pathways). The ritual persists through legal requirement and inertia, not functional protection. Theater ratio 0.68 reflects that consent mechanisms are performative rather than protective. The infrastructure maintains the appearance of informed consent while extraction mechanisms continue unchanged.
constraint_indexing:constraint_classification(reader_informed_consent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM OPERATOR (TANGLED ROPE, INSTITUTIONAL VARIANT) — Platform operators experience dual tension: they coordinate genuine information access AND extract behavioral data. They face regulatory constraint (GDPR, DMA, PPA) but operate within it through technical compliance theater. Their exit options are constrained by regulatory requirement and reputation risk. This is a second institutional perspective showing asymmetric extraction between two powerful actors — operator and regulator — with different structural relationships to the constraint. Operator's d is higher than publisher's (operator extracts; publisher coordinates). Both may classify Tangled Rope but via different mechanisms.
constraint_indexing:constraint_classification(reader_informed_consent, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN, FALSE SUMMIT) — From a civilizational view, information asymmetry and attention scarcity are inherent to any communication system — readers cannot know all consequences of their consent in complex information ecology, and no technology can eliminate this fundamental gap. The mountain framing naturalizes what is actually a contingent architectural choice: platforms could be designed with greater transparency, user control, and exit optionality (as alternative platforms demonstrate). The false summit detector identifies this as naturalization of institutional arrangement as law of nature.
constraint_indexing:constraint_classification(reader_informed_consent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reader_informed_consent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reader_informed_consent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reader_informed_consent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reader_informed_consent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reader_informed_consent, TR),
    TR >= 0.70.

:- end_tests(reader_informed_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms extract significant value through attention capture, behavioral data harvesting, and predictive manipulation. However, extraction is not maximal (not 0.72+) because: (1) readers retain some agency through platform switching and selective consumption, (2) information access itself is a genuine benefit that readers value, (3) alternative platforms exist that reduce extraction asymmetry. The value reflects steady growth over the interval as platforms have moved from simple distribution toward behavioral prediction. Suppression (0.65): High. Suppression mechanisms include cognitive capture (algorithmic personalization limiting exposure to contrary framings), platform lock-in (network effects, data portability friction), and epistemic isolation (filter bubbles). Readers face real barriers to understanding consent implications and to exiting the system — but suppression is not total (0.95) because some readers successfully navigate alternatives and because transparency initiatives are creating exit pathways. Theater ratio (0.68): High and rising. Cookie banners, privacy policies, and consent pop-ups are substantially performative: (1) readers rarely comprehend disclosure, (2) alternatives are not presented, (3) non-consent carries hidden costs, (4) extraction mechanisms continue regardless. The ratio has risen over the interval as regulatory visibility has increased while functional protection has not. Theater reflects the gap between the visible consent apparatus and the underlying behavioral extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the classical perspective divergence between extractors and targets, with the additional complexity of inter-institutional and organized-resistance perspectives. The reader (powerless/trapped) perceives a Snare — maximum extraction with no exit. The platform operator (institutional/constrained) perceives Tangled Rope — mixed coordination and extraction, with regulatory constraint limiting extraction. The publisher (institutional/arbitrage) perceives Rope — pure coordination benefit. The digital rights coalition (organized/constrained) perceives Scaffold — a temporary constraint with a sunset as regulatory frameworks and alternative platforms mature. The consent ritual (institutional/arbitrage) perceives Piton — performative mechanism maintained through inertia. The analytical observer risks a false Mountain — naturalizing information asymmetry as inherent to all communication. The perspectival gap between reader and publisher is the largest: same structural constraint classified as Snare (extraction maximum) vs. Rope (coordination pure). This gap reflects their opposite directionality values derived from beneficiary/victim status and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from structural position: (1) Readers (powerless/trapped/victim): d approaches 1.0 — experience maximum extraction, no exit alternatives. f(d) ≈ 1.42 amplifies experienced extraction. (2) Conscious readers (moderate/constrained/mixed): d ≈ 0.55 — moderate victim status (some extraction) combined with some agency (constrained exit). f(d) ≈ 0.75. (3) Publishers (institutional/arbitrage/beneficiary): d ≈ 0.15 — beneficiary status (extraction flows toward them) combined with maximal exit optionality. f(d) ≈ -0.01 (near-zero or negative effective extraction from their perspective). (4) Platform operators (institutional/constrained/both): d ≈ 0.40-0.45 — extract from readers but constrained by regulation and competition. f(d) ≈ 0.40-0.50. (5) Digital rights movement (organized/constrained): d ≈ 0.35 — organized actors that perceive exits and agency. f(d) ≈ 0.20. The scope modifier σ(S) = 1.2 for global amplifies effective extractiveness given the planetary scale of platform dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through perspectival analysis: the constraint is NOT a Rope (pure coordination) when evaluated from the reader's structural position, despite publishers experiencing it as Rope. The constraint is NOT a Mountain (natural law) when the analytical observer notes that alternative platforms with lower theater and higher transparency demonstrate that extraction is architectural choice rather than inherent necessity. The Tangled Rope classification at the primary level captures the hybrid nature: genuine coordination function (information access) combined with asymmetric extraction (behavioral data harvesting). The Scaffold classification from the digital rights perspective is not aspirational but structural — regulatory requirements (GDPR, DMA) and technological alternatives (privacy tools, alternative platforms, interoperability standards) are creating real exit pathways. The Piton classification accurately reflects the performative consent mechanisms: they are maintained through legal requirement and institutional inertia rather than functional protection. The classification prevents three mislabelings: (1) treating the system as pure coordination (Rope) when readers experience maximum extraction, (2) treating extraction as inherent to information systems (Mountain) when alternatives exist, (3) treating consent mechanisms as functionally protective (implicit Rope) when they are substantially theatrical (Piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_comprehension_threshold,
    'What constitutes ''informed'' consent for information systems that affect cognition and behavior?',
    'Cognitive science study of what information readers actually need to understand impacts; correlation between disclosure complexity and comprehension rates; analysis of whether any realistic disclosure volume can achieve genuine understanding',
    'If threshold is achievable: consent can be structurally enforced (Scaffold sunset becomes real). If threshold is unachievable: current consent framework is inherently extractive theater (Snare/Piton classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_comprehension_threshold, empirical, 'What information actually constitutes informed consent').

omega_variable(
    algorithmic_opacity_necessity,
    'Is complete algorithmic transparency necessary for informed consent, or can meaningful consent exist with partial disclosure plus user control?',
    'Comparison of user behavior under full transparency vs. controlled-choice systems; analysis of whether consent remains meaningful when disclosure is simplified vs. comprehensive',
    'If partial transparency sufficient: consent can be functionally improved without architectural revolution (Scaffold with shorter sunset). If full transparency necessary: current systems cannot satisfy consent requirement (confirms Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, conceptual, 'Whether algorithmic transparency is strictly necessary for informed consent').

omega_variable(
    exit_cost_materiality,
    'At what level of economic/social cost does constrained exit collapse into trapped exit?',
    'Empirical study of cost to reader of platform switching, data portability, information access reduction; analysis of whether costs are material enough to eliminate meaningful choice',
    'If costs are material: readers are effectively trapped regardless of formal exit options (classification shifts toward Snare). If costs are navigable: constrained exit classification holds (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_materiality, empirical, 'Threshold where exit costs eliminate meaningful choice').

omega_variable(
    coordination_extraction_decomposition,
    'Can the genuine coordination function (information access) be structurally separated from the extraction function (attention/data harvesting), or are they architecturally coupled?',
    'Analysis of alternative platform models that provide information access without behavioral data extraction; empirical study of whether decoupled systems are economically viable',
    'If separable: Tangled Rope classification is correct, and Scaffold sunset is structural (alternative platforms prove extraction is contingent). If coupled: extraction is inherent to scale, and classification approaches Snare (Tangled Rope is aspirational cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_decomposition, empirical, 'Whether coordination and extraction can be architecturally separated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reader_informed_consent, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ric_tr_t0, reader_informed_consent, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ric_tr_t5, reader_informed_consent, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ric_tr_t10, reader_informed_consent, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ric_tr_t15, reader_informed_consent, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(ric_be_t0, reader_informed_consent, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ric_be_t5, reader_informed_consent, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ric_be_t10, reader_informed_consent, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ric_be_t15, reader_informed_consent, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reader_informed_consent, information_standard).
narrative_ontology:boltzmann_floor_override(reader_informed_consent, 0.12).
narrative_ontology:affects_constraint(reader_informed_consent, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(reader_informed_consent, attention_economy_labor_extraction).
narrative_ontology:affects_constraint(reader_informed_consent, epistemic_bubble_formation).

% DUAL FORMULATION NOTE:
% Reader informed consent decomposes into three structurally related constraints: (1) algorithmic_amplification_bias (ε ≈ 0.35) — how algorithmic curation shapes information distribution without transparency, (2) attention_economy_labor_extraction (ε ≈ 0.62) — how reader attention is monetized without compensation or consent, (3) epistemic_bubble_formation (ε ≈ 0.51) — how personalization creates cognitive isolation. Reader informed consent (ε = 0.58) sits above this cluster, capturing the meta-constraint that unites them: the absence of reader agency in how these mechanisms operate. Each upstream constraint has its own extractiveness reflecting the specific mechanism; the meta-constraint reflects lack of informed choice about the entire system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reader_informed_consent, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
