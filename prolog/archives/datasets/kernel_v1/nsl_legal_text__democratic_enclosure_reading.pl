% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: National Security Law as Democratic Enclosure and Dissent Criminalization
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested National
 *   Security Law (NSL) kernel — the DEMOCRATIC ENCLOSURE READING. Hong Kong's
 *   2020 National Security Law, implemented unilaterally by Beijing under
 *   Article 23 of the Basic Law, is interpreted here as a mechanism for
 *   permanent closure of democratic space and criminalization of dissent.
 *   Civil society, press, and political opposition enter the victim set.
 *   Beijing central authority and the Hong Kong executive establishment enter
 *   the beneficiary set. The constraint exhibits high extractiveness (0.78)
 *   and high suppression (0.82), meeting the snare threshold. The theater
 *   ratio (0.65) reflects that NSL enforcement uses formal legal procedures
 *   and court systems (theater) alongside direct suppression, but lacks the
 *   high performative content of a piton — the enforcement is functionally
 *   effective, not merely ceremonial. The democratic enclosure reading
 *   differs structurally from two sibling readings: (1) the SOVEREIGNTY
 *   RESTORATION reading, which frames NSL as legitimate reassertion of state
 *   authority over a contested territory, and (2) the JURISDICTIONAL CAPTURE
 *   reading, which emphasizes institutional takeover of HK governance by
 *   mainland agencies. This story focuses narrowly on NSL as a mechanism that
 *   closes the democratic infrastructure itself — the capacity for collective
 *   deliberation, opposition organization, and institutional pluralism. The
 *   measurement trajectory shows rising extractiveness (0.62 → 0.78 over 3
 *   years) and rising suppression requirement (0.68 → 0.82), indicating
 *   enforcement intensification and deepening institutional capture rather
 *   than degradation or decay. This is the opposite of the decay pattern seen
 *   in piton constraints — NSL enforcement is consolidating, not decaying.
 *
 * KEY AGENTS:
 *   - Civil Society Organizations: Primary victim (powerless/trapped) — face criminalization under vague security provisions; cannot exit HK jurisdiction; lose organizing capacity
 *   - Press and Media (international and local): Primary victim (moderate/constrained) — lose editorial independence; constrained by legal exposure and licensing restrictions; some can exit (international bureaus) but at high cost
 *   - Political Opposition: Primary victim (powerless/trapped) — criminalized via vague 'subversion' provisions; no legitimate organizing pathway; no exit option within HK
 *   - Hong Kong Democratic Infrastructure (institutions, norms, constitutional pluralism): Primary victim (institutional/trapped) — the abstract institutional architecture supporting electoral and deliberative processes is systematically dismantled
 *   - Epistemic Commons: Primary victim (analytical/analytical) — inquiry into political alternatives, comparative governance, democratic theory become risky; self-censorship and institutional suppression of research pathways
 *   - Beijing Central Authority: Primary beneficiary (institutional/arbitrage) — centralizes security governance; consolidates control over contested territory; has maximum discretion in interpretation and enforcement; experiences NSL as coordination
 *   - Hong Kong Executive Establishment: Secondary beneficiary & constrained actor (institutional/constrained) — benefits from centralized authority (reduced friction with Beijing) but constrained by loss of local legitimacy and subordination to central directives
 *   - State Security Apparatus (mainland and HK): Beneficiary (institutional/arbitrage) — vastly expanded enforcement authority, jurisdiction, and resource allocation
 *   - International Democratic Coalition: Organized observer (organized/mobile) — attempts to impose costs via sanctions, diplomatic pressure, and diaspora support; views constraint as temporary with generational sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.78).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.82).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law as Democratic Enclosure and Dissent Criminalization").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '4d90e70b-4c2f-4c98-95dd-00e39cc6841b').
narrative_ontology:cs_kernel_codification('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', formalized).
narrative_ontology:cs_authority_grounding('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', extraction).
narrative_ontology:cs_interpretation_layer_present('4d90e70b-4c2f-4c98-95dd-00e39cc6841b').
narrative_ontology:cs_reading_relation('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', foundational, dissent_and_opposition_organizing_are_not_security_threats).
narrative_ontology:cs_axiom_status(dissent_and_opposition_organizing_are_not_security_threats, holdable).
narrative_ontology:cs_axiom_grounding('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', dissent_and_opposition_organizing_are_not_security_threats, deontological).
narrative_ontology:cs_axiom('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', foundational, institutional_pluralism_is_prerequisite_for_legitimate_governance).
narrative_ontology:cs_axiom_status(institutional_pluralism_is_prerequisite_for_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', institutional_pluralism_is_prerequisite_for_legitimate_governance, deontological).
narrative_ontology:cs_reference_frame('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', hong_kong_democratic_institutional_pluralism).
narrative_ontology:cs_drift_state('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', post_nsl_implementation_2020_2023, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('4d90e70b-4c2f-4c98-95dd-00e39cc6841b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_executive_establishment).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, state_security_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, press_and_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, political_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_democratic_infrastructure).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC OPPOSITION & CIVIL SOCIETY (SNARE) — Trapped within Hong Kong's territorial jurisdiction; criminalization of peaceful protest, journalism, and political organization under undefined security grounds leaves no exit option. Faces maximum extraction: loss of political voice, organizational capacity, and physical freedom. Zero alternatives within the constraint's enforcement zone.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERNATIONAL PRESS & NGOs (SNARE) — Constrained by operational presence and Hong Kong staff; can exit partially (withdraw bureau, reduce coverage) but at high cost (loss of access, organizational capacity, market presence). Suppression takes the form of legal exposure for reporters and stringent licensing/visa restrictions. Effective extraction of editorial control and self-censorship.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BEIJING CENTRAL AUTHORITY (ROPE) — Experiences NSL as a coordination mechanism: centralizing security authority, consolidating control over a contested territory, and aligning Hong Kong institutional governance with PRC systems. The constraint solves the coordination problem of how to govern a former colony with residual democratic norms. Beneficiary with maximum arbitrage options (can adjust interpretation, enforcement timing, scope) — effectively experiences zero extraction.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HONG KONG EXECUTIVE ESTABLISHMENT (TANGLED ROPE) — Constrained by dual accountability (to Beijing and to residual HK institutions). Experiences coordination benefits (centralized security authority reduces operational friction) alongside asymmetric extraction (loss of legitimate local legislative process, subordination to central directives). Can constrain but not exit the relationship with Beijing.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRATIC COALITION (SCAFFOLD) — Organized states and institutions (US, EU, G7) view NSL as a temporary enforcement mechanism with a sunset embedded in generational change: economic pressure, diplomatic isolation, and support for diaspora organizations are building alternative legitimacy and institutional pathways. High organized power and mobile exit options (can redirect investment, change alliance structures) reduce experienced extraction. Theater at medium level (performative international condemnations).
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: 'ONE COUNTRY, TWO SYSTEMS' (PITON) — The founding legitimacy frame (Joint Declaration 1984, Basic Law) has become largely performative. NSL operationally forecloses the institutional separation on which 'two systems' depended, yet the framework is maintained as a legitimacy claim and theater. The constraint persists through inertia (formal treaties, international recognition) despite its core function being degraded. Theater ratio is high (formal commitments maintained despite practical violation). Piton classification reflects this institutional degradation.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — PERMANENT ENCLOSURE (SNARE) — From a civilizational/global perspective, NSL as implemented functions as a pure extraction mechanism with permanent effect: closing the democratic space entirely, criminalizing the epistemic commons (inquiry into political alternatives), and producing irreversible institutional capture. Unlike some snares with degradation paths, this constraint is designed for stability — the extraction mechanism is not decaying but consolidating. The analytical view confirms the snare classification across all structural axes.
constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_legal_text__democratic_enclosure_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, TR),
    TR >= 0.70.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High, indicating severe asymmetric extraction. The constraint extracts political voice, organizational capacity, and institutional pluralism from the victim set (civil society, opposition, press) and concentrates it in the beneficiary set (Beijing authority, state security apparatus). The extractiveness is high rather than maximal (0.95) because some agents (international press, organized states) retain partial exit capacity and because enforcement still uses formal legal theater rather than pure coercion. Suppression (0.82): High, indicating few alternatives and high costs to resistance. The vague definitional scope of 'subversion,' 'secession,' and 'foreign collusion' creates maximum prosecutorial discretion, eliminating any predictable safe space for dissent. Physical confinement is not the suppression mechanism — rather, legal uncertainty and institutional closure suppress alternatives. Theater ratio (0.65): Medium, indicating mixed performative and functional content. NSL enforcement includes formal court trials, published judgments, and legal procedures (theater elements), but the enforcement is functionally effective at constraining behavior — people and organizations genuinely cease activities due to legal risk, not because the legal process is merely symbolic. The theater is lower than a piton's because the suppressive effect is real; higher than a pure snare because the state uses legalistic forms. Trajectory shows increasing extractiveness and suppression over 3 years (post-implementation period), indicating enforcement consolidation rather than decay or resistance. This diverges from piton-pattern decay and confirms snare-pattern stabilization.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how NSL is perceived fundamentally differently depending on structural position: Beijing authority sees coordination (Rope) — solving the governance problem of a contested territory. Hong Kong executive sees mixed coordination-extraction (Tangled Rope) — benefits from centralized authority but constrained by loss of local legitimacy. Opposition sees pure extraction (Snare) — criminalization with no exit. International press sees extraction with limited arbitrage (Snare) — can partially exit but at high cost. International coalition sees temporary mechanism with sunset (Scaffold) — believes generational pressure and economic cost will reverse the enclosure. The piton perspective on 'One Country, Two Systems' reveals how the founding legitimacy frame has become degraded — formally maintained but operationally foreclosed. The analytical observer risks seeing this as either an immutable consolidation of state authority (snare confirms extraction) or a temporary enforcement mechanism (scaffold's sunset logic). The gap between perspectives reveals the genuine structural contest: Beijing/HK establishment experience coordination and governance clarification; everyone else experiences extraction and institutional closure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains: Beijing authority — beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction (benefits). Opposition/civil society — victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum effective extraction (bears full cost). International press — victim + constrained exit → d ≈ 0.82 → f(d) ≈ 1.15 → high effective extraction (constrained choice to exit partially). International coalition — organized observer + mobile exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate effective extraction (organized power reduces the experienced constraint). HK executive — mixed actor with constrained exit → d ≈ 0.68 → f(d) ≈ 0.95 → experiences both coordination benefits (from Beijing alignment) and extraction costs (from lost local legitimacy). The spatial scope modifier (regional σ=0.9) dampens the global scope modifier slightly — NSL operates regionally on Hong Kong population and those with HK presence, though extraterritorial enforcement claims (scope expansion to global σ=1.2) represent an escalation documented in omega variables.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: NSL is classified as SNARE (ε=0.78 > 0.70 threshold). The constraint presents as security law (legitimate state authority) but functions as pure extraction (suppresses entire democratic infrastructure). The mandatrophy is resolved by recognizing that NSL conflates two distinct functions — (1) legitimate security authority to prevent terrorism/espionage (coordination benefit) and (2) systematic criminalization of peaceful dissent and opposition organizing (pure extraction). The snare classification captures this: the constraint appears to provide security coordination but actually functions to extract political voice and institutional pluralism. The beneficiary (Beijing authority) experiences genuine coordination benefit (consolidated control, reduced institutional friction), while the victim (opposition, civil society) experiences pure extraction with no coordination offset. This resolves the temptation to classify NSL as tangled rope (mixed coordination-extraction) — the coordination function is real but highly asymmetric and entirely captured by the beneficiary. From the opposition's perspective, there is zero coordination benefit; from Beijing's perspective, the coordination benefit is substantial. The snare classification reflects the victim's structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_ambiguity,
    'What constitutes ''subversion of state power'' and ''secession'' under Article 22-23 NSL? Are these definitions operationally bounded or deliberately undefined to maximize prosecutorial discretion?',
    'Comparative analysis of prosecutions; linguistic analysis of statutory language vs. prosecutorial interpretation; expert legal review of definitional precedent in mainland security law',
    'If definitions are operationally bounded: constraint may function as targeted coercive mechanism (snare with limited scope). If deliberately undefined: NSL is designed for maximum suppression and becomes mechanism for criminalizing any dissent (confirms snare with total suppression). Definitional ambiguity is a feature, not a bug — it enables scope creep.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, empirical, 'Whether NSL definitions are operationally bounded or deliberately undefined for prosecutorial discretion').

omega_variable(
    extraction_irreversibility,
    'Is the democratic enclosure produced by NSL structurally reversible (could be undone by repeal/reinterpretation) or permanently embedded through institutional capture and generational socialization?',
    'Historical analysis of mainland PRC security law durability; analysis of institutional entrenchment mechanisms (curriculum changes, media control, civil service vetting); comparison with Taiwan pre-democratization constraints',
    'If reversible: snare classification is correct but scaffold perspective (generational exit through democratic coalition pressure) is structurally plausible. If permanently embedded: snare is structurally stable and scaffold sunset is aspiration rather than real structural feature. Permanence is the key question for understanding whether constraint can degrade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_irreversibility, empirical, 'Whether democratic enclosure is reversible or permanently embedded').

omega_variable(
    reading_contention_foreclosure,
    'Does the democratic enclosure reading of NSL logically foreclose the sovereignty restoration reading (claim that NSL is legitimately restoring state authority over contested territory)? Or do these readings coexist as alternative interpretive frames held by different parties?',
    'Logical analysis of core premises; assessment of whether a single institutional actor could simultaneously hold both readings without contradiction; examination of how mainland and HK authorities frame NSL''s purpose',
    'If forecloses: the readings are mutually exclusive within any consistent framework, and one must be rejected. If coexists: both readings are live options for different parties, and the constraint embodies the genuine dispute between them. This determines whether the kernel permits multiple readings or whether one reading has defeated the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_foreclosure, conceptual, 'Whether democratic enclosure reading forecloses or coexists with sovereignty restoration reading').

omega_variable(
    international_enforcement_asymmetry,
    'Is NSL enforcement entirely within Beijing/HK authority, or do international jurisdictional claims (extraterritorial prosecution under NSL, asset seizure, diaspora targeting) represent a structural escalation that changes the constraint''s scope and power?',
    'Analysis of prosecution patterns; documentation of extraterritorial enforcement attempts; assessment of how international legal frameworks respond to NSL enforcement claims',
    'If purely territorial: NSL is regional snare affecting Hong Kong population and those with HK presence. If extraterritorial: NSL scope expands globally, affecting diaspora, international media, and academic freedom worldwide — reclassifies to global snare with universal impact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_enforcement_asymmetry, empirical, 'Whether NSL enforcement is territorial or extraterritorial').

omega_variable(
    succession_of_frameworks,
    'This reading assumes NSL is ONE constraint instantiating a contested kernel (the legal text of NSL itself, interpreted through different framings). Are there MULTIPLE distinct constraints layered on the same doctrinal text (enforcement constraint, epistemic suppression constraint, jurisdictional capture constraint)? Or is NSL genuinely a single constraint viewed through different observer positions?',
    'ε-invariance test: Do the enforcement mechanism, epistemic suppression, and jurisdictional capture have different extractiveness values when measured independently? If yes, they are separate constraints and should be decomposed into separate stories linked by network.affects_constraints.',
    'If single constraint: perspectival analysis is appropriate. If multiple constraints: each should get its own story with its own ε, and this story becomes the ''NSL enforcement mechanism'' member of a constraint family. The omega documents why the decomposition was or was not performed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_of_frameworks, conceptual, 'Whether NSL is a single constraint or multiple constraints layered on one legal text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_dem_theater_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(nsl_dem_theater_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.62).
narrative_ontology:measurement(nsl_dem_theater_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.65).

% Extraction over time
narrative_ontology:measurement(nsl_dem_extract_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(nsl_dem_extract_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.7).
narrative_ontology:measurement(nsl_dem_extract_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nsl_dem_supp_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(nsl_dem_supp_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.76).
narrative_ontology:measurement(nsl_dem_supp_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_institutional_autonomy).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, mainland_security_law_regime).

% DUAL FORMULATION NOTE:
% This constraint (democratic enclosure reading) is a member of the NSL kernel family. Three distinct readings of the same legal text produce three different constraint stories with different beneficiary/victim sets and extractiveness profiles. The democratic enclosure reading emphasizes the closure of democratic infrastructure (ε=0.78, snare). The sovereignty restoration reading emphasizes legitimate state authority reassertion (different ε, likely rope/tangled rope). The jurisdictional capture reading emphasizes institutional takeover mechanisms (different focus, different metrics). All three are linked through network.affects_constraints to indicate they are reading-siblings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
