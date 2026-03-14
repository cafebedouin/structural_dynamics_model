% ============================================================================
% CONSTRAINT STORY: police_use_of_force_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_police_use_of_force_authority, []).

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
 *   constraint_id: police_use_of_force_authority
 *   human_readable: Police Use of Force Authority
 *   domain: law_enforcement/criminal_justice/state_violence
 *
 * SUMMARY:
 *   Police use of force authority represents a structural constraint where
 *   state agents (police) are granted legal discretion to apply violence
 *   within defined parameters, creating inherent asymmetry between enforcer
 *   and enforced-upon. The constraint exhibits multiple decomposable
 *   mechanisms: (1) the structural authority itself (who can apply force and
 *   under what circumstances), (2) the suppression of victim recourse
 *   (qualified immunity, evidentiary burdens, trauma barriers), (3) the
 *   discretion in force threshold application (officer-by-officer variation),
 *   and (4) the legitimacy narrative that treats force authority as natural
 *   law. Citizens, particularly those in marginalized communities, are
 *   trapped within this constraint: they cannot exit the jurisdiction without
 *   economic displacement, cannot predictably avoid force encounters, and
 *   cannot effectively contest force through available legal remedies. Police
 *   departments are primary beneficiaries, experiencing the constraint as
 *   functional coordination for crime response and officer safety. Reform
 *   movements occupy an intermediate position where they benefit from the
 *   constraint's visibility for organizing while bearing its costs through
 *   escalation. The analytical challenge is to distinguish whether the
 *   constraint's extractiveness derives from the structural authority itself
 *   (immutable for state function) or from contingent institutional
 *   arrangements (reform-addressable). The theater_ratio decline (0.65 →
 *   0.52) reflects increasing video evidence and accountability pressure
 *   reducing the performative content of police justifications, while base
 *   extractiveness has increased (0.52 → 0.68) as awareness of harm has grown
 *   despite accountability reforms.
 *
 * KEY AGENTS:
 *   - Citizens and Civilian Populations: Primary victims (powerless/trapped) — subjected to police discretion with no exit capacity and suppressed legal recourse
 *   - Marginalized Communities: Secondary victims (moderate/constrained) — higher force encounter frequency, lower force thresholds, constrained geographic exit, systematic extraction
 *   - Law Enforcement Agencies: Primary beneficiaries (institutional/arbitrage) — gain authority to apply force, control threat definitions, reinterpret force levels, benefit from victim suppression
 *   - Municipal Governments: Secondary beneficiaries (institutional/arbitrage) — avoid accountability costs, maintain monopoly on legitimate violence, benefit from police legitimacy claims
 *   - Police Reform Movements: Organized challengers (organized/constrained) — benefit from constraint visibility for organizing while facing escalation risk and institutional resistance
 *   - Oversight and Accountability Bodies: Intermediate institutional actors (organized/constrained) — designed to constrain force authority but often lack enforcement power (piton dynamics)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as immutable features of state function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(police_use_of_force_authority, 0.68).
domain_priors:suppression_score(police_use_of_force_authority, 0.75).
domain_priors:theater_ratio(police_use_of_force_authority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(police_use_of_force_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(police_use_of_force_authority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(police_use_of_force_authority, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(police_use_of_force_authority, snare).
narrative_ontology:human_readable(police_use_of_force_authority, "Police Use of Force Authority").
narrative_ontology:topic_domain(police_use_of_force_authority, "law_enforcement/criminal_justice/state_violence").

domain_priors:requires_active_enforcement(police_use_of_force_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(police_use_of_force_authority, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(police_use_of_force_authority, municipal_governments).
narrative_ontology:constraint_victim(police_use_of_force_authority, civilian_populations).
narrative_ontology:constraint_victim(police_use_of_force_authority, marginalized_communities).
narrative_ontology:constraint_victim(police_use_of_force_authority, low_income_neighborhoods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN UNDER POLICE DISCRETION (SNARE) — No exit capacity. Citizens cannot refuse police encounters, cannot reliably predict force thresholds, cannot effectively contest use of force in real time. Suppression is extreme: legal recourse is structural fiction (qualified immunity, burden of proof on victim, trauma barriers to litigation). Extracted continuously through fear, compliance demand, and asymmetric violence risk.
constraint_indexing:constraint_classification(police_use_of_force_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (SNARE) — Structurally constrained. Cannot exit geographic location without economic displacement. Police encounters are higher frequency, force thresholds lower, and consequences more severe due to criminalization overlap. Constrained exit at financial/relocation cost. Extraction is systematic and documented.
constraint_indexing:constraint_classification(police_use_of_force_authority, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLICE DEPARTMENT (ROPE) — Institutional beneficiary with maximum arbitrage (can redefine threats, reinterpret force levels, reclassify incidents). Experiences the constraint as coordination mechanism: force authority enables crime response, suspect control, officer safety. The institution perceives the authority as functional necessity, not extraction. Net beneficiary — authority flows toward police agencies.
constraint_indexing:constraint_classification(police_use_of_force_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICE REFORM MOVEMENT (TANGLED ROPE) — Organized resistance with genuine power but constrained exit. Reform movements benefit from the constraint's visibility (generates coalition organizing, legal precedents, policy momentum) while also bearing its costs (violence escalation during protests, surveillance targeting, institutional resistance). Mixed extraction and coordination function.
constraint_indexing:constraint_classification(police_use_of_force_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOUNTABILITY MECHANISM (SCAFFOLD) — Independent oversight, civilian review boards, body cameras, and use-of-force reporting represent temporary interventions with sunset clauses. These mechanisms show low theater (direct measurement vs performative ritual) and have sunset logic: as transparency norms mature, external accountability mechanisms decentralize from formal boards to distributed scrutiny (body camera footage, social media review, community patrols). Suppression should decline over time if mechanisms become generational.
constraint_indexing:constraint_classification(police_use_of_force_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CRIMINAL JUSTICE LEGITIMACY RITUAL (PITON) — The constraint persists through institutional inertia and theatrical maintenance (due process rituals, court procedures, legal appeals) despite degraded function. The ritual of legal accountability no longer meaningfully constrains police discretion due to qualified immunity, evidentiary standards favoring police testimony, and victim trauma barriers. Theater_ratio remains high because the legal machinery performs justice without delivering it.
constraint_indexing:constraint_classification(police_use_of_force_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, police force authority appears as an immutable prerequisite of state function: all societies require mechanisms for enforcing law and securing monopoly on legitimate violence. From this view, the constraint is unchangeable — a natural law of political organization. However, this naturalizes what is a contingent institutional choice. The divergence between this and the snare/piton perspectives reveals false naturalization.
constraint_indexing:constraint_classification(police_use_of_force_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(police_use_of_force_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(police_use_of_force_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(police_use_of_force_authority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(police_use_of_force_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(police_use_of_force_authority, TR),
    TR >= 0.70.

:- end_tests(police_use_of_force_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Police use of force authority creates systematic extraction from civilians through multiple channels: direct harm (injury/death from force), fear tax (behavioral modification to avoid force), opportunity cost (time/resources devoted to police interaction), and identity tax (internalized criminalization by marginalized communities). The 0.68 value reflects that the constraint is primarily extractive rather than coordinative — while police legitimately need some force capacity for violent crime response, the documented evidence shows force is applied far beyond genuine threat response, disproportionately against low-threat populations (traffic stops, mental health crises, protest situations). Suppression (0.75): Very high. Multiple suppression mechanisms operate in parallel: (1) legal suppression through qualified immunity, evidentiary standards favoring police testimony, and tort liability caps; (2) institutional suppression through police union contracts protecting accused officers and restricting discipline; (3) psychological suppression through trauma barriers to formal complaints and litigation; (4) informational suppression through police control of incident narratives and evidence. Theater ratio (0.58): Moderate-high. Criminal justice ritual (arrest, booking, court procedures, sentencing) performs accountability but with degraded function — most use-of-force cases never reach trial, police testify credibly despite body camera contradictions due to testimony hierarchies, and conviction rates for police violence are near-zero nationally. The theater_ratio decline in measurements indicates accountability mechanisms (body cameras, civilian review) are reducing performative content by making force visible, creating pressure for actual constraint. Claimed type (Snare) driven by: ε ≥ 0.46, suppression ≥ 0.60, and χ ≥ 0.66 with multiple perspectives experiencing maximal extraction and minimal alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural authority produces opposite classifications depending on structural position. The beneficiary (police) perceives coordination; the victim perceives extraction. This is not disagreement about facts but disagreement about whose interests the constraint serves. The perspectival gap reveals the constraint's asymmetry rather than resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position. Police departments (institutional/arbitrage) are full beneficiaries with ability to exit (arbitrage to different jurisdictions, regulatory frameworks). Citizens (powerless/trapped) are full targets with zero exit capacity. The sigmoid f(d) scales the effective extraction chi upward as agents move from beneficiary to victim positions. Police experience low or negative chi (the authority benefits them); citizens experience maximum chi (the authority extracts from them). Marginalized communities occupy intermediate d due to constrained exit (higher cost than arbitrage but lower cost than physical/legal trapping) — they experience high chi but not maximal. The directionality derivation explains why a single institutional arrangement (police force authority) classifies as both Rope (police perspective) and Snare (civilian perspective) — it is genuinely asymmetric, and the asymmetry is structural rather than perspectival.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The constraint satisfies snare gates across multiple dimensions. Extractiveness (0.68) exceeds snare threshold (≥0.46). Suppression (0.75) exceeds snare requirement (≥0.60). The constraint requires active enforcement (police personnel, training, legal authority delegation). The victims are identifiable and numerous (civilians in police-jurisdiction areas). The beneficiaries are identifiable (police departments, municipal governments). The chi formula produces χ ≥ 0.66 for the powerless/trapped agent: with d ≈ 0.95 (full target), f(d) ≈ 1.42, and σ(national) = 1.0, chi ≈ 0.68 × 1.42 × 1.0 ≈ 0.96. The constraint is not a false snare (coordination masquerading as extraction) — it has genuine extractive content that exceeds any coordination function for the victimized population. The polymorph (multiple perspectives yielding different types) does not contradict the snare classification; it reveals the constraint's asymmetry. The snare is asymmetric — it functions as rope for the beneficiary and snare for the victim — which is exactly how extractive constraints maintain themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    force_threshold_discretion_ambiguity,
    'Is the constraint''s extraction driven by structural authority asymmetry or by individual officer discretion in force threshold application?',
    'Comparative analysis of force incident rates and severity across jurisdictions with identical legal authority but different training, accountability, and demographic composition. High variance across jurisdictions indicates discretion dominance; low variance indicates structural authority.',
    'If discretion-driven: constraint can be addressed through training/accountability changes (scaffold logic). If authority-driven: constraint requires legal restructuring (deeper snare). Classification shifts from snare/scaffold to pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(force_threshold_discretion_ambiguity, empirical, 'Whether extraction flows from structural authority or individual discretion').

omega_variable(
    qualified_immunity_causal_role,
    'How much of the suppression (victim inability to seek recourse) flows from qualified immunity doctrine vs. from underlying force authority?',
    'Jurisdictional comparison between US states with qualified immunity and those without (Illinois, New Mexico); incident outcome tracking and litigation costs before/after immunity removal.',
    'If immunity is primary: removing it addresses suppression without changing force authority (moderate reform possible). If immunity is secondary to structural authority: removing it merely shifts costs without changing extraction (snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualified_immunity_causal_role, empirical, 'Whether suppression is driven by immunity doctrine or structural authority').

omega_variable(
    alternative_law_enforcement_models,
    'Do social worker response models, community policing, or decentralized enforcement show comparable effectiveness at crime reduction without force asymmetry?',
    'Controlled comparison of response outcomes in jurisdictions piloting alternative models; crime rate, community safety, and force incident tracking; community satisfaction surveys.',
    'If effective: force authority is not structurally necessary — classification shifts from mountain/rope to snare (contingent institutional choice). If ineffective: force authority has genuine function — snare classification confirmed but with coordination component acknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_law_enforcement_models, empirical, 'Whether alternative enforcement models can replace force-based authority').

omega_variable(
    community_identity_lock_mechanism,
    'Do some communities internalize police authority as legitimate or ''just how things are'' through identity fusion (identity_locked) rather than structural entrapment?',
    'Survey and interview analysis distinguishing structural barriers to mobility (trapped), financial costs to exit (constrained), and internalized legitimacy framing (identity_locked). Longitudinal tracking of community organizing responses across different psychological bases.',
    'If identity_locked is significant: changing structural authority alone insufficient — must address internalized legitimacy narratives. If primarily trapped/constrained: structural changes (reform, accountability) address the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_identity_lock_mechanism, conceptual, 'Extent to which communities internalize police authority through identity fusion').

omega_variable(
    escalation_cycle_mechanism,
    'Is police use of force increasing over time due to changing threat environment, changing police training/norms, or changing community resistance patterns?',
    'Longitudinal tracking of force incident severity, incident type, and officer training doctrine across decades. Analysis of threat environment (weapons prevalence, serious crime rates) vs. force escalation trends.',
    'If threat-driven: force may be responding to genuine safety problems (constrained by external factors). If training/norm-driven: force is institutional choice (modifiable). If resistance-driven: force is responsive to contestation (indicates constraint''s visibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escalation_cycle_mechanism, empirical, 'Primary driver of force escalation over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(police_use_of_force_authority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pufa_tr_t0, police_use_of_force_authority, theater_ratio, 0, 0.65).
narrative_ontology:measurement(pufa_tr_t10, police_use_of_force_authority, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pufa_tr_t20, police_use_of_force_authority, theater_ratio, 20, 0.52).
narrative_ontology:measurement(pufa_tr_t30, police_use_of_force_authority, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(pufa_be_t0, police_use_of_force_authority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pufa_be_t10, police_use_of_force_authority, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(pufa_be_t20, police_use_of_force_authority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(pufa_be_t30, police_use_of_force_authority, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(police_use_of_force_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(police_use_of_force_authority, qualified_immunity_doctrine).
narrative_ontology:affects_constraint(police_use_of_force_authority, police_union_power).
narrative_ontology:affects_constraint(police_use_of_force_authority, criminalization_machinery).
narrative_ontology:affects_constraint(police_use_of_force_authority, community_distrust_feedback).

% DUAL FORMULATION NOTE:
% Police use of force authority decomposes into multiple structurally distinct constraints: (1) the structural authority itself (force_authority_grant), (2) the suppression of recourse (qualified_immunity_doctrine), (3) the discretion application (officer_force_threshold_discretion), and (4) the legitimacy narrative (force_naturalization_myth). This story focuses on the integrated constraint; decomposed stories should separate empirical (force incident data) from conceptual (legitimacy framing) and structural (legal authority) dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(police_use_of_force_authority, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
