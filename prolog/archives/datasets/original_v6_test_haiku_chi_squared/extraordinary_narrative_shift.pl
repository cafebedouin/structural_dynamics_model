% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extraordinary_narrative_shift, []).

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
 *   constraint_id: extraordinary_narrative_shift
 *   human_readable: The Narrative Framing of "Extraordinary" Experience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The framing of experiences as 'extraordinary' or 'ordinary' constitutes a
 *   structural constraint on whose suffering is validated, whose narrative is
 *   heard, and whose needs receive institutional resources. This constraint
 *   operates through institutional authorities (media, medicine, psychology,
 *   academia) who possess the social power to designate certain experiences
 *   as noteworthy and others as mundane. The mechanism extracts from those
 *   whose experiences are designated ordinary: their distress is
 *   delegitimized, their needs are deprioritized, their agency in
 *   interpreting their own experience is denied. Simultaneously, the
 *   constraint performs a coordination function—shared frameworks for
 *   distinguishing signal from noise are genuinely necessary for collective
 *   meaning-making. The tension between this legitimate coordination role and
 *   the extractive gatekeeping creates a tangled rope structure. The theater
 *   ratio has increased over the 50-year interval (0.42 → 0.65) as
 *   institutional authorities have increasingly relied on performative
 *   diagnostic and narrative rituals to maintain their authority while the
 *   actual validating function of their frameworks has degraded. Examples
 *   include: psychiatric diagnosis expanding to pathologize ordinary distress
 *   (DSM expansion), media 'inspiration narratives' extracting from
 *   disability communities while denying systemic resource needs, trauma
 *   validation remaining inaccessible to those without institutional
 *   pathways, and lived experience being centered rhetorically while material
 *   gatekeeping persists.
 *
 * KEY AGENTS:
 *   - Ordinary Experiencers: Primary victim (powerless/trapped) — individuals whose experience is deemed mundane by institutional arbiters; trapped within delegitimizing frames with no exit
 *   - Institutional Narrative Arbiters: Primary beneficiary (institutional/arbitrage) — media organizations, medical/psychiatric institutions, academic authorities; benefit from coordination function while exercising extractive gatekeeping
 *   - Alternative Narrators: Secondary victim (moderate/constrained) — individuals and small groups developing counter-framings; resource-constrained but able to build local credibility
 *   - Collective Counter-Movements: Organized victim (organized/constrained) — disability justice, trauma-informed practice, peer support movements building collective challenge to institutional framings; constrained by power asymmetry but organized for resistance
 *   - Professional Diagnostic Systems: Institutional actor (institutional/arbitrage) — psychiatric, psychological, medical classification systems; see their own categories as degraded (expanded DSM, contested validity) yet maintain through professional investment (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional power arrangements as inevitable features of human cognition and social organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.52).
domain_priors:suppression_score(extraordinary_narrative_shift, 0.58).
domain_priors:theater_ratio(extraordinary_narrative_shift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extraordinary_narrative_shift, tangled_rope).
narrative_ontology:human_readable(extraordinary_narrative_shift, "The Narrative Framing of \"Extraordinary\" Experience").
narrative_ontology:topic_domain(extraordinary_narrative_shift, "social/psychological").

domain_priors:requires_active_enforcement(extraordinary_narrative_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, narrative_authority_holders).
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, attention_concentrators).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, ordinary_experiencers).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, unsanctioned_narratives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY EXPERIENCER (SNARE) — Individual whose experience is deemed ordinary by institutional arbiters cannot claim attention, resources, or validation without risking ridicule or delegitimization. Trapped within the frame that designates their experience as mundane. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE NARRATOR (TANGLED ROPE) — Constrained by lack of institutional platform and social capital, yet may achieve local credibility through persistent countervailing narratives. Experiences both extraction (delegitimization, resource denial) and coordination benefit (community formation around alternative framings). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL NARRATIVE ARBITER (ROPE) — Media organizations, academic institutions, therapeutic professions maintain gatekeeping power over what experiences are framed as extraordinary vs ordinary. Benefits from coordination function (sharing common framings reduces social friction), experiences constraint as enabling their legitimate authority to distinguish signal from noise. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLLECTIVE COUNTER-MOVEMENT (TANGLED ROPE) — Social movements (disability justice, lived experience testimony, trauma-informed practice) are building organized challenge to institutional extraordinary/ordinary framings. Constrained by institutional power asymmetry, but possess strategic capacity to reframe through solidarity and testimony multiplication. See coordination function (validating shared suppressed narratives) alongside extraction resistance (delegitimization, funding denial). d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.29.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL DIAGNOSTIC SYSTEM (PITON) — Psychiatric, psychological, and medical diagnostic categories (DSM, ICD) purport to classify experiences objectively (extraordinary = pathological, ordinary = healthy). The classification ritual persists through institutional inertia and professional credential maintenance, but lacks genuine validating function for individual experience. Theater ratio 0.65 reflects performative diagnostic gatekeeping. The system sees its own categories as increasingly degraded (multiple editions, expansions, contested validity) yet persists through professional investment. d≈0.05, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some distinction between extraordinary and ordinary experiences appears inherent to human cognition and social organization: attention is finite, not all experiences can be equally validated, and collective meaning-making requires shared frameworks. The framing appears as inevitable constraint of human social nature. However, structural data (ε=0.52, suppression=0.58, theater=0.65) contradicts mountain classification — reveals false summit. The distinction is contingent institutional power arrangement, not natural law.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extraordinary_narrative_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(extraordinary_narrative_shift, TR),
    TR >= 0.70.

:- end_tests(extraordinary_narrative_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from ordinary experiencers through denial of validation and resources, but this is not maximal extraction because alternative networks and counter-movements do achieve some validity independent of institutional recognition. The extraction operates through gatekeeping (controlling who is heard) rather than through violent coercion. Suppression (0.58): Moderate-high. Significant barriers include: institutional credential requirements for narrative authority, media gatekeeping, professional licensing that restricts who can validate experiences, internalized delegitimization (ordinary experiencers often don't attempt to claim extraordinary status even when they might). But suppression is incomplete — alternative pathways exist through peer networks, social media, lived experience communities. Theater ratio (0.65): Moderate-high. Institutional framing operates substantially through performative rituals: diagnostic interviews that create appearance of objective assessment, professional authority displays that mask subjective gatekeeping, media narrative formulas that extract emotional labor while denying systemic acknowledgment. These rituals have increased in performativity as their functional validating capacity has declined (expansion of diagnostic categories, contested validity of professional assessment, mismatch between diagnostic labels and lived experience). The trajectory from 0.42 to 0.65 reflects degradation: the ritual continues but increasingly for its own sake.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival disagreement. The ordinary experiencer sees pure extraction (Snare) — their suffering is denied legitimacy for no benefit to them. The institutional arbiter sees coordination (Rope) — shared frameworks genuinely enable collective sense-making. The alternative narrator sees mixed extraction and community benefit (Tangled Rope) — delegitimization is real but so is the validation found in counter-communities. The organized counter-movement sees a temporary problem with a growing exit path (Scaffold implicit, Tangled Rope explicit) — institutional narratives are losing power as distributed authority (peer testimony, lived experience testimony, social media) increases. The professional diagnostic system sees its own degradation (Piton) — categories persist through institutional investment despite declining functional validity. The civilizational observer risks seeing inevitable human constraint (Mountain) — but the structural data reveals this as false summit: extraordinary/ordinary distinction is contingent institutional arrangement, not natural law of experience or cognition.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordinary experiencers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No institutional platform, no credential, no social capital to challenge designation. Institutional narrative arbiters: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Possess authority to designate and resources to enforce. Alternative narrators: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction (delegitimization, resource denial) but not total (can build local credibility). Collective counter-movements: Organized + constrained → d≈0.45, f(d)≈0.45. Moderate extraction offset by collective organizing capacity and growing institutional vulnerability to counter-narrative pressure. Professional diagnostic systems: Institutional + arbitrage → d≈0.05, f(d)≈-0.10. Piton classification comes from theater gate (0.65 ≥ 0.70 gate fails narrowly; piton is diagnostic choice not automatic). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk; false summit detection applies.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint is a textbook case of how pure coordination language can mask extraction. The mandate would be: 'The extraordinary/ordinary distinction is a necessary coordination mechanism for collective sense-making.' This is partially true — shared frameworks are necessary. But the mandate systematically obscures the extraction: the designation is controlled by institutional authorities who benefit from exclusive gatekeeping power, the process is performative (diagnostic rituals that simulate objectivity while masking subjective gatekeeping), and the victims (ordinary experiencers) have no voice in the framing. The tangled rope classification resolves this by acknowledging both coordination function (genuine) and extraction asymmetry (also genuine). The mandate becomes visible as a form of capture: 'We need gatekeeping for sense-making, and we (institutional authorities) will be the gatekeepers.' The theater ratio increase (0.42 → 0.65) marks the point at which institutional coordination language becomes predominantly performative — the ritual persists past the point of functional necessity, maintained by professional investment rather than actual validating capacity. The counter-movements are beginning to offer competing coordination mechanisms (peer validation, testimony networks, collective framings) that may sunset the institutional extraction while preserving the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_construction,
    'Is the distinction between extraordinary and ordinary experiences a reflection of genuine structural differences in experience itself, or is it constructed entirely through social framing and institutional power?',
    'Cross-cultural ethnographic analysis comparing extraordinary/ordinary distinctions across cultures with different narrative authorities; neurobiological markers of subjective intensity vs institutional designation',
    'If structural differences exist: constraint has mountain properties despite high extraction. If purely constructed: classification as snare/tangled rope is confirmed, naturalizing language is ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_construction, empirical, 'Whether extraordinary/ordinary distinction reflects real experience structure or pure social construction').

omega_variable(
    alternative_authority_legitimacy,
    'Can non-institutional narrative authorities (peer networks, lived experience communities, indigenous knowledge systems) successfully legitimize experiences that institutional arbiters classify as ordinary without institutional recognition?',
    'Longitudinal tracking of healing/validation outcomes in alternative networks vs institutional systems; measurement of social capital accumulation through counter-narrative validation; analysis of cultural transmission of reframed narratives',
    'If yes: scaffold perspective is valid (alternative framings can sunset institutional control). If no: institutional gatekeeping is nearly absolute (snare classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_authority_legitimacy, empirical, 'Whether alternative authorities can legitimize suppressed narratives without institutional support').

omega_variable(
    narrative_spillover_velocity,
    'What is the typical lag time before institutional narrative authorities incorporate reframed experiences, and does this lag represent extraction (delayed validation creating harm) or legitimate epistemic caution?',
    'Historical analysis of narrative reframing adoption rates (PTSD recognition in VA, chronic pain in medicine, autism self-identification, etc.); correlation between lag time and documented harm to suppressed experiencers; comparison with adoption rates for institutional-originated narratives',
    'If lag < 5 years average: epistemic caution plausible. If lag > 15 years average: extraction via denial is structural feature, not side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_spillover_velocity, empirical, 'Timeline lag between counter-narrative emergence and institutional adoption').

omega_variable(
    experience_intensity_vs_authority_status,
    'Is ''extraordinariness'' correlated with actual intensity/impact of experience, or primarily with social status and narrative authority of the narrator?',
    'Quantitative analysis of experience intensity metrics (subjective distress, functional impairment, health outcomes) vs institutional extraordinary designation; comparison across narrator social status levels holding experience intensity constant',
    'If intensity-correlated: framing is epistemically functional (snare aspects are overstatement). If authority-correlated: framing is pure extraction mechanism (snare/tangled rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(experience_intensity_vs_authority_status, empirical, 'Correlation between experience intensity and institutional extraordinary designation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extraordinary_narrative_shift, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exnr_tr_t0, extraordinary_narrative_shift, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exnr_tr_t25, extraordinary_narrative_shift, theater_ratio, 25, 0.58).
narrative_ontology:measurement(exnr_tr_t50, extraordinary_narrative_shift, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(exnr_be_t0, extraordinary_narrative_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exnr_be_t25, extraordinary_narrative_shift, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(exnr_be_t50, extraordinary_narrative_shift, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extraordinary_narrative_shift, information_standard).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, diagnostic_category_proliferation).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, attention_economy_gatekeeping).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, institutional_credential_monopoly).

% DUAL FORMULATION NOTE:
% The narrative framing constraint is downstream of specific institutional power asymmetries (who controls media, who possesses credentials, who defines professional authority). Each of those upstream constraints has distinct ε values reflecting their specific structural properties. The narrative framing constraint aggregates the extractive effects of institutional control across multiple domains (medicine, psychology, media, academia) into a single shared mechanism: the extraordinary/ordinary distinction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(extraordinary_narrative_shift, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
