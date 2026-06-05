% ============================================================================
% CONSTRAINT STORY: narcissistic_ego_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narcissistic_ego_maintenance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: narcissistic_ego_maintenance
 *   human_readable: The Mirror Trap: Narcissistic Ego Maintenance
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The Mirror Trap describes a relational constraint where one partner (The
 *   Echo) is structurally positioned to provide constant narcissistic supply
 *   — validation, admiration, attention, identity-mirroring — to the other
 *   partner (The Image). This constraint exhibits high extractiveness (0.68)
 *   because the echo bears psychological, emotional, and often material costs
 *   while the narcissist captures ego-regulation benefits. The constraint is
 *   maintained through high suppression (0.75): the echo experiences limited
 *   exit options due to identity fusion (self-concept becomes defined through
 *   the relationship), emotional/financial entanglement, fear of abandonment
 *   or retaliation, and institutional failure to intervene. The theater ratio
 *   (0.58) reflects that much narcissistic supply-seeking appears as ordinary
 *   relationship behavior — gift-giving, attention-seeking, displays of
 *   affection — making the extractive mechanism difficult to name or
 *   intervene in. The constraint operates through psychological isolation
 *   rather than overt coercion, yet produces substantial relational harm: the
 *   echo experiences identity erosion, hypervigilance, attachment trauma, and
 *   often intergenerational transmission of narcissistic dynamics to
 *   children. The constraint classifies as a Snare from the echo's
 *   perspective (powerless/trapped), as Rope from the narcissist's
 *   perspective (institutional/arbitrage beneficiary), and as Tangled Rope
 *   from the extended social network's perspective (constrained participation
 *   in supply provision and social facade management). The analytical
 *   naturalization — that ego maintenance and mirror-seeking are inherent to
 *   human psychology — represents a false summit that obscures the contingent
 *   institutional and relational mechanisms that enable extraction.
 *
 * KEY AGENTS:
 *   - The Echo: Primary victim (powerless/trapped) — partner compelled to provide narcissistic supply; bears psychological and relational costs
 *   - The Narcissistic Subject (The Image): Primary beneficiary (institutional/arbitrage) — extracts ego-maintenance benefits; retains high exit optionality and can replace mirrors
 *   - Dependent Family Members: Secondary victims (moderate/constrained) — children, elderly parents, or household dependents experience secondary extraction and may internalize narcissistic dynamics
 *   - Extended Social Network: Tertiary participants (moderate/constrained) — friends, colleagues, extended family recruited into supply provision and facade maintenance; experience mixed coordination-extraction
 *   - Institutional Systems: Observational actors (institutional/arbitrage) — therapists, HR professionals, legal systems that attempt intervention but often devolve into performative compliance (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent relational dynamics as immutable psychological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narcissistic_ego_maintenance, 0.68).
domain_priors:suppression_score(narcissistic_ego_maintenance, 0.75).
domain_priors:theater_ratio(narcissistic_ego_maintenance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, extractiveness, 0.68).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narcissistic_ego_maintenance, snare).
narrative_ontology:human_readable(narcissistic_ego_maintenance, "The Mirror Trap: Narcissistic Ego Maintenance").
narrative_ontology:topic_domain(narcissistic_ego_maintenance, "psychological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narcissistic_ego_maintenance, narcissistic_subject).
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, echo_partner).
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, echo_relational_integrity).
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, collective_trust_environment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ECHO (SNARE) — The partner trapped in narcissistic supply provision. Powerless + trapped → d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.77. Maximum extraction. Exit is perceived as impossible due to identity fusion, financial entanglement, or threat of abandonment/retaliation. The echo cannot organize exit because the constraint operates through psychological isolation and identity degradation.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEPENDENT FAMILY MEMBERS (SNARE) — Children, elderly parents, or other household members dependent on the narcissistic dyad. Moderate + constrained → d≈0.75, f(d)≈1.10, σ=0.8 → χ≈0.60. High extraction. Exit constrained by guardianship, financial dependency, or fear of disrupting household stability. Secondary victims who internalize narcissistic dynamics or develop attachment trauma.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE NARCISSISTIC SUBJECT (ROPE) — The beneficiary experiences the constraint as a coordination mechanism: organizing mirrors (partners, audiences, admirers) to maintain ego-state. Institutional + arbitrage → d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.07. Net beneficiary. The narcissist has high exit optionality (can discard mirrors and find new ones) and experiences the constraint as solving the problem of maintaining grandiose self-image through supply streams.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: EXTENDED SOCIAL NETWORK (TANGLED ROPE) — Friends, colleagues, extended family who observe or experience narcissistic dynamics. Moderate + constrained → d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.61. Mixed coordination-extraction. The network both coordinates around the narcissist's social presentation (managing reputational risk, providing validation) AND experiences extraction (being recruited into supply provision, emotional labor, social exclusion if they don't mirror). Active social enforcement maintains the facade.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THERAPEUTIC AND PROFESSIONAL SYSTEMS (PITON) — Therapists, counselors, coaches, human resources personnel who attempt to address narcissistic dynamics. Institutional + arbitrage → d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.06. Theater_ratio=0.58 reflects that therapeutic interventions often devolve into performative compliance: narcissists undergo therapy to demonstrate change while maintaining core extraction mechanisms. Professional systems maintain the appearance of addressing the constraint (court-ordered therapy, workplace mediation) while failing to fundamentally alter the relational structure.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURALIZATION (MOUNTAIN?) — A civilizational/universal perspective might frame narcissistic supply-seeking as an immutable feature of human psychology: ego maintenance and mirror-seeking are inherent to consciousness and identity formation. However, the structural data (ε=0.68, suppression=0.75, theater=0.58) contradicts mountain classification. This represents a false summit: the confusion of individual psychological tendency (ego investment) with structural constraint (compelled supply provision and partner entrapment). The constraint is contingent on relational dynamics and institutional silence, not a law of nature.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narcissistic_ego_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narcissistic_ego_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narcissistic_ego_maintenance, TR),
    TR >= 0.70.

:- end_tests(narcissistic_ego_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The narcissist systematically extracts validation, attention, and identity-reflecting labor from the echo. Unlike legitimate relationship reciprocity, narcissistic supply provision is unilateral: the narcissist does not provide equivalent validation or emotional support. The extraction is not crude material theft but subtle psychological harvesting — the echo's emotional resources, cognitive bandwidth, and identity flexibility are consumed to maintain the narcissist's grandiose self-image. The initial value (0.35 at T=0) reflects early-relationship idealization where extraction is masked by intensity and novelty; it rises to 0.68 by T=10 as the echo's identity becomes increasingly fused with supply provision and the narcissist's entitlement escalates. Suppression (0.75): High. The echo faces substantial barriers to exit: (1) Identity fusion — self-concept is defined through the relationship and the echo's role as mirror/validator. Leaving feels like self-obliteration. (2) Emotional entanglement — trauma bonding from intermittent reinforcement (periods of intensity alternating with withdrawal) creates attachment despite relational harm. (3) Financial/custodial entanglement — shared assets, children, housing dependencies create material barriers. (4) Institutional silence — family, friends, and professional systems often normalize the dynamics or victim-blame the echo for 'not leaving.' (5) Threat environment — narcissists often escalate control when exit is attempted: threats of custody loss, financial ruin, public humiliation, or harm to self. Theater ratio (0.58): Moderate-high. Much narcissistic supply-seeking appears as ordinary relationship behavior: compliments, gift-giving, dates, attention. The performative content increases over time as the narcissist must maintain the facade for external audiences (family, colleagues, therapy) while intensifying extraction behind closed doors. By T=10, the ratio reaches 0.58, reflecting that public interactions are increasingly theatrical (showing up as charming, successful, devoted) while private dynamics become extractive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a maximum perspectival gap. The echo perceives pure extraction (Snare) — powerless, trapped, bearing all costs of supply provision. The narcissist perceives coordination (Rope) — solving the legitimate problem of maintaining self-image through efficient mirror allocation. The extended network perceives mixed extraction with coordination (Tangled Rope) — they both enable the facade (coordination function: managing social presentation) and experience pressure to provide validation and suppress truth-telling about narcissistic behavior (extraction function). Therapeutic systems perceive their own degradation (Piton) — attempting interventions that devolve into performative compliance (narcissist attends therapy, performs change, continues extraction). The analytical observer risks a false naturalization (false Mountain) — seeing ego maintenance as an inherent law of psychology rather than a contingent relational structure enabled by institutional silence and psychological conditioning. The perspectival gap is maximal because the same constraint appears as immutable law (mountain) vs. pure predation (snare) depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   The Echo: Victim + trapped → d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.77. Maximum extraction. The echo's exit options are severely constrained by identity fusion, emotional entanglement, financial barriers, and threat environment. The narcissist holds all structural power in the relationship. Narcissistic Subject: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.07. Net beneficiary. The narcissist can exit and find replacement mirrors; has high agency; experiences the relationship as solving the problem of obtaining validation. Dependent Family Members: Victim + constrained → d≈0.75, f(d)≈1.10, σ=0.8 → χ≈0.60. The constraint is expressed through psychological dynamics (intergenerational transmission of narcissistic patterns) and indirect extraction (children experience neglect when narcissist's needs dominate). Extended Network: Mixed + constrained → d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.58. The network is both participating in coordination (maintaining facade) and experiencing extraction (pressure to provide validation, suppress truth, manage narcissist's emotional regulation). Therapeutic Systems: Institutional + arbitrage → d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.06. Systems maintain arbitrage (professional positioning, billable hours, reputation) while the constraint persists. Theater_ratio explains piton classification despite low chi: the therapeutic encounter becomes performative (narcissist performs change, system documents progress) rather than functional.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the same relational structure is experienced as pure extraction (Snare), pure coordination (Rope), and mixed hybrid (Tangled Rope) from different structural positions. The analytical risk is naturalizing the constraint — framing ego maintenance and mirror-seeking as immutable psychological law, then claiming that 'relationships naturally involve some extraction' or 'narcissism is inherent to consciousness.' This naturalization is prevented by the structural data: if the constraint were truly immutable, it would appear as Mountain (ε≤0.25, suppression≤0.05, accessibility_collapse≥0.85, resistance≤0.15). Instead, ε=0.68 and suppression=0.75 indicate contingent institutional and relational mechanisms. The mandatrophy resolution: the constraint persists because (1) institutions fail to intervene (silence), (2) psychological conditioning makes exit perception impossible (trauma bonding, identity fusion), (3) cultural norms around ego investment and relationship reciprocity are under-theorized, and (4) the extractive mechanism is disguised as normal relationship behavior. The false summit (analytical mountain) occurs when observers naturalize the psychological dynamics rather than examining the relational structure that enables extraction. Addressing the constraint requires: (1) institutional intervention (mandatory screening, educational programs on healthy relationship dynamics, legal protections for exit), (2) psychological support for identity reconstruction post-exit, (3) cultural reframing of ego-maintenance from individual entitlement to reciprocal validation, and (4) recognition that the constraint is a contingent feature of institutions that permit silent psychological extraction, not an immutable feature of human consciousness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grandiosity_fragility_threshold,
    'At what point does the narcissist''s grandiose self-image become so dependent on supply that the constraint becomes mutually self-reinforcing rather than unilaterally extractive?',
    'Longitudinal psychological assessment of narcissist''s self-regulation capacity when supply is withdrawn; analysis of whether narcissist experiences perceived constraint or merely adaptation',
    'If threshold low: narcissist is equally trapped as echo, reframing from snare to mutually-reinforcing extraction. If threshold high: narcissist retains full exit optionality, confirming snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grandiosity_fragility_threshold, empirical, 'Threshold at which grandiose self-image becomes supply-dependent').

omega_variable(
    socialization_vs_pathology_boundary,
    'Is the constraint a consequence of clinical narcissistic personality pathology (DSM-5 criteria) or a manifestation of culturally-normalized ego-maintenance behavior present across the population spectrum?',
    'Comparison of supply-seeking and mirror-requiring behavior across clinical narcissists vs non-clinical high-narcissism vs general population; analysis of cultural context and gender normativity',
    'If pathology-driven: constraint is localized to clinical population, remediation is individualized treatment. If normalized: constraint is distributed across relational systems, remediation requires cultural/institutional reframing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(socialization_vs_pathology_boundary, conceptual, 'Boundary between pathological and normalized narcissistic extraction').

omega_variable(
    exit_perception_vs_material_barrier,
    'To what degree is the echo''s perceived inability to exit (psychological trap) vs. actual material barrier (financial, custodial, social)?',
    'Ethnographic analysis of exit attempts; comparison of echo''s stated constraints vs. objective structural barriers (economic support systems, legal aid, housing alternatives); longitudinal follow-up of separations',
    'If perception-dominant: constraint depends on psychological conditioning and belief systems; reframing/support can enable exit. If material-dominant: exit requires structural intervention (housing, financial resources, legal support).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_perception_vs_material_barrier, empirical, 'Psychological vs. material basis for exit barriers').

omega_variable(
    institutional_silence_mechanism,
    'Does the constraint persist primarily through explicit coercion/threat or through institutional failure to recognize/intervene in narcissistic dynamics?',
    'Analysis of threat prevalence in narcissistic relationships; comparison of constraints where institutional intervention occurs vs. where it does not; longitudinal study of family/workplace institutional responses',
    'If coercion-driven: suppression≥0.75 is correct; system is predatory. If silence-driven: suppression should be lower (0.40-0.50), and the constraint is better classified as degraded coordination (piton) than pure extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_silence_mechanism, empirical, 'Role of institutional silence vs. explicit coercion in constraint maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narcissistic_ego_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narc_tr_t0, narcissistic_ego_maintenance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(narc_tr_t5, narcissistic_ego_maintenance, theater_ratio, 5, 0.48).
narrative_ontology:measurement(narc_tr_t10, narcissistic_ego_maintenance, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(narc_be_t0, narcissistic_ego_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(narc_be_t5, narcissistic_ego_maintenance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(narc_be_t10, narcissistic_ego_maintenance, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narcissistic_ego_maintenance, information_standard).
narrative_ontology:affects_constraint(narcissistic_ego_maintenance, intergenerational_trauma_transmission).
narrative_ontology:affects_constraint(narcissistic_ego_maintenance, relational_attachment_codependency).

% DUAL FORMULATION NOTE:
% The narcissistic ego maintenance constraint decomposes into two related structures: (1) the dyadic extraction mechanism (narcissist-echo) described in this story, with ε=0.68 reflecting unilateral supply provision, and (2) the intergenerational transmission mechanism (echo-children), where children internalize narcissistic relational patterns, with ε potentially lower (~0.42) because children have some developmental plasticity and institutional intervention points (school, peer relationships). These are structurally distinct constraints linked by family systems dynamics. The dyadic constraint is a Snare; the intergenerational transmission may be Tangled Rope (coordination function of family structure + extraction via modeling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
