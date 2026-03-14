% ============================================================================
% CONSTRAINT STORY: trauma_bonding_attachment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trauma_bonding_attachment, []).

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
 *   constraint_id: trauma_bonding_attachment
 *   human_readable: Trauma Bonding Attachment in Intimate Relationships
 *   domain: interpersonal/psychological
 *
 * SUMMARY:
 *   Trauma bonding describes the psychological attachment that develops
 *   between a victim and perpetrator in relationships involving cycles of
 *   abuse and reconciliation. The constraint exhibits the six DR types from
 *   different structural positions, revealing how interpersonal extraction
 *   mechanisms hide within attachment narratives. The same relationship
 *   dynamic — alternating violence and affection creating intermittent
 *   reinforcement — appears as pure extraction (snare) from the victim's
 *   perspective, mixed coordination-extraction (tangled rope) from the
 *   perpetrator's perspective, performative institutional response (piton)
 *   from the social-legal system, genuine protective coordination (rope) from
 *   DV advocacy, and falsely natural (mountain) from the analytical observer
 *   who naturalizes relationship suffering. The measurements track the
 *   constraint's escalation over time: extractiveness increases as the
 *   pattern solidifies (0.45 → 0.72), then slightly decreases as the
 *   relationship reaches a homeostatic intensity (0.72 → 0.68). Theater ratio
 *   rises as the couple develops elaborate reconciliation narratives and
 *   justifications (0.30 → 0.58), then stabilizes at the point where both
 *   partners have constructed consistent internal explanations for the cycle.
 *   The constraint is perpetuated by intermittent reinforcement (periodic
 *   affection after violence), identity fusion (the victim's self-concept
 *   becomes inseparable from the relationship), and institutional failure
 *   (the social-legal system often constrains exit more than it enables it).
 *
 * KEY AGENTS:
 *   - Trauma-bonded partner: Primary victim (powerless/trapped and identity_locked) — bears full psychological and often physical cost; identity constituted through the relationship
 *   - Perpetrator: Primary beneficiary (powerful/arbitrage) — extracts emotional regulation, labor, control, sexual access; experiences constraint as coordination; maintains coercive power through cycles
 *   - Social-legal institutions: Secondary actor (institutional/constrained) — family law, social services, religious institutions; provide performative protection but often constrain exit (demanding reconciliation, child custody penalties)
 *   - Domestic violence advocates and shelters: Protective actors (moderate/constrained) — provide genuine coordination (safe housing, legal support, community) that can interrupt the trauma bond
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing relationship suffering as inevitable; false mountain classification masks institutional failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trauma_bonding_attachment, 0.68).
domain_priors:suppression_score(trauma_bonding_attachment, 0.75).
domain_priors:theater_ratio(trauma_bonding_attachment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trauma_bonding_attachment, extractiveness, 0.68).
narrative_ontology:constraint_metric(trauma_bonding_attachment, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trauma_bonding_attachment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trauma_bonding_attachment, snare).
narrative_ontology:human_readable(trauma_bonding_attachment, "Trauma Bonding Attachment in Intimate Relationships").
narrative_ontology:topic_domain(trauma_bonding_attachment, "interpersonal/psychological").

domain_priors:requires_active_enforcement(trauma_bonding_attachment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(trauma_bonding_attachment, trauma_bonded_partner).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAUMA-BONDED PARTNER (SNARE) — Structurally trapped by internalized fear, shame, and fused identity. Despite having legal rights, income, housing options, and no physical confinement, the partner cannot exit because their self-concept is constituted through the relationship. Maximum suppression (0.75): alternating cycles of abuse and reconciliation create intermittent reinforcement that sustains the lock. High extractiveness (0.68) with no perceived alternatives. The trap is psychological rather than material, but equally absolute.
constraint_indexing:constraint_classification(trauma_bonding_attachment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRAUMA-BONDED PARTNER — IDENTITY_LOCKED (SNARE) — Same agent, same structural reality, but classified from the identity_locked exit axis. The partner's identity is fused with the relationship: they cannot imagine themselves as a separate person; their self-worth is tied to managing the abuser's emotions; exit would require abandoning the caregiving identity they've constructed. This perspective demonstrates the identity-lock mechanism as distinct from material constraint. The classification remains Snare, but the exit axis reveals the binding is cognitive rather than structural. This is the diagnostic signal: powerless/trapped → mountain, but powerless/identity_locked → rope at biographical time. The gap reveals cognitive capture.
constraint_indexing:constraint_classification(trauma_bonding_attachment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: PERPETRATOR (TANGLED ROPE) — From the perpetrator's immediate perspective, the constraint serves genuine coordination: emotional regulation through the partner's responsiveness, resource access, identity reinforcement through dominance. The constraint also enables extraction: control over autonomy, labor, sexuality, financial resources, social isolation. The perpetrator experiences this as coordination (the relationship 'works' for them) while maintaining coercive power. High effective extraction because the perpetrator has organized power and arbitrage options (can leave, can replace the partner). Active enforcement is required — cycles of tension and violence maintain the lock.
constraint_indexing:constraint_classification(trauma_bonding_attachment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SOCIAL-LEGAL SYSTEM (PITON) — Institutional actors (family law, social services, religious institutions, community norms) have formal mandates to protect and coordinate family safety. In practice, the system is largely performative: restraining orders without enforcement, mandatory counseling that perpetuates the myth of treatability, victim-blaming narratives that constrain exit, religious doctrine that reframes the relationship as redemptive. Theater ratio (0.55): significant performative activity (court orders, counseling referrals, shelter services) alongside low functional protection. Extraction: the system often constrains the victim more than it protects them (demanding reconciliation, requiring victim testimony, imposing child custody penalties for leaving). Piton classification reflects degraded institutional function maintained through inertia: the system persists as family institution despite low efficacy.
constraint_indexing:constraint_classification(trauma_bonding_attachment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PROTECTIVE INSTITUTIONS (ROPE) — Domestic violence shelters, victim advocacy organizations, and community support networks provide genuine coordination: safe housing, legal navigation, social validation, exit pathways. These agents benefit from the partner's exit (survivor story, political leverage for funding, expanded mandate) but provide real coordination service. Low extractiveness: the relationship is reciprocal, not parasitic. Suppression is reduced when protective institutions engage (they provide knowledge, alternatives, and community that counter the trauma bond's isolating effects). This perspective demonstrates that genuine coordination can coexist with the snare/tangled rope exploitation, provided external agents actively intervene.
constraint_indexing:constraint_classification(trauma_bonding_attachment, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — The analytical context risks naturalizing trauma bonding as an inevitable feature of intimate relationships ('love is blind,' 'attachment involves suffering,' 'conflict is inherent'). This perspective mistakes the constraint's psychological mechanisms for natural law. However, the structural data contradicts the mountain classification: high extractiveness (0.68), high suppression (0.75), high effective enforcement, asymmetric victim declaration all mark this as a Snare, not a law of nature. The false summit reveals how psychological naturalization prevents recognizing institutional failure to protect.
constraint_indexing:constraint_classification(trauma_bonding_attachment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trauma_bonding_attachment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trauma_bonding_attachment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trauma_bonding_attachment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trauma_bonding_attachment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trauma_bonding_attachment, TR),
    TR >= 0.70.

:- end_tests(trauma_bonding_attachment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The perpetrator extracts emotional labor (partner manages perpetrator's emotions), physical labor (partner accommodates perpetrator's needs/schedule), sexual access, financial resources, and social isolation (partner loses autonomy, contact with others). The victim receives intermittent affection and crisis resolution as 'rewards,' but these are withholding-followed-by-restoration cycles, not genuine reciprocity. The trajectory (0.45 → 0.72 → 0.68) shows extraction escalating as the pattern solidifies, then plateauing at a homeostatic intensity. Suppression (0.75): Very high. The victim faces psychological barriers (fear, shame, identity fusion, cognitive distortion), social barriers (isolation, loss of support networks, fear of disbelief), material barriers (economic dependence, child custody concerns, housing insecurity), and institutional barriers (police response failures, court bias, mandates for reconciliation). The intermittent reinforcement schedule (periodic affection after violence) creates addiction-like neurochemistry that sustains the lock even when barriers are theoretically surmountable. Theater ratio (0.55): Moderate. The relationship involves performative activity — reconciliation scenes, promises to change, couples narratives of 'working through' conflict — but these are not as high-ratio as purely institutional pitons. The theater increases early (as both partners construct explanations for the pattern) but stabilizes as the cycle becomes routinized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a fundamental divergence in how the constraint is experienced and classified. The victim (powerless/trapped) sees a Snare: pure extraction with no escape. The victim from the identity_locked perspective still sees a Snare, but the mechanism is revealed as cognitive lock rather than material constraint — this gap is diagnostic of the identity-fusion binding. The perpetrator (powerful/arbitrage) experiences Tangled Rope: genuine coordination (the partner's emotional responsiveness regulates the perpetrator's dysregulation) coexists with extraction (control, labor, sexual access). The social-legal system (institutional/constrained) enacts Piton logic: performative protection rituals that often constrain the victim more than they help. Protective organizations (moderate/constrained) see Rope: pure coordination without extraction. The analytical observer (analytical/analytical) risks seeing Mountain: the false naturalization that 'relationships involve conflict' and 'love requires sacrifice.' The perspectival gap between the victim's Snare and the perpetrator's Tangled Rope is the most diagnostic: it reveals that the perpetrator's genuine coordination needs may be real, but they are being met through extraction rather than reciprocal negotiation. The perpetrator could meet these needs through therapy, peer support, or consensual relationship structures, but instead uses coercion. The gap exposes mandatrophy: is this a mixed relationship that needs to be repaired, or a predatory extraction that needs to be ended?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. The trauma-bonded partner has d ≈ 0.95 (full victim + trapped exit) or d ≈ 0.89 (full victim + identity_locked exit), producing high f(d) ≈ 1.42 or f(d) ≈ 1.28, amplifying experienced extractiveness. The perpetrator has d ≈ 0.05 (full beneficiary + arbitrage exit), producing low f(d) ≈ -0.12, resulting in negative effective extraction from the perpetrator's perspective (they experience net benefit). The institutional system has d ≈ 0.50 (mixed victim/beneficiary + constrained exit), producing f(d) ≈ 0.65, generating moderate extraction experienced by the system itself (institutional burden). The protective advocates have d ≈ 0.15 (minor beneficiary + constrained exit), producing f(d) ≈ -0.01 (near-zero scaling), preserving the genuine coordination signal. These derived d values explain why the perspectives diverge so radically: the same constraint produces opposite directionality for the victim and perpetrator.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED THROUGH IDENTITY-LOCK DIAGNOSIS. The trauma bond constraint resolves its mandatrophy through the identity_locked exit option, which reveals the binding mechanism as cognitive rather than material. At biographical time, identity_locked produces Rope classification (the agent perceives the constraint as changeable in principle), while trapped produces Mountain (unchangeable). This gap is the diagnostic signal: the victim's constraint is not immutable physics but internalized psychological lock. The perpetrator's Tangled Rope perspective (with genuine coordination function + extraction) is accurate if the perpetrator's dysregulation is genuine and the partner's emotional labor actually regulates it. However, this genuine coordination does not justify the coercive control mechanism — alternative coordination pathways exist (therapy, peer support, consensual negotiation). The perpetrator's selection of coercive control reveals that the coordination function is incidental to predatory extraction: the perpetrator could meet dysregulation needs through other means but chooses control because it is more efficient and more rewarding. The mandatrophy resolves by distinguishing: (1) the genuine coordination present in the tangled rope (real, worth recognizing) from (2) the predatory selection of coercive mechanisms to meet that coordination need (the actual constraint structure). A trauma bond can be broken by removing suppression (protective interventions, identity reconstruction, material support) even if the coordination function is genuine — because alternative coordination pathways exist. The false natural law mountain perspective is rejected: trauma bonding is not inherent to intimate relationships; it requires specific institutional conditions (isolation, intermittent reinforcement, identity vulnerability, institutional failure to protect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.75) primarily structural (material barriers to exit) or internalized (cognitive patterns that persist even after barriers are removed)?',
    'Post-exit suppression trajectory: track survivor''s decision-making, confidence, and isolation patterns for 12-24 months after relationship termination. If suppression persists after material barriers are removed, reclassify as partially internalized.',
    'If suppression is primarily internalized: the constraint''s effective suppression is even higher than the measured 0.75 — the partner carries the psychological lock after physical exit. Treatment and recovery time becomes much longer (years, not months). If structural: focused removal of barriers (housing, legal protection, financial independence) can achieve exit even if identity is fused.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Proportion of suppression that is internalized vs structural').

omega_variable(
    identity_lock_reversibility,
    'Is the identity fusion (identity_locked exit classification) reversible through therapeutic intervention, or does it represent a permanent identity transformation?',
    'Longitudinal study of survivors in therapeutic vs non-therapeutic recovery: measurement of self-concept stability, relationship autonomy, and capacity to envision self outside the constraint. Comparison with identity dissolution in other contexts (cult recovery, ideological deprogramming).',
    'If reversible: protective interventions (therapy, community, identity reconstruction work) can restore the partner''s capacity to exit. If irreversible: identity-locked partners require deep identity work and may remain at risk for re-bonding. Classification may need to shift to trapped if recovery is sufficiently difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity fusion is reversible through intervention').

omega_variable(
    intermittent_reinforcement_sufficiency,
    'Is intermittent reinforcement alone sufficient to sustain the trauma bond, or does it require ongoing acute trauma (violence, threat) to maintain suppression at 0.75?',
    'Measurement of trauma bond persistence in relationships with historical violence but long periods of calm (no recent acute incidents). Comparison of exit rates/duration in high-acute-incident relationships vs low-incident relationships with periodic tension cycles.',
    'If intermittent reinforcement is sufficient: the constraint persists even in relationships that have ''improved'' or become less violent. If acute trauma is necessary: suppression should decline as violence frequency decreases, opening exit windows. Measurement sensitivity to phase of cycle becomes critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittent_reinforcement_sufficiency, empirical, 'Whether intermittent reinforcement sustains the bond independently of acute trauma').

omega_variable(
    perpetrator_coordination_vs_predation,
    'Does the perpetrator genuinely experience the relationship as coordination (emotional regulation, identity affirmation), or does the ''coordination'' description mask predatory selection for vulnerable partners?',
    'Perpetrator psychological assessment: measurement of empathy deficits, narcissism, and intentionality of partner selection. Comparison of perpetrator''s behavior with previous partners (pattern of coercion vs genuine relationship variability). Whether perpetrator shows capacity for reciprocal relationships with non-vulnerable partners.',
    'If genuine coordination perception: perpetrator may be treatable through relationship skills. If predatory: the perpetrator selected the partner specifically for vulnerability to trauma bonding. Treatment focus shifts from relationship skills to accountability and victim protection. Classification may remain Tangled Rope (genuine coordination + extraction) or shift to pure Snare if predatory intent is demonstrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetrator_coordination_vs_predation, empirical, 'Whether perpetrator experiences genuine coordination or masks predatory selection').

omega_variable(
    cycle_phase_measurement_dependence,
    'Does the measured extractiveness (0.68) and suppression (0.75) vary significantly with the abuse cycle phase (tension building, acute incident, reconciliation, calm)?',
    'Continuous measurement of partner''s perceived extraction and suppression across multiple complete cycles. Identification of phase-specific peaks (suppression highest post-incident? Extraction perceived as lowest during calm?). Measurement of whether exit attempts are phase-dependent.',
    'If highly phase-dependent: base properties represent a snapshot and may not capture the constraint''s actual range. Theater ratio may also vary by phase (higher during reconciliation myth, lower during acute incidents). Measurement collection protocol must specify cycle phase for reproducibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cycle_phase_measurement_dependence, empirical, 'Dependence of extractiveness and suppression on abuse cycle phase').

omega_variable(
    mandatrophy_benevolence_cover,
    'Does the perpetrator''s genuine coordination function (emotional regulation, identity reinforcement) serve as a cover story that prevents recognizing the constraint as pure extraction (Snare) rather than mixed coordination-extraction (Tangled Rope)?',
    'Counterfactual analysis: if the perpetrator''s coordination needs were met by alternative sources (therapy, peer support, secure attachment), would the coercive control disappear? Or would the perpetrator find new extraction mechanisms? Comparison with perpetrators who lose control over partners through separation: do they develop new control strategies, or cease coercive behavior?',
    'If the coordination is genuine AND necessary: the Tangled Rope classification is correct, and relationship reconstruction (with professional support) may be possible. If the coordination is incidental to predatory control: the Snare classification is more accurate, and perpetrator selectivity for vulnerable partners is the primary mechanism. Mandatrophy resolution depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_benevolence_cover, conceptual, 'Whether perpetrator''s genuine coordination needs justify the Tangled Rope classification or mask a Snare mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trauma_bonding_attachment, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trauma_tr_t0, trauma_bonding_attachment, theater_ratio, 0, 0.3).
narrative_ontology:measurement(trauma_tr_t2, trauma_bonding_attachment, theater_ratio, 2, 0.42).
narrative_ontology:measurement(trauma_tr_t4, trauma_bonding_attachment, theater_ratio, 4, 0.55).
narrative_ontology:measurement(trauma_tr_t6, trauma_bonding_attachment, theater_ratio, 6, 0.58).
narrative_ontology:measurement(trauma_tr_t8, trauma_bonding_attachment, theater_ratio, 8, 0.55).

% Extraction over time
narrative_ontology:measurement(trauma_be_t0, trauma_bonding_attachment, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trauma_be_t2, trauma_bonding_attachment, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(trauma_be_t4, trauma_bonding_attachment, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(trauma_be_t6, trauma_bonding_attachment, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(trauma_be_t8, trauma_bonding_attachment, base_extractiveness, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trauma_bonding_attachment, attachment_coordination).
narrative_ontology:affects_constraint(trauma_bonding_attachment, domestic_violence_cycle_escalation).
narrative_ontology:affects_constraint(trauma_bonding_attachment, trauma_recovery_capacity).
narrative_ontology:affects_constraint(trauma_bonding_attachment, institutional_protection_failure).

% DUAL FORMULATION NOTE:
% Trauma bonding comprises multiple structurally distinct constraints: the attachment mechanism itself (attachment_coordination), the perpetrator's dysregulation dynamics (behavioral/psychological), the victim's identity fusion (cognitive), and the institutional response failure (system-level). This story focuses on the integrated psychological mechanism. Related stories address the perpetrator's behavioral pathology (separate ε) and institutional failure (separate ε). The network links show how trauma bonding's persistence depends on institutional failure and how recovery depends on breaking both psychological lock AND removing suppression barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
