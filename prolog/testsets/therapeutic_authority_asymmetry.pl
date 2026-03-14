% ============================================================================
% CONSTRAINT STORY: therapeutic_authority_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_therapeutic_authority_asymmetry, []).

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
 *   constraint_id: therapeutic_authority_asymmetry
 *   human_readable: Therapeutic Authority Asymmetry in Clinical Relationships
 *   domain: healthcare/psychology/interpersonal
 *
 * SUMMARY:
 *   Therapeutic authority asymmetry describes the structural power
 *   differential inherent in clinical relationships where one party
 *   (therapist) holds specialized knowledge, professional authority,
 *   institutional backing, and decision-making power over treatment, while
 *   the other party (client) enters in a state of vulnerability, distress,
 *   and limited information. This constraint exhibits the full spectrum of DR
 *   classification across a single interpersonal dyad, making it
 *   diagnostically rich for understanding how identity-based binding
 *   mechanisms operate. The asymmetry serves a genuine coordination function
 *   — establishing a safe, bounded container for healing work — while
 *   simultaneously creating extraction mechanisms through information
 *   asymmetry, economic dependency, and emotional vulnerability. The theater
 *   ratio (0.55, climbing to 0.72 by measurement point 6 in some therapeutic
 *   modalities) reflects the performative elements of therapeutic process:
 *   diagnostic assessment rituals that appear scientific but have low
 *   predictive validity; treatment plans that serve administrative and
 *   billing purposes more than directional guidance; therapeutic techniques
 *   presented as evidence-based despite weak empirical support. The identity
 *   lock mechanism — where the client's self-concept becomes constituted
 *   through the patient role and relationship to the therapist — is the key
 *   structural distinction from other professional asymmetries. Unlike a
 *   financial advisor or attorney, where exit is primarily constrained by
 *   cost or switching friction, the therapeutic client's exit is constrained
 *   by identity fusion: leaving therapy feels like abandoning recovery
 *   itself.
 *
 * KEY AGENTS:
 *   - Vulnerable Client in Acute Distress: Primary victim (powerless/identity_locked) — perceives therapist as sole pathway to healing; identity fused with patient role; maximum suppression internalized
 *   - Moderately Resourced Client: Secondary participant (moderate/constrained) — has alternative supports and financial means; experiences both coordination and extraction; able to compare alternatives
 *   - Credentialed Therapist: Primary beneficiary (institutional/arbitrage) — experiences asymmetry as necessary coordination structure; multiple arbitrage options (client base, fee control, practice autonomy); benefits through professional authority and income
 *   - Clinical Institution: Secondary beneficiary (institutional/constrained) — coordinates care delivery and maintains standards (coordination function) while capturing surplus through employment, credentialing, and liability asymmetry; constrained by regulation and reputation
 *   - Professional Ethics Framework: Degraded coordination mechanism (organized/constrained) — licensing boards and codes created to prevent abuse but operate largely post-hoc; enforces theater rather than prevention; high suppression on reporting pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent institutional arrangement (degree of asymmetry, confidentiality barriers, identity fusion mechanisms) as inherent features of care work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(therapeutic_authority_asymmetry, 0.58).
domain_priors:suppression_score(therapeutic_authority_asymmetry, 0.68).
domain_priors:theater_ratio(therapeutic_authority_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(therapeutic_authority_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(therapeutic_authority_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(therapeutic_authority_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(therapeutic_authority_asymmetry, tangled_rope).
narrative_ontology:human_readable(therapeutic_authority_asymmetry, "Therapeutic Authority Asymmetry in Clinical Relationships").
narrative_ontology:topic_domain(therapeutic_authority_asymmetry, "healthcare/psychology/interpersonal").

domain_priors:requires_active_enforcement(therapeutic_authority_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(therapeutic_authority_asymmetry, credentialed_therapists).
narrative_ontology:constraint_beneficiary(therapeutic_authority_asymmetry, clinical_institutions).
narrative_ontology:constraint_victim(therapeutic_authority_asymmetry, clients_in_distress).
narrative_ontology:constraint_victim(therapeutic_authority_asymmetry, therapeutic_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE CLIENT (SNARE) — Client in acute distress perceives therapist as sole pathway to healing. Identity is fused with 'patient' role; recovery requires the relationship that constitutes the constraint. Structural mobility exists (can find another therapist) but identity lock prevents exercise of it — exiting feels like abandoning recovery itself. Maximum experienced extraction: asymmetric information, power differential, emotional dependency, and identity fusion create suppression that client internalizes as personal inadequacy.
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERATELY RESOURCED CLIENT (TANGLED ROPE) — Client with financial means, alternative supports, and some skepticism toward therapeutic authority. Benefits from genuine coordination: therapist provides skill and attention. Costs exist: fee structure, professional boundaries limiting intimacy, asymmetric information about effectiveness. Exit is possible (find new therapist, discontinue) but carries costs (therapy disruption, financial loss, relational rupture). Experiences both coordination and extraction; has sufficient agency to compare alternatives.
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALED THERAPIST (ROPE) — Therapist experiences the constraint as pure coordination: establishing authority and boundary structures is necessary to create the safe, structured container where healing work occurs. Therapeutic frame (fee, schedule, confidentiality, asymmetry) enables genuine coordination of care. Therapist has multiple clients and can end relationships; arbitrage exit (switch to different practice, raise fees, establish reputation). Benefits from the authority asymmetry through professional autonomy, fee income, and intellectual control. Suppression and extraction are experienced as necessary structural features, not as exploitative.
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL INSTITUTION (TANGLED ROPE) — Hospital, clinic, or licensing board coordinates therapeutic care delivery and maintains professional standards (genuine coordination function). Also captures surplus value through employment structures, credential gatekeeping, and malpractice liability asymmetry that concentrates risk downward. Constrained by regulatory frameworks and reputation dependence. Benefits from institutional authority but faces institutional risk — class action lawsuits, licensing board investigations, reputation damage from abuse cases make extraction unstable long-term.
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROFESSIONAL ETHICS FRAMEWORK (PITON) — Licensing boards, ethics codes, and supervision structures were created to prevent abuse of therapeutic authority (genuine coordination intent). But the theater ratio is high: ethics enforcement is mostly post-hoc investigation after harm occurs; confidentiality protections prevent external verification of therapeutic process; 'clinical judgment' creates a protected sphere where authority operates with minimal oversight. The ethics framework persists through institutional inertia — it maintains theatrical legitimacy while the actual prevention mechanism (peer review, supervisory oversight) remains sparse and reactive. Exit exists (clients can report to licensing boards) but suppression is high (reporting risks relationship rupture, retaliation through diagnosis, loss of access to care).
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some power asymmetry is inherent to therapeutic work: the client must be in a state of vulnerability to seek help; the therapist must hold authority to create psychological safety; knowledge asymmetry about mental process is irreducible; the asymmetry is necessary for the coordination to function. The constraint appears as an inherent feature of care relationships themselves — immutable across cultures and time. However, this naturalizes what is empirically contingent: the degree of asymmetry, the absence of peer review, the confidentiality barriers to accountability, and the identity fusion mechanisms are institutional choices, not laws of psychology. The mountain classification is a false summit.
constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(therapeutic_authority_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(therapeutic_authority_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(therapeutic_authority_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(therapeutic_authority_asymmetry, TR),
    TR >= 0.70.

:- end_tests(therapeutic_authority_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The therapeutic relationship combines genuine coordination (safety container, skill transfer, emotional attunement) with systematic extraction (asymmetric information, economic dependency, identity fusion, professional authority). The 0.58 value reflects that extraction is real but not total — many clients experience net benefit even accounting for the asymmetric power structure. However, the value is skewed upward by the identity-locked agent's experience (who perceives maximum extraction at 1.0) and institutional suppression mechanisms. Suppression (0.68): Moderate-high. Multiple sources: (1) Structural barriers — expensive therapy, limited provider access, insurance/credential gatekeeping; (2) Internalized barriers — identity lock, epistemic deference to therapist, learned helplessness from acute distress state, self-blame for lack of progress; (3) Institutional barriers — confidentiality preventing external review, licensing board reporting mechanisms that are slow and threatening, professional retaliation through diagnosis. The 0.68 reflects a mix where internalized mechanisms account for roughly 40% of the measured suppression, making this a candidate for constraint decomposition (structural suppression story + internalized suppression story). Theater ratio (0.55, increasing to 0.58 by end of interval): Moderate. Therapeutic process involves significant performative elements: diagnostic assessment using categorical systems (DSM-5) with low predictive validity for treatment; treatment plans serving billing and administrative requirements more than directional guidance; therapist attunement performances that create safety through presence and validation rather than through specific evidence-based techniques; session structure (50-minute hour, weekly schedule) driven by billing and administrative convenience rather than therapeutic optimization. Open-source therapeutic approaches (peer support, online communities, self-help literature) have theater ratios around 0.25-0.35, suggesting that the measured theater in credentialed therapy reflects institutional overhead, not therapeutic necessity. As professionalization has increased (credentials, licensing, malpractice insurance), theater ratio has risen, indicating therapeutic theater serves institutional legitimation more than therapeutic efficacy.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable client and credentialed therapist occupy inverted perceptual worlds relative to the same constraint. The client perceives maximum extraction (snare) — the therapist's authority is insurmountable, the exit impossible, the suppression pervasive. The therapist perceives pure coordination (rope) — the authority is structurally necessary, the client benefits, the framework enables healing. Both perceptions are structurally accurate from their positions; they do not contradict each other, they reveal the constraint's true structure through their incompatibility. The moderately resourced client perceives tangled rope — genuine benefits (skill, attention, emotional safety) alongside real costs (fees, time, asymmetry). The institutional clinical setting also perceives tangled rope — coordination of care delivery alongside institutional extraction through employment and credentialing. The ethics framework perceives piton — the machinery exists and is performatively legitimate but has lost its actual prevention function through confidentiality barriers and liability asymmetry. The civilizational analytical perspective risks mountain — naturalizing the asymmetry as inherent to care work rather than recognizing its institutional contingency. The perspectival gaps reveal the constraint's true mechanism: it is tangled rope at the institutional level (coordination + extraction coexist), snare at the vulnerable client level (identity lock makes escape impossible), rope at the beneficiary level (authority creates genuine coordination), and piton at the regulatory level (ethics machinery is theatrical).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural position relative to the constraint. The vulnerable client entering acute distress enters with d ≈ 0.95 (full target): maximum dependency, no alternatives perceived, identity-locked exit option. Their d remains high throughout the relationship because identity lock prevents them from exercising the mobile or constrained exit options that structural analysis would suggest. The moderately resourced client with financial means and skepticism enters with d ≈ 0.55 (symmetric): they benefit from genuine coordination (therapist skill) and bear costs (fees, time, vulnerability). The credentialed therapist operates at d ≈ 0.05 (full beneficiary): they control the process, set fees, define success, can exit without cost, benefit through authority and income. The institutional clinical setting operates at d ≈ 0.15-0.25 (partial beneficiary): captures surplus through employment and credentialing but faces institutional risk and regulatory constraints. The ethics framework operates at d ≈ 0.40 (partial target): created to serve clients but largely powerless against institutional momentum and confidentiality barriers; constrained by practitioners' ability to defend against investigation through clinical judgment opacity. The analytical observer at civilizational scope faces d ≈ 0.72 (moderate-high target) because the naturalizing frame (inherent asymmetry) obscures contingent institutional arrangements that could be otherwise organized.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by decomposing across perspectives rather than seeking a single 'true' type. The vulnerable client genuinely experiences snare (identity-locked, no exit, maximum extraction perceived). The therapist genuinely experiences rope (asymmetry is coordination, benefits flow to them). Both are true simultaneously; they describe the same structural object from positions with inverted directionality values. The institutional clinical setting experiences tangled rope because it simultaneously coordinates care and extracts institutional surplus. The ethics framework experiences piton because it maintains theatrical legitimacy while actual prevention function has atrophied. The analytical observer risks false mountain by naturalizing contingency. The mandatrophy is resolved not by selecting one type but by recognizing that the constraint IS a presheaf of perspectives with different d values and different classifications. The claimed_type (tangled_rope) at the institutional level is correct: genuine coordination (clinical care) + asymmetric extraction (authority, fees, information control) + active enforcement (licensing, billing, documentation requirements) satisfies the tangled rope gate. The snare classification at the powerless perspective is also correct: identity lock + trapped/identity_locked exit + maximum experienced extraction. Both are real; neither is 'the' classification. The system's robustness lies in showing how the same structural data produces different types from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_necessity_vs_exploitation,
    'What proportion of observed authority asymmetry is necessary for therapeutic efficacy versus exploitative overhead?',
    'Comparative outcome analysis: therapeutic approaches with different asymmetry levels (therapist-directed vs collaborative vs peer support) and their effectiveness rates; meta-analysis of outcomes controlling for asymmetry variables',
    'If therapeutic necessity > 60%: the snare classification is overstated, genuine rope dominates. If exploitative overhead > 40%: the tangled_rope classification is accurate, institutional extraction is real and separable from care. If the ratio varies by client population: decompose into separate stories per therapeutic modality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_necessity_vs_exploitation, empirical, 'Partition of asymmetry between therapeutic necessity and exploitative overhead').

omega_variable(
    identity_lock_persistence_post_therapy,
    'After successful therapy completion and termination, does the client''s identity-locked relationship to therapeutic authority persist, transfer to new therapists, or resolve?',
    'Longitudinal tracking of post-termination client behavior: autonomy in decision-making, likelihood of re-engagement with therapy for new issues, transference patterns with new practitioners, internalization of therapist''s perspective as client''s own',
    'If identity lock persists post-therapy: suppression is partly internalized and carried forward; the constraint has generational reach. If it resolves: identity lock is contextual to active therapeutic relationship; suppression is situational rather than internalized. If it transfers to new therapists: identity lock is a learned pattern, suggesting the constraint operates at the interpersonal skill level, not at the inherent power asymmetry level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_therapy, empirical, 'Persistence of identity lock after therapy completion').

omega_variable(
    confidentiality_barrier_mechanism,
    'Does strict therapist-patient confidentiality primarily protect client privacy (genuine coordination) or prevent institutional accountability (extraction cover)?',
    'Analysis of reporting mechanisms: when clients do report harm, what proportion cite confidentiality as a barrier to external review? Comparison of ethics outcomes in jurisdictions with stronger peer-review requirements vs strong confidentiality protections; analysis of harm disclosure patterns in therapies with different confidentiality structures (group therapy, peer consultation models)',
    'If confidentiality protects privacy primarily: it is a genuine coordination feature. If it prevents accountability: it is extraction infrastructure. If both: partition the mechanism and separate stories per context (e.g., individual therapy with weak peer review vs group therapy with stronger mutual accountability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidentiality_barrier_mechanism, empirical, 'Whether confidentiality serves privacy protection or prevents accountability').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.68) primarily structural barriers (economic dependency, geographic isolation, lack of alternatives) or internalized mechanisms (identity fusion, learned helplessness, epistemic deference)?',
    'Analysis of post-relationship suppression: clients who exit therapy (voluntarily or involuntarily) and their subsequent decision-making autonomy; mapping of suppression sources — identify which clients are suppressed by material barriers vs by internalized patterns; measurement of client autonomy before, during, and after therapy',
    'If structural > 50%: suppression is largely external and removable. If internalized > 50%: suppression is self-reinforcing; the constraint carries higher post-exit residue; the identity-locked classification is accurate. If both: create separate measurement tracks and potentially separate constraint stories per suppression type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized components of suppression').

omega_variable(
    institutional_extraction_mechanisms,
    'What specific institutional revenue mechanisms extract value from therapeutic authority asymmetry (billing practices, unnecessary sessions, credential gatekeeping, client switching friction)?',
    'Financial analysis: session length trends, session frequency trends, correlation between client improvement metrics and session continuation; analysis of credential gatekeeping (insurance barriers, licensure requirements limiting supply); measurement of switching costs (therapist search effort, treatment disruption, re-establishment time)',
    'If mechanisms are systematic: institutional extraction is separable from therapeutic coordination and should be a distinct story. If mechanisms are sporadic: individual therapist behavior rather than structural constraint. If both: create distinct stories per mechanism and link with network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_mechanisms, empirical, 'Institutional revenue extraction mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(therapeutic_authority_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thera_tr_t0, therapeutic_authority_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(thera_tr_t3, therapeutic_authority_asymmetry, theater_ratio, 3, 0.48).
narrative_ontology:measurement(thera_tr_t6, therapeutic_authority_asymmetry, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(thera_be_t0, therapeutic_authority_asymmetry, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(thera_be_t3, therapeutic_authority_asymmetry, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(thera_be_t6, therapeutic_authority_asymmetry, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(therapeutic_authority_asymmetry, attachment_coordination).
narrative_ontology:boltzmann_floor_override(therapeutic_authority_asymmetry, 0.12).
narrative_ontology:affects_constraint(therapeutic_authority_asymmetry, therapeutic_boundary_violation).
narrative_ontology:affects_constraint(therapeutic_authority_asymmetry, mental_health_dependency_trap).
narrative_ontology:affects_constraint(therapeutic_authority_asymmetry, credentialism_access_restriction).

% DUAL FORMULATION NOTE:
% Therapeutic authority asymmetry is a higher-order constraint coordinating multiple structural mechanisms. Decomposition creates separate stories for: (1) therapeutic_necessity_vs_extraction (ε ≈ 0.30, rope at core; tangled_rope at institutional level), (2) identity_lock_in_therapeutic_relationships (ε ≈ 0.72, snare; dependency-based binding), (3) institutional_extraction_mechanisms (ε ≈ 0.55, tangled rope; fees, credentialing, surveillance). The parent story represents the integrated view; downstream stories decompose per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(therapeutic_authority_asymmetry, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
