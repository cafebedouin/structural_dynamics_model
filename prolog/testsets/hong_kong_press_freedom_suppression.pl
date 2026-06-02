% ============================================================================
% CONSTRAINT STORY: hong_kong_press_freedom_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hong_kong_press_freedom_suppression, []).

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
 *   constraint_id: hong_kong_press_freedom_suppression
 *   human_readable: Hong Kong Press Freedom Suppression via National Security Law
 *   domain: political/media_freedom
 *
 * SUMMARY:
 *   Hong Kong's press freedom suppression underwent a structural
 *   transformation following the 2020 National Security Law implementation,
 *   shifting from regulatory capture within a quasi-democratic framework to
 *   direct legal suppression. Prior to 2020, press freedom was constrained
 *   through economic incentives (advertiser pressure, Beijing-aligned capital
 *   ownership), self-censorship norms, and indirect legal threats (sedition
 *   laws). The post-2020 constraint is structurally distinct: the National
 *   Security Law creates explicit criminal liability for journalism deemed
 *   subversive (articles 29–31 targeting secession, subversion, terrorism,
 *   foreign collusion). The constraint extracts narrative control from
 *   journalists and independent media with zero offsetting coordination
 *   benefit — journalists gain no access to information, no institutional
 *   protection, no resource subsidy in exchange for suppression. The
 *   extractiveness has risen from 0.38 (pre-NSL regulatory capture era) to
 *   0.68 (current legal suppression regime), while theater has risen from
 *   0.28 to 0.55 as enforcement shifts from active arrests to internalized
 *   self-censorship. The suppression requirement (enforcement burden) has
 *   risen from 0.55 to 0.78, indicating increasing legal machinery and police
 *   infrastructure devoted to suppression.
 *
 * KEY AGENTS:
 *   - Journalists in Hong Kong: Primary victims (powerless/trapped) — face criminal liability, legal investigation, visa restrictions, employer pressure. No exit options; reporting scope contracts under legal threat.
 *   - Independent Media Outlets (Apple Daily, Stand News, etc.): Primary victims (organized/constrained) — face raids, arrests of editors, advertiser exodus, distribution blockage. Can theoretically continue but under economic strangulation and legal threat.
 *   - Central Government Narrative Control: Primary beneficiary (institutional/arbitrage) — captures sole authority over information environment, controls counter-narrative suppression, benefits from unified official story.
 *   - International Press Organizations (Reuters, BBC, AP, etc.): Secondary victims (moderate/constrained) — can exit but at cost of market access and reporting scope. Face visa denial, accreditation refusal, legal liability.
 *   - Self-Censorship Culture / Editorial Practices: Institutional mechanism (institutional/mobile at first, becoming piton over time) — initially an external pressure, increasingly internalized as editorial norm.
 *   - Public Information Commons: Victim of constraint (powerless/trapped) — loses access to independent information, gains only state-approved narratives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hong_kong_press_freedom_suppression, 0.68).
domain_priors:suppression_score(hong_kong_press_freedom_suppression, 0.78).
domain_priors:theater_ratio(hong_kong_press_freedom_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hong_kong_press_freedom_suppression, snare).
narrative_ontology:human_readable(hong_kong_press_freedom_suppression, "Hong Kong Press Freedom Suppression via National Security Law").
narrative_ontology:topic_domain(hong_kong_press_freedom_suppression, "political/media_freedom").

domain_priors:requires_active_enforcement(hong_kong_press_freedom_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hong_kong_press_freedom_suppression, central_government_narrative_control).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, journalists).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, independent_media_outlets).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, pro_democracy_publications).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, public_information_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOURNALIST (SNARE) — Faces concrete legal jeopardy for publishing. Exit options are severely constrained: self-censorship (perceived as voluntary but enforced by legal threat), emigration (career termination), or continued journalism under legal risk. The constraint extracts editorial control with no offsetting benefit. Suppression is structural — legal penalties, police investigation, liability for 'subversion' or 'foreign collusion' offenses carry prison sentences up to life. No coordination function exists. Maximum experienced extraction.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDEPENDENT MEDIA OUTLET (TANGLED ROPE) — Organized but constrained. Can continue publishing (modest coordination benefit: access to supply chain, distribution networks, audience), but under legal threat. Advertising revenue dries up due to advertiser risk aversion; distribution channels become unreliable; reporting scope contracts sharply. The constraint provides minimal coordination — the outlet exists within a system — but asymmetric extraction dominates. Active enforcement is constant (police raids, advertiser pressure, legal surveillance). Suppression is high but not absolute; the outlet has some agency in choosing what to publish.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT / NARRATIVE CONTROL (ROPE) — Primary beneficiary. Experiences the constraint as coordination: suppressing independent narrative sources enables centralized information control, military-grade propaganda narrative maintenance, and suppression of destabilizing political discourse. The constraint solves a coordination problem (unified official narrative) with enforcement overhead, but the enforcement machinery extracts from journalists, not from the government. The government has arbitrage options (can shift enforcement, can negotiate with international bodies, can calibrate repression). Net beneficiary with positive directionality.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SELF-CENSORSHIP CULTURE / INSTITUTIONAL INERTIA (PITON) — Perspective at longer timescale. The National Security Law's threat of legal consequences has shifted from active enforcement (police raids, arrests) to internalized suppression (editors decline stories without explicit government instruction; publishers withdraw books preemptively; journalists leave the field). Theater ratio is high in this dimension — much of the suppressive effect operates through expectation and reputation risk, not through arrests. The formal enforcement machinery persists but an increasing share of constraint function is performed by self-imposed boundaries. From this perspective, the mechanism is becoming inertial — the threat environment is real but performance of suppression is increasingly theatrical (editors proving loyalty through caution, not through necessity).
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / FALSE SUMMIT RISK (MOUNTAIN) — From a global/civilizational perspective, press suppression via legal frameworks is presented as an immutable feature of sovereignty and security governance: 'every nation protects its core interests from external subversion; press freedom is a Western value not universal law.' This perspective risks naturalizing what is actually a contingent institutional choice. However, the structural data reveals beneficiary concentration (central government narrative control), asymmetric extraction (journalists bear all costs, gain nothing), and total suppression (no offsetting coordination) — the false summit detector will classify this as constructed constraint with identifiable beneficiaries, not as a natural law.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL PRESS ORGANIZATIONS (SNARE) — Can exit Hong Kong (journalists can be relocated, bureaus closed), but at significant cost: loss of market access, reduced reporting on China, diminished institutional mission. Constraints include visa denial, correspondent accreditation refusal, and legal liability for reporting deemed subversive. These organizations have more power and mobility than local journalists, but the mechanism still extracts narrative control with minimal offsetting benefit. They experience lower suppression than trapped local journalists, but the extraction logic is identical: control of what can be reported from Hong Kong.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hong_kong_press_freedom_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hong_kong_press_freedom_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hong_kong_press_freedom_suppression, TR),
    TR >= 0.70.

:- end_tests(hong_kong_press_freedom_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting asymmetric flow of control from journalists to government. The original regulatory capture (0.38) provided some coordination benefit — independent media outlets had space to operate, journalists had career paths, audiences had information diversity. The post-NSL constraint (0.68) extracts without reciprocal benefit: government gains narrative monopoly, journalists gain legal jeopardy. The 0.68 value reflects that complete suppression (1.0) is not yet achieved — some independent media persist, some international bureaus remain, some journalists continue reporting at risk. If arrests intensify and outlets close completely, extractiveness approaches 0.85+. Suppression (0.78): High, reflecting multiple enforcement mechanisms working in parallel. Legal criminal penalties (up to life imprisonment for subversion/foreign collusion) are the primary gate. Secondary mechanisms include police investigation threat, visa denial, business registration revocation, advertiser pressure, and social exclusion. Theater ratio (0.55): Moderate, and rising. Early enforcement (2020-2021) was highly theatrical: high-profile arrests of editors, raids on newsrooms, confiscation of printing equipment — visible enforcement demonstrating power. Recent trajectory (2022-2024) shows increasing shift toward invisible suppression: editors practice self-censorship without explicit instruction, publishers withdraw books preemptively, journalists leave the field voluntarily. The theater ratio rises as enforcement becomes internalized and less visible, meaning the same suppressive effect is achieved with fewer active enforcement events.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The government sees Rope — the National Security Law solves a coordination problem (unified narrative, suppressed separatism/subversion, centralized political control) with enforcement overhead. From the government's position, extraction is merely cost-of-operation, and the constraint coordinates information control. Journalists see Snare — pure extraction with legal threat, no benefit, no escape. Independent outlets see Tangled Rope — some benefit from existing media ecosystem, but extraction dominates. International organizations see constrained Snare — mobile exit option exists but at prohibitive cost. The self-censorship culture sees Piton — the threat environment is real (legal jeopardy persists) but performance of suppression is increasingly theatrical and internalized (no need for police raids when editors censor themselves). The analytical observer risks seeing Mountain — press suppression naturalized as inherent to sovereignty and security — but the structural data reveals beneficiary concentration and asymmetric extraction, triggering false summit detection.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position: beneficiaries have low d (experience suppression as coordination benefit); victims have high d (experience extraction). The central government, as primary beneficiary with arbitrage options (can adjust enforcement, can negotiate international pressure, can calibrate repression), derives d ≈ 0.10-0.20 → negative effective extraction (the government experiences the constraint as subsidizing its authority). Journalists, as powerless victims with no exit, derive d ≈ 0.92-0.98 → maximum effective extraction (f(d) ≈ 1.35-1.42). Independent media outlets, as organized but constrained victims, derive d ≈ 0.65-0.75 → high effective extraction. International organizations, as moderate-power actors with mobile exit options, derive d ≈ 0.55-0.65 → moderate-to-high extraction. The piton perspective (self-censorship culture) derives from institutional position and theater gate (rising theater ratio ≥ 0.55), not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the pre-2020 and post-2020 constraint regimes as structurally distinct (should be two separate stories). The pre-2020 constraint (ε ≈ 0.38, suppression ≈ 0.55) was genuinely tangled_rope — regulatory capture within quasi-democratic framework, with real but constrained press freedom and some coordination benefit (media outlets participated in information ecosystem, journalists had careers). The post-2020 constraint (ε ≈ 0.68, suppression ≈ 0.78) is snare — legal suppression with zero coordination benefit to victims. The transition was triggered by the National Security Law, which criminalized the coordination benefit (independent journalism became subversive journalism). The current story documents the post-2020 snare regime. The false summit risk is that analysts naturalize suppression as inherent to sovereignty (mountain perspective) rather than identifying it as a constructed constraint benefiting narrative control (snare with identifiable beneficiary). The engine's false summit detection will flag this through beneficiary declaration (central_government_narrative_control) and omega variables documenting the sovereignty naturalization risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_censorship_internalization_timeline,
    'How long before legal threat externality fully internalizes as voluntary editorial restraint, and how reversible is the internalization if legal threat is removed?',
    'Post-repeal analysis (if National Security Law enforcement ceases): do journalists/editors return to pre-2020 reporting scope? Timeline and magnitude of recovery.',
    'If internalization is rapid and stable: constraint has created cognitive capture that persists beyond legal enforcement. If reversal is quick: suppression mechanism is purely legal threat, not cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_censorship_internalization_timeline, empirical, 'Timeline and reversibility of self-censorship internalization').

omega_variable(
    coordination_function_existence,
    'Does the constraint provide any genuine coordination benefit to the targeted journalists/media outlets, or is extraction purely unidirectional?',
    'Comparative analysis: do independent media outlets gain access to resources, audiences, or institutional support that they lacked before? Do they report any offsetting benefits?',
    'If benefits exist: reclassify as Tangled Rope (mixed coordination and extraction). If zero benefits: classification as Snare is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_existence, empirical, 'Whether constraint provides offsetting coordination benefits to victims').

omega_variable(
    international_arbitrage_closure,
    'Are international press organizations truly mobile (can exit Hong Kong), or does the constraint effectively trap them through economic/political pressure despite formal mobility?',
    'Historical analysis of media bureau closures: initiated by organizations'' voluntary decisions or by pressure (visa denial, advertiser pressure, market collapse)?',
    'If genuinely mobile: international perspective remains constrained/snare hybrid. If trap mechanisms override formal mobility: reclassify as trapped, snare at global institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_arbitrage_closure, empirical, 'Whether international press organizations have genuine exit options').

omega_variable(
    false_summit_sovereignty_naturalization,
    'Is press suppression via national security law a natural feature of sovereignty (mountain), or a constructed constraint that benefits identifiable actors (snare)?',
    'Comparative analysis across democracies and authoritarian regimes: variation in press freedom correlates with specific institutional choices (judicial independence, rule of law, competing power centers), not with sovereignty itself. Sovereignty is compatible with both press freedom and suppression.',
    'If naturalized: analysis defaults to accepting suppression as inevitable. If constructed: identifies beneficiary (central narrative control) and enables analysis of dismantling mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_sovereignty_naturalization, conceptual, 'Whether press suppression is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hong_kong_press_freedom_suppression, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hkpf_tr_t0, hong_kong_press_freedom_suppression, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hkpf_tr_t2, hong_kong_press_freedom_suppression, theater_ratio, 2, 0.42).
narrative_ontology:measurement(hkpf_tr_t4, hong_kong_press_freedom_suppression, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(hkpf_be_t0, hong_kong_press_freedom_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hkpf_be_t2, hong_kong_press_freedom_suppression, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(hkpf_be_t4, hong_kong_press_freedom_suppression, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hkpf_su_t0, hong_kong_press_freedom_suppression, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hkpf_su_t2, hong_kong_press_freedom_suppression, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(hkpf_su_t4, hong_kong_press_freedom_suppression, suppression_requirement, 4, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hong_kong_press_freedom_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(hong_kong_press_freedom_suppression, taiwan_cross_strait_information_control).
narrative_ontology:affects_constraint(hong_kong_press_freedom_suppression, china_internet_control_system).

% DUAL FORMULATION NOTE:
% Hong Kong press suppression is structurally similar to Taiwan information control and China internet censorship but operates through different mechanisms (legal criminal liability vs administrative censorship vs platform control). Each operates with distinct ε values: Hong Kong NSL-based suppression (ε ≈ 0.68), Taiwan geopolitical narrative control (ε ≈ 0.55), China internet platform censorship (ε ≈ 0.72). Network linkage reflects shared beneficiary (central government narrative monopoly) and shared victim type (information commons, independent media), but distinct suppression mechanisms and enforcement infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hong_kong_press_freedom_suppression, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
