% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
 *   constraint_id: qwerty_vs_dvorak
 *   human_readable: QWERTY Keyboard Lock-in vs. Dvorak Alternative
 *   domain: technological/product_standardization
 *
 * SUMMARY:
 *   The QWERTY keyboard lock-in is a canonical example of technological path
 *   dependence and network effects creating a constraint that persists long
 *   after its original functional justification disappears. QWERTY was
 *   designed to prevent mechanical key jamming in typewriters — a problem
 *   that is completely irrelevant to digital input devices. Yet QWERTY
 *   remains the default global keyboard standard despite clear evidence that
 *   alternative layouts like Dvorak improve typing ergonomics and efficiency.
 *   This constraint exhibits tangled coordination (the ecosystem genuinely
 *   benefits from standardization) and asymmetric extraction (individual
 *   users who prefer Dvorak are trapped by the installed base and switching
 *   costs). The constraint's nature is revealed through perspectival
 *   analysis: institutional actors see it as pure coordination (rope);
 *   trapped individuals see it as extraction (snare); the ergonomic movement
 *   sees it as temporary (scaffold); the historical legacy sees it as ghostly
 *   and inertial (piton). The analytical observer risks naturalizing what is
 *   actually a historical contingency as an inevitable law of network
 *   effects.
 *
 * KEY AGENTS:
 *   - Dvorak Adopters: Individual users (powerless/trapped) — choose ergonomic efficiency but face insurmountable switching costs on every shared computer and job
 *   - QWERTY Ecosystem Vendors: Hardware and software manufacturers (institutional/arbitrage) — benefit from standardization, supply chain coordination, driver compatibility, linguistic processing infrastructure optimized for QWERTY
 *   - Installed User Base: Existing QWERTY-trained workers (moderate/mobile or constrained) — benefit from dominance of their learned skill; switching costs for retraining are real but surmountable
 *   - OS and Software Ecosystem: Apple, Microsoft, Linux (institutional/arbitrage) — could support Dvorak at marginal cost but have zero incentive; QWERTY standardization subsidizes their ecosystem
 *   - Ergonomic Alternative Movement: Accessibility researchers, layout advocates, input innovation communities (organized/mobile) — see QWERTY lock-in as temporary technical debt, not inevitable constraint
 *   - Mechanical Typewriter Legacy: Historical artifact (institutional/arbitrage) — the original mechanical jamming problem that justified QWERTY's design is completely obsolete; QWERTY persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.55).
domain_priors:suppression_score(qwerty_vs_dvorak, 0.68).
domain_priors:theater_ratio(qwerty_vs_dvorak, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.55).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_vs_dvorak, tangled_rope).
narrative_ontology:human_readable(qwerty_vs_dvorak, "QWERTY Keyboard Lock-in vs. Dvorak Alternative").
narrative_ontology:topic_domain(qwerty_vs_dvorak, "technological/product_standardization").

domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, qwerty_ecosystem_vendors).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, installed_user_base).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, dvorak_adopters).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, typing_ergonomics_improvement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DVORAK ADOPTER (SNARE) — Trapped by the installed base. An individual user who learns Dvorak for ergonomic benefit faces insurmountable switching costs: every shared computer, every job, every public terminal runs QWERTY. They cannot exit without abandoning their investment in Dvorak skill. No alternatives exist at scale. Maximum experienced extraction.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: KEYBOARD MANUFACTURER (TANGLED ROPE) — Faces high switching costs to produce Dvorak keyboards (lower production volume, higher per-unit cost, reduced compatibility) but benefits from QWERTY coordination — manufacturing standardization, supply chain simplicity, OS driver support. Could theoretically exit by specializing in Dvorak, but market size too small. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OS/SOFTWARE ECOSYSTEM (ROPE) — Institutional beneficiary with full arbitrage capacity. Apple, Microsoft, Linux, and smartphone OS vendors benefit from QWERTY coordination: default keyboard layout, character input standards, linguistic processing, accessibility infrastructure all assume QWERTY. They could support Dvorak at zero marginal cost (remapping is trivial) but have no incentive — standardization on QWERTY subsidizes their ecosystem. Net beneficiary.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ERGONOMIC ALTERNATIVE MOVEMENT (SCAFFOLD) — Organized agents (ergonomics researchers, alternative layout advocates, accessibility communities) see QWERTY lock-in as a temporary technical debt. Voice input, eye-tracking, neural interfaces, and adaptive layouts represent genuine alternative pathways that bypass the QWERTY/Dvorak binary entirely. Unlike traditional transition mechanisms (migration, re-education), these modalities fundamentally change the problem space. Sunset clause: as input modalities diversify away from physical keyboards (estimated 30-50 years), QWERTY loses enforcement power. Low effective extraction because the coalition sees escape routes.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MECHANICAL TYPEWRITER LEGACY (PITON) — QWERTY's original function (preventing key jamming in mechanical typewriters) is completely obsolete in digital systems where keystroke collisions are software-managed, not mechanical. The layout persists through institutional inertia: decades of embedded software assumptions, training materials, linguistic processing built on QWERTY's letter frequency distribution. The ghost of the mechanical constraint haunts modern computing. Theater ratio reflects that QWERTY continues as institutional ritual long after its functional justification vanished.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NETWORK EFFECTS VIEW (MOUNTAIN) — From a universal/civilizational scale, network effects in standardized communication appear as immutable laws: a coordination standard's value increases with its user population, creating a stable equilibrium that is incompatible with alternatives. QWERTY is locked in by positive feedback (more users → more keyboards → more training → more users). This perspective risks naturalizing what is actually a historical contingency — the initial choice of QWERTY was driven by mechanical constraints (key jamming), not network effects analysis. The engine will identify this as a false summit: the 'inevitable lock-in' framing obscures that alternative layouts could theoretically achieve coordination if sufficient migration occurred.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_vs_dvorak_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_vs_dvorak, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_vs_dvorak, TR),
    TR >= 0.70.

:- end_tests(qwerty_vs_dvorak_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint extracts from Dvorak adopters by imposing switching costs (relearning, keyboard procurement, accommodation requests) and from society by preventing efficiency gains that could emerge from layout optimization. However, extraction is not total because the installed base legitimately benefits from standardization coordination — QWERTY enables common keyboards, training, cross-organization compatibility. The moderate-high value reflects this mix: real extraction (Dvorak users bear costs while ecosystem benefits) but with genuine coordination function (the ecosystem does solve a real problem). Trajectory: rising from 0.35 to 0.55 over 50 years reflects increasing suppression as the installed base grows and digital ecosystems deepen their QWERTY assumptions (linguistic models, accessibility standards, voice input training data). Suppression (0.68): High. Multiple barriers prevent exit: switching costs for users (relearning), low market demand deterring keyboard manufacturers from producing Dvorak units, OS vendors' lack of incentive to support alternatives, workplace standardization policies, and ergonomic research not translating to market adoption. Theater ratio (0.45): Moderate. QWERTY is functionally important for coordination but carries less theater than piton-level constraints — it actually solves the real problem of keyboard standardization. The theater component reflects obsolete mechanical justifications still embedded in cultural narratives ('QWERTY is designed for typing speed' — false; 'mechanical constraints make QWERTY optimal' — obsolete in digital). Trajectory: rising from 0.32 to 0.45 reflects increasing theater as the mechanical justification becomes further divorced from digital reality, while coordination benefits remain.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is extreme and diagnostic. The institutional beneficiary (OS/software vendors) sees pure coordination — QWERTY enables their ecosystem function and costs them nothing to maintain. The trapped Dvorak adopter sees extraction — they sacrifice ergonomic gain to participate in the installed base. The organized alternative movement (scaffold perspective) sees a temporal structure — input modality diversification (voice, neural, eye-tracking) will eventually displace keyboard-based input, making QWERTY irrelevant. The piton perspective (mechanical typewriter legacy) sees a degraded ritual — QWERTY's original function (key jamming prevention) is completely obsolete; the layout persists through institutional inertia. The analytical observer (mountain perspective) risks naturalizing what is historically contingent — treating network effects as immutable laws rather than recognizing that QWERTY's lock-in depends on the specific historical moment when mechanical typewriters achieved dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (QWERTY ecosystem vendors and installed base) derives d ≈ 0.10-0.25: they are institutional/arbitrage actors with full exit capacity and are beneficiaries of the standardization function. The sigmoid f(d) produces negative or very low effective extractiveness — they experience the constraint as pure coordination, not extraction. The victim group (Dvorak adopters) derives d ≈ 0.85-0.95: they are powerless/trapped, with no realistic exit options; they experience the constraint as full extraction. The moderate installed base (workers who learned QWERTY) derives d ≈ 0.45-0.60: constrained exit options (could theoretically retrain but with high cost), moderate power; they experience mixed extraction-coordination. The analytical observer derives d ≈ 0.72: by definition, analytical contexts map to canonical d=0.72 in the absence of specific beneficiary/victim data. The engine scales extractiveness via f(d) and scope σ(S): global scope σ = 1.2 amplifies the effective extraction. The resulting chi values explain why trap+global produces snare classification (χ ≥ 0.66), while institutional+arbitrage+global produces rope (χ ≤ 0.35).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies all three gates for tangled rope classification. (1) Genuine coordination function: QWERTY does solve the real problem of keyboard standardization — unified training, compatible devices, shared linguistic processing. (2) Asymmetric extraction: The ecosystem and installed base benefit while Dvorak adopters bear costs without corresponding benefit. (3) Active enforcement: The constraint is actively maintained through OS vendor choices, workplace policies, hardware design decisions, and absence of support for alternatives. The mandatrophy is resolved by recognizing that the constraint's longevity comes from the real coordination benefit it provides — standardization is genuinely valuable — not from pure extraction. However, the constraint is ALSO extractive because it prevents superior alternatives (Dvorak's ergonomic benefits) from displacing the suboptimal standard. The false summit perspective (mountain/analytical) tempts the observer to naturalize this as inevitable — 'network effects create lock-in, and lock-in is a law of technology.' But the perspectival analysis reveals the contingency: Dvorak could theoretically achieve tipping point adoption if sufficient migration occurred; the lock-in is deep but not immutable. The Colemak layout (which is easier to transition to from QWERTY than Dvorak) has achieved ~1% adoption in some regions, proving that alternatives CAN gain foothold — the question is not mechanical impossibility but economic insufficiency. The mandatrophy dissolves when we recognize that the constraint's type depends on temporal scale: at immediate timescales, it is organizational rope (pure coordination). At biographical timescales, it is tangled rope (users face both coordination benefit and extraction cost). At civilizational timescales, it approaches piton (historical artifact), especially as input modalities diversify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_dvorak,
    'What user population fraction would enable Dvorak adoption to reach self-sustaining network effects?',
    'Historical comparative analysis: other keyboard layout adoptions (Colemak, Workman); critical mass models for technology adoption; market simulation of regional or sectoral Dvorak dominance',
    'If threshold ≤ 5%: Dvorak remains suppressed despite potential; constraints the snare classification. If threshold > 20%: adoption is impossible given current incentive structure; strengthens mountain view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_dvorak, empirical, 'User population threshold for Dvorak self-sustaining adoption').

omega_variable(
    ergonomic_benefit_measurement,
    'Do Dvorak layouts produce measurable improvements in typing speed, accuracy, or repetitive strain injury reduction across diverse user populations?',
    'Meta-analysis of controlled typing studies; longitudinal injury data for Dvorak vs QWERTY users; skill transfer and learning curve analysis; ecological validity across typing contexts',
    'If benefits marginal or context-dependent: lock-in is weaker (users lack compelling reason to defect; tangled rope softens). If benefits substantial: lock-in is extractive (users sacrifice performance; snare classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ergonomic_benefit_measurement, empirical, 'Measured ergonomic advantage of Dvorak over QWERTY').

omega_variable(
    institutional_incentive_asymmetry,
    'What fraction of OS/software ecosystem participants would benefit from multistandard keyboard support vs. incurring costs?',
    'Survey of OS developers, input method engineering teams, accessibility specialists; cost-benefit analysis for supporting dual keyboard standards; market segmentation analysis of Dvorak willingness-to-pay',
    'If ecosystem cost < willingness-to-pay: technical barrier is artificial; constraint reclassifies toward rope (pure coordination, solvable). If ecosystem cost >> willingness-to-pay: requires coordinated migration; strengthens snare/tangled-rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_asymmetry, empirical, 'Cost-benefit calculation for OS ecosystem keyboard standard support').

omega_variable(
    alternative_input_modality_timeline,
    'At what adoption rate do voice, neural, and eye-tracking interfaces displace keyboard-based input as the primary human-computer interface?',
    'Trend analysis of interface technology adoption curves; market research on input modality preferences; measurement of keyboard input as fraction of total human-computer interactions',
    'If displacement occurs within 30 years: scaffold sunset clause is structural and the constraint degrades toward rope/piton. If displacement > 50 years: QWERTY lock-in remains binding for multiple generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_input_modality_timeline, empirical, 'Adoption timeline for non-keyboard input modalities').

omega_variable(
    historical_contingency_vs_inevitable_lock_in,
    'Was QWERTY''s dominance inevitable given network effects alone, or was it contingent on the specific historical moment of mechanical typewriter standardization?',
    'Counterfactual historical analysis: what if Dvorak had achieved 10% market share by 1950? Comparative study of other technology standards that failed despite superiority (Beta VCR, HDDVD). Game-theoretic analysis of lock-in bifurcation points.',
    'If contingent: mountain classification is false summit; constraint is actually tangled rope with very high suppression. If inevitable: network effects theory is vindicated and mountain view is analytically justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_contingency_vs_inevitable_lock_in, conceptual, 'Whether QWERTY lock-in is inevitable or historically contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_vs_dvorak, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_tr_t0, qwerty_vs_dvorak, theater_ratio, 0, 0.32).
narrative_ontology:measurement(qwerty_tr_t25, qwerty_vs_dvorak, theater_ratio, 25, 0.38).
narrative_ontology:measurement(qwerty_tr_t50, qwerty_vs_dvorak, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qwerty_be_t0, qwerty_vs_dvorak, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_be_t25, qwerty_vs_dvorak, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(qwerty_be_t50, qwerty_vs_dvorak, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_su_t0, qwerty_vs_dvorak, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(qwerty_su_t25, qwerty_vs_dvorak, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(qwerty_su_t50, qwerty_vs_dvorak, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_vs_dvorak, information_standard).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, typing_skill_transferability).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, ergonomic_standards_institutional_resistance).

% DUAL FORMULATION NOTE:
% QWERTY lock-in is upstream of multiple downstream constraints. The typing skill transferability problem (whether skills learned in one layout transfer to another) is downstream — it depends on QWERTY's global dominance. The institutional resistance to ergonomic standards is also downstream — organizations adopt suboptimal ergonomic policies because QWERTY is treated as a given. Network decomposition: QWERTY lock-in is the parent constraint; typing transfer and ergonomic policy are distinct child constraints with their own ε values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
