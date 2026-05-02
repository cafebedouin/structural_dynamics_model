% ============================================================================
% CONSTRAINT STORY: nonproliferation_feasibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nonproliferation_feasibility, []).

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
 *   constraint_id: nonproliferation_feasibility
 *   human_readable: AI Nonproliferation Treaty Feasibility
 *   domain: international_relations/technology_governance/strategic_competition
 *
 * SUMMARY:
 *   Sebastian Mallaby's proposal to trade chip export controls for an AI
 *   nonproliferation treaty presents a structural trade: scrap an enforceable
 *   but eroding mechanism (chip controls) for an unenforceable but
 *   diplomatically appealing mechanism (treaty commitments). The constraint's
 *   classification depends critically on the enforcement asymmetry between
 *   physical goods (chips) and information goods (model weights). NPT-style
 *   nonproliferation worked because nuclear material is physical, scarce, and
 *   detectable — uranium enrichment facilities cannot be hidden in academic
 *   papers or transmitted via GitHub. AI models are none of these things. The
 *   proposed treaty structure creates extraction: the diplomatic
 *   establishment gains institutional roles and processes regardless of
 *   verification feasibility, while US technological advantage bears the cost
 *   of scrapping enforceable controls for theatrical commitments. The theater
 *   ratio (0.65) reflects that verification mechanisms for AI development
 *   restrictions are largely performative — no inspection regime can monitor
 *   model training runs, weight sharing, or algorithmic insights diffusing
 *   through academic channels and open-source releases. The constraint has
 *   degraded over the interval as the enforcement asymmetry has become more
 *   apparent and open-source proliferation pathways have expanded.
 *
 * KEY AGENTS:
 *   - US Technological Advantage: Primary victim (powerless/trapped) — loses enforceable chip controls, gains unverifiable treaty commitments; cannot exit treaty framework once established without diplomatic cost
 *   - Mallaby/CFR Diplomatic Establishment: Primary beneficiary (institutional/arbitrage) — gains status, institutional roles, and career pathways from treaty architecture regardless of functional effectiveness
 *   - Multilateral Treaty Bureaucracy: Secondary beneficiary (institutional/constrained) — expands mandate and resources but constrained by verification impossibility; mixed coordination and extraction
 *   - Third-Party States: Secondary victim (moderate/constrained) — face asymmetric compliance costs and verification burdens without enforcement capacity; constrained by great-power treaty architecture
 *   - Verification Regime Credibility: Institutional victim (institutional/mobile) — treaty's verification mechanisms are theatrical; regime maintains form without substance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees enforcement asymmetry revealing the trade as structurally incoherent extraction masked as coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nonproliferation_feasibility, 0.68).
domain_priors:suppression_score(nonproliferation_feasibility, 0.72).
domain_priors:theater_ratio(nonproliferation_feasibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nonproliferation_feasibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(nonproliferation_feasibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nonproliferation_feasibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nonproliferation_feasibility, snare).
narrative_ontology:human_readable(nonproliferation_feasibility, "AI Nonproliferation Treaty Feasibility").
narrative_ontology:topic_domain(nonproliferation_feasibility, "international_relations/technology_governance/strategic_competition").

domain_priors:requires_active_enforcement(nonproliferation_feasibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nonproliferation_feasibility, mallaby_cfr_diplomatic_establishment).
narrative_ontology:constraint_beneficiary(nonproliferation_feasibility, multilateral_treaty_bureaucracy).
narrative_ontology:constraint_victim(nonproliferation_feasibility, us_technological_advantage).
narrative_ontology:constraint_victim(nonproliferation_feasibility, verification_regime_credibility).
narrative_ontology:constraint_victim(nonproliferation_feasibility, third_party_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: US TECHNOLOGICAL ADVANTAGE (SNARE) — Trapped by the proposed treaty structure: scrapping chip controls eliminates the one enforceable mechanism while gaining only unverifiable commitments. Cannot exit the treaty framework once established without diplomatic cost. Bears maximum extraction — gives up tangible leverage for theatrical compliance.
constraint_indexing:constraint_classification(nonproliferation_feasibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD-PARTY STATES (SNARE) — Constrained by great-power treaty architecture that imposes verification burdens and development restrictions without addressing the enforcement asymmetry. Can technically refuse to join but face diplomatic pressure and exclusion from AI governance forums. Experience high extraction through asymmetric compliance costs.
constraint_indexing:constraint_classification(nonproliferation_feasibility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MALLABY/CFR DIPLOMATIC ESTABLISHMENT (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: the treaty framework creates institutional roles, diplomatic processes, and career pathways regardless of verification feasibility. Extraction runs toward this agent — they gain status and influence from the treaty architecture itself, not from its functional effectiveness.
constraint_indexing:constraint_classification(nonproliferation_feasibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTILATERAL TREATY BUREAUCRACY (TANGLED ROPE) — Benefits from expanded mandate and resources for AI governance but also constrained by the verification impossibility. Genuine coordination function (establishing norms, facilitating dialogue) exists alongside extraction (bureaucratic expansion without enforcement capacity). Mixed experience — institutional growth coupled with structural futility.
constraint_indexing:constraint_classification(nonproliferation_feasibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: VERIFICATION REGIME CREDIBILITY (PITON) — The treaty's verification mechanisms are largely theatrical: model weights diffuse through academic channels, open-source releases, and third-party development that no inspection regime can monitor. The verification ritual persists through diplomatic inertia despite near-zero functional enforcement capacity. High theater ratio — the regime maintains the form of nonproliferation without the substance.
constraint_indexing:constraint_classification(nonproliferation_feasibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination need (preventing AI arms race dynamics) and the structural extraction (diplomatic establishment benefits from treaty architecture that cannot achieve its stated purpose). The enforcement asymmetry between chip controls (verifiable, enforceable) and model development restrictions (unverifiable, unenforceable) reveals the trade as structurally incoherent — scrapping the enforceable mechanism for the unenforceable one is extraction masked as coordination.
constraint_indexing:constraint_classification(nonproliferation_feasibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nonproliferation_feasibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nonproliferation_feasibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nonproliferation_feasibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nonproliferation_feasibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nonproliferation_feasibility, TR),
    TR >= 0.70.

:- end_tests(nonproliferation_feasibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The treaty structure extracts from US technological advantage by trading an enforceable mechanism (chip controls can be monitored at chokepoints, enforced through export licensing, and verified through supply chain auditing) for an unenforceable mechanism (model development restrictions cannot be monitored without inspecting every research lab, university, and compute cluster globally). The diplomatic establishment captures the extraction — they gain institutional roles from the treaty architecture whether or not it achieves nonproliferation. The value reflects that the enforcement asymmetry is structural, not contingent: information goods diffuse in ways physical goods do not. Suppression (0.72): High. Once the treaty framework is established, exiting it carries significant diplomatic cost. The US cannot easily reverse course without appearing to abandon multilateral cooperation. Third-party states face pressure to join and comply despite asymmetric burdens. The suppression is institutional rather than physical — the constraint operates through diplomatic norms and reputational costs. Theater ratio (0.65): High and rising. Verification mechanisms for AI development restrictions are substantially performative. Proposed methods (model fingerprinting, compute auditing, training run detection) face fundamental evasion pathways: academic publication of algorithmic insights, open-source model releases, distributed training across jurisdictions, and third-party replication. The theater has increased over the interval as open-source proliferation has expanded and the verification impossibility has become more apparent.
 *
 * PERSPECTIVAL GAP:
 *   The diplomatic establishment sees pure coordination (Rope) — the treaty creates institutional processes and dialogue mechanisms that they experience as beneficial regardless of enforcement capacity. US technological advantage sees pure extraction (Snare) — scrapping enforceable controls for unverifiable commitments with no exit path once the treaty framework is established. Third-party states also see extraction (Snare) — asymmetric compliance burdens without enforcement capacity. The multilateral bureaucracy sees mixed coordination and extraction (Tangled Rope) — genuine norm-setting function alongside institutional expansion without verification capacity. The verification regime sees degraded theater (Piton) — maintains the form of nonproliferation without the substance. The analytical observer sees the enforcement asymmetry revealing extraction masked as coordination (Tangled Rope) — the genuine coordination need (preventing AI arms race) exists but the proposed mechanism structurally cannot achieve it, making the trade extractive rather than functional.
 *
 * DIRECTIONALITY LOGIC:
 *   The Mallaby/CFR diplomatic establishment is the primary beneficiary with arbitrage exit options — they can move between policy roles, think tanks, and academic positions regardless of treaty effectiveness. Their directionality is low (d ≈ 0.10), producing negative or near-zero effective extraction — they experience the constraint as pure coordination because extraction runs toward them. US technological advantage is the primary victim with trapped exit options — once chip controls are scrapped and treaty commitments made, reversing course carries prohibitive diplomatic cost. Their directionality is high (d ≈ 0.92), producing maximum effective extraction. Third-party states are secondary victims with constrained exit options — they can refuse to join but face diplomatic pressure and exclusion from governance forums. Their directionality is moderately high (d ≈ 0.68). The multilateral treaty bureaucracy has mixed beneficiary/victim status with constrained exit — they gain resources and mandate but are constrained by verification impossibility. Their directionality is moderate (d ≈ 0.45). The verification regime credibility is institutional with mobile exit — the regime can shift focus to other domains when AI verification proves infeasible. The analytical observer uses the analytical exit option and derives directionality from the structural asymmetry (d ≈ 0.55), seeing both coordination need and extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that the classification depends on whether the enforcement asymmetry is acknowledged. If AI model development restrictions were enforceable at NPT-comparable levels, the treaty would be genuine coordination (Rope or Tangled Rope depending on compliance costs). But the structural difference between physical goods (chips) and information goods (models) makes enforcement asymmetric: chip controls are verifiable, model restrictions are not. The diplomatic establishment benefits from ignoring this asymmetry — the treaty architecture creates institutional roles regardless of functional effectiveness. US technological advantage bears the cost of the asymmetry — loses enforceable leverage, gains theatrical commitments. The mandatrophy is not 'is nonproliferation good?' (yes, preventing AI arms races is a genuine coordination need) but 'does this specific mechanism achieve nonproliferation or does it extract from enforceable controls for diplomatic theater?' The structural data (verification impossibility, open-source proliferation pathways, third-party diffusion) reveals extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_technology_breakthrough,
    'Could future verification technology (model fingerprinting, training run detection, compute auditing) make AI nonproliferation enforceable at scales comparable to NPT nuclear material accounting?',
    'Technical feasibility analysis of proposed verification methods; comparison of evasion costs for AI model development vs nuclear material diversion; assessment of third-party proliferation pathways that bypass verification entirely',
    'If verification breakthrough occurs: treaty structure shifts from snare to tangled_rope (genuine coordination function emerges). If verification remains infeasible: current snare classification persists and treaty becomes pure theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_technology_breakthrough, empirical, 'Whether verification technology can achieve NPT-comparable enforceability').

omega_variable(
    china_compliance_credibility,
    'Does China have structural incentives to comply with AI development restrictions when enforcement is unverifiable and strategic advantage from defection is high?',
    'Game-theoretic analysis of compliance incentives under unverifiable commitments; historical analysis of Chinese compliance with unenforceable treaty provisions; assessment of domestic political costs of visible vs invisible defection',
    'If compliance credible: treaty has coordination value despite verification limits. If compliance not credible: treaty is pure extraction from US (scraps enforceable controls for theatrical commitments).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_compliance_credibility, conceptual, 'Whether China has credible incentives for unverifiable compliance').

omega_variable(
    open_source_proliferation_pathway,
    'Do open-source model releases and academic diffusion create proliferation pathways that make any bilateral treaty structurally irrelevant?',
    'Empirical tracking of frontier model capabilities appearing in open-source releases; analysis of academic publication and replication timelines; assessment of whether US-China restrictions matter when third parties can replicate',
    'If open-source pathway dominates: treaty is theater regardless of compliance (the constraint it claims to address is already uncontrollable). If bilateral control remains meaningful: treaty structure matters but enforcement asymmetry remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_proliferation_pathway, empirical, 'Whether open-source diffusion makes bilateral treaties irrelevant').

omega_variable(
    chip_control_durability,
    'How long can chip export controls maintain effectiveness as China develops indigenous semiconductor capabilities and third-party suppliers emerge?',
    'Technical assessment of Chinese semiconductor roadmap; analysis of third-party supplier development (Taiwan, South Korea, Netherlands under different political scenarios); projection of compute cost curves and substitution pathways',
    'If chip controls have short remaining lifespan: trading them for treaty commitments is less costly (losing an eroding asset). If chip controls remain durable: trading them is high-cost extraction (scrapping a functional mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chip_control_durability, empirical, 'Remaining durability of chip export controls as enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nonproliferation_feasibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nonprolif_theater_initial, nonproliferation_feasibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nonprolif_theater_early, nonproliferation_feasibility, theater_ratio, 3, 0.48).
narrative_ontology:measurement(nonprolif_theater_mid, nonproliferation_feasibility, theater_ratio, 6, 0.58).
narrative_ontology:measurement(nonprolif_theater_current, nonproliferation_feasibility, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(nonprolif_extract_initial, nonproliferation_feasibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nonprolif_extract_early, nonproliferation_feasibility, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(nonprolif_extract_mid, nonproliferation_feasibility, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(nonprolif_extract_current, nonproliferation_feasibility, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nonproliferation_feasibility, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of chip_control_efficacy (the enforceable mechanism being traded away) and cooperation_credibility (whether treaty commitments are believable). The nonproliferation feasibility constraint has its own extractiveness reflecting the enforcement asymmetry and institutional extraction, distinct from the upstream constraints' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nonproliferation_feasibility, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
