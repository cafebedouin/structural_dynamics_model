% ============================================================================
% CONSTRAINT STORY: crypto_export_controls_piton
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crypto_export_controls_piton, []).

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
 *   constraint_id: crypto_export_controls_piton
 *   human_readable: Crypto Export Controls as Degraded Strategic Asset Protection
 *   domain: economic_policy/national_security/technology
 *
 * SUMMARY:
 *   Cryptographic export controls represent a canonical piton: a Cold War-era
 *   strategic mechanism designed to protect U.S. asymmetric advantage in
 *   cryptographic capability that has become substantially performative as
 *   the technology distributed globally. The constraint originated as rope
 *   (coordination mechanism with genuine strategic benefit) in the
 *   1970s-1980s, evolved into tangled_rope (coordination with extraction)
 *   during the 1990s-2000s as international capabilities matured, and has
 *   degraded into piton (performative theater) since 2010 as open-source
 *   cryptography, academic publishing, and international research networks
 *   made the control mechanism functionally obsolete. The theater_ratio has
 *   risen from 0.42 (1995, controls still had measurable function) to 0.78
 *   (2025, controls are primarily performative ritual), while
 *   base_extractiveness has fallen from 0.35 to 0.18 as the actual cost to
 *   technology companies and researchers has fallen relative to the
 *   bureaucratic theater of compliance. The constraint is maintained through
 *   institutional inertia, classified-world risk aversion, and political
 *   narrative investment ('protecting U.S. security'), not through functional
 *   strategic necessity. A decentralized cryptography movement (open-source
 *   libraries, international academic collaboration, blockchain
 *   implementation) is providing end-runs around the constraint, creating a
 *   scaffold-like exit pathway that is gradually rendering the control
 *   mechanism obsolete.
 *
 * KEY AGENTS:
 *   - Cryptographers and Academic Researchers: Primary victims (powerless/trapped) — cannot publish, collaborate, or implement freely across borders
 *   - Technology Companies: Secondary victims (powerful/constrained) — face compliance burden and market limitations; larger firms can absorb costs, creating competitive disadvantage for startups
 *   - Defense Intelligence Establishment: Primary beneficiary (institutional/arbitrage) — benefits from coordination mechanism and maintained asymmetry, though strategic value of asymmetry has eroded
 *   - Export Control Bureaucracy: Institutional actor (institutional/constrained) — maintains apparatus through inertia; recognizes constraint is degraded but lacks exit mechanism
 *   - U.S. Strategic Advantage (Historical): Victim of technological diffusion (institutional/trapped at civilizational scope) — the claimed strategic justification for the constraint no longer coheres empirically
 *   - Open-Source Cryptography Movement: Organized exit actor (organized/mobile) — providing scaffold pathways that make export controls functionally irrelevant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crypto_export_controls_piton, 0.18).
domain_priors:suppression_score(crypto_export_controls_piton, 0.35).
domain_priors:theater_ratio(crypto_export_controls_piton, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crypto_export_controls_piton, extractiveness, 0.18).
narrative_ontology:constraint_metric(crypto_export_controls_piton, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(crypto_export_controls_piton, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crypto_export_controls_piton, piton).
narrative_ontology:human_readable(crypto_export_controls_piton, "Crypto Export Controls as Degraded Strategic Asset Protection").
narrative_ontology:topic_domain(crypto_export_controls_piton, "economic_policy/national_security/technology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crypto_export_controls_piton, domestic_intelligence_agencies).
narrative_ontology:constraint_beneficiary(crypto_export_controls_piton, legacy_defense_contractors).
narrative_ontology:constraint_victim(crypto_export_controls_piton, technology_companies).
narrative_ontology:constraint_victim(crypto_export_controls_piton, academic_researchers).
narrative_ontology:constraint_victim(crypto_export_controls_piton, international_competitiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRYPTOGRAPHER (SNARE) — Academic researchers and company engineers face absolute export barriers on source code and cryptographic implementations. Cannot publish freely, cannot collaborate internationally, cannot move intellectual property across borders. Trapped by legal prohibition and career vulnerability. Zero degrees of freedom within the constraint.
constraint_indexing:constraint_classification(crypto_export_controls_piton, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TECHNOLOGY COMPANY (TANGLED ROPE) — Faces genuine coordination burden: must implement cryptographic security for diverse customer bases while complying with export restrictions. But also benefits from the constraint — competitors face identical barriers, creating de facto market protection. Extraction (compliance cost + market limitation) is real but asymmetric: larger firms absorb cost more easily than startups.
constraint_indexing:constraint_classification(crypto_export_controls_piton, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE INTELLIGENCE ESTABLISHMENT (ROPE) — Benefits from the constraint as a coordination mechanism: unified control of cryptographic capability enables intelligence advantage and interoperability standards. Experiences the constraint as functional — maintains domestic asymmetry in cryptographic knowledge. Low extraction because this is their primary function.
constraint_indexing:constraint_classification(crypto_export_controls_piton, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXPORT CONTROL BUREAUCRACY (PITON) — The implementation machinery (State Department Directorate of Defense Trade Controls, NSF export compliance offices, institutional legal review) has become substantially performative. The technology is now too widely distributed (open-source cryptography, academic publications, international research networks) for controls to function as originally designed. Yet the bureaucratic apparatus persists through inertia, consuming resources and creating theater-grade compliance rituals without proportional strategic benefit. The mechanisms were designed to restrict 1980s-era military advantage; they now restrict diffuse dual-use knowledge that is already globally available.
constraint_indexing:constraint_classification(crypto_export_controls_piton, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: U.S. STRATEGIC CRYPTO ADVANTAGE (PITON) — In the 1970s-1990s, cryptographic capability was genuinely concentrated in the United States; export controls created measurable asymmetry. This advantage has eroded completely: international cryptographic research (Europe, Israel, China, Japan) has matched and exceeded U.S. capabilities, academic cryptography is global and open-source, and competing nations have built indigenous capabilities. The constraint now maintains theater of control without functional strategic benefit. The perception that export controls still protect U.S. advantage is kept alive through institutional inertia and risk-aversion narratives, but the empirical reality is that the constraint no longer achieves its original function.
constraint_indexing:constraint_classification(crypto_export_controls_piton, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZED CRYPTOGRAPHY MOVEMENT (SCAFFOLD) — Open-source cryptographic libraries (OpenSSL, libsodium, Bouncy Castle), international academic publishing, and blockchain technology have created effective alternative pathways that bypass the export control constraint. Organized actors (developers, researchers, companies) are exiting the constraint through implementation of post-regulation alternatives. The sunset is being driven not by policy change but by technological obsolescence of the control mechanism itself. The constraint has a finite functional lifespan as open-source tools mature.
constraint_indexing:constraint_classification(crypto_export_controls_piton, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — The constraint could be mistakenly classified as an unchangeable natural law of strategic necessity ('cryptographic capability must be controlled for national security'). But this misses the structural reality: the control mechanism is not natural, and the strategic rationale is historically contingent (coherent 1975-2000, incoherent 2010-present). The false summit here is naturalizing a degraded institutional arrangement as inevitable security requirement.
constraint_indexing:constraint_classification(crypto_export_controls_piton, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crypto_export_controls_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crypto_export_controls_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crypto_export_controls_piton, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(crypto_export_controls_piton, TR),
    TR >= 0.70.

:- end_tests(crypto_export_controls_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low and declining. The constraint no longer extracts meaningful strategic advantage because the technological asymmetry it was designed to protect no longer exists. Cryptographic capability is globally distributed. What extraction does occur is primarily the compliance cost imposed on technology companies and researchers, but this cost has declined as open-source alternatives have matured and export controls have become less consistently enforced. The 1995 extractiveness (0.35) reflected a genuine strategic asymmetry; the 2025 value (0.18) reflects the reality that the constraint is being worked around rather than enforced. Suppression (0.35): Moderate and declining. Barriers to international collaboration and knowledge transfer are real (legal liability, institutional risk-aversion, export licensing delays) but increasingly porous (open-source libraries bypass controls, academic publishing is global, blockchain implementations are decentralized). Theater ratio (0.78): High and rising. The bulk of export control activity is now performative: compliance audits, licensing review processes, and institutional legal gates consume significant resources but produce negligible strategic benefit. The theater rises as the actual control function decays — bureaucracies maintain apparatus through ritual when function erodes. Claimed type (Piton): Justified by theater_ratio ≥ 0.70 and the historical trajectory from functional constraint (rope) → extraction mechanism (tangled_rope) → degraded theatrical apparatus (piton).
 *
 * PERSPECTIVAL GAP:
 *   The defense intelligence establishment sees rope (coordination mechanism). The cryptographer sees snare (absolute prohibition). The technology company sees tangled_rope (mixed coordination and extraction). The bureaucracy sees piton (recognizes the apparatus is degraded but continues maintaining it). The open-source movement sees scaffold (building alternative pathways that bypass the constraint entirely). The historical strategic advantage sees piton from the civilizational perspective (what was once natural advantage has become artificial theater). The analytical observer risks seeing mountain (naturalizing the control as inherent security requirement) but the structural data reveals this as a false summit — the constraint is contingent on a technological asymmetry that no longer exists. The perspectival gap reveals that the constraint is most accurately classified as piton (degraded apparatus maintained through inertia) rather than rope (functional coordination mechanism) or snare (effective prohibition).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (defense intelligence, legacy defense contractors) have arbitrage-level exit options — they can choose to rely on export controls or develop alternative security mechanisms. They experience the constraint as low-extraction coordination benefit because their primary function (intelligence advantage) is no longer served by the constraint. Victims (cryptographers, technology companies, academic researchers) face trapped or constrained exit options depending on institutional affiliation, but the trap is weakening as decentralized alternatives emerge. The export control bureaucracy is constrained (cannot exit the apparatus without political authorization) but receives no clear extraction benefit — it maintains the constraint through institutional path-dependence rather than strategic interest. The technology companies experience moderate extraction relative to their power level because large firms can absorb compliance costs while startups cannot, creating market protection that benefits incumbents at the expense of competitive entry.
 *
 * MANDATROPHY ANALYSIS:
 *   The crypto export controls exemplify mandatrophy resolution through historical decomposition. The constraint is NOT a single unchanging entity — it has transitioned from rope (1970s-1980s, genuine strategic asymmetry requiring coordination mechanism) → tangled_rope (1990s-2000s, asymmetry eroding, coordination mixed with extraction) → piton (2010s-present, asymmetry eliminated, apparatus purely theatrical). The mandatrophy question 'is this extraction dressed as coordination, or coordination with extractive side effects?' resolves by showing both were historically true at different timepoints, and the current answer is 'neither — it is neither functional coordination nor efficient extraction, but degraded institutional theater.' The classification as piton (not snare or rope) is precise because: (1) extractiveness is low (0.18) relative to suppression (0.35), which is the opposite signature of pure extraction; (2) theater ratio (0.78) is the dominant signal — the apparatus is maintained for performative/institutional reasons rather than functional strategy; (3) the historical trajectory is clear — a once-functional constraint has decayed into institutional zombie. The false summit risk (mountain/natural law) is the most dangerous misclassification here: one could argue 'cryptography must be controlled for national security' as a transcendent truth. But this naturalizes what is actually a policy choice based on contingent technological conditions. The analytical observer must resist this temptation and recognize the constraint as degraded institutional arrangement, not immutable strategic requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    current_strategic_value,
    'Does cryptographic export control still provide meaningful strategic advantage to the United States in 2026?',
    'Comparative cryptanalytic capability assessment; competitive intelligence on international cryptographic research output; empirical measure of intelligence collection advantage traceable to export control enforcement',
    'If no meaningful advantage: the constraint is pure piton (theater without function), potentially candidate for formal sunset. If residual advantage exists: constraint shifts toward tangled_rope (coordination mechanism with strategic benefit justifying some extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(current_strategic_value, empirical, 'Whether export controls provide current strategic advantage').

omega_variable(
    compliance_cost_proportionality,
    'Is the compliance cost to technology companies and researchers proportionate to the actual security benefit achieved?',
    'Cost-benefit audit: total institutional compliance expense vs measured prevention of unauthorized cryptographic technology transfer; comparison to alternative control mechanisms (classification, institutional partnership agreements, international treaty frameworks)',
    'If cost significantly exceeds benefit: piton diagnosis is confirmed. If proportionate: constraint may be an expensive but functional rope or tangled_rope, not a degraded piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_proportionality, empirical, 'Proportionality of compliance cost to security benefit').

omega_variable(
    open_source_substitution_speed,
    'What is the effective functional lifetime of export controls as open-source cryptographic alternatives mature and achieve certification/institutional adoption?',
    'Timeline analysis of open-source tooling maturity vs institutional (government, financial, healthcare) adoption rates; identification of inflection point where open-source becomes dominant in critical infrastructure',
    'If substitution is rapid (2-5 years): scaffold sunset is real and imminent. If slow (10+ years): constraint persists longer as partial piton. If stuck: becomes permanent dead regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_substitution_speed, empirical, 'Rate of open-source cryptography substitution').

omega_variable(
    enforcement_degradation_mechanism,
    'What proportion of apparent non-compliance with export controls is undetectable vs actively tolerated due to cost-benefit assessment?',
    'Analysis of enforcement data: ratio of detected to estimated violations; interviews with export control compliance officers on enforcement prioritization; identification of de facto tolerance zones',
    'If high undetectable compliance: theater ratio is even higher than measured (0.78), constraint is approaching complete degradation. If high active tolerance: bureaucracy is making implicit cost-benefit calculations, behaving like rational piton rather than mechanical enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_degradation_mechanism, empirical, 'Undetectable vs tolerated non-compliance with export controls').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crypto_export_controls_piton, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypto_export_tr_t0, crypto_export_controls_piton, theater_ratio, 0, 0.42).
narrative_ontology:measurement(crypto_export_tr_t10, crypto_export_controls_piton, theater_ratio, 10, 0.58).
narrative_ontology:measurement(crypto_export_tr_t20, crypto_export_controls_piton, theater_ratio, 20, 0.72).
narrative_ontology:measurement(crypto_export_tr_t30, crypto_export_controls_piton, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(crypto_export_be_t0, crypto_export_controls_piton, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crypto_export_be_t10, crypto_export_controls_piton, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(crypto_export_be_t20, crypto_export_controls_piton, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(crypto_export_be_t30, crypto_export_controls_piton, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crypto_export_controls_piton, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(crypto_export_controls_piton, 0.12).
narrative_ontology:affects_constraint(crypto_export_controls_piton, semiconductor_export_restrictions).
narrative_ontology:affects_constraint(crypto_export_controls_piton, ai_chip_access_controls).
narrative_ontology:affects_constraint(crypto_export_controls_piton, quantum_computing_export_licensing).

% DUAL FORMULATION NOTE:
% Crypto export controls are historically linked to other strategic technology restrictions (semiconductors, AI chips, quantum computing). All share the piton diagnosis: they originated as rope (genuine asymmetry-based coordination), evolved into tangled_rope (asymmetry eroding), and are degrading into piton (theater maintained through bureaucratic inertia). The network effects matter because all three constraints compete for enforcement resources and political capital; as one becomes more obviously performative (crypto), pressure mounts on the others (semiconductors, AI) to justify their own continuing enforcement. A unified strategic technology export control framework would be more transparent but would require acknowledging that none of the individual constraints achieve their stated strategic objectives anymore.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crypto_export_controls_piton, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
