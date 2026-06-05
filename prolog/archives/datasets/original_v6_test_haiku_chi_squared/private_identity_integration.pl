% ============================================================================
% CONSTRAINT STORY: private_identity_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_identity_integration, []).

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
 *   constraint_id: private_identity_integration
 *   human_readable: The Closed-Door Identity Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The closed-door identity protocol mandates that individuals keep insights
 *   about their evolving needs, wants, and identity private for several years
 *   while undergoing integration — a period of psychological consolidation
 *   before external disclosure and social judgment. This constraint sits at
 *   the intersection of developmental psychology and contemporary
 *   surveillance capitalism. Identity integration is a genuine psychological
 *   need: premature exposure to social judgment can create defensive rigidity
 *   or identity diffusion. However, the constraint is increasingly enforced
 *   not by internal developmental logic but by the perceived permanence of
 *   digital records, social media algorithms that amplify inconsistency, and
 *   institutional expectations for continuous public authenticity. The
 *   constraint exhibits a classic tangled rope structure: the coordination
 *   function (private space for reflection with trusted others) is genuine
 *   and healthy, but the extraction mechanism (permanent digital records,
 *   social judgment, reputational cost of documented change) has increased
 *   due to technological and institutional changes.
 *
 * KEY AGENTS:
 *   - Identity Integrators: Primary agents (moderate/constrained) — undergoing psychological development; need privacy but face suppression from digital surveillance and social judgment
 *   - Close Relationships: Primary beneficiaries (institutional/arbitrage) — partners, therapists, trusted family; benefit from private vulnerability and deepened authenticity
 *   - Identity Experimenters: Potential victims (powerless/trapped) — especially young people, marginalized communities, gender/sexuality explorers who face high reputational risk from documented identity exploration
 *   - Social Media Platforms: Institutional beneficiaries (institutional/arbitrage) — benefit from continuous disclosure, algorithmic content generation, permanence of the record; experience constraint as coordination requirement
 *   - Privacy-First Movement: Organized actors (organized/mobile) — therapists, digital literacy advocates, privacy technologists building alternative frameworks and norms
 *   - Authenticity-Mandate Culture: Institutional enforcement mechanism (institutional/constrained) — corporate brand-building discourse, influencer economy, Instagram optimization culture that mandates continuous public self-disclosure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine developmental need and contingent institutional construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_identity_integration, 0.38).
domain_priors:suppression_score(private_identity_integration, 0.48).
domain_priors:theater_ratio(private_identity_integration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_identity_integration, extractiveness, 0.38).
narrative_ontology:constraint_metric(private_identity_integration, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(private_identity_integration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_identity_integration, tangled_rope).
narrative_ontology:human_readable(private_identity_integration, "The Closed-Door Identity Protocol").
narrative_ontology:topic_domain(private_identity_integration, "social/psychological").

domain_priors:requires_active_enforcement(private_identity_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_identity_integration, identity_integrators).
narrative_ontology:constraint_beneficiary(private_identity_integration, close_relationships).
narrative_ontology:constraint_beneficiary(private_identity_integration, psychological_coherence).
narrative_ontology:constraint_victim(private_identity_integration, public_exposure).
narrative_ontology:constraint_victim(private_identity_integration, identity_experimenters).
narrative_ontology:constraint_victim(private_identity_integration, premature_commitment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY EXPERIMENTER (SNARE) — Social media, peer observation, and algorithmic recording create a permanent archive of every identity exploration. Trapped by the permanence of digital records and social judgment; cannot experiment with new identity dimensions without documentation and reputational extraction. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.64. Suppression is high: social shaming, algorithmic amplification of inconsistency, employer/peer-group monitoring.
constraint_indexing:constraint_classification(private_identity_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY INTEGRATOR (TANGLED ROPE) — Individual undergoing genuine psychological development benefits from coordination (having a safe space to explore with close others) but is constrained by the risk of premature disclosure and social extraction. Coordination function: private conversations enable feedback and reflection. Extraction: once disclosed, new identity dimensions become social property subject to others' expectations and judgment. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(private_identity_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLOSE RELATIONSHIPS (ROPE) — Partners, therapists, close family experience the closed-door protocol as pure coordination: witnessing identity integration deepens trust and enables authentic support. Arbitrage exit: can withdraw from participation without cost; relationship deepens through chosen vulnerability. d≈0.10, f(d)≈0.02, σ=0.8 → χ≈0.003. Minimal extraction; genuine coordination benefit.
constraint_indexing:constraint_classification(private_identity_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PRIVACY-FIRST MOVEMENT (SCAFFOLD) — Organized agents (therapists, Gen-Z digital literacy advocates, privacy technologists) recognize the protocol as a temporary solution with a sunset: as digital norms mature toward greater privacy protection and psychological literacy becomes mainstream, individuals will have legal frameworks (data minimization, right to be forgotten) and cultural acceptance that reduce the need for private integration windows. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.13. High theater (advocacy, consciousness-raising) paired with genuine institutional scaffolding.
constraint_indexing:constraint_classification(private_identity_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTHENTICITY PARADOX (PITON) — Social media platforms mandate continuous self-disclosure in the name of 'authenticity' and 'building your brand.' The constraint persists as institutional inertia: the mandate to be publicly visible and consistently authentic conflicts with the reality that identity development requires privacy. Theater ratio ≈0.65: the performative demand to be 'authentic' crowds out actual authenticity work. The piton observation: people maintain the public authenticity theater while doing real identity work in closed doors, then experience cognitive dissonance. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.49.
constraint_indexing:constraint_classification(private_identity_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a developmental psychology perspective, identity integration requires a consolidation phase before external validation or judgment. This is an immutable property of psychological maturation: premature disclosure creates defensive rigidity or identity diffusion. However, the structural data (ε=0.38, suppression=0.48, theater=0.55) suggests this is NOT an invariant law but a contingent institutional fact about how surveillance capitalism and social judgment work. The observer risks naturalizing what is actually a socially constructed constraint.
constraint_indexing:constraint_classification(private_identity_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_identity_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_identity_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_identity_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_identity_integration, TR),
    TR >= 0.70.

:- end_tests(private_identity_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The base extraction is significant but not severe because the constraint reflects a genuine psychological need (identity integration does require time) alongside contingent institutional suppression (digital permanence, social judgment). The extraction has increased from 0.22 to 0.38 over the interval as social media adoption has made privacy more costly and disclosure more permanent. Suppression (0.48): Moderate-high. Suppression is enacted through social shaming (public inconsistency as disqualification), algorithmic amplification (contradiction becomes 'content'), reputational extraction (documented change used against the person), and institutional expectations (employer screening of social media). Theater ratio (0.55): Moderate. The constraint involves substantial performative work: maintaining a consistent public persona while doing private identity work creates cognitive load and behavioral theater. However, the theater is not total — the constraint also reflects genuine developmental psychology. The increase from 0.35 to 0.55 reflects growing institutional pressure for public authenticity (LinkedIn, brand-building discourse).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. Close relationships experience pure coordination (Rope): private vulnerability deepens authenticity. Identity integrators experience mixed extraction and coordination (Tangled Rope): they need privacy but feel trapped by the cost of disclosure. Identity experimenters facing high social risk experience pure extraction (Snare): every identity exploration is recorded and weaponized. The privacy movement sees a temporary problem with a solution path (Scaffold): as privacy rights and digital literacy mature, the suppression will decline. The authenticity mandate culture sees itself as enforcing coordination (public authenticity) while actually maintaining a piton (theater). The analytical observer risks naturalizing what is a socially contingent constraint as an inherent property of development.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity integrators: Victim + constrained → d≈0.62, f(d)≈0.85. Genuine need for privacy but constrained by suppression. Close relationships: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Experience minimal extraction; benefit from deeper authenticity. Identity experimenters (powerless): Victim + trapped → d≈0.93, f(d)≈1.40. Documented exploration becomes reputational liability with no exit option. Privacy movement (organized): d≈0.35, f(d)≈0.35. Organized actors with agency and exit paths; experience constraint as temporary and solvable. Authenticity culture (institutional): d≈0.55, f(d)≈0.75. Partly beneficiary (continuous disclosure generates engagement), partly victim (constrained by the mandate they enforce). Analytical observer: d≈0.72, f(d)≈1.15. Risks naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION ROUTE: Decompose the constraint into two distinct claims: (1) Psychological maturation requires private integration time (ε≈0.08, Mountain); (2) Contemporary surveillance capitalism and social judgment extract reputational costs from identity exploration (ε≈0.55, Snare). These are different constraints with different ε values. Claim 1 is an immutable property of development — all psychological perspectives agree. Claim 2 is a contingent institutional fact that varies with privacy technology, social norms, and employment law. The mandatrophy arises from conflating them: naturalizing the suppression cost (contingent) as inherent to development (immutable). The resolution: keep the closed-door protocol as a legitimate private practice (based on psychology) while decoupling it from the institutional suppression that makes privacy costly (through law, norm change, and digital privacy technology).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_window_duration,
    'What is the minimum duration for genuine identity integration before disclosure becomes sustainable without defensive rigidity?',
    'Longitudinal studies of identity disclosure timing correlated with psychological coherence, identity stability, and relationship quality outcomes; comparison of individuals who disclosed early vs. late vs. not at all',
    'If < 1 year: constraint is primarily extraction (social suppression of exploration). If 2-5 years: constraint reflects genuine developmental need. If > 10 years: constraint enables indefinite inauthenticity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_window_duration, empirical, 'Optimal duration for private identity integration before disclosure').

omega_variable(
    digital_permanence_ceiling,
    'Does the existence of digital permanence (permanent archives, screenshots, algorithmic memory) change the functional psychology of identity exploration, or merely the perceived risk?',
    'Comparative studies of identity disclosure patterns before/after social media adoption; analysis of psychological resilience to documented inconsistency in offline vs. online contexts; neuroimaging of threat response to permanent vs. ephemeral disclosure',
    'If perceived risk exceeds actual risk: suppression is primarily psychological (constraint can be addressed through cognitive reframing). If actual risk is proportionate: suppression is structural (constraint requires policy/technological change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_permanence_ceiling, empirical, 'Whether digital permanence has functional psychological impact or is primarily a perceived threat').

omega_variable(
    authenticity_mandate_incompatibility,
    'Are the social expectations for continuous public authenticity and genuine private identity integration structurally incompatible, or can they coexist with appropriate boundary management?',
    'Studies of boundary-management strategies (selective disclosure, context-collapse mitigation, privacy-preserving digital practices) and their effectiveness in enabling both public presence and private integration; ethnography of individuals who successfully maintain both',
    'If incompatible: constraint is driven by institutional contradiction requiring policy change. If compatible: constraint is primarily a skill/literacy issue requiring education.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_mandate_incompatibility, conceptual, 'Whether public authenticity and private integration are compatible or structurally conflicting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_identity_integration, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priv_identity_tr_t0, private_identity_integration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(priv_identity_tr_t2, private_identity_integration, theater_ratio, 2, 0.48).
narrative_ontology:measurement(priv_identity_tr_t5, private_identity_integration, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(priv_identity_be_t0, private_identity_integration, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(priv_identity_be_t2, private_identity_integration, base_extractiveness, 2, 0.31).
narrative_ontology:measurement(priv_identity_be_t5, private_identity_integration, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_identity_integration, global_infrastructure).
narrative_ontology:affects_constraint(private_identity_integration, authenticity_mandate_paradox).
narrative_ontology:affects_constraint(private_identity_integration, digital_permanence_reputational_extraction).
narrative_ontology:affects_constraint(private_identity_integration, therapeutic_privacy_asymmetry).

% DUAL FORMULATION NOTE:
% The closed-door identity protocol decomposes into distinct constraint families: (1) The developmental constraint (genuine need for private integration, low ε, mountain-adjacent); (2) The institutional suppression constraint (surveillance capitalism, social judgment, high ε, snare-adjacent). These have different resolution pathways: the developmental constraint is permanent and healthy; the institutional suppression constraint is contingent and should be minimized through privacy law and norm change. Upstream: emerges from digital permanence and authenticity mandates. Downstream: affects individual psychological coherence and institutional trust.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(private_identity_integration, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
