% ============================================================================
% CONSTRAINT STORY: biometric_surveillance_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biometric_surveillance_infrastructure, []).

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
 *   constraint_id: biometric_surveillance_infrastructure
 *   human_readable: Biometric Surveillance Infrastructure as Extractive Constraint
 *   domain: security/technology/governance
 *
 * SUMMARY:
 *   Biometric surveillance infrastructure represents a structural constraint
 *   where technological capability, state security interests, and corporate
 *   profit incentives converge to extract behavioral predictability and
 *   social control from surveilled populations. The constraint exhibits
 *   multiple structural functions depending on observer position: a genuine
 *   security coordination mechanism for state apparatus, a profit mechanism
 *   for technology vendors, a coordination-extraction hybrid for law
 *   enforcement, an identity-locked advocacy battlefield for privacy
 *   institutions, and pure extraction from the perspective of trapped
 *   surveilled citizens. The extractiveness has grown from 0.35 (early
 *   deployment, limited scope) to 0.68 (mature infrastructure, expanded
 *   mandate) over the 20-year interval, driven by scope creep and increasing
 *   integration across government systems. The theater ratio (0.58) reflects
 *   that significant enforcement effort goes into legitimacy maintenance —
 *   public reporting, privacy impact assessments, oversight frameworks — that
 *   have minimal functional impact on actual surveillance capability
 *   expansion. Counter-surveillance technical alternatives (decentralized
 *   identity, zero-knowledge proofs) represent a genuine sunset mechanism if
 *   adoption barriers can be overcome, but institutional and regulatory
 *   capture by surveillance vendors and security apparatus work to prevent
 *   transition.
 *
 * KEY AGENTS:
 *   - Surveilled Citizens: Primary victim (powerless/trapped) — mandatory biometric collection, no exit mechanism, categorical identification enabling behavioral tracking and profiling
 *   - Marginalized Communities: Secondary victim (powerless/trapped) — disproportionate targeting, higher false positive rates, compounding surveillance burden across systems, generational impact
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures security benefits and social control capacity, selectively deploys surveillance, upgrades capability over time
 *   - Surveillance Technology Vendors: Secondary beneficiary (powerful/arbitrage) — sustained revenue streams, capability escalation, global market arbitrage across jurisdictions
 *   - Law Enforcement Agencies: Hybrid actor (moderate/constrained, identity_locked) — genuine coordination function (criminal investigation) fused with extractive function (mass profiling); identity locked to surveillance capability
 *   - Privacy Advocacy Community: Secondary actor (moderate/identity_locked) — holds opposing identity position; institutional dependence on surveillance debate for relevance prevents seeing partial solutions
 *   - International Human Rights Institutions: Monitoring actor (institutional/constrained) — extensive documentation with minimal functional enforcement power; piton classification reflects degraded constraint mechanism
 *   - Technical Counter-Surveillance Communities: Organized alternatives (organized/constrained) — building decentralized identity systems as sunset mechanism; represent genuine exit path if adoption barriers overcome
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees information asymmetry as core extraction mechanism; surveillance enables epistemic closure preventing contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biometric_surveillance_infrastructure, 0.68).
domain_priors:suppression_score(biometric_surveillance_infrastructure, 0.72).
domain_priors:theater_ratio(biometric_surveillance_infrastructure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biometric_surveillance_infrastructure, extractiveness, 0.68).
narrative_ontology:constraint_metric(biometric_surveillance_infrastructure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biometric_surveillance_infrastructure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biometric_surveillance_infrastructure, snare).
narrative_ontology:human_readable(biometric_surveillance_infrastructure, "Biometric Surveillance Infrastructure as Extractive Constraint").
narrative_ontology:topic_domain(biometric_surveillance_infrastructure, "security/technology/governance").

domain_priors:requires_active_enforcement(biometric_surveillance_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biometric_surveillance_infrastructure, state_security_apparatus).
narrative_ontology:constraint_beneficiary(biometric_surveillance_infrastructure, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(biometric_surveillance_infrastructure, surveillance_technology_vendors).
narrative_ontology:constraint_victim(biometric_surveillance_infrastructure, surveilled_populations).
narrative_ontology:constraint_victim(biometric_surveillance_infrastructure, marginalized_communities).
narrative_ontology:constraint_victim(biometric_surveillance_infrastructure, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Trapped in nationwide biometric infrastructure with no realistic exit. Faces categorical identification, tracking, and behavioral modification without meaningful consent mechanism or opt-out. Suppression is structural: legal requirement to provide biometrics, criminal penalties for non-compliance, lack of alternative identity pathways. Maximum extraction — the agent bears full cost of surveillance while beneficiaries extract behavioral predictability and social control.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Face disproportionate biometric targeting, higher false positive rates in facial recognition, and compounding surveillance burden across immigration, criminal justice, and social services systems. Trapped by legal status, economic vulnerability, and systemic distrust. Generational impact: surveillance data creates permanent records affecting employment, housing, and opportunity. No exit mechanism at individual or collective level.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Experiences biometric infrastructure as coordination mechanism for legitimate security functions: criminal investigation, border control, counterterrorism. Benefits from standardized identification and rapid pattern matching. Extraction runs toward this agent — they capture the surveillance benefit without bearing the populace cost. Arbitrage exit: can selectively surveil or exempt specific groups, can upgrade or downgrade infrastructure deployment.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SURVEILLANCE TECHNOLOGY VENDORS (ROPE) — Pure beneficiary. Contracts with state security apparatus, law enforcement, border agencies create sustained revenue streams. Can arbitrage by offering systems to multiple jurisdictions and upgrading capabilities over time. Experience the infrastructure as coordination mechanism (solving state's security challenge), not as extraction.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LAW ENFORCEMENT AGENCIES (TANGLED ROPE) — Genuine coordination function: biometric tools enable faster suspect identification, reduce wrongful arrests (in theory), streamline investigations. But also participates in extractive function: escalating surveillance capability expands investigative scope beyond original mandate, enables profiling and social control. Constrained exit: institutional identity fused with surveillance tools; reducing surveillance capacity is seen as reducing investigative capability even when capacity exceeds functional necessity.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PRIVACY ADVOCACY COMMUNITY (TANGLED ROPE) — Structurally mobile (can raise awareness, lobby for regulation, build alternatives) but identity-locked through institutional and epistemic capture: the advocacy community's professional identity, funding, policy influence, and intellectual frameworks are constituted through opposition to surveillance. Exit would require abandoning their identity-invested position. They maintain genuine coordination function (holding government accountable) alongside extractive function (institutional dependence on surveillance debate for relevance). The identity lock prevents seeing that full regulatory abolition may not be their genuine preference.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL HUMAN RIGHTS INSTITUTIONS (PITON) — Monitoring and protesting biometric surveillance through reports, frameworks, and rhetoric, but institutional capacity to enforce or reverse deployment is degraded. Human rights mechanisms lack enforcement power against states; surveillance continues despite international condemnation. Theater ratio high: extensive documentation and advocacy with minimal functional impact on actual surveillance expansion. Maintained through institutional inertia and symbolic legitimacy rather than effective constraint.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: TECHNICAL COUNTER-SURVEILLANCE COMMUNITIES (SCAFFOLD) — Building alternative identity and authentication pathways (decentralized identity, cryptographic privacy tools, biometric spoofing resistance, privacy-enhancing technology) that provide sunset trajectory for centralized biometric infrastructure. Organized agents (technical collectives, privacy technologists) have agency and see an exit path: distributed identity systems and zero-knowledge proofs can satisfy security functions without centralized biometric extraction. Sunset mechanism: as decentralized alternatives mature and interoperability standards stabilize, dependency on centralized biometric systems declines.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, biometric surveillance represents a structural shift in state capacity toward comprehensive behavioral tracking with no theoretical exit for surveilled populations. The analytical perspective sees the true extraction: information asymmetry becomes institutional power asymmetry. Extraction mechanism is not coercion alone but epistemic closure — surveilled agents cannot know what data exists, how it is being used, or how to contest it. This is pure extraction with coordination function as cover story.
constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biometric_surveillance_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biometric_surveillance_infrastructure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biometric_surveillance_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biometric_surveillance_infrastructure, TR),
    TR >= 0.70.

:- end_tests(biometric_surveillance_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Biometric surveillance enables identification, tracking, and behavioral prediction with minimal compensation or autonomy for surveilled populations. The extraction grows over time as infrastructure matures and integrates across government systems (criminal justice, immigration, social services, financial). At interval T=0 (early deployment), extractiveness was lower (0.35) because scope was limited and integration was incomplete. At T=20 (mature deployment), extractiveness is high (0.68) because coverage is near-universal and data flows create permanent records affecting opportunity and autonomy. Suppression (0.72): Very high. Surveilled populations face legal mandates to provide biometrics, criminal penalties for non-compliance, no realistic opt-out, and no alternative identity pathways. Technical barriers (no spoofing without legal consequences) and legal barriers (identity documents require biometric registration) combine to create structural trapping. Theater ratio (0.58): Moderate-high. Significant institutional effort goes into legitimacy maintenance — privacy impact assessments, oversight committees, regulatory frameworks — but these have minimal functional impact on actual surveillance expansion. The theater is not as high as a pure piton (0.70+) because some genuine technical and procedural safeguards exist; but theater is substantial because public accountability mechanisms rarely prevent capability escalation. The trend shows theater increasing slightly (0.42→0.58) as surveillance becomes more politically contested, requiring more legitimacy work.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across positions. The state security apparatus and technology vendors see Rope — a coordination mechanism solving legitimate security problems with minimal coercion. Surveilled citizens see Snare — pure extraction with no escape and maximum suppression. Law enforcement agencies see Tangled Rope — genuine coordination function (criminal investigation) hybridized with extractive function (mass profiling and social control). Privacy advocates see a constraint requiring total abolition but are identity-locked to this position, preventing recognition of partial solutions (Tangled Rope from the advocate position could become Scaffold if sunset mechanisms like decentralized identity adoption could be credibly established). International human rights institutions see a Piton — surveillance expansion continues despite extensive condemnation, suggesting degraded constraint function maintained through inertia and legitimacy theater rather than actual power. Technical counter-surveillance communities see a Scaffold — decentralized identity systems represent a genuine sunset mechanism if adoption barriers can overcome institutional and regulatory capture. The analytical observer sees Snare with epistemic closure as the core extraction mechanism. The perspectival divergence reflects genuine structural differences: agent power level, exit options, and beneficiary/victim status create incommensurable experiences of the same infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d value) reflects their structural position in the extraction flow. Surveilled citizens are victims trapped in the system (d ≈ 0.95, maximum target): they cannot exit, bear full suppression, and experience all behavioral costs. State security apparatus are beneficiaries with arbitrage options (d ≈ 0.10, beneficiary): they choose deployment level, can upgrade or downgrade, and capture surveillance benefits. Marginalized communities are super-victims with even higher d (≈ 0.98) because they face disproportionate targeting and compounding surveillance across systems. Technology vendors benefit without bearing suppression (d ≈ 0.05, pure beneficiary): they profit from capability escalation without surveillance costs. Law enforcement agencies are moderate actors (d ≈ 0.55) with mixed coordination-extraction experience. Privacy advocates are identity-locked (d ≈ 0.80 but analytically visible as such through the identity_locked exit option): they are structurally mobile but cannot exercise exit because their identity is constituted through opposition to surveillance. The analytical observer (d ≈ 0.85) sees from the perspective of information asymmetry victims: epistemic closure prevents surveilled agents from knowing or contesting their surveillance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by decomposing biometric surveillance into structurally distinct components. The core constraint is Snare (extraction-dominant): mandatory biometric collection, behavioral tracking, predictive profiling of surveilled populations with no exit. This cannot be reframed as coordination without denying the surveilled agent's actual structural position. However, within biometric infrastructure, there exists a genuine subordinate coordination function — warrant-based criminal investigation — that could theoretically be separated from the extractive mass surveillance function. If this separation were architecturally feasible (end-to-end encryption preventing mass collection, warrant requirements enforced by technical design rather than oversight), biometric infrastructure could transition from Snare to Tangled Rope with extraction-suppressed coordination preserved. The mandatrophy blocks false natural law interpretation: biometric surveillance is not an immutable requirement of security governance; it is a contingent institutional choice to maximize state information access. Alternative architectures (decentralized identity, cryptographic authentication) could satisfy legitimate security functions without centralized extraction. The current snare classification is stable, but the possibility of architectural transition to tangled_rope (with further potential transition to scaffold if counter-surveillance alternatives mature) prevents naturalizing surveillance as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_positive_rate_threshold,
    'At what false positive rate does facial recognition become primarily an extraction mechanism rather than a security tool?',
    'Empirical measurement of false positive rates across demographic groups; correlation between false positive rate and wrongful detention/accusation rates; comparison of accuracy thresholds across deployed systems',
    'If FPR < 1%: coordination function may be genuine (security benefit outweighs cost). If FPR > 5%: extraction dominates (social cost of misidentification exceeds security benefit). Current deployed systems show 10-20% demographic variation, suggesting extraction-dominant regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_positive_rate_threshold, empirical, 'False positive rate threshold for security functionality').

omega_variable(
    scope_creep_inevitability,
    'Is surveillance scope expansion an accidental institutional drift or a structural feature of how surveillance systems develop?',
    'Historical analysis of purpose creep in deployed surveillance systems; comparison of original mandate vs actual surveillance scope across jurisdictions; study of institutional incentives for capability expansion',
    'If accidental: regulation and oversight can constrain scope. If structural: constraints cannot prevent expansion without system abolition. Evidence suggests structural inevitability — every biometric system shows scope creep within 5-10 years.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_creep_inevitability, empirical, 'Whether surveillance scope expansion is inevitable').

omega_variable(
    decentralized_authentication_sufficiency,
    'Can decentralized cryptographic identity systems replace state biometric infrastructure while maintaining necessary security and access control functions?',
    'Technical feasibility studies; pilot deployments of zero-knowledge proof identity systems; comparison of security properties and operational requirements',
    'If feasible: scaffold sunset is real and constraint can terminate. If infeasible: counter-surveillance alternatives cannot reduce dependency on centralized biometric systems. Current research suggests technical feasibility but adoption barriers remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_authentication_sufficiency, empirical, 'Whether decentralized authentication can replace centralized biometrics').

omega_variable(
    extraction_vs_coordination_decomposition,
    'Can genuine coordination functions (criminal investigation, border security) be separated from extractive functions (mass surveillance, social control, profiling)?',
    'Comparative analysis of targeted surveillance (warrant-based, specific investigation) vs mass surveillance (universal collection); measurement of harm reduction from mass surveillance vs targeted surveillance achieving same security outcomes',
    'If separable: some surveillance infrastructure could persist as rope (coordination) while eliminating snare (extraction). If inseparable: biometric infrastructure is inherently snare, and meaningful constraint reduction requires abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, conceptual, 'Separability of coordination and extraction functions in surveillance').

omega_variable(
    identity_lock_reversibility,
    'Can privacy advocacy institutions escape identity lock and recognize that some surveillance infrastructure reduction is achievable without total abolition?',
    'Cognitive interviews with privacy advocates; analysis of policy positions pre- and post-major technical breakthroughs; study of institutional willingness to accept partial solutions',
    'If reversible: advocacy can shift to designing constraints that preserve coordination while eliminating extraction. If irreversible: advocacy remains locked in all-or-nothing opposition, reducing practical policy impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, preference, 'Whether privacy advocacy identity lock is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biometric_surveillance_infrastructure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biom_tr_t0, biometric_surveillance_infrastructure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(biom_tr_t10, biometric_surveillance_infrastructure, theater_ratio, 10, 0.5).
narrative_ontology:measurement(biom_tr_t20, biometric_surveillance_infrastructure, theater_ratio, 20, 0.58).
narrative_ontology:measurement(biom_tr_t5, biometric_surveillance_infrastructure, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(biom_be_t0, biometric_surveillance_infrastructure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biom_be_t10, biometric_surveillance_infrastructure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(biom_be_t20, biometric_surveillance_infrastructure, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(biom_be_t5, biometric_surveillance_infrastructure, base_extractiveness, 5, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biometric_surveillance_infrastructure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biometric_surveillance_infrastructure, 0.12).
narrative_ontology:affects_constraint(biometric_surveillance_infrastructure, algorithmic_bias_criminal_justice).
narrative_ontology:affects_constraint(biometric_surveillance_infrastructure, informational_asymmetry_surveillance_state).
narrative_ontology:affects_constraint(biometric_surveillance_infrastructure, identity_documentation_state_dependency).

% DUAL FORMULATION NOTE:
% Biometric surveillance infrastructure decomposes into two structurally distinct constraints: (1) mass surveillance infrastructure (this story, Snare, ε=0.68) enabling behavioral tracking and social control extraction, and (2) targeted biometric investigation tools (separate story, potential Tangled Rope, ε=0.45) for warrant-based criminal investigation. The family relationship is upstream/downstream: mass surveillance infrastructure emerged from and justifies expansion of targeted investigation capability; targeted capability legitimizes the larger infrastructure. Stories linked via affects_constraints to enable contamination analysis — degradation of targeted system's integrity affects mass surveillance extraction mechanism, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biometric_surveillance_infrastructure, institutional, 0.08).
constraint_indexing:directionality_override(biometric_surveillance_infrastructure, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
