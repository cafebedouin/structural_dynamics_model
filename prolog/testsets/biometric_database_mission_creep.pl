% ============================================================================
% CONSTRAINT STORY: biometric_database_mission_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biometric_database_mission_creep, []).

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
 *   constraint_id: biometric_database_mission_creep
 *   human_readable: Biometric Database Mission Creep
 *   domain: surveillance/governance/privacy
 *
 * SUMMARY:
 *   Biometric database mission creep represents a structural extraction
 *   mechanism embedded in the technical infrastructure of identity
 *   governance. A system built for a narrow, transparent purpose (fingerprint
 *   matching for criminal background checks, facial recognition for passport
 *   verification) expands its scope through regulatory reinterpretation,
 *   inter-agency data sharing, technical capability enhancement, and
 *   institutional inertia. Enrolled citizens cannot exit once their biometric
 *   data enters the system; scope expansion occurs without new consent; and
 *   the mechanisms of expansion are opaque to subjects. The constraint
 *   exhibits all six DR types from different positions, but the core
 *   mechanism is extraction: the state captures biological identity data,
 *   expands its use beyond stated limits, and suppresses awareness and
 *   recourse. The theater ratio (0.65) reflects that mission creep is partly
 *   hidden behind legitimate-sounding procedural safeguards (data protection
 *   impact assessments, access controls, oversight committees) that function
 *   primarily to legitimize the system rather than constrain it. The
 *   extractiveness trajectory (0.35 → 0.68 over 20 years) shows accelerating
 *   scope expansion as technical capabilities outpace regulatory lag.
 *
 * KEY AGENTS:
 *   - Enrolled Citizens: Primary victims (powerless/trapped) — cannot opt out once biometric data enters database; bear full cost of scope expansion without consent or recourse
 *   - Law Enforcement Agencies: Primary beneficiaries (institutional/arbitrage) — gain reliable identification capabilities and evolving forensic capacity; can extend operations without new procurement or legal authority
 *   - Privacy Advocacy Organizations: Secondary actors (moderate/constrained) — work both to limit expansion and to legitimize the system through regulation; face professional and political costs
 *   - Data Protection Authorities: Regulatory actors (organized/constrained) — build oversight frameworks but may experience capture; have exit options (reform regulation) but face institutional friction
 *   - International Privacy Coalition: Organized opposition (organized/constrained) — GDPR, international agreements, privacy-tech development; building alternative architectures with sunset timeline
 *   - System Operators (Government IT contractors, Police IT): Institutional maintenance (institutional/arbitrage) — profit from system operations; capture through procurement dependencies; degraded performance masked by inertia
 *   - Privacy Commons: Victim abstraction (powerless/trapped) — collective right to informational self-determination; cannot organize, cannot exit, depleted as scope expands
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biometric_database_mission_creep, 0.68).
domain_priors:suppression_score(biometric_database_mission_creep, 0.72).
domain_priors:theater_ratio(biometric_database_mission_creep, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biometric_database_mission_creep, extractiveness, 0.68).
narrative_ontology:constraint_metric(biometric_database_mission_creep, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biometric_database_mission_creep, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biometric_database_mission_creep, snare).
narrative_ontology:human_readable(biometric_database_mission_creep, "Biometric Database Mission Creep").
narrative_ontology:topic_domain(biometric_database_mission_creep, "surveillance/governance/privacy").

domain_priors:requires_active_enforcement(biometric_database_mission_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biometric_database_mission_creep, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(biometric_database_mission_creep, state_surveillance_apparatus).
narrative_ontology:constraint_victim(biometric_database_mission_creep, database_subjects).
narrative_ontology:constraint_victim(biometric_database_mission_creep, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENROLLED CITIZEN (SNARE) — Once biometric data (fingerprints, iris scans, facial geometry) enters the database for one stated purpose (driver's license, passport, criminal background check), the citizen cannot opt out. Scope expands beyond original justification through regulatory reinterpretation, cross-agency data sharing, and technical capability creep. Maximum suppression: citizens have no meaningful exit, no legal recourse against new uses, and no visibility into application expansion. The constraint operates through asymmetric information (they don't know what the data is used for) and irreversibility (enrollment cannot be reversed without abandoning essential civic functions).
constraint_indexing:constraint_classification(biometric_database_mission_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIVACY ADVOCATE (TANGLED ROPE) — Privacy advocacy groups experience both coordination function (establishing principles, pushing for transparency requirements, building civil society capacity) and extraction (their work legitimizes the system by making it appear regulated; their victories are often reversed or reinterpreted through regulatory capture). They have exit options (cease advocacy, relocate) but face high professional costs. The constraint has genuine coordination elements (data protection impact assessments, consent frameworks) alongside asymmetric extraction of their political labor.
constraint_indexing:constraint_classification(biometric_database_mission_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT AGENCIES (ROPE) — Agencies experience the database as pure coordination: it solves the collective action problem of reliable identification across jurisdictions. No single agency built the system, but all benefit from it. From this perspective, the constraint is a Rope — a cooperative mechanism with clear coordination benefits and minimal coercive overhead *from the agencies' point of view*. They can exit (jurisdictions have existed without centralized databases) but face high friction costs from competing agencies.
constraint_indexing:constraint_classification(biometric_database_mission_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL REGULATORY COALITION (SCAFFOLD) — GDPR, international privacy agreements, and regulatory harmonization efforts (EU, New Zealand, Canada) represent an organized response to mission creep with explicit sunset logic: data minimization requirements, purpose limitation, storage time caps, and right-to-deletion provisions are designed to limit scope expansion. From this perspective, the constraint is temporary — regulatory pressure is building alternative architectures (federated identity, zero-knowledge proofs, privacy-preserving analytics) that will phase out centralized biometric databases. High suppression tolerance is justified because organized agents see an exit path and declining timeline.
constraint_indexing:constraint_classification(biometric_database_mission_creep, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BIOMETRIC SYSTEM OPERATORS (PITON) — Organizations that built and maintain centralized biometric databases (government IT contractors, legacy police information systems) are locked into operations by institutional inertia. The systems perform their original function poorly (false match rates, demographic bias) and new functions inefficiently (constant reintegration with emerging capabilities), yet persist because replacing them is organizationally complex. Theater ratio reflects performative compliance work: regular security audits that don't prevent breaches, access controls that circumvent in practice, purpose limitation clauses that agencies reinterpret through administrative memo. The systems persist not because they work but because the alternative (decentralized verification) would disrupt established procurement and career paths.
constraint_indexing:constraint_classification(biometric_database_mission_creep, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PRIVACY COMMONS (SNARE) — The collective right to privacy, autonomous identity, and bodily integrity (biometric data is extracted without continuous consent) has no institutional advocate and no exit option. As biometric database scope expands (facial recognition networks, behavioral biometrics, continuous monitoring capabilities), the privacy commons is depleted — once the infrastructure exists, it cannot be uninvented, and privacy principles (contextual integrity, informational self-determination) become increasingly aspirational. The extraction is total and irreversible at this scope.
constraint_indexing:constraint_classification(biometric_database_mission_creep, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNICAL INEVITABILITY VIEW (MOUNTAIN) — From a universal analytical perspective, this perspective risks naturalizing mission creep as an inevitable consequence of surveillance technology: once the technical capacity to match biometric data across domains exists, mission creep becomes 'natural' — it will happen regardless of policy intent. However, this naturalizes what is structurally a political and organizational choice (scope expansion is not technically required; it reflects institutional incentives and power asymmetries). The engine will flag this as a false summit: the constraint is contingent on institutional arrangements, not inherent to the technology itself.
constraint_indexing:constraint_classification(biometric_database_mission_creep, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biometric_database_mission_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biometric_database_mission_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biometric_database_mission_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biometric_database_mission_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biometric_database_mission_creep, TR),
    TR >= 0.70.

:- end_tests(biometric_database_mission_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, increasing. The system extracts biological identity information without continuous consent, expands scope beyond stated justification (base 0.35 → final 0.68), and offers no meaningful compensation to subjects. The extraction is not total because some citizens have mobility options (emigration, civic avoidance) and some scope uses (law enforcement criminal matching) retain some legitimacy. Theater ratio (0.65): Moderate-high. Mission creep is partly hidden. Scope expansion occurs through administrative memo rather than explicit legal change; technical capability expansion appears as 'optimization' rather than power expansion; regulatory safeguards (consent requirements, audit logs, purpose limitation) are frequently bypassed in practice, suggesting their primary function is legitimacy rather than constraint. Suppression (0.72): High. Citizens cannot exit after enrollment; scope expansion decisions are not transparent; legal recourse is minimal (FOIA requests rarely reveal full use extent; constitutional privacy rights are jurisdictionally fragmented); and the irreversibility of biometric data creates permanent lock-in. The suppression is partly structural (technical: once data is collected, its deletion is organizationally difficult) and partly institutional (political: agencies resist transparency and constraint).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the enrolled citizen (Snare: total extraction, zero exit, maximum suppression) and law enforcement agencies (Rope: coordination benefit, minimal extraction from their position, low suppression of their agency). These are not alternative interpretations of ambiguous structure — they are opposing experiences of the same constraint, which is precisely why Snare classification is appropriate: the constraint's extraction depends on asymmetric visibility and asymmetric voice. The citizen does not know the scope of their data's use; the agency does. The citizen cannot change the rules; the agency can, through internal policy. The piton perspective (legacy system operators) reveals that some institutional actors experience the constraint as degraded rather than functional — they see the theater clearly, understand the mismatch between stated purpose and actual use, but lack alternatives because the system is deeply embedded in operations. The scaffold perspective reveals that organized resistance (international regulatory coalitions, privacy-tech development) is real and has concrete sunset logic: GDPR, biometric recognition bans, and decentralized identity architectures create alternative pathways. This gap between scaffold (temporary, solvable) and snare (permanent, inescapable) is the central strategic tension: do alternative architectures develop fast enough to prevent total lock-in?
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation applies the beneficiary/victim/exit framework. Enrolled citizens: victim status (bear costs of scope expansion) + trapped exit (cannot exit after enrollment without abandoning driver's license, passport, welfare access) → d = 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Law enforcement: beneficiary status (gain capability) + arbitrage exit (can exit through jurisdictional work-arounds or alternative databases) → d = 0.08 → f(d) ≈ -0.12 (negative experienced extraction, net benefit). The scope modifier σ(S) = 1.0 (national scope, standard modifier). Final effective extraction χ = ε × f(d) × σ(S). For the enrolled citizen: χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (nearly complete extraction as experienced). For law enforcement: χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (net subsidy as experienced). Privacy advocates: mixed victim/beneficiary status + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65. Experienced extraction: χ ≈ 0.68 × 0.65 × 1.0 ≈ 0.44 (moderate, reflecting coordination function alongside extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the risk of misclassifying the entire system as Rope (coordination benefit without extraction) because law enforcement genuinely benefits and genuinely solves legitimate identification problems. The snare classification is warranted because the extraction asymmetry is not incidental — it is structural to the constraint's operation. The system works as extraction precisely because subjects cannot see, consent to, or exit scope expansion. If all actors had transparent, equal voice in scope decisions, the constraint would collapse to Rope. Instead, opacity and power asymmetry are features, not bugs. The scaffold perspective offers an escape: decentralized architecture, purpose limitation with teeth, privacy-tech development, and regulatory harmony could convert this to temporary (Scaffold) or even eliminate it entirely. But the piton perspective warns that institutional inertia is powerful — legacy systems persist despite poor performance. The mandatrophy is resolved by showing that the six types are not competing claims about 'what the constraint really is' but rather different structural observations that together reveal the constraint's mechanism: it is extraction (Snare) for the powerless (victims), coordination (Rope) for the powerful (beneficiaries), degraded infrastructure (Piton) for institutional operators, and temporary (Scaffold) from the position of organized resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mission_creep_causation,
    'Is mission creep driven by deliberate institutional expansion or by technical capability diffusion and regulatory ambiguity?',
    'Documentary analysis of database scope changes: explicit policy decisions vs. incremental regulatory reinterpretation; timeline correlation between new legal authorities and new data uses',
    'If deliberate: classification remains Snare across all perspectives (intentional extraction). If capability-driven: classification shifts toward Tangled Rope (coordination failure mistaken for extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_creep_causation, empirical, 'Whether mission creep is deliberate expansion or regulatory drift').

omega_variable(
    alternative_identification_feasibility,
    'Can government functions (licensing, law enforcement, border control, welfare verification) operate without centralized biometric databases using decentralized or federated verification models?',
    'Technical feasibility studies; pilot projects in jurisdictions with lower centralization (Estonia''s blockchain-based identity, Switzerland''s federated approach); cost-benefit analysis of decentralized vs. centralized systems',
    'If feasible and cost-comparable: scaffold perspective confirmed — sunset from centralized architecture is real. If technically impossible or prohibitively expensive: scaffold becomes aspirational, and the constraint is closer to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_identification_feasibility, empirical, 'Feasibility of decentralized biometric verification').

omega_variable(
    scope_limitation_enforceability,
    'Do purpose limitation statutes, data minimization requirements, and access controls actually prevent scope expansion in practice, or are they performative?',
    'Empirical analysis of databases with explicit scope limitations (EU databases post-GDPR, Canadian PIPEDA-regulated systems): frequency of scope violations, enforcement outcomes, technical circumvention patterns',
    'If enforceable: scope expansion is contingent on weak governance (not inherent to the technology or the constraint). Regulatory reform can reduce extractiveness. If consistently circumvented: limitation clauses are theater, and the constraint is Snare regardless of formal rules.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_limitation_enforceability, empirical, 'Whether scope limitations are enforceable or performative').

omega_variable(
    demographic_bias_direction,
    'Does mission creep disproportionately target vulnerable populations (by design or by technical bias), or does it affect all enrolled citizens equally?',
    'Racial equity analysis of biometric database use rates across law enforcement jurisdictions; comparison of match rates, false positive costs, and expansion timelines by demographic group',
    'If targeting is real: suppression mechanism is differential (some groups face higher suppression than others), warranting higher overall suppression value. Snare classification strengthened. If equal effect: suppression is uniform, and the constraint might be closer to Tangled Rope for privileged groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_bias_direction, empirical, 'Whether mission creep targets vulnerable populations disproportionately').

omega_variable(
    regulatory_capture_extent,
    'To what extent are privacy advocacy organizations and data protection authorities captured by or dependent on the biometric system they regulate?',
    'Funding source analysis of advocacy groups and regulatory bodies; employment tracking (revolving door between industry, law enforcement, and oversight); policy outcomes favoring system operators vs. subjects',
    'If capture is extensive: tangled rope perspective''s coordination function is illusory — regulation legitimizes extraction without constraining it. Classification shifts toward Snare. If capture is limited: regulation has genuine force, and scaffold/tangled rope perspectives are structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture by biometric system operators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biometric_database_mission_creep, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biomet_tr_t0, biometric_database_mission_creep, theater_ratio, 0, 0.4).
narrative_ontology:measurement(biomet_tr_t10, biometric_database_mission_creep, theater_ratio, 10, 0.55).
narrative_ontology:measurement(biomet_tr_t20, biometric_database_mission_creep, theater_ratio, 20, 0.65).
narrative_ontology:measurement(biomet_tr_t5, biometric_database_mission_creep, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(biomet_be_t0, biometric_database_mission_creep, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biomet_be_t10, biometric_database_mission_creep, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(biomet_be_t20, biometric_database_mission_creep, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(biomet_be_t5, biometric_database_mission_creep, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biometric_database_mission_creep, enforcement_mechanism).
narrative_ontology:affects_constraint(biometric_database_mission_creep, facial_recognition_scope_drift).
narrative_ontology:affects_constraint(biometric_database_mission_creep, behavioral_biometric_expansion).
narrative_ontology:affects_constraint(biometric_database_mission_creep, cross_border_data_sharing_asymmetry).

% DUAL FORMULATION NOTE:
% Biometric database mission creep is part of a constraint family covering different biometric modalities and data-sharing regimes. Facial recognition scope drift has higher theater (0.75) and lower baseline extractiveness (0.45) because the technology is newer and opacity is more visible. Behavioral biometric expansion has higher extractiveness (0.78) because the technical capability to track and profile continuously is less transparent than identity matching. Cross-border sharing has higher suppression (0.80) due to jurisdictional fragmentation making legal recourse nearly impossible. All three are downstream of the core database infrastructure constraint and share common beneficiaries (law enforcement, state surveillance) and victims (enrolled populations). They are linked via network.affects_constraints to show structural dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
