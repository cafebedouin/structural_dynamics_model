% ============================================================================
% CONSTRAINT STORY: surveillance_authority_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surveillance_authority_expansion, []).

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
 *   constraint_id: surveillance_authority_expansion
 *   human_readable: Surveillance Authority Expansion
 *   domain: political/security/governance
 *
 * SUMMARY:
 *   Surveillance authority expansion represents the state's systematic
 *   accumulation of information-gathering capacity over populations,
 *   justified through security narratives but functioning primarily as a
 *   mechanism of social control and power concentration. The constraint
 *   operates across all six DR types depending on the observer's structural
 *   position: the security state experiences it as coordination (rope);
 *   surveilled citizens experience it as extraction with no exit (snare);
 *   political dissidents experience it as extraction fused with identity loss
 *   (snare with identity_lock); compliant businesses experience mixed costs
 *   and benefits (tangled rope); privacy advocates see a temporary
 *   institutional imbalance with technical and legal exits (scaffold);
 *   obsolete legal frameworks persist through inertia (piton); and the
 *   civilizational observer risks naturalizing a contingent institutional
 *   choice as an immutable feature of governance (false mountain). The
 *   extractiveness trajectory (0.42 → 0.68 over the interval) reflects
 *   technological capacity accumulation, normalization of surveillance
 *   practice, and erosion of legal/technical barriers. The theater ratio
 *   (0.48 → 0.58) reflects increasing gap between legitimation narratives
 *   (security necessity, public safety) and actual functional surveillance
 *   use (minority surveillance targeting, political monitoring, social
 *   control).
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) — bears surveillance costs without consent or exit; informationally asymmetric
 *   - Political Dissidents: Victim with identity lock (powerless/identity_locked) — activism identity fused with public presence; vulnerability to selective enforcement
 *   - Marginalized Groups: Victims (powerless/trapped) — over-surveillance through algorithmic bias, law enforcement concentration; minimal exit options
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — accumulates information power, coordinates across agencies, enforces social norms
 *   - Law Enforcement Agencies: Beneficiary (institutional/arbitrage) — gains investigative capacity, access to population data, coordination infrastructure
 *   - Intelligence Services: Beneficiary (institutional/arbitrage) — expands operational scope, monitoring capacity, institutional reach
 *   - Compliant Businesses: Secondary actor (moderate/constrained) — participate in surveillance ecosystem; benefit from law enforcement coordination; bear compliance costs
 *   - Civil Liberties Coalition: Organized opposition (organized/constrained) — builds alternative technical and legal pathways; creates sunset mechanism through encryption, decentralization, privacy law
 *   - Legal Framework: Institutional artifact (institutional/arbitrage) — pre-digital law persists through inertia despite technical obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surveillance_authority_expansion, 0.68).
domain_priors:suppression_score(surveillance_authority_expansion, 0.72).
domain_priors:theater_ratio(surveillance_authority_expansion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surveillance_authority_expansion, extractiveness, 0.68).
narrative_ontology:constraint_metric(surveillance_authority_expansion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(surveillance_authority_expansion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surveillance_authority_expansion, snare).
narrative_ontology:human_readable(surveillance_authority_expansion, "Surveillance Authority Expansion").
narrative_ontology:topic_domain(surveillance_authority_expansion, "political/security/governance").

domain_priors:requires_active_enforcement(surveillance_authority_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(surveillance_authority_expansion, state_security_apparatus).
narrative_ontology:constraint_beneficiary(surveillance_authority_expansion, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(surveillance_authority_expansion, intelligence_services).
narrative_ontology:constraint_victim(surveillance_authority_expansion, general_population).
narrative_ontology:constraint_victim(surveillance_authority_expansion, marginalized_groups).
narrative_ontology:constraint_victim(surveillance_authority_expansion, political_dissidents).
narrative_ontology:constraint_victim(surveillance_authority_expansion, privacy_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Citizens have no meaningful exit option from state surveillance; they cannot opt out of the surveillance apparatus without leaving the nation-state. Suppression is extreme: data collection is legal, normalized, and pervasive. Extraction operates through behavioral chilling effects, loss of informational autonomy, and vulnerability to selective enforcement. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(surveillance_authority_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL DISSIDENT (SNARE, IDENTITY-LOCKED) — Activists and dissidents are both structurally trapped (exit would mean abandoning their identity and cause) and informationally vulnerable (surveillance enables selective prosecution). Their identity as agents of change is fused with their public presence, making privacy impossible. Suppression is extreme through both legal exposure and epistemic isolation.
constraint_indexing:constraint_classification(surveillance_authority_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY STATE (ROPE) — Law enforcement and intelligence agencies experience expanded surveillance authority as pure coordination: enabling information-sharing, threat detection, and institutional coordination across agencies. From their institutional position, surveillance solves collective action problems around national security coordination. Net beneficiary with maximal exit flexibility (arbitrage).
constraint_indexing:constraint_classification(surveillance_authority_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLIANT BUSINESS (TANGLED ROPE) — Corporations participate in surveillance ecosystems through data sharing agreements and compliance with legal demands. They benefit from law enforcement coordination and market stability, but bear costs of regulatory compliance and reputational risk. Extraction is asymmetric but not total — firms have some negotiating power and exit options (relocation, encryption adoption).
constraint_indexing:constraint_classification(surveillance_authority_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL LIBERTIES COALITION (SCAFFOLD) — Privacy advocates, encryption advocates, and digital rights organizations see surveillance expansion as a temporary institutional imbalance with a sunset clause: technological barriers to total surveillance (encryption, decentralization, anonymization protocols) and legal/political pushback create paths toward privacy restoration. The coalition has agency and sees structural exits. Theater ratio reflects that surveillance legitimation narratives (security necessity) may not persist as technical alternatives mature.
constraint_indexing:constraint_classification(surveillance_authority_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OUTDATED LEGAL FRAMEWORK (PITON) — Pre-digital surveillance law and constitutional protections (4th Amendment, reasonable expectation of privacy) have atrophied relative to technical surveillance capacity. Legal frameworks persist through institutional inertia, declaring protection while surveillance occurs openly. The theater is high: legal ritual (warrant requirements, oversight committees) continues while practical privacy has vanished. The framework is maintained because alternatives have not fully replaced it institutionally.
constraint_indexing:constraint_classification(surveillance_authority_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, surveillance is an inherent feature of complex governance: any large-scale coordination system requires information-gathering mechanisms, and technological advancement always increases surveillance capacity. This perspective sees expansion as an immutable consequence of technology and human organization. However, the structural data contradicts this — surveillance authority expansion is a contingent institutional choice, not a natural law. The engine's false summit detector identifies this as naturalization of a policy choice.
constraint_indexing:constraint_classification(surveillance_authority_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surveillance_authority_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(surveillance_authority_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(surveillance_authority_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(surveillance_authority_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(surveillance_authority_expansion, TR),
    TR >= 0.70.

:- end_tests(surveillance_authority_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state captures information advantage, enforcement selectivity, and behavioral control. The extraction increases over time as technological capacity accumulates (surveillance infrastructure becomes cheaper, more comprehensive, harder to avoid). The trajectory from 0.42 to 0.68 reflects that surveillance is not a single snapshot but an ongoing process of institutional power accumulation. Suppression (0.72): High. Citizens cannot exit surveillance without emigration (physical barrier); legal protections are eroded (institutional barrier); surveillance is normalized and expected (cognitive barrier). Suppression combines structural, legal, and internalized components. Theater ratio (0.58): Moderate-high but not extreme. Security justifications and warrant processes create performance of accountability, but the performance is increasingly transparent as a theater — actual surveillance operates beneath legal categories, algorithms operate outside oversight, and selective enforcement reveals underlying power asymmetries. The gap between legal legitimation and operational reality grows as technical capacity expands.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the security state (rope — sees coordination benefit) and surveilled citizens (snare — experiences pure extraction). The security apparatus experiences the same institutional mechanism as solving collective action problems (coordination between agencies, threat detection, social ordering). Surveilled citizens experience the same mechanism as asymmetric power extraction with no ability to exit, defend, or refuse. This gap is not based on different metrics or different values — it reflects different structural positions within the same constraint. The security state's arbitrage exit option (they can choose what data to collect, how to use it, when to surveil) is directly enabled by the trapped population's lack of exit (they cannot refuse participation). The tangled rope perspective (compliant businesses) represents the boundary: businesses benefit from law enforcement coordination but bear costs of surveillance demands and reputational risk. The scaffold perspective (civil liberties coalition) attempts to shift the structural position by lowering suppression and creating new exit options (encryption, privacy law, decentralization). The piton perspective reveals that legal frameworks are performing accountability while surveillance operates outside their reach. The false mountain perspective exposes the naturalization error: civilization requires surveillance is a political choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d is derived from structural position within the surveillance system. The general population and dissidents are full victims of the extraction mechanism (d ≈ 0.95, trapped) — high experienced extraction through behavioral chilling, informational vulnerability, and lack of exit. The security state is a full beneficiary (d ≈ 0.05, arbitrage) — low experienced extraction because they control the mechanism. Compliant businesses are symmetric (d ≈ 0.50, constrained) — they benefit from law enforcement coordination but bear compliance costs and reputational risk. The civil liberties coalition is a partial victim fighting the mechanism (d ≈ 0.60, constrained with organized power) — they bear costs of surveillance but have institutional resources to organize opposition and create alternative pathways. The legal framework sits outside the victim/beneficiary distinction; it is a piton artifact experiencing zero effective extraction because it is ceremonial. The analytical observer at the universal scope faces a false summit: if they treat surveillance as immutable (mountain), they have naturalized a contingent institutional choice and missed the constraint's actual structure. The engine's chi formula with these d values produces the perspectival gap: victim agents experience high χ; beneficiary agents experience low/negative χ; organized opposition experiences moderate χ with potential for change.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint is classified as snare (extractiveness > 0.70, suppression ≥ 0.60, χ ≥ 0.66 in victim perspectives), resolving potential confusion with tangled rope. The potential mandatrophy is: 'Does surveillance expansion also provide genuine coordination benefit that would elevate it to tangled rope?' The resolution: security apparatus coordination is a real coordination function (agencies share threat intelligence, coordinate investigations, maintain border security), BUT this coordination benefit accrues exclusively to the beneficiary (state/law enforcement) while extraction accrues exclusively to the victims (general population). This asymmetry is the defining feature of snare vs tangled rope. In tangled rope, both beneficiaries and victims derive net benefit from the coordination function despite extraction. In snare, the victims derive only costs. The surveillance constraint is pure snare because the general population does not benefit from surveillance-enabled law enforcement coordination — they experience only the extraction (behavioral chilling, vulnerability, informational asymmetry) without the coordination benefit. The coordination occurs within the security state; it does not extend to the surveilled. This is why the security state perspective sees rope (they benefit from coordination) while the population perspective sees snare (they experience only extraction). The mandatrophy is resolved by the beneficiary/victim declaration and the structural asymmetry in benefit distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_inevitability,
    'Is surveillance expansion driven by technological capability (inevitable given digital infrastructure) or by institutional choices to weaponize that capability?',
    'Comparative analysis of surveillance adoption rates across democracies with similar technical capacity but different legal frameworks; historical analysis of alternative design choices for digital infrastructure (privacy-by-design vs surveillance-by-design)',
    'If technology-driven: surveillance is architectural and nearly impossible to reverse. If choice-driven: expansion is reversible through institutional reform. This determines whether the constraint should be classified as mountain (immutable) or snare (extractive and contingent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_inevitability, empirical, 'Whether surveillance expansion is technology-driven or institutionally contingent').

omega_variable(
    collateral_security_benefits,
    'Do surveillance systems provide genuine security benefits that balance against privacy costs, or are security narratives post-hoc justifications for power accumulation?',
    'Analysis of crime-solving and threat-prevention attribution: what percentage of crimes prevented are attributable to mass surveillance vs targeted investigation? Cross-country comparison of security outcomes vs surveillance intensity.',
    'If genuine benefits: classification shifts toward tangled rope (mixed coordination and extraction). If security narratives are ex-post: classification remains snare (pure extraction rationalized as necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collateral_security_benefits, empirical, 'Whether surveillance systems provide genuine security benefits or justify extraction through security narratives').

omega_variable(
    identity_lock_mechanism,
    'For political dissidents, is the identity lock a matter of fused activist identity, or is it better modeled as constrained exit due to legal prosecution risk?',
    'Qualitative analysis of dissident narratives: do they describe inability to leave their cause (identity fusion) or inability to leave safely (legal barriers)? Do exiled dissidents report identity loss or relief?',
    'If identity lock: the dissident perspective models cognitive capture and internal binding. If legal constraint: reclassify exit_options as trapped. This affects the interpretation of how surveillance binds political agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether dissident entrapment is identity-based or legally/materially constrained').

omega_variable(
    privacy_restoration_feasibility,
    'Can surveillance expansion be reversed through technical (encryption, decentralization) or legal (privacy law, constitutional amendment) reforms, or are expansion mechanisms locked in by institutional path dependence?',
    'Historical case studies of surveillance rollback or constraint (post-GDPR enforcement, EU data protection, unsuccessful US privacy legislation); analysis of technical barriers to implementing strong privacy protections in existing digital infrastructure',
    'If restoration feasible: scaffold perspective is structurally sound with real sunset. If locked in: scaffold is aspirational and should downgrade to piton or snare. This determines the likelihood of the constraint''s own classification changing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_restoration_feasibility, empirical, 'Whether surveillance expansion can be reversed through technical or legal reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surveillance_authority_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, surveillance_authority_expansion, theater_ratio, 0, 0.48).
narrative_ontology:measurement(surv_tr_t5, surveillance_authority_expansion, theater_ratio, 5, 0.53).
narrative_ontology:measurement(surv_tr_t10, surveillance_authority_expansion, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, surveillance_authority_expansion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(surv_be_t5, surveillance_authority_expansion, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(surv_be_t10, surveillance_authority_expansion, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(surveillance_authority_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(surveillance_authority_expansion, privacy_erosion).
narrative_ontology:affects_constraint(surveillance_authority_expansion, policing_bias_amplification).
narrative_ontology:affects_constraint(surveillance_authority_expansion, political_dissent_suppression).

% DUAL FORMULATION NOTE:
% Surveillance authority expansion decomposes into multiple structurally distinct constraints: technical surveillance capacity (engineering/architecture), legal authority expansion (policy), institutional coordination benefits (security apparatus), and behavioral extraction through visibility. This story addresses the macro constraint (institutional power accumulation); downstream constraints address specific domain applications (predictive policing, border surveillance, dissent monitoring).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(surveillance_authority_expansion, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
