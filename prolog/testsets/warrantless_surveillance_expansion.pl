% ============================================================================
% CONSTRAINT STORY: warrantless_surveillance_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_warrantless_surveillance_expansion, []).

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
 *   constraint_id: warrantless_surveillance_expansion
 *   human_readable: Warrantless Surveillance Expansion as State Extraction Mechanism
 *   domain: governance/security/civil_liberties
 *
 * SUMMARY:
 *   Warrantless surveillance expansion represents one of the most
 *   structurally clear examples of state extraction using technological
 *   infrastructure as the enforcement mechanism. The constraint operates
 *   through mandatory digital participation: citizens cannot opt out of
 *   surveillance without abandoning modern communication, financial, and
 *   social participation. The surveillance apparatus extracts behavioral
 *   data, chilling effects on speech, identity modification under
 *   observation, and informational asymmetry (state knows citizen behavior;
 *   citizen knows surveillance exists but not implementation details). The
 *   constraint exhibits characteristics across all six classification types
 *   from different observer positions. The extractiveness trajectory (0.35 →
 *   0.68 over 15 years) reflects post-Snowden expansion despite public
 *   revelation and legal challenges. The theater ratio (0.45 → 0.68) shows
 *   increasing performative oversight (FISA court procedures, inspector
 *   general reports) that masks weak functional restrictions on actual
 *   collection. The suppression metric (0.75) reflects that citizens have no
 *   direct exit option (constrained communications infrastructure), no
 *   alternative digital infrastructure available at scale (network effects
 *   lock), and no functional legal remedy (courts rarely rule surveillance
 *   unconstitutional due to state secrets privilege and standing doctrine).
 *   The identity_locked exit option appears for younger cohorts whose social
 *   identity is constituted through platforms that require surveillance
 *   participation.
 *
 * KEY AGENTS:
 *   - General Population / Data Subjects: Primary victim (powerless/trapped) — mandatory digital participation enables warrantless data extraction with no consent mechanism and no escape route
 *   - State Security Apparatus (NSA, GCHQ, etc.): Primary beneficiary (institutional/arbitrage) — captures expanded intelligence capacity, threat detection advantages, and operational efficiency through warrantless access
 *   - Law Enforcement Agencies: Secondary beneficiary (institutional/constrained) — benefit from surveillance intelligence but face some legal/political constraints and liability concerns
 *   - Tech Platform Operators (Google, Facebook, Microsoft): Secondary beneficiary (powerful/constrained) — profit from behavioral data while complying with government access mandates; face regulatory compliance costs but gain intelligence byproducts
 *   - Civil Liberties Organizations (ACLU, EFF): Organized victim (organized/constrained) — mobilize resistance through litigation, legislation, and international advocacy; see surveillance restriction as achievable through policy reform
 *   - Younger Digital Cohorts: Victim with identity_locked exit (moderate/identity_locked) — surveillance transparency normalized in identity formation; exit requires abandoning social participation, not just paying privacy cost
 *   - Oversight Institutions (FISA courts, Congressional committees): Performative actors (institutional/arbitrage) — maintain legitimacy through audit procedures while functional enforcement remains weak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(warrantless_surveillance_expansion, 0.68).
domain_priors:suppression_score(warrantless_surveillance_expansion, 0.75).
domain_priors:theater_ratio(warrantless_surveillance_expansion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(warrantless_surveillance_expansion, extractiveness, 0.68).
narrative_ontology:constraint_metric(warrantless_surveillance_expansion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(warrantless_surveillance_expansion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(warrantless_surveillance_expansion, snare).
narrative_ontology:human_readable(warrantless_surveillance_expansion, "Warrantless Surveillance Expansion as State Extraction Mechanism").
narrative_ontology:topic_domain(warrantless_surveillance_expansion, "governance/security/civil_liberties").

domain_priors:requires_active_enforcement(warrantless_surveillance_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(warrantless_surveillance_expansion, state_security_apparatus).
narrative_ontology:constraint_beneficiary(warrantless_surveillance_expansion, law_enforcement_agencies).
narrative_ontology:constraint_victim(warrantless_surveillance_expansion, general_population_data_subjects).
narrative_ontology:constraint_victim(warrantless_surveillance_expansion, privacy_rights_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SURVEILLED CITIZEN (SNARE) — Trapped within a communication infrastructure that cannot be exited without abandoning modern life. Warrantless surveillance extracts intimate behavioral data with no meaningful consent mechanism, no escape route, and no alternative communication substrate. The citizen bears full extraction cost: loss of privacy, chilling effects on speech, behavioral modification under observation. Zero exit options — participation in society requires accepting digital surveillance as a baseline. This is the maximum experienced extraction from the least powerful position.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SECURITY ESTABLISHMENT (ROPE) — Experiences warrantless surveillance as a coordination mechanism for addressing genuine collective action problems: terrorism prevention, crime investigation, national defense. The constraint benefits these agents through expanded intelligence capacity, faster threat detection, and reduced operational friction. They have arbitrage options (exit to alternative security frameworks) and institutional power to shape constraints. From their perspective, the constraint solves coordination problems with minimal coercive overhead from their vantage point.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH PLATFORM OPERATORS (TANGLED ROPE) — Constrained by data retention and government access mandates but also benefit from surveillance infrastructure. Platforms have access to aggregate behavioral data for targeted advertising, recommendation systems, and user modeling. They face legal compliance costs (constrained exit) but gain valuable intelligence byproducts. The constraint coordinates data-sharing infrastructure while extracting differential value from platforms that serve as de facto intelligence collection agencies.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL LIBERTIES COALITION (SCAFFOLD) — Organized agents (ACLU, EFF, international human rights bodies) treat warrantless surveillance expansion as a temporary institutional failure with sunset potential. They see encryption mandates, warrant requirements, and end-to-end encryption as structural solutions that can eliminate the constraint entirely. Low effective extraction from this perspective because the coalition has agency, political organizing capacity, and sees a clear exit path through privacy-protective legislation. The constraint's lifespan is politically bounded.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-SNOWDEN LEGAL THEATER (PITON) — Surveillance oversight mechanisms (FISA courts, inspector generals, periodic audits) are substantially performative. FISA courts approve 99.7% of warrant requests. Inspector general reports document violations but rarely trigger prosecutions. Congressional briefings occur but oversight authority is limited. The theater persists through institutional inertia — the apparatus of oversight maintains legitimacy while actual constraint enforcement remains weak. Theater ratio reflects that formal oversight exists but functional restriction is minimal.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL NATIVE RESIGNED TO SURVEILLANCE (SNARE with identity_locked) — Generational cohorts for whom surveillance transparency is a taken-for-granted feature of digital life. Their identity is constituted through platforms and digital infrastructure that require surveillance participation. Exit would require abandoning not just the constraint but the social practices and identity framings built within it. They cannot imagine privacy-first digital life because their identity is fused with platforms that normalize observation. Structurally mobile (could theoretically exit) but identity-locked (perceive no exit because their sense of self depends on digital participation).
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a civilizational/universal perspective, some surveillance is inevitable in complex societies: large-scale coordination requires information sharing, threat detection requires behavioral observation, digital infrastructure inherently creates data traces. This perspective risks naturalizing contingent institutional arrangements (legal choice to enable warrantless surveillance) as immutable features of technological civilization. The classification is a false summit — the engine's structural analysis reveals this as naturalization of policy choices, not laws of nature.
constraint_indexing:constraint_classification(warrantless_surveillance_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(warrantless_surveillance_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(warrantless_surveillance_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(warrantless_surveillance_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(warrantless_surveillance_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(warrantless_surveillance_expansion, TR),
    TR >= 0.70.

:- end_tests(warrantless_surveillance_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state apparatus captures expansive behavioral data, location tracking, communication metadata, and content access through warrantless collection. The extraction is asymmetric: citizens cannot reciprocally surveil the state. The value asymmetry is extreme — the state gains strategic intelligence advantage; citizens lose privacy. The upward trajectory (0.35 → 0.68 over 15 years) reflects that despite post-Snowden revelation and public opposition, surveillance capacity has expanded rather than contracted: new collection programs, expanded internet backbone taps, cloud data access, and international surveillance cooperation have increased the state's extractive capacity. Suppression (0.75): High. Citizens face multiple barriers to exit. Technical barriers: digital infrastructure is built for data collection; encryption is sometimes prohibited or backdoored; alternative communication systems (mesh networks, decentralized protocols) lack scale and usability. Legal barriers: courts have rarely ruled warrantless surveillance unconstitutional; the state secrets privilege prevents disclosure of surveillance scope; standing doctrine prevents class-action privacy suits. Economic barriers: participation in digital economy requires accepting surveillance as a baseline condition. Political barriers: security framing (terrorism, crime) is invoked to override privacy concerns in public discourse; majority consent for surveillance expansion is manufactured through fear narratives. Theater ratio (0.68): Moderately high. Post-Snowden, the surveillance apparatus has invested heavily in performative oversight: FISA courts (99.7% approval rate — functionally rubber-stamp), inspector general offices (document violations but rarely trigger enforcement), congressional briefings (classify threat assessment, preventing real oversight), and privacy boards (advisory only, no enforcement power). The theater serves to legitimize surveillance while maintaining minimal functional restriction.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap exists between the powerless citizen (Snare, maximum extraction) and the security establishment (Rope, coordination). The security establishment genuinely experiences warrantless surveillance as solving coordination problems — terrorism prevention, crime investigation, national security. They are not lying about effectiveness (some terrorism is prevented through surveillance). But they underweight the extraction experienced by powerless agents who cannot exit and cannot consent. The gap is not reconcilable through better information — it reflects genuine asymmetry in experienced constraint. The security establishment would need to internalize the powerless perspective and accept that preventing terrorism does not justify unlimited extraction from citizens. The civil liberties coalition sees a Scaffold (temporary, with sunset) while the security establishment sees a permanent coordination need (Rope). This gap reflects different time horizons and different confidence in alternative solutions (privacy-protective threat detection). The identity-locked younger cohort sees no constraint at all (or sees it as necessary baseline), while the powerless adult with memory of pre-ubiquitous-surveillance sees a Snare. This gap reflects generational identity formation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is computed from structural position. The powerless citizen with trapped exit and victim status derives d ≈ 0.95 (maximum target), producing f(d) ≈ 1.42 and maximum experienced extraction chi. The state security apparatus with institutional power, arbitrage exit, and beneficiary status derives d ≈ 0.05 (maximum beneficiary), producing f(d) ≈ -0.12 and negative/low chi (they experience the constraint as beneficial). Tech platforms with powerful status, constrained exit (compliance mandates), and mixed beneficiary/victim relationship derive d ≈ 0.55 (symmetric), producing moderate chi. The civil liberties coalition with organized power, constrained exit, and victim-advocate relationship derive d ≈ 0.40 (low target), producing lower chi than powerless citizens because they have organizing capacity and exit knowledge. The identity-locked younger cohort with trapped structural barriers but identity-fused relationship derive d ≈ 0.89 (high target), producing chi ≈ 1.28 — higher than the rational trapped agent because their identity fusion prevents recognition of exit as possible. Suppression is structural (not scaled): citizens face genuine technical, legal, and economic barriers to exit regardless of position. Extractiveness is scaled by f(d) × σ(S): local scope (σ=0.8) would dampen chi; national scope (σ=1.0) produces baseline; global scope (σ=1.2) amplifies extraction if surveillance is internationally coordinated (Five Eyes intelligence sharing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how a pure extraction mechanism (Snare from the powerless perspective) can be legitimized as coordination (Rope from the security perspective) and as temporary (Scaffold from the civil liberties perspective). Mandatrophy asks: How do we distinguish real coordination from coordination rhetoric that disguises extraction? The data: threat prevention statistics improve post-surveillance-expansion, so the security establishment's coordination claim is not baseless. But the improvement rate plateaus after initial expansion, suggesting diminishing returns and increasing extraction-to-benefit ratio. The identity-locked perspective shows a younger generation internalizing surveillance as normal, suggesting successful cultural lock-in rather than real coordination. The performative oversight (piton perspective) reveals that formal constraint mechanisms have degraded — the theater persists but functional restriction has atrophied. Mandatrophy is resolved by observing that warrantless surveillance expansion exhibits the structural signature of extraction disguised as coordination: (1) asymmetric power (state vs citizen), (2) suppressed alternatives (no functional exit), (3) expanding beneficiary claims (terrorism, crime, cybersecurity, public health — threat scope expands to justify continued expansion), (4) performative oversight (procedures that maintain legitimacy without functional constraint), (5) identity capture (normalization in younger cohorts). These are the markers of an extractive constraint that has successfully captured coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_prevention_effectiveness,
    'What proportion of prevented terrorism/serious crime genuinely requires warrantless bulk surveillance versus targeted warrants?',
    'Empirical analysis of classified threat cases: comparison of intelligence from warrantless programs vs warrant-based investigation chains; attribution of specific prevented attacks to bulk versus targeted collection methods',
    'If warrantless collection prevents < 10% of threats beyond warrant-based capacity: constraint is pure extraction (Snare from all positions). If > 40%: constraint has genuine coordination function (shifts some perspectives to Tangled Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_prevention_effectiveness, empirical, 'Effectiveness of warrantless surveillance in threat prevention vs warrant-based methods').

omega_variable(
    encryption_backdoor_feasibility,
    'Can encryption backdoors or surveillance-compatible cryptography be designed without substantially weakening security for all users?',
    'Cryptographic analysis of proposed backdoor mechanisms; empirical testing of whether deliberate weaknesses remain exploitable only by state actors or leak to criminal/rival-state actors; longitudinal security incident tracking',
    'If backdoors cannot be designed safely: encryption prohibition becomes option-destroying, shifting the scaffold sunset to ''indefinite'' (constraint becomes Snare). If safe backdoors exist: compromise solutions become viable, potentially moving civil liberties coalition to acceptance (constraint shifts to Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encryption_backdoor_feasibility, empirical, 'Technical feasibility of surveillance-compatible encryption without universal security degradation').

omega_variable(
    identity_lock_brittle_threshold,
    'At what point do younger cohorts'' identity locks around digital platforms become reversible through policy intervention, or are they structurally irreversible?',
    'Cohort studies of young adults with restricted platform access (privacy-by-default interventions); measurement of psychological distress, identity reconstruction capability, and adoption of privacy-protective practices; cross-national comparison of strict vs permissive surveillance regimes'' effects on generational identity formation',
    'If identity locks are brittle (reversible by policy at young enough age): earlier intervention points exist; privacy-protective norms can be established before lock-in. If locks are structurally irreversible: the constraint becomes multigenerational and increasingly difficult to escape, shifting classification toward permanent Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_brittle_threshold, empirical, 'Reversibility of identity-lock to surveillance-normalized digital infrastructure').

omega_variable(
    surveillance_scope_boundary,
    'Does warrantless surveillance expansion inevitably reach all citizens and communications, or do technical/economic limits cap surveillance at some fraction of the population?',
    'Forensic analysis of actual surveillance capability: percentage of communications technically intercepted, data storage capacity, processing bandwidth for useful intelligence extraction; scaling analysis of expansion trajectories',
    'If boundary exists at < 80% coverage: some citizens retain practical privacy through statistical evasion (constraint is constrained-exit, not trapped). If universal coverage is technically feasible and economically affordable: all citizens face trapped exit, confirming maximum Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_scope_boundary, empirical, 'Technical and economic limits on surveillance expansion coverage').

omega_variable(
    international_regulatory_arbitrage,
    'Can privacy-protective jurisdictions (GDPR EU, data localization China) create sufficient economic pressure to force surveillance limitation in less-protective regimes?',
    'Trade analysis: economic penalties imposed by privacy-protective regions on non-compliant surveillance states; corporate relocation patterns in response to data protection requirements; longitudinal tracking of international data governance agreements',
    'If yes: external pressure creates sunset mechanism (scaffold logic). If no: surveillance expansion is isolated from economic consequences, confirming permanent extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_arbitrage, empirical, 'International regulatory leverage on warrantless surveillance policies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(warrantless_surveillance_expansion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsurv_tr_t0, warrantless_surveillance_expansion, theater_ratio, 0, 0.45).
narrative_ontology:measurement(wsurv_tr_t5, warrantless_surveillance_expansion, theater_ratio, 5, 0.58).
narrative_ontology:measurement(wsurv_tr_t10, warrantless_surveillance_expansion, theater_ratio, 10, 0.68).
narrative_ontology:measurement(wsurv_tr_t15, warrantless_surveillance_expansion, theater_ratio, 15, 0.75).

% Extraction over time
narrative_ontology:measurement(wsurv_be_t0, warrantless_surveillance_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wsurv_be_t5, warrantless_surveillance_expansion, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(wsurv_be_t10, warrantless_surveillance_expansion, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(wsurv_be_t15, warrantless_surveillance_expansion, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(warrantless_surveillance_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(warrantless_surveillance_expansion, encryption_prohibition).
narrative_ontology:affects_constraint(warrantless_surveillance_expansion, data_localization_mandate).
narrative_ontology:affects_constraint(warrantless_surveillance_expansion, platform_liability_expansion).
narrative_ontology:affects_constraint(warrantless_surveillance_expansion, end_to_end_encryption_adoption).

% DUAL FORMULATION NOTE:
% Warrantless surveillance expansion is upstream of encryption policy, platform liability, and data localization constraints. These downstream constraints represent institutional responses (some amplifying extraction, some opposing it). Encryption prohibition represents security establishment effort to prevent end-user exit; end-to-end encryption adoption represents civil liberties effort to make warrantless surveillance functionally impossible. Decomposition follows from observable-dependent ε: the core surveillance mechanism has ε ≈ 0.68; encryption policy has ε ≈ 0.55 (hybrid extraction-coordination); platform liability has ε ≈ 0.45 (regulation attempting to constrain extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
