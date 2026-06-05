% ============================================================================
% CONSTRAINT STORY: digital_identity_tether
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_identity_tether, []).

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
 *   constraint_id: digital_identity_tether
 *   human_readable: The Centralized Identity Anchor
 *   domain: technological/digital_identity/platform_governance
 *
 * SUMMARY:
 *   The centralized identity anchor represents a structural constraint on
 *   digital autonomy, where individuals, applications, and service providers
 *   become increasingly dependent on a single identity provider for
 *   authentication, reputation, and social connectivity. This constraint
 *   exhibits the snare classification from multiple perspectives (individual
 *   users, dependent services) with alternative institutional pathways
 *   visible to organized actors (standards bodies, regulators). The
 *   constraint emerged from genuine coordination needs (centralized
 *   authentication solved real interoperability problems) but has evolved
 *   into rent-seeking extraction through lock-in: users cannot port their
 *   social graph, applications cannot authenticate against alternative
 *   identity standards without rebuilding user bases, and the identity
 *   provider captures value from data monopoly, profiling, and gatekeeping.
 *   The theatrical dimension (theater_ratio=0.61) reflects the framing of
 *   centralized identity as necessary for security and trust when distributed
 *   alternatives exist but are deliberately underdeveloped. The
 *   extractiveness has increased over the measurement interval (0.32 → 0.58)
 *   as social graph lock-in deepened and authentication dependency hardened.
 *   Theater ratio increase (0.38 → 0.61) reflects intensifying security/trust
 *   rhetoric masking data harvesting and profiling goals.
 *
 * KEY AGENTS:
 *   - Digital Subjects (Individuals): Primary victims (powerless/trapped) — cannot exit without abandoning years of accumulated social graph, reputation, and credentials. Exit cost approaches 100% of accumulated identity capital.
 *   - Service Ecosystem (Apps/Web Services): Secondary victims (moderate/constrained) — dependent on identity anchor for user authentication; switching providers requires rebuilding user bases and credential trust.
 *   - Identity Anchor Provider (Meta, Google, Apple, Microsoft): Primary beneficiary (institutional/arbitrage) — captures data monopoly, user profiling, gatekeeping control, and network effects lock-in. Experiences constraint as coordination mechanism, not extraction.
 *   - Decentralized Identity Coalition (W3C, standards bodies, blockchain projects): Organized actors (organized/constrained) — attempting to build interoperable identity standards as alternative pathways; see both coordination value and need to break extraction lock-in.
 *   - Regulatory Bodies (EU, NIST, national identity authorities): Organized enforcers (organized/constrained) — deploying portability mandates (GDPR, eIDAS) as sunset mechanisms to force technical interoperability over 5-10 year timescales.
 *   - Historical Federated Systems (Institutional LDAP, Shibboleth): Analytical reference point — represent earlier coordination mechanisms with lower centralization; highlight path-dependent nature of constraint rather than technical inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_identity_tether, 0.58).
domain_priors:suppression_score(digital_identity_tether, 0.72).
domain_priors:theater_ratio(digital_identity_tether, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_identity_tether, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_identity_tether, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_identity_tether, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_identity_tether, snare).
narrative_ontology:human_readable(digital_identity_tether, "The Centralized Identity Anchor").
narrative_ontology:topic_domain(digital_identity_tether, "technological/digital_identity/platform_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_identity_tether, identity_anchor_provider).
narrative_ontology:constraint_victim(digital_identity_tether, digital_subjects).
narrative_ontology:constraint_victim(digital_identity_tether, service_ecosystem).
narrative_ontology:constraint_victim(digital_identity_tether, identity_portability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGITAL SUBJECT / INDIVIDUAL USER (SNARE) — Trapped by social graph lock-in, reputation accumulation tied to single platform, and authentication dependency. Cannot exit without abandoning years of accumulated identity context, social connections, and credential history. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(digital_identity_tether, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SERVICE ECOSYSTEM / DEPENDENT APPLICATIONS (SNARE) — Apps and services built on the identity anchor have constrained exit: switching to alternative identity providers requires rebuilding user bases and authenticating against new standards. d≈0.78, f(d)≈1.18, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(digital_identity_tether, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: IDENTITY ANCHOR PROVIDER / PLATFORM (ROPE) — Benefits from first-mover lock-in, network effects, and data monopoly. Experiences the constraint as a coordination mechanism: providing centralized authentication solves a genuine coordination problem (users need one credential store; services need unified identity). d≈0.10, f(d)≈0.03, σ=1.2 → χ≈0.02. Net beneficiary; effective extraction minimal from this perspective.
constraint_indexing:constraint_classification(digital_identity_tether, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED IDENTITY COALITION / STANDARDS BODIES (TANGLED ROPE) — Organized actors (W3C, blockchain identity projects, OAuth alternative consortia) seek to build coordination mechanisms (open identity standards, portable credentials) while breaking the extractor's lock-in. They see both genuine coordination value (interoperability) and the need to redistribute extraction. d≈0.52, f(d)≈0.67, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / HISTORICAL VIEW (PITON) — The federated identity model (pre-2010s: institutional LDAP, Shibboleth, federated login) solved the coordination problem with lower centralization. The centralized anchor is not inherent to digital identity but a path-dependent result of platform consolidation. From a civilizational view, this constraint represents the degradation of a more distributed coordination mechanism into rent-seeking centralization. theater_ratio=0.61 reflects that much 'identity verification' rhetoric masks data harvesting and profiling goals. The centralized model persists through network effects inertia, not technical necessity.
constraint_indexing:constraint_classification(digital_identity_tether, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY / STANDARDS PATHWAY (SCAFFOLD) — EU's GDPR and emerging digital identity regulations (eIDAS, NIST frameworks) create temporary enforcement mechanisms to break the anchor's extraction power through data portability mandates, API requirements, and interoperability standards. These are sunset mechanisms: if successful, they reduce the anchor's lock-in asymmetry over 5-10 years. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.34.
constraint_indexing:constraint_classification(digital_identity_tether, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_identity_tether_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_identity_tether, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_identity_tether, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_identity_tether, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_identity_tether, TR),
    TR >= 0.70.

:- end_tests(digital_identity_tether_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The identity anchor extracts through multiple mechanisms: (1) data monopoly on user behavior, preferences, social graph; (2) gatekeeping on service access and third-party integrations; (3) profiling for targeted advertising; (4) forced upgrade to proprietary authentication schemes (biometric lock-in, device OS coupling). However, the extraction is not absolute (0.90+) because regulated jurisdictions are forcing some portability, and alternative identity providers exist for users with high exit cost tolerance. Suppression (0.72): High. Significant barriers to exit: (a) social graph lock-in (no platform provides equivalent status transfer); (b) authentication dependency (services integrate deeply into anchor's API); (c) credentialing lock-in (years of accumulated reputation context); (d) coordinated network effects (everyone else still uses the anchor); (e) information asymmetry (users unaware of alternatives). Theater ratio (0.61): Moderate. The identity anchor invokes security, trust, and verification language extensively, but much of the verification apparatus is performative: two-factor authentication is framed as trust mechanism but also increases lock-in through device coupling; biometric verification is framed as security but primarily serves profiling and account recovery lock-in. Genuine trust infrastructure exists but is dramatically overscaled relative to the stated security function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival gaps. Individual users (powerless/trapped) experience pure snare: they cannot exit. The identity anchor (institutional/arbitrage) experiences rope: providing centralized authentication genuinely solves coordination problems. Standards bodies and regulators (organized/constrained) experience tangled rope or scaffold: they see both the coordination value and the extraction, and they possess agency to build alternatives. The historical analytical perspective (piton) reveals that centralization is not technically inevitable — federated models existed and worked; centralization reflects path-dependent platform consolidation, not natural law. The regulatory pathway perspective (scaffold) sees a genuine sunset: GDPR portability rights, eIDAS frameworks, and emerging open identity standards could materially reduce lock-in over 5-10 years if enforcement accelerates.
 *
 * DIRECTIONALITY LOGIC:
 *   Digital subjects: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction direction; users have zero meaningful exit options relative to this specific constraint. Service ecosystem: Victim + constrained → d≈0.78, f(d)≈1.18. High extraction direction; services depend on anchor's authentication but can theoretically rebuild on alternatives at significant cost. Identity anchor provider: Beneficiary + arbitrage → d≈0.10, f(d)≈0.03. Minimal extraction direction from beneficiary's perspective; they experience the constraint as beneficial coordination. Decentralized coalition: Mixed + constrained → d≈0.52, f(d)≈0.67. Moderate extraction direction; coalition sees extraction but has technical and organizational agency to counter it. Regulatory bodies: Enforcer + constrained → d≈0.45, f(d)≈0.48. Moderate direction; regulators intervene from outside the lock-in but face compliance lag and technical complexity. Historical perspective: analytical → d≈0.75, f(d)≈1.15. Medium-high direction; reveals contingency and path dependency rather than natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The centralized identity anchor avoids mandatrophy by maintaining distinct beneficiary (anchor provider) and victim (digital subjects, dependent services) groups with no ambiguity about who extracts and who bears costs. The snare classification is unambiguous from the primary target perspective (powerless/trapped users experience maximum extraction). However, the mandatrophy surfaces in the institutional perspective: the anchor provider genuinely solves a coordination problem (centralized authentication), so their experience is rope, not snare. The resolution is perspectival: all six types are valid readings from different structural positions. The constraint is snare for users, rope for the provider, piton for the historical analyst (revealing degraded federation), and scaffold for the regulator (deploying enforcement mechanisms with sunset clauses). The mandatrophy dissolves when the analytical frame shifts from 'what is the true type?' to 'how does this constraint manifest differently across institutional positions?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_threshold_feasibility,
    'What technical/social cost of identity portability would eliminate the snare classification? At what friction level does ''trapped'' become ''constrained'' or ''mobile''?',
    'Empirical measurement of users successfully migrating across identity providers with varying friction costs; correlation of friction levels to lock-in persistence',
    'If friction can be reduced below ~5% transaction cost: snare degrades to tangled rope or rope. If friction < 2%: potentially mobile exit. If friction > 20%: snare classification hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portability_threshold_feasibility, empirical, 'Technical/social cost threshold for effective identity portability').

omega_variable(
    reputation_transferability_versus_identity,
    'Can reputation/social graph be ported separately from authentication identity, or is the lock-in fundamentally about reputation context rather than credential mechanics?',
    'Case studies of identity providers that succeeded in decoupling reputation (Twitter/X reputation portable to Bluesky) vs authentication (Google Sign-In lock-in); structural analysis of what aspects are technically portable vs socially sticky',
    'If reputation is fundamentally non-portable: snare classification holds even if authentication decouples. If reputation portable: exit options upgrade significantly; snare may degrade to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputation_transferability_versus_identity, conceptual, 'Whether reputation is separately portable from authentication identity').

omega_variable(
    regulatory_mandate_enforceability,
    'Can regulatory portability mandates (GDPR, eIDAS) actually force technical interoperability at the speed required to establish genuine exit options, or is regulatory lag a structural feature that perpetuates the lock-in indefinitely?',
    'Temporal analysis of regulatory mandate → platform compliance timelines; measurement of actual user migration rates post-mandate; comparison of enforcement speed across jurisdictions',
    'If enforceable in < 3 years: scaffold perspective valid; sunset mechanism real. If compliance takes 10+ years or platforms evade through technical barriers: scaffold is aspirational; snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_enforceability, empirical, 'Whether regulatory portability mandates can enforce genuine interoperability').

omega_variable(
    network_effects_asymmetry_hardness,
    'Is the lock-in driven by unavoidable network effects (mathematics of graph connectivity) or contingent path dependency (historical platform choices)? Can a new coordinated migration break the anchor without waiting for regulatory enforcement?',
    'Historical case analysis (WhatsApp vs Signal, Twitter vs Bluesky); network effects modeling with migration thresholds; behavioral economics of coordination incentives',
    'If mathematical lock-in: snare is hardened; only regulatory or technological revolution breaks it. If path-dependent: organized coalition could force rapid migration with coordination (reducing effectiveness of anchor''s extraction power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_asymmetry_hardness, empirical, 'Whether network effects lock-in is mathematically unavoidable or path-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_identity_tether, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(didt_tr_t0, digital_identity_tether, theater_ratio, 0, 0.38).
narrative_ontology:measurement(didt_tr_t5, digital_identity_tether, theater_ratio, 5, 0.5).
narrative_ontology:measurement(didt_tr_t10, digital_identity_tether, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(didt_be_t0, digital_identity_tether, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(didt_be_t5, digital_identity_tether, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(didt_be_t10, digital_identity_tether, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_identity_tether, information_standard).
narrative_ontology:affects_constraint(digital_identity_tether, authentication_dependency_monoculture).
narrative_ontology:affects_constraint(digital_identity_tether, data_portability_friction).
narrative_ontology:affects_constraint(digital_identity_tether, social_graph_lock_in).

% DUAL FORMULATION NOTE:
% The centralized identity anchor represents a cluster of structurally distinct but coupled constraints. The anchor is downstream of authentication infrastructure choices and upstream of data portability friction and social graph lock-in. Each sibling constraint has its own ε reflecting specific aspects of the lock-in (authentication mechanics, data export costs, social context transfer costs). The anchor story (ε=0.58) captures the aggregate extraction across all coupled mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_identity_tether, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
