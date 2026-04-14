% ============================================================================
% CONSTRAINT STORY: digital_preservation_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_preservation_standardization, []).

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
 *   constraint_id: digital_preservation_standardization
 *   human_readable: Digital Preservation Standardization Regime
 *   domain: cultural_heritage/information_systems
 *
 * SUMMARY:
 *   Digital preservation standardization emerged in the 1990s-2000s as a
 *   genuine coordination response to the challenge of maintaining digital
 *   materials across technical obsolescence, format migration, and
 *   organizational change. OAIS (Open Archival Information System), MIAOU,
 *   and subsequent standards provided frameworks for describing preservation
 *   metadata, storage architectures, and integrity checking. However, over
 *   the past 15 years, the standardization regime has accumulated extraction
 *   mechanisms: compliance audits that measure theater rather than
 *   preservation success, vendor lock-in through proprietary implementations
 *   of open standards, vendor consolidation that reduces choice, and
 *   perpetual standard upgrades that force institutional rearchitecture.
 *   Simultaneously, decentralized alternatives have emerged (federated
 *   storage, content-addressed archives, community-operated preservation
 *   networks) that challenge the assumption that standardization is
 *   structurally necessary. The constraint now exhibits a hybrid character:
 *   genuine coordination value (institutions do benefit from interoperability
 *   and shared tooling) layered with asymmetric extraction (standards
 *   designed to advantage large institutional repositories and commercial
 *   vendors while imposing disproportionate compliance burden on small and
 *   community-operated archives). Theater ratio has risen from 0.35
 *   (functional preservation focus) to 0.58 (compliance-audit focus) over 14
 *   years, indicating that the regime has drifted from solving preservation
 *   problems toward maintaining the standardization infrastructure itself.
 *
 * KEY AGENTS:
 *   - Standards Bodies (institutional/arbitrage): Set preservation standards; primary beneficiaries of regime; design standards that advantage their own infrastructure
 *   - Large Institutional Repositories (institutional/arbitrage): Benefit from interoperability and shared development costs; have capacity to implement standards; can shift between standards if needed
 *   - Commercial Preservation Vendors (powerful/arbitrage): Provide technical capacity; extract through proprietary implementations and vendor lock-in; genuine coordination function but also genuine extraction
 *   - Mid-Scale Cultural Institutions (moderate/constrained): Benefit from standardization but face expensive compliance; constrained by dependency on preserved materials; cannot easily exit
 *   - Small Community Archives (powerless/trapped): Lack technical and financial capacity; forced to comply with standards designed by and for larger institutions; trapped by absence of alternative preservation pathways
 *   - Marginalized Cultural Producers (powerless/identity_locked): Culturally specific archival practices may not map onto standardized preservation categories; identity-fused with non-standardized preservation methods; standards implicitly encode dominant-culture preservation practices
 *   - Open Preservation Movement (organized/mobile): Internet Archive, NDIIPP, community digitization projects; see standardization as temporary problem with exit pathways; building decentralized alternatives
 *   - Analytical Observer: Risks naturalizing contingent institutional standardization as immutable requirement for preserving digital materials
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_preservation_standardization, 0.38).
domain_priors:suppression_score(digital_preservation_standardization, 0.42).
domain_priors:theater_ratio(digital_preservation_standardization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_preservation_standardization, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_preservation_standardization, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_preservation_standardization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_preservation_standardization, tangled_rope).
narrative_ontology:human_readable(digital_preservation_standardization, "Digital Preservation Standardization Regime").
narrative_ontology:topic_domain(digital_preservation_standardization, "cultural_heritage/information_systems").

domain_priors:requires_active_enforcement(digital_preservation_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_preservation_standardization, standards_bodies).
narrative_ontology:constraint_beneficiary(digital_preservation_standardization, institutional_repositories).
narrative_ontology:constraint_beneficiary(digital_preservation_standardization, commercial_preservation_vendors).
narrative_ontology:constraint_victim(digital_preservation_standardization, small_archives).
narrative_ontology:constraint_victim(digital_preservation_standardization, community_collections).
narrative_ontology:constraint_victim(digital_preservation_standardization, marginalized_cultural_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL COMMUNITY ARCHIVES (SNARE) — Trapped by lack of technical capacity and funding. Forced to adopt standards they did not design and cannot modify. No alternative preservation pathways available. Maximum extraction: comply with expensive standards or lose digital materials to bit rot. No coordination benefit — the standards exist to serve institutional repositories, not to solve community-level problems.
constraint_indexing:constraint_classification(digital_preservation_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SCALE CULTURAL INSTITUTIONS (TANGLED ROPE) — Genuinely benefit from standardization: interoperability with other institutions, shared tooling, reduced reinvention cost. BUT face significant extraction: expensive compliance audits, vendor lock-in through proprietary implementations of standards, forced upgrades. Exit is constrained by dependency on preserved materials and institutional reputation — cannot abandon preservation without losing collections.
constraint_indexing:constraint_classification(digital_preservation_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STANDARDS BODIES AND LARGE REPOSITORIES (ROPE) — Primary beneficiaries. Set standards that advantage their own technical infrastructure. Experience the constraint as pure coordination: standardization enables interoperability that benefits them directly. Arbitrage option allows them to shift between standards if needed. Net positive extraction flow.
constraint_indexing:constraint_classification(digital_preservation_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN PRESERVATION MOVEMENT (SCAFFOLD) — Organized agents (NDIIPP, Internet Archive, community digitization projects) see standardization as a temporary problem with a sunset: open-source preservation tools, decentralized storage, and lightweight alternative standards are creating exit pathways. Theater low because this perspective values functional redundancy over compliance theater. Sunset mechanism: maturation of federated, non-commercial preservation infrastructure in 15-25 years.
constraint_indexing:constraint_classification(digital_preservation_standardization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY STANDARDS COMMITTEES (PITON) — Obsolete standards (OAIS, MIAOU) persist through institutional inertia despite being superseded by practice. Committees maintain standards that nobody actually implements fully, generating compliance theater. Low extractiveness because the standards have lost functional force, but high theater because institutional actors continue performing compliance.
constraint_indexing:constraint_classification(digital_preservation_standardization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMMERCIAL PRESERVATION VENDORS (TANGLED ROPE) — Provide genuine coordination function: technical capacity that cash-poor institutions cannot develop internally. BUT extract through proprietary implementations, vendor lock-in, and perpetual licensing. Arbitrage option means they can shift to other market sectors; extraction is profitable but not total. They benefit from standardization because it creates compliance demand.
constraint_indexing:constraint_classification(digital_preservation_standardization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, standardization appears immutable: coordinating diverse preservation systems requires shared formats and protocols; this is an unavoidable constraint on any complex information system. However, the base properties contradict the mountain classification — the constraint exhibits suppression (0.42), theater (0.58), and active enforcement, all of which point to institutional contingency rather than natural law. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(digital_preservation_standardization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_preservation_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_preservation_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_preservation_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_preservation_standardization, TR),
    TR >= 0.70.

:- end_tests(digital_preservation_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint combines genuine coordination benefits (interoperability, shared tools, reduced reinvention) with real extraction mechanisms (vendor lock-in, compliance theater, design bias toward large institutions). The intermediate value reflects that beneficiaries experience net positive flow while victims experience net negative flow. Extractiveness has risen over the interval (0.22 → 0.38) as vendor consolidation and compliance bureaucracy have grown relative to functional preservation benefits. Suppression (0.42): Moderate. Barriers to independent preservation include technical expertise requirements, storage infrastructure costs, and implicit assumption that preservation requires compliance with formal standards. However, suppression is not total — some communities operate effective alternative preservation systems (community digitization projects, federated storage, lightweight standards). Suppression is structural but not inevitable. Theater ratio (0.58): High and rising. Much institutional preservation labor goes to compliance documentation, audit preparation, and format conformance verification — activities that measure standardization adherence rather than actual preservation success (bit integrity, access longevity, discovery effectiveness). The rise from 0.35 to 0.58 indicates drift from solving preservation problems toward performing compliance with the standardization regime. Standard implementations are increasingly complex, generating professional specialization (preservation engineers) that depends on the regime's continued authority.
 *
 * PERSPECTIVAL GAP:
 *   The seven perspectives span the full classification spectrum because different agents have radically different structural relationships to the same constraint. The beneficiaries (standards bodies, large repositories, vendors) see coordination value and net positive flow. The mid-scale institutions see mixed coordination and extraction. The victims (small archives, marginalized communities) see extraction with minimal coordination benefit. The organized alternatives movement sees a temporary structure with a viable exit pathway. The legacy standards committees see their own work as partially degraded through obsolescence and institutional inertia. The civilizational analytical observer risks naturalizing a contingent institutional arrangement as an immutable requirement. The perspectival gap is diagnostic: it reveals that 'digital preservation standardization' is not a single structural phenomenon but an asymmetric extraction regime layered on top of a genuine coordination need. Coordination (interoperability benefits) is real and benefits large institutions. Extraction (compliance burden, design bias, vendor lock-in) is also real and harms small and marginalized institutions. Both perspectives are empirically justified.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options using the standard pipeline. Standards bodies and large repositories are beneficiaries with arbitrage exit options: d ≈ 0.15-0.20 (low, negative effective extraction). Mid-scale institutions are both beneficiary and victim with constrained exit: d ≈ 0.50-0.55 (symmetric). Small archives are victims with trapped exit: d ≈ 0.85-0.95 (high, maximum effective extraction from their perspective). Marginalized cultural producers add identity_locked exit option (d ≈ 0.88-0.92, identity-locked victims cannot exercise structural exit because doing so would require abandoning culturally-specific preservation practices). Open preservation movement is organized with mobile exit: d ≈ 0.45-0.55 (moderate). These directionality values feed the sigmoid f(d) to produce effective extractiveness chi = ε × f(d) × σ(S). For victims with trapped exit (d ≈ 0.90), f(d) ≈ 1.35, so chi ≈ 0.38 × 1.35 × 1.0 ≈ 0.51 (snare range). For beneficiaries with arbitrage (d ≈ 0.15), f(d) ≈ -0.01, so chi ≈ 0.38 × (-0.01) × 1.0 ≈ -0.004 (rope range). For analysts, chi ≈ 0.38 × 1.15 × 1.0 ≈ 0.44 (tangled rope range). The directionality pipeline explains why the same base extractiveness produces snare for victims, rope for beneficiaries, and tangled rope for analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The mandatrophy question is 'Is standardization primarily a coordination mechanism or primarily extraction?' The answer is: both, for different agents. For large institutional repositories and standards bodies, standardization is primarily coordination (Rope from their perspective). For small archives and marginalized cultural producers, standardization is primarily extraction (Snare from their perspective). For vendors, it is profitable coordination (Tangled Rope). For the open preservation movement, it is a temporary structure being obsoleted by decentralized alternatives (Scaffold). The resolution is not to declare one perspective correct and the others wrong, but to recognize that the constraint is genuinely hybrid in structure: it provides coordination value that large institutions benefit from, while imposing extraction burden that small and marginalized institutions bear. The 'resolution' of the mandatrophy is to design the standardization regime to distribute coordination benefits more broadly and extraction burden more proportionally — to shift from benefiting large institutions at the expense of small institutions toward a more symmetric structure. This is a policy recommendation, not a classification correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_viability,
    'Can fully decentralized preservation (no central standards, federated redundancy) maintain data integrity and findability at scale?',
    'Empirical testing of community-operated decentralized archives; comparison of bit-rot rates, metadata preservation, and discovery effectiveness against centralized standardized systems over 10+ years',
    'If viable: scaffold perspective confirmed — standardization regime is temporary, and sunset mechanism is real. If not viable: standardization appears more structurally necessary, and open preservation movement''s exit pathway is aspirational rather than realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_viability, empirical, 'Whether fully decentralized preservation infrastructure can maintain integrity at scale').

omega_variable(
    compliance_vs_functionality,
    'What proportion of institutional preservation labor goes to compliance theater (audits, documentation, format migration) versus actual bit-preservation and access functions?',
    'Cost accounting analysis across 20+ institutions of staff time allocation: compliance/audit vs. storage/maintenance vs. access/discovery. Correlation between audit rigor and actual preservation success rates.',
    'If compliance >> functionality: theater ratio is understated, and piton classification is stronger. If compliance ≈ functionality: theater is legitimately justified, and standard implementations have lower performance cost than assessed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_vs_functionality, empirical, 'Proportion of preservation effort devoted to compliance vs. actual preservation').

omega_variable(
    proprietary_standard_lock_in,
    'Are commercial implementations of open preservation standards genuinely interoperable, or do they fragment into incompatible proprietary dialects that entrench vendor lock-in?',
    'Compatibility testing across vendor implementations; cost analysis of migrating between vendors; frequency of de facto standard fragmentation (where vendors implement different subsets or extensions).',
    'If genuine interoperability: extraction mechanism is weaker than assessed, and tangled_rope classification is appropriate. If fragmentation: extraction through lock-in is higher, and snare classification may be more accurate for mid-scale institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_standard_lock_in, empirical, 'Whether commercial implementations maintain genuine interoperability or fragment into proprietary variants').

omega_variable(
    format_obsolescence_cascades,
    'Do digital preservation standards themselves become obsolete faster than the materials they preserve? Is the standard-compliance cycle a permanent treadmill or a temporary calibration phase?',
    'Historical analysis of standard lifespans (OAIS, MIAOU, etc.); measurement of format migration frequency vs. material preservation timescale; analysis of cumulative compliance cost across standard generations.',
    'If standards obsolete faster than materials: the constraint itself becomes unstable, and scaffold/piton dynamics dominate. If standards provide stable long-term framework: extraction through compliance treadmill is genuine, and snare classification may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(format_obsolescence_cascades, empirical, 'Whether preservation standards become obsolete faster than the materials they preserve').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_preservation_standardization, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digpres_tr_t0, digital_preservation_standardization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(digpres_tr_t7, digital_preservation_standardization, theater_ratio, 7, 0.52).
narrative_ontology:measurement(digpres_tr_t14, digital_preservation_standardization, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(digpres_be_t0, digital_preservation_standardization, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(digpres_be_t7, digital_preservation_standardization, base_extractiveness, 7, 0.3).
narrative_ontology:measurement(digpres_be_t14, digital_preservation_standardization, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_preservation_standardization, information_standard).
narrative_ontology:affects_constraint(digital_preservation_standardization, digital_format_obsolescence).
narrative_ontology:affects_constraint(digital_preservation_standardization, institutional_repository_consolidation).
narrative_ontology:affects_constraint(digital_preservation_standardization, vendor_lock_in_preservation_services).

% DUAL FORMULATION NOTE:
% Digital preservation standardization decomposes into three structurally distinct constraints with different ε values: (1) the genuine coordination problem (formats, metadata, integrity checking) which is necessary and relatively stable; (2) the vendor lock-in extraction mechanism which arises through proprietary implementations of open standards; (3) the compliance theater drift which accumulates as legacy standards persist through inertia. This story addresses the hybrid tangled_rope at the aggregate level. Upstream: format obsolescence (external constraint driving standardization demand). Downstream: institutional consolidation and vendor market concentration (consequences of standardization regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_preservation_standardization, powerless, 0.92).
constraint_indexing:directionality_override(digital_preservation_standardization, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
