% ============================================================================
% CONSTRAINT STORY: trust_boundary_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trust_boundary_architecture, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trust_boundary_architecture
 *   human_readable: Trust Boundary Architecture in Legal AI Systems
 *   domain: legal_technology/information_security/professional_responsibility
 *
 * SUMMARY:
 *   Trust boundary architecture in legal AI systems refers to the technical
 *   requirement that AI inference occur within a network perimeter that
 *   prevents data exfiltration to external parties. This constraint emerges
 *   from the intersection of attorney-client privilege doctrine (upstream
 *   constraint: privilege_waiver_threshold) and the technical architecture of
 *   AI systems. When legal professionals use AI tools to analyze privileged
 *   material, transmission of that material to external servers may
 *   constitute a privilege waiver under existing doctrine. Air-gapped
 *   networks and local inference eliminate this transmission pathway,
 *   preserving privilege. However, the capital requirements for implementing
 *   trust boundary architecture create a structural asymmetry: large firms
 *   with capital can access AI coordination benefits while maintaining
 *   privilege; solo practitioners and small firms face a choice between
 *   forgoing AI tools or accepting waiver risk. The constraint exhibits
 *   different types from different structural positions: large firms
 *   experience pure coordination (Rope), mid-size firms experience mixed
 *   coordination and extraction (Tangled Rope), and solo practitioners
 *   experience extraction with no viable exit (Snare). The analytical
 *   observer sees genuine coordination — trust boundaries solve a real
 *   information security problem — but this perspective risks naturalizing
 *   the capital barrier as inherent to the technical solution when
 *   alternative architectures (encrypted cloud, shared infrastructure, safe
 *   harbor rules) may be viable.
 *
 * KEY AGENTS:
 *   - Large Law Firms with Capital: Primary beneficiary (institutional/arbitrage) — can afford air-gapped infrastructure and local AI inference; capture competitive advantage from AI-assisted work without privilege waiver risk
 *   - Solo Practitioners: Primary victim (powerless/trapped) — cannot afford trust boundary infrastructure; face choice between competitive disadvantage (no AI) or legal risk (cloud AI with potential waiver)
 *   - Mid-Size Firms: Secondary victim (moderate/constrained) — can implement trust boundaries at significant cost; experience both coordination benefit and extraction burden
 *   - Enterprise Clients: Beneficiary (institutional/arbitrage) — receive more efficient legal services without bearing infrastructure costs or privilege waiver risk
 *   - Legal Aid Organizations: Mixed position (organized/mobile) — face resource constraints but have collective bargaining power and potential access to foundation funding for shared infrastructure
 *   - Information Security Analyst: Analytical observer (analytical/analytical) — sees trust boundary architecture as standard coordination mechanism for data exfiltration prevention; risks naturalizing capital requirements as inherent to technical solution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trust_boundary_architecture, 0.38).
domain_priors:suppression_score(trust_boundary_architecture, 0.42).
domain_priors:theater_ratio(trust_boundary_architecture, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trust_boundary_architecture, extractiveness, 0.38).
narrative_ontology:constraint_metric(trust_boundary_architecture, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trust_boundary_architecture, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trust_boundary_architecture, rope).
narrative_ontology:human_readable(trust_boundary_architecture, "Trust Boundary Architecture in Legal AI Systems").
narrative_ontology:topic_domain(trust_boundary_architecture, "legal_technology/information_security/professional_responsibility").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trust_boundary_architecture, law_firms_with_capital).
narrative_ontology:constraint_beneficiary(trust_boundary_architecture, enterprise_clients).
narrative_ontology:constraint_beneficiary(trust_boundary_architecture, in_house_legal_departments).
narrative_ontology:constraint_victim(trust_boundary_architecture, solo_practitioners).
narrative_ontology:constraint_victim(trust_boundary_architecture, small_firms).
narrative_ontology:constraint_victim(trust_boundary_architecture, legal_aid_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LARGE LAW FIRM (ROPE) — Can afford air-gapped infrastructure and local AI inference. Experiences the constraint as pure coordination: the trust boundary architecture solves the legitimate problem of maintaining privilege while using AI tools. Network isolation is a technical solution to a real professional responsibility requirement. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(trust_boundary_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-SIZE FIRM (TANGLED ROPE) — Can implement trust boundaries but at significant cost. Experiences both coordination (the architecture does protect privilege) and extraction (capital requirements create competitive disadvantage). Constrained exit: can adopt cloud AI and accept waiver risk, or invest heavily in local infrastructure. Mixed experience of benefit and burden.
constraint_indexing:constraint_classification(trust_boundary_architecture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOLO PRACTITIONER (SNARE) — Cannot afford air-gapped infrastructure or local AI inference hardware. Faces choice between forgoing AI tools entirely (competitive disadvantage) or using cloud services and risking privilege waiver. The trust boundary requirement extracts from this agent by making the coordination solution economically inaccessible. Trapped: no capital for infrastructure, no viable alternative pathway.
constraint_indexing:constraint_classification(trust_boundary_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ENTERPRISE CLIENT (ROPE) — Benefits from counsel's ability to use AI tools without privilege waiver risk. The trust boundary architecture enables more efficient legal service delivery while maintaining confidentiality. Experiences as coordination: the technical solution solves a real problem (protecting sensitive information) without imposing costs on the client.
constraint_indexing:constraint_classification(trust_boundary_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL AID ORGANIZATION (TANGLED ROPE) — Organized agents with some collective bargaining power and access to foundation funding, but still face significant resource constraints. Can potentially pool resources for shared infrastructure or negotiate volume licensing for compliant tools. Experiences both coordination benefit (when infrastructure is accessible) and extraction (when capital requirements exclude participation). Mobile exit: can advocate for alternative compliance pathways or subsidized infrastructure access.
constraint_indexing:constraint_classification(trust_boundary_architecture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational information security perspective, trust boundary architecture is a well-established coordination mechanism. Air-gapped networks and local processing are standard solutions to data exfiltration risk across many domains. The constraint solves a genuine technical problem: preventing unauthorized data transmission. The capital requirements are a consequence of the physics of computation and network isolation, not an extractive design choice.
constraint_indexing:constraint_classification(trust_boundary_architecture, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trust_boundary_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trust_boundary_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trust_boundary_architecture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(trust_boundary_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The trust boundary requirement creates a capital barrier that extracts competitive advantage from resource-constrained practitioners. However, the extraction is not as severe as pure rent-seeking because the constraint does solve a genuine technical problem (preventing data exfiltration). The value reflects that the capital asymmetry, while real, is partly a consequence of the technical solution rather than pure extractive design. Suppression (0.42): Moderate. Significant barriers include hardware costs ($10K-$100K+ for viable local inference), network infrastructure requirements, technical expertise for implementation and maintenance, and lack of alternative compliance pathways. But suppression is not total — some practitioners can pool resources, some clients will subsidize infrastructure, and regulatory safe harbors may emerge. Theater ratio (0.35): Low-moderate. Trust boundary architecture has genuine functional content — air-gapped networks do prevent data exfiltration, and local inference does eliminate the transmission pathway that triggers privilege waiver. However, some theater exists: security audits may focus on perimeter compliance rather than actual data protection, and the binary local/external distinction may be over-specified when encrypted cloud architectures could provide equivalent protection. Theater has increased slightly over the interval as compliance documentation has grown more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same technical architecture appears as different constraint types depending on the observer's structural position and resource access. Large firms and enterprise clients experience pure coordination (Rope) — the trust boundary solves a real problem (privilege protection) without imposing costs on them. Mid-size firms and legal aid organizations experience mixed coordination and extraction (Tangled Rope) — the architecture does protect privilege but at significant cost that creates competitive disadvantage. Solo practitioners experience pure extraction (Snare) — the capital requirements make the coordination solution inaccessible, forcing a choice between competitive disadvantage and legal risk. The analytical observer sees genuine coordination but risks naturalizing the capital barrier as inherent to the technical solution when alternative architectures may be viable. The perspectival gap reveals that 'is this coordination or extraction?' depends critically on whether the observer can afford to participate in the coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Large law firms are primary beneficiaries with arbitrage exit options — they can afford trust boundary infrastructure and capture competitive advantage from AI-assisted work. Their low d value (derived from beneficiary status + arbitrage exit + institutional power) produces low or negative effective extraction. Solo practitioners are primary victims with trapped exit options — they cannot afford infrastructure and have no viable alternative pathway. Their high d value (derived from victim status + trapped exit + powerless position) produces maximum effective extraction. Mid-size firms occupy an intermediate position with constrained exit — they can implement trust boundaries at significant cost, experiencing both coordination benefit and extraction burden. Enterprise clients are beneficiaries who receive more efficient legal services without bearing infrastructure costs. Legal aid organizations are organized agents with mobile exit — they face resource constraints but have collective bargaining power and can advocate for alternative compliance pathways. The analytical observer's rope classification reflects genuine coordination function but risks naturalizing the capital barrier.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that both coordination and extraction are present, with their relative salience depending on the observer's structural position. The trust boundary architecture does solve a genuine coordination problem (preventing privilege waiver through data exfiltration), but the capital requirements for implementing the solution create asymmetric extraction from resource-constrained practitioners. The mandatrophy is not 'is this coordination or extraction?' but 'for whom is it coordination, and for whom is it extraction?' Large firms experience coordination; solo practitioners experience extraction; mid-size firms experience both. The analytical observer's rope classification is correct from a civilizational information security perspective but incomplete — it captures the genuine coordination function while missing the distributional consequences of the capital barrier. The constraint is simultaneously a legitimate technical solution and a mechanism for concentrating AI-assisted legal work in well-capitalized firms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cloud_encryption_sufficiency,
    'Do end-to-end encrypted cloud AI services with contractual non-disclosure agreements provide equivalent privilege protection to air-gapped local inference?',
    'Legal precedent analysis: whether courts treat encrypted cloud transmission as privilege waiver; regulatory guidance from bar associations on cloud AI use; cryptographic analysis of encryption implementation quality',
    'If equivalent: trust boundary requirement is over-specified, and the capital barrier is extractive rather than necessary. If not equivalent: the constraint is genuine coordination and the capital requirement is inherent to the technical solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cloud_encryption_sufficiency, empirical, 'Whether encrypted cloud AI provides equivalent privilege protection').

omega_variable(
    local_inference_cost_floor,
    'What is the minimum viable cost for local AI inference hardware capable of useful legal analysis?',
    'Hardware cost trajectory analysis; benchmark testing of consumer-grade vs enterprise-grade local inference; identification of minimum model size for legal task performance',
    'If floor drops below $5K: solo practitioners can access the coordination benefit, reducing extraction. If floor remains above $50K: the capital barrier persists and extraction continues.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_inference_cost_floor, empirical, 'Minimum cost threshold for viable local AI inference').

omega_variable(
    privilege_waiver_doctrine_evolution,
    'Will courts and bar associations develop safe harbor rules for specific cloud AI architectures, or will the privilege waiver risk remain binary (local vs external)?',
    'Tracking of bar association ethics opinions; case law development on AI-assisted legal work; regulatory guidance from state bars and ABA',
    'If safe harbors emerge: the constraint becomes less extractive as alternative compliance pathways open. If doctrine remains binary: the trust boundary architecture remains the only viable solution and capital requirements persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(privilege_waiver_doctrine_evolution, preference, 'Whether legal doctrine will recognize intermediate compliance pathways').

omega_variable(
    shared_infrastructure_viability,
    'Can bar associations, legal aid networks, or professional organizations provide shared air-gapped AI infrastructure at scale, or do trust boundary requirements inherently require firm-level implementation?',
    'Pilot programs for shared legal AI infrastructure; analysis of multi-tenant security models; assessment of whether privilege protection survives shared infrastructure use',
    'If shared infrastructure is viable and privilege-preserving: extraction is reduced through collective action. If trust boundaries must be firm-specific: capital requirements remain a structural barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_infrastructure_viability, empirical, 'Whether shared infrastructure can preserve privilege protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trust_boundary_architecture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trust_boundary_tr_t0, trust_boundary_architecture, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trust_boundary_tr_t3, trust_boundary_architecture, theater_ratio, 3, 0.3).
narrative_ontology:measurement(trust_boundary_tr_t6, trust_boundary_architecture, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(trust_boundary_be_t0, trust_boundary_architecture, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trust_boundary_be_t3, trust_boundary_architecture, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(trust_boundary_be_t6, trust_boundary_architecture, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trust_boundary_architecture, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of privilege_waiver_threshold (mountain) — the trust boundary architecture is a technical response to the legal doctrine that transmission to external parties may constitute privilege waiver. The upstream constraint establishes the requirement; this constraint describes one implementation pathway and its distributional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
