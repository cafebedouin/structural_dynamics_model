% ============================================================================
% CONSTRAINT STORY: expertise_specialization_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expertise_specialization_trap, []).

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
 *   constraint_id: expertise_specialization_trap
 *   human_readable: Expertise Specialization Trap in Professional Systems
 *   domain: institutional/professional/cognitive
 *
 * SUMMARY:
 *   The expertise specialization trap is a structural constraint embedded in
 *   professional and academic systems where deep specialization enables
 *   genuine coordination benefits (knowledge accumulation, quality standards,
 *   methodological rigor) while simultaneously generating extraction dynamics
 *   through credentialing lock-in, gatekeeping, and cognitive/identity
 *   capture. The constraint exhibits tangled rope structure at its core:
 *   legitimate coordination function (maintaining knowledge quality through
 *   specialization) coupled with asymmetric extraction (new entrants bear
 *   credentialing costs; credential authorities benefit from gatekeeping).
 *   However, different institutional perspectives reveal the full range of
 *   constraint types. The aspiring practitioner faces snare-level extraction;
 *   the established specialist elite experience pure coordination (rope); the
 *   credential authorities maintain the system through arbitrage; the
 *   disciplinary ritual system has degraded into theater (piton); and the
 *   knowledge commons is constrained in its accessibility (tangled rope). The
 *   trajectory shows increasing theater ratio (ritual performance increasing
 *   over time as verification mechanisms degrade) and rising suppression
 *   (credentialing requirements and barriers intensifying) alongside rising
 *   extractiveness — the classic signature of a constraint shifting from
 *   coordination toward pure extraction.
 *
 * KEY AGENTS:
 *   - Aspiring Practitioners: Primary victims (powerless/trapped) — face credentialing barriers with no alternative entry paths; invest multi-year capital with contingent payoff
 *   - Interdisciplinary Workers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with specialized expertise; cannot exercise exit without abandoning self-concept
 *   - Mid-Career Specialists: Mixed (moderate/constrained) — benefit from specialization but bear suppression costs; limited career mobility despite expertise
 *   - Established Specialist Elite: Primary beneficiaries (institutional/arbitrage) — protected market position; can arbitrage out if threatened; experience specialization as pure coordination
 *   - Credential Authorities: Primary beneficiaries (institutional/arbitrage) — control entry standards and revenue from credentialing; regulate supply of specialists
 *   - Knowledge Commons: Victim (moderate/constrained) — benefits from quality standards but trapped in declining accessibility; cannot organize resistance
 *   - Disciplinary System: Institutional theater (institutional/arbitrage) — maintains performative verification rituals; sees own mechanisms as degraded but persists through inertia
 *   - Analytical Observer: Sees natural law (analytical/analytical) — risks naturalizing contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expertise_specialization_trap, 0.54).
domain_priors:suppression_score(expertise_specialization_trap, 0.62).
domain_priors:theater_ratio(expertise_specialization_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expertise_specialization_trap, extractiveness, 0.54).
narrative_ontology:constraint_metric(expertise_specialization_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(expertise_specialization_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expertise_specialization_trap, tangled_rope).
narrative_ontology:human_readable(expertise_specialization_trap, "Expertise Specialization Trap in Professional Systems").
narrative_ontology:topic_domain(expertise_specialization_trap, "institutional/professional/cognitive").

domain_priors:requires_active_enforcement(expertise_specialization_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expertise_specialization_trap, established_specialists).
narrative_ontology:constraint_beneficiary(expertise_specialization_trap, professional_gatekeepers).
narrative_ontology:constraint_beneficiary(expertise_specialization_trap, credential_authorities).
narrative_ontology:constraint_victim(expertise_specialization_trap, aspiring_practitioners).
narrative_ontology:constraint_victim(expertise_specialization_trap, interdisciplinary_workers).
narrative_ontology:constraint_victim(expertise_specialization_trap, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PRACTITIONER (SNARE) — Trapped by credentialing requirements that are presented as quality filters but function as extraction mechanisms. Cannot practice without obtaining specialized credentials; credentials require multi-year capital investment with no alternative paths. Career trajectory entirely locked into the specialist hierarchy. Maximum experienced extraction with zero exit capacity.
constraint_indexing:constraint_classification(expertise_specialization_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERDISCIPLINARY WORKER (SNARE) — Structurally mobile (could leave the field) but identity-fused with their specialized expertise. Cannot exercise exit options without abandoning professional identity and years of accumulated human capital. The specialist identity constitutes their self-concept; exit would require becoming a different person. Trapped by internal framing despite having material alternatives.
constraint_indexing:constraint_classification(expertise_specialization_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER SPECIALIST (TANGLED ROPE) — Experiences genuine coordination benefit: specialized knowledge enables high-quality work and peer collaboration. But also experiences extraction through high suppression costs (continued credential maintenance, conference attendance, publication requirements) and gatekeeping that limits career mobility. Benefits from specialization while also bearing its asymmetric costs.
constraint_indexing:constraint_classification(expertise_specialization_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED SPECIALIST ELITE (ROPE) — Benefits from specialization without bearing suppression costs. Credentialing and gatekeeping protect their market position and income. Can arbitrage out if threatened (retain prestige, shift to administration, consulting, or legacy positions). Experiences specialization as pure coordination: knowledge standards, peer recognition, disciplinary coherence.
constraint_indexing:constraint_classification(expertise_specialization_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIAL AUTHORITY (ROPE) — Licensing boards, accreditation bodies, professional associations. Benefits from specialization through control of entry standards and revenue from credentialing. Arbitrage options abundant: can relax standards strategically, create new credential tiers, or defend turf. Sees the trap as a coordination mechanism: maintaining knowledge quality through credentialing.
constraint_indexing:constraint_classification(expertise_specialization_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: KNOWLEDGE COMMONS (TANGLED ROPE) — The shared pool of accessible professional knowledge. Benefits from specialization through accumulated methodological rigor and quality standards. But extraction occurs through gatekeeping (knowledge locked behind credentialing, publication paywalls, jargon barriers). As specialization deepens, knowledge becomes less accessible to outsiders and interdisciplinary workers. Trapped by inability to organize collectively.
constraint_indexing:constraint_classification(expertise_specialization_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DISCIPLINARY RITUAL SYSTEM (PITON) — Professional conferences, journal peer review, credentialing exams. These rituals were designed to maintain quality standards but increasingly function as theater. Practitioners engage in performative specialization: publishing to maintain status rather than to advance knowledge, attending conferences for networking rather than learning. Theater ratio high because the verification mechanisms (peer review, conference presentations) have degraded but persist through institutional inertia. The disciplinary system sees itself as necessary but no longer believes in its own mechanisms.
constraint_indexing:constraint_classification(expertise_specialization_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, deep specialization appears to be an immutable requirement of complex knowledge: you cannot master modern physics, medicine, or engineering without deep focus. The cognitive limits of human learning demand specialization; the extraction is presented as a necessary side effect of knowledge accumulation. However, this naturalizes what is actually a contingent institutional arrangement — the specificity of credentialing gatekeeping, the depth of required specialization, and the exclusivity of knowledge access are designed choices, not laws of nature.
constraint_indexing:constraint_classification(expertise_specialization_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expertise_specialization_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expertise_specialization_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expertise_specialization_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expertise_specialization_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expertise_specialization_trap, TR),
    TR >= 0.70.

:- end_tests(expertise_specialization_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high, increasing over interval. The constraint exhibits genuine coordination benefit — specialization does enable knowledge quality, methodological rigor, and disciplinary coherence. But extractiveness has risen from 0.35 to 0.54 over the 20-year interval as credentialing requirements have intensified and gatekeeping has become more selective. New entrants face higher barriers; credential authorities have tightened supply. The rising trajectory indicates extraction mechanisms are accumulating atop the coordination function. Suppression (0.62): High and rising. Multiple suppression mechanisms operate: multi-year credentialing requirements (5-8 years typical in medical, legal, engineering fields), publication paywalls and journal gatekeeping, professional licensing exam barriers, conference attendance costs as markers of professional membership, jargon used to exclude non-specialists, and psychological barriers from identity fusion. These are not incidental costs — they are designed into the system as quality filters. Theater ratio (0.58): Moderate-high and rising. Peer review increasingly functions as performance rather than verification — reviewers cannot assess raw data quality, replication probability, or practical applicability, only novelty and disciplinary relevance. Conferences are networking theater disguised as knowledge exchange. Credentialing exams test disciplinary canon rather than practical competence. The rising trajectory (0.38 → 0.58) reflects that as knowledge complexity outpaces verification capacity, ritual performance substitutes for actual quality control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full tangled rope spectrum. The established specialist sees coordination (rope) — their experience is pure benefit from knowledge standards. The aspiring practitioner sees extraction (snare) — their experience is pure cost with no exit. The mid-career specialist sees mixed benefit and cost (tangled rope) — they experience the real structure of the constraint. The credential authorities see coordination and arbitrage option (rope) — their experience is protection and market control. The disciplinary system sees degraded ritual (piton) — peer review persists despite lost functional power. The analytical observer risks seeing natural law (mountain) — specialization appears necessary. The knowledge commons experiences partial entrapment (tangled rope) — benefits from quality standards but trapped in inaccessibility. The perspectival gaps are driven by position in the extraction pipeline: those upstream (beneficiaries) see coordination; those trapped at the bottleneck (new entrants) see snare; those with partial agency see tangled rope. The analytical observer's mountain classification is a false summit — the constraint is presented as an immutable feature of knowledge accumulation, but the rising theater ratio and suppression trajectory indicate it is a designed institutional arrangement sustained by beneficiary gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from the agent's structural relationship to the constraint. Beneficiaries (established specialists, credential authorities) derive low directionality (d ≈ 0.1-0.2) — they control gatekeeping and experience extraction flowing toward them. Their power level (institutional) and exit options (arbitrage) produce low f(d), giving them negative or near-zero effective extraction. Victims (aspiring practitioners, interdisciplinary workers) derive high directionality (d ≈ 0.85-0.95) — they face credentialing barriers with trapped or identity_locked exit options. Their high d produces high f(d), giving them maximum experienced extraction. The mid-career specialist at moderate power derives mid-range directionality (d ≈ 0.55), experiencing mixed coordination and extraction. The knowledge commons (collective/constrained) derives moderate-high directionality; it cannot organize to resist despite bearing costs. The credential authorities' arbitrage options (ability to relax standards, create new credential tiers, shift to consulting) are the structural key to why they experience rope rather than snare — they always have exit. The aspiring practitioner's trap options (trapped, no alternatives to credentialing if they want professional status) make snare inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the real tangled rope structure (coordination + extraction) from false-summit mountains (naturalized institutional arrangement) and degraded pitons (ritual persisting without function). The coordination function is real: specialization does maintain quality standards and enable knowledge accumulation. The extraction is also real: gatekeeping, credentialing lock-in, and identity capture are designed mechanisms, not necessary side effects. The critical diagnostic is the theater ratio and suppression trajectory — if these were truly necessary to maintain quality, they would be stable or declining as the system optimizes. Instead, they are rising, indicating that extraction mechanisms are being actively strengthened beyond what coordination requires. The piton perspective identifies that professional rituals (peer review, conferences, credentialing exams) are increasingly theater — maintained through inertia despite degraded verification capacity. This is a signal that the system has shifted from coordination toward extraction. The false summit is the analytical perspective's claim that specialization is a natural law of knowledge accumulation — the constraint is presented as immutable, but beneficiaries have an interest in naturalizing it, and the trajectory data (rising theater, rising suppression, rising extractiveness) suggests it is a constructed and intensifying institutional arrangement. Resolution requires distinguishing what specialization depth is necessary for quality from what is extraction beyond that threshold, and identifying which suppression mechanisms maintain quality versus which extract rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specialization_depth_optimum,
    'What depth of specialization optimizes knowledge quality vs. accessibility? Is current specialization depth necessary or excessive?',
    'Comparative analysis across professions: measure knowledge quality metrics (error rates, innovation speed) against specialization requirements. Identify whether quality degrades when specialization requirements are reduced.',
    'If current depth is necessary: specialization trap is closer to Mountain (inherent structure). If excessive: extraction mechanism is contingent and could be redesigned by reducing credentialing requirements while maintaining quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialization_depth_optimum, empirical, 'Whether current specialization depth is necessary or extractive').

omega_variable(
    alternative_knowledge_verification,
    'Can interdisciplinary verification, peer networks, and portfolio assessment replace specialized credentialing without quality loss?',
    'Pilot programs testing alternative credentialing (skills-based, project-portfolio, peer attestation). Measurement of outcomes (error rates, practitioner performance, innovation) vs. traditional credentialed cohorts.',
    'If alternatives work: the trap is an institutional choice (credentialing authorities maintain it for extraction, not necessity). If alternatives fail: suppression mechanisms may be necessary for quality control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_knowledge_verification, empirical, 'Whether alternative credentialing can replace specialization without quality loss').

omega_variable(
    identity_lock_mechanism,
    'Is the interdisciplinary worker''s inability to exit driven by structural barriers (economic cost, family dependence) or internalized identity fusion?',
    'Post-exit longitudinal tracking: do workers who leave the field report relief or continued self-doubt? Do they reconstruct professional identity or remain psychologically trapped? Interview data on identity narratives.',
    'If structural barriers dominate: reclassify interdisciplinary perspective as constrained rather than identity_locked; suppression can be reduced through economic support. If identity fusion dominates: the constraint persists even when material barriers drop; psychological work required for exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. identity-based mechanisms of worker entrapment').

omega_variable(
    gatekeeping_necessity,
    'How much gatekeeping is necessary to prevent incompetent practitioners from harming the public? How much of current gatekeeping is extraction beyond that threshold?',
    'Historical analysis of licensing requirement changes; correlation between stricter credentialing and harm reduction vs. market consolidation. Study of professions with minimal gatekeeping (software, management consulting) to identify harm rates.',
    'If high gatekeeping is necessary: snare perspective is justified by quality/safety requirements. If most gatekeeping is extractive: credential authority benefits could be eliminated without public harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity, empirical, 'How much gatekeeping is necessary vs. extractive').

omega_variable(
    disciplinary_theater_degradation_rate,
    'Is peer review, conference presentation, and credentialing exam functionality degrading over time, or is the system adapting?',
    'Longitudinal study of publication retraction rates, citation patterns of non-peer-reviewed vs. peer-reviewed work, and practitioner assessments of whether credentialing exams predict competence.',
    'If degrading: piton classification confirmed — rituals persist despite lost function. If adapting: institutional system has agency and can reform itself. If functionality stable: theater ratio may be overestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_theater_degradation_rate, empirical, 'Whether disciplinary verification rituals are degrading or adapting').

omega_variable(
    false_summit_commitment_system,
    'Is specialization presented as immutable natural law because it actually is, or because beneficiaries have an interest in naturalizing a contingent institutional arrangement?',
    'Historical analysis: document how specialization requirements have changed over time. Identify cases where specialization was reduced without quality loss. Examine whether credential authorities actively resist reducing specialization depth.',
    'If beneficiaries actively resist evidence-based reform: false summit confirmed. The mountain classification is a cover story for extraction. If specialization depth is stable despite reform pressure: likely necessary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_commitment_system, conceptual, 'Is specialization a natural law or a constructed constraint naturalized by beneficiaries?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expertise_specialization_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(est_tr_t0, expertise_specialization_trap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(est_tr_t10, expertise_specialization_trap, theater_ratio, 10, 0.48).
narrative_ontology:measurement(est_tr_t20, expertise_specialization_trap, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(est_be_t0, expertise_specialization_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(est_be_t10, expertise_specialization_trap, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(est_be_t20, expertise_specialization_trap, base_extractiveness, 20, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(est_su_t0, expertise_specialization_trap, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(est_su_t10, expertise_specialization_trap, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(est_su_t20, expertise_specialization_trap, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expertise_specialization_trap, identity_coordination).
narrative_ontology:affects_constraint(expertise_specialization_trap, credentialing_authority_capture).
narrative_ontology:affects_constraint(expertise_specialization_trap, knowledge_accessibility_barrier).
narrative_ontology:affects_constraint(expertise_specialization_trap, professional_burnout_cycle).

% DUAL FORMULATION NOTE:
% The expertise specialization trap decomposes into structurally distinct constraints: (1) credentialing_authority_capture — regulatory capture of professional licensing bodies by established specialists, a snare-level constraint at the institutional level; (2) knowledge_accessibility_barrier — the gatekeeping of professional knowledge through specialization jargon and paywall-based publication, a coordination function with extraction asymmetry; (3) professional_burnout_cycle — the intensification of suppression requirements (publication pressure, credential maintenance, ritual performance) leading to practitioner demoralization, a snare at the individual psychological level. These are separate stories with different ε values and beneficiary/victim structures, linked through network causality: specialization trap creates conditions for authority capture, which intensifies accessibility barriers, which drives burnout. Each story captures a distinct structural layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expertise_specialization_trap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
