% ============================================================================
% CONSTRAINT STORY: epistemic_authority_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_authority_consolidation, []).

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
 *   constraint_id: epistemic_authority_consolidation
 *   human_readable: Epistemic Authority Consolidation Across Knowledge Domains
 *   domain: epistemology/institutional_knowledge
 *
 * SUMMARY:
 *   Epistemic authority consolidation describes the structural concentration
 *   of knowledge validation power within credentialed institutions,
 *   peer-review systems, and degree-granting bodies. This constraint operates
 *   across all knowledge domains and exhibits the full range of DR
 *   classifications depending on observer position. The same institutional
 *   structure — credential-mediated gatekeeping — appears as an immutable
 *   feature of knowledge systems (mountain), a coordination mechanism
 *   enabling shared standards (rope), a hybrid mechanism with genuine
 *   coordination alongside extraction (tangled rope), a temporary problem
 *   being solved by alternative credentialing (scaffold), a degraded ritual
 *   maintained by inertia (piton), or pure exclusion and rent extraction
 *   (snare). The theater ratio has risen from 0.42 to 0.64 over the interval,
 *   indicating that credentialing mechanisms are increasingly performative:
 *   citation metrics proxy for quality without measuring it; journal prestige
 *   proxies for reliability without guaranteeing it; degree credentials proxy
 *   for competence without validating it. The extractiveness has risen from
 *   0.38 to 0.58, reflecting accumulating barriers to non-credentialed
 *   knowledge production and increasing rent extraction by gatekeeping
 *   institutions. Alternative credentialing systems (open-access journals,
 *   citizen science networks, blockchain reputation) represent a genuine
 *   sunset pathway, but their maturation is slower than scaffold theory
 *   predicts.
 *
 * KEY AGENTS:
 *   - Credentialed Institutions: Primary beneficiary (institutional/arbitrage) — universities, professional societies, journals extract monopoly rents from credential gatekeeping; face abundant exit options (can adjust standards, create alternative pathways) but choose consolidation
 *   - Alternative Knowledge Producers: Primary victim (powerless/trapped) — non-institutional researchers, indigenous knowledge systems, community practitioners systematically excluded regardless of competence; face insurmountable credentialing barriers or must abandon knowledge work entirely
 *   - Junior Researchers: Secondary victim (moderate/constrained) — early-career scholars in gatekept disciplines benefit from institutional resources and credential validation but pay extraction costs through publish-or-perish pressure, methodological conformity, and career fragility; can exit but at significant cost
 *   - Field Epistemic Diversity: Abstract collective victim (powerless/trapped) — the stock of available knowledge narrows as alternative knowledge systems are excluded; no mechanism for self-advocacy; bears full cost of lost knowledge pathways
 *   - Open Knowledge Coalition: Organized agents (organized/constrained) — Wikipedia, citizen science networks, open-access journals, blockchain platforms building parallel credentialing systems; face resistance from incumbents but have real exit pathways and visible sunset timeline
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent gatekeeping structures as inevitable features of knowledge systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_authority_consolidation, 0.58).
domain_priors:suppression_score(epistemic_authority_consolidation, 0.68).
domain_priors:theater_ratio(epistemic_authority_consolidation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_authority_consolidation, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_authority_consolidation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(epistemic_authority_consolidation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_authority_consolidation, tangled_rope).
narrative_ontology:human_readable(epistemic_authority_consolidation, "Epistemic Authority Consolidation Across Knowledge Domains").
narrative_ontology:topic_domain(epistemic_authority_consolidation, "epistemology/institutional_knowledge").

domain_priors:requires_active_enforcement(epistemic_authority_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_authority_consolidation, credentialed_institutions).
narrative_ontology:constraint_beneficiary(epistemic_authority_consolidation, gatekeeping_credentialing_bodies).
narrative_ontology:constraint_victim(epistemic_authority_consolidation, alternative_knowledge_producers).
narrative_ontology:constraint_victim(epistemic_authority_consolidation, field_epistemic_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE KNOWLEDGE PRODUCER (SNARE) — Non-credentialed practitioners, indigenous knowledge keepers, community researchers, and self-taught experts face systematic exclusion from epistemic authority regardless of empirical competence. Trapped by credentialing requirements that cannot be met without institutional resources. No arbitrage path; exit requires abandoning knowledge production entirely or capitulating to institutional requirements.
constraint_indexing:constraint_classification(epistemic_authority_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JUNIOR RESEARCHER (TANGLED ROPE) — Early-career scholars in credential-dependent fields benefit from access to institutional resources and credential validation, yet pay extraction costs through publish-or-perish pressure, methodological conformity requirements, and career fragility. Experiences genuine coordination (shared standards, collaborative access) alongside asymmetric extraction (seniors extract credit; juniors bear risk).
constraint_indexing:constraint_classification(epistemic_authority_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED INSTITUTION (ROPE) — Universities, professional societies, and journals experience the constraint as pure coordination: shared standards enable collaboration, trust, and knowledge accumulation. Institutional actors benefit from credentialing authority and experience low extraction costs. Exit options abundant — can migrate credentials, establish alternative pathways, adjust standards.
constraint_indexing:constraint_classification(epistemic_authority_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE COALITION (SCAFFOLD) — Wikipedia, citizen science networks, open-access journals, and blockchain-based credentialing systems represent emerging alternative authority structures with sunset logic. These organized agents build parallel epistemic pathways (reputation via contribution history, decentralized validation) that bypass traditional gatekeeping. High agency; visible exit path as alternatives mature. Sunset: 15-25 years as decentralized credentialing gains institutional acceptance.
constraint_indexing:constraint_classification(epistemic_authority_consolidation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING RITUAL APPARATUS (PITON) — Peer review, academic journals, degree requirements, and citation metrics persist largely through institutional inertia despite documented limitations (reproducibility crisis, prestige concentration, validation theater). The apparatus sees itself as degraded — actors within it acknowledge the bottleneck and performative elements — yet maintenance continues because replacements haven't fully materialized. Theater ratio 0.64 reflects that credential validation is increasingly performative (citation counts proxy for quality; journal impact factors proxy for truth-value) rather than functional verification.
constraint_indexing:constraint_classification(epistemic_authority_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED AUTHORITY VIEW (MOUNTAIN) — From a civilizational perspective, epistemic authority consolidation appears as an immutable feature of knowledge: all knowledge systems require consensus mechanisms, credential-like signals (whether institutional or peer-based), and authority hierarchies to prevent epistemic chaos. This view naturalizes gatekeeping as an inevitable governance structure. However, this is a false summit — the engine's analysis reveals that consolidated authority is a contingent institutional arrangement, not a natural law. Alternative credentialing (decentralized, reputation-based, contribution-tracked) demonstrates that authority consolidation is not inevitable.
constraint_indexing:constraint_classification(epistemic_authority_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_authority_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_authority_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_authority_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_authority_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_authority_consolidation, TR),
    TR >= 0.70.

:- end_tests(epistemic_authority_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. The base measure reflects the extraction visible at the moderate agent level — junior researchers pay real costs for institutional credentialing (publish-or-perish, career risk, methodological conformity). From the powerless victim perspective, extraction is maximal (alternative knowledge is systematically excluded). The rise from 0.38 to 0.58 reflects two mechanisms: (1) credential inflation (more credentials required for same recognition), (2) consolidation (fewer alternative pathways as institutional gatekeeping strengthens). Suppression (0.68): High. Barriers to non-credentialed knowledge production include institutional resource requirements (research equipment, computing, libraries), network access (collaboration requires institutional affiliation), credentialing costs (time and money required for degrees/certifications), and publication bias (non-institutional research lacks prestige signaling). These are real structural barriers, not merely psychological. Theater ratio (0.64): High and rising. Credential validation is increasingly performative: journal impact factors correlate poorly with citation impact and not at all with knowledge reliability; citation counts measure prestige concentration more than quality; degree credentials correlate with entry-level performance but not with lifetime contribution. The rise from 0.42 to 0.64 reflects that as knowledge has become increasingly specialized and voluminous, gatekeeping mechanisms have shifted from substantive validation (peer reviewers verifying claims) to proxy signaling (citation metrics, journal names, degree types). Tangled Rope type: Genuine coordination function exists (shared standards enable collaboration and knowledge accumulation); asymmetric extraction also exists (credentialed institutions extract monopoly rents; junior researchers and non-credentialed producers bear disproportionate costs). The constraint is not pure extraction because it does solve coordination problems; it is not pure coordination because beneficiaries extract more than necessary for coordination.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives reveals the constraint's true structure. Beneficiaries perceive coordination because institutional credentialing provides them with collaboration infrastructure, prestige signaling, and resource access. Victims perceive extraction because the same gatekeeping excludes them with no corresponding benefit. The gap widens at smaller time scales (immediate and biographical) and narrows at larger scales (generational and civilizational, where alternative credentialing systems are visible as plausible). This is diagnostic: if the constraint were pure coordination, beneficiaries and victims would experience it similarly; the large perspectival gap indicates asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural position: power level, exit options, and benefit/cost relationship to the constraint. Beneficiaries with arbitrage options (credentialed institutions) experience low effective extraction via the sigmoid function; they can leave, adapt, or bypass the constraint if it becomes disadvantageous. Trapped agents with no exit (alternative knowledge producers) experience maximum extraction regardless of the constraint's functional value. Constrained agents (junior researchers) with partial exit options but high costs experience moderate-to-high extraction; their d-value reflects that they cannot easily leave despite the costs. The institutional perspective yields different d-values for beneficiaries (arbitrage → low d → low χ) than for victims (trapped → high d → high χ), creating the perspectival gap. Organized agents (open knowledge coalition) with real exit pathways experience lower extraction than unorganized victims; their ability to build alternatives directly reduces effective extractiveness from their viewpoint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING MANDATROPHY: The constraint achieves mandatrophy resolution by showing that credentialing systems are genuinely mixed — they coordinate knowledge production AND extract rents, not one or the other. The coordination function is real (shared standards enable collaboration); the extraction is also real (credentialed institutions monopolize authority and exclude alternatives). The classification as Tangled Rope is not a compromise or averaging — it is a precise statement that both functions are structurally present. The theater ratio (0.64) distinguishes this from pure Rope: if credentialing systems purely coordinated, validation would be functional (low theater); high theater indicates increasing performativity. The mandatrophy resolves because the engine can now distinguish: (1) genuine coordination constraints (low extraction, functional validation, symmetrical benefit structure) → Rope, (2) mixed systems (real coordination + real extraction + performative elements) → Tangled Rope, (3) pure extraction systems (minimal coordination function) → Snare. Epistemic authority consolidation is Tangled Rope because it coordinates knowledge production while extracting monopoly rents; eliminating the rents would undermine the coordination, and eliminating the coordination would eliminate knowledge system function. This is not a 'both sides' equivocation — it is a structural fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_validation_threshold,
    'What validation standard distinguishes legitimate quality control from gatekeeping extraction?',
    'Empirical comparison: correlation between credentialing mechanisms (peer review, journal impact, degree requirements) and subsequent knowledge reliability; analysis of gatekept vs ungated knowledge quality metrics',
    'If credentials show high predictive power for reliability: consolidation appears as legitimate coordination (Rope). If credentials show low predictive power: consolidation appears as pure extraction (Snare). Current evidence suggests mixed signal — status quo ante predicts neither truth-value nor practical utility reliably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_validation_threshold, empirical, 'Validation threshold between quality control and gatekeeping').

omega_variable(
    alternative_credentialing_viability,
    'Can decentralized credentialing systems (blockchain reputation, contribution-tracked authority, peer-attestation networks) achieve validation functions equivalent to institutional credentialing at scale?',
    'Field deployment data from Wikipedia, ArXiv, blockchain scientific publishing platforms; comparative analysis of error rates, contradiction detection, and knowledge accumulation across credentialing types; longitudinal tracking of reliability as decentralized systems mature',
    'If decentralized systems prove viable: scaffold sunset is structural (alternatives really work). If decentralized systems fail at scale: scaffold is aspirational, and consolidated authority is revealed as necessary. Current evidence: decentralized systems work for low-stakes knowledge (encyclopedia entries) and high-transparency domains (mathematics preprints) but struggle with high-stakes, opaque domains (medicine, policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Viability of decentralized credentialing at scale').

omega_variable(
    credential_capture_mechanism,
    'To what extent does credentialing authority capture exclude competent knowledge producers vs. exclude incompetent ones? What proportion of exclusion is accuracy-targeting vs. rent extraction?',
    'Longitudinal analysis of non-credentialed knowledge producers: track career outcomes, knowledge contributions, and recognition received; compare error rates and knowledge quality between credentialed and non-credentialed producers in domains where alternative pathways exist; audit credentialing criteria for race/gender/institutional bias',
    'If capture mechanism primarily excludes incompetence: consolidation is coordination with some extraction overhead (Tangled Rope). If capture mechanism primarily excludes on non-competence grounds: consolidation is rent extraction (Snare). Current evidence: credentialing excludes on institutional, financial, and demographic grounds as much as competence grounds; estimated 40-60% of exclusion is gatekeeping rather than quality-targeting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_capture_mechanism, empirical, 'Proportion of credentialing exclusion that targets competence vs. rent extraction').

omega_variable(
    identity_lock_in_academic_careers,
    'Among credentialed knowledge producers who perceive the system as extractive (high publishing pressure, methodological conformity requirements), how many remain trapped by career identity fusion vs. how many remain trapped by material career costs?',
    'Qualitative interview analysis of exit-considering researchers; longitudinal career tracking of post-exit trajectories (do ex-academics maintain research identity?); comparison of exit barriers articulated as structural vs. identity-based',
    'If identity-locked dominance high: academic psychology is structurally captured (institutional identity supplants individual identity); exit requires identity reformation, making the constraint structurally less changeable than credentialing requirements alone. If material barriers dominate: exit is costly but possible without identity reconstruction; constraint is theoretically changeable if material costs were addressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_academic_careers, empirical, 'Role of identity fusion vs. material costs in academic career lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_authority_consolidation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_authority_consolidation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(epis_tr_t15, epistemic_authority_consolidation, theater_ratio, 15, 0.58).
narrative_ontology:measurement(epis_tr_t30, epistemic_authority_consolidation, theater_ratio, 30, 0.64).
narrative_ontology:measurement(epis_tr_t45, epistemic_authority_consolidation, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_authority_consolidation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(epis_be_t15, epistemic_authority_consolidation, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(epis_be_t30, epistemic_authority_consolidation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(epis_be_t45, epistemic_authority_consolidation, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_authority_consolidation, information_standard).
narrative_ontology:boltzmann_floor_override(epistemic_authority_consolidation, 0.12).
narrative_ontology:affects_constraint(epistemic_authority_consolidation, publication_bias_in_peer_review).
narrative_ontology:affects_constraint(epistemic_authority_consolidation, academic_credential_inflation).
narrative_ontology:affects_constraint(epistemic_authority_consolidation, knowledge_commons_enclosure).

% DUAL FORMULATION NOTE:
% Epistemic authority consolidation is upstream of specific domain-level constraints (publication bias, credential inflation) because gatekeeping power flows from the credentialing system to each domain. A separate constraint story on 'decentralized epistemic validation' would represent the downstream alternative pathway; epistemic authority consolidation affects both the old pathway (institutional credentialing) and enables the new one (alternative authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_authority_consolidation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
