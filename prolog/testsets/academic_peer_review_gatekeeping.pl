% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping, []).

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
 *   constraint_id: academic_peer_review_gatekeeping
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Academic journal gatekeeping represents a hybrid constraint combining
 *   genuine coordination (peer review quality signaling, knowledge
 *   distribution infrastructure) with systematic extraction (paywalls,
 *   monopolistic pricing, rent-seeking on researcher labor). The system
 *   extracts value from multiple nodes: early-career researchers provide free
 *   peer review and editing labor to maintain career credentials;
 *   institutions pay escalating subscription fees for access to research
 *   their own faculty produced; the Global South bears disproportionate costs
 *   as percentage of research budgets; and the knowledge commons is
 *   suppressed by paywalls and embargo periods. Over the past 30 years
 *   (measurement interval), extractiveness has increased from 0.32 to 0.58 as
 *   publisher consolidation has strengthened pricing power and theater has
 *   increased from 0.48 to 0.65 as the peer review mechanism has become
 *   increasingly performative — reviewers cannot detect fraud or
 *   methodological errors that preprint communities catch, yet the ritual
 *   persists. Simultaneously, alternative mechanisms (arXiv, bioRxiv, Plan S
 *   mandates, institutional repositories) have matured, creating a visible
 *   sunset clause and transforming the constraint from pure snare (in early
 *   career paths) or rope (from publisher perspective) into tangled_rope for
 *   most moderate agents and scaffold for organized coalitions.
 *
 * KEY AGENTS:
 *   - For-Profit Publishers: Primary beneficiary (institutional/arbitrage) — control journal brands, extract rents through paywalls and subscription bundling, experience system as pure coordination
 *   - Early Career Researchers: Primary victim (powerless/trapped) — must publish in high-impact journals for career advancement, provide free peer review labor, cannot exit without career cost
 *   - Resource-Constrained Institutions: Secondary victim (moderate/constrained) — pay escalating subscription fees for access to faculty research, limited negotiating power
 *   - Global South Researchers and Institutions: Secondary victim (organized/constrained) — face disproportionate extraction relative to research budgets, some coalition power through consortia agreements
 *   - Journal Editors: Institutional beneficiary (institutional/constrained) — manage peer review and editorial processes, constrained by publisher policies, benefit from prestige system
 *   - Open Access Movement: Organized agents (organized/mobile) — arXiv, bioRxiv, Plan S mandates, university repositories building alternative pathways with sunset logic
 *   - The Knowledge Commons: Victim (powerless/trapped) — abstract collective good that bears cost of restricted access and delayed dissemination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing journal gatekeeping as necessary scientific infrastructure rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.58).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.68).
domain_priors:theater_ratio(academic_peer_review_gatekeeping, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, for_profit_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, journal_editors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, early_career_researchers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, resource_constrained_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, global_south_researchers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, open_knowledge_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Must publish in high-impact journals to secure grants, tenure, and career advancement. Provides free peer review labor to maintain legitimacy in the field. Cannot exit: rejecting the system means career death. Maximum suppression and extraction — no alternatives for career signaling exist.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED INSTITUTION (TANGLED ROPE) — Libraries must pay escalating subscription fees to access research produced by their own faculty. Receives coordination benefit (access to global knowledge) but faces asymmetric extraction through pricing. Constrained exits: cannot negotiate journal-by-journal, bundled subscription models limit options. Mixed experience of coordination and coercion.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FOR-PROFIT PUBLISHER (ROPE) — Controls the journal brand and distribution infrastructure. Coordinates global knowledge circulation and quality signaling through peer review mechanism. Experiences the constraint as pure coordination: managing reviewer networks, maintaining journal prestige, distributing articles. Net beneficiary — extraction flows toward this actor, not away.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN ACCESS MOVEMENT (SCAFFOLD) — Organized agents (arXiv, bioRxiv, PubMed Central, Plan S mandates, university repositories) are building alternative verification and distribution pathways. See the journal gatekeeper constraint as a temporary coordination failure with a sunset: preprint servers establish priority, institutional repositories provide access, and funder mandates shift incentive alignment. High agency and visible exit pathways.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The peer review process has become substantially performative rather than functionally protective of knowledge quality. Publishers use peer review theater to justify paywalls and extract rents, but the mechanism's actual quality filtering is modest and inconsistent across disciplines. The ritual persists through institutional inertia (career incentives, prestige narratives) despite widespread recognition of its limitations (bias, slow timescale, inability to catch fraud). Theater ratio remains high.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL SOUTH INSTITUTION (TANGLED_ROPE) — Benefits from access to global research through journal subscriptions, enabling participation in worldwide knowledge production. Simultaneously bears disproportionate extraction: subscription costs as percentage of research budget are much higher than in wealthy nations. Organized but constrained — coalition-based negotiation (INASP, AGORA programs) provides some exit options but limited leverage. Hybrid experience: coordination benefit with severe asymmetric extraction.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT / PITON DETECTION) — At the civilizational scale, the system appears as an immutable feature of scientific knowledge production: some credentialing mechanism is inherently necessary to distinguish signal from noise in research claims. However, the structural data reveals this as naturalization of a contingent institutional arrangement. The 'necessity' of for-profit journal gatekeeping (and its attendant extraction) is institutional, not inherent. The engine's false summit detector identifies this as piton-disguised-as-mountain.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_peer_review_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The system extracts value through multiple mechanisms: monopolistic paywalls with markup of 300-800% over production costs, free reviewer labor worth ~$1.9 billion annually globally, suppressed alternative pathways, and restricted access creating information asymmetries. However, extraction is not maximal (0.66+) because legitimate coordination value exists (peer review does provide some quality filtering, journals do maintain knowledge infrastructure) and partial exit options exist for well-resourced actors. The trajectory from 0.32 to 0.58 reflects consolidation (Elsevier, Springer, Wiley controlling ~50% of subscription market) and pricing acceleration. Suppression (0.68): High. Multiple barriers to exit: career incentive lock (publication in high-impact journals required for advancement), journal bundling (institutions cannot negotiate journal-by-journal), technical barriers (paywall infrastructure, licensing restrictions), and normative suppression (prestige hierarchy centered on traditional journals). However, suppression is not maximal (0.85+) because visible alternatives now exist and adoption is increasing. Theater ratio (0.65): Moderate-high. Peer review has become substantially performative: reviewers assess novelty and plausibility but cannot verify reproducibility, access raw data, or detect methodological errors that distributed communities on preprint servers now catch. The ritual persists through inertia and career incentive lock, not because it provides superior quality filtering. The increase from 0.48 to 0.65 reflects growing recognition of peer review limitations (speed, bias, fraud bypass) while the mechanism remains institutionally entrenched.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position and exit options. Publishers see pure coordination (Rope) — they are providing quality signaling and distribution infrastructure, solving the real problem of credentialing research claims. Early career researchers see pure extraction (Snare) — they are locked in by career incentives and extract zero value from the prestige system beyond survival credentials. Resource-constrained institutions see mixed coordination and extraction (Tangled Rope) — they benefit from access to global knowledge but pay disproportionate rents with limited negotiating power. The Open Access Coalition sees a temporary problem with visible exit pathways (Scaffold) — preprint servers establish priority, funder mandates shift incentives, and alternative reputation mechanisms are emerging. The Global South institution sees even steeper tangled_rope dynamics than wealthy institutions — coordination benefit is identical (access to research) but asymmetric extraction is amplified by lower budgets and less coalition leverage. The journal editorial system sees its own degraded ritual (Piton) — editors recognize that peer review persists through institutional inertia rather than functional superiority, with high theater and diminishing quality returns. The civilizational analytical observer risks seeing an inevitable natural law (Mountain) — credentialing research claims requires gatekeepers — but the structural data reveals contingency: distributed mechanisms (arXiv, blockchain peer review, institutional attestation) provide alternative credentialing without monopolistic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across agent positions in this constraint. For-profit publishers occupy low d (0.05-0.15 range) as beneficiaries with arbitrage exit options — they experience negative effective extraction chi, where the constraint subsidizes them. Early-career researchers occupy high d (0.85-0.95 range) as trapped victims — they experience maximum effective extraction chi. Moderate institutions (resourced enough to negotiate but not enough to self-publish) occupy mid-range d (0.55-0.65 range) with constrained exit options. Global South institutions occupy d closer to 0.75 due to victim status and constrained exit options. The open access coalition occupies d around 0.40-0.50 with mobile exit options — they are organized enough to act and can exit toward preprint alternatives, reducing experienced extraction. The peer review ritual (from editorial system perspective) occupies d around 0.60-0.70 — editors are both constrained victims (required to manage peer review) and moderate-power actors with some agency. The derivation from beneficiary/victim declarations and power/exit combinations produces these d values without additional computation — structural position alone determines directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint simultaneously exhibits both coordination and extraction functions, and the classification hinges on identifying which function is primary and which is parasitic. The tangled_rope classification correctly captures that the system provides genuine coordination (peer review quality signaling, knowledge distribution, global research integration) AND genuine extraction (monopolistic paywalls, free labor capture, knowledge access restriction). The mandatrophy resolution distinguishes these: peer review coordination COULD be provided by distributed mechanisms (arXiv, institutional peer review committees, blockchain-based review) without the extraction (paywalls, monopolistic pricing). Therefore, the extraction is not intrinsic to the coordination function — it is layered atop a replaceable mechanism. The constraint is tangled_rope, not rope (which would indicate coordination with minimal extraction) and not snare (which would indicate extraction masquerading as coordination). The beneficiary (for-profit publishers) benefits from BOTH the coordination value (they manage legitimate infrastructure) AND the extraction (they monopolize access to coordinated knowledge). The victims (researchers, institutions, global south) experience both: they receive coordination benefit (access to research) and extraction cost (paywalls, labor appropriation). The sunset clause visible in the open access perspective (arXiv maturation, Plan S mandates, institutional repositories) is a real structural feature — the coordination function has viable alternatives that would reduce extraction. The constraint is transitional, not permanent. The piton perspective correctly identifies peer review theater at 0.65 and reveals that the ritual persists through inertia and prestige narratives rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_gate_necessity,
    'Is centralized journal gatekeeping (managed by for-profit publishers) the necessary mechanism for maintaining research quality, or is it contingent institutional arrangement that could be replaced by distributed mechanisms?',
    'Longitudinal comparison of research quality metrics across publication venues with different peer review structures (traditional journals, preprint + distributed commentary, blockchain-based peer review, institutional peer review committees). Analysis of fraud, retraction, and confirmation rates.',
    'If necessary: journal gatekeeper extraction becomes justified coordination cost. If contingent: the entire suppression and extraction structure is revealed as rent-seeking layered onto a replaceable mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quality_gate_necessity, empirical, 'Whether quality gatekeeping requires centralized journal control').

omega_variable(
    career_signaling_alternative,
    'Can alternative reputation mechanisms (h-index on author profiles, institutional attestation, preprint citation rates, open peer review records) replace journal impact factor as a career signaling tool?',
    'Adoption rates and predictive validity of alternative metrics in hiring and funding decisions across discipline. Hiring committee surveys. Correlation between alternative metrics and research impact.',
    'If alternatives viable: early-career powerless agents have exit option (mobile, not trapped). Entire snare classification becomes tangled_rope. If alternatives fail: career signaling remains journal-locked, and trapped exit persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_signaling_alternative, empirical, 'Whether alternative career signaling can replace journal impact').

omega_variable(
    subscription_model_sustainability,
    'Is the for-profit journal business model sustainable if institutional repositories and author self-archiving become normative, or does it depend on active suppression of these alternatives?',
    'Economic modeling of journal revenue under scenarios of increasing Green Open Access adoption. Behavioral analysis of publisher actions against preprint servers and institutional repositories (litigation, API blocking, embargo pressures). Market analysis of journal cancellations and offsetting agreements.',
    'If sustainable independently: extraction is justified by publisher value-add. If dependent on suppression: the suppression metric (0.68) is the mechanism that maintains extractiveness, revealing core rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subscription_model_sustainability, empirical, 'Whether journal business model requires suppression of alternatives').

omega_variable(
    peer_review_effectiveness,
    'Does traditional journal peer review provide quality filtering substantially better than distributed preprint commentary or other mechanisms?',
    'Meta-analysis of peer review effectiveness: fraud detection rates, bias toward replication failures, speed of error correction, correlation with downstream citation rates and confirmations. Comparative analysis with arXiv/bioRxiv ecosystems.',
    'If effective: piton classification is incorrect; peer review theater has real function, reducing theater_ratio. If ineffective: theater_ratio ≥ 0.70 confirmed, and peer review is ritual rather than mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_effectiveness, empirical, 'Whether peer review provides substantial quality filtering').

omega_variable(
    institutional_power_asymmetry,
    'Do wealthy institutions have meaningfully different exit options than resource-constrained institutions, or does the globalization of journal access collapse the distinction?',
    'Analysis of negotiating power in consortium subscriptions, ability to cancel journals, access to preprints via informal networks, institutional repository uptake rates. Comparison of journal subscriptions per capita and per research output across institution types.',
    'If asymmetry persistent: global_south victim status is structural and permanent. If collapsing: exit options become more symmetric, moving classification toward rope rather than tangled_rope/snare for institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_power_asymmetry, empirical, 'Whether institutional wealth determines exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aprg_tr_t0, academic_peer_review_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aprg_tr_t15, academic_peer_review_gatekeeping, theater_ratio, 15, 0.58).
narrative_ontology:measurement(aprg_tr_t30, academic_peer_review_gatekeeping, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(aprg_be_t0, academic_peer_review_gatekeeping, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aprg_be_t15, academic_peer_review_gatekeeping, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(aprg_be_t30, academic_peer_review_gatekeeping, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, research_funding_concentration).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, metric_fixation_goodhart).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, knowledge_access_inequality).

% DUAL FORMULATION NOTE:
% Academic journal gatekeeping decomposes into two structurally distinct constraints: (1) Peer review quality assurance (ε ≈ 0.15, mountain from technical perspective) — the intrinsic requirement to verify research claims before dissemination; (2) For-profit monopolistic extraction (ε ≈ 0.58, tangled_rope from structural perspective) — the rent-seeking layered atop peer review through paywalls, bundling, and prestige capture. These two stories are linked: the technical requirement has been captured by institutional arrangements that extract value from both researchers and institutions. The quality assurance function could be performed without extraction (via preprint + distributed review + institutional attestation), but the extraction persists because it has become institutionally entrenched through prestige hierarchies and career incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_peer_review_gatekeeping, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
