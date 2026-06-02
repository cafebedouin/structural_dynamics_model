% ============================================================================
% CONSTRAINT STORY: epistemic_authority_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_authority_gatekeeping, []).

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
 *   constraint_id: epistemic_authority_gatekeeping
 *   human_readable: Epistemic Authority Gatekeeping in Knowledge Production
 *   domain: epistemology/institutional_knowledge
 *
 * SUMMARY:
 *   Epistemic authority gatekeeping is the structural mechanism by which
 *   communities determine whose claims are legitimate, which methodologies
 *   are acceptable, and what counts as valid knowledge. This constraint
 *   operates across all knowledge domains — science, medicine, law, policy,
 *   technology, and scholarship. The gatekeeper apparatus (credentialing
 *   institutions, peer review systems, editorial authority, professional
 *   licensing) creates a double function: it genuinely coordinates knowledge
 *   quality and reputation, but simultaneously extracts value from knowledge
 *   producers excluded from the gatekeeping apparatus. The constraint
 *   exhibits striking perspectival variation. To credentialed institutions,
 *   gatekeeping is pure coordination (Rope) — it maintains standards and
 *   enables collaboration. To early-career researchers, it is mixed (Tangled
 *   Rope) — constraining but also enabling through credential pathways. To
 *   excluded producers, it is pure extraction (Snare) — they cannot publish,
 *   cannot build careers, cannot participate in knowledge production without
 *   permission. The open knowledge movement (preprints, community review,
 *   blockchain timestamping, outcome-based reputation) sees gatekeeping as a
 *   temporary institutional arrangement with an expiration date (Scaffold) —
 *   as alternatives mature, traditional gatekeeping loses its coordination
 *   necessity. The analytical observer risks seeing credentialing as an
 *   immutable law (Mountain) — that knowledge production inherently requires
 *   authority structures — but this naturalizes what is actually a contingent
 *   institutional choice.
 *
 * KEY AGENTS:
 *   - Credentialed Institutions: Primary beneficiary (institutional/arbitrage) — universities, journals, professional societies that control knowledge legitimacy and funding concentration
 *   - Established Expert Networks: Primary beneficiary (institutional/arbitrage) — senior researchers whose reputation and credentials are highly valuable within the system
 *   - Knowledge Producers Without Credentials: Primary victim (powerless/trapped) — independent researchers, community scientists, practitioners with experiential knowledge but no institutional affiliation
 *   - Alternative Methodologies: Secondary victim (powerless/trapped) — qualitative research, community-based approaches, indigenous knowledge systems, non-peer-reviewed methodologies that fall outside credentialing frameworks
 *   - Epistemic Commons: Tertiary victim (powerless/trapped) — the shared knowledge resource contaminated by false claims and impoverished by suppressed alternatives
 *   - Early-Career Researchers: Mixed (moderate/constrained) — locked into credentialing pathways but also benefiting from credential recognition
 *   - Peer Review System: Institutional actor (institutional/constrained) — enforces gatekeeping through largely performative review rituals maintained by institutional inertia
 *   - Open Knowledge Movement: Organized agents (organized/mobile) — arXiv, PubPeer, community review platforms, blockchain-timestamped work building alternative epistemic authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_authority_gatekeeping, 0.58).
domain_priors:suppression_score(epistemic_authority_gatekeeping, 0.65).
domain_priors:theater_ratio(epistemic_authority_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_authority_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_authority_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_authority_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_authority_gatekeeping, tangled_rope).
narrative_ontology:human_readable(epistemic_authority_gatekeeping, "Epistemic Authority Gatekeeping in Knowledge Production").
narrative_ontology:topic_domain(epistemic_authority_gatekeeping, "epistemology/institutional_knowledge").

domain_priors:requires_active_enforcement(epistemic_authority_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_authority_gatekeeping, credentialed_institutions).
narrative_ontology:constraint_beneficiary(epistemic_authority_gatekeeping, established_expert_networks).
narrative_ontology:constraint_victim(epistemic_authority_gatekeeping, knowledge_producers_without_credentials).
narrative_ontology:constraint_victim(epistemic_authority_gatekeeping, alternative_methodologies).
narrative_ontology:constraint_victim(epistemic_authority_gatekeeping, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED KNOWLEDGE PRODUCER (SNARE) — Structurally trapped outside the credentialing apparatus. Cannot publish in high-status venues without institutional affiliation or recognized credentials. Faces publication bias, citation suppression, and professional marginalization. Cannot exit without abandoning knowledge production entirely. Experiences maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER RESEARCHER (TANGLED ROPE) — Constrained by credential requirements, advisor dependencies, and career risk. But also benefits from the gatekeeping system: credentials provide recognition, institutional affiliation enables resource access and collaboration. Must conform to established methodologies to succeed. Mixed extraction and coordination — both locked in and incentivized.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED INSTITUTION (ROPE) — Benefits from gatekeeping through funding concentration, prestige accumulation, and control over knowledge legitimacy. Experiences the constraint as coordination: credentialing enables quality control and reputation maintenance. Net beneficiary with full exit optionality — can arbitrage credentials across domains and build new institutions.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PEER REVIEW SYSTEM (PITON) — The ritualistic enforcement mechanism has largely degraded into performative gatekeeping. Reviews often fail to catch methodological errors, replicate results, or evaluate non-standard approaches fairly. Theater ratio (0.68) reflects that much review effort is theatrical rather than substantive. System persists through institutional inertia and lack of viable alternatives, not functional necessity.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized actors (open-access platforms, preprint servers, community review systems) are building alternative legitimacy pathways that bypass traditional credentialing. Sees gatekeeping as a temporary coordination failure with sunset logic: decentralized verification, blockchain-timestamped work, and reputation systems based on outcomes rather than credentials are creating parallel epistemic authority structures. Suppression is declining as alternatives mature.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HETERODOX RESEARCH COMMUNITY (TANGLED ROPE) — Communities using non-standard methodologies (complexity science, qualitative synthesis, indigenous knowledge integration) experience both extraction and coordination from the gatekeeping system. Extraction: their work is systematically undervalued and faces publication barriers. Coordination: the same gatekeeping creates strong in-group identity and alternative peer review networks that strengthen heterodox community bonds. Power level reflects that some heterodox communities (e.g., environmental justice science) have organizational capacity, but face systematic suppression.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some epistemic authority structure is inherent to knowledge production: claims need verification, experts need training, coordination requires standards. This perspective risks naturalizing contingent credentialing arrangements as immutable laws of how knowledge works. The engine's false summit detector identifies this as naturalization — the architectural details of WHO holds authority and on WHAT basis are not laws of nature.
constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_authority_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_authority_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_authority_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_authority_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(epistemic_authority_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: (1) credential rent-seeking — gatekeeping institutions capture funding and prestige by monopolizing legitimacy, (2) access restriction — excluded producers must redirect effort toward gaining credentials rather than producing knowledge, (3) methodological suppression — alternative approaches are systematically undervalued, (4) time tax — gatekeeping delays knowledge spread through publication cycles. The value reflects that extraction is real and substantial but not absolute — some knowledge does reach audiences through alternative channels, and credential holders also bear costs (citation pressure, conformity expectations). Suppression (0.65): High. Barriers to exit are significant: no alternative is fully established yet (open science still gaining legitimacy), credential access requires years of effort and resources, publication outside traditional channels brings career risk, and internalized gatekeeping norms make exit feel impossible even for those with structural alternatives. Theater ratio (0.68): High and increasing. Traditional peer review has evolved toward performative gatekeeping: reviewers cannot assess raw data quality, reproduce experiments, or evaluate non-standard methodologies fairly. The theater serves primarily to maintain institutional legitimacy rather than ensure quality — much review labor is spent on formatting, framing, and conformity assessment rather than substantive verification. The measurement trajectory (0.42 → 0.68 over 40 years) shows increasing theater as disciplinary specialization has made cross-field review impossible.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between credentialed institutions and excluded producers is maximal: the same constraint is rope (coordination) from one perspective and snare (extraction) from the other. This gap reveals the core structural asymmetry. Credentialed institutions see gatekeeping as legitimate quality control because they are the beneficiaries. Excluded producers see it as pure extraction because they bear all costs with no benefit. The early-career researcher occupies a liminal position — they experience both coordination (credentials open doors) and extraction (they must conform and reproduce the system). The open knowledge movement's scaffold perspective shows that the gatekeeping constraint is not inevitable — alternative epistemic authority models are technically possible and are being constructed. This undermines the false mountain perspective: credentialing structures are contingent institutional choices, not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's position in the extraction flow. Beneficiaries (credentialed institutions, established experts) receive low or negative d values — gatekeeping subsidizes them. Their exit options are arbitrage (they can move between institutions, establish new ones, shift fields). Victims (excluded producers, alternative methodologies) receive high d values — gatekeeping extracts from them through publication barriers, citation suppression, and credential requirements. Their exit options are trapped (cannot participate in knowledge production without permission) or constrained (can access alternatives but at high cost). Organized actors (open knowledge movement) have moderate d reflecting their capacity to create alternatives — exit options are mobile, not trapped. The early-career researcher's directionality is intermediate: they are structurally mobile (can gain credentials) but functionally constrained (years of time investment, opportunity cost). The epistemic commons has no agency and cannot exit — pure trap at powerless. The peer review system's directionality is institutional/constrained: it maintains extraction mechanisms (gatekeeping) but cannot independently exit its role.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy through perspectival decomposition. The false natural law (mountain) claims gatekeeping is inherent to knowledge production — that all knowledge requires credentialing and peer review. But the structural data and the open knowledge movement's scaffold perspective show this is naturalization of a contingent institutional arrangement. The constraint is genuinely mixed (Tangled Rope) at institutional analysis: gatekeeping does provide coordination (quality control, reputation maintenance, expertise signaling) AND extraction (access restriction, rent-seeking, methodological suppression). Both functions are real. The mandatrophy is resolved by showing that the coordination function can be preserved through alternative mechanisms (community verification, outcome-based reputation, decentralized timestamping) that reduce extraction. The endpoint is not 'eliminate all authority' but 'decouple coordination from extraction' — move from Tangled Rope (mixed) toward Rope (pure coordination) through institutional redesign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_proxy_validity,
    'Do credentials (degrees, institutional affiliation, publication record) reliably predict knowledge quality, or have they become pure status markers decoupled from actual capability?',
    'Empirical tracking of credential holders'' outcomes vs non-credentialed producers; meta-analysis of credential-blinded vs credential-visible peer review quality assessments',
    'If credentials predict quality: gatekeeping is justified coordination (Rope from more perspectives). If decoupled: gatekeeping is pure extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_proxy_validity, empirical, 'Whether credentials predict knowledge quality or are decoupled status markers').

omega_variable(
    alternative_verification_sufficiency,
    'Do decentralized verification mechanisms (community review, outcome tracking, reputation systems, blockchain timestamping) provide equal or superior epistemic reliability compared to traditional peer review?',
    'Comparative error detection rates; longitudinal tracking of claim validity; replication success rates across traditional vs alternative verification pathways',
    'If alternatives are sufficient: scaffold is real structural transition (open knowledge sunset is legitimate). If inferior: gatekeeping is necessary coordination despite extraction overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether alternative verification mechanisms are epistemically sufficient').

omega_variable(
    credentialing_cost_distribution,
    'What proportion of gatekeeping suppression is structural vs identity-locked? Are excluded producers unable to exit (trapped/constrained barriers) or do they experience their exclusion as inherent to who they are (identity-locked)?',
    'Post-entry analysis: when previously-excluded producers gain credentials, do suppression and extraction immediately decline or do internalized barriers persist? Longitudinal identity-transformation tracking.',
    'If primarily structural: targeted credential access reduces extraction. If primarily identity-locked: credential access alone insufficient; cognitive reframing required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_cost_distribution, empirical, 'Structural vs internalized suppression in credentialing barriers').

omega_variable(
    epistemic_commons_recovery,
    'How much damage to the epistemic commons (contamination by false claims, loss of alternative methodologies, reduced innovation diversity) persists even after the constraint is recognized and is what is the recovery timeline?',
    'Inventory of lost methodological traditions; tracking of false-positive claims in literature; diversity metrics for research approaches; post-reform diversification rates',
    'If recovery is rapid and complete: constraint is Tangled Rope (short-term extraction, long-term recovery). If recovery is slow or incomplete: constraint is Snare (permanent epistemic damage).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_recovery, empirical, 'Epistemic commons damage and recovery timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_authority_gatekeeping, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epauth_tr_t0, epistemic_authority_gatekeeping, theater_ratio, 0, 0.42).
narrative_ontology:measurement(epauth_tr_t20, epistemic_authority_gatekeeping, theater_ratio, 20, 0.55).
narrative_ontology:measurement(epauth_tr_t40, epistemic_authority_gatekeeping, theater_ratio, 40, 0.68).
narrative_ontology:measurement(epauth_tr_t10, epistemic_authority_gatekeeping, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(epauth_be_t0, epistemic_authority_gatekeeping, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(epauth_be_t20, epistemic_authority_gatekeeping, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(epauth_be_t40, epistemic_authority_gatekeeping, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(epauth_be_t10, epistemic_authority_gatekeeping, base_extractiveness, 10, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_authority_gatekeeping, information_standard).
narrative_ontology:boltzmann_floor_override(epistemic_authority_gatekeeping, 0.08).
narrative_ontology:affects_constraint(epistemic_authority_gatekeeping, credential_access_inequality).
narrative_ontology:affects_constraint(epistemic_authority_gatekeeping, publication_bias_in_knowledge).
narrative_ontology:affects_constraint(epistemic_authority_gatekeeping, indigenous_knowledge_suppression).

% DUAL FORMULATION NOTE:
% Epistemic authority gatekeeping is the upstream structural constraint. Three downstream constraints decompose specific instantiations: credential access (focusing on how identity and resource barriers prevent entering the credentialing system), publication bias (focusing on how gatekeeping filters knowledge selection), and indigenous knowledge suppression (focusing on how credentialing frameworks systematically devalue non-scientific knowledge forms). Each downstream constraint has its own ε value reflecting domain-specific extractiveness patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_authority_gatekeeping, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
