% ============================================================================
% CONSTRAINT STORY: peer_review_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peer_review_gatekeeping, []).

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
 *   constraint_id: peer_review_gatekeeping
 *   human_readable: Peer Review Gatekeeping in Academic Publishing
 *   domain: academic/scientific_publishing
 *
 * SUMMARY:
 *   Peer review gatekeeping in academic publishing creates a structural
 *   tension between legitimate quality control and institutional rent-seeking
 *   through credentialing monopoly. Early-career researchers and
 *   unconventional methodologies bear the costs of gatekeeping delays and
 *   bias, while established researchers and journal editors extract authority
 *   and career control. The constraint exhibits multiple classification types
 *   from different perspectives: pure extraction (snare) for trapped
 *   early-career researchers; mixed coordination-extraction (tangled rope)
 *   for the research community that depends on but is harmed by the system;
 *   pure coordination (rope) for institutional beneficiaries; and degraded
 *   ritual (piton) for an institution recognizing its own dysfunction. The
 *   theater ratio (0.68) indicates that peer review processes are
 *   substantially performative — the ritual of expert scrutiny matters
 *   institutionally even when actual review quality is compromised. Open
 *   science movements (preprints, open-access journals, distributed review)
 *   represent sunset mechanisms that could displace traditional gatekeeping
 *   within 15-25 years. The core extraction mechanism is authority
 *   concentration: credentialing power is monopolized by journals and
 *   established reviewers, creating barriers to entry that extract time,
 *   credibility delay, and career opportunity from those outside the
 *   credentialed network.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victims (powerless/trapped) — dependent on peer review for career advancement; no alternative provides equivalent institutional credibility; bear full cost of gatekeeping delays and bias
 *   - Unconventional Methodologies: Primary victims (powerless/trapped) — face systematic gatekeeping by reviewers defending incumbent paradigms; cannot establish legitimacy without passing review by defenders of the paradigm they challenge
 *   - Established Research Groups: Primary beneficiaries (institutional/arbitrage) — gatekeeping bias favors familiar approaches and credentialed researchers; can arbitrage between journals and maintain authority through review networks
 *   - Journal Editors and Credentialed Reviewers: Secondary beneficiaries (institutional/arbitrage) — control gate-keeping authority; experience system as pure coordination (quality filtering, knowledge organization); minimal extraction cost because system allocates authority to their layer
 *   - Field Research Community: Mixed victims and beneficiaries (moderate/constrained) — benefit from quality control and reputation coordination; harmed by gatekeeping delays, methodology suppression, and arbitrary review variance; trapped by prestige monopoly
 *   - Open Science Movement: Organized agents (organized/mobile) — building alternative credentialing pathways (preprints, open-access journals, distributed review); see traditional gate as temporary with sunset logic via distributed reputation systems
 *   - Peer Review Institution: Degraded beneficiary (institutional/arbitrage) — recognizes its own dysfunction; maintains ritual through inertia; theater ratio reflects performative character
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peer_review_gatekeeping, 0.58).
domain_priors:suppression_score(peer_review_gatekeeping, 0.62).
domain_priors:theater_ratio(peer_review_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peer_review_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(peer_review_gatekeeping, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(peer_review_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(peer_review_gatekeeping, "Peer Review Gatekeeping in Academic Publishing").
narrative_ontology:topic_domain(peer_review_gatekeeping, "academic/scientific_publishing").

domain_priors:requires_active_enforcement(peer_review_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peer_review_gatekeeping, established_research_groups).
narrative_ontology:constraint_beneficiary(peer_review_gatekeeping, journal_editors).
narrative_ontology:constraint_beneficiary(peer_review_gatekeeping, credentialed_reviewers).
narrative_ontology:constraint_victim(peer_review_gatekeeping, early_career_researchers).
narrative_ontology:constraint_victim(peer_review_gatekeeping, unconventional_methodologies).
narrative_ontology:constraint_victim(peer_review_gatekeeping, field_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Dependent on peer-reviewed publication for career advancement. No alternative credentialing system achieves institutional recognition. Cannot exit the review system without abandoning academic career prospects. Bears full cost of reviewer rejection, arbitrary delays, and gatekeeping bias. Maximum structural extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNCONVENTIONAL METHODOLOGIES (SNARE) — Innovations that challenge mainstream paradigm face systematic gatekeeping by reviewers invested in existing approaches. No parallel publication pathway provides equivalent institutional credibility. Trapped by epistemic monopoly — cannot establish legitimacy without passing review by those defending the paradigm they challenge.
constraint_indexing:constraint_classification(peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FIELD RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from peer review's quality control and reputational coordination: published papers signal reliability, reviewer feedback improves manuscripts, editorial curation organizes knowledge. But also bears significant extraction: gatekeeping delays innovation, orthodoxy suppresses methodological diversity, and review variance creates arbitrary barriers. High exit costs (alternative journals lack prestige) create constrained mobility.
constraint_indexing:constraint_classification(peer_review_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JOURNAL EDITORS AND CREDENTIALED REVIEWERS (ROPE) — Benefit from institutional authority and gate-control. Experience peer review as pure coordination: filtering noise from signal, maintaining standards, advancing knowledge. Can arbitrage between journals and reviewer networks. Experience minimal extraction because the system allocates authority to their layer.
constraint_indexing:constraint_classification(peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, open-access journals, post-publication review platforms) are building alternative credentialing pathways with sunset logic. ArXiv, bioRxiv, and open-peer-review systems bypass traditional gatekeeping. As distributed reputation systems mature (blockchain credentials, decentralized review networks), the traditional monopoly's extraction mechanism weakens. Estimated sunset: 15-25 years for institutional recognition to shift toward alternative credentialing.
constraint_indexing:constraint_classification(peer_review_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW INSTITUTION (PITON) — The institution itself recognizes its degradation. Anonymous review quality is declining (reviewer burden increasing, incentive misalignment), editor workload makes gatekeeping rushed, and citation-based metrics have replaced manuscript assessment. The ritual persists through institutional inertia — universities and funding agencies require peer-reviewed publications despite widespread agreement that the system is broken. Theater ratio (0.68) reflects that much peer review activity is performative: the appearance of expert scrutiny matters more than actual rigor.
constraint_indexing:constraint_classification(peer_review_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of quality control is inherent to knowledge production: claims always require verification, and gatekeeping lag is an inescapable feature of how consensus forms. However, structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'gatekeeping is inherent to science' naturalizes what is actually a contingent institutional choice about whose authority counts.
constraint_indexing:constraint_classification(peer_review_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peer_review_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peer_review_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peer_review_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system extracts time (review delays average 6-12 months), credibility (early-career researchers cannot publish until established researchers grant permission), and opportunity (gatekeeping bias systematically favors incumbent paradigms). But extraction is not total — some research gets published, alternatives exist (preprints), and the system provides genuine quality filtering benefit. The upward trajectory over 20 years reflects increasing gatekeeping stringency as journal prestige concentration has grown. Suppression (0.62): High. Multiple barriers prevent exit: alternative journals lack prestige (reputation monopoly), academic hiring committees require peer-reviewed publications (institutional lock-in), and funding agencies use publication metrics to allocate resources (systemic dependency). Barriers are not insurmountable but are substantial and structural. Theater ratio (0.68): High. Peer review rituals are increasingly performative: anonymous review quality declines as reviewer workload increases; editorial decisions are rushed; citation-based metrics replace manuscript assessment; the appearance of expert scrutiny matters institutionally despite acknowledged dysfunction. The theater has increased over 20 years as system strain has grown.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between authority-holding institutions (rope: beneficiaries experience coordination) and authority-excluded actors (snare: victims experience pure extraction). Established researchers and journal editors see quality control and reputation management — genuine coordination functions. Early-career researchers and unconventional methodologies see arbitrary barriers and biased gatekeeping — extraction mechanisms. The field research community, needing both coordination benefits and experiencing gatekeeping harm, occupies the tangled rope middle: the system provides essential infrastructure (journal organization, quality signals, reputational coordination) while also suppressing innovation and delaying knowledge diffusion. The perspectival gap is bridged by recognizing that peer review provides both functions simultaneously — the same gatekeeping that maintains quality standards also maintains authority concentration and extracts opportunity costs from those outside the credentialed network.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position in the extraction flow. Beneficiaries with arbitrage options (established researchers, editors, reviewers) derive d from their ability to move between journals, maintain authority networks, and capture career benefits from gatekeeping monopoly — low d values producing negative or near-zero chi (they experience the constraint as beneficial coordination). Victims with trapped or constrained exit options (early-career researchers, unconventional methodologies) derive d from their dependency on the gatekeeper's permission and lack of alternative credentialing paths — high d values producing high chi (they experience gatekeeping as extraction). The research community's moderate power and constrained exit (can publish in lower-prestige journals but loses status) produces mid-range d values and moderate chi. The systematic upward trajectory of both extractiveness and theater ratio indicates increasing institutional degradation: the system's extraction is growing not because coordination functions have improved but because gatekeeping control is concentrating (top journals receiving increasing proportion of submissions) while review quality is declining (theater rising as performative content dominates actual scrutiny).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that peer review is genuinely hybrid: it coordinates quality signals and knowledge organization (rope functions) while also extracting time, credibility, and opportunity (snare/tangled rope functions) from those outside the credentialed center. The mandatrophy is resolved by accepting that both readings are structurally accurate — the system IS a coordination mechanism AND an extraction mechanism simultaneously, but these functions are distributed asymmetrically. Beneficiaries experience coordination; victims experience extraction; the community experiences both. The architectural question is whether decomposition is possible: can the coordination functions (quality filtering, reputation signaling, knowledge organization) be separated from the extraction mechanisms (credentialing monopoly, gatekeeping bias, prestige concentration)? Open science alternatives suggest yes — distributed review networks, preprint scrutiny, and decentralized credentials can provide coordination benefits with lower extraction cost. The piton classification indicates institutional recognition of this decomposition: the peer review establishment knows its traditional form is degraded but maintains it through inertia. The scaffold classification for the open science movement indicates the sunset is structural and achievable — alternative credentialing pathways are technically feasible and institutionally maturing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_necessity,
    'Is peer review gatekeeping a necessary check on knowledge quality or an institutional monopoly maintaining credentialing authority?',
    'Comparative analysis of publication outcomes: retraction rates, citation patterns, and downstream research productivity for peer-reviewed vs preprint-only vs open-review platforms; measurement of false-positive and false-negative publication rates across systems',
    'If necessary: constraint is closer to Rope (coordination with genuine value). If monopoly: classification remains Snare/Tangled Rope (extraction maintained by authority lock). If mixed: explains why multiple perspectives simultaneously legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_necessity, empirical, 'Whether gatekeeping is necessary quality control or institutional rent-seeking').

omega_variable(
    alternative_credentialing_viability,
    'Can decentralized reputation systems (blockchain credentials, distributed review networks, citation-based metrics) replace institutional peer review in establishing research credibility?',
    'Empirical tracking of alternative credentialing adoption; longitudinal study of whether alternative-credentialed researchers achieve equivalent career outcomes; analysis of employer/funder recognition of non-traditional credentials',
    'If viable: scaffold sunset is achievable (15-25 year transition to alternative systems). If not viable: alternative credentials remain signals of exclusion, and the gate remains locked regardless of formal system changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Viability of decentralized credentialing systems').

omega_variable(
    reviewer_bias_persistence,
    'Is reviewer gatekeeping bias (against unconventional methods, early-career researchers, underfunded institutions) a correctable process issue or a structural feature of credentialed authority?',
    'Systematic analysis of acceptance rates by researcher seniority, methodology type, and institution prestige; audit of reviewer identities and citation patterns; comparison of bias levels before and after blinded-review interventions',
    'If correctable process issue: organizational reform could reduce extraction without system-level change. If structural: the bias is inherent to authority concentration, and only decomposing the gate (via alternatives) addresses it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reviewer_bias_persistence, empirical, 'Whether reviewer bias is correctable or structural').

omega_variable(
    identity_lock_in_credentialing,
    'Are early-career researchers trapped by material barriers (career dependency) or identity-locked by internalized credentialism (belief that peer review gatekeeping is legitimate)?',
    'Surveys and interviews tracking researcher attitudes toward peer review legitimacy; analysis of researcher behavior post-exit (do alternative publishers/preprints receive equal recognition?); measurement of cognitive dissonance between perceived system unfairness and continued participation',
    'If material trap: exit costs are structural (alternative publishers lack prestige). If identity-locked: the trap is partially cognitive — agents could exit if identity frame shifted but cannot see the exit from within credentialist worldview.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_credentialing, empirical, 'Material vs identity-based entrapment in peer review system').

omega_variable(
    journal_extractiveness_concentration,
    'Are high journal rejection rates and restrictive gatekeeping driven by quality control or by artificial scarcity creation (limiting publication supply to maintain prestige and extraction)?',
    'Comparative analysis of editorial decision-making: correlation between rejection rates and journal prestige; measurement of whether acceptance rates have decreased while manuscript quality has increased; analysis of whether high-rejection journals produce higher-impact papers or just more prestigious branding',
    'If quality-driven: extractiveness justifiable as coordination cost. If scarcity-driven: extractiveness is pure rent-seeking through artificial limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(journal_extractiveness_concentration, empirical, 'Whether gatekeeping is quality-filtering or artificial scarcity creation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peer_review_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prg_tr_t0, peer_review_gatekeeping, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prg_tr_t10, peer_review_gatekeeping, theater_ratio, 10, 0.6).
narrative_ontology:measurement(prg_tr_t20, peer_review_gatekeeping, theater_ratio, 20, 0.68).
narrative_ontology:measurement(prg_tr_t5, peer_review_gatekeeping, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(prg_be_t0, peer_review_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prg_be_t10, peer_review_gatekeeping, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(prg_be_t20, peer_review_gatekeeping, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prg_be_t5, peer_review_gatekeeping, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peer_review_gatekeeping, identity_coordination).
narrative_ontology:boltzmann_floor_override(peer_review_gatekeeping, 0.12).
narrative_ontology:affects_constraint(peer_review_gatekeeping, academic_publishing_oligopoly).
narrative_ontology:affects_constraint(peer_review_gatekeeping, research_paradigm_lock_in).
narrative_ontology:affects_constraint(peer_review_gatekeeping, early_career_researcher_precarity).

% DUAL FORMULATION NOTE:
% Peer review gatekeeping is distinct from but coupled with academic publishing oligopoly (journal pricing monopoly) and research paradigm lock-in (theoretical gatekeeping). These three constraints form a constraint family: peer review gatekeeping controls who can publish, publishing oligopoly controls who can access published work, and paradigm lock-in controls what kinds of research get accepted. Decomposition: peer_review_gatekeeping focuses on the credentialing/authority mechanism; academic_publishing_oligopoly focuses on economic extraction through journal pricing; research_paradigm_lock_in focuses on epistemic suppression of unconventional approaches. All three affect early_career_researcher_precarity as a downstream victim. ε values increase along the chain: peer review (0.58) → publishing oligopoly (0.65) → paradigm lock-in (0.72).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peer_review_gatekeeping, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
