% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Proprietary Licensing Frame)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   This constraint story captures the 'property rights reading' of software
 *   control legitimacy — the frame that treats software as a form of property
 *   where creators hold legitimate authority to restrict use, modification,
 *   and distribution. This reading underpins the proprietary software
 *   industry, venture capital models, and modern IP enforcement regimes (DMCA
 *   1201, software patents, EULA enforceability). The constraint is claimed
 *   as a tangled_rope because it performs a genuine coordination function
 *   (solving the public-goods problem of software investment) while
 *   simultaneously extracting asymmetric costs from FOSS advocates, users,
 *   researchers, and interoperability seekers. Active enforcement (legal,
 *   technical, contractual) is required to maintain the exclusion boundary.
 *   The metrics reflect the standing arrangement under contest: the
 *   proprietary licensing system as it operates today, not the reading's
 *   endorsed ideal. The kernel context: this is one of four readings of the
 *   'software_control_legitimacy' kernel. The property_rights_reading
 *   coexists with freedom_imperative, pragmatic_openness, and commons
 *   readings — no single reading forecloses the others in public discourse,
 *   though each creates structural pressure on the others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.42).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Proprietary Licensing Frame)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '1f6866dc-cc71-4f75-8ddb-c8be1974c24b').
narrative_ontology:cs_kernel_codification('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', formalized).
narrative_ontology:cs_authority_grounding('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', extraction).
narrative_ontology:cs_interpretation_layer_present('1f6866dc-cc71-4f75-8ddb-c8be1974c24b').
narrative_ontology:cs_reading_relation('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', foundational, creator_has_exclusive_control_right).
narrative_ontology:cs_axiom_status(creator_has_exclusive_control_right, holdable).
narrative_ontology:cs_axiom_grounding('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', creator_has_exclusive_control_right, deontological).
narrative_ontology:cs_axiom('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', foundational, exclusion_necessary_for_commercial_investment).
narrative_ontology:cs_axiom_status(exclusion_necessary_for_commercial_investment, holdable).
narrative_ontology:cs_axiom_grounding('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', exclusion_necessary_for_commercial_investment, instrumental).
narrative_ontology:cs_reference_frame('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', proprietary_software_industry_establishment).
narrative_ontology:cs_drift_state('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', cloud_saas_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f6866dc-cc71-4f75-8ddb-c8be1974c24b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, ip_holding_companies).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users_restricted_by_eula).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, independent_security_researchers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_seekers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, property_rights_incentivize_innovation).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, commercial_sustainability_requires_exclusion).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, creator_authority_over_artifact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce proprietary licensing terms (EULAs, subscription models, API restrictions) that define permissible use, modification, and distribution. Their business models depend on the legal enforceability of these restrictions. They invest in lobbying for stronger IP enforcement and DRM legal protections (DMCA 1201, EUCD Article 6). They can pivot across licensing models but the property-right frame is the legal foundation of their asset valuation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide capital predicated on the enforceability of software IP as an asset class. Returns depend on portfolio companies' ability to exclude non-payers and control distribution channels. They do not administer licenses but their capital allocation reinforces the property-right frame as the dominant governance model. Exit is liquid and diversified across the software economy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Acquire and monetize software patent portfolios and copyrights without producing software. Their revenue is pure extraction from the licensing constraint — they are the most concentrated beneficiaries. They lobby for expansive patent eligibility and statutory damages. They have no operational dependency on any particular software artifact; their exit is selling the portfolio.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, ip_holding_companies, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, ip_holding_companies, agenda_setter).

% Build and maintain free/open-source ecosystems (Linux, GCC, PostgreSQL, etc.) that are structurally incompatible with proprietary enclosure. They bear the cost of: (a) legal compliance overhead navigating patent thickets and license compatibility, (b) defensive patent pools (OIN) and copyright assignments, (c) foregone network effects when proprietary platforms exclude interoperability, (d) ideological friction — the property-right frame treats their mode of production as an exception to be contained rather than a legitimate parallel. Exit means abandoning the free software identity and community that constitutes their professional and ethical self-concept.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, identity_locked, global).

% Cannot modify, repair, or inspect software they depend on (medical devices, tractors, IoT, SaaS). Bear costs: vendor lock-in, forced upgrades, inability to audit for security/privacy, repair monopolies. Alternatives exist but switching costs are high (data portability, retraining, ecosystem lock-in). Exit is possible but expensive — they are constrained, not trapped.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users_restricted_by_eula, payer,
    moderate, biographical, constrained, global).

% Face legal risk (DMCA 1201, CFAA) when reverse-engineering proprietary software to find vulnerabilities. The property-right frame criminalizes their coordination function (independent audit). They bear the cost of chilled research and vendor-controlled disclosure timelines. Some exit to authorized bug-bounty programs (constrained exit), others operate pseudonymously (higher risk).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, independent_security_researchers, payer,
    powerless, biographical, constrained, global).

% Need to build compatible products (competing clients, migration tools, accessibility layers). Proprietary protocols, encrypted interfaces, and legal threats (API copyright claims, anti-circumvention) block them. They bear the cost of clean-room reimplementation or vendor permission regimes. Exit means abandoning the integration target — constrained by market demand.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_seekers, payer,
    moderate, biographical, constrained, global).

% Evaluate whether IP-based exclusion crosses into anti-competitive conduct (refusal to license, tying, predatory API changes). They can impose remedies (mandatory licensing, interoperability mandates) that reshape the constraint's enforcement boundary. Their seat is analytical — they do not collect rents nor pay them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, enforceable framework for commercial software investment: creators can recoup R&D, investors can price risk, users get a defined product with vendor accountability. Solves the public-goods problem of software (zero marginal cost, high fixed cost) by creating excludability.
% TRANSFER_FUNCTION: Moves licensing revenue and control rights from users/modifiers/researchers to vendors and IP holders. Users pay monetary fees and surrender freedoms (modify, repair, inspect, redistribute); vendors receive recurring revenue and strategic control over the software's evolution and ecosystem.
% ABSENT_VOICES: Global South software developers who cannot afford proprietary toolchains and are excluded from proprietary ecosystems' network effects. Users in jurisdictions with weak consumer protection who bear the full cost of lock-in without regulatory recourse. Future maintainers of abandoned proprietary software (orphaned works) who have no legal path to sustain it.
% DISAPPEARANCE_RATIONALE: If proprietary licensing's legal enforceability vanished overnight: (1) Vendors would shift to SaaS/hosted models where control is technical not legal, (2) FOSS ecosystems would absorb displaced development effort, (3) Investment capital would reprice software risk — likely toward service/hosting models, (4) Interoperability would become the default competitive axis. The software economy would reorganize around operational control (servers, keys, data) rather than copyright/patent exclusion.
% FOUNDING_PROBLEM: Early commercial software (1970s-80s) faced rampant unauthorized copying with no technical protection. The property-right frame (copyright for software, later patents) was adopted to make software a tradeable asset, enabling a commercial industry to form.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and investors attest the problem is live: piracy persists, and new threats (AI model weights, cloud API scraping) require strong IP. FOSS advocates and competition economists attest the founding problem is substantially solved — the industry exists, technical protection (SaaS, DRM, TPMs) now does what legal exclusion did, and the property-right frame has expanded beyond its original scope (patenting algorithms, copyrighting APIs, anti-circumvention on owned devices). Independent economic historians (e.g., Bessen & Meurer, Boldrin & Levine) document that software innovation thrived under weak IP in the 1980s-90s and that patent expansion correlated with increased litigation, not increased R&D.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48) is moderate: the constraint coordinates a real industry (hence not pure extraction) but the property-right frame has expanded well beyond recouping R&D — into API copyright, anti-circumvention on owned devices, patent thickets that tax independent development. Suppression (0.42) is significant but not total: alternatives (FOSS, SaaS) exist and grow, but face structural barriers (network effects, legal risk, capital access). Theater (0.22) is present but not dominant: the 'incentivize innovation' justification is real but increasingly decoupled from marginal enforcement actions (e.g., suing security researchers, blocking repair). Accessibility collapse (0.35) reflects that alternatives exist but are structurally disadvantaged. Resistance (0.58) is high: FOSS growth, right-to-repair movements, interoperability mandates (DMA), and judicial pushback (Google v. Oracle) show active contestation. The claim/metric independence is deliberate: the reading CLAIMS rope/coordination; the metrics describe a structure with substantial extractive overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/investor seat, the constraint is a rope: it coordinates capital allocation and risk-bearing for software creation. From the FOSS advocate seat, it is a snare: the coordination story is cover for enclosing a commons that would self-sustain. From the end-user seat, it is a tangled rope: they get a supported product but lose agency over their computing. The engine computes per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and IP holders are structural beneficiaries (collect licensing revenue, control ecosystem evolution — d near 0). Investors benefit indirectly via asset valuation (d ~ 0.15). FOSS advocates are identity-locked payers: their mode of production is structurally excluded by the property-right frame's legal architecture (patents, DMCA, API copyright), and exit means abandoning their professional/ethical identity (d ~ 0.9). End users, researchers, and interoperability seekers are constrained payers: they bear costs but have some exit paths, however expensive (d ~ 0.6-0.75). Competition authorities are analytical observers (d = 0.5). The engine derives d from these structural positions; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling a commercial software industry) is contested as live vs. substantially solved. The property-right frame persists because: (a) it benefits concentrated actors who administer it, (b) technical enforcement (SaaS, TPMs) has made legal exclusion partially redundant but the legal frame remains the fallback and expansion vector, (c) no political coalition has formed to roll back software patents or DMCA 1201. The constraint shows mandatrophy signals: the original justification (prevent copying to enable commerce) has been superseded by technical means, but the legal frame expands into new territories (API copyright, algorithm patents, repair restrictions) — classic mandate drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_frame_vs_technical_control_substitution,
    'To what extent has technical enforcement (SaaS, TPMs, encrypted boot, cloud hosting) substituted for legal property rights as the actual exclusion mechanism, and does the legal frame persist primarily as a legacy artifact or as an active expansion vector?',
    'Counterfactual analysis: if copyright/patent/DMCA were removed but technical controls remained, how much would vendor control and revenue change? Measure the marginal contribution of legal vs. technical exclusion.',
    'If technical controls are the primary exclusion mechanism, the property-right frame''s extractiveness is overstated in current metrics — the real constraint is technical architecture. If the legal frame actively expands (new statutes, broader interpretations), it remains an independent extraction vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_frame_vs_technical_control_substitution, empirical, 'Whether legal property rights or technical architecture is the binding constraint on software freedom today.').

omega_variable(
    foss_identity_lock_mechanism,
    'Is the FOSS advocate''s identity_locked exit option driven by (a) genuine community/professional identity fusion, (b) structural exclusion from proprietary ecosystems (network effects, capital), or (c) ideological commitment that makes exit conceptually incoherent?',
    'Longitudinal study of FOSS contributors who transition to proprietary roles: do they experience identity rupture, or is the transition friction primarily economic/network? Compare with other identity-locked populations (religious, professional).',
    'If (a) or (c), the identity_locked classification is structurally robust — the constraint binds at the self-concept level. If (b), the ''identity'' framing masks a structural trap (network effects + capital) that could be relieved by policy (interoperability mandates, public funding).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foss_identity_lock_mechanism, conceptual, 'What mechanism binds FOSS advocates to their position — identity fusion or structural exclusion?').

omega_variable(
    coordination_extraction_boundary_shift,
    'At what point does the expansion of IP enforcement (API copyright, algorithm patents, anti-circumvention on repair, model weight protection) cross from coordination-supporting to net-extractive, and can that boundary be identified ex ante?',
    'Historical analysis of IP expansion episodes: correlate scope expansions with R&D investment rates, entry rates, and innovation metrics. Identify inflection points where marginal enforcement reduced rather than increased follow-on innovation.',
    'If a clear boundary exists, the tangled_rope classification is stable — the constraint has a coordination core and extractive periphery. If the boundary is indeterminate or has already been crossed, the constraint may be drifting toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_shift, empirical, 'Whether the property-right frame''s coordination function has a definable limit beyond which it becomes net extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__property_rights_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__property_rights_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__property_rights_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__property_rights_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__property_rights_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__property_rights_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__property_rights_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__property_rights_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__property_rights_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__property_rights_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__property_rights_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__property_rights_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.15).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, drm_anti_circumvention).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_patent_eligibility).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, api_copyright_doctrine).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, right_to_repair_software).

% DUAL FORMULATION NOTE:
% Part of the software_control_legitimacy constraint family. This reading (property_rights) provides the dominant legal/institutional frame that the other three readings react to, navigate, or negotiate within. The freedom_imperative reading defines itself in opposition; pragmatic_openness operates within the frame's interstices; commons_reading proposes a third governance logic. All four share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
