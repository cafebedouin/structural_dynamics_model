% ============================================================================
% CONSTRAINT STORY: internet_archive_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_internet_archive_preservation, []).

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
 *   constraint_id: internet_archive_preservation
 *   human_readable: The Internet Archive Preservation-Copyright Conflict
 *   domain: technological/legal
 *
 * SUMMARY:
 *   The Internet Archive preservation-copyright conflict represents a
 *   fundamental structural tension between two legitimate but incompatible
 *   institutional objectives: the cultural imperative to preserve digital
 *   knowledge against entropy and decay, and the legal framework that grants
 *   copyright holders exclusive control over reproduction and distribution.
 *   The Internet Archive (IA), a nonprofit organization founded in 1996,
 *   operates the Wayback Machine and maintains one of the largest digital
 *   collections in existence. Its core mission is to preserve human knowledge
 *   and culture for future generations. However, this mission directly
 *   conflicts with copyright law, particularly the Digital Millennium
 *   Copyright Act (DMCA) and international copyright frameworks that protect
 *   authors' and publishers' exclusive rights to control how their works are
 *   reproduced, distributed, and accessed. The constraint exhibits all six DR
 *   types from different perspectives, revealing how the same structural
 *   phenomenon — digital archiving of copyrighted materials — appears as pure
 *   extraction (snare) to rights holders, as mixed coordination-extraction
 *   (tangled rope) to publishers, as coordination (rope) to IA itself, as a
 *   temporary problem with a legal sunset (scaffold), as performative
 *   enforcement (piton), and as an immutable informational law (mountain).
 *   The theater_ratio (0.58) reflects that copyright enforcement against IA
 *   is substantially performative: cease-and-desist letters, takedown
 *   requests, and litigation threats exceed actual enforcement capacity given
 *   IA's scale. Yet the suppression (0.68) is genuinely high because the
 *   legal framework creates real barriers to IA's operations and the threat
 *   of injunctive relief remains credible. The constraint has become more
 *   extractive and theatrical over the past decade as IA's collection has
 *   grown, publishers have adopted more aggressive enforcement tactics, and
 *   the functional necessity of enforcement has declined (digital
 *   distribution is already decentralized).
 *
 * KEY AGENTS:
 *   - Internet Archive Organization: Primary beneficiary (institutional/arbitrage) — captures mission fulfillment and public trust; faces legal liability but operates within legal uncertainty
 *   - Copyright Holders (Authors/Estates): Primary victim (powerless/trapped) — lose control over works already in IA collection; exit options are nil (works already preserved)
 *   - Publishers: Secondary actor (moderate/constrained) — experience mixed extraction/coordination; depend on IA for legitimacy but seek to constrain access
 *   - Public/Researchers/Accessibility Advocates: Beneficiary (organized/mobile) — gain free access to preserved knowledge; experience suppression through legal restrictions and availability gaps
 *   - Copyright Law Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative compliance mechanisms; functional capacity has atrophied relative to scale
 *   - Legal/Regulatory Framework (Fair Use, Orphan Works Doctrine): Organized actors (organized/constrained) — represent sunset pathways through legislative expansion and judicial clarification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional conflict as an immutable law of information physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_archive_preservation, 0.52).
domain_priors:suppression_score(internet_archive_preservation, 0.68).
domain_priors:theater_ratio(internet_archive_preservation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_archive_preservation, extractiveness, 0.52).
narrative_ontology:constraint_metric(internet_archive_preservation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(internet_archive_preservation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_archive_preservation, tangled_rope).
narrative_ontology:human_readable(internet_archive_preservation, "The Internet Archive Preservation-Copyright Conflict").
narrative_ontology:topic_domain(internet_archive_preservation, "technological/legal").

domain_priors:requires_active_enforcement(internet_archive_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_archive_preservation, public_knowledge_commons).
narrative_ontology:constraint_beneficiary(internet_archive_preservation, accessibility_advocates).
narrative_ontology:constraint_beneficiary(internet_archive_preservation, researchers_historians).
narrative_ontology:constraint_victim(internet_archive_preservation, copyright_holders).
narrative_ontology:constraint_victim(internet_archive_preservation, publishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COPYRIGHT HOLDER (SNARE) — Authors and publishers lack practical ability to opt out of IA preservation without abandoning digital distribution entirely. Exit is costly (move to proprietary platforms) or impossible (works already collected). IA's operations extract value (free preservation, accessibility) against copyright holder will. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86. Pure extraction with high suppression (legal/technical barriers to enforcement).
constraint_indexing:constraint_classification(internet_archive_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLISHER INDUSTRY (TANGLED ROPE) — Publishers benefit from IA as a distribution network and legitimacy tool (preserved works have archival value), but also experience extraction through loss of control over access, pricing, and lifecycle management. Suppression is high (legal frameworks, DRM, takedown notices) but not total — IA finds workarounds. d≈0.70, f(d)≈1.05, σ=1.2 → χ≈0.65. Coordination function (IA preserves publisher cultural output) mixed with asymmetric extraction (publishers lose control).
constraint_indexing:constraint_classification(internet_archive_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNET ARCHIVE (ROPE) — IA frames preservation as a coordination service: solving the collective action problem of cultural memory in the digital age. From IA's structural position, the constraint is a pure coordination mechanism — they provide infrastructure that all parties benefit from (archived works have lasting value). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.09. Net beneficiary perspective; sees copyright law as a coordination tool (establishing attribution, respecting intent), not as an extraction mechanism.
constraint_indexing:constraint_classification(internet_archive_preservation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC KNOWLEDGE COMMONS (TANGLED ROPE) — Accessibility advocates, researchers, and scholars benefit from IA preservation (coordination function: free access to works for study and reference), but also experience suppression through legal restrictions, copyright claims, and takedown notices. They have some mobility (VPN access, alternative archives, institutional access), but barriers remain high. d≈0.45, f(d)≈0.40, σ=1.2 → χ≈0.25. Mixed: genuine coordination benefit (IA solves access problem) with constraints on mobility and enforcement threats.
constraint_indexing:constraint_classification(internet_archive_preservation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL FRAMEWORK (SCAFFOLD) — Copyright law as currently enforced constrains IA operations, but is intended as temporary scaffolding for a transition to clearer norms around digital preservation. Fair use doctrine, orphan works exceptions, and emerging preservation rights frameworks (EU, some nations) represent sunset mechanisms. theater_ratio=0.58 reflects that legal theater (formal cease-and-desist, litigation threat) exceeds actual enforcement. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Scaffold classification requires sunset visibility: explicit policy pathways (fair use expansion, preservation exceptions, national library licensing) are emerging.
constraint_indexing:constraint_classification(internet_archive_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT ENFORCEMENT APPARATUS (PITON) — The structural mechanisms for enforcing copyright against large-scale archiving (takedown notices, litigation, injunctions) are degraded: IA processes takedown requests but the scale of operations exceeds enforcement capacity. theater_ratio=0.62 reflects performative compliance: IA honors some removals while continuing core preservation. Enforcement is theater because the underlying function (controlling distribution) has atrophied — digital distribution is already decentralized. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.04. Enforcement persists through institutional inertia, not functional necessity.
constraint_indexing:constraint_classification(internet_archive_preservation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, digital decay is an immutable property of information systems: without active preservation, digital works become inaccessible within decades (media obsolescence, link rot, platform collapse). This perspective sees the IA as fighting a natural law rather than violating one. Preservation work is extracting against the entropy floor. However, structural data (ε=0.52, suppression=0.68) contradicts mountain classification — the conflict is a contingent institutional arrangement (copyright law and IA business model), not a law of nature. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(internet_archive_preservation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_archive_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(internet_archive_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_archive_preservation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(internet_archive_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(internet_archive_preservation, TR),
    TR >= 0.70.

:- end_tests(internet_archive_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. IA derives significant value from copyrighted works without compensating rights holders (free preservation and access). However, the extraction is not total because IA operates openly (declaring its preservation mission), provides public benefit (enables research and accessibility), and respects copyright law (processes takedown requests, cooperates with publishers on some works). The asymmetry exists but is bounded. Over the interval, extractiveness has risen as IA's collection has grown and publishers have become more aware of preservation's scale. Suppression (0.68): High. Multiple barriers suppress both rights holders' ability to prevent archiving and the public's ability to access archived works: (1) DMCA provisions restrict circumvention even for preservation; (2) takedown mechanisms are administratively costly for rights holders; (3) IA operates in legal gray zones (fair use, public domain questions) where enforcement is risky; (4) Access barriers (robots.txt, court injunctions, selective removal) limit public use. Suppression is not maximal (some works are fully accessible, fair use provides a legal pathway, some publishers cooperate) but is structurally high. Theater ratio (0.58): Moderate-high. Copyright enforcement against IA is substantially performative. Cease-and-desist letters, takedown notices, and litigation threats (Authors Guild cases) create theater of enforcement without eliminating IA's core preservation function. IA can and does continue operations while honoring selective removals. The theater has increased over the interval as enforcement mechanisms have become more visible (litigation, media coverage) relative to functional impact (preservation continues). Theater does not mean 'fake' — it means the performative content (legal theater, public debate) exceeds the functional outcome (actual access curtailment).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Copyright holders see pure extraction (Snare) — IA takes their works without permission or compensation, and they have no exit. IA itself sees pure coordination (Rope) — preservation is a service that benefits everyone including rights holders (cultural legacy, research access, attributability). Publishers see mixed extraction-coordination (Tangled Rope) — they benefit from IA's preservation (legitimacy, archival infrastructure) but lose control over pricing, access, and lifecycle. Accessibility advocates see coordination with suppression (Tangled Rope/Scaffold) — IA solves access problems but legal barriers prevent full utilization. The enforcement apparatus sees degraded ritual (Piton) — copyright enforcement persists through institutional inertia even though its functional purpose (controlling distribution) has eroded in a decentralized internet. The analytical observer risks seeing an immutable law (Mountain) — that digital preservation requires violating copyright law — but the structural data reveals this as a false summit: the conflict arises from contingent institutional arrangements (copyright law's design, IA's business model), not from physics or logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. They cannot exit (works already archived, impossible to retrieve all copies) and cannot enforce their rights practically. Publishers: Mixed victim/beneficiary + constrained → d≈0.70, f(d)≈1.05. Significant extraction but not maximal because publishers derive legitimacy benefits and distribution infrastructure from IA. Internet Archive: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. They can navigate legal gray zones, operate on their own terms, and frame preservation as a public good. Public/Researchers: Beneficiary + mobile → d≈0.45, f(d)≈0.40. Mixed because they gain access but face suppression through legal barriers and selective removal. Enforcement apparatus: Institutional + arbitrage → d≈0.15, f(d)≈0.05. Low extraction because enforcement capacity has atrophied (theater exceeds function). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATTERN: The mandatrophy is resolved by recognizing that the constraint's classification depends critically on the exit options and structural position of the observer. The copyright holder's Snare is real (d≈0.92, trapped, high suppression). IA's Rope is also real (d≈0.08, arbitrage, beneficiary). These are not contradictory — they are two structurally distinct readings of the same constraint from positions with opposite directionalities. The publisher's Tangled Rope identifies the genuine hybrid: they experience both coordination (benefits from IA infrastructure) and extraction (loss of control). The public's Tangled Rope with Scaffold elements reflects the legal transition in progress (fair use expansion, orphan works doctrines, preservation exceptions in some jurisdictions). The enforcement apparatus's Piton correctly identifies that copyright law as applied to IA is substantially performative. The analytical observer's false summit warns against naturalizing what is actually a contested institutional arrangement. The mandatrophy dissolves when we recognize: (1) this constraint does not decompose into 'snare' vs 'rope' — both are true from their respective positions; (2) the false summit (natural law reading) is a real error we can detect; (3) the legislative/judicial pathways (fair use clarification, preservation exemptions, licensing frameworks) represent genuine sunset mechanisms that can transition this toward Scaffold or Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_scope_boundary,
    'Does full-text preservation of copyrighted works for long-term access constitute fair use transformative purpose, or does it constitute wholesale reproduction that fair use does not protect?',
    'Judicial precedent (ongoing litigation: Authors Guild v. Google, HathiTrust cases); statutory clarification from Congress on preservation exemptions',
    'If fair use expands to cover preservation: IA classification moves to Rope (pure coordination). If fair use remains narrow: IA constraint remains Tangled Rope or Snare from copyright holder perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_scope_boundary, conceptual, 'Whether preservation constitutes fair use transformative purpose').

omega_variable(
    orphan_works_prevalence,
    'What fraction of IA''s collection consists of orphan works (copyright holder unknown/unreachable) vs actively managed works with identifiable copyright holders?',
    'Empirical sampling and metadata analysis of IA catalog; cross-reference with copyright registration databases',
    'If orphan works > 60%: preservation is mostly uncontested (Rope dominates). If orphan works < 30%: extraction against active copyright holders is the primary constraint (Snare dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orphan_works_prevalence, empirical, 'Prevalence of orphan vs actively managed works in IA collection').

omega_variable(
    license_alternative_viability,
    'Are emerging collective licensing frameworks (Creative Commons, open licenses, publisher-negotiated agreements) reducing the copyright conflict''s severity, or do they remain marginal?',
    'Trend analysis of licensed vs copyrighted works in IA; adoption rates of preservation-friendly licenses; publisher negotiation outcomes',
    'If licensing frameworks mature: scaffold sunset is real (transition pathway visible). If licensing remains marginal: the constraint hardens into structural snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(license_alternative_viability, empirical, 'Viability of collective licensing as conflict resolution').

omega_variable(
    digital_decay_rate_empirical,
    'What is the actual empirical rate of digital decay (media obsolescence, link rot, platform collapse) that justifies preservation urgency against copyright concerns?',
    'Longitudinal studies of digital survival rates; analysis of link rot in academic references; media format obsolescence timelines',
    'If decay rate > 5% per year: preservation urgency justifies lower suppression rating. If decay rate < 1% per year: preservation rationale is weaker, suppression rating remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_decay_rate_empirical, empirical, 'Empirical rate of digital decay and media obsolescence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_archive_preservation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iarchive_tr_t0, internet_archive_preservation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iarchive_tr_t5, internet_archive_preservation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(iarchive_tr_t10, internet_archive_preservation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(iarchive_be_t0, internet_archive_preservation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iarchive_be_t5, internet_archive_preservation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(iarchive_be_t10, internet_archive_preservation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_archive_preservation, information_standard).
narrative_ontology:affects_constraint(internet_archive_preservation, copyright_term_extension).
narrative_ontology:affects_constraint(internet_archive_preservation, digital_format_obsolescence).
narrative_ontology:affects_constraint(internet_archive_preservation, knowledge_commons_access).

% DUAL FORMULATION NOTE:
% The preservation-copyright conflict is downstream of broader copyright law design (term extension, scope of exclusive rights). It is upstream of digital format obsolescence as a constraint — IA's preservation efforts directly address format decay. It connects structurally to knowledge commons access as a constraint — IA is one mechanism (imperfect, contested) for solving access problems that copyright restrictions create.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
