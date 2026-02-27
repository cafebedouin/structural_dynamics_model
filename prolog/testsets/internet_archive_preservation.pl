% ============================================================================
% CONSTRAINT STORY: internet_archive_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   The Internet Archive's mission to preserve global digital culture
 *   collides structurally with copyright law's grant of exclusive
 *   distribution rights to authors and publishers. This constraint exhibits
 *   the core tension between two legitimate public goods: universal cultural
 *   preservation (coordination benefit) and creator economic control
 *   (extraction mechanism). The IA operates in persistent legal ambiguity —
 *   claiming fair use for institutional archival and relying on Library of
 *   Congress exemptions while publishers and authors launch recurring
 *   lawsuits to constrain the archive's lending operations. The constraint's
 *   extractiveness (0.58) reflects that the IA's Controlled Digital Lending
 *   (CDL) model selectively extracts value from copyright holders' exclusive
 *   distribution rights without direct compensation, yet the constraint also
 *   provides genuine coordination benefits (backup preservation, access to
 *   out-of-print works, cultural continuity). The suppression (0.72) is high:
 *   copyright holders cannot prevent archival without expensive litigation,
 *   and they lack alternative mechanisms to enforce exclusive rights against
 *   a well-funded institution with international scope and public legitimacy.
 *   The theater ratio (0.48, rising to 0.72 at the civilizational scope)
 *   reflects that copyright enforcement is increasingly performative —
 *   Digital Rights Management, licensing complexity, and DMCA
 *   anti-circumvention provisions consume enforcement resources without
 *   fundamentally enabling the law's stated purpose (incentivizing creation).
 *   This constraint is a gold-standard exemplar of tangled rope: the IA
 *   provides genuine coordination (centralized preservation reduces
 *   duplication), requires active enforcement (copyright litigation is
 *   continuous), and extracts asymmetrically (digital lending undermines
 *   in-print sales).
 *
 * KEY AGENTS:
 *   - Internet Archive: Primary beneficiary (institutional/arbitrage) — gains operational legitimacy, cultural status, funding, and exemption from copyright restrictions through fair use and section 108 claims
 *   - Copyright Holders (Authors/Publishers): Primary victim (powerless/trapped for authors; moderate/constrained for publishers) — bear extraction of exclusive rights; litigation is expensive and outcomes uncertain
 *   - Publishing Industry: Secondary actor (moderate/constrained) — benefits from IA backup archival (coordination), simultaneously undermined by CDL lending (extraction); maintains legal action through trade groups
 *   - Public Scholarship and Access Communities: Beneficiary (moderate/mobile) — gain access to preserved materials, out-of-print works, and cultural heritage; but benefit is contingent on IA's legal survival
 *   - Open Access and Digital Commons Movement: Organized coalition (organized/constrained) — building alternative preservation pathways (public-domain works, open-access mandates, decentralized archival); sees constraint as temporary institutional failure with sunset clause
 *   - Copyright Law (Institutional System): Analytical actor — degraded coordination mechanism (piton from civilizational view); theater increasing as licensing complexity and DRM enforcement accumulate without enabling primary function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_archive_preservation, 0.58).
domain_priors:suppression_score(internet_archive_preservation, 0.72).
domain_priors:theater_ratio(internet_archive_preservation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_archive_preservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(internet_archive_preservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(internet_archive_preservation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_archive_preservation, tangled_rope).
narrative_ontology:human_readable(internet_archive_preservation, "The Internet Archive Preservation-Copyright Conflict").
narrative_ontology:topic_domain(internet_archive_preservation, "technological/legal").

domain_priors:requires_active_enforcement(internet_archive_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_archive_preservation, public_scholarship_access).
narrative_ontology:constraint_beneficiary(internet_archive_preservation, cultural_preservation).
narrative_ontology:constraint_beneficiary(internet_archive_preservation, internet_archive_operational_sustainability).
narrative_ontology:constraint_victim(internet_archive_preservation, copyright_holder_exclusive_rights).
narrative_ontology:constraint_victim(internet_archive_preservation, author_economic_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COPYRIGHT HOLDER (SNARE) — Trapped by the scope of IA's archival ambitions. Copyright holders cannot prevent archival without legal action (high suppression), and litigation is resource-intensive. The IA's Controlled Digital Lending (CDL) model extracts value from copyright holders' exclusive rights without compensation. Holders bear full cost; exit options are constrained to expensive litigation or passive acceptance. Maximal experienced extraction.
constraint_indexing:constraint_classification(internet_archive_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLISHING INDUSTRY (TANGLED ROPE) — Mixed coordination and extraction. Publishers benefit from the IA as a backup archival mechanism (coordination function), ensuring cultural continuity and reducing their own preservation burden. Simultaneously, CDL enables lending of in-print books, undermining sales and exclusive distribution rights (extraction function). Active enforcement through trade group litigation (Authors Guild, Publishers Association lawsuits) is required to maintain copyright boundaries. Exit options are constrained — legal action is expensive and outcomes uncertain.
constraint_indexing:constraint_classification(internet_archive_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNET ARCHIVE (ROPE) — Experiences the constraint as a coordination mechanism: standardizing digital preservation reduces duplication across memory institutions (coordination benefit). The IA's exemptions from copyright law (fair use doctrine, Library of Congress Section 108 exemptions) provide arbitrage exit options — the organization navigates legal ambiguity to sustain operations. The IA benefits from cultural legitimacy and institutional partnerships. Effective extraction runs toward the IA; they are the primary beneficiary of the coordination function.
constraint_indexing:constraint_classification(internet_archive_preservation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN ACCESS MOVEMENT (SCAFFOLD) — Organized agents (creative commons proponents, open-source advocates, library systems) see the preservation conflict as a temporary institutional failure. Alternative models (public-domain works, open-access mandates, author-approved archival) are building parallel preservation pathways. The sunset clause is embedded: as copyright terms shorten (Australia, UK movement toward life+50 or life+60), as open-access mandates mature, and as decentralized archival (IPFS, blockchain) scales, the friction between copyright law and digital preservation reduces. Suppression is high (legal barriers remain), but the organized coalition has agency and sees an exit path.
constraint_indexing:constraint_classification(internet_archive_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT LAW (PITON) — From a civilizational scope, copyright law is a degraded coordination mechanism. Its original function (incentivizing creation) persists, but theater has accumulated: licensing regimes, DMCA enforcement, and Digital Rights Management (DRM) consume resources without enabling the primary function. The law persists through institutional inertia (international treaties, publishing industry lobbying) despite mounting evidence that alternative incentives (patronage, reputation, open licensing) sustain creation. The constraint is maintained performatively — courts cite creative incentives while extended copyright terms primarily benefit estates of deceased authors. Theater ratio approaching 0.70 marks the piton threshold.
constraint_indexing:constraint_classification(internet_archive_preservation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some tension between creator control and cultural memory is inherent to any system of intellectual property. Perfect preservation requires complete access; complete author control prevents access. The gap is a structural feature, not a contingent institutional failure. However, this perspective naturalizes what is actually a legal and policy choice. The engine's false summit detector identifies this as potential naturalization — copyright duration, fair use scope, and exemption mechanisms are all contingent policy parameters, not immutable laws.
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
 *   Extractiveness (0.58): Moderate-high. The IA's CDL model extracts value from copyright holders' exclusive distribution rights. In-print lending by the IA creates a direct substitute for authorized sales, particularly for academic and research materials where the IA has deep collections. However, extraction is not maximal (snare-level 0.66+) because: (1) out-of-print lending does not compete with sales, (2) the IA is selective in lending practices, and (3) cultural preservation provides a public good that partly justifies the extraction. The value increases over time (0.42→0.58) as the IA's collection grows and CDL lending volume rises. Suppression (0.72): High. Copyright holders face substantial barriers to enforcement: (a) the IA is large and international, making jurisdiction challenging, (b) fair use claims create legal uncertainty, (c) the IA has institutional legitimacy and library partnerships that complicate direct action, (d) authors lack resources for individual litigation. The Authors Guild has mounted recurring suits (Authors Guild v. Google Books, Authors Guild v. HathiTrust), but legal outcomes remain uncertain. Suppression reflects not total prevention but high cost of exit — copyright holders must sue rather than prevent archival through contractual mechanisms. Theater ratio (0.48, increasing to 0.72 at civilizational scope): Moderate and rising. At the operational level (immediate/institutional scope), theater is moderate — CDL has genuine coordination and preservation functions. At the civilizational level, theater increases: copyright law enforcement increasingly relies on performative mechanisms (DMCA notices, licensing regimes) without enabling the law's stated purpose. The theater accumulation reflects Goodhart drift: as copyright enforcement technology (DRM, automated takedown notices) scales, the theater-to-function ratio rises. Claimed type: Tangled Rope — justified by: (1) genuine coordination function (centralized preservation reduces duplication across memory institutions), (2) asymmetric extraction (CDL lending undermines copyright holder sales), (3) active enforcement requirement (continuous litigation and legal strategy from publishers/authors). The constraint cannot classify as pure coordination (Rope) because extraction is significant; cannot classify as pure extraction (Snare) because coordination benefits are real and institutional partnerships depend on preservation function.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal the constraint as a genuinely contested space where legitimate institutional logics collide. The copyright holder's Snare (extraction with no exit) is structurally real — they cannot prevent archival without litigation. The publisher's Tangled Rope (mixed coordination and extraction) is equally real — publishers do benefit from archival backup, but CDL undermines sales. The IA's Rope (coordination mechanism with arbitrage exit) reflects their actual legal position (fair use claims, section 108 exemptions). The open-access coalition's Scaffold (temporary institutional failure with policy sunset) is real — copyright policy IS changing, and decentralized archival alternatives ARE maturing. The Piton classification of copyright law itself is real — enforcement theater has grown (DRM, licensing complexity) without enabling the original purpose. The mountain classification is a false summit — the analytical observer risks naturalizing a contingent legal framework as immutable law of intellectual property. These are not measurement artifacts or perspective tricks. They reflect genuine disagreement about what the constraint IS: extraction mechanism, coordination problem, temporary institutional failure, or natural law. The disagreement is resolvable only by tracking which institutional actors have power to change the constraint (publishers, legislators, courts) and which do not (authors, public scholarship communities, the IA itself operates in legal gray zone). The structural power to change the constraint currently rests with publishers and courts; the organizational power to build alternatives rests with the open-access movement and library systems. This is the perspectival gap at the deepest level: disagreement not just on classification but on who has agency to resolve the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position — specifically, who benefits from the constraint and who bears costs, combined with exit options. Copyright holders are victims with no exit (high d, trapped → high f(d) → high experienced extraction). The IA is a beneficiary with arbitrage exit options (low d, arbitrage → low/negative f(d) → low experienced extraction). Publishers occupy a middle position: victims of CDL (high d for extraction component) but also beneficiaries of backup preservation (low d for coordination component) — this mixture justifies the Tangled Rope classification at moderate power level. The open-access movement has constrained exit but organized power, which moderates their experienced extraction compared to powerless authors. The constraint's effective extractiveness (χ) varies by perspective because directionality varies: from the copyright holder's view, χ approaches snare levels (high d × moderate f(d) × scope modifier); from the IA's view, χ is low (low d × negative f(d) × scope modifier); from the analyst's view, χ is framed differently depending on whether the observer naturalizes copyright as immutable law or recognizes it as contingent policy.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. Extractiveness (0.58) is below the 0.70 threshold requiring mandatrophy resolution, but the constraint is sensitive to the mandatrophy confusion because different perspectives disagree on the fundamental classification. The copyright holder perspective (Snare) would demand mandatrophy resolution if extractiveness rose above 0.66 — at that point, the constraint would risk mislabeling pure extraction (enforcement of copyright monopoly) as mixed coordination-extraction (Tangled Rope). The IA perspective (Rope) would contest any classification above Rope, arguing that the coordination function is primary and extraction is secondary. The mandate-preservation distinction is critical here: ARE copyright enforcement and archival preservation in actual tension (separate institutional mandates pulling in opposite directions), or is copyright enforcement parasitic on the preservation mandate (extraction disguised as law enforcement)? The mandatrophy confusion arises because the constraint conflates two separate institutional logics: (1) the mandate to preserve cultural heritage (IA's primary function), and (2) the mandate to enforce copyright (publishers' and authors' institutional logic). These mandates are not simply in tension — they are pulling toward different constraint configurations. If the cultural preservation mandate dominates (open-access movement perspective), the constraint softens toward Rope or Scaffold (coordination, temporary institutional failure). If the copyright enforcement mandate dominates (publisher perspective), the constraint hardens toward Snare or Tangled Rope (extraction, asymmetric enforcement). The resolution mechanism is political, not analytical: whichever institutional mandate gains regulatory or legislative dominance will determine whether the constraint classifies as coordination or extraction in future iterations. The engine cannot resolve this mandatrophy automatically — it requires explicit policy choice. For now, the constraint is classified as Tangled Rope, reflecting the genuine presence of both mandates and their ongoing collision. As policy evolves (copyright term reduction, open-access mandates mature, decentralized archival scales), the constraint's classification will shift. Tracking that shift is the corpus's job.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_scope_boundary,
    'Does institutional archival (IA''s CDL model) constitute fair use, or does it exceed the transformative use threshold?',
    'Appellate court ruling on Sony v. Reimerdes (DMCA circumvention) or Authors Guild v. Google (Books) precedent applications; legal clarification of institutional archival scope under 17 U.S.C. § 107',
    'If fair use upheld: CDL is legal coordination mechanism (Rope from all perspectives). If fair use rejected: CDL is extraction mechanism (Snare from copyright holders, Tangled Rope from publishers). Classification shifts 2-3 types depending on ruling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_scope_boundary, empirical, 'Whether institutional archival meets fair use standards').

omega_variable(
    copyright_duration_policy_window,
    'Will copyright term reduction (life+50 vs life+70) or public-domain expansion policies materialize within the next 10-30 years?',
    'Legislative developments (EU Copyright Directive revisions, US Term Extension Act challenges); adoption of life+50 or shorter in major jurisdictions',
    'If policies mature: scaffold perspective confirmed — open-access exit path is real. If policies stall: scaffold is aspirational, constraint persists as Snare/Tangled Rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_duration_policy_window, conceptual, 'Whether copyright term reduction will enable preservation alternatives').

omega_variable(
    decentralized_archival_viability,
    'Can decentralized archival systems (IPFS, blockchain-based preservation) achieve institutional-grade reliability and accessibility without central authority coordination?',
    'Technical assessment of IPFS retention rates, Filecoin incentive sustainability; comparison of decentralized vs centralized archival uptime and access speeds for scholarly use',
    'If viable: constraint becomes coordination problem (Rope) rather than extraction conflict — distributed archival eliminates copyright holder leverage. If nonviable: IA''s centralized role remains essential, compression increases, constraint tightens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_archival_viability, empirical, 'Whether decentralized archival can replace institutional preservation').

omega_variable(
    legal_exemption_expansion,
    'Will Library of Congress Section 108 exemptions expand to explicitly cover institutional digital lending, or will they remain narrowly construed?',
    'Copyright Office rulemaking under DMCA § 1201(a)(1)(B); Section 108 legislative amendment or court precedent expansion',
    'If expanded: IA gains legal cover (compression reduces, classification softens to Rope). If narrowly construed: IA operates in legal gray zone (tension remains, constraint stays Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_exemption_expansion, conceptual, 'Whether legal exemptions will clarify digital archival rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_archive_preservation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ia_pres_tr_t0, internet_archive_preservation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ia_pres_tr_t10, internet_archive_preservation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ia_pres_tr_t20, internet_archive_preservation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ia_pres_be_t0, internet_archive_preservation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ia_pres_be_t10, internet_archive_preservation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ia_pres_be_t20, internet_archive_preservation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_archive_preservation, information_standard).
narrative_ontology:affects_constraint(internet_archive_preservation, copyright_term_length).
narrative_ontology:affects_constraint(internet_archive_preservation, fair_use_scope_expansion).
narrative_ontology:affects_constraint(internet_archive_preservation, open_access_mandate_diffusion).
narrative_ontology:affects_constraint(internet_archive_preservation, decentralized_preservation_scalability).

% DUAL FORMULATION NOTE:
% The preservation-copyright conflict decomposes into multiple downstream constraints: copyright term length (policy choice affecting public domain growth), fair use scope (legal question affecting IA's operational legality), open-access mandate diffusion (policy question affecting alternative preservation pathways), and decentralized archival scalability (technical question affecting IA's competitive position). The IA constraint is upstream — its classification as Tangled Rope depends on all downstream constraints remaining contested. If copyright terms shorten (public domain expansion), or if fair use expands (IA gains legal cover), or if open-access mandates mature (alternative pathways eliminate extraction mechanism), or if decentralized archival scales (IA loses monopoly position on archival), the IA constraint classification shifts toward Rope or Scaffold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(internet_archive_preservation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
