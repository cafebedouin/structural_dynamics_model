% ============================================================================
% CONSTRAINT STORY: temporal_boundary_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_boundary_constraint, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_boundary_constraint
 *   human_readable: Temporal Boundary Constraint in Copyright's Founding Moment
 *   domain: legal_history/intellectual_property/commitment_systems
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is conventionally treated as the founding
 *   moment of modern copyright law. Yet this founding contains a structural
 *   ambiguity: did copyright become thinkable as a concept (a change in the
 *   conceptual space available for legal and philosophical discourse) before
 *   it became institutionalized in law, or did institutional occupation
 *   create the conceptual possibility? This constraint examines whether
 *   'became thinkable' and 'first held' are genuinely different readings of a
 *   contested kernel (the nature of intellectual property as a property
 *   right) or whether they represent a spurious cut imposed by analytical
 *   habit. The Statute of Anne instantiates a kernel commitment: that authors
 *   have a property right in their works, grounded in natural law (Lockean
 *   labor theory) or statutory grant (pragmatic regulation of the book
 *   trade). Different interpretive traditions read this kernel
 *   differently—originalist reading emphasizes the statutory privilege
 *   framing; natural-law reading emphasizes the right-recognition framing.
 *   Each reading produces a different constraint story with different
 *   victim/beneficiary structures. The temporal boundary is the hinge: if the
 *   concept preceded the institution, the readings are alternatives. If the
 *   institution created the concept, the constraint is a single story of
 *   institutional innovation with no kernel underneath.
 *
 * KEY AGENTS:
 *   - Unpatronized Authors: Primary victims (powerless/trapped) — lose access to non-property-based circulation (manuscript sharing, gift networks, patronage alternatives); face suppression by the property frame itself
 *   - Booksellers Guild / Stationers: Primary beneficiaries (institutional/arbitrage) — gain formal monopoly on printing, stabilized market, enforceable licensing; experience the constraint as pure coordination
 *   - Patronized Authors and Literary Circles: Secondary actors (moderate/constrained) — benefit from formal protection and licensing income; bear costs of dependency on commercial distribution; mixed experience
 *   - Enlightenment Literary Public: Organized agents (organized/mobile) — build alternative circulation networks (academies, salons, subscription models); perceive copyright as temporary, sunset-able constraint
 *   - Modern Copyright Law Apparatus: Institutional heir (institutional/arbitrage) — maintains copyright structure through inertia despite degraded function; theater-dominant perspective
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing copyright as immutable law rather than contingent choice; candidate for false summit detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_boundary_constraint, 0.58).
domain_priors:suppression_score(temporal_boundary_constraint, 0.52).
domain_priors:theater_ratio(temporal_boundary_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_boundary_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(temporal_boundary_constraint, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(temporal_boundary_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_boundary_constraint, tangled_rope).
narrative_ontology:human_readable(temporal_boundary_constraint, "Temporal Boundary Constraint in Copyright's Founding Moment").
narrative_ontology:topic_domain(temporal_boundary_constraint, "legal_history/intellectual_property/commitment_systems").

domain_priors:requires_active_enforcement(temporal_boundary_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(temporal_boundary_constraint, fixed_text).
narrative_ontology:cs_authority_grounding(temporal_boundary_constraint, lineage).
narrative_ontology:cs_interpretation_layer_present(temporal_boundary_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_boundary_constraint, booksellers_guild).
narrative_ontology:constraint_beneficiary(temporal_boundary_constraint, institutional_copyright_holders).
narrative_ontology:constraint_victim(temporal_boundary_constraint, authors_without_patronage).
narrative_ontology:constraint_victim(temporal_boundary_constraint, public_domain_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPATRONIZED AUTHOR (SNARE) — Trapped within the emerging copyright regime as a structural requirement. Before 1710, scribal copying and manuscript circulation created alternative publication pathways outside guild control. The Statute of Anne forecloses these by transforming 'authorship' into a property claim administered by stationers. An author without patronage or guild affiliation faces suppression: no choice between copyright protection and direct circulation, no ability to opt out of the property frame. This agent experiences maximum extraction — the constraint defines the very terms under which their work can exist.
constraint_indexing:constraint_classification(temporal_boundary_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOOKSELLERS GUILD / STATIONERS (ROPE) — Institutional beneficiary (powerful/arbitrage). The Statute of Anne appears to the stationers as pure coordination: organizing the book trade, preventing unauthorized copying, and establishing orderly market mechanisms for distribution. The beneficiary frame emphasizes the genuine coordination problem solved — unregulated copying destabilizes the book market. The stationers have exit options (they could continue under prior scrivener arrangements) but the statute improves their position unambiguously. Effective extraction is dampened by arbitrage exit and institutional power.
constraint_indexing:constraint_classification(temporal_boundary_constraint, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRONIZED AUTHORS & LITERARY CIRCLES (TANGLED ROPE) — Moderate power, constrained exit. These agents benefit from the copyright regime (formal protection, licensing income, public recognition of authorship as a legitimate role) while also bearing extraction costs (loss of gift economy circulation, dependency on commercial markets, inability to circulate work outside the property frame). Literary circles experience the constraint as a mixed good: coordination of market distribution mixed with suppression of alternative circulation modes. The tension is real — moderate extraction with genuine benefit.
constraint_indexing:constraint_classification(temporal_boundary_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ENLIGHTENMENT LITERARY PUBLIC (SCAFFOLD) — Organized agents (salons, academies, printing networks) see copyright as a temporary coordination mechanism with an eventual sunset. The constraint solves the immediate problem of stabilizing the book trade and establishing authorship as a recognized role. But the underlying function — ensuring authors are compensated for intellectual labor — could be solved via other mechanisms (state patronage, subscription models, public libraries). The literary public experiences the copyright frame as contingent, not essential. Their exit is mobile, their agency is real, and they perceive alternative pathways emerging. Theater is present but not dominant.
constraint_indexing:constraint_classification(temporal_boundary_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: MODERN COPYRIGHT LAW APPARATUS (PITON) — The legal apparatus that inherits the Statute of Anne's structure faces the piton classification: high theater (copyright enforcement has become substantially about ritual and legitimation rather than actual authorial compensation), persistent form despite degraded function (copyright persists as a legacy institutional structure even as its actual mechanisms—securing author income—have eroded in the digital era), and inertial maintenance. Copyright continues not because it solves the original coordination problem (it does not — authors are less compensated now than before) but because the institutional apparatus has accumulated dependent interests (publishers, lawyers, enforcement bureaucracies) that sustain the theater.
constraint_indexing:constraint_classification(temporal_boundary_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, copyright might be read as emerging from a natural right of authors to the fruits of their labor (Lockean labor theory). This perspective sees the Statute of Anne not as an institutional choice but as the recognition and formalization of a pre-existing natural right to intellectual property. The constraint appears as an unchangeable feature of property law itself—a natural law of human creativity. However, the historical record contradicts this: before 1710, intellectual property was NOT conceptualized in Lockean terms; the constraint is a constructed institutional arrangement that becomes naturalized through repetition. This perspective is a FALSE SUMMIT candidate, revealing how legal-philosophical framing naturalizes what is actually a contingent choice.
constraint_indexing:constraint_classification(temporal_boundary_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_boundary_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_boundary_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_boundary_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_boundary_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_boundary_constraint, TR),
    TR >= 0.70.

:- end_tests(temporal_boundary_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Statute of Anne immediately imposes the property frame on all literary works, creating extraction through the suppression of non-property-based circulation modes (manuscript sharing, gift networks, patronage circuits). But the extraction is not maximal (Snare-level ≥0.66) because genuine coordination benefits exist—the Statute does solve the problem of uncontrolled copying undermining market stability. The intermediate value reflects both extraction and real function. Over the interval, extractiveness accumulates as the property frame deepens (by 1770, copyright is more embedded and harder to escape, pushing toward 0.65). Suppression (0.52): Moderate. Before 1710, authors and readers had alternatives—direct manuscript circulation, patronage funding, guild-governed apprenticeship in the printing trades. The Statute forecloses these by requiring all publication to flow through the property frame. But suppression is not total—readers still have access to printed books, and authors can still negotiate contracts. Theater ratio (0.68): Moderate-high. The Statute's actual function (stabilizing the book trade) is pursued through property-law theater (the language of natural rights, the analogy to land property, the framing of copying as theft). Much of the enforcement effort involves convincing people that intellectual works should be treated as property—a performative labor that increases over time as the framing becomes institutionalized and naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim is stark: one sees coordination, the other sees suppression. The gap between historical and civilizational perspectives is equally sharp: at biographical time, the Statute appears as a contingent institutional choice (Tangled Rope, Scaffold); at civilizational time, it risks appearing as a natural law (Mountain). This gap instantiates the oracle gap: the civilizational perspective cannot see what the biographical perspective sees—the contingency and constructed nature of the copyright frame—because the frame has become so naturalized that the civilizational observer mistakes its institutional solidity for natural law. The false summit detection mechanism is essential here: it catches the moment when naturalization threatens to foreclose alternative framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's position relative to extraction flow. The unpatronized author is a victim with no exit (trapped) — derives high d (≈0.95) → high f(d) ≈ 1.42 → high experienced χ. The stationers are beneficiaries with arbitrage options — derives low d (≈0.05) → negative f(d) ≈ -0.12 → negative/dampened χ. Patronized authors are caught between: they benefit from formal recognition but lose flexibility — derives mid-range d (≈0.55) → moderate f(d) ≈ 0.75 → moderate χ. The literary public are organized with exit options (they can build alternative networks) — derives constrained d (≈0.45) → low f(d) ≈ 0.50 → low χ despite moderate power. The modern legal apparatus maintains the structure through institutional inertia — derives institutional d (≈0.00) → negative f(d) — but the theater_ratio gate (≥0.70) is not met, so piton classification derives from inertial maintenance rather than high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT resolved (mandatrophy_resolved: false) because the constraint genuinely contains the structural tension: copyright simultaneously coordinates (solves the uncontrolled-copying problem) and extracts (forecloses non-property-based circulation). The tangled_rope classification captures this precisely—there is no way to classify this constraint as pure coordination (Rope) without ignoring the real suppression of alternatives; there is no way to classify it as pure extraction (Snare) without ignoring the genuine market stabilization function. The tension is real and irreducible at the institutional/biographical level. However, the mandatrophy COULD be resolved at different temporal/scope positions: at the piton level (institutional/civilizational), the constraint appears degraded because modern copyright serves neither its original coordination function (book trade is now digital, not threatened by copying) nor does it compensate authors (most authors earn less than pre-copyright-era patronage). At the natural-law level (analytical/civilizational), the mandatrophy dissolves through false-summit detection: the mountain classification is unmasked as naturalization of a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    became_thinkable_vs_first_held,
    'Does ''became thinkable'' (when intellectual property as a concept first entered the legal-philosophical discourse) differ structurally from ''first held'' (when the Statute of Anne institutionalized the legal claim)? Are these genuinely different readings of the same kernel or a spurious analytical distinction?',
    'Genealogical analysis of intellectual property discourse: did Locke''s labor theory precede or follow statutory copyright? Did the concept of ''literary property'' emerge before or after the Statute? Do pre-1710 guild records show ownership framing of intangible works? Historical reconstruction of which came first: the idea or the institution.',
    'If ''became thinkable'' precedes ''first held'' by decades or centuries: two different readings of the founding kernel, each with different victim/beneficiary structures. If they are nearly simultaneous or if ''first held'' precedes ''became thinkable'': the distinction is analytical rather than structural, and the constraint should be modeled as a single story with temporal drift rather than as a kernel with alternative readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(became_thinkable_vs_first_held, empirical, 'Whether conceptual emergence and institutional occupation are genuinely distinct readings or analytical artifacts').

omega_variable(
    stationers_charter_continuity,
    'Did the Statute of Anne truly establish a novel property right in intellectual works, or did it formalize and extend privileges the Stationers'' Company already held through prior charters (monopoly on printing rights, registration authority)?',
    'Comparative legal analysis of the Company of Stationers'' charter (1556) vs. the Statute of Anne (1710); examination of whether the Statute shifts the property holder from the guild to the author or merely reorganizes guild privileges. Analysis of actual licensing and revenue flows before and after 1710.',
    'If the Statute extends pre-existing guild monopoly: the institutional continuity weakens the ''first held'' claim, and the constraint is a degradation/reorganization of prior power rather than a novel institutional creation. If the Statute genuinely shifts property ownership from guild to author: the reading is structurally distinct—the founding moment creates a new victim/beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stationers_charter_continuity, empirical, 'Whether copyright represents institutional novelty or formalization of existing guild privileges').

omega_variable(
    kernel_codification_ambiguity,
    'Is the kernel of copyright a fixed, codified text (the Statute of Anne itself), or is it a distributed conceptual claim about property rights in intellectual works that the Statute merely instantiates?',
    'Textual analysis of the Statute''s language regarding ''the sole Right and Liberty of Printing'' — does it claim to establish a natural right or a statutory privilege? Examination of subsequent interpretive traditions: do courts, legislators, and theorists treat copyright as a natural law discovery or a statutory convention? Analysis of how the kernel shifts across jurisdictions that do not have the Statute but adopt copyright frameworks.',
    'If kernel is the fixed text: the Statute of Anne is the constraint''s anchor point, and alternative readings emerge through interpretive traditions (living reading vs originalist reading). If kernel is distributed: the constraint decomposes into multiple institutional instantiations with different ε values, each a reading of the underlying conceptual claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, conceptual, 'Whether copyright''s kernel is a fixed statutory text or a distributed conceptual claim').

omega_variable(
    author_agency_in_property_framing,
    'Did the Statute of Anne recognize authors as property-holders (granting them new agency and rights), or did it reframe authorship as a property claim subordinate to stationer/publisher control (reducing authorial agency by converting labor into property administered by others)?',
    'Historical analysis of authorial experience and negotiating power before and after the Statute; examination of licensing agreements, income flows, and contractual terms available to authors in 1700 vs 1750; comparison of authors'' ability to circulate work outside the property frame before and after 1710.',
    'If authors gained agency: the Statute is a beneficial constraint with genuine coordination function—tangled_rope classification justified. If authors lost agency by being subsumed into the property frame: the Statute is a predatory constraint that uses property language to justify publisher monopoly—snare classification more accurate, and the tangled_rope is a false frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_agency_in_property_framing, empirical, 'Whether copyright formalization enhanced or diminished authorial agency').

omega_variable(
    false_summit_natural_law_framing,
    'Is copyright accurately characterized as a natural law (property right in labor products emerges necessarily from human nature), or is this a false summit that naturalizes a contingent institutional arrangement?',
    'Cross-cultural and historical comparison: do non-Western legal traditions before European contact recognize intellectual property? Do pre-1710 European frameworks treat literary works as property or as gifts, theft, or contractual goods? Genealogy of Lockean labor theory applied to intangibles—when does this application occur relative to statutory copyright?',
    'If copyright is a natural law: mountain classification is correct, false_summit_mountain signature does not fire, and the constraint is unchangeable across all perspectives. If copyright is a naturalized institutional choice: mountain classification is false, FSM signature fires (beneficiaries present), and the constraint reclassifies as tangled_rope or snare depending on structural analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'Whether copyright represents natural law or naturalized institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_boundary_constraint, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tempbound_theater_pre1710, temporal_boundary_constraint, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tempbound_theater_1710_immediate, temporal_boundary_constraint, theater_ratio, 1, 0.52).
narrative_ontology:measurement(tempbound_theater_1740, temporal_boundary_constraint, theater_ratio, 3, 0.62).
narrative_ontology:measurement(tempbound_theater_1770, temporal_boundary_constraint, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(tempbound_extr_pre1710, temporal_boundary_constraint, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tempbound_extr_1710_immediate, temporal_boundary_constraint, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(tempbound_extr_1740, temporal_boundary_constraint, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(tempbound_extr_1770, temporal_boundary_constraint, base_extractiveness, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_boundary_constraint, resource_allocation).
narrative_ontology:boltzmann_floor_override(temporal_boundary_constraint, 0.18).
narrative_ontology:affects_constraint(temporal_boundary_constraint, patronage_system_displacement).
narrative_ontology:affects_constraint(temporal_boundary_constraint, authorial_labor_commodification).
narrative_ontology:affects_constraint(temporal_boundary_constraint, manuscript_circulation_suppression).

% DUAL FORMULATION NOTE:
% The temporal boundary constraint decomposes into three downstream constraints: (1) patronage_system_displacement (ε≈0.48) — how copyright as property-right framing replaced patronage as the primary compensation mechanism for authors; (2) authorial_labor_commodification (ε≈0.62) — how copyright converts literary labor into a commodity traded in markets; (3) manuscript_circulation_suppression (ε≈0.52) — how copyright forecloses non-commercial circulation modes. Each has different extraction mechanisms and different victim/beneficiary structures. The temporal_boundary_constraint is the parent constraint that establishes the institutional frame within which all three operate. Links flow from parent to children via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temporal_boundary_constraint, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
