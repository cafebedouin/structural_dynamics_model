% ============================================================================
% CONSTRAINT STORY: institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_reallocation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_reallocation_reading
 *   human_readable: Institutional Reallocation of IP Rights: Statute of Anne Reading
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is foundational to modern copyright law and
 *   intellectual property doctrine. This constraint story models ONE reading
 *   of the contested kernel: the institutional reallocation reading. This
 *   reading frames the statute as a redistribution of property rights from
 *   the Stationers' Company monopoly to authors (and derivative
 *   beneficiaries, publishers). The statute reallocated the institutional
 *   space occupied by copyright-holding entities: prior to 1710, copyright
 *   was effectively held by the guild of London stationers; after 1710,
 *   authorial rights were formally recognized and became assignable, enabling
 *   publishers to acquire and control rights. The reallocation was
 *   structural, not merely conceptual: it changed which agents could legally
 *   claim ownership, changed who could enforce rights through courts, and
 *   changed the economic flow from a guild-controlled monopoly to a
 *   market-based assignment mechanism. This reading coexists with (but is
 *   distinct from) two sibling readings: the conceptual_emergence_reading,
 *   which frames the statute as the moment IP as a category conceptually
 *   emerged (what is copyright as an idea?), and the entangled_event_reading,
 *   which argues the institutional and conceptual transformations occurred
 *   simultaneously and cannot be disentangled. The institutional reallocation
 *   reading isolates the property-rights-transfer dimension and treats the
 *   conceptual dimension as secondary (or as rationalization for the
 *   structural change). The reallocation carries both coordination value
 *   (authors now have a legal standing from which to bargain) and extraction
 *   risks (authors face immediate pressure to assign rights to publishers at
 *   unfavorable terms).
 *
 * KEY AGENTS:
 *   - Stationers' Company: Chartered monopoly (institutional/trapped) — primary victim; lost vested rights to guild control without transition compensation. Institutional position reallocated without agency.
 *   - Publishers/Booksellers: Commercial actors (institutional/arbitrage) — primary beneficiaries; gained ability to acquire assignable rights from authors. Arbitrage option enabled by statute.
 *   - Authors (as Emerging Class): Previously unrecognized right-holders (moderate to powerful depending on patronage/access/reputation) — simultaneously elevated (given legal standing) and constrained (faced assignment pressure). Mobile exit for well-positioned authors; constrained for others.
 *   - Traditional Patrons and Court Connections: Elite actors (powerful/arbitrage) — lost direct control but retained influence through author relationships and assignment practices. Mixed position.
 *   - Crown / Licensing Authority: Meta-institutional actor (institutional/arbitrage) — maintained censorship and regulatory power while shifting monopoly enforcement burden to courts. Theater masks continued control.
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing institutional reallocation as discovery of natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_reallocation_reading, 0.38).
domain_priors:suppression_score(institutional_reallocation_reading, 0.42).
domain_priors:theater_ratio(institutional_reallocation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_reallocation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(institutional_reallocation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(institutional_reallocation_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(institutional_reallocation_reading, "Institutional Reallocation of IP Rights: Statute of Anne Reading").
narrative_ontology:topic_domain(institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(institutional_reallocation_reading, formalized).
narrative_ontology:cs_authority_grounding(institutional_reallocation_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(institutional_reallocation_reading).
narrative_ontology:cs_kernel_id(institutional_reallocation_reading, statute_of_anne_ip_foundation).
narrative_ontology:cs_reading_relation(institutional_reallocation_reading, conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation(institutional_reallocation_reading, entangled_event_reading, influences).
narrative_ontology:cs_axiom(institutional_reallocation_reading, foundational, property_rights_transferable_from_guild_to_market).
narrative_ontology:cs_axiom_status(property_rights_transferable_from_guild_to_market, holdable).
narrative_ontology:cs_axiom_grounding(institutional_reallocation_reading, property_rights_transferable_from_guild_to_market, conventional).
narrative_ontology:cs_axiom(institutional_reallocation_reading, secondary, author_right_holder_recognition_primary).
narrative_ontology:cs_axiom_status(author_right_holder_recognition_primary, holdable).
narrative_ontology:cs_axiom_grounding(institutional_reallocation_reading, author_right_holder_recognition_primary, deontological).
narrative_ontology:cs_reference_frame(institutional_reallocation_reading, guild_monopoly_ip_regime).
narrative_ontology:cs_drift_state(institutional_reallocation_reading, mature_statute_period, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_reallocation_reading, book_publishers).
narrative_ontology:constraint_beneficiary(institutional_reallocation_reading, authors_as_assignees).
narrative_ontology:constraint_victim(institutional_reallocation_reading, stationers_company_monopoly).
narrative_ontology:constraint_victim(institutional_reallocation_reading, traditional_patrons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATIONERS' COMPANY (SNARE) — The chartered monopoly faced extraction of its institutional position without compensatory alternative. Suppression was structural: the Crown's statutory reallocation foreclosed other exit paths. The company experienced this as a direct loss of vested rights with no agency to negotiate transition.
constraint_indexing:constraint_classification(institutional_reallocation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL PATRONS / COURT-PRIVILEGED AUTHORS (TANGLED ROPE) — Mixed experience: patrons lost their direct gatekeeper role (extraction), but retained influence through author assignment and dedication practices (coordination benefit). Constrained exit — they could not easily revert to pre-statute patronage structures, but maintained economic involvement through the new assignment mechanism.
constraint_indexing:constraint_classification(institutional_reallocation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLISHERS AND COMMERCIAL BOOKSELLERS (ROPE) — Primary beneficiaries. The statute created assignable rights that publishers could acquire from authors, converting intellectual property into tradeable assets. This was coordination (standardized ownership transfer) with net flow toward publishers. High arbitrage — publishers could exit into alternative distribution models.
constraint_indexing:constraint_classification(institutional_reallocation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTHORS AS EMERGING CLASS (TANGLED ROPE) — Mixed experience reflects the reading's core claim: authors were recognized as right-holders for the first time, but immediately faced pressure to assign those rights to publishers for upfront payment. This is coordination (establishing authorial standing) coupled with extraction (assignment pressure). Mobile exit — some authors could exploit multiple publishers or refuse assignment, but most faced strong economic incentives to assign.
constraint_indexing:constraint_classification(institutional_reallocation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CROWN'S REGULATORY AUTHORITY (PITON) — The statutory reallocation itself was performative from the Crown's perspective: it announced a shift in institutional priority (from guild monopoly to market-based publishing) while preserving the Crown's veto power via censorship and licensing. The constraint's function (controlling publication) persisted despite the ostensible rights redistribution. Theater_ratio captures the gap between the statute's language (author rights) and its practice (rights attached to censorship authority).
constraint_indexing:constraint_classification(institutional_reallocation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, the statute appears to discover an inherent natural right: authors inherently own their intellectual creation; the statute merely recognized a pre-existing legal entitlement. This reading sees property rights as emerging from natural law rather than institutional allocation. However, the structural data contradicts mountain classification — the statute's actual effect was institutional reallocation, not discovery. The analytical perspective risks false-summit naturalization.
constraint_indexing:constraint_classification(institutional_reallocation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_reallocation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_reallocation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_reallocation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_reallocation_reading, TR),
    TR >= 0.70.

:- end_tests(institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reallocation generated extraction flows in both directions — Stationers lost monopoly rents (extraction from them), authors faced assignment pressure (extraction from them), but publishers gained tradeable assets (beneficiary position). The net extractiveness is moderate rather than high because the mechanism included genuine coordination (legal standing for authors) alongside extraction. Over the measured interval, extractiveness rose from 0.18 (pre-statute) to 0.38 (mature statute) as the assignment market developed and publishers consolidated control of rights acquisition. Suppression (0.42): Moderate. The statute reduced some suppression mechanisms (guild monopoly, Crown licensing for censorship purposes) while preserving others (authors' inability to resist assignment pressure, Crown's continued censorship authority). Theater_ratio (0.35): Moderate-low. The statute's performative content was not high — it had real structural effects (Stationers lost monopoly, rights became assignable, courts gained jurisdiction). However, theater rose over time as the Crown's regulatory authority (through censorship and licensing) became less visible: the statute appeared to replace monopoly with free market, masking that Crown control persisted through different mechanisms. The rise in theater from 0.22 to 0.35 reflects this obscuration of regulatory continuity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits strong perspectival variation across the six perspectives. The Stationers' Company sees pure extraction (Snare) — they are the institutional class being displaced by reallocation. Publishers see coordination (Rope) — the statute solved a collective action problem (standardized rights transfer) that enabled the book trade to scale. Authors see mixed coordination and extraction (Tangled Rope) — they gained legal standing but faced immediate assignment pressure. Patrons see mixed effects (also Tangled Rope) — they lost direct control but retained influence through assignment relationships. The Crown sees its own function as degraded (Piton) — the statute ostensibly replaced monopoly control with market mechanisms, but Crown censorship authority persisted, making the reform performative. The analytical observer risks seeing an immutable natural law (Mountain) — IP rights as pre-existing natural entitlements — but the structural data reveals this as a false summit: the statute created institutional conditions that are historically contingent, not discovered. The perspectival gap is large and diagnostic: the range from Snare (Stationers) to Rope (publishers) to Piton (Crown) demonstrates that the same structural event produces incommensurable classifications depending on the observer's position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position relative to this constraint. Stationers' Company: beneficiary of pre-statute monopoly, victim of statute; trapped exit (no alternative institutional role); d ≈ 0.92 (near-maximal target). Publishers: beneficiaries of assignable rights; arbitrage exit (could operate outside statute); d ≈ 0.10 (beneficiary). Authors: ambiguous — elevated to right-holders but constrained by assignment pressure; d ≈ 0.55 (symmetric, depending on author position). Patrons: lost gatekeeper role but retained influence; powerful and mobile; d ≈ 0.40 (moderate). Crown: maintained regulatory control through different mechanism; arbitrage exit; d ≈ 0.08 (institutional beneficiary via obscured authority). These values feed into the chi calculation χ = ε × f(d) × σ(S), which scales extractiveness by the agent's structural relationship and the constraint's national scope (σ=1.0). The perspectival gap arises because different agents experience different d values despite the same ε: Stationers experience high chi (extraction); publishers experience low or negative chi (coordination); authors experience moderate chi (mixed). Piton perspective (Crown) shows low experienced extraction because arbitrage exit (d ≈ 0.08) produces negative f(d), masking the structural constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by showing that institutional reallocation is genuinely a tangled rope: it combined coordination (legal standing for authors, standardized rights transfer enabling market mechanism) with extraction (displacement of Stationers' vested rights, assignment pressure on authors, Crown's obscured regulatory continuation). The extraction component (0.38) is real but not overwhelming (snare-level would be ≥ 0.46). The suppression is moderate (0.42), not crushing. The theater is below the piton threshold (0.35 < 0.70). The statute was neither pure coordination nor pure extraction — it was a structural reallocation that created coordination mechanisms (author-publisher assignment) while generating extraction flows (against Stationers, against authors without bargaining power). This mixed character is the reading's substantive claim: the reallocation created new institutional positions (author as right-holder, publisher as rights acquirer) that were simultaneously enabling and constraining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_vs_creation_ambiguity,
    'Did the statute recognize pre-existing authorial rights or CREATE new ones through institutional reallocation?',
    'Historical analysis of pre-statute author compensation mechanisms and rights claims; comparison with parallel institutional contexts (Continental copyright traditions); examination of legislative intent documents and preamble language',
    'If recognition: Mountain classification is defensible (discovered natural right). If creation: Tangled Rope stands — institutional reallocation with mixed beneficiary/victim structure. This omega routes the committer-frame ambiguity (which reading is ''true''?) into empirical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_vs_creation_ambiguity, empirical, 'Whether statute recognized or created authorial IP rights').

omega_variable(
    monopoly_transfer_mechanism,
    'Was the reallocation a transfer from Stationers to publishers, or a transfer from Crown (via guild monopoly) to a market mechanism?',
    'Structural analysis of post-statute publishing economics: did market concentration shift, or did monopoly power remain through de facto publisher control of rights acquisition?',
    'If Stationers→publishers: clear reallocation with identifiable winners. If Crown→market: reallocation of regulatory authority, not property rights. Changes who the primary beneficiary is and whether extraction flows toward publishers or remains Crown-controlled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monopoly_transfer_mechanism, empirical, 'Whether reallocation transferred monopoly or authority').

omega_variable(
    author_agency_in_assignment,
    'To what degree were author-to-publisher assignments voluntary vs coerced by economic necessity?',
    'Economic analysis of author bargaining power relative to publishers; study of non-assigned versus assigned works and outcomes; comparison of compensation terms across high-agency authors (well-connected, multiple bidders) versus low-agency (unknown, single publisher option)',
    'If coerced: extraction component of tangled_rope dominates, χ increases. If voluntary: coordination component dominates, χ decreases. This determines whether the reading depicts institutional reallocation as genuinely empowering authors or as converting them into assignees under market pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_agency_in_assignment, empirical, 'Degree of author agency in rights assignment').

omega_variable(
    competing_reading_logical_status,
    'Do the sibling readings (conceptual_emergence_reading, entangled_event_reading) logically foreclose this institutional reallocation reading, or do they coexist as alternative framings of the same event?',
    'Formal analysis of each reading''s core premise: does recognizing ''emergent IP concept'' (conceptual reading) rule out ''institutional reallocation'' (this reading)? Does recognizing ''entangled institutional and conceptual event'' (entangled reading) eliminate either alternative? Or do these readings describe different structural layers of the same historical moment?',
    'If foreclosed: this reading is incoherent within the same framework. If coexists: all three readings are live perspectives from different analytical positions. This omega is the committer-axis recognition step: documenting that the kernel itself is legitimately contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_reading_logical_status, conceptual, 'Logical status of competing kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_reallocation_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_statute, institutional_reallocation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(theater_early_statute, institutional_reallocation_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(theater_mature_statute, institutional_reallocation_reading, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_pre_statute, institutional_reallocation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(extractiveness_early_statute, institutional_reallocation_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(extractiveness_mature_statute, institutional_reallocation_reading, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(institutional_reallocation_reading, entangled_event_reading).
narrative_ontology:affects_constraint(institutional_reallocation_reading, publisher_assignment_market).
narrative_ontology:affects_constraint(institutional_reallocation_reading, crown_censorship_continuation).

% DUAL FORMULATION NOTE:
% institutional_reallocation_reading is one reading of statute_of_anne_ip_foundation kernel. Three constraint stories decompose the statute: institutional reallocation (property rights transfer), conceptual emergence (IP as novel category), and entangled event (institutional + conceptual simultaneously). Each has its own ε. The reallocation reading (ε=0.38, Tangled Rope) shows the statute as mixed coordination (author standing, market mechanism) and extraction (Stationers displaced, authors assigned). The emergence reading (ε likely ≥0.30, possibly Tangled Rope or Snare) emphasizes the conceptual novelty. The entangled reading (ε variant unknown) claims they cannot be separated. All three are linked via network.affects_constraints to indicate kernel family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_reallocation_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
