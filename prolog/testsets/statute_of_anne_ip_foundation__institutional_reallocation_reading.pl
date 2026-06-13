% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne IP Foundation (Institutional Reallocation Reading)
 *   domain: legal/institutional/economic
 *
 * SUMMARY:
 *   The Statute of Anne (1710) reallocated institutional authority over
 *   published works from the Stationers' Company (who held perpetual
 *   inherited rights via guild custom) to individual authors (who gained
 *   limited statutory ownership of 14 years, renewable once). This reading
 *   treats the statute as an INSTITUTIONAL REALLOCATION: the same property
 *   object (published literary works) changed hands from one occupant class
 *   (guild members) to another (authors), with the Crown as the agenda-setter
 *   defining the new terms. The founding bottleneck—the Stationers' perpetual
 *   monopoly preventing reprints and new editions—is solved by the
 *   reallocation. This reading is distinct from the conceptual_emergence
 *   reading (which emphasizes the statute's creation of copyright as a novel
 *   regulatory concept) and the entangled_event reading (which holds that
 *   institutional and conceptual change cannot be separated). Under this
 *   reading, the institutional reallocation is primary; the conceptual
 *   novelty is a byproduct of the new occupant structure.
 *
 * KEY AGENTS:
 *   - authors: gain statutory ownership claim, limited to 14 years renewable once; shift from powerless (guild excluded) to moderate (statutory claimant)
 *   - licensed_publishers: acquire negotiating position with authors; retain constraint of Crown licensing monopoly
 *   - stationers_company: loses perpetual inherited claim, transitions from rent collector to licensed operator; still controls printing infrastructure but now under statutory constraint
 *   - the_crown: agenda-setter; enacts statute to rebalance monopoly between guild (below) and authors (new statutory class); retains licensing authority
 *   - readers_and_scholars: gain access to expanded competitive corpus (publishers now compete for author-held rights); remain subject to licensing and term limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.38).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.22).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne IP Foundation (Institutional Reallocation Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal/institutional/economic").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'b429f99a-8bfd-401b-bf11-cb61465ea1f0').
narrative_ontology:cs_kernel_codification('b429f99a-8bfd-401b-bf11-cb61465ea1f0', formalized).
narrative_ontology:cs_authority_grounding('b429f99a-8bfd-401b-bf11-cb61465ea1f0', lineage).
narrative_ontology:cs_interpretation_layer_present('b429f99a-8bfd-401b-bf11-cb61465ea1f0').
narrative_ontology:cs_reading_relation('b429f99a-8bfd-401b-bf11-cb61465ea1f0', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b429f99a-8bfd-401b-bf11-cb61465ea1f0', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('b429f99a-8bfd-401b-bf11-cb61465ea1f0', foundational, institutional_occupancy_is_primary_analytical_unit).
narrative_ontology:cs_axiom_status(institutional_occupancy_is_primary_analytical_unit, holdable).
narrative_ontology:cs_axiom_grounding('b429f99a-8bfd-401b-bf11-cb61465ea1f0', institutional_occupancy_is_primary_analytical_unit, conventional).
narrative_ontology:cs_axiom('b429f99a-8bfd-401b-bf11-cb61465ea1f0', foundational, reallocation_from_guild_to_authors_solves_monopoly_bottleneck).
narrative_ontology:cs_axiom_status(reallocation_from_guild_to_authors_solves_monopoly_bottleneck, holdable).
narrative_ontology:cs_axiom_grounding('b429f99a-8bfd-401b-bf11-cb61465ea1f0', reallocation_from_guild_to_authors_solves_monopoly_bottleneck, empirically_contingent).
narrative_ontology:cs_reference_frame('b429f99a-8bfd-401b-bf11-cb61465ea1f0', stationers_company_perpetual_monopoly).
narrative_ontology:cs_drift_state('b429f99a-8bfd-401b-bf11-cb61465ea1f0', fifty_years_post_statute, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b429f99a-8bfd-401b-bf11-cb61465ea1f0', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, licensed_publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric starts high (0.68) because the pre-statute Stationers' monopoly is the measuring baseline: perpetual holding, complete veto over reprints, no author alternative. The statute's reallocation REDUCES extractiveness sharply—authors now hold property and can negotiate; licensed publishers can acquire rights competitively; readers gain public-domain access at term end. By t=50 (50 years post-statute), extractiveness has settled at 0.38—substantially lower than the pre-statute monopoly rent but not zero: the Crown retains licensing authority, term limits remain, the printing monopoly persists. Suppression follows: the statutory regime requires less active suppression because authors now have incentives to cooperate (they own the rights), and publishers compete rather than inherit. Theater ratio is low (0.15–0.22) because the statute's coordination function is genuine—incentivizing authorship and competitive publishing—not performative. The measurement grid shares all time points across all three metrics, anchored to the statute's enactment at t=0.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' Company and licensed publishers should compute to opposite seats on the beneficiary/victim axis. For the Company, the statute is a loss—victims are those who claimed perpetual rights and no longer have them; the Company is still an organized actor but now pays (via term limits) rather than collects. For licensed publishers, the statute is a gain—they are beneficiaries who gain negotiating access previously blocked by the Company. Both are powerful institutional actors, both operate under Crown licensing, but the statute reallocates the institutional ground under them: one loses property claims, the other gains access. The engine computes this divergence from the base_properties declarations (Company as victim, publishers as beneficiary) and the power/exit atoms (organized/trapped for Company, organized/constrained for publishers). Authors sit at the fulcrum: they are the beneficiaries who gain the statutory property claim the statute grants, and they are the new occupants of the institutional space that was previously held by the Company.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by seat. Authors shift from near-target (d ≈ 0.85 pre-statute: they produce the content the Stationers monopolize, trapped, no alternative) to near-beneficiary (d ≈ 0.25 post-statute: they own limited rights, gain negotiating leverage, can refuse licensing). Licensed publishers shift from moderate (d ≈ 0.55 pre-statute: they inherit titles but cannot enter the market freely) to low-beneficiary (d ≈ 0.30 post-statute: they can bid for author rights, compete, but remain subject to licensing). The Stationers' Company shifts from powerful beneficiary (d ≈ 0.15 pre-statute: they collect perpetual rents on inherited backlist) to powerful payer (d ≈ 0.75 post-statute: they lose perpetual claims and must operate under statutory term limits). Readers and scholars shift from powerless targets (d ≈ 0.80 pre-statute: price-takers, no public domain, stuck with suppressed backlist) to powerless balanced-payers (d ≈ 0.50 post-statute: prices still set by publishers, but public domain exists at term end, corpus expands). These directionality shifts are COMPUTED by the engine from the structural beneficiary/victim data and power/exit declarations; the authored claim (rope) and metrics (descending extractiveness) establish the framework but do not determine d per seat—the engine's derivation does.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not present. The statute's founding problem (Stationers' perpetual monopoly bottleneck) remains live 50 years post-statute: the statute's reallocation to authors continues to incentivize authorship and competitive publishing, solving the bottleneck it was built to address. The disappearance verdict is world_rearranges: if the statute vanished and the Stationers' perpetual claim reverted, the corpus would contract, authorship incentives would weaken, and the distribution chain would reorganize around guild control. The founding problem and its solution are coterminous; mandatrophy would signal if the constraint persisted without solving the problem it was built for, which is not the case here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_primacy,
    'Does the institutional reallocation (who holds the right) cause the conceptual emergence (copyright as a novel category), or are the two aspects inseparably entangled?',
    'Historical counterfactual analysis: if authors had been granted the same limited statutory right but the Stationers'' Company had been eliminated entirely (no guild monopoly replacement), would copyright emerge as a conceptual category? Alternatively, analyze whether the conceptual move (from perpetual property to limited regulatory right) is logically dependent on the institutional shift (from guild occupancy to author occupancy).',
    'If institutional primacy is established, this reading''s framing holds and the sibling conceptual_emergence_reading is a downstream effect. If entanglement is established, the entangled_event_reading is more accurate, and this reading omits a necessary structural dimension. If conceptual primacy is established, this reading has inverted the causal direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_primacy, conceptual, 'Whether institutional reallocation is the primary change or a byproduct of conceptual emergence.').

omega_variable(
    crown_agenda_vs_parliamentary_compromise,
    'Is the Statute of Anne the Crown''s deliberate reallocation strategy (institutional redesign from above), or a compromise outcome from Parliament''s mediation between the Stationers'' Company and author/publisher interests?',
    'Analysis of parliamentary debate records, Crown correspondence, and the Stationers'' Company''s lobbying positions. Did the Crown propose the reallocation and impose it, or did Parliament negotiate it over the Company''s objections?',
    'If Crown agenda, the reallocation is an exercise of centralized authority redesigning monopoly distribution. If compromise, the reallocation emerges from contention between three institutional actors (Crown, Company, Parliament) with different interests. This affects whether to classify the constraint''s enforcement as unilateral (Crown) or negotiated (Parliament mediating).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_agenda_vs_parliamentary_compromise, empirical, 'Whether the statute represents Crown institutional design or parliamentary compromise.').

omega_variable(
    reallocation_completeness,
    'Does the statute fully reallocate perpetual rights from the Stationers'' Company to authors, or does it allow the Company to retain inherited holdings in pre-1710 works?',
    'Close reading of the statute''s grandfather clauses and exemptions. Historical records of Company holdings post-statute.',
    'If the reallocation is incomplete (Company retains some perpetual holdings), then the institutional change is partial, not total, and the extractiveness decline should plateau at a level reflecting the Company''s retained monopoly rents. If complete, the decline to 0.38 reflects only the Crown''s licensing constraint, not lingering guild monopoly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reallocation_completeness, empirical, 'The scope of the institutional reallocation: does it apply retroactively to all published works, or only prospectively to new works?').

omega_variable(
    author_property_vs_regulatory_license,
    'Is the statute granting authors a true property right (transferable, defensible, inalienable) or a regulatory license revocable by the Crown?',
    'Legal analysis of the statute''s language and subsequent case law. Can authors transfer their statutory right to heirs or assignees? Can the Crown revoke it? Are there conditions under which the right lapses?',
    'If true property, authors become occupants of an institutionalized position (the reading''s frame). If regulatory license, authors hold a revocable grant from the Crown, and the institutional reallocation is conditional—the Crown remains the primary occupant. This affects whether to classify the beneficiary position of authors as secure or precarious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(author_property_vs_regulatory_license, empirical, 'Whether the statute creates transferable property or conditional regulatory permission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_anne_theater_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(statute_anne_theater_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(statute_anne_theater_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(statute_anne_theater_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(statute_anne_theater_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement(statute_anne_theater_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(statute_anne_extractiveness_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(statute_anne_extractiveness_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(statute_anne_extractiveness_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(statute_anne_extractiveness_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(statute_anne_extractiveness_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(statute_anne_extractiveness_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(statute_anne_suppression_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(statute_anne_suppression_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(statute_anne_suppression_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(statute_anne_suppression_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(statute_anne_suppression_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 32, 0.23).
narrative_ontology:measurement(statute_anne_suppression_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% The statute_of_anne_ip_foundation kernel is instantiated by three constraint stories corresponding to three analytical readings: institutional_reallocation_reading (this story), conceptual_emergence_reading, and entangled_event_reading. Each reading has different ε, beneficiary structure, and founding problem interpretation. The institutional_reallocation reading emphasizes the occupancy shift (guild → authors). The conceptual_emergence reading emphasizes the emergence of limited copyright as a novel regulatory category. The entangled_event reading holds that institutional and conceptual change are inseparable. Each story should be authored independently with its own metrics and stakeholder analysis; the sibling constraints are linked via network.affects_constraints to enable corpus-level analysis of kernel contest representation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
