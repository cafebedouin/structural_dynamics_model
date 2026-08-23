% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright Constitutional Mandate — Corporate Enclosure Reading
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the corporate enclosure reading of the
 *   U.S. Constitution's Copyright Clause (Art. I, §8, cl. 8: 'To promote the
 *   Progress of Science and useful Arts, by securing for limited Times to
 *   Authors and Inventors the exclusive Right to their respective Writings
 *   and Discoveries'). The reading treats copyright as a natural property
 *   right requiring maximal protection, interprets 'limited Times' as
 *   permitting any extension short of explicit perpetuity, and drives serial
 *   term extensions (1976 Act, 1998 CTEA), anti-circumvention criminalization
 *   (DMCA 1201), and fair-use restriction. Beneficiaries are corporate
 *   incumbents (Disney, RIAA, MPAA); victims are derivative creators,
 *   educators, and archivists. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as a property-right coordination mechanism while
 *   the authored metrics describe a substantially extractive, actively
 *   enforced regime — the engine measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.8).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright Constitutional Mandate — Corporate Enclosure Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '88bb1789-4f83-4100-9233-1f8ead4e48fa').
narrative_ontology:cs_kernel_codification('88bb1789-4f83-4100-9233-1f8ead4e48fa', fixed_text).
narrative_ontology:cs_authority_grounding('88bb1789-4f83-4100-9233-1f8ead4e48fa', extraction).
narrative_ontology:cs_interpretation_layer_present('88bb1789-4f83-4100-9233-1f8ead4e48fa').
narrative_ontology:cs_reading_relation('88bb1789-4f83-4100-9233-1f8ead4e48fa', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_reading_relation('88bb1789-4f83-4100-9233-1f8ead4e48fa', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('88bb1789-4f83-4100-9233-1f8ead4e48fa', foundational, copyright_is_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('88bb1789-4f83-4100-9233-1f8ead4e48fa', copyright_is_natural_property_right, deontological).
narrative_ontology:cs_axiom('88bb1789-4f83-4100-9233-1f8ead4e48fa', foundational, limited_times_permits_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_permits_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('88bb1789-4f83-4100-9233-1f8ead4e48fa', limited_times_permits_maximal_extension, conventional).
narrative_ontology:cs_reference_frame('88bb1789-4f83-4100-9233-1f8ead4e48fa', constitutional_property_entitlement).
narrative_ontology:cs_drift_state('88bb1789-4f83-4100-9233-1f8ead4e48fa', post_ctea_1998, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('88bb1789-4f83-4100-9233-1f8ead4e48fa', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_as_natural_property_right).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, limited_times_permits_perpetual_extension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major rights-holding corporations (Disney, RIAA/MPAA member labels and studios) lobby Congress for term extensions, fund litigation to expand exclusive rights, and use trade agreements to harmonize maximalist standards globally. They collect monopoly rents from century-old works and control licensing ecosystems for derivative uses. Exit is trivial: they adapt business models to any regime and capture value across jurisdictions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, beneficiary).

% Remix artists, sampling musicians, fan-fiction writers, documentary filmmakers, and transformative creators who must clear rights or risk infringement liability. The public domain has effectively frozen since 1998; orphan works cannot be used safely; fair use is a defense not a right. Exit means abandoning their practice or moving to jurisdictions with broader exceptions — neither is realistic for most.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Teachers, professors, and educational institutions needing classroom copies, course packs, digital reserves, and distance-learning materials. Statutory exceptions (Section 110, TEACH Act) are narrow, technologically outdated, and overridden by license terms. They pay escalating license fees or self-censor curricula. Exit means non-compliance risk or degraded pedagogy.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Libraries, museums, and digital preservationists stewarding orphan works, deteriorating media, and born-digital collections. Section 108 exceptions permit limited preservation copying but not public access; DMCA 1201 blocks circumvention even for lawful preservation. They bear preservation costs without legal clarity. Exit means incomplete cultural record or unlawful activity.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    organized, generational, constrained, global).

% Civil-society organizations (EFF, Public Knowledge, Authors Alliance, Creative Commons, library associations) litigate, lobby, and campaign for balanced copyright. They are structurally excluded from the revolving-door negotiation between corporate lobbyists and congressional committees that produces term extensions and anti-circumvention rules.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    moderate, generational, mobile, global).

% Enact copyright term extensions (CTEA 1998, prior acts), anti-circumvention law (DMCA 1998), and implement trade-agreement mandates (TRIPS, bilateral FTAs). Campaign finance from corporate incumbents shapes the legislative agenda; public-domain advocates lack comparable access. They could shorten terms or broaden exceptions but face concentrated opposition and diffuse benefits.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress_legislators, agenda_setter,
    institutional, biographical, constrained, national).

% Interpret 'limited times' and 'promote the Progress' clauses. In Eldred v. Ashcroft (2003), the Supreme Court deferred to Congress via rational-basis review, holding that repeated extensions compliant with 'limited times' text are non-justiciable. They provide the judicial-ambiguity reading's deference mechanism while occasionally narrowing enforcement (e.g., Google v. Oracle fair use).
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Incentivizes creation of original works by granting authors and their assignees a temporary exclusive right to control reproduction, distribution, adaptation, and public performance — solving the public-goods problem of underinvestment in expressive works.
% TRANSFER_FUNCTION: Moves control and economic value from derivative creators, educators, archivists, and the public domain to corporate rights holders through serial term extensions (1790: 14+14 years → 1998: life+70 / 95 years corporate), criminalized circumvention of technological protection measures (DMCA 1201), and judicial narrowing of fair use — converting a temporary public-good instrument into a perpetual corporate asset class.
% ABSENT_VOICES: Derivative creators (remix artists, sampling musicians, fan-fiction writers), educators needing classroom and digital-reserve exceptions, archivists preserving orphan works and born-digital heritage, and the general public losing access to 20th-century cultural commons — they are structurally excluded from the congressional-negotiation / trade-agreement process that ratifies maximalist expansions.
% DISAPPEARANCE_RATIONALE: If maximalist copyright vanished overnight, the public domain would immediately absorb all pre-1954 works (and post-1978 works at life+50 Berne minimum), derivative creation would flourish without clearance friction, archives could digitize and provide access to orphan works, educational use would expand to statutory minimums, and the cultural economy would reorganize around permissionless innovation rather than licensing gatekeepers.
% FOUNDING_PROBLEM: The 1790 Copyright Act aimed to 'encourage learning' by securing authors' exclusive rights for 14 years (renewable once) — the founding problem was underproduction of books, maps, and charts in a fragile post-revolutionary printing market with no international protection.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Boldrin & Levine, 'Against Intellectual Monopoly'; B. Zorina Khan, 'The Democratization of Invention') document that the 1790 term was calibrated to 18th-century printing economics and that modern extensions serve incumbent rent extraction, not the founding incentive problem. The U.S. Copyright Office's own 2012 'Copyright and the Marketplace' study acknowledges term extensions do not incentivize new creation. No independent scholar outside the beneficiary set attests that life+70 / 95-year terms solve the 1790 problem.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because term extensions far exceed any incentive requirement (empirical consensus: optimal term ≈ 15–25 years), anti-circumvention law criminalizes lawful uses, and fair use has been narrowed to a defendant's burden. Suppression is high (0.8) because the regime actively blocks alternatives: public domain is frozen, orphan works are unusable, DRM is legally protected against circumvention even for permitted uses, and trade agreements export the regime globally. Theater ratio is moderate (0.4): the incentive function is real at the margin for new commercial works but performative for the vast majority of extended-term works (98% of commercially valueless works by year 20). Accessibility collapse (0.75) reflects the effective closure of the public domain and fair-use safe harbors. Resistance (0.6) is real but constrained: civil society litigates and lobbies but lacks the concentrated resources of corporate incumbents.
 *
 * PERSPECTIVAL GAP:
 *   The corporate incumbent seat experiences this as legitimate property protection (low χ); the derivative creator / educator / archivist seats experience it as enforced extraction with no coordination benefit (high χ). The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are structural beneficiaries (agenda_setter + beneficiary): they draft legislation, fund litigation, collect rents, and face near-zero exit costs (d ≈ 0.05). Derivative creators, educators, and archivists are targets (payer): they bear compliance costs, license fees, chilling effects, and preservation barriers with constrained exit (d ≈ 0.85). Public domain advocates are excluded from the legislative process (d ≈ 0.9). Congress is agenda_setter but constrained by campaign finance (d ≈ 0.4 — they could change the law but face asymmetric pressure). Courts are analytical observers with deference doctrine (d ≈ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (encouraging learning in a 1790 printing market) is dead; modern digital production and distribution have solved underproduction. The arrangement persists because corporate incumbents capture concentrated rents from term extensions while the costs (lost public domain, chilled derivative creation, archival paralysis) are diffuse. Mandatrophy is unresolved: the mandate has outlived its function but the constraint intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_right_naturalness,
    'Is the ''copyright as natural property right'' framing a genuine natural-law discovery or a constructed metaphor that benefits corporate incumbents by analogizing expressive works to tangible property?',
    'Genealogical analysis of the property-right rhetoric''s emergence in 19th-century judicial opinions and legislative debates; comparison with founding-era understanding (copyright as statutory monopoly, not natural right).',
    'If constructed, the property-right frame is a legitimating cover for extraction; if natural, the maximalist reading has stronger normative footing. Drives false-summit-mountain evaluation if the constraint were claimed as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_right_naturalness, conceptual, 'Whether the property-right framing is natural law or constructed cover.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine incentive coordination end and pure rent extraction begin in copyright term length?',
    'Empirical estimation of optimal copyright term from production-cost curves, discount rates, and marginal incentive effects (cf. Pollock, Rufus; Heald, Paul; Buccafusco, Christopher).',
    'If the coordination-extraction boundary lies far below current terms (life+70/95), the excess is pure extraction; if near current terms, the maximalist reading has coordination legitimacy. Informs tangled_rope vs snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Location of the incentive-coordination / rent-extraction boundary in term length.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of derivative creation, educational use, and archival access primarily structural (legal barriers, criminal penalties) or internalized (chilling effects, self-censorship, risk aversion)?',
    'Post-reform observation: if jurisdictions that broaden fair use or shorten terms see immediate flowering of derivative/educational/archival activity, suppression was largely structural; if activity remains suppressed, internalized chilling dominates.',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s extraction persists after legal barriers are removed because targets carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for derivative creators, educators, archivists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(copy_tr_t12, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(copy_tr_t24, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(copy_tr_t36, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(copy_tr_t48, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 48, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(copy_be_t12, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(copy_be_t24, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(copy_be_t36, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 36, 0.8).
narrative_ontology:measurement(copy_be_t48, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 48, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(copy_su_t12, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(copy_su_t24, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(copy_su_t36, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 36, 0.75).
narrative_ontology:measurement(copy_su_t48, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 48, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.15).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anticircumvention).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, ctea_1998).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, international_copyright_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is the corporate_enclosure_reading of the copyright_constitutional_mandate kernel. It decomposes the natural-language concept 'copyright term' into a structurally distinct claim with ε ≈ 0.85, beneficiaries (corporate_incumbents), victims (derivative_creators, educators, archivists), and active enforcement. The judicial_ambiguity_reading (deference to Congress) and public_scaffold_reading (public-good instrumentalism) are separate constraint stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, institutional, 0.05).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, moderate, 0.85).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
