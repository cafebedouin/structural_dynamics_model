% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The fair use four-factor test (17 U.S.C. §107) is a statutory framework
 *   for determining when unauthorized use of copyrighted material is
 *   permissible. This story instantiates the USER-CENTRIC READING: fair use
 *   is an affirmative user right, and the four factors are weighed to
 *   preserve public access and cultural production. Under this reading, the
 *   constraint's coordination function is enabling follow-on creativity,
 *   education, commentary, and cultural participation without
 *   permission-seeking friction. Its extraction falls primarily on rights
 *   holders who lose licensing revenue from uses the law declares
 *   non-infringing. The claimed type is ROPE because the structure solves a
 *   genuine collective-action problem — coordinating millions of
 *   decentralized cultural participants without central permission — with
 *   minimal coercive overhead. However, the theater_ratio has risen since the
 *   1990s as rights holders deploy automated enforcement (Content ID, DMCA
 *   takedowns) that chills legitimate fair uses, and as the 'transformative
 *   use' sub-doctrine has become a litigation battleground rather than a
 *   clear standard.
 *
 * KEY AGENTS:
 *   - public_users: Primary beneficiary (powerless/constrained) — exercises fair use for personal, educational, creative purposes; chilled by automated enforcement
 *   - educational_institutions: Primary beneficiary (organized/constrained) — relies on fair use for teaching, research, preservation; bears compliance costs
 *   - libraries_archives: Primary beneficiary (organized/constrained) — preservation and access mission depends on fair use; targeted by controlled digital lending disputes
 *   - documentary_filmmakers: Primary beneficiary (moderate/constrained) — incorporates copyrighted material for commentary/criticism; faces clearance culture pressure
 *   - academic_researchers: Primary beneficiary (moderate/mobile) — text/data mining, quotation, reproduction for analysis; varies by discipline and jurisdiction
 *   - creative_remixers: Primary beneficiary (moderate/constrained) — transformative works, parody, remix; high uncertainty zone
 *   - rights_holders: Primary victim (powerful/organized/institutional) — loses licensing revenue from declared fair uses; enforces via automated systems and litigation
 *   - commercial_publishers: Secondary victim (institutional/arbitrage) — business models disrupted by unauthorized but lawful uses; lobbies for narrower exceptions
 *   - licensing_collectives: Secondary victim (organized/arbitrage) — revenue from licensing fair uses displaced; advocates for collective licensing schemes
 *   - courts: Agenda setter (institutional/analytical) — adjudicates four-factor balancing; transformative use doctrine evolved judicially
 *   - copyright_office: Agenda setter (institutional/analytical) — rulemaking, DMCA exemptions, policy guidance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.22).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.18).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '7465a1c4-999f-4e2a-945d-c4b73f5d17dc').
narrative_ontology:cs_kernel_codification('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', formalized).
narrative_ontology:cs_authority_grounding('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', lineage).
narrative_ontology:cs_interpretation_layer_present('7465a1c4-999f-4e2a-945d-c4b73f5d17dc').
narrative_ontology:cs_reading_relation('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', foundational, fair_use_as_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', fair_use_as_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', foundational, cultural_production_requires_unpermissioned_access).
narrative_ontology:cs_axiom_status(cultural_production_requires_unpermissioned_access, holdable).
narrative_ontology:cs_axiom_grounding('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', cultural_production_requires_unpermissioned_access, instrumental).
narrative_ontology:cs_axiom('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', secondary, transformative_use_not_sole_factor).
narrative_ontology:cs_axiom_status(transformative_use_not_sole_factor, holdable).
narrative_ontology:cs_axiom_grounding('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', transformative_use_not_sole_factor, conventional).
narrative_ontology:cs_reference_frame('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', statutory_four_factor_balance_1976).
narrative_ontology:cs_drift_state('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', contemporary_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7465a1c4-999f-4e2a-945d-c4b73f5d17dc', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, creative_remixers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, commercial_publishers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, licensing_collectives).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, fair_use_as_affirmative_right).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, cultural_production_requires_access).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, first_amendment_copyright_accommodation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Everyday users who quote, share, remix, and build on copyrighted works for personal expression, education, and cultural participation. They rely on fair use implicitly — memes, reaction videos, fan fiction, classroom presentations. They have no legal resources; exit means self-censorship or platform removal. Automated takedown systems (Content ID, DMCA bots) chill their uses without judicial review.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    powerless, biographical, constrained, national).

% Universities, schools, and colleges that rely on fair use for course reserves, classroom copying, distance education, and research. They invest in copyright compliance offices and fair use guidelines. Their exit is constrained by accreditation requirements and the necessity of using copyrighted materials for teaching. They face pressure from publishers to license instead of relying on fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Public and research libraries, archives, and memory institutions. Their mission — preservation, access, interlibrary loan, controlled digital lending — depends on fair use and §108 exceptions. They face litigation from publishers over controlled digital lending (e.g., Hachette v. Internet Archive) and ebook licensing terms that circumvent fair use. Exit means failing their public mission.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_archives, beneficiary,
    organized, generational, constrained, national).

% Filmmakers who incorporate copyrighted footage, music, and images for commentary, criticism, and historical documentation. They developed the Documentary Filmmakers' Statement of Best Practices in Fair Use (2005) to coordinate practice. Their exit is constrained by errors-and-omissions insurance requirements and distributor clearance demands. Without fair use, many documentaries cannot be made.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Scholars across disciplines who quote, reproduce, and analyze copyrighted works. Text and data mining, computational humanities, and large-scale corpus analysis push fair use boundaries. Exit varies: some disciplines have strong fair use norms; others face publisher pressure. International researchers face different exception regimes (fair dealing, specific exceptions).
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, academic_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Artists, musicians, video essayists, and creators who transform existing works into new expression — parody, critique, collage, sampling. They operate in the highest-uncertainty zone of transformative use doctrine. Platform algorithms (YouTube Content ID, TikTok muting) automatically flag and monetize or remove their work. Exit means changing platform or practice; litigation defense is prohibitively expensive.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, creative_remixers, beneficiary,
    moderate, biographical, constrained, national).

% Individual authors, artists, musicians, and creators who hold copyright. They lose licensing revenue when uses are deemed fair. They are also agenda setters: they lobby for copyright term extension, narrower exceptions, and stronger enforcement. Their exit is arbitrage-grade — they can license, enforce, or adapt business models. But the *class* of rights holders includes corporations with structural power far beyond individual creators.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_holders, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, rights_holders, agenda_setter).

% Book, journal, music, and media publishers whose business models depend on controlling reproduction and licensing. They view fair use as a threat to licensing revenue and invest heavily in lobbying, litigation, and technological enforcement (DRM, controlled digital lending opposition, textbook licensing). They have arbitrage-grade exit: they shape legislation (DMCA, CASE Act), negotiate platform deals, and shift to licensing models that bypass fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_publishers, payer,
    institutional, generational, arbitrage, global).

% Collective management organizations (ASCAP, BMI, SESAC, CCC, etc.) that license repertoires at scale. Fair use displaces licensing revenue, particularly for educational and institutional uses. They advocate for collective licensing schemes that would replace fair use with paid permissions. Their exit is arbitrage: they administer the alternative (licensing) and profit from its expansion.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, licensing_collectives, payer,
    organized, generational, arbitrage, global).

% Federal courts (especially Second, Ninth Circuits; Supreme Court) that adjudicate fair use disputes. They created the 'transformative use' framework (Campbell v. Acuff-Rose, 1994; Google v. Oracle, 2021) that now dominates factor one. Their decisions set the operational boundary of the constraint. They are analytical observers of the structure they maintain, but their precedent-setting power makes them agenda setters.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% U.S. Copyright Office: registers claims, administers DMCA rulemaking (triennial exemptions), advises Congress, and issues policy reports. It shapes the constraint's practical scope through exemption classes (e.g., for text/data mining, accessibility, repair, remix). It is both agenda setter (rulemaking authority) and analytical observer (policy analysis).
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_office, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables decentralized cultural production, education, commentary, and access without requiring permission from or payment to rights holders for every incorporation of existing works. Solves the collective-action problem of millions of creators and users needing to build on a shared cultural commons without transaction costs that would halt most follow-on creativity.
% TRANSFER_FUNCTION: Transfers the value of unauthorized but socially valuable uses — education, commentary, parody, research, preservation, transformative creation — from rights holders (who would charge licensing fees) to users (who exercise the use without payment). The four factors operationalize this transfer: purpose (nonprofit/educational favored), nature (factual over creative), amount (less favored), market harm (the central valve).
% ABSENT_VOICES: Future creators and users whose works don't yet exist but will depend on the cultural commons fair use preserves. Indigenous communities whose traditional knowledge is often treated as public domain by copyright but whose cultural production is constrained by Western IP frameworks. Incarcerated and institutionalized people with no internet access whose fair use rights are theoretically intact but practically nonexistent. Global South users subject to U.S. platform terms but not U.S. fair use law.
% DISAPPEARANCE_RATIONALE: If fair use vanished overnight, education would shift to licensed-only materials (cost explosion), documentary filmmaking would collapse for many subjects, libraries could not preserve or lend digitally, remix culture would move underground or onto encrypted channels, academic research would require permission for every quotation and data mining run, and platforms would default to maximalist takedown. The cultural ecosystem would reorganize around permission and payment — a fundamental rearrangement.
% FOUNDING_PROBLEM: The 1976 Copyright Act codified fair use to balance copyright's monopoly grant with the First Amendment and the constitutional purpose of 'promoting the Progress of Science and useful Arts.' The founding problem was that a rigid exclusive right would strangle the very creativity copyright aims to foster — commentary, criticism, education, and follow-on innovation all require using existing works. Fair use was the statutory safety valve.
% FOUNDING_PROBLEM_CORROBORATION: The Copyright Office and courts attest the problem is live (ongoing need for safety valve). Rights holders and publishers attest the problem is substantially solved by licensing markets and that fair use has expanded beyond its founding scope (testimony in DMCA section 512 hearings, Authors Guild v. Google briefing). Independent scholars (Pam Samuelson, Jessica Litman, James Boyle) and library associations (ARL, ALA) corroborate from outside the beneficiary set that the founding problem persists but the constraint's operation has been distorted by enforcement drift and clearance culture.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) is low because the constraint *limits* extraction from users — it carves out space where copyright's default extraction does not apply. The extraction that exists falls on rights holders who lose licensing revenue; from the user's seat, ε is negative (subsidy). Suppression (0.18) is low in statutory terms — the law does not forbid fair use — but operational suppression is higher due to automated takedown systems, clearance culture, and litigation risk (captured in theater_ratio 0.35). Theater rose from ~0.10 (1976) to ~0.40 (2005) as DMCA safe harbors and automated filtering created a gap between statutory right and practical exercisability; it has modestly declined as platform-creator negotiations and DMCA exemptions created partial workarounds. Accessibility_collapse (0.30) is moderate: alternatives (licensing, public domain, orphan works) exist but are incomplete. Resistance (0.45) is moderate: rights holders actively litigate and lobby to narrow fair use; users and institutions push back via best practices codes and litigation defense funds.
 *
 * PERSPECTIVAL GAP:
 *   From the public_user seat (powerless, constrained exit), the constraint is a vital but fragile coordination mechanism — low ε, moderate suppression, but high theater (the right exists on paper but is hard to exercise). From the rights_holder seat (powerful, arbitrage exit), the constraint is extractive — they lose revenue with no compensation, and the four-factor test's unpredictability creates settlement leverage. From the court seat (institutional, analytical), the constraint is a balancing framework that has evolved toward transformative use as a dominant but unstable heuristic. The engine computes these seat-level divergences from the structural data; the claimed type (rope) reflects the user-centric reading's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (public_users, educational_institutions, libraries_archives, documentary_filmmakers, academic_researchers, creative_remixers) are declared because the constraint affirmatively enables their activities — they are the coordination function's intended participants. Victims (rights_holders, commercial_publishers, licensing_collectives) are declared because they bear the revenue loss from uses declared non-infringing. The directionality derivation assigns low d (beneficiary) to users and high d (target) to rights holders. The engine scales ε by these d values and by spatial_scope (national for US law, but global in effect via platform terms).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling cultural production without permission friction) remains LIVE but CONTESTED. The constraint has not atrophied — fair use is more litigated and more culturally central than in 1976. However, the coordination/extraction balance has shifted: rights holders have built enforcement infrastructure (automated takedown, Content ID) that extracts from users *outside* the statutory framework (false positives, chilling effects). This is not mandatrophy (the constraint's function hasn't disappeared) but *enforcement drift* — the constraint's shadow enforcement has become more extractive than the constraint itself. The theater_ratio rise documents this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_fair_use_user_centric,
    'Does this constraint instantiate the user_centric_reading of the fair_use_four_factor_test kernel, or is the kernel label conflating multiple structurally distinct constraints?',
    'Decompose the ''fair use'' label into kernel readings with distinct ε values, beneficiary structures, and victim sets. Verify that the user_centric_reading has low extraction on unauthorized use and beneficiaries = public/educational users while the creator_centric_reading has higher extraction and beneficiaries = rights holders.',
    'If the kernel label conflates distinct constraints, each reading must be a separate constraint story with its own ε and classification. Link them via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_fair_use_user_centric, conceptual, 'Whether ''fair use four-factor test'' is one constraint or a kernel with multiple readings').

omega_variable(
    transformative_use_boundary_ambiguity,
    'Where does the boundary between ''transformative use'' and ''derivative use'' fall in practice, and does judicial application of the four-factor test produce consistent outcomes?',
    'Empirical analysis of case law outcomes coding transformative vs. derivative findings against the four statutory factors and market harm findings.',
    'If boundary is unstable, extractiveness and suppression vary by jurisdiction/era — the constraint may be multiple constraints or a contested reading rather than a stable structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundary_ambiguity, empirical, 'Whether transformative use doctrine creates predictable coordination or unpredictable extraction').

omega_variable(
    market_harm_measurement_problem,
    'How is ''market harm'' measured when the alleged harm is to potential licensing markets that do not yet exist?',
    'Economic analysis of licensing market formation and judicial willingness to recognize speculative markets as cognizable harm.',
    'If speculative markets count as harm, extraction shifts toward rights holders (higher ε); if only actual markets count, extraction stays low. This is the structural hinge between user_centric and creator_centric readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_measurement_problem, conceptual, 'Whether potential licensing markets constitute cognizable market harm').

omega_variable(
    user_right_vs_exception_framing,
    'Is fair use structurally an affirmative user right (coordination function) or a narrow exception to property rights (extraction from users via uncertainty)?',
    'Track whether the constraint''s operation produces net coordination (users reliably exercising recognized uses) or net extraction (users chilled by uncertainty, rights holders extracting settlements).',
    'If affirmative right, classification tends toward rope/tangled_rope with low ε; if exception, classification tends toward snare/tangled_rope with higher ε on users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_right_vs_exception_framing, conceptual, 'Structural framing of fair use: right vs. exception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t1998, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(fair_tr_t2005, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(fair_tr_t2015, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.15).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(fair_be_t1998, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(fair_be_t2005, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(fair_be_t2015, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.1).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(fair_su_t1998, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement(fair_su_t2005, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(fair_su_t2015, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, information_standard).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.02).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, dmca_section_512_safe_harbor).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, orphan_works_problem).

% DUAL FORMULATION NOTE:
% Fair use four-factor test kernel decomposes into three constraint stories: user_centric_reading (this story, rope, ε≈0.22), creator_centric_reading (snare/tangled_rope, higher ε on users), transformative_use_reading (tangled_rope, distinct transformativeness-dominance structure). The ε values differ because each reading weights the four factors differently, producing different extraction profiles. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, powerless, 0.15).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, organized, 0.25).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, institutional, 0.75).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
