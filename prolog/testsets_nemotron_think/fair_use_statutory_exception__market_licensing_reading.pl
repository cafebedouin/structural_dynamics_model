% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Market Licensing Reading (Factor 4 Dominance)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'market licensing reading' of the fair
 *   use statutory exception (17 U.S.C. § 107). Under this reading, the fourth
 *   fair use factor — effect on the potential market — dominates the analysis
 *   such that any use for which a licensing market exists or could be
 *   constructed is presumptively unfair. The doctrine's transformative
 *   purpose (enabling criticism, commentary, education, research) collapses:
 *   fair use survives only for de minimis uses or uses where no rightsholder
 *   can be found to demand a license. The constraint presents itself as
 *   protecting creators' property rights (coordination story) but operates as
 *   a mechanism for rightsholders and licensing intermediaries to extract
 *   revenue from all downstream uses that can be monetized, including
 *   transformative uses that the statute was designed to protect. The claimed
 *   type is 'snare' — pure extraction where the coordination rationale is
 *   cover; the metrics reflect near-total extraction (ε=0.92) and high
 *   suppression (0.88) with moderate theater (0.45) as courts still recite
 *   the four-factor test while making factor 4 dispositive.
 *
 * KEY AGENTS:
 *   - rightsholders: Primary beneficiary (institutional/arbitrage) — major studios, labels, publishers collect licensing revenue and set terms
 *   - licensing_collectives: Beneficiary (institutional/arbitrage) — ASCAP, BMI, CCC, HFA administer collective licensing and take administrative cuts
 *   - publishers: Beneficiary (institutional/arbitrage) — control distribution channels and licensing gatekeeping
 *   - downstream_users: Primary target (organized/constrained) — tech platforms, businesses, individuals must license or forego use
 *   - educators: Target (organized/constrained) — educational institutions pay coursepack licenses, streaming licenses, database subscriptions
 *   - researchers: Target (organized/constrained) — text/data mining, corpus analysis, reproduction for analysis require licenses
 *   - transformative_creators: Target (moderate/identity_locked) — parodists, critics, remix artists, documentarians whose practice depends on fair use but face licensing demands
 *   - libraries_archives: Target (institutional/constrained) — preservation, lending, interlibrary loan, digital access constrained by licensing
 *   - courts: Observer (institutional/analytical) — interpret and apply the four-factor test; factor 4 dominance makes them enforcement arm
 *   - congress: Excluded (institutional/analytical) — could amend §107 but has not; legislative silence treated as acquiescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.92).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Market Licensing Reading (Factor 4 Dominance)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '7cee9ee7-80dd-47ea-ae84-24d4d551696c').
narrative_ontology:cs_kernel_codification('7cee9ee7-80dd-47ea-ae84-24d4d551696c', formalized).
narrative_ontology:cs_authority_grounding('7cee9ee7-80dd-47ea-ae84-24d4d551696c', extraction).
narrative_ontology:cs_interpretation_layer_present('7cee9ee7-80dd-47ea-ae84-24d4d551696c').
narrative_ontology:cs_reading_relation('7cee9ee7-80dd-47ea-ae84-24d4d551696c', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('7cee9ee7-80dd-47ea-ae84-24d4d551696c', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('7cee9ee7-80dd-47ea-ae84-24d4d551696c', foundational, licensing_market_precludes_fair_use).
narrative_ontology:cs_axiom_status(licensing_market_precludes_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('7cee9ee7-80dd-47ea-ae84-24d4d551696c', licensing_market_precludes_fair_use, conventional).
narrative_ontology:cs_axiom('7cee9ee7-80dd-47ea-ae84-24d4d551696c', secondary, fair_use_null_where_monetizable).
narrative_ontology:cs_axiom_status(fair_use_null_where_monetizable, holdable).
narrative_ontology:cs_axiom_grounding('7cee9ee7-80dd-47ea-ae84-24d4d551696c', fair_use_null_where_monetizable, empirically_contingent).
narrative_ontology:cs_reference_frame('7cee9ee7-80dd-47ea-ae84-24d4d551696c', statutory_four_factor_framework).
narrative_ontology:cs_drift_state('7cee9ee7-80dd-47ea-ae84-24d4d551696c', contemporary_digital_licensing_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7cee9ee7-80dd-47ea-ae84-24d4d551696c', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rightsholders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_collectives).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, publishers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, downstream_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, libraries_archives).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, copyright_as_property_right).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_precludes_fair_use).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, licensing_market_exhausts_fair_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major studios, record labels, and publishers hold copyright portfolios and set licensing terms for all downstream uses. They lobby for expansive copyright interpretation, fund litigation to establish precedent, and collect licensing revenue directly or through collectives. Their exit is arbitrage-grade: they choose jurisdictions, forums, and enforcement strategies; they can monetize works through multiple channels (licensing, direct sales, advertising). They administer the constraint by defining what counts as a 'potential market.'
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rightsholders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rightsholders, beneficiary).

% Collective management organizations (ASCAP, BMI, SESAC, CCC, HFA, SoundExchange) administer blanket licenses, collect royalties, and distribute to rightsholders minus administrative fees (10-20%). They benefit from the market licensing reading because it expands the universe of licensable uses. They lobby for statutory licenses and against fair use expansions. Their exit is arbitrage: they operate in multiple territories, adapt repertoire, and face no meaningful competition for their mandated roles.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_collectives, beneficiary,
    institutional, generational, arbitrage, national).

% Trade, academic, and educational publishers control distribution channels and licensing gatekeeping. They require authors to transfer copyright, then license back limited rights. They operate coursepack licensing, e-book lending licenses, text/data mining licenses, and permissions departments. The market licensing reading validates their business model: every use becomes a permission request. Exit is arbitrage: they diversify across content types, territories, and licensing models.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Technology platforms, businesses, and individual users who incorporate copyrighted works into new products, services, or personal uses. They face a binary: negotiate licenses (costly, uncertain, time-consuming) or risk infringement liability (statutory damages, injunctions). Fair use defense is theoretically available but practically foreclosed by factor-4 dominance — if a license could be obtained, fair use fails. Their exit is constrained: they cannot easily avoid copyrighted works in digital culture, and building alternatives (public domain, CC) is incomplete for contemporary works.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, downstream_users, payer,
    organized, biographical, constrained, national).

% K-12 schools, universities, and educational institutions that reproduce, display, distribute, and perform works for teaching. They pay coursepack licenses (CCC), streaming licenses (Swank, Kanopy), database subscriptions, and permissions fees. The TEACH Act and §110 provide narrow safe harbors but the market licensing reading treats any use with a licensing option as presumptively infringing. Exit is constrained: curriculum requires contemporary works; fair use guidelines (CONFU, ARL) are voluntary and narrowing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educators, payer,
    organized, biographical, constrained, national).

% Academic and corporate researchers conducting text/data mining, corpus linguistics, computational analysis, and systematic reproduction for study. They need to copy entire works for analysis. Publishers demand TDM licenses with restrictive terms (non-commercial only, no redistribution, specific tools). Fair use for research (factor 1) is overridden by factor 4: if a TDM license exists, fair use fails. Exit is constrained: research questions dictate corpus; public domain is insufficient for contemporary questions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, researchers, payer,
    organized, biographical, constrained, national).

% Parodists, critics, remix artists, documentarians, memoirists, and fan creators whose work transforms existing expression. Their creative identity is constituted through fair use — they cannot 'exit' to non-transformative work without ceasing to be who they are. Rightsholders demand licenses for transformative uses (e.g., sampling, quotation, adaptation); the market licensing reading treats transformative use as a licensable market. Identity lock: their practice requires engaging with copyrighted culture; licensing is economically impossible for most; self-censorship is the common outcome.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, identity_locked, national).

% Libraries and archives conducting preservation, digitization, interlibrary loan, controlled digital lending, and orphan works access. They pay for e-book licenses (with metered access), database subscriptions, and permissions. Section 108 provides limited exceptions but the market licensing reading treats any use with a licensing option as outside §108. Exit is constrained: their mission requires providing access to copyrighted works; they cannot 'choose' public domain collections for contemporary needs; controlled digital lending is litigated as market harm.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, libraries_archives, payer,
    institutional, generational, constrained, national).

% Federal courts (especially 2nd, 9th, DC Circuits) interpret the four fair use factors. Since Harper & Row (1985) and Campbell (1994), factor 4 (market effect) has become the 'most important' factor. Courts perform the four-factor test but factor 4 dominance makes fair use analysis a market harm inquiry. They neither collect nor pay licensing fees but their interpretive choices determine the constraint's reach. Circuit splits (9th Circuit more transformative-friendly) create geographic variance. Their exit is analytical: they could adopt a different reading but stare decisis and institutional incentives favor the dominant reading.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, observer,
    institutional, generational, analytical, national).

% The legislative branch that enacted §107 and could amend it. Has not substantively revised fair use since 1976 despite massive technological change. Legislative silence is treated by courts as acquiescence to factor-4 dominance. Rightsholder lobbying (RIAA, MPAA, AAP) vastly outspends user-side advocacy. Congress could restore transformative fair use, create safe harbors, or codify factor balancing — but is excluded from the doctrinal conversation by design. Their exit is analytical: they have the power to change the constraint but face political economy barriers.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, congress, excluded,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rightsholders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures creators and rightsholders receive compensation for uses of their works by channeling all monetizable uses through licensing markets, theoretically solving the transaction cost problem of individual permissions.
% TRANSFER_FUNCTION: Moves licensing revenue from downstream users (educators, researchers, platforms, transformative creators, libraries) to rightsholders and licensing intermediaries for every use that could theoretically be licensed — including uses the statute lists as fair (criticism, teaching, scholarship, research).
% ABSENT_VOICES: Future creators whose work would build on today's culture but cannot because licensing costs are prohibitive; users in jurisdictions without fair use (where this reading is exported via trade agreements); orphan works rightsholders who cannot be found to license but whose works remain locked; the public domain which shrinks as 'potential markets' expand.
% DISAPPEARANCE_RATIONALE: If the market licensing reading vanished overnight, transformative uses (parody, criticism, commentary, data mining, educational reproduction) would revert to fair use analysis balancing all four factors; licensing markets would shrink to uses that genuinely substitute for the original; rightsholder revenue would drop significantly; courts would need to develop a transformed factor-4 analysis; Congress would face pressure to codify fair use protections.
% FOUNDING_PROBLEM: The Copyright Act of 1976 codified fair use to prevent copyright's monopoly from choking the very creativity it aims to promote — enabling criticism, comment, news reporting, teaching, scholarship, and research without permission. The founding problem: rigid property rules in expressive works block the cultural dialogue that generates new expression.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the statute's text (preamble to §107), the Supreme Court in Campbell v. Acuff-Rose (1994) ('fair use protects the transformative uses that promote the Progress of Science'), and the Register of Copyrights' 2015 report acknowledging fair use's essential role in digital culture. The market licensing reading's beneficiaries (rightsholders, publishers) assert the problem is 'solved' by licensing markets — but this is self-serving. Independent corroboration comes from: the 2017-2018 Copyright Office Section 512 study (documenting fair use chilling); the 2020-2021 Copyright Office fair use index (showing factor-4 dominance); and amicus briefs from library, education, and technology coalitions in Google v. Oracle and Warhol v. Goldsmith.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.92) is near-maximum because the reading converts virtually all socially valuable downstream uses into licensable transactions — the 'potential market' construct means rightsholders need not actually offer licenses; the mere possibility of licensing forecloses fair use. Suppression (0.88) is high because the enforcement machinery (statutory damages up to $150k/work, injunctions, DMCA 1201 anti-circumvention) makes resistance prohibitively risky for most users. Theater (0.45) is moderate: courts still perform the four-factor analysis but factor 4 swallows the others; the transformative use inquiry (factor 1) has been subordinated to market harm. Accessibility collapse (0.91) reflects that alternatives (public domain, CC licensing, fair use) are structurally foreclosed for most contemporary works. Resistance (0.38) is modest: academic critique, some circuit splits (e.g., 9th Circuit's more transformative-friendly approach), and library/education advocacy exist but have not shifted the dominant doctrinal trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder/agenda_setter seat, the constraint is genuine coordination: creators deserve payment for uses of their work, licensing markets efficiently allocate rights, and fair use properly fills only true market failures. From the payer seats (educators, researchers, transformative creators), the same structure operates as enforced extraction: they pay for uses that the statute explicitly lists as fair (criticism, comment, teaching, scholarship, research) because rightsholders have constructed licensing markets for those very uses. Courts (observer) see the doctrinal performance but their factor-4 dominance makes them complicit in the extraction. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholders, licensing collectives, and publishers are structural beneficiaries (d ≈ 0.1-0.2): they collect licensing revenue, control the licensing infrastructure, and face arbitrage-grade exit (they can choose terms, forums, enforcement strategies). Downstream users, educators, researchers, and libraries are structural targets (d ≈ 0.8-0.9): they bear the licensing costs, face constrained exit (must license, forego use, or risk ruinous litigation), and have no collective bargaining power. Transformative creators are identity-locked targets (d ≈ 0.85): their creative practice is constituted through fair use, making exit from the constraint existential. Courts are analytical (d = 0.5): they neither collect nor pay but their interpretive choices determine the constraint's reach. Congress is excluded but analytical — their inaction sustains the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The fair use kernel was founded to solve the problem of rigid copyright monopolies choking cultural dialogue — enabling criticism, education, and transformative reuse without permission. That founding problem is LIVE (digital culture makes transformative reuse more essential, not less), but this reading declares it DEAD by defining 'market' to include all licensable uses. The mandatrophy is unresolved: the arrangement persists not because the founding problem is gone, but because the reading has captured the interpretive authority. The snare classification prevents mislabeling this as coordination — the coordination story (efficient licensing) is cover for extraction (monopolizing transformative uses).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the market_licensing_reading a genuine interpretation of the statutory fair use factors, or a doctrinal mutation that forecloses the kernel''s transformative purpose?',
    'Trace the doctrinal genealogy from Sony v. Universal (1984) through Campbell v. Acuff-Rose (1994) to contemporary factor-4 dominance; identify the inflection point where ''market harm'' became ''any licensable use harms the market''.',
    'If mutation, this reading is a snare using statutory text as cover for rent extraction; if genuine interpretation, the kernel itself contains a snare-reading that dominates practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading faithfully instantiates the fair use kernel or parasitizes it').

omega_variable(
    transformative_use_boundary,
    'Does transformative use that could theoretically be licensed (e.g., parody, criticism, data mining) retain fair use protection under this reading, or is it categorically foreclosed?',
    'Survey post-Campbell case law: identify cases where transformative use was denied fair use because a licensing market existed or could be constructed.',
    'If transformative use is categorically foreclosed, the reading collapses the kernel to de minimis; if transformative use survives, the reading has internal limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundary, empirical, 'Whether the reading admits any transformative fair use when licensing is possible').

omega_variable(
    orphan_works_and_market_failure,
    'How does this reading handle uses where no licensing market exists because rightsholders are unidentifiable or unwilling to license (orphan works, out-of-commerce works)?',
    'Analyze whether ''market that could exist'' includes hypothetical markets for orphan/out-of-commerce works, or whether genuine market failure creates a fair use space.',
    'If hypothetical markets count, the reading extracts from orphan works too; if only actual markets count, a residual fair use space persists for market failures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orphan_works_and_market_failure, conceptual, 'Whether ''could be licensed'' extends to hypothetical/constructed markets for unavailable works').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fair use under this reading structural (litigation risk, statutory damages, injunctions) or internalized (creators self-censor because they believe licensing is morally required)?',
    'Post-reform suppression trajectory: if statutory damages were reformed or fair use safe harbors enacted, measure whether chilling effects persist.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — creators carry the suppression with them after legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in copyright chilling effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_market_licensing_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t0, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t8, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t16, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t24, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t32, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t40, observed).
narrative_ontology:measurement(fair_use_market_licensing_tr_t48, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement_basis(fair_use_market_licensing_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(fair_use_market_licensing_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t0, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t8, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t16, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t24, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t32, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t40, observed).
narrative_ontology:measurement(fair_use_market_licensing_be_t48, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 48, 0.92).
narrative_ontology:measurement_basis(fair_use_market_licensing_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_market_licensing_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t0, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t8, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t16, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t24, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.83).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t32, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t40, observed).
narrative_ontology:measurement(fair_use_market_licensing_su_t48, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 48, 0.88).
narrative_ontology:measurement_basis(fair_use_market_licensing_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, dmca_1201_anticircumvention).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, orphan_works_mass_digitization).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, text_data_mining_licensing).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, educational_licensing_collectives).

% DUAL FORMULATION NOTE:
% This story decomposes the 'fair use' label into its market_licensing_reading. The sibling transformative_right_reading (constraint_id: fair_use_statutory_exception__transformative_right_reading) has ε ≈ 0.15 and claimed_type rope. The sibling narrow_defense_reading (constraint_id: fair_use_statutory_exception__narrow_defense_reading) has ε ≈ 0.75 and claimed_type tangled_rope. All three share the same statutory kernel but instantiate different constraints with different ε, beneficiaries, and victims. This decomposition follows the ε-invariance principle: the label 'fair use' conflates structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, organized, 0.75).
constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
