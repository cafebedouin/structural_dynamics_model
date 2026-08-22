% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Market Licensing Reading — Market Harm Preclusion Doctrine
 *   domain: intellectual_property/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   The market_licensing_reading of the fair use statutory exception (17
 *   U.S.C. §107) holds that any use for which a licensing market exists or
 *   could exist causes cognizable market harm to the rightsholder, thereby
 *   precluding fair use. This reading emerged from Texaco v. American
 *   Geophysical Union (1994) and was reinforced in subsequent circuit
 *   decisions, reaching its apotheosis in the 'fourth factor dominance' era
 *   where market harm became a de facto dispositive factor. The reading
 *   structurally benefits rightsholder collectives (RIAA, MPAA, AAP,
 *   ASCAP/BMI) and licensing intermediaries (CCC, Harry Fox, SoundExchange)
 *   by converting fair use from a flexible exception into a null set wherever
 *   licensing infrastructure exists. Educational institutions, documentary
 *   filmmakers, researchers, and independent creators bear the extraction —
 *   they must license or abstain. The constraint is claimed as a tangled_rope
 *   (coordination of licensing markets + extraction from users), but its ε
 *   trajectory shows steady accumulation toward snare territory. Theater
 *   ratio remains low — the coordination function (efficient licensing) is
 *   real but increasingly vestigial relative to the extraction.
 *
 * KEY AGENTS:
 *   - rightsholder_collectives: Primary beneficiary (institutional/arbitrage) — collect licensing revenue, shape market definition through litigation
 *   - licensing_intermediaries: Primary beneficiary (organized/arbitrage) — operate clearance infrastructure, take transaction cuts
 *   - incumbent_platforms: Secondary beneficiary (institutional/arbitrage) — leverage content ID systems to monetize user uploads via licensing
 *   - educational_institutions: Primary victim (organized/constrained) — face escalating license fees for course materials, distance education
 *   - documentary_filmmakers: Primary victim (moderate/trapped) — clearance costs often exceed production budgets; insurance requires licensing
 *   - academic_researchers: Primary victim (moderate/constrained) — text/data mining, reproduction for analysis blocked by licensing demands
 *   - independent_creators: Primary victim (powerless/trapped) — cannot afford clearance; fair use uncertainty chills creation
 *   - libraries_archives: Primary victim (organized/constrained) — preservation, digitization, access blocked by orphan works and licensing gaps
 *   - courts: Agenda setter (institutional/analytical) — interpret and apply the market harm test; circuit splits create forum shopping
 *   - congress: Observer (institutional/analytical) — statutory authority; periodic reform attempts (orphan works, §108) stall
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.87).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.78).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Market Licensing Reading — Market Harm Preclusion Doctrine").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '3bcff20c-7410-40d7-9ed6-3b9b739a55b4').
narrative_ontology:cs_kernel_codification('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', fixed_text).
narrative_ontology:cs_authority_grounding('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', lineage).
narrative_ontology:cs_interpretation_layer_present('3bcff20c-7410-40d7-9ed6-3b9b739a55b4').
narrative_ontology:cs_reading_relation('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', foundational, licensing_market_harm_is_cognizable_harm).
narrative_ontology:cs_axiom_status(licensing_market_harm_is_cognizable_harm, holdable).
narrative_ontology:cs_axiom_grounding('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', licensing_market_harm_is_cognizable_harm, instrumental).
narrative_ontology:cs_axiom('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', foundational, potential_licensing_revenue_is_property_right).
narrative_ontology:cs_axiom_status(potential_licensing_revenue_is_property_right, holdable).
narrative_ontology:cs_axiom_grounding('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', potential_licensing_revenue_is_property_right, conventional).
narrative_ontology:cs_axiom('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', secondary, fourth_factor_is_dominant_in_fair_use_analysis).
narrative_ontology:cs_axiom_status(fourth_factor_is_dominant_in_fair_use_analysis, holdable).
narrative_ontology:cs_axiom_grounding('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', fourth_factor_is_dominant_in_fair_use_analysis, conventional).
narrative_ontology:cs_reference_frame('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', statutory_fair_use_balance_1976).
narrative_ontology:cs_drift_state('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', post_google_oracle_2021, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3bcff20c-7410-40d7-9ed6-3b9b739a55b4', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rightsholder_collectives).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, incumbent_platforms).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, academic_researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, independent_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, libraries_archives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, incumbent_platforms).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_preclusion_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, licensing_market_primacy).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, copyright_as_property_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect licensing revenue from educational, documentary, and commercial uses through blanket licenses and per-use fees. Shape market definition through strategic litigation (Texaco, Princeton University Press, Georgia State). Can shift enforcement to new markets (AI training, text mining) without losing existing revenue streams. Their extraction is the constraint's primary transfer function.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rightsholder_collectives, beneficiary,
    institutional, generational, arbitrage, global).

% Operate clearance infrastructure (Copyright Clearance Center, Harry Fox Agency, SoundExchange, music publishers). Take transaction fees (10-20%) on every license. Their business model depends on the market-harm test treating all potential licensing as cognizable harm — if fair use expands, their volume drops. They lobby for broader market definitions and against orphan works exceptions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries, beneficiary,
    organized, generational, arbitrage, global).

% YouTube, Meta, TikTok use Content ID to monetize user uploads via licensing deals with rightsholder collectives. They benefit from the market-harm test because it forces licensing rather than fair use disputes. But they also pay — they fund the enforcement infrastructure and share revenue. Their exit is arbitrage: they can adjust revenue splits, shift to user-generated content, or lobby for safe harbors.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, incumbent_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, incumbent_platforms, payer).

% Pay escalating fees for course packs, electronic reserves, distance education licenses, and streaming media. The Georgia State case (2011-2020) established that even nonprofit educational use requires licensing when a market exists. They can negotiate blanket licenses (constrained exit) but cannot avoid the system — accreditation and student demand require providing materials. Their resistance takes the form of fair use guidelines, library advocacy, and legislative lobbying.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Clearance costs for archival footage, music, and incidental capture often exceed total production budgets. Errors & omissions insurance requires licensing all recognizable content. The 'market' for 30-second clips from 1970s news broadcasts is a monopoly rent — no substitute exists, rightsholders know this. Fair use defenses are theoretically available but practically foreclosed by insurance and distributor requirements. Their exit is trapped: stop making documentaries or accept the extraction.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, trapped, global).

% Text and data mining, reproduction for computational analysis, and sharing of research materials are blocked by licensing demands. Publishers claim TDM markets exist (via API licenses) even where no functional market operates. Researchers at well-funded institutions can sometimes negotiate (constrained exit); independent researchers and global south scholars are effectively trapped. Their resistance is open access mandates, fair use best practices codes, and Sci-Hub-style shadow libraries.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, academic_researchers, payer,
    moderate, biographical, constrained, global).

% Cannot afford clearance for samples, quotes, or incidental inclusion. Fair use uncertainty chills creation — the cost of a single lawsuit exceeds lifetime earnings. Platform Content ID systems demonetize or block work automatically. Their exit is trapped: create within the licensed ecosystem (surrendering creative control) or remain invisible. They have no collective bargaining power and no litigation capacity.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, independent_creators, payer,
    powerless, biographical, trapped, global).

% Preservation, digitization, and access for orphan works and fragile materials blocked by market-harm test. Section 108 exceptions are narrower than the market-harm reading allows. They pay for digitization rights they cannot always identify owners for. Their exit is constrained: they can advocate for legislative reform (orphan works bills have failed repeatedly since 2006) and assert fair use via best practices codes, but the structural pressure is toward licensing or non-access.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, libraries_archives, payer,
    organized, generational, constrained, national).

% Interpret and apply the four-factor test. Circuit splits (2nd Circuit market-harm dominance vs. 9th Circuit transformative focus) create forum shopping and legal uncertainty. Supreme Court interventions (Campbell 1994, Google v. Oracle 2021) shift the boundary but do not resolve the structural tension. Their decisions determine whether the constraint operates as tangled_rope (some fair use survives) or snare (fair use collapses).
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Statutory authority for §107 and copyright law generally. Periodic reform attempts (orphan works legislation 2006, 2008, 2015; Music Modernization Act 2018; CASE Act 2020) address symptoms but not the market-harm test's structural expansion. Rightsholder collective lobbying dominates the legislative process. Congressional hearings document the constraint's effects but produce no structural reform.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, congress, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Licensing markets solve transaction cost problems for copyright clearance: they aggregate rights, standardize terms, and enable efficient payment flow from users to rightsholders. In theory, this coordinates access to copyrighted works without requiring individual negotiation for each use.
% TRANSFER_FUNCTION: Moves licensing fees from educational institutions, documentary filmmakers, researchers, independent creators, and libraries to rightsholder collectives and licensing intermediaries (who take transaction cuts), with incumbent platforms capturing a share via content ID monetization. The transfer is justified as compensation for market harm but operates on theoretical markets, not demonstrated substitution.
% ABSENT_VOICES: Orphan work rightsholders (unidentifiable, deceased, or defunct entities) — their 'market interests' are asserted by collectives but they cannot object. Global South creators and users — the market-harm test is calibrated to US/EU licensing infrastructure; their uses are invisible in the market definition. Future creators — chilled by current clearance culture, they never enter the conversation. The public domain — works that should enter it are kept in licensing markets through term extension and market-harm expansion.
% DISAPPEARANCE_RATIONALE: If the market-harm preclusion doctrine vanished overnight, educational and documentary uses would shift to fair use analysis within weeks. Licensing revenue for rightsholder collectives would drop 40-60% (estimated from uses currently licensed only due to market-harm risk). Content ID systems would lose their legal predicate for mandatory licensing. The mobile software economy of cultural production would reorganize around transformative use and negotiated licensing for genuinely substitutive uses only.
% FOUNDING_PROBLEM: The 1976 Copyright Act's §107 codified fair use to balance creator incentive with public access, education, and cultural production — preventing copyright from becoming a censorship tool. The market-harm factor (fourth factor) was meant to prevent commercial substitution, not to monetize all potential licensing.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder collectives (RIAA, MPAA, AAP) attest the problem is live: digital piracy, AI training, and global enforcement gaps require strong market-harm protection. Educational institutions (ARL, AAUP), libraries (ALA, IFLA), documentary filmmakers (IDA), and independent creators (Authors Alliance, Creative Commons) attest the problem is dead: licensing markets now over-serve, fair use is hollow, and the constraint persists as rent extraction. Independent economic studies (Lemley, Lessig, Samuelson, and Copyright Office reports) corroborate the shifted-function reading — the market-harm test has expanded beyond its coordinating function.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.87) reflects near-total capture of value from uses that could theoretically be licensed — the reading treats any potential licensing revenue as a property right. Suppression (0.78) reflects active enforcement: content ID systems, statutory damages, injunctions, and the chilling effect of litigation risk. Theater ratio (0.22) is low because the licensing coordination function is real but increasingly marginal — most licensing revenue comes from incumbent works, not new coordination. Accessibility collapse (0.71) is high: once a licensing market is recognized, alternatives (fair use, public domain, orphan works exceptions) collapse structurally. Resistance (0.64) is substantial: circuit splits (2nd vs 9th), academic criticism, legislative reform attempts, and platform resistance (YouTube Content ID disputes) show the constraint is contested. The extraction trajectory (0.55→0.87 over 34 years) shows steady rent accumulation — the coordination function has not grown proportionally.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder_collectives seat (institutional/arbitrage), this is genuine coordination: licensing markets solve transaction cost problems, ensure creator compensation, and fund future creation. From the educational_institutions seat (organized/constrained), the same structure is extraction: they pay escalating fees for uses that were historically fair, with no quality improvement in the licensed product. From the documentary_filmmakers seat (moderate/trapped), the constraint is a snare — clearance costs exceed budgets, insurance mandates licensing, and the 'market' for archival footage is a monopoly rent. The engine computes these divergences from power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder collectives and licensing intermediaries are structural beneficiaries (d ≈ 0.1-0.2): they collect the transfer, control market definition through litigation, and have arbitrage-grade exit (can shift enforcement to new markets). Incumbent platforms are secondary beneficiaries (d ≈ 0.25): they monetize via content ID but face regulatory pressure. Educational institutions, libraries, and researchers are organized but constrained payers (d ≈ 0.7-0.8): they must comply but can sometimes negotiate blanket licenses. Documentary filmmakers and independent creators are trapped/moderate payers (d ≈ 0.85-0.95): no practical exit, litigation risk is existential. Courts are institutional agenda-setters (d ≈ 0.5 symmetric) but their interpretation choices determine the constraint's scope. Congress is analytical observer (d ≈ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1976 Act, §107): balance creator incentive with public access through flexible fair use. The market_licensing_reading claims this problem is still live (contested status) — but the corroboration is split. Rightsholder collectives attest the problem is live (piracy, AI training). Educational and library communities attest the problem is dead (licensing markets now over-serve, fair use is hollow). The constraint persists because rightsholder collectives extract enough to fund enforcement, while victims are too diffuse to coordinate reform. This is not mandatrophy in the pure sense (the coordination function of licensing is real) but a captured coordination mechanism — the mandate has expanded beyond its function. The reading's persistence depends on suppressing the transformative_right_reading's market definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the market_licensing_reading a legitimate interpretation of the fair_use_statutory_exception kernel, or a constructed constraint that benefits identifiable agents?',
    'Comparative analysis of sibling readings'' structural profiles and their empirical consequences across jurisdictions; legislative history of §107 and subsequent case law trajectory.',
    'If constructed, the constraint is a false summit candidate — its ''natural law'' framing of market harm as inevitable is a cover story for extraction by rightsholder collectives and licensing intermediaries. Would trigger FSM reclassification to tangled_rope with higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a genuine kernel interpretation or a false summit benefiting rightsholder collectives.').

omega_variable(
    licensing_market_definition_ambiguity,
    'What counts as a ''market that could be licensed'' — any theoretical licensing scheme, or only functioning markets with actual demand?',
    'Circuit-split analysis on market definition (e.g., Texaco v. American Geophysical Union vs. Campbell v. Acuff-Rose; Google LLC v. Oracle America); empirical study of licensing revenue for uses later deemed fair.',
    'Broad definition collapses fair use to de minimis (ε → 0.95+); narrow definition preserves transformative and non-commercial space (ε → 0.4-0.6). The boundary determines whether the constraint is a snare (near-total extraction) or a tangled_rope (coordination + extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(licensing_market_definition_ambiguity, conceptual, 'Structural ambiguity in the market-harm test''s referent — drives ε variance across circuits.').

omega_variable(
    transformative_use_boundary,
    'Does transformative use create a new market that the original rightsholder is entitled to capture, or does transformation by definition fall outside the licensing market?',
    'Supreme Court guidance on Campbell''s transformative test vs. market-harm test integration; economic analysis of whether transformative works substitute for or complement originals.',
    'If transformative works generate capturable markets, fair use collapses entirely for commercial transformation. If transformation escapes market-harm analysis, a structural safe harbor exists within the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundary, empirical, 'Whether the reading''s internal logic forecloses transformative use or leaves a contested boundary.').

omega_variable(
    orphan_works_and_market_failure,
    'How does the reading handle uses where no licensing market exists because rightsholders cannot be identified or transaction costs exceed value?',
    'Analysis of orphan works legislation attempts; Copyright Office studies on market failure; empirical data on licensing clearance costs for archival/documentary uses.',
    'If market failure is treated as ''no market exists'' (fair use permitted), the reading has a structural escape valve. If market failure is treated as ''market could exist'' (fair use denied), the reading becomes a snare for archival/educational uses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orphan_works_and_market_failure, empirical, 'Whether the reading''s market definition includes theoretical markets that fail in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1994, 0.16).
narrative_ontology:measurement(fair_tr_t1998, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1998, 0.17).
narrative_ontology:measurement(fair_tr_t2002, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(fair_tr_t2006, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(fair_tr_t2014, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2014, 0.21).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(fair_tr_t2022, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1994, 0.62).
narrative_ontology:measurement(fair_be_t1998, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(fair_be_t2002, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2002, 0.73).
narrative_ontology:measurement(fair_be_t2006, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2006, 0.78).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement(fair_be_t2014, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(fair_be_t2022, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2022, 0.87).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1994, 0.52).
narrative_ontology:measurement(fair_su_t1998, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(fair_su_t2002, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2002, 0.63).
narrative_ontology:measurement(fair_su_t2006, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(fair_su_t2014, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2014, 0.74).
narrative_ontology:measurement(fair_su_t2018, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2018, 0.76).
narrative_ontology:measurement(fair_su_t2022, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, dmca_anticircumvention).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, orphan_works_legislative_failure).

% DUAL FORMULATION NOTE:
% This reading and transformative_right_reading are ε-invariant decompositions of the fair_use_statutory_exception kernel. Market_licensing_reading ε ≈ 0.87 (high extraction, licensing market primacy). Transformative_right_reading ε ≈ 0.35 (coordination of cultural production, transformative safe harbor). They share the same statutory text but instantiate different constraints with different beneficiary/victim structures. Narrow_defense_reading is a third decomposition (ε ≈ 0.65, formalist, pre-economic analysis). The three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, organized, 0.75).
constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, moderate, 0.85).
constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
