% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense to Preserve Market Value
 *   domain: intellectual_property_law
 *
 * SUMMARY:
 *   This constraint story captures one reading of the fair use statutory
 *   exception kernel: the 'narrow defense' reading that treats fair use as an
 *   affirmative defense narrowly construed to preserve the copyright holder's
 *   market value. Under this reading, commercial nature is dispositive
 *   against fair use, transformativeness is subordinated to market harm
 *   analysis, and the defendant bears the burden of proof on all four
 *   factors. The constraint operates as a tangled rope: it coordinates the
 *   copyright system's incentive structure (genuine coordination function)
 *   while asymmetrically extracting value from users, educators, researchers,
 *   and follow-on creators who cannot afford licenses or litigation
 *   (extraction function). The coordination story — 'fair use balances
 *   copyright' — is maintained theatrically while the operational reality
 *   channels virtually all commercial and many noncommercial uses into paid
 *   licensing.
 *
 * KEY AGENTS:
 *   - major_copyright_holders: Primary beneficiary (institutional/arbitrage) — captures licensing revenue, sets enforcement agenda
 *   - collecting_societies: Secondary beneficiary (organized/constrained) — collects administrative fees on licensed uses
 *   - licensing_revenue_aggregators: Beneficiary (powerful/mobile) — monetizes permission layer
 *   - educational_institutions: Primary payer (organized/constrained) — pays escalating fees for teaching/research uses
 *   - independent_creators: Payer/beneficiary (moderate/identity_locked) — pays clearance costs, self-censors, nominally protected
 *   - researchers: Payer (moderate/constrained) — blocked from text-mining, computational analysis
 *   - libraries_archives: Payer/excluded (organized/trapped) — statutory mission overridden by narrow fair use and DMCA 1201
 *   - general_users: Payer (powerless/trapped) — faces automated takedowns, platform filters, chilling effects
 *   - courts: Observer/agenda_setter (institutional/analytical) — applies four-factor test with commerciality/market-harm weighting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.82).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense to Preserve Market Value").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'e0967342-0d3f-42b8-b906-dbecc2031958').
narrative_ontology:cs_kernel_codification('e0967342-0d3f-42b8-b906-dbecc2031958', fixed_text).
narrative_ontology:cs_authority_grounding('e0967342-0d3f-42b8-b906-dbecc2031958', lineage).
narrative_ontology:cs_interpretation_layer_present('e0967342-0d3f-42b8-b906-dbecc2031958').
narrative_ontology:cs_reading_relation('e0967342-0d3f-42b8-b906-dbecc2031958', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0967342-0d3f-42b8-b906-dbecc2031958', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('e0967342-0d3f-42b8-b906-dbecc2031958', foundational, copyright_as_property_right_absolute).
narrative_ontology:cs_axiom_status(copyright_as_property_right_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e0967342-0d3f-42b8-b906-dbecc2031958', copyright_as_property_right_absolute, deontological).
narrative_ontology:cs_axiom('e0967342-0d3f-42b8-b906-dbecc2031958', foundational, market_harm_dispositive_over_transformativeness).
narrative_ontology:cs_axiom_status(market_harm_dispositive_over_transformativeness, holdable).
narrative_ontology:cs_axiom_grounding('e0967342-0d3f-42b8-b906-dbecc2031958', market_harm_dispositive_over_transformativeness, conventional).
narrative_ontology:cs_axiom('e0967342-0d3f-42b8-b906-dbecc2031958', secondary, defendant_bears_burden_on_all_factors).
narrative_ontology:cs_axiom_status(defendant_bears_burden_on_all_factors, holdable).
narrative_ontology:cs_axiom_grounding('e0967342-0d3f-42b8-b906-dbecc2031958', defendant_bears_burden_on_all_factors, conventional).
narrative_ontology:cs_reference_frame('e0967342-0d3f-42b8-b906-dbecc2031958', statutory_fair_use_balance_1976).
narrative_ontology:cs_drift_state('e0967342-0d3f-42b8-b906-dbecc2031958', post_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0967342-0d3f-42b8-b906-dbecc2031958', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, major_copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, collecting_societies).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_revenue_aggregators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, libraries_archives).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, general_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, independent_creators).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_right).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_value_preservation_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, commercial_nature_determinative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large media corporations and publishers who hold vast copyright portfolios. They lobby for narrow fair use interpretations, initiate enforcement actions, and capture licensing revenue. Their exit is arbitrage-grade: they can shift enforcement across jurisdictions, change licensing terms, or acquire competitors. They set the agenda through trade associations (MPAA, RIAA, AAP) and direct legislative access.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, major_copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, major_copyright_holders, agenda_setter).

% Organizations (ASCAP, BMI, SESAC, CCC, etc.) that collect and distribute licensing royalties. They benefit from narrow fair use because every use channeled through licensing generates administrative fees. Their exit is constrained: they depend on the statutory framework that authorizes their operation, but they have institutional momentum and regulatory capture.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, collecting_societies, beneficiary,
    organized, biographical, constrained, national).

% Platforms and intermediaries (stock photo agencies, music licensing platforms, text licensing services) that monetize permission-based access. They benefit when fair use is narrow because more uses require paid licenses. They have mobile exit: they can pivot business models or jurisdictions, but their core revenue depends on the permission culture.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_revenue_aggregators, beneficiary,
    powerful, biographical, mobile, global).

% Universities, schools, and libraries that need to copy, excerpt, and share works for teaching and research. They pay escalating license fees, face litigation risk for classroom uses, and must maintain compliance offices. Exit is constrained: they cannot easily stop teaching or researching, and collective licensing schemes (e.g., CCC academic licenses) lock them in.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Artists, writers, musicians, filmmakers who both draw on existing culture and seek protection for their own work. They pay through clearance costs, self-censorship, and inability to afford licenses. They benefit nominally from copyright protection but are structurally positioned as payers because they lack portfolio leverage. Exit is identity-locked: their creative practice is constituted by engagement with cultural materials they cannot freely use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_creators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, independent_creators, beneficiary).

% Scholars needing to reproduce figures, text, data, and code for analysis and publication. They face paywalls, restrictive licenses, and fear of infringement claims for text-mining or computational analysis. Exit is constrained: research agendas require specific materials; alternative topics may be infeasible. Open access mandates provide partial relief but do not cover all needed materials.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, researchers, payer,
    moderate, biographical, constrained, global).

% Institutions with statutory preservation missions (Section 108) that are functionally overridden by narrow fair use and digital locks. They pay for preservation licenses, face DMCA anti-circumvention barriers, and cannot fulfill their public mission without risk. Exit is trapped: their mandate requires preserving works that are legally inaccessible; they cannot 'choose' different materials.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, libraries_archives, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, libraries_archives, excluded).

% Everyday users who share, remix, quote, and build on culture in noncommercial contexts (memes, fan works, personal archives, social media). They face automated takedowns, platform filters, and chilling effects. Exit is trapped: participation in digital culture requires using platforms that enforce narrow fair use; opting out means social and cultural exclusion.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, general_users, payer,
    powerless, immediate, trapped, global).

% Federal courts that adjudicate fair use cases. They apply the four-factor test but increasingly weight commerciality and market harm heavily, treating transformativeness as subordinate. They set precedent that narrows the defense. Their seat is analytical (they interpret) but also agenda-setting (their rulings shape the constraint's operation).
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and distribution of creative works by granting exclusive rights that enable creators and investors to recoup costs and profit, theoretically incentivizing new creation.
% TRANSFER_FUNCTION: Moves licensing revenue and control over cultural materials from users, educators, researchers, and follow-on creators to major copyright holders and their collecting intermediaries. The narrow defense reading ensures that any use with a plausible licensing market must be paid for, transferring value upstream.
% ABSENT_VOICES: Future creators whose work would build on today's locked culture; communities whose cultural practices (remix, oral tradition, sampling) are criminalized; Global South users for whom licensing fees are prohibitive; the public domain itself, which shrinks as terms extend and fair use contracts. These voices are absent because they are not recognized as legal parties in infringement actions and lack standing to challenge the constraint's scope.
% DISAPPEARANCE_RATIONALE: If the narrow defense reading vanished overnight, educational copying, research text-mining, library preservation, transformative remix, and noncommercial sharing would expand dramatically. Licensing revenue would collapse for marginal uses. Courts would need new frameworks. The entire permission-culture economy would reorganize around broader user rights.
% FOUNDING_PROBLEM: The 1976 Copyright Act codified fair use as an affirmative defense (Section 107) to balance copyright's monopoly with First Amendment and public interest needs. The narrow reading treats this balance as heavily weighted toward the copyright holder's market control, making fair use a safety valve only for uses that cannot be licensed.
% FOUNDING_PROBLEM_CORROBORATION: The narrow reading's proponents (major rightsholder trade associations) attest the founding problem (piracy, market displacement) is live and worsening. Educational, library, and technology coalitions (ARL, ALA, CCIA, EFF) plus independent economic studies (e.g., CCIA 'Fair Use in the U.S. Economy' reports) attest the founding problem is substantially solved for core markets and the constraint now primarily extracts from non-substitutive uses. The Supreme Court's Campbell v. Acuff-Rose (1994) and Google v. Oracle (2021) majorities endorse broader transformativeness, contradicting the narrow reading's commercial determinism.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the narrow reading converts virtually any use with a plausible licensing market into a paid transaction — the coordination function (balancing test) operates as a licensing requirement for all but the most clearly noncommercial, non-market-harming uses. Suppression (0.82) is very high because the constraint's persistence depends on active enforcement: litigation against transformative uses (e.g., Authors Guild v. Google, Oracle v. Georgia State), DMCA 1201 anti-circumvention blocking fair use of DRM-protected works, automated Content ID systems that disregard fair use, and the prohibitive cost of asserting the defense. Theater ratio (0.42) is moderate: the four-factor test is real doctrine and occasionally produces fair use wins (parody, some search/indexing), but a growing share of judicial and administrative activity defends the licensing regime rather than the balance. Accessibility collapse (0.85) is very high: once a user understands the narrow reading, alternatives (licensing, abstaining, risking litigation) are nearly the only paths — the defense is practically unavailable for most commercial transformative work. Resistance (0.48) is moderate: there is sustained pushback (library exceptions, open access mandates, transformative use victories in some circuits, Google v. Oracle), but it has not shifted the dominant judicial framework.
 *
 * PERSPECTIVAL GAP:
 *   From the major copyright holder seat, the constraint is genuine coordination: it protects the market that funds creation. From the independent creator seat, the same constraint is extraction: they pay to license materials their art requires while receiving minimal protection for their own work. From the library seat, the constraint is a snare: their statutory preservation mission (Section 108) is functionally nullified by the narrow fair use reading combined with DMCA 1201. The engine computes this divergence from the structural data — the declared roles, power, exit, and scope. The claimed_type (tangled_rope) reflects the author's structural assessment; the engine will compute per-seat types that may differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Major copyright holders are structural beneficiaries (d ≈ 0.1): they collect the transfer, control enforcement, have arbitrage-grade exit across jurisdictions and business models. Collecting societies and licensing aggregators are also beneficiaries (d ≈ 0.2-0.3) but with more constrained exit — they depend on the statutory framework. Educational institutions, researchers, and libraries are payers with constrained exit (d ≈ 0.7-0.8): they must use the materials, cannot easily substitute, and face escalating compliance costs. Independent creators are identity-locked payers (d ≈ 0.85): their creative practice requires engaging with culture they cannot freely use, and exit means abandoning their artistic identity. General users are trapped payers (d ≈ 0.9): participation in digital culture requires platforms that enforce the narrow reading. Courts sit near analytical (d ≈ 0.5) but their rulings set the constraint's operational parameters, making them partial agenda-setters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing copyright monopoly with public access) is contested: rightsholders say piracy and market displacement make narrow fair use necessary; users and independent studies say core markets are secure and the constraint now extracts from non-substitutive uses. The constraint persists not because the founding problem is live, but because the beneficiaries (major holders, collecting societies) have institutional power to maintain it, and the payers are fragmented, identity-locked, or trapped. This is classic mandatrophy: the arrangement's original justification is contested or dead, but the constraint persists through institutional inertia and beneficiary capture. The theater ratio (0.42) captures the performative maintenance of the 'balance' narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this narrow_defense_reading structurally diverge from its sibling readings of the fair_use_statutory_exception kernel?',
    'Compare the four-factor application across readings: this reading weights factor 1 (commercial nature) and factor 4 (market harm) as dispositive; transformative_right_reading weights factor 1 (transformativeness) as dispositive; market_licensing_reading treats factor 4 (licensing market existence) as a threshold gate. The disagreement is located in the factor-weighting hierarchy, not the statutory text.',
    'If the disagreement is in factor weighting (conceptual), the kernel''s text cannot resolve it — classification divergence persists. If the disagreement is in empirical market-harm measurement (empirical), systematic evidence could shift the dominant reading. This determines whether the constraint family stabilizes or remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement among fair use readings: factor-weighting hierarchy vs. empirical market measurement').

omega_variable(
    transformative_use_measurement,
    'Can transformativeness be measured independently of commerciality and market harm, or does this reading''s framework make transformativeness epistemically dependent on the other factors?',
    'Analyze whether courts applying this reading ever find transformative use that is also commercial and market-harming. If zero such findings exist, transformativeness is not an independent factor here — it is a rhetorical placeholder.',
    'If transformativeness is epistemically dependent, the four-factor test is a single-factor test in disguise (market harm). This would confirm the constraint as snare rather than tangled_rope, because the coordination story (balancing test) would be pure cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_measurement, empirical, 'Whether transformativeness operates as an independent factor or collapses into market-harm analysis under this reading').

omega_variable(
    burden_of_proof_asymmetry,
    'Does placing the burden of proof on the defendant for all four factors (as this reading does) structurally convert fair use from a defense into a licensing requirement for any use with a plausible market?',
    'Track win rates for fair use defendants in commercial vs. noncommercial cases under this reading''s precedent. Compare to jurisdictions or periods where burden-shifting frameworks apply.',
    'If burden asymmetry makes fair use practically unavailable for commercial transformative uses, the constraint''s extraction is near-total for that category — confirming high ε. If some commercial transformative uses still prevail, the constraint retains residual coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_asymmetry, empirical, 'Whether defendant-bearing burden of proof on all factors eliminates fair use as a practical matter for commercial uses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_narrow_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(fair_use_narrow_tr_t1988, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1988, 0.28).
narrative_ontology:measurement(fair_use_narrow_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.32).
narrative_ontology:measurement(fair_use_narrow_tr_t1998, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1998, 0.36).
narrative_ontology:measurement(fair_use_narrow_tr_t2005, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(fair_use_narrow_tr_t2015, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(fair_use_narrow_tr_t2021, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2021, 0.41).
narrative_ontology:measurement(fair_use_narrow_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_use_narrow_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(fair_use_narrow_be_t1988, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1988, 0.52).
narrative_ontology:measurement(fair_use_narrow_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(fair_use_narrow_be_t1998, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(fair_use_narrow_be_t2005, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(fair_use_narrow_be_t2015, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(fair_use_narrow_be_t2021, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(fair_use_narrow_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_narrow_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(fair_use_narrow_su_t1988, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1988, 0.6).
narrative_ontology:measurement(fair_use_narrow_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(fair_use_narrow_su_t1998, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1998, 0.72).
narrative_ontology:measurement(fair_use_narrow_su_t2005, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(fair_use_narrow_su_t2015, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(fair_use_narrow_su_t2021, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement(fair_use_narrow_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, dmca_1201_anticircumvention).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, orphan_works_barrier).

% DUAL FORMULATION NOTE:
% This constraint is one member of the fair_use_statutory_exception constraint family (kernel_id: fair_use_statutory_exception). The three readings decompose the single statutory text into structurally distinct constraints with different ε values, beneficiary/victim structures, and classification outcomes. The narrow_defense_reading has high ε (0.78) because it treats market harm as dispositive; the transformative_right_reading has lower ε because it enables transformative uses; the market_licensing_reading has ε that varies with market development. They are linked because the upstream statutory text is cited as authority by all three, and judicial precedent in one reading influences the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, organized, 0.75).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, moderate, 0.8).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
