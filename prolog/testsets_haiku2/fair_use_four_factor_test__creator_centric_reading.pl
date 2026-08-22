% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the CREATOR-CENTRIC READING of the
 *   fair use four-factor test (17 U.S.C. § 107). Under this reading, fair use
 *   is framed as a narrow exception to the copyright holder's exclusive
 *   rights, and the four factors—purpose, nature, amount, and market
 *   effect—are weighted to prioritize creator incentives and prevent market
 *   substitution. The reading characterizes transformative uses (remixes,
 *   parodies, scholarship) as legitimate only when they do not compete with
 *   the copyright holder's licensing market, and treats derivative creators
 *   and public-access advocates as constrained rather than enabled by fair
 *   use. This reading does NOT claim fair use is absent or should be
 *   abolished; rather, it claims the doctrine's proper function is a narrow
 *   valve, not a broad user right. The extractiveness increases over the
 *   interval (1976–2025) as digital distribution amplified the licensing
 *   opportunities available to rights holders and courts applied
 *   creator-centric doctrine to constrain fair use in digital contexts
 *   (peer-to-peer, sampling, digital archiving).
 *
 * KEY AGENTS:
 *   - copyright_holders: institutional beneficiaries (collects licensing revenue protected by narrow fair use interpretation)
 *   - courts: institutional agenda-setter (applies four-factor test with weighting that prioritizes market harm and creator incentives)
 *   - transformative_users: powerless victims (self-censor derivative work under chilling effect of narrow fair-use doctrine)
 *   - derivative_creators: moderate-power victims (negotiate licenses or abandon projects due to constrained fair-use space)
 *   - public_domain_seekers: organized victim (unable to access works without waiting for expiration)
 *   - user_advocates: excluded seat (not represented in the beneficiary structure; their alternative reading is not this constraint)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.78).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.71).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '0a7ed9ef-06fd-4262-b790-a679164ebcfd').
narrative_ontology:cs_kernel_codification('0a7ed9ef-06fd-4262-b790-a679164ebcfd', formalized).
narrative_ontology:cs_authority_grounding('0a7ed9ef-06fd-4262-b790-a679164ebcfd', extraction).
narrative_ontology:cs_interpretation_layer_present('0a7ed9ef-06fd-4262-b790-a679164ebcfd').
narrative_ontology:cs_reading_relation('0a7ed9ef-06fd-4262-b790-a679164ebcfd', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a7ed9ef-06fd-4262-b790-a679164ebcfd', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('0a7ed9ef-06fd-4262-b790-a679164ebcfd', foundational, market_harm_primacy_factor_four).
narrative_ontology:cs_axiom_status(market_harm_primacy_factor_four, holdable).
narrative_ontology:cs_axiom_grounding('0a7ed9ef-06fd-4262-b790-a679164ebcfd', market_harm_primacy_factor_four, instrumental).
narrative_ontology:cs_axiom('0a7ed9ef-06fd-4262-b790-a679164ebcfd', foundational, creator_incentive_doctrine_necessity).
narrative_ontology:cs_axiom_status(creator_incentive_doctrine_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0a7ed9ef-06fd-4262-b790-a679164ebcfd', creator_incentive_doctrine_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('0a7ed9ef-06fd-4262-b790-a679164ebcfd', copyright_exclusive_rights_protection).
narrative_ontology:cs_drift_state('0a7ed9ef-06fd-4262-b790-a679164ebcfd', digital_distribution_era_2000_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a7ed9ef-06fd-4262-b790-a679164ebcfd', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, derivative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own exclusive reproduction and derivative rights. Under this reading, the four-factor test is engineered to narrow fair use and protect their licensing revenue. They commission legal arguments emphasizing market harm and the primacy of creator incentives. They profit from licensing agreements and benefit when transformative reuse requires permission or payment.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, national).

% Remix, parody, adapt, or annotate existing works. Under this reading, the four-factor test subordinates transformativeness and requires them to negotiate licenses or absorb litigation risk. They lack institutional resources to fight adverse fair-use rulings. Their constraint is not economic cost alone but legal chilling: they self-censor derivative work out of risk aversion even when they might have won fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    powerless, biographical, constrained, national).

% Artists, scholars, and publishers who build on existing work (fan fiction, academic commentary, cover art, remixes). They negotiate licenses when possible but face a narrowed fair-use space under this reading. The four-factor test, weighted toward creator incentives and market harm, makes it harder to argue their transformation adds sufficient new value to qualify.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Libraries, digital humanities projects, and cultural advocates argue that works should enter the public domain or that copying expired works should be unencumbered. Under this reading, fair use is constrained to narrow exceptions; they are forced to either wait for copyright expiration, seek permission, or operate in legal uncertainty. Their claim that copyright terms are too long is outside the four-factor frame.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_seekers, payer,
    organized, generational, constrained, national).

% Adjudicate fair use disputes by applying the four-factor test. Under this reading, they weight Factor 1 (nature and purpose of use) and Factor 4 (market harm) heavily toward the rights holder's benefit. They set doctrinal expectations that narrow what counts as transformative. They do not themselves extract from fair use but they administer the constraint that determines who pays.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Codified fair use in section 107 as a safe harbor for certain uses. Congress did not specify how the four factors would be weighted; courts have adopted weighting patterns that this reading characterizes as creator-centric. Congress could amend the statute to require balancing that privileges transformativeness, but copyright extension bills have repeatedly extended protection instead.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% EFF, public interest scholars, and digital rights organizations argue fair use should privilege user access and transformative reuse. They would reweight the four factors to subordinate market harm when transformation is substantial. They are excluded from the beneficiary structure of this reading but present in the contested broader dispute about copyright balance.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, user_advocates, excluded,
    organized, generational, constrained, national).

% Corporate and entertainment-industry legal teams. They operate inside the creator-centric framing and benefit from narrow fair use because their clients (copyright holders) can demand licensing. They advise caution to potential transformative users and fund test cases that push the boundaries of fair use inward.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holder_counsel, observer,
    powerful, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances exclusive copyright incentives (needed to motivate creation) against limited user access (needed to preserve cultural production that builds on existing work). The four-factor test offers a rule-based escape valve: certain uses are fair without permission, preventing absolute enclosure.
% TRANSFER_FUNCTION: Moves control of derivative and reuse rights from users and cultural builders toward copyright holders. Licensing fees and permission requirements flow from transformative users and derivative creators to rights holders. Defensive costs (litigation risk, litigation itself) flow from less-resourced users toward institutional copyright owners and their counsel.
% ABSENT_VOICES: User advocates and digital rights organizations dispute the creator-centric weighting of the four factors. International jurisdictions with broader fair-use or fair-dealing exceptions (Canada, Australia, parts of EU) represent alternative framings not present in U.S. copyright disputes. Transformative users and remix artists lack organized institutional voice in courtrooms.
% DISAPPEARANCE_RATIONALE: If the creator-centric fair use constraint were supplanted by a user-centric reading that privileged transformativeness, licensing rates would shift, litigation risk would change, and the cultural production landscape would reorganize around different incentive structures. Derivative creators and public-access advocates would shift from constrained to mobile. The copyright incentive structure itself would not disappear (exclusive rights would remain) but would rebalance.
% FOUNDING_PROBLEM: Copyright incentive: creators must be rewarded to produce; exclusive rights provide that reward. The founding problem solved by fair use (the narrow exception) is: how do we preserve copyright incentive while allowing certain uses (scholarship, criticism, news reporting) that society values and that do not directly compete with the original's market?
% FOUNDING_PROBLEM_CORROBORATION: The copyright-holder constituency and courts administering the doctrine attest the founding problem is live: strong creator incentives remain necessary, and fair use must be constrained to prevent market-substitute uses from eroding those incentives. User advocates and digital scholars attest the founding problem is obsolete in its original form: creators are incentivized by markets, prestige, and network effects; copyright extension beyond 50 years provides incentive far beyond marginal need. The empirical question—do creators actually require the incentives the current narrowed fair use protects—is attested by both parties but contested on evidence: studies cited by rights holders show licensing demands; studies cited by user advocates show markets function with weaker copyright.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The creator-centric reading extracts substantially (ε=0.78) because it allocates control of reuse and derivative rights toward copyright holders and away from users. The suppression (σ=0.71) is high because the constraint's persistence depends on active legal enforcement: courts must rule against fair-use claims, rights holders must litigate to narrow precedent, and legal risk must deter transformative users even when their fair use might be meritorious. Theater is moderate (τ=0.28) because the four-factor test is genuinely applied by courts—it is not pure performance—but a growing share of enforcement energy defends licensing exclusivity rather than legitimate policy objectives like preventing piracy. The measurement series tracks doctrine hardening: from 1976 (Sony Betamax—fair use permitted home recording) to 2025 (licensing markets for remixes, digital humanities, fan content are contractually managed; fair-use defensibility for these uses has weakened). Suppression requirement increased as digital distribution enabled fine-grained licensing and courts applied creator-centric doctrine across new media. Theater ratio rose as corporate counsel instrumentalized fair-use doctrine to shape settlements and licensing terms rather than resolving copyright conflicts on the merits.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder seat and the court seat experience this constraint very differently from the transformative-user seat. From the rights-holder perspective, the constraint is proper balancing: it preserves copyright incentive (the founding problem) while allowing narrow exceptions for socially valuable uses. From the transformative-user perspective, the same constraint is extracted licensing control: they cannot afford litigation, cannot predict fair-use outcomes, and self-censor to avoid risk. The asymmetry is not in the rule itself but in institutional capacity and exit options. A powerful publisher can litigate fair use and often win; a powerless remix artist cannot. The engine computes per-seat directionality from these asymmetries—copyright holders sit at d near 0.1 (beneficiary end), transformative users at d near 0.85 (target end)—and the computed type should diverge: rights holders perceive rope (coordination benefit + legitimate exception), users perceive snare (licensing control enforced against constrained targets).
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders benefit from the creator-centric weighting: it narrows fair use and increases licensing leverage. Their directionality (d) is low (near beneficiary end, ~0.15) because the constraint subsidizes their position—they set terms, courts enforce, users pay or self-censor. Transformative users are the constraint's targets: they bear the suppression (litigation risk, licensing costs, chilling effect on creativity). Their directionality (d) is high (near target end, ~0.82) because the constraint extracts from them without their consent and without meaningful exit options (they cannot migrate to another copyright system, cannot invent their own IP regime, are identity-locked to their creative practice). Courts sit near symmetric (d~0.5): they administer the constraint but do not directly extract; they experience it as legitimate doctrine but must absorb the burden of adjudicating disputes. Public-domain seekers and user advocates have high d (targets) because the constrained fair-use space limits their options for accessing and circulating cultural heritage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (copyright incentive for creators) was live when codified in 1976. The question of whether it remains live in 2025 is contested. Rights holders attest it is still live: digital piracy and sampling technologies require copyright protection to sustain creator income. User advocates attest it is dead: creators are incentivized by markets, prestige, patronage, and network effects; copyright extensions beyond 50 years provide incentive far exceeding marginal need, and the narrowed fair use constrains cultural production without proportional benefit to creator incentive. The creator-centric reading does not require the founding problem to be live—it could function as theatrical maintenance of a dead problem—but mandatrophy is present when extractiveness remains high while the problem it solves has atrophied. Evidence for mandatrophy: (1) copyright terms have extended repeatedly (CTEA 1998) without corresponding evidence of increased creation; (2) licensing markets have proliferated (Creative Commons, compulsory licenses) suggesting alternatives to copyright exclusivity; (3) derivative and remix creation thrives in communities that operate outside fair-use constraints (fan communities, international jurisdictions) suggesting the constraint is not necessary for the coordination function it claims. The theater-ratio increase (0.12 to 0.28) is consistent with mandatrophy: a growing share of enforcement energy maintains licensing control rather than addressing the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_attenuation,
    'Is the copyright incentive problem for individual creators still live, or has it been substantially solved by digital distribution, patronage, and network effects?',
    'Empirical studies of creator income and incentive structures; comparison of creation rates across jurisdictions with different copyright strength; analysis of open-source and collaborative creation models.',
    'If the founding problem is dead or substantially attenuated, the creator-centric narrowing of fair use is mandatrophic: it extracts from users without solving the problem it claims to serve. If the problem is live, the narrowing is legitimate doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_attenuation, empirical, 'Whether the founding copyright incentive problem persists or has been substantially solved.').

omega_variable(
    transformative_use_separation,
    'Is transformativeness structurally separable from market harm, or does transformativeness necessarily imply market competition with the original?',
    'Case-by-case analysis of transformative uses that did not compete with licensing markets (commentary, criticism, parody, remix) against uses that did. Jurisdictional comparison: countries that privilege transformativeness (Canada, parts of EU) show different fair-use outcomes; if those outcomes preserve creator incentive while expanding user access, transformativeness is separable.',
    'If separable, the creator-centric weighting that subordinates transformativeness is a choice to favor licensing revenue over cultural access, not a necessity. If inseparable, transformative uses genuinely threaten market incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_separation, conceptual, 'Whether transformative use can be disentangled from market harm in fair-use analysis.').

omega_variable(
    institutional_capture_of_doctrine,
    'Has the four-factor test been systematically reweighted by copyright-holder litigation and judicial doctrine-building, rather than evolving from textual Congress intent?',
    'Historical analysis of doctrine evolution: comparison of early fair-use cases (1976–1995, diverse outcomes) against modern doctrine (2000–2025, narrower outcomes). Funding source analysis: which parties commission litigation that shapes precedent; do rights holders disproportionately set cases that narrow fair use?',
    'If institutional capture is substantial, the creator-centric reading is not mandated by statute but authored by judicial practice favoring institutional litigants. Doctrine could be reweighted without statutory change. If evolution is organic, the narrowing reflects genuine legal principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_doctrine, empirical, 'Whether copyright-holder litigation has systematically skewed fair-use doctrine toward creator-centric interpretation.').

omega_variable(
    digital_context_shift,
    'Does fair use require different weighting in digital distribution contexts (sampling, internet remix, digital archives) than in print-era contexts (photocopying, academic excerpts)?',
    'Case law comparison: do courts apply the four factors differently to digital versus print cases? Do digital licensing markets (Spotify, YouTube''s Content ID) create market-harm findings that print licensing never did? Do jurisdictions with separate digital-copyright frameworks (EU digital single market) produce different fair-use outcomes?',
    'If digital contexts require different weighting, the creator-centric doctrine as applied to digital uses may be mistaken; fair use may be appropriately narrower for print but should be broader for digital to account for technological change. If weighting is context-independent, the narrowing is justified across media.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_context_shift, conceptual, 'Whether the four-factor test should be reweighted for digital distribution contexts.').

omega_variable(
    reading_specificity_test,
    'Is this reading (creator-centric) truly distinct from the user-centric and transformative-use readings, or is the distinction merely a matter of emphasis rather than structural principle?',
    'Structural analysis: can the three readings be distinguished by which factor is treated as primary? Can outcomes be predicted from reading assignment alone, or do case-specific facts dominate? Do the readings foreclose each other''s core claims, or do they coexist by privileging different factors?',
    'If the readings are structurally distinct (privileging different factors, producing different outcomes for the same case), they are genuine alternative frameworks. If the distinction is rhetorical emphasis only, the kernel may not be genuinely contested at the structural level—the contest may be over values, not logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_test, conceptual, 'Whether the creator-centric, user-centric, and transformative-use readings are structurally distinct or rhetorical variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(fair_tr_t2018, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(fair_tr_t2025, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(fair_be_t2018, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement(fair_be_t2025, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.42).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fair_su_t2018, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(fair_su_t2025, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension_constraint).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, sampling_permission_licensing).

% DUAL FORMULATION NOTE:
% This story and its sibling readings (transformative_use and user_centric) decompose the contested fair-use kernel into three structurally distinct constraints, each with different ε values and beneficiary/victim structures. The creator-centric reading (this story) authors high extractiveness and rights-holder benefit; the transformative-use reading authors lower extractiveness and shared coordination benefit; the user-centric reading authors low extractiveness and user-side coordination benefit. Each story is ε-invariant and independent; they are linked as readings of the same kernel (17 U.S.C. § 107) that different institutional actors and courts instantiate in their doctrine. The constraint_family_relation captures their kernel kinship without requiring them to resolve into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, powerless, 0.82).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, institutional, 0.13).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
