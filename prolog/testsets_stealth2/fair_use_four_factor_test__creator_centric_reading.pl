% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test — Creator-Centric Reading (Narrow Exception Preserving Creator Incentives)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the creator-centric reading of the fair use
 *   four-factor test: fair use as a narrow exception carved out of the
 *   property right, with the four statutory factors weighed so that market
 *   effect and the preservation of creator incentives anchor the analysis.
 *   Under this reading, unlicensed use is presumptively suspect,
 *   clearance-before-use is the rational default, and the practical space for
 *   transformative work shrinks to what rights holders decline to litigate.
 *   The measurable consequences are a licensing-first culture, systematic
 *   chilling of derivative production, and a steady transfer of expressive
 *   freedom into rights-holder portfolios. KEY AGENTS (by structural
 *   relationship): copyright_rights_holders — primary beneficiary
 *   (institutional/arbitrage), collects licensing revenue and exclusion
 *   value; licensing_intermediaries — secondary beneficiary
 *   (organized/mobile), brokerage volume scales with narrowness;
 *   federal_courts — agenda setter (institutional/constrained), administers
 *   the four-factor weighing; transformative_creators — primary target
 *   (moderate/constrained), bears clearance costs, takedown risk, and project
 *   abandonment; libraries_and_archives — target with partial shelter
 *   (organized/constrained); general_public — excluded bearer of diffuse
 *   costs (powerless/trapped); ip_law_academy — analytical observer mapping
 *   doctrinal drift. This file is one reading of a contested kernel; the
 *   user-centric and transformative-use readings are separate constraint
 *   files with their own epsilon, victim sets, and classifications, linked
 *   through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - copyright_rights_holders: primary beneficiary (institutional/arbitrage) — collects licensing revenue and exclusion value; funds enforcement
 *   - licensing_intermediaries: secondary beneficiary (organized/mobile) — clearance-brokerage volume scales with the narrowness of the exception
 *   - federal_courts: agenda setter (institutional/constrained) — administers the four-factor weighing and sets the practical width of the exception
 *   - transformative_creators: primary target (moderate/constrained) — bears clearance costs, takedown risk, and abandonment of unmade work
 *   - libraries_and_archives: target with partial shelter (organized/constrained) — pays for mass-digitization licenses against finite budgets
 *   - general_public: excluded bearer of diffuse costs (powerless/trapped) — thinner derivative culture, no procedural seat
 *   - ip_law_academy: analytical observer — documents doctrinal drift and chilling effects without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.7).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test — Creator-Centric Reading (Narrow Exception Preserving Creator Incentives)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '5e813c1b-a21b-4771-97ba-2e736fb4ee1e').
narrative_ontology:cs_kernel_codification('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', formalized).
narrative_ontology:cs_authority_grounding('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', lineage).
narrative_ontology:cs_interpretation_layer_present('5e813c1b-a21b-4771-97ba-2e736fb4ee1e').
narrative_ontology:cs_reading_relation('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', fair_use_four_factor_test__user_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', fair_use_four_factor_test__transformative_use_reading, forecloses).
narrative_ontology:cs_axiom('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', foundational, fair_use_subordinate_to_property_right).
narrative_ontology:cs_axiom_status(fair_use_subordinate_to_property_right, holdable).
narrative_ontology:cs_axiom_grounding('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', fair_use_subordinate_to_property_right, conventional).
narrative_ontology:cs_axiom('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', foundational, market_harm_presumptively_unfair).
narrative_ontology:cs_axiom_status(market_harm_presumptively_unfair, holdable).
narrative_ontology:cs_axiom_grounding('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', market_harm_presumptively_unfair, empirically_contingent).
narrative_ontology:cs_reference_frame('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', incentive_preserving_property_primacy).
narrative_ontology:cs_drift_state('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', contemporary_transformativeness_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5e813c1b-a21b-4771-97ba-2e736fb4ee1e', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, incentive_theory_of_copyright).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, property_primacy_in_expression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishers, studios, labels, and estates holding exclusive rights over catalogs of expressive works. Collect licensing revenue on every permitted use and hold the option to refuse permission for uses they dislike. Fund litigation and legislative advocacy to keep unlicensed use presumptively suspect. Treat any unlicensed transformative use as encroachment on derivative-market value. Exit looks like portfolio arbitration: shifting enforcement to favorable jurisdictions, monetizing catalogs through different channels, or selling rights onward.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Collective rights organizations, stock-content agencies, and clearance houses that broker permissions between users and rights holders. Their transaction volume scales with the presumption that unlicensed use requires a license: the narrower the exception, the larger the brokerage market. They take fees on both sides of each clearance. Exit is comparatively easy — the brokering competence transfers to adjacent content verticals.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Apply the four statutory factors case by case and set precedents that determine how wide the practical space for unlicensed use is. Weigh the purpose and character of the use, the nature of the work, the amount taken, and the market effect, with market-effect reasoning anchoring the analysis under this reading. Bound by statute, precedent, and appellate hierarchy; they administer the boundary but cannot step outside the interpretive role.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Documentary filmmakers, remix artists, fan creators, critics, and essayists whose work builds on existing cultural material. Face clearance costs, takedown notices, and litigation exposure before and during production. Their alternatives are unattractive: license (often priced beyond independent budgets or simply refused), substitute originally created material (weakening the work's critical relationship to its subject), or abandon the project. They cannot make the work without the source culture, so exit means not making the work.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Institutions that acquire, preserve, digitize, and lend the cultural record. Occasionally shielded when a court accepts preservation or scholarship purposes, but under a narrow-exception reading they routinely receive demand letters and must negotiate licenses for mass-digitization and access programs against finite budgets. Exit means shrinking digitization and access programs, leaving materials locked in physical formats.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives, beneficiary).

% Audiences, readers, and future creators who inherit the cultural commons. Bear the diffuse costs: thinner critical and derivative culture, higher prices where licensing costs pass through, and works that were never made. Have no seat in the litigation and legislative processes that shape the doctrine; their interests enter only as abstractions argued by proxies. Cannot exit the copyright environment, since nearly all expressive goods pass through it.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, general_public, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, general_public, payer).

% Legal scholars and empirical researchers who map the doctrine's drift, publish critiques of incentive rhetoric, and study clearance costs and chilling effects. Hold no enforcement power; their analyses circulate to courts, Congress, and advocacy organizations and occasionally surface in opinions and hearings.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, ip_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, copyright_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the incentive-appropriation problem of expressive production: exclusive control over reproduction and distribution lets creators and rights assignees recoup creation investment, while the four-factor test polices the boundary so the property system does not choke criticism, scholarship, and commentary entirely.
% TRANSFER_FUNCTION: Moves control-value and licensing revenue from anyone making unauthorized use of expressive works to rights holders and their brokers; moves expressive freedom — the ability to quote, remix, adapt, and build without permission — from the public and downstream creators into the rights holder's portfolio.
% ABSENT_VOICES: The general public and the never-initiated transformative users: people who would make derivative works but abandon them at the clearance stage never appear in any proceeding, and audiences have no procedural seat at all. Their interests are voiced only secondhand, by counsel for defendants who happen to be litigating and by advocacy organizations.
% DISAPPEARANCE_RATIONALE: If the narrow-exception arrangement vanished overnight — fair use becoming a broad user right — back-catalog and archive licensing revenue models would collapse, takedown enforcement would lose its legal footing, derivative and critical production would expand rapidly, and rights-holder portfolios would lose most of their exclusion value. The licensing intermediary sector would contract sharply.
% FOUNDING_PROBLEM: How to give authors enough control over copies to recoup the cost of creation — the scarcity-era problem of the printing press — without granting a perpetual monopoly that starves the learning, criticism, and follow-on creation a functioning culture requires. The Statute of Anne and the constitutional copyright clause encode this bargain; the four-factor test is its modern administrative form.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders attest the problem is live, citing digital piracy and collapsed revenue windows. Outside the benefiting parties, economic historians of copyright document that the scarcity rationale has weakened for most works in an abundance era; the Supreme Court has repeatedly stated copyright is a means to public ends rather than an entitlement (Fox Film v. Doyal; the Eldred majority's limited-times framing); and consumer-technology advocacy organizations attest the arrangement now functions substantially as portfolio rent collection. Corroboration exists on both sides, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72) because the arrangement decouples compensation from marginal cost: rights holders collect on uses whose social value is often greatest precisely when they refuse or price out permission, and the deterrent shadow extends to uses never attempted. Suppression (0.70) is structural rather than internalized: takedown machinery, litigation exposure, and clearance pricing close off the unlicensed route, while licensing and original creation remain as costly substitutes — hence accessibility_collapse sits mid-range (0.45) rather than mountain-high, since workable alternatives persist at a price. Resistance (0.55) is real and organized: fair-use advocacy, defensive litigation by libraries and documentarians, and academic critique continuously contest the narrowing. Theater_ratio (0.32) reflects a growing share of incentive-preservation rhetoric detached from demonstrated incentive sensitivity — invoked to defend catalog rents the incentive theory does not obviously cover. The temporal series run on one shared grid (t=0,10,20,30,40,50 across all three metrics) so no metric borrows another's endpoints. The suppression_requirement series is authored deliberately: the story specifically tracks enforcement-capacity build-out (litigation-only enforcement, then statutory takedown regimes around t≈22, then automated filtering at scale from t≈35 onward), which is an enforcement-infrastructure trajectory, not merely shifting extraction. Claim and metrics are independent authored facts: claimed_type tangled_rope rests on the structural triad (genuine incentive-coordination core, asymmetric extraction with named victims, active enforcement), not on tuning to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the rights-holder seat the arrangement is earned protection of sunk creative investment, and every unlicensed use is a taking. From the transformative-creator seat the same structure operates as prior restraint by liability: the dispute arrives before the work does, in the form of a clearance invoice or a takedown notice. From the bench the test presents as neutral multi-factor balancing, though the market-effect factor's gravitational pull is seat-dependent. The general public experiences none of the machinery directly — only its outputs in the culture available to it. The engine derives these divergent per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: rights holders collect the transfer directly and hold arbitrage-grade exit (portfolio and jurisdiction mobility), intermediaries collect brokerage fees with mobile exit. Targets sit near the full-target end: transformative creators bear the transfer with constrained exit (the source culture cannot be substituted away), libraries bear it with organizational budget limits, and the public bears it diffusely with no exit at all — trapped inside a copyright environment covering effectively all expressive goods. The courts, as agenda setters, are positioned near symmetric administration: they neither collect nor pay, but their precedents move everyone else's d. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and by the national-to-global scope that makes verification of actual market harm harder and amplifies effective extraction modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — provisioning creation incentives under copy scarcity — is contested rather than dead: it remains arguable for investment-heavy production (film, software, database journalism) and largely obsolete for the long tail of twentieth-century catalogs whose incentive window closed decades ago. The classification resists two symmetrical errors. Reading the arrangement as pure snare would erase the real coordination core: exclusive exploitation rights demonstrably finance some production that would otherwise not occur, and the four-factor valve genuinely shelters criticism and scholarship at the margins. Reading it as pure rope would erase the asymmetric extraction: the same structure that finances production also converts the public's expressive freedom into portfolio value, with named victims and active enforcement holding the boundary. The arrangement is not a piton — enforcement is real and intensifying, not theatrical residue — so mandatrophy is not resolved; the contested-status finding routes through the founding_problem_status x disappearance_verdict mismatch check instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the creator_centric_reading of kernel fair_use_four_factor_test; what structural differences would instantiating user_centric_reading or transformative_use_reading instead produce, and where exactly is the disagreement located?',
    'Generate and classify the sibling files (fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading) and compare computed types, victim sets, and epsilon across the family.',
    'The user-centric reading relocates the burdened seat: low epsilon on access restriction with rights holders'' exclusion expectations as the constrained party. The transformative-use reading yields intermediate epsilon with market harm subordinated for meaning-adding uses. This file''s classification holds only for the creator-centric seat; cross-reading comparison is the point of the family, not a defect in this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the fair-use kernel; siblings are separate constraints with divergent victim sets and epsilon.').

omega_variable(
    incentive_sensitivity_empirics,
    'Is creative production actually responsive to the breadth of unlicensed use this reading forbids — does narrowing fair use measurably increase creation investment?',
    'Cross-jurisdiction natural experiments comparing creation output and investment around exception-widening reforms (Canadian fair-dealing expansions, UK quotation and parody exceptions) against matched unchanged jurisdictions.',
    'If incentives are insensitive to exception breadth, the coordination half of the tangled rope atrophies and the arrangement trends toward snare — the incentive story becomes cover for portfolio rent. If sensitive, part of the measured extraction is the genuine price of the incentive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_sensitivity_empirics, empirical, 'Whether the incentive-preservation rationale survives empirical test, determining how much of the extraction is coordination cost versus rent.').

omega_variable(
    chilling_effect_counterfactual,
    'How much transformative creation is never attempted because anticipated liability deters it before any enforceable dispute arises?',
    'Clearance-cost and project-abandonment studies — documentary oral histories, platform removal-rate audits, and surveys of abandoned derivative projects — triangulating invisible non-use.',
    'A larger hidden chilling effect raises effective suppression above the measured scalar and strengthens the victim-side reading of the declared victim set; a small one supports the reading''s claim that the exception, though narrow, captures most socially valuable use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_counterfactual, empirical, 'Size of the unobservable deterrence margin — uses prevented before they could be contested.').

omega_variable(
    authority_grounding_framing,
    'Is judicial authority over the kernel grounded in lineage (continuity with the statutory text and doctrinal transmission from Folsom forward) or in practice (the common-law method itself as the self-justifying standard)?',
    'Examine whether courts treat section 107''s text and factor structure as constraining the analysis (lineage framing) or treat evolving fair-use practice as its own warrant (practice framing).',
    'Under the practice framing, the drift_state reads as ordinary common-law evolution rather than axiom overriding, weakening the substantial-magnitude drift finding and changing the computed terminal attractor for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination in the commitment-system classification: lineage versus practice as the authority''s ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_creator_centric_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t0, observed).
narrative_ontology:measurement(fair_use_creator_centric_tr_t10, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t10, observed).
narrative_ontology:measurement(fair_use_creator_centric_tr_t20, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t20, observed).
narrative_ontology:measurement(fair_use_creator_centric_tr_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t30, observed).
narrative_ontology:measurement(fair_use_creator_centric_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t40, observed).
narrative_ontology:measurement(fair_use_creator_centric_tr_t50, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(fair_use_creator_centric_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(fair_use_creator_centric_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t0, observed).
narrative_ontology:measurement(fair_use_creator_centric_be_t10, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t10, observed).
narrative_ontology:measurement(fair_use_creator_centric_be_t20, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t20, observed).
narrative_ontology:measurement(fair_use_creator_centric_be_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t30, observed).
narrative_ontology:measurement(fair_use_creator_centric_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t40, observed).
narrative_ontology:measurement(fair_use_creator_centric_be_t50, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(fair_use_creator_centric_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_creator_centric_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t0, observed).
narrative_ontology:measurement(fair_use_creator_centric_su_t10, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t10, observed).
narrative_ontology:measurement(fair_use_creator_centric_su_t20, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t20, observed).
narrative_ontology:measurement(fair_use_creator_centric_su_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t30, observed).
narrative_ontology:measurement(fair_use_creator_centric_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t40, observed).
narrative_ontology:measurement(fair_use_creator_centric_su_t50, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(fair_use_creator_centric_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the fair use four-factor test' decomposes into three reading-constraints of one kernel, each with its own stable epsilon per the epsilon-invariance principle. This creator-centric file is upstream in genealogy: its incentive rationale and market-harm primacy shaped the doctrinal terrain on which the transformative-use reading developed, and its enforcement posture (litigation, then takedown regimes) sets the operating environment in which the user-centric reading's access claims are contested. The siblings are linked back to this file; no member of the family stands alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
