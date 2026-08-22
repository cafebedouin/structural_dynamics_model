% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense - Property-Preserving Construal
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   Under the narrow-defense reading, copyright operates as property and fair
 *   use functions as an affirmative defense that the user must plead and
 *   prove, construed so that recognized exceptions do not erode the market
 *   value of the copyrighted work. The standing arrangement this story
 *   describes is that regime: default liability for unauthorized use,
 *   defendant-carried burden, market-harm analysis as the decisive factor,
 *   and commercial purpose weighing heavily against the defense. The regime
 *   performs real coordination - predictable ownership boundaries finance
 *   creative production - while simultaneously channeling licensing revenue
 *   and settlement value to catalog owners beyond what incentive preservation
 *   strictly requires, sustained by active judicial enforcement. FAMILY NOTE:
 *   this file instantiates one reading of the fair_use_statutory_exception
 *   kernel. The sibling files
 *   fair_use_statutory_exception__transformative_right_reading and
 *   fair_use_statutory_exception__market_licensing_reading instantiate
 *   different constraints over the same statutory text with different
 *   epsilon: the transformative reading authors high epsilon for a
 *   market-dominated regime that suppresses reuse, while the market-licensing
 *   reading authors epsilon near the ceiling because any potentially
 *   licensable use counts as harmed. This reading authors epsilon 0.68 -
 *   high, because most unauthorized uses bear full liability or license at
 *   the holder's rate, but bounded, because the criticism, news-reporting,
 *   and classroom cores survive even narrow construction. The stories are
 *   linked through network.affects_constraints; the contest between them
 *   lives in the omega variables, not inside this constraint.
 *
 * KEY AGENTS:
 *   - federal_courts_judiciary: Agenda setter (institutional/analytical) - allocates the burden of proof and weights the statutory factors; collects nothing
 *   - commercial_rights_holders: Primary beneficiary (institutional/arbitrage) - collects licensing revenue under default liability
 *   - collective_rights_organizations: Secondary beneficiary (organized/constrained) - intermediates and distributes license fees
 *   - unauthorized_user_defendants: Primary target (moderate/constrained) - bears litigation risk, settlements, and licensing costs
 *   - independent_creators_remixers: Target with dual position (powerless/identity_locked) - pays through blocked reuse while holding copyrights of their own
 *   - educational_institutions: Payer with protected residual (organized/constrained) - funds course-pack licensing yet shelters classroom uses
 *   - public_interest_libraries_archives: Excluded voice (organized/trapped) - argues for preservation and access outside the bilateral frame
 *   - ip_law_scholars: Analytical observer (analytical/analytical) - codes outcomes and exposes the factor-ritual gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.68).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.62).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense - Property-Preserving Construal").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'b86e3d53-9555-4825-8ab1-93afbb5a9c32').
narrative_ontology:cs_kernel_codification('b86e3d53-9555-4825-8ab1-93afbb5a9c32', fixed_text).
narrative_ontology:cs_authority_grounding('b86e3d53-9555-4825-8ab1-93afbb5a9c32', lineage).
narrative_ontology:cs_interpretation_layer_present('b86e3d53-9555-4825-8ab1-93afbb5a9c32').
narrative_ontology:cs_reading_relation('b86e3d53-9555-4825-8ab1-93afbb5a9c32', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('b86e3d53-9555-4825-8ab1-93afbb5a9c32', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('b86e3d53-9555-4825-8ab1-93afbb5a9c32', foundational, unauthorized_use_presumptively_infringing).
narrative_ontology:cs_axiom_status(unauthorized_use_presumptively_infringing, holdable).
narrative_ontology:cs_axiom_grounding('b86e3d53-9555-4825-8ab1-93afbb5a9c32', unauthorized_use_presumptively_infringing, conventional).
narrative_ontology:cs_axiom('b86e3d53-9555-4825-8ab1-93afbb5a9c32', foundational, market_value_preservation_construes_fair_use).
narrative_ontology:cs_axiom_status(market_value_preservation_construes_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('b86e3d53-9555-4825-8ab1-93afbb5a9c32', market_value_preservation_construes_fair_use, instrumental).
narrative_ontology:cs_reference_frame('b86e3d53-9555-4825-8ab1-93afbb5a9c32', property_first_narrow_exception_frame).
narrative_ontology:cs_drift_state('b86e3d53-9555-4825-8ab1-93afbb5a9c32', contemporary_post_campbell_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b86e3d53-9555-4825-8ab1-93afbb5a9c32', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, commercial_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, collective_rights_organizations).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, unauthorized_user_defendants).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_creators_remixers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, independent_creators_remixers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_incentive_theory).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide infringement actions, allocate the burden of proving fair use to defendants, and weight the four statutory factors; each opinion narrows or widens the defense for every later litigant. They collect no licensing revenue; their product is precedent.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, federal_courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Studios, major publishers, record labels, and authors' estates holding large catalogs. Because unauthorized use defaults to infringement with the defense narrowly drawn, most prospective users must come to them; they set rates, choose which defendants to sue, and monetize back catalogs across successive media formats. Exit is easy - they can reprice, repurpose, or sell catalogs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, commercial_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Licensing intermediaries such as reprographic clearinghouses and performance-rights societies whose fee schedules presuppose that unlicensed use is presumptively actionable. They aggregate and distribute license revenue; their institutional form depends on the default-liability baseline staying intact.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, collective_rights_organizations, beneficiary,
    organized, generational, constrained, national).

% Documentary filmmakers, biographers, news aggregators, and small software developers who incorporate existing works into new ones. When sued they must prove fair use at their own expense against statutory damages that dwarf their litigation budgets; most settle or license regardless of merit. Exit means abandoning the project or cutting the material.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, unauthorized_user_defendants, payer,
    moderate, biographical, constrained, national).

% Sampling musicians, video essayists, fan-fiction writers, and meme-makers whose chosen art forms consist in building on existing works. They hold copyrights of their own that the same regime protects, but their creative identity is fused with appropriation; leaving the arrangement would mean abandoning the form itself. Most operate below the litigation radar until a takedown or demand letter arrives.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_creators_remixers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, independent_creators_remixers, beneficiary).

% Universities and school systems that pay course-pack and streaming licenses under threat of suit, while retaining protected space for classroom teaching, criticism, and parody under the same doctrine. They fund the licensing stream yet also shelter the defense's surviving core.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, beneficiary).

% Libraries, archives, and digital-preservation projects that would argue for broad preservation, orphan-work, and access exceptions. They appear as amici when invited but hold no seat in the bilateral licensor-licensee frame that structures negotiation, and they cannot exit the copyright system at all.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_interest_libraries_archives, excluded,
    organized, generational, trapped, national).

% Academic commentators who code outcomes, document the gap between the four-factor ritual and market-dominant operation, and supply the critiques that all three readings of the doctrine draw on. They decide nothing and collect nothing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, ip_law_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, commercial_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a predictable default around intangible works: creators and investors can finance production knowing that wholesale copying is actionable, and the narrow defense marks a bounded, litigable edge to that exclusivity, reducing ex ante uncertainty about what ownership covers.
% TRANSFER_FUNCTION: Moves licensing fees, settlements, and statutory-damages awards from unauthorized users and would-be users to rights holders and their collective organizations, and moves litigation risk and deterrence onto defendants through the defendant-carried burden.
% ABSENT_VOICES: The downstream public - readers, viewers, and future creators who are parties to no infringement action - has no seat; libraries and archives speak only when invited as amici; and the largest absent constituency is the set of uses never attempted because the burden of proving the defense makes trial irrational, voices that by definition never reach any courtroom.
% DISAPPEARANCE_RATIONALE: If the narrow-defense construal vanished overnight - burden shifted to the party attacking the use, transformativeness made primary - licensing demand would contract to genuinely substitutive uses, documentary and scholarly reuse would proceed without clearance, catalog owners would reprice and litigate differently, and appropriative creative forms would expand; the information economy around expressive works would visibly reorganize within years.
% FOUNDING_PROBLEM: Reconcile exclusive rights sufficient to finance the creation of new works with a cultural commons that critics, teachers, and later creators can draw on - the problem Joseph Story addressed in Folsom v. Marsh and Congress codified in 17 U.S.C. 107.
% FOUNDING_PROBLEM_CORROBORATION: Historically corroborated from outside any benefiting party: the constitutional grant itself (art. I, sec. 8, cl. 8) frames limited terms for progress, and Story's 1841 opinion predates the modern licensing industries entirely. Contemporary corroboration is structurally compromised: content-industry briefs attest the problem live from inside the beneficiary set, while transformative-use scholars attest the balance has tipped - no fully disinterested attester speaks to the present status, which is itself signal.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: under default liability with the burden on the defendant, the overwhelming majority of challenged unauthorized uses end in license, settlement, or abandonment; the defense survives mainly for non-commercial commentary and classroom cores, and commercial character is close to determinative in close cases. Suppression 0.62 is authored as a raw structural property - statutory damages up to the willful ceiling, injunctive relief, notice-and-takedown regimes, and fee-shifting risk constitute the enforcement machinery; it is not scaled by scope or directionality, only extractiveness is. Theater ratio 0.35: courts perform a four-factor balancing ritual whose outcome is disproportionately determined by the market factor, so a meaningful minority of doctrinal activity is performative, but the defense does real work at its core. Accessibility collapse 0.45: alternatives do not vanish - licensing is a functioning, priced exit, which is the regime's design - so understood alternatives compress but persist. Resistance 0.55: fair use litigation, amicus campaigns, scholarly critique, and periodic legislative proposals meet the construal continuously. MEASUREMENTS run on one shared nine-point grid (1976-2026) so every tracked metric is authored at every examined time point. The base_extractiveness series is a ratchet with dips: each user-side landmark (Sony 1984, Campbell 1994, Authors Guild v. Google 2015) temporarily lowers extraction, after which licensing markets reprice and enforcement adapts, restoring the climb - the dips are adaptation cycles, not noise, and the interval-end scalar equals the final grid point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same doctrine. From the commercial_rights_holders seat the arrangement is a property system built precedent by precedent: predictable, enforceable, fairly priced - a coordination structure. From the unauthorized_user_defendants seat the identical structure operates as exposure: any use can be sued, the defense must be proven at ruinous cost, and settlement beats merit. The independent_creators_remixers seat adds identity fusion - the regime simultaneously protects their outputs and forecloses their method. The engine derives these divergent per-seat classifications from the declared roles, power atoms, and exit options; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: commercial_rights_holders collect the transfer directly and hold arbitrage-grade exit (repricing, catalog rotation), driving derived d toward the beneficiary pole; collective_rights_organizations collect pass-through fees with less exit flexibility. Targets sit near the full-target pole: unauthorized_user_defendants bear the transfer with constrained exit (the borrowed material cannot be unborrowed), and independent_creators_remixers are further locked by identity fusion with appropriative forms. educational_institutions occupy a genuinely mid-range position - they fund the licensing stream as payers yet retain protected teaching space - and that dual position is carried by their secondary_role beneficiary rather than by a directionality override: overrides are keyed to a power atom, and the only atom they share (organized) is also held by collective_rights_organizations, whose beneficiary position is unambiguous, so overriding the atom would corrupt the clearer case. federal_courts_judiciary administers without collecting; public_interest_libraries_archives are excluded rather than coordinated - their exclusion from the negotiating table is part of what keeps the bilateral frame stable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconciling exclusive rights sufficient to finance creation with a cultural commons that later creators can draw on - remains live, and the disappearance verdict is world_rearranges: burden reversal or factor re-weighting would immediately reshape licensing markets, litigation volumes, and creative practice. The R5 mismatch consumer therefore finds status=live paired with verdict=world_rearranges: no zombie flag. The tangled_rope classification earns its keep in both directions against mislabeling: reading the regime as a snare would erase the genuine incentive-coordination function that finances production; reading it as a rope would erase the burden-allocation asymmetry that channels value to catalog owners beyond incentive requirements. Fixing cost is authored prohibitive: the construal is locked by Berne/TRIPS three-step-test obligations, by reliance interests embedded in billions of dollars of licensing markets, and by the political economy of copyright legislation - the fixers (Congress, or a Court willing to go beyond Campbell's modest correction) bear concentrated, immediate costs against diffuse, deferred benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the fair_use_statutory_exception kernel; would instantiating a sibling reading change the constraint''s victim set, burden allocation, and epsilon?',
    'Not resolvable within this story: resolution consists of authoring and comparing the sibling files (transformative_right_reading, market_licensing_reading), whose differing victim sets and epsilon values ARE the answer.',
    'If the transformative reading were adopted as governing, the victim set contracts to suppressed transformers and epsilon falls sharply; if the market-licensing reading were adopted, the victim set expands to nearly all unauthorized users and epsilon approaches the ceiling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the fair use kernel this constraint instantiates and what siblings would change.').

omega_variable(
    market_harm_referent_ambiguity,
    'Does ''market harm'' under this construal mean demonstrated substitution in existing licensing markets, or foreclosed hypothetical markets the holder might someday develop?',
    'Empirical substitution studies (music-sampling and image-use econometrics) comparing licensed versus unlicensed outcomes, plus systematic coding of opinions resting on hypothetical-market reasoning.',
    'If harm requires demonstrated substitution, measured extractiveness drops materially and the regime looks closer to a priced coordination system; if hypothetical markets count, the current 0.68 stands or rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_referent_ambiguity, empirical, 'Whether the market factor measures actual or hypothetical licensing markets.').

omega_variable(
    chilling_effect_unobservability,
    'How many uses are never attempted because the burden of proving fair use makes the expected cost of trying prohibitive - uses that therefore never enter the litigation record the metrics are built from?',
    'Survey and interview studies of abandoned projects; natural experiments from jurisdictions with broader enumerated exceptions (Canadian fair-dealing categories, EU quotation and parody rights).',
    'If chilling is large, the true victim set exceeds litigated cases, observable suppression understates the regime''s force, and the accessibility_collapse figure is overstated in the direction of apparent availability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_unobservability, empirical, 'Unobserved self-censorship among would-be users under defendant-carried burden.').

omega_variable(
    doctrine_vs_reading_gap,
    'Applied doctrine after Campbell v. Acuff-Rose already incorporates transformativeness; is the standing arrangement this story describes the doctrine as actually applied, or this reading''s ideal construal of it?',
    'Systematic outcome coding of fair use decisions by circuit, use type, and commercial character across the interval; convergence toward transformative outcomes would indicate the reading describes a shrinking regime.',
    'If applied doctrine has drifted decisively toward the transformative reading, this story''s epsilon describes a legacy configuration and the cs drift magnitude should read severe rather than substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_reading_gap, conceptual, 'Whether the narrow-defense construal describes live doctrine or an aspirational restoration target.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(fair_tr_t1984, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1984, 0.22).
narrative_ontology:measurement(fair_tr_t1991, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(fair_tr_t2003, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2003, 0.34).
narrative_ontology:measurement(fair_tr_t2011, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2011, 0.36).
narrative_ontology:measurement(fair_tr_t2015, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(fair_tr_t2021, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(fair_tr_t2026, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.52).
narrative_ontology:measurement(fair_be_t1984, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1984, 0.47).
narrative_ontology:measurement(fair_be_t1991, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1991, 0.59).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.54).
narrative_ontology:measurement(fair_be_t2003, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2003, 0.63).
narrative_ontology:measurement(fair_be_t2011, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement(fair_be_t2015, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(fair_be_t2021, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(fair_be_t2026, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1984, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1984, 0.42).
narrative_ontology:measurement(fair_su_t1991, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.5).
narrative_ontology:measurement(fair_su_t2003, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(fair_su_t2011, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2011, 0.6).
narrative_ontology:measurement(fair_su_t2015, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(fair_su_t2021, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(fair_su_t2026, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% 'Fair use' as a colloquial label decomposes into three structurally distinct regimes sharing one statutory text. This file is the narrow-defense member; the transformative-right and market-licensing members carry different epsilon, different victim sets, and different burden allocations. By historical priority this reading is upstream (Folsom-lineage property frame), and the transformative reading formed against it; each family member declares its edges to the others in its own file so contamination analysis can trace, for example, a market-licensing drift raising effective extraction inside this regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
