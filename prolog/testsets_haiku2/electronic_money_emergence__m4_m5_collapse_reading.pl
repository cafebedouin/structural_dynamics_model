% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Classification as Retroactive Electronic Money Category
 *   domain: economic/monetary/epistemic
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'electronic_money_emergence.' The kernel asks: when and how did
 *   electronic money become a category? Three readings offer competing
 *   answers: (1) the became_thinkable reading argues it emerged when the
 *   concept became technically and socially thinkable; (2) the first_held
 *   reading argues it emerged when the first institutional actor held
 *   dematerialized currency in a form distinguishable from physical notes;
 *   (3) THIS reading — the m4_m5_collapse reading — argues that no genuine
 *   emergence event occurred; instead, the M4/M5 statistical distinction
 *   RETROACTIVELY CREATED the category of electronic money as a post-hoc
 *   definitional accommodation to financial innovation, making 'emergence' a
 *   measurement artifact rather than an event. Central banks expanded the M4
 *   aggregate to include money market funds, short-term securities, and other
 *   near-monies starting in the 1980s without formally announcing that this
 *   definitional shift was constituting 'electronic money' as a new category.
 *   The constraint is generated from THIS reading alone; the sibling readings
 *   are other constraint files linked via the network.
 *
 * KEY AGENTS:
 *   - central_bank_statistical_authorities: set and maintain the M4/M5 distinction; gain institutional legitimacy from being positioned as measuring objective monetary phenomena
 *   - monetary_economics_guild: use the distinction as an established analytical category; career incentives reward treating the distinction as discovered rather than constructed
 *   - policy_makers: operate under the assumption that M4 and M5 measure distinct real phenomena; bear the cost of policy misdiagnosis when the distinction collapses
 *   - alternative_monetary_theorists: excluded from orthodox publication and policy discourse; suppressed from developing rival categorizations
 *   - financial_technologists: created the systems being retroactively classified; excluded from the definitional process
 *   - historical_reconstructors: document the artifact status; face gatekeeping and suppression from benefiting institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.62).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.71).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.43).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Classification as Retroactive Electronic Money Category").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic/monetary/epistemic").

domain_priors:requires_active_enforcement(electronic_money_emergence__m4_m5_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '20c6975f-243b-4214-8353-32f6441b021a').
narrative_ontology:cs_kernel_codification('20c6975f-243b-4214-8353-32f6441b021a', formalized).
narrative_ontology:cs_authority_grounding('20c6975f-243b-4214-8353-32f6441b021a', extraction).
narrative_ontology:cs_interpretation_layer_present('20c6975f-243b-4214-8353-32f6441b021a').
narrative_ontology:cs_reading_relation('20c6975f-243b-4214-8353-32f6441b021a', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('20c6975f-243b-4214-8353-32f6441b021a', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('20c6975f-243b-4214-8353-32f6441b021a', foundational, electronic_money_is_definitional_artifact).
narrative_ontology:cs_axiom_status(electronic_money_is_definitional_artifact, holdable).
narrative_ontology:cs_axiom_grounding('20c6975f-243b-4214-8353-32f6441b021a', electronic_money_is_definitional_artifact, empirically_contingent).
narrative_ontology:cs_axiom('20c6975f-243b-4214-8353-32f6441b021a', foundational, monetary_category_boundaries_are_chosen_not_discovered).
narrative_ontology:cs_axiom_status(monetary_category_boundaries_are_chosen_not_discovered, overridden).
narrative_ontology:cs_axiom_grounding('20c6975f-243b-4214-8353-32f6441b021a', monetary_category_boundaries_are_chosen_not_discovered, deontological).
narrative_ontology:cs_reference_frame('20c6975f-243b-4214-8353-32f6441b021a', monetary_measurement_coherence).
narrative_ontology:cs_drift_state('20c6975f-243b-4214-8353-32f6441b021a', contemporary_cryptocurrency_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('20c6975f-243b-4214-8353-32f6441b021a', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_authorities).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_economics_guild).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, policy_makers_operating_without_categorical_clarity).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the M4/M5 distinction in national and international monetary statistics. Expanded M4 to include money market funds and short-term securities in response to financial innovation circa 1980–2000 without formally announcing that the definition retroactively constituted 'electronic money' as a category. Continue defending the distinction as capturing real monetary phenomena while resisting decomposition into component flows. Collects institutional legitimacy and policy influence from being positioned as the authoritative voice on monetary aggregates.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Uses the M4/M5 distinction as an established analytical category in peer-reviewed research, teaching, and policy briefs. The distinction provides a stable classification that appears to be discovered rather than constructed, enabling career publication and institutional prestige around 'electronic money' as a unified object of study. Would face reputational and methodological pressure if forced to acknowledge the distinction is retrospectively imposed rather than descriptively grounded.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_economics_guild, beneficiary,
    institutional, generational, analytical, global).

% Central banks and finance ministries operate under the assumption that M4 and M5 measure distinct monetary phenomena with distinct economic effects. Design policy (interest rates, reserve requirements, quantitative easing targeting) around the presumed categorical stability and realness of the distinction. Bear the cost of misdiagnosis when policy assumes a real distinction that is actually definitional artifact — control becomes ineffective, unintended consequences propagate, blame accrues to policy error rather than categorical confusion.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, policy_makers_operating_without_categorical_clarity, payer,
    organized, biographical, constrained, national).

% Propose rival monetary theories (Modern Monetary Theory, Bitcoin-adjacent frameworks, credit-based accounts) that challenge the adequacy of the M4/M5 apparatus. Cannot publish in orthodox venues without accepting the distinction as foundational. Face institutional suppression of alternative categorization schemes — funding is scarce, peer review is hostile, and the dominant statistics authorities refuse to produce data in forms their theories would require.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_theorists, payer,
    moderate, biographical, constrained, global).

% Created the systems that blurred the boundary between cash, bank deposits, and securities in the 1990s–2010s (payment networks, digital wallets, repo markets, stablecoins). Would argue that the M4/M5 distinction was always post-hoc rationalization of their innovations and should be reopened as technologies evolve. Structurally excluded from the monetary statistics conversation — their data and frameworks are not considered inputs to the definition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_technologists, excluded,
    powerful, biographical, trapped, global).

% Examine archival records of how the M4 definition came to include near-monies and securities. Can document that the boundary was moved to encompass new financial instruments without any discovery claim (there was no empirical moment when economists suddenly found evidence that M4's new constituents were 'really' money — the definition shifted to accommodate institutional change). Their scholarship is suppressed by gatekeepers who benefit from treating the definitions as natural.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, historical_reconstructors, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_authorities).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Providing a single unified statistical aggregation that summarizes monetary conditions across multiple institutions and instruments, enabling comparative analysis of money supply across time and jurisdictions.
% TRANSFER_FUNCTION: Moves definitional authority from scattered financial innovators and practitioners to the centralized statistical authority (central banks); practitioners' informal categories are supplanted by the official M4/M5 framework, and alternative categorizations are rendered illegitimate or invisible in policy discourse.
% ABSENT_VOICES: Financial technologists who built the systems being classified retroactively are excluded from the definition process. Heterodox monetary theorists whose frameworks depend on different categorical boundaries are locked out of orthodox publication venues. Historical reconstruction scholars who document the artifact status of the distinction face suppression.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished and central banks were forced to specify what monetary phenomena they were targeting, policy would fragment into multiple competing frameworks — Modern Monetary Theory, credit-based accounting, blockchain-native categories, and empirical flow analysis would all become viable again. The institutional unified field would collapse and be replaced by transparent methodological pluralism.
% FOUNDING_PROBLEM: As financial markets diversified and dematerialized in the 1980s–1990s, monetary aggregates computed on traditional definitions (physical cash + bank deposits) became increasingly misaligned with how money functioned in actual transactions. The statistical authorities needed a way to track what 'felt like' money in modern economies without theoretically justifying why money market funds and repo markets had become money.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is acknowledged as SOLVED by the monetary statistics authorities themselves — they expanded the definition to include near-monies and count them as 'electronic money' (M4/M5). The problem is no longer live; what persists is institutional inertia and the performative appearance of categorical coherence. Historical reconstruction scholars and heterodox theorists outside the benefiting guild attest that the problem was never about discovering what money IS, but about managing definitional convenience as financial systems evolved.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 terminal, rising from 0.35 at 1980) measures the constraint's capacity to move definitional authority from dispersed financial practitioners to centralized statistical gatekeepers, suppressing alternative frameworks. The rise through 1990–2008 reflects the period when the M4/M5 distinction became institutionalized as THE way to think about electronic money, followed by stabilization (2008–2024) as the apparatus became entrenched despite growing incompleteness (cryptocurrencies, stablecoins, CBDC proposals now exist outside the M4/M5 frame). Suppression (0.71 terminal) is high because the constraint's persistence depends on actively defending the distinction against competing categorizations and preventing alternative frameworks from gaining institutional standing. Theater_ratio (0.68 terminal) is the key diagnostic of Piton: the constraint's primary function (coordinating measurement across time and jurisdictions) is genuine but increasingly dwarfed by its secondary function (defending a categorical boundary that no longer corresponds to empirical monetary realities). As financial innovation accelerated post-2008, the ratio suggests that more enforcement energy goes to maintaining the appearance of coherence than to actually measuring what the distinction purports to measure. The shared time grid allows measurement of the accumulation pattern: extractiveness and suppression both rise together through 1990–2008 (the constraint's entrenchment phase), then plateau (2008–2024) as the constraint becomes theatrical — it is no longer growing stronger, but is maintained inertially despite mounting pressure from cryptocurrency, CBDC, and alternative-theory constituencies.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (central bank statistical authorities) and the target seats (alternative theorists, financial technologists, policy makers bearing misdiagnosis costs) compute dramatically different types from the same structural data. From the authority seat, this constraint is a Rope — a successful coordination mechanism that solved the genuine problem of tracking money in dematerializing financial systems. From the target seats, it is a Snare — a gatekeeping apparatus that suppresses competing frameworks and forces policy makers into misdiagnosis. The engine computes this divergence from the stakeholder roles and exit options: authorities have arbitrage-grade exit (they can change the definition at will with institutional consequences but no structural barrier), while alternative theorists and technologists have constrained or trapped exit (they cannot change the definition from within the system and face institutional penalties for operating outside it). This asymmetry should produce different computed types per seat, which is the measurement the system exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical authorities are the structural beneficiary (d near 0.0 — they collect institutional legitimacy and policy influence from their gatekeeping position and bear no cost if the distinction is arbitrary). The monetary economics guild is secondary beneficiary (d ~0.15 — they benefit from a stable category that appears discovered, but face reputational risk if the artifact status is exposed). Policy makers are positioned as symmetric (d ~0.45) — they receive genuine coordination benefit from having a unified monetary aggregate to target, but pay the cost of operating under false categorical clarity. Alternative theorists and financial technologists are targets (d near 1.0 — they are suppressed from developing frameworks and excluded from resource allocation). The directionality derivation from the beneficiary/victim structure is not overridden here; the roles are straightforward and the power atoms (institutional vs. moderate) lock the directionality in place.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the canonical Piton signature: (1) the founding problem (coordinating monetary measurement as financial systems dematerialized) is DEAD — the problem was solved by 1990s, when the statistical authorities simply expanded the definition to encompass new instruments. (2) The constraint persists inertially beyond the solution (2000–2024) without benefiting any actor enough to maintain it actively — central banks maintain it more through theatrical compliance than through genuine monetary control. (3) Theater_ratio rises throughout the interval (0.42 → 0.68), indicating that performative defense of the distinction is increasingly the constraint's only function. (4) No actor is substantially harmed by its persistence (unlike Snare, where victims bear concentrated costs) — the costs are diffuse (policy misdiagnosis, suppression of alternatives) and are borne by actors whose institutional power is insufficient to change the arrangement. The constraint is not a failure of Rope (coordination broken) or a pure Snare (extraction concentrated); it is a Piton whose primary function has atrophied but which persists through institutional inertia, with suppression increasingly dedicated to defending the appearance of coherence rather than actual coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_artifact_vs_ontological_emergence,
    'Is electronic money a category that was discovered through statistical analysis, or was it retroactively imposed by a definitional shift to encompass financial innovations for which no consistent theoretical justification exists?',
    'Archival and interview-based reconstruction of the decision process that expanded M4: did the authorities discover evidence that near-monies were ''truly'' money by some independent standard, or did they pragmatically expand the definition to track financial behavior while avoiding the admission that ''money'' is underdefined?',
    'If discovered: the constraint is a Rope that successfully coordinates monetary measurement around an objective monetary boundary. If retroactively imposed: the constraint is a Piton whose extraction lies in suppressing the fact that ''electronic money'' is a classificatory artifact maintained for institutional convenience rather than empirical coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_artifact_vs_ontological_emergence, empirical, 'Whether the M4/M5 distinction tracks underlying monetary phenomena or is a post-hoc definitional accommodation.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the became_thinkable and first_held readings offer genuinely alternative accounts of electronic money emergence, or does accepting this reading (the M4/M5 collapse reading) logically foreclose both siblings within a single theoretical framework?',
    'Formalization of the three readings'' core premises into explicit propositions and examination of their logical compatibility. Can a single coherent account of monetary history hold all three, or does the M4/M5 collapse reading''s claim (emergence is definitional artifact, not event) directly contradict the became_thinkable and first_held readings'' implicit claims (there exists an empirical emergence event).',
    'If the readings coexist: they represent different legitimate framings that different constituencies hold simultaneously (the constraint family pattern is genuine pluralism). If the M4/M5 collapse reading forecloses the others: the kernel contest resolves into one reading invalidating the others, and the constraint family becomes a detection mechanism for false summits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical compatibility of the three readings of electronic money emergence.').

omega_variable(
    institutional_path_dependence,
    'How much of the suppression and extraction measured in this constraint is due to institutional actors'' genuine belief in the categorical coherence of M4/M5, versus conscious performative maintenance of an incoherent distinction because institutional power depends on it?',
    'Qualitative interviews with monetary statisticians, central bank policy directors, and heterodox economists about the cognitive status of the distinction — do they treat it as discovered fact or pragmatic convention? Comparison with jurisdictions that use alternative monetary frameworks (e.g., Modern Monetary Theory in some policy circles) to examine whether belief in M4/M5 is structurally necessary or contingent.',
    'If genuine belief: the suppression is unintentional downstream effect of shared confusion. If performative maintenance: the extraction is deliberate gatekeeping and the constraint is more purely extractive than Piton (closer to Snare with diffuse victims). The distinction affects remedial strategy — does one educate about the artifact, or challenge institutional power directly?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_path_dependence, empirical, 'Cognitive vs. performative maintenance of the M4/M5 distinction.').

omega_variable(
    alternative_category_suppression,
    'How much of the accessibility collapse and resistance suppression is attributable to active exclusion of alternative monetary categorizations (MMT, credit-flow accounting, blockchain-native frames) from orthodox venues versus passive default to the established M4/M5 apparatus?',
    'Analysis of peer review and publication patterns in monetary economics journals: do editors and reviewers reject alternative frameworks explicitly because they violate the M4/M5 apparatus, or do they reject them on other grounds (empirical inadequacy, incoherence)? Do alternative frameworks receive funding or institutional support? Can practitioners operate outside the M4/M5 frame without professional penalty?',
    'If active exclusion: the constraint''s suppression is a real exercise of institutional power to enforce a definition. If passive default: the suppression is the gravitational effect of an established standard, and the constraint is less purely extractive. The distinction affects whether remediation requires structural institutional change or merely lowering barriers to entry for alternative frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_category_suppression, empirical, 'Active gatekeeping vs. passive institutional inertia in the suppression of alternative monetary categorizations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement_basis(elec_tr_t1980, projected).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.64).
narrative_ontology:measurement_basis(elec_tr_t2000, observed).
narrative_ontology:measurement(elec_tr_t2008, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2008, 0.69).
narrative_ontology:measurement_basis(elec_tr_t2008, observed).
narrative_ontology:measurement(elec_tr_t2015, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2015, 0.68).
narrative_ontology:measurement_basis(elec_tr_t2015, observed).
narrative_ontology:measurement(elec_tr_t2024, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2024, 0.68).
narrative_ontology:measurement_basis(elec_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(elec_be_t1980, projected).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement_basis(elec_be_t1990, observed).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(elec_be_t2000, observed).
narrative_ontology:measurement(elec_be_t2008, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement_basis(elec_be_t2008, observed).
narrative_ontology:measurement(elec_be_t2015, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(elec_be_t2015, observed).
narrative_ontology:measurement(elec_be_t2024, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(elec_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement_basis(elec_su_t1980, projected).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement_basis(elec_su_t1990, observed).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(elec_su_t2000, observed).
narrative_ontology:measurement(elec_su_t2008, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2008, 0.71).
narrative_ontology:measurement_basis(elec_su_t2008, observed).
narrative_ontology:measurement(elec_su_t2015, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(elec_su_t2015, observed).
narrative_ontology:measurement(elec_su_t2024, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(elec_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.12).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the electronic_money_emergence kernel family decomposed per ε-invariance (DP-001): three readings, three distinct ε values. The became_thinkable reading measures emergence as conceptual possibility (low ε, discovery frame). The first_held reading measures emergence as institutional instantiation (moderate ε, realization frame). THIS reading measures emergence as retroactive categorization (moderate-high ε, artifact frame). The three readings share a kernel ('when did electronic money emerge') but produce different constraints because their ε referents differ: what is the 'emergence' being measured? The m4_m5_collapse reading argues that no emergence occurred — the category was created retroactively by statistical authority. This reading influences both siblings by suggesting that any 'emergence event' story (became_thinkable, first_held) rests on the unexamined assumption that 'electronic money' is a natural category, when in fact it was retroactively imposed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
