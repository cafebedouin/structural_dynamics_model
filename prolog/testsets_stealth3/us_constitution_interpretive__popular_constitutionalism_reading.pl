% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Interpretive Authority Through Democratic Contestation
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading - popular_constitutionalism - of the
 *   contested us_constitution_interpretive kernel. The originalist and
 *   living-constitution readings are separate constraint stories with their
 *   own epsilon, beneficiary/victim structures, and classifications; they are
 *   linked through network.affects_constraints and are not described or
 *   averaged inside this one (Rule 1, epsilon-invariance). The arrangement
 *   under contest here: constitutional meaning is shaped by popular political
 *   movements and democratic contestation, and judicial interpretation does
 *   not carry the final word. The arrangement coordinates a real collective
 *   good - it keeps the people's constituent authority over fundamental law
 *   operative and answers the counter-majoritarian legitimacy problem of
 *   judicial supremacy - while extracting, through the same structure, the
 *   security of minorities who depend on final judicial protection (and
 *   cannot win the contests that now decide their rights), the settlement
 *   certainty of long-horizon planners, and the professional finality of the
 *   judiciary and its advocates. Claim and metrics are authored
 *   independently: claimed_type tangled_rope states my structural belief
 *   (genuine coordination plus asymmetric extraction through one structure);
 *   the metric values state what I believe is descriptively true of the
 *   arrangement's operation, and the engine computes each seat's type from
 *   the structural data. Time grid: time points are years since 1789 (0 =
 *   founding-era departmentalism; 237 = 2026).
 *
 * KEY AGENTS:
 *   - popular_movements: agenda-setting beneficiary (organized/mobile) - abolition, suffrage, labor, civil-rights, gun-rights, and life-rights movements contest constitutional meaning through mobilization, amendment campaigns, elections, and coordinated noncompliance; their victories rewrite constitutional practice; exit would mean abandoning the constitutional claim that constitutes them
 *   - legislative_majorities: institutionalized beneficiary (institutional/arbitrage) - enact their constitutional visions through statute and amendment, invoke coordinate interpretive authority against judicial veto, and capture the arrangement's gains as usable authority; the seat's occupant rotates with elections but the seat captures
 *   - anti_elitist_claimants: secondary beneficiary (moderate/constrained) - citizens whose standing depends on interpretive contestation remaining open against legal-elitist closure
 *   - counter_majoritarian_dependent_minorities: primary target (powerless/trapped) - discrete minorities whose protection historically depends on final judicial authority the arrangement denies; they cannot exit their minority status or reliably win the political contests that now decide their rights
 *   - constitutional_settlement_dependents: secondary target (organized/constrained) - businesses, institutions, and planners whose long-horizon commitments price constitutional uncertainty and legal exposure
 *   - judicial_finality_advocates: payer (organized/identity_locked) - judges, scholars, and lawyers whose professional authority is constituted by finality claims the arrangement denies; abandoning the claim would dissolve their professional identity
 *   - us_supreme_court: payer (institutional/identity_locked) - the institution whose asserted supremacy the arrangement contests; it retains review power but loses the last word, and its legitimacy depends on a public acceptance the arrangement withholds as a matter of right
 *   - unmobilized_citizens: excluded seat (moderate/constrained) - the non-contesting numerical majority who live under constitutional meaning made by mobilized minorities and bear its costs without exercising its authority
 *   - constitutional_theorists: analytical observer (analytical/analytical) - scholars across camps who map the arrangement's structure from outside the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.55).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Interpretive Authority Through Democratic Contestation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'd166311e-1c5e-4422-9737-36fac4cab966').
narrative_ontology:cs_kernel_codification('d166311e-1c5e-4422-9737-36fac4cab966', fixed_text).
narrative_ontology:cs_authority_grounding('d166311e-1c5e-4422-9737-36fac4cab966', distributed).
narrative_ontology:cs_reading_relation('d166311e-1c5e-4422-9737-36fac4cab966', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d166311e-1c5e-4422-9737-36fac4cab966', us_constitution_interpretive__living_constitution_reading, influences).
narrative_ontology:cs_axiom('d166311e-1c5e-4422-9737-36fac4cab966', foundational, constitutional_authority_derives_from_popular_contestation).
narrative_ontology:cs_axiom_status(constitutional_authority_derives_from_popular_contestation, holdable).
narrative_ontology:cs_axiom_grounding('d166311e-1c5e-4422-9737-36fac4cab966', constitutional_authority_derives_from_popular_contestation, deontological).
narrative_ontology:cs_axiom('d166311e-1c5e-4422-9737-36fac4cab966', foundational, judicial_finality_lacks_democratic_warrant).
narrative_ontology:cs_axiom_status(judicial_finality_lacks_democratic_warrant, holdable).
narrative_ontology:cs_axiom_grounding('d166311e-1c5e-4422-9737-36fac4cab966', judicial_finality_lacks_democratic_warrant, deontological).
narrative_ontology:cs_axiom('d166311e-1c5e-4422-9737-36fac4cab966', secondary, coordinate_departmental_interpretation).
narrative_ontology:cs_axiom_status(coordinate_departmental_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('d166311e-1c5e-4422-9737-36fac4cab966', coordinate_departmental_interpretation, conventional).
narrative_ontology:cs_reference_frame('d166311e-1c5e-4422-9737-36fac4cab966', departmentalist_popular_sovereignty).
narrative_ontology:cs_drift_state('d166311e-1c5e-4422-9737-36fac4cab966', contemporary_post_dobbs_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d166311e-1c5e-4422-9737-36fac4cab966', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, us_supreme_court).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, coordinate_departmental_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Abolition, suffrage, labor, civil-rights, gun-rights, and life-rights movements set the agenda of constitutional politics: they contest meaning through mobilization, amendment campaigns, elections, jury politics, and coordinated noncompliance, and their victories (the Reconstruction Amendments, the Nineteenth Amendment, Brown-era implementation politics) rewrite constitutional practice. What flows to them is interpretive authority - the capacity to make constitutional meaning stick without a court's permission. Exit looks like abandoning the constitutional claim that constitutes the movement, which for any standing movement is not a real option; as a class, however, movements form, dissolve, and redirect freely.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary).

% Sitting congressional and state majorities enact their constitutional visions through statute and amendment, invoke coordinate interpretive authority against judicial veto, and institutionalize the arrangement's gains as usable governing authority. Because their tenure is electoral, they arbitrage between interpretive regimes - championing judicial restraint when courts block them and judicial enforcement when courts advance them - and the seat's occupant rotates with each election even though the seat itself persistently captures the arrangement's gains.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, arbitrage, national).

% Citizens and local organizations whose standing in constitutional politics depends on contestation remaining open against legal-elitist closure. They gain when interpretive authority is distributed and lose standing under any settlement that recenters meaning in credentialed hands. Their leverage tracks mobilization capacity, which most of them have only episodically; exit means accepting that constitutional questions are not theirs to press.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Discrete religious, racial, and ideological minorities whose protection historically depended on courts willing to stand against political majorities with final authority. Under contestation without finality, their security depends on winning political contests they are structurally positioned to lose - which is why they relied on judicial protection in the first place. They cannot exit their minority status, cannot relocate out of the arrangement's reach, and their potential coalition power is limited by the same discreteness that made them counter-majoritarian-dependent.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities, payer,
    powerless, generational, trapped, national).

% Businesses, universities, hospitals, municipalities, and institutional planners who order long-horizon commitments around settled constitutional rules. Permanent contestation raises their planning costs, compliance complexity, and legal exposure across jurisdictions whose constitutional requirements diverge as contestation devolves. They can lobby for settlement but cannot cheaply exit the instability, since their operations are fixed inside it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_dependents, payer,
    organized, biographical, constrained, national).

% Judges, justices, law professors, and practitioners whose professional authority is constituted by the claim that judicial interpretation settles constitutional meaning. The arrangement denies that claim as a matter of principle, not case-by-case. Their exit would require renouncing the rule-of-law settlement story that defines their professional identity, so they defend finality as the alternative to rule by fluctuating majorities rather than as one institutional preference among several.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    organized, generational, identity_locked, national).

% The institution whose asserted supremacy the arrangement contests. It retains the power to review and to invalidate, but loses the last word: its holdings bind only so long as the political branches and the public keep accepting them, an acceptance the arrangement treats as contingent political fact rather than settled right. The Court cannot exit its position - its authority is its interpretive role - and its institutional strategy (legitimacy maintenance, jurisdictional self-limitation as in the Dobbs devolution) is shaped by the arrangement's denial.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, us_supreme_court, payer,
    institutional, generational, identity_locked, national).

% The numerical majority of people who live under constitutional meaning made by mobilized minorities but never contest anything: they do not litigate, mobilize, or amend, and they bear the arrangement's costs (uncertainty, instability, rights volatility) without exercising its authority. Their absence from the conversation is structural - contestation selects for the mobilized - and it is the seat from which the objection that popular constitutionalism is really mobilized-minority constitutionalism would be voiced.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, unmobilized_citizens, excluded,
    moderate, biographical, constrained, national).

% Scholars across the interpretive camps - popular constitutionalists, originalists, living-constitutionalists, and their critics - who map the arrangement's structure, document its coordination function and its extraction, and testify in confirmation fights and court-curbing debates without holding a stake in which reading prevails. They see the full structure from outside it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the people's constituent authority over fundamental law operative: by denying any single institution - historically the federal judiciary - a monopoly on constitutional meaning, the arrangement lets mobilized majorities and movements settle constitutional questions through politics, giving constitutional change a peaceful political channel (movement politics, amendment, election) instead of pure compliance crisis, and answering the legitimacy problem that unelected final interpretation poses for self-government.
% TRANSFER_FUNCTION: Moves interpretive authority - and the security, settlement, and finality that flow from controlling constitutional meaning - from courts and judicially-protected parties to whichever coalition currently wins constitutional contestation (movements first, then the legislative majorities that institutionalize their wins); it moves the costs of that transfer (rights insecurity, planning uncertainty, loss of institutional finality) onto minorities, settlement-dependents, and the judiciary.
% ABSENT_VOICES: Unmobilized citizens - the numerical majority who live under meaning made by mobilized minorities but never contest it - would object that 'popular' constitutionalism is mobilized-minority constitutionalism and that they bear its costs without its authority. Also absent: future generations bound by meanings unsettled in today's contests, and minority litigants whose claims lose politically and find no final forum. They are outside the conversation the arrangement convenes because contestation selects for the mobilized.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight - judicial finality became uncontested and popular contestation lost interpretive standing - movements would re-channel claims into litigation, legislative majorities would defer to judicial pronouncements, the counter-majoritarian legitimacy problem would return unbuffered, and constitutional politics would reorganize around the courtroom. The recent Dobbs-era devolution shows the rearrangement runs the other way when the arrangement strengthens: constitutional questions returned to state politics, movements, and legislatures within a single term.
% FOUNDING_PROBLEM: The founding generation assumed coordinate departmental interpretation under popular sovereignty - Jefferson, Jackson, and Lincoln each claimed the departments were coordinate interpreters. The problem the arrangement was built to solve: how does a self-governing people remain the author of its fundamental law when interpretation is institutionalized in elite hands, and how do coordinate departments and mobilized majorities share that authority without dissolving constitutional constraint into raw politics?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Alexander Bickel, a judicial-supremacy-sympathetic scholar, named 'the counter-majoritarian difficulty' from inside the legal establishment; the Supreme Court's own opinions concede that its authority rests on public acceptance rather than finality of right; and political scientists across camps document recurring judicial legitimacy crises (court-packing debates, shadow-docket controversy, post-Dobbs compliance strain). The founding problem's persistence is attested by the arrangement's opponents, not only its beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) reflects a real, identifiable transfer: security moves from minorities who cannot win contestation, settlement certainty moves from long-horizon planners, and finality moves from the judiciary, into the hands of whichever coalition currently wins constitutional politics. Suppression (0.55) is authored as a raw structural property - the engine scales only extractiveness - and measures the force required to hold the arrangement open against the entrenched rival (judicial supremacy), not any scaling by power or scope. Theater (0.42) has risen across the interval: founding-era contestation was largely functional power struggle, while contemporary constitutional politics carries heavy performative load (confirmation hearings as theater, court-curbing proposals as positioning, symbolic originalism and public-meaning campaigns). Accessibility_collapse (0.28) is low and honest: the rival readings remain fully live and practiced daily - nothing about this arrangement collapses the alternatives, which is precisely its claim. Resistance (0.60) is substantial: finality holders and settlement-seekers actively resist. The temporal series runs on one shared seven-point grid. Base_extractiveness is non-monotonic: highest in the raw-majoritarian early republic (enslavement, Indian removal - minorities fully exposed), dipping at Reconstruction when movement-driven codification of rights was popular constitutionalism at its most protective, then rising again in the Dobbs era as settlement-dependents and minorities are re-exposed. Suppression_requirement ratchets upward for two centuries (keeping contestation alive against consolidating judicial supremacy - Ex parte McCardle jurisdiction-stripping, the Lochner-era fights, the court-packing crisis, the Cooper-era entrenchment) with a recent partial relief: the Court's own Dobbs devolution reopened constitutional politics to the states and movements, lowering the force needed to keep the arrangement alive. The pattern is a ratchet with one recent release, not a cycle - no intermittent-reinforcement mechanism is implicated. Base_properties values match the interval-end measurements.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the engine derives that divergence from the structural data. From the movement and legislative-majority seats, the arrangement is democracy working as designed - the same structure reads as coordination with modest overhead. From the counter-majoritarian-dependent-minority seat, the identical structure reads as the removal of the only protection that has ever reliably beaten a political majority - extraction with no exit. From the Court and finality-advocate seats it reads as an institutional expropriation of authority their identities are constituted by. From the unmobilized-citizen seat it reads as taxation without representation in constitutional meaning. From the analytical seat, all of these are simultaneously true of one structure - which is the tangled-rope signature, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: popular_movements (organized, mobile - they choose their contests and can redirect), legislative_majorities (institutional, arbitrage - they move between interpretive regimes as politics shifts, sitting nearest the beneficiary end), anti_elitist_claimants (moderate, constrained). Victim declarations map to high directionality, amplified by exit structure: counter_majoritarian_dependent_minorities (powerless, trapped - identity and status are not exitable, so they sit nearest the full-target end), judicial_finality_advocates and us_supreme_court (identity_locked - professional and institutional identity fused with finality, so exit is unthinkable even where formally available), constitutional_settlement_dependents (organized, constrained - they can lobby but not cheaply leave the jurisdiction's interpretive instability). No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus power and exit produces accurate d values for every seat, and the override mechanism is keyed per power atom, so any override would cross-contaminate the two moderate-power seats (anti_elitist_claimants and unmobilized_citizens), which sit on opposite sides. The unmobilized_citizens seat is authored as excluded (commentary-grade per R3) rather than forced into the victim array, and carries the derivation fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure rope would erase the extraction: the counter-majoritarian cost is not diffuse background friction but a concentrated transfer from identifiable parties (discrete minorities, planners, the judiciary) to mobilized winners - and the minorities' coalition potential is structurally limited precisely because they are discrete and insular, which is why they depended on courts in the first place. Reading it as pure snare would erase the coordination: democratic answerability of fundamental law is a genuine collective good this arrangement demonstrably delivers (Reconstruction's amendments were popular constitutionalism at its most constructive), and the extraction is a byproduct of the contestation mechanism, not its cover. On mandatrophy: the founding problem - keeping popular sovereignty real against institutionalized elite interpretation - is live, not dead; the arrangement has not outlived its function, so no mandatrophy_resolved declaration is authored, and the R5 fields record the live status with corroboration from the arrangement's own opponents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading - popular_constitutionalism - of the us_constitution_interpretive kernel; what would change structurally if a sibling reading (originalist_reading or living_constitution_reading) were the operative arrangement instead?',
    'No internal resolution: the readings coexist as live positions held by different parties. Resolution is corpus-level - comparison of the three sibling stories'' epsilon values, beneficiary/victim sets, and computed classifications shows which structural claims hold under which reading.',
    'The disagreement is located in the source and seat of interpretive authority, not in the constitutional text. If the originalist reading were operative, this constraint''s beneficiary set (movements, majorities) becomes its excluded set and judicial_finality-style settlement returns as coordination; if the living-constitution reading were operative, authority recenters on reasoned judicial adaptation and this reading''s anti-elitist extraction claim inverts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints, not parts of this one.').

omega_variable(
    mobilized_minority_vs_people,
    'Is the arrangement''s authority genuinely ''popular,'' or an elite-of-mobilization structure in which organized minorities (movements, sitting majorities) make constitutional meaning for the unmobilized majority?',
    'Measure participation and representativeness in constitutional contestation: who litigates, who mobilizes, who wins, and whether outcomes track diffuse public preferences or organized-coalition preferences across high- and low-mobilization eras.',
    'If mobilized-minority rule, extraction from the unmobilized seat is higher than authored, the coordination claim weakens toward capture by organized interests, and the arrangement drifts snare-flavored; if contestation genuinely aggregates popular will, the coordination function is strong and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilized_minority_vs_people, empirical, 'Whether the ''popular'' in popular constitutionalism names the people or the mobilized.').

omega_variable(
    minority_security_price,
    'Is the security extracted from counter-majoritarian-dependent minorities a necessary price of legitimate self-government, or an avoidable cost that departmental review (judicial review without finality) could internalize?',
    'Comparative institutional analysis of weak-form judicial review regimes (Canada s.1 notwithstanding practice, UK Human Rights Act, New Zealand): do discrete minorities fare durably worse without final judicial authority, controlling for other institutional differences?',
    'If the cost is avoidable, the extraction is contingent and the arrangement is reformable toward rope; if necessary, the extraction is structural and tangled_rope is the ceiling for this arrangement''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_security_price, empirical, 'Whether the minority-security cost is structural or contingent.').

omega_variable(
    stability_extraction_ambiguity,
    'Does the uncertainty borne by constitutional settlement-dependents count as extraction at all, or as the ordinary cost of democratic self-correction?',
    'Not resolvable by data alone: depends on how much constitutional stability a polity owes its long-horizon planners relative to the value of keeping fundamental law answerable to politics - a weighting of values, not a measurement.',
    'If ordinary cost, epsilon drops materially and the reading moves toward rope; if extraction, the tangled_rope classification holds and settlement-dependents remain a named victim seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_extraction_ambiguity, preference, 'Value question: stability owed to planners versus self-government.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 237).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usconst_popular_con_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t0, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t40, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t80, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t80, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t120, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t120, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t160, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 160, 0.33).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t160, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t200, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t200, observed).
narrative_ontology:measurement(usconst_popular_con_tr_t237, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 237, 0.42).
narrative_ontology:measurement_basis(usconst_popular_con_tr_t237, observed).

% Extraction over time
narrative_ontology:measurement(usconst_popular_con_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(usconst_popular_con_be_t0, observed).
narrative_ontology:measurement(usconst_popular_con_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(usconst_popular_con_be_t40, observed).
narrative_ontology:measurement(usconst_popular_con_be_t80, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement_basis(usconst_popular_con_be_t80, observed).
narrative_ontology:measurement(usconst_popular_con_be_t120, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(usconst_popular_con_be_t120, observed).
narrative_ontology:measurement(usconst_popular_con_be_t160, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 160, 0.44).
narrative_ontology:measurement_basis(usconst_popular_con_be_t160, observed).
narrative_ontology:measurement(usconst_popular_con_be_t200, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement_basis(usconst_popular_con_be_t200, observed).
narrative_ontology:measurement(usconst_popular_con_be_t237, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 237, 0.52).
narrative_ontology:measurement_basis(usconst_popular_con_be_t237, observed).

% Suppression requirement over time
narrative_ontology:measurement(usconst_popular_con_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(usconst_popular_con_su_t0, observed).
narrative_ontology:measurement(usconst_popular_con_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement_basis(usconst_popular_con_su_t40, observed).
narrative_ontology:measurement(usconst_popular_con_su_t80, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement_basis(usconst_popular_con_su_t80, observed).
narrative_ontology:measurement(usconst_popular_con_su_t120, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 120, 0.46).
narrative_ontology:measurement_basis(usconst_popular_con_su_t120, observed).
narrative_ontology:measurement(usconst_popular_con_su_t160, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement_basis(usconst_popular_con_su_t160, observed).
narrative_ontology:measurement(usconst_popular_con_su_t200, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement_basis(usconst_popular_con_su_t200, observed).
narrative_ontology:measurement(usconst_popular_con_su_t237, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 237, 0.55).
narrative_ontology:measurement_basis(usconst_popular_con_su_t237, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional interpretation' covers three structurally distinct claims about where interpretive authority resides, decomposed per the epsilon-invariance principle into three stories sharing the us_constitution_interpretive kernel. This (popular-constitutionalist) story authors epsilon ~0.52 over the contestation arrangement, with victims among minorities, settlement-dependents, and finality holders; the originalist story would author epsilon over a fixed-meaning arrangement with a different victim set (living-adaptation claimants, popular-sovereignty claimants); the living-constitution story over a judicial-adaptation arrangement with yet another. The upstream/downstream structure runs both ways: the popular reading supplies the legitimacy critique that pressures both siblings, while the living reading registers the societal-value changes the popular reading's movements produce (hence influences toward the living sibling, coexistence with the originalist sibling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
