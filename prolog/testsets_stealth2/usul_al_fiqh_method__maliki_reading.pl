% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Source Hierarchy: Medinan Practice, Unrestricted Public Interest, Integrated Custom
 *   domain: religious/legal-theoretic/comparative-law
 *
 * SUMMARY:
 *   A single colloquial label — 'the Maliki method' — covers a standing
 *   evidentiary constitution for Islamic law in the Muslim west: continuous
 *   Medinan communal practice counts as independent evidence alongside
 *   transmitted reports; public interest unanchored in any text (maslaha
 *   mursala) is a valid source; local custom ('urf) is integrated wherever it
 *   does not contradict the texts. This file instantiates the MALIKI READING
 *   of the contested kernel usul_al_fiqh_method; the hanafi, shafii and
 *   hanbali readings are separate constraints with their own epsilon values,
 *   beneficiary structures and classifications, linked through
 *   network.affects_constraints. The decomposition follows the
 *   epsilon-invariance principle: measuring 'the source hierarchy' through
 *   the authentication gate (shafii) versus through practice-continuity
 *   (maliki) yields different extraction profiles because they are different
 *   arrangements, not one arrangement viewed twice. Under this reading the
 *   arrangement solves a real coordination problem — convergent adjudication
 *   across a custom-diverse empire without a central magisterium — while
 *   extracting asymmetrically: reports conflicting with established practice
 *   lose procedural force, rival methodologies are shut out of western courts
 *   and curricula, and customary minorities whose usage differs from the
 *   codified standard bear rulings they had no hand in shaping. KEY AGENTS
 *   (by structural relationship): - madinan_juristic_establishment:
 *   agenda-setting beneficiary (institutional/identity_locked) — sets the
 *   evidentiary standard its own practice satisfies - maliki_school_jurists:
 *   primary beneficiary (organized/identity_locked) — careers and endowments
 *   ride on the method - north_african_andalusian_communities:
 *   dual-positioned (moderate/constrained) — custom integrated where
 *   compliant, overridden where not - traditionist_hadith_scholars: primary
 *   target (organized/constrained) — reports devalued against practice -
 *   rival_madhhab_textualists: secondary target (organized/constrained) —
 *   excluded from western courts - customary_minority_groups: diffuse target
 *   (powerless/trapped) — usage loses to the codified standard -
 *   muslim_rulers_of_the_west: enforcer seeking legitimacy
 *   (powerful/arbitrage) - academic_historians_of_islamic_law: analytical
 *   observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.5).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Source Hierarchy: Medinan Practice, Unrestricted Public Interest, Integrated Custom").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious/legal-theoretic/comparative-law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'e6e17823-b5e6-42a0-a536-70f473103674').
narrative_ontology:cs_kernel_codification('e6e17823-b5e6-42a0-a536-70f473103674', formalized).
narrative_ontology:cs_authority_grounding('e6e17823-b5e6-42a0-a536-70f473103674', lineage).
narrative_ontology:cs_interpretation_layer_present('e6e17823-b5e6-42a0-a536-70f473103674').
narrative_ontology:cs_reading_relation('e6e17823-b5e6-42a0-a536-70f473103674', usul_al_fiqh_method__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('e6e17823-b5e6-42a0-a536-70f473103674', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('e6e17823-b5e6-42a0-a536-70f473103674', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_axiom('e6e17823-b5e6-42a0-a536-70f473103674', foundational, medinan_practice_independent_evidence).
narrative_ontology:cs_axiom_status(medinan_practice_independent_evidence, holdable).
narrative_ontology:cs_axiom_grounding('e6e17823-b5e6-42a0-a536-70f473103674', medinan_practice_independent_evidence, empirically_contingent).
narrative_ontology:cs_axiom('e6e17823-b5e6-42a0-a536-70f473103674', foundational, maslaha_mursala_unrestricted_validity).
narrative_ontology:cs_axiom_status(maslaha_mursala_unrestricted_validity, holdable).
narrative_ontology:cs_axiom_grounding('e6e17823-b5e6-42a0-a536-70f473103674', maslaha_mursala_unrestricted_validity, instrumental).
narrative_ontology:cs_axiom('e6e17823-b5e6-42a0-a536-70f473103674', secondary, urf_integrated_where_not_contradicting_text).
narrative_ontology:cs_axiom_status(urf_integrated_where_not_contradicting_text, holdable).
narrative_ontology:cs_axiom_grounding('e6e17823-b5e6-42a0-a536-70f473103674', urf_integrated_where_not_contradicting_text, conventional).
narrative_ontology:cs_reference_frame('e6e17823-b5e6-42a0-a536-70f473103674', medinan_continuous_practice_standard).
narrative_ontology:cs_drift_state('e6e17823-b5e6-42a0-a536-70f473103674', post_shafii_systematization, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e6e17823-b5e6-42a0-a536-70f473103674', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, madinan_juristic_establishment).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_school_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, north_african_andalusian_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, traditionist_hadith_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, rival_madhhab_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, customary_minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, muslim_rulers_of_the_west).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, north_african_andalusian_communities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_transmission_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_integration_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The scholars and judges of Medina whose continuously transmitted communal practice became the benchmark for deriving divine law. They set which evidence counts: when an isolated report conflicts with established local practice, practice prevails as abrogating, particularizing, or outweighing the report. Their authority rests on the claim of unbroken inheritance from the Prophet's own community; abandoning that claim would dissolve the basis of their own standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, madinan_juristic_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).

% Jurists trained in the Maliki method across Qayrawan, Fez, Cordoba and the wider Muslim west. They staff the courts, teach in the mosque-colleges, receive stipends and endowment income, and answer legal questions using the school's source hierarchy. Their credentials, posts and learned standing are constituted by the method itself; defecting to a rival method would forfeit position and reputation together.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_school_jurists, beneficiary,
    organized, generational, identity_locked, continental).

% Muslim populations of the Maghrib and al-Andalus living under law derived by this method. Their marriage, commercial and inheritance customs are absorbed into binding law wherever they do not clash with established practice, so the law largely tracks how they actually live. Where their local usage differs from the Medinan benchmarks, the benchmark prevails and their usage yields; they cannot select a different source hierarchy for their courts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, north_african_andalusian_communities, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, north_african_andalusian_communities, payer).

% Specialists in collecting, grading and transmitting reports of the Prophet's words and deeds. Under this arrangement a report that conflicts with established Medinan practice can be set aside, so the procedural value of their transmissions falls inside Maliki jurisdictions however carefully graded. Many relocated to eastern centers where report-criticism set the research agenda; within the western lands their objections carried little procedural weight.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, traditionist_hadith_scholars, payer,
    organized, biographical, constrained, continental).

% Jurists of other schools — Shafi'is, Hanbalis, Zahiri literalists — whose methods require authenticated reports before derivation and deny independent evidentiary weight to communal practice. In Maliki-governed territories they are passed over for judicial appointments, their manuals go untaught in the endowed colleges, and their rulings find no traction in the courts. Some built careers in eastern cities; the western judiciary stayed closed to them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, rival_madhhab_textualists, payer,
    organized, biographical, constrained, continental).

% Communities — rural tribesmen, recent converts, merchant diasporas — whose usage differs from the dominant urban custom the jurists codified. When their practice reaches a judge it is measured against the integrated standard and typically loses; they lack the scholarly representation that would get their usage written into the manuals, and relocating to another jurisdiction means leaving kin, land and livelihood behind.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, customary_minority_groups, payer,
    powerless, generational, trapped, regional).

% Dynasts from the Umayyads of Cordoba through the Almoravids, Almohads and Marinids who appoint the judges, endow the colleges, and episodically persecute or protect particular doctrines. Alignment with the established school buys legitimacy with the urban religious classes; enforcing its exclusivity costs them flexibility, and several attempted to rein the establishment in when it obstructed fiscal or military policy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, muslim_rulers_of_the_west, agenda_setter,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, muslim_rulers_of_the_west, beneficiary).

% Modern scholars comparing the four source-hierarchy regimes from outside any confessional commitment. They reconstruct how practice, report and reason were weighted, test the transmission claims against the documentary record, and publish findings that neither feed nor starve the arrangement.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, academic_historians_of_islamic_law, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_school_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a shared decision procedure for deriving divine law across the Muslim west: which evidence counts (continuous communal practice, transmitted reports, analogical extension, public interest, custom) and in what order of precedence, so that judges from Qayrawan to Cordoba converge on rulings without a central magisterium, and so that law remains administrable across a custom-diverse empire.
% TRANSFER_FUNCTION: Moves adjudicative authority and endowment income toward the Madinan scholarly lineages and their western heirs; moves evidentiary value away from isolated reports that conflict with established practice and away from rival schools' procedures; moves the customary norms of integrated urban populations into binding law.
% ABSENT_VOICES: Traditionist report-specialists and Zahiri literalists objected in polemic — al-Shafi'i denied that communal practice is evidence at all, and Ibn Hazm dismissed the school's extra-textual sources as fabrication — but held no seat in the western courts, curricula or endowment boards. Non-integrated customary minorities had no forum at all: no jurist represented their usage until it had already lost.
% DISAPPEARANCE_RATIONALE: If the source hierarchy vanished overnight, the western judiciary would fragment: judges would fall back on raw report-criticism or local preference, thousands of settled rulings resting on practice-and-interest derivations would lose their warrant, the endowed colleges' curricula would collapse, and the ruler-scholar legitimacy bargains built on school alignment would renegotiate from zero.
% FOUNDING_PROBLEM: After the Prophet's death the young community faced legal questions the surviving revelations did not answer verbatim, and reports of his practice circulated unevenly and often in isolation. Medina — where he had lived, judged and legislated for a decade — possessed a continuous communal practice that seemed to preserve his precedent better than any single transmitted report; later, governing a vast empire of diverse customs required sources beyond the texts.
% FOUNDING_PROBLEM_CORROBORATION: Rival-school jurists corroborate the founding problem while contesting the solution: al-Shafi'i accepted the need for a disciplined source hierarchy but denied that communal practice is independent evidence, and Ibn Hazm rejected the extra-textual sources outright. Modern academic historians from Goldziher and Schacht onward attest the underlying problem of textual silence and imperial diversity while disputing the unbroken-transmission premise on documentary grounds. Corroboration inside the school is unanimous — which is precisely why the outside seats are named here.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.50) is moderate and accumulates over the interval: at t0 the hierarchy mainly coordinates (0.28), and extraction layers on as school and state fuse — appointment control, curriculum monopoly, and the discounting of adverse reports harden into institutional rent. Suppression (0.58) is the raw structural force keeping alternatives down inside the western lands: judicial appointment, endowed-college curricula, and episodic persecution of rival doctrines; it is unscaled by power or scope in the engine's arithmetic. Theater (0.30) is low-to-moderate: the derivational function runs daily and really decides cases, but ritualized defense of madhhab standing grows as the school ages. Accessibility collapse (0.40): alternatives remain visible and operable in other territories, so understanding the hierarchy does not close the option set the way a natural limit would. Resistance (0.55): the arrangement met sustained organized opposition for centuries — al-Shafi'i's direct attack on 'amal ahl al-Madina, Ibn Hazm's Zahiri polemic, intra-school fights over the bounds of maslaha — and survived by adaptation rather than silence. Claim and metrics are independent: I claim tangled_rope because the coordination function is primary and genuine while the extraction is real, asymmetric and actively enforced; the engine computes per-seat types from the structural data and may disagree, and that divergence is the measurement. Identity-lock: the jurists' exit is identity_locked in the professional sense — credentials, posts and learned standing are constituted by the method, so defection costs the self, not just the salary; breaking that fusion would shift the payer-side arithmetic faster than any enforcement change. The suppression series oscillates with exogenous political shocks — Fatimid persecution of Maliki scholars (t≈975) and Almohad reformism (t≈1125) degrade the enforcement machinery from outside, while Almoravid rigor (t≈1050) and the Marinid restoration (t≈1200) rebuild it — a shock-and-repair cycle, not intermittent reinforcement; the scalar suppression reflects the interval-end state. All three series share one time grid so no metric row is silently backfilled.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the Madinan establishment and the Maliki juristic class, the hierarchy is what makes law possible: a shared procedure, a living link to the founding generation, a way to absorb custom so law stays practicable — extraction is nearly invisible from inside because its costs fall on people who were never seated. From the traditionist report-specialists, the same structure is a devaluation machine: transmissions graded with immense care are set aside whenever they conflict with practice, and no appeal exists because the tribunal's own standard defines what counts as evidence. Rival-school textualists experience exclusion from appointments and curricula; customary minorities experience the codified standard as an alien benchmark their usage must match or lose. The engine computes this divergence from power, exit and declared position; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation. The Madinan establishment (agenda_setter, institutional, identity_locked) sits at the beneficiary extreme — the standard is its own practice. Maliki jurists (beneficiary, organized, identity_locked) sit just off the extreme, collecting careers and endowments while bearing maintenance costs. The western communities carry a dual declaration (beneficiary with secondary payer) because custom-integration subsidizes them where they comply while the Medinan benchmark overrides them where they do not; derivation from the beneficiary role alone would park them too near the subsidy end, so a directionality override sets the moderate-power seat to d=0.30. Traditionist report-specialists and rival-school textualists (payers, organized, constrained) sit near the target end: the enforcement machinery exists substantially to discount what they produce. Customary minorities (payer, powerless, trapped) sit nearest the full-target end — no exit, no representation. Rulers (agenda_setter with secondary beneficiary, powerful, arbitrage) sit mid-range: they pay enforcement costs and collect legitimacy. Academic historians (observer, analytical) stand outside the flow entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deriving divine law where the texts fall silent and reports conflict — is contested rather than dead: the general problem is permanently live in any text-bound legal system, while the specific Maliki wager (that Medinan continuity certifies the solution) has been disputed from outside since al-Shafi'i. Because the problem is not dead, the arrangement is not a piton: its derivational function runs daily, its theater ratio stays well below half, and its administrators could not abandon it without dissolving their own authority. Because the coordination function is primary, it is not a snare despite a named capturer: the extraction rides on a structure that would still coordinate if the asymmetries were flattened. The mandatrophy risk here runs in the less common direction — mistaking a live, load-bearing coordination structure for mere rent because its beneficiaries are identifiable. The six-questions mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie flag, correctly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel usul_al_fiqh_method (reading: maliki_reading). Which structural facts would change if a sibling reading were adopted instead?',
    'Side-by-side comparison of the four reading files: beneficiary/victim inversion (textualists gain standing under shafii and hanbali while practice-bearers lose), epsilon relocation (extraction migrates from report-discounting to authentication-gate control), and recomputation of the foreclosure edges.',
    'Under the shafii_reading the current victims become beneficiaries and vice versa; this file''s classification would not change, but the family-level extraction profile shifts from practice-privilege rent to authentication-control rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the source-hierarchy kernel this story instantiates and what sibling adoption would alter.').

omega_variable(
    medinan_transmission_historicity,
    'Is the premise that Medinan communal practice transmits the Prophet''s precedent unbroken historically accurate, or was it constructed retroactively to authorize existing local law?',
    'Documentary hadith criticism and historiography of early Medinan legal practice; dating the earliest ''amal citations against the papyrological and epigraphic record.',
    'If transmission is broken or constructed, ''amal''s evidentiary warrant collapses toward convention — the privilege becomes unwarranted, the extraction component of the measured profile rises, and the foundational axiom routes toward computed foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_transmission_historicity, empirical, 'Whether the school''s core evidentiary premise survives historical scrutiny.').

omega_variable(
    maslaha_discretion_bounds,
    'Does maslaha mursala in operation track general welfare, or does it function as discretionary space in which the jurist class legitimizes whatever the ruling order needs?',
    'Audit of rulings justified by unanchored public interest across the interval: outcome distribution, who initiated the invocation (state fiscal needs versus litigant protection), and reversal rates when interests changed.',
    'If invocations cluster around state and jurist-class interests, the effective extraction attributable to this source exceeds the base measure and the tangled-rope balance tilts toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_discretion_bounds, empirical, 'Whether the text-free interest source is welfare-tracking or discretion-rent.').

omega_variable(
    enforcement_vs_assent,
    'How much of the arrangement''s persistence is coercive enforcement versus genuine scholarly assent to the method''s soundness?',
    'Compare adherence in periods and territories of weak enforcement (Fatimid pressure, Almohad disruption): if practice-weighting persisted without enforcement machinery, assent dominates; if it lapsed, enforcement dominates.',
    'High assent means the suppression scalar overstates coercive maintenance and the arrangement is more rope-like than the profile suggests; enforcement-dependence confirms the actively-enforced hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_assent, empirical, 'Splitting persistence into coercion versus conviction.').

omega_variable(
    inter_madhhab_recognition_trajectory,
    'Did later Sunni ecumenism (mutual recognition of the four schools from the thirteenth century onward) convert the foreclosure relations into practical coexistence?',
    'Track cross-school appointment patterns, joint curricula, and school-switching rates in the post-classical period; if rival-school jurists gained western posts without abandoning their own usul, foreclosure softened at the institutional level.',
    'Softened foreclosure would reduce the victim burden on rival textualists, lowering measured suppression and extraction at the margin without changing this reading''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_madhhab_recognition_trajectory, conceptual, 'Whether the reading-family''s oppositional structure decayed into pluralism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 750, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_usul_drift_tr_t750, usul_al_fiqh_method__maliki_reading, theater_ratio, 750, 0.1).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t750, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t825, usul_al_fiqh_method__maliki_reading, theater_ratio, 825, 0.12).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t825, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.14).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t900, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t975, usul_al_fiqh_method__maliki_reading, theater_ratio, 975, 0.17).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t975, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t1050, usul_al_fiqh_method__maliki_reading, theater_ratio, 1050, 0.2).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t1050, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t1125, usul_al_fiqh_method__maliki_reading, theater_ratio, 1125, 0.22).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t1125, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t1200, usul_al_fiqh_method__maliki_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t1200, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t1275, usul_al_fiqh_method__maliki_reading, theater_ratio, 1275, 0.28).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t1275, observed).
narrative_ontology:measurement(maliki_usul_drift_tr_t1350, usul_al_fiqh_method__maliki_reading, theater_ratio, 1350, 0.3).
narrative_ontology:measurement_basis(maliki_usul_drift_tr_t1350, observed).

% Extraction over time
narrative_ontology:measurement(maliki_usul_drift_be_t750, usul_al_fiqh_method__maliki_reading, base_extractiveness, 750, 0.28).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t750, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t825, usul_al_fiqh_method__maliki_reading, base_extractiveness, 825, 0.33).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t825, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.37).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t900, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t975, usul_al_fiqh_method__maliki_reading, base_extractiveness, 975, 0.4).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t975, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t1050, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1050, 0.44).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t1050, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t1125, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1125, 0.45).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t1125, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t1200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1200, 0.48).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t1200, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t1275, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1275, 0.49).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t1275, observed).
narrative_ontology:measurement(maliki_usul_drift_be_t1350, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1350, 0.5).
narrative_ontology:measurement_basis(maliki_usul_drift_be_t1350, observed).

% Suppression requirement over time
narrative_ontology:measurement(maliki_usul_drift_su_t750, usul_al_fiqh_method__maliki_reading, suppression_requirement, 750, 0.35).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t750, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t825, usul_al_fiqh_method__maliki_reading, suppression_requirement, 825, 0.45).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t825, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t900, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t975, usul_al_fiqh_method__maliki_reading, suppression_requirement, 975, 0.48).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t975, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t1050, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1050, 0.68).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t1050, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t1125, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1125, 0.6).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t1125, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t1200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t1200, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t1275, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1275, 0.62).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t1275, observed).
narrative_ontology:measurement(maliki_usul_drift_su_t1350, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1350, 0.58).
narrative_ontology:measurement_basis(maliki_usul_drift_su_t1350, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'usul al-fiqh' decomposes into four structurally distinct source-hierarchy regimes — one per reading of the kernel. Each member has its own epsilon, beneficiary/victim structure and classification; this (maliki) member elevates practice-bearing communities and extracts from report-specialists and textualists, whereas the shafii member concentrates extraction at the authentication gate and the hanbali member at the innovation-blocking boundary. The upstream/downstream citation pattern runs through the family: al-Shafi'i's systematization was formulated AGAINST Medinan practice-weighting, so the shafii and maliki files are mutual antagonists rather than parent-child, and all four are linked pairwise through affects_constraints so contamination analysis can trace doctrinal spillover.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
