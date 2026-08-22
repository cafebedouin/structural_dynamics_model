% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual-Form Preservation as Survival (Symbol-Survival Reading of Post-Catastrophe Jewish Practice)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the symbol-survival reading of the
 *   catastrophe_memory_survival kernel: the arrangement under contest is the
 *   regime that preserves Jewish identity and boundary-norms through symbolic
 *   ritual experience, in which the people's survival consists in the
 *   continuity of practice itself. Forged in the catastrophes of the
 *   twentieth century, the arrangement holds that form must be kept even
 *   where meaning has thinned: every maintained practice is survival, and
 *   every secular household is a failed transmission. Compliance — time,
 *   dietary and marital constraint, endogamy pressure, interpretive deference
 *   — is taken from practicing members and boundary-crossers; interpretive
 *   monopoly over valid practice, valid marriage, and valid membership
 *   accrues to rabbinic authority, which in Israel exercises it with state
 *   backing. The epsilon authored here is reading-indexed over the fixed
 *   referent: the standing ritual-form-preservation arrangement, assessed by
 *   this reading's own lights. The reading does not deny the extraction — it
 *   justifies it as the price of survival; the metric records that it is
 *   extraction. KEY AGENTS (by structural relationship): rabbinic_authority:
 *   agenda-setter and primary beneficiary (institutional/identity_locked) —
 *   administers the boundary machinery and collects interpretive monopoly;
 *   observant_community_members: beneficiary with secondary payer position
 *   (organized/identity_locked) — receive identity goods, pay compliance;
 *   secularized_jews: primary target (moderate/mobile) — counted as
 *   transmission losses, invalidated where the rabbinate holds jurisdiction;
 *   agunot: concentrated targets (powerless/trapped) — the boundary machinery
 *   at full force; progressive_jewish_movements: excluded challengers
 *   (organized/constrained) — would redefine transmission, kept outside the
 *   table; ritual_memory_scholars: analytical observer — sees the full
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.75).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual-Form Preservation as Survival (Symbol-Survival Reading of Post-Catastrophe Jewish Practice)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'f032ad0f-4fd6-43b9-92ef-33cee54b8a80').
narrative_ontology:cs_kernel_codification('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', distributed).
narrative_ontology:cs_authority_grounding('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', extraction).
narrative_ontology:cs_interpretation_layer_present('f032ad0f-4fd6-43b9-92ef-33cee54b8a80').
narrative_ontology:cs_reading_relation('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', catastrophe_memory_survival__hybrid_encoding_reading, forecloses).
narrative_ontology:cs_axiom('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', foundational, survival_is_practice_continuity).
narrative_ontology:cs_axiom_status(survival_is_practice_continuity, holdable).
narrative_ontology:cs_axiom_grounding('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', survival_is_practice_continuity, deontological).
narrative_ontology:cs_axiom('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', secondary, form_preservation_outranks_semantic_loss).
narrative_ontology:cs_axiom_status(form_preservation_outranks_semantic_loss, holdable).
narrative_ontology:cs_axiom_grounding('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', form_preservation_outranks_semantic_loss, conventional).
narrative_ontology:cs_reference_frame('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', post_catastrophe_practice_continuity).
narrative_ontology:cs_drift_state('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', contemporary_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f032ad0f-4fd6-43b9-92ef-33cee54b8a80', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, agunot).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, observant_community_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, practice_continuity_constitutes_survival).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, boundary_norms_preserve_stateless_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordains, interprets, and enforces the boundary-norms: kashrut supervision, marriage and divorce validity, conversion standards, and the accounting that counts non-practicing Jews as transmission losses. In Israel it holds state-backed jurisdiction over marriage and divorce; in the diaspora it holds authority over synagogues, schools, courts, and life-cycle rites. Its standing rests on the survival-equals-continuity equation remaining authoritative; relaxing that equation would dissolve the office's own warrant. Leaving the arrangement would mean surrendering interpretive office as such, which the office cannot do and remain itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Keep Shabbat, kashrut, and the festival cycle; raise children inside day schools, youth movements, and endogamous marriage networks. They receive identity, community, and a continuity narrative that binds them to ancestors and murdered kin. They pay in time, dietary and marital constraint, endogamy pressure, and deference to rabbinic interpretation of disputes. Leaving means losing family, community, and a self-concept built on continuity; many experience the prospect as betrayal of the dead. Some chafe at particular rulings — divorce refusal, conversion gatekeeping, the status of intermarried children — while remaining inside.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_community_members, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, observant_community_members, payer).

% Identify as Jewish through ancestry, memory, culture, language, or politics without ritual practice. Under the arrangement's accounting each secular household is a failed transmission, a loss that legitimates the machinery that counts it. Where the rabbinate holds state power they meet its rules directly — no civil marriage in Israel, progressive conversions and marriages ruled invalid. Elsewhere they live outside the boundary machinery while remaining inside its survival statistics. Further assimilation remains open to them; they have, in the main, already walked out.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, mobile, global).

% Women whose husbands refuse to grant, or cannot be located to grant, a religious divorce. Under the boundary rules they may not remarry; a later union is adulterous and its children carry impaired status for generations. Rabbinic courts control the remedies and their pace; civil pressure in mixed jurisdictions helps only partially. Their situation is the boundary machinery applied at full force at its most concentrated point, and they have no exit inside the framework that confines them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, agunot, payer,
    powerless, biographical, trapped, global).

% Reform, Conservative, Reconstructionist, and liberal religious currents that re-describe survival as covenant, culture, ethical practice, or chosen peoplehood rather than form-continuity. They ordain their own clergy and validate their own conversions and marriages, but where rabbinic authority holds jurisdiction those acts are ruled void and their adherents are counted as losses. They would redefine what counts as transmission and who counts as a survivor; the table that decides does not include them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, progressive_jewish_movements, excluded,
    organized, generational, constrained, continental).

% Historians, anthropologists, and sociologists of Jewish ritual and collective memory. They document how the survival-equals-continuity equation was forged in catastrophe and how it operates now: whom it mobilizes, whom it counts as losses, where its enforcement concentrates, and what its practice actually transmits. They hold no stake in the boundary machinery and can hold the rival accounts of what ritual preserves side by side.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, ritual_memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a stateless, scattered minority as a distinct people across catastrophe and assimilation pressure: a shared practice calendar synchronizes communal life, marks who is inside the boundary, and hands membership to children through repetition rather than argument. The arrangement solves the collective problem of keeping a dispersed population recognizable to itself and to others without territory or sovereignty.
% TRANSFER_FUNCTION: Moves compliance — time, dietary and marital constraint, endogamy pressure, interpretive deference — from practicing members and boundary-crossers to rabbinic authority, which converts it into interpretive monopoly: the power to define valid practice, valid marriage and divorce, valid conversion, and who counts as surviving transmission. A second transfer runs in the accounting itself: legitimacy flows to the arrangement from every secular household it counts as a loss.
% ABSENT_VOICES: Secularized Jews and the progressive movements are the structurally absent voices: they would contest the equation of survival with practice-continuity and the rabbinate's monopoly on its definition, but the boundary machinery itself keeps them out — their marriages and conversions are ruled invalid, their practice ruled non-survival, so the conversation that would include them presupposes the very arrangement they dispute.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the Jewish population would not vanish with it: identity would re-form around language, genealogy, memory institutions, and ethnic self-description; rabbinic courts would lose their marriage-and-divorce jurisdiction and their survival accounting; agunot could remarry; the category 'lost to the Jewish people' would lose its enforcement. What rearranges is the definition of continuity and the offices that administer it.
% FOUNDING_PROBLEM: Repeated catastrophe — destruction of the Temples, expulsions, massacres, and finally the Holocaust — posed the question of what a stateless, scattered people must preserve to survive as itself: with territory and sovereignty gone, what carries 'the Jewish people' from one generation to the next?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is real and partially live — persistent antisemitism and assimilation pressure are documented by demographers and historians outside the rabbinate — but the claim that ritual-form continuity specifically is the operative survival mechanism is disputed from outside the beneficiary set: large-scale population surveys of contemporary Jewish identity (e.g. the Pew Research surveys), the demographic vitality of secular and cultural Jewish life, and the institutional carriers of the rival readings all attest that the problem persists while this arrangement's specific answer does not command assent. No corroborating source outside the benefiting parties attests that form-continuity is what survival requires.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the arrangement takes real compliance from practicing members and boundary-crossers while its accounting converts every secular household into legitimacy for the offices that administer the boundary; the extraction is decoupled from any service the payer can decline. Suppression (0.70) is substantial and dual-mechanism: structural sanction (family rupture, communal exclusion, state-enforced marriage jurisdiction) fused with internalized identity (exit experienced as betraying the dead) — the omega on suppression mechanism carries the ambiguity the scalar cannot. Theater (0.45) is moderate and rising: early in the interval practice was lived reconstruction and mourning; over time a growing share of observance and memory-work becomes continuity performance — remembrance ceremonies, continuity campaigns, identity tourism — where demonstrating continuity displaces the symbolic experience it was meant to carry. Accessibility_collapse (0.55) is mid-range: empirically the alternatives persist and flourish (secular and cultural Jewish life is demographically vital), but inside the framework they are rhetorically foreclosed — non-practice is defined as non-survival. Resistance (0.62) is high: mass secularization, intermarriage, organized progressive movements, and feminist challenges to divorce-refusal rulings are active, ongoing defection from the arrangement's terms. The measurement series run on one shared grid (1945-2017, seven points, every tracked metric authored at every point); the trajectories are a ratchet rather than a cycle — enforcement hardens as the practice base erodes, with crisis-mobilization pulses (post-war reconstruction, intermarriage panics, continuity campaigns) superimposed on the rise rather than driving a full oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the rabbinate's position the arrangement is covenantal fidelity it stewards: the compliance it collects is the price of the people's existence, and the loss-accounting is honest bookkeeping. From the agunot's position the same machinery is confinement without remedy; from the secularized Jews' position it is a definition of survival that counts them dead while demanding nothing they agreed to. Observant members straddle: they receive the identity goods and pay the compliance, and their identity-lock makes the two sides of their own position hard to separate even from the inside. The diffuse secularized population has little coalition incentive — its members are mobile and have already exited — so the resistance the arrangement meets is defection rather than organized counter-power, except where the progressive movements institutionalize it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: rabbinic_authority (collects the interpretive monopoly — the arrangement's gains demonstrably accrue to this seat) and observant_community_members (collect identity, community, and continuity goods). Victims: secularized_jews (invalidated, excluded, counted as losses) and agunot (confined by the boundary rules at their most concentrated point). Directionality follows the declarations: the rabbinate sits near the full-beneficiary end; observant members sit low-to-mid — net beneficiaries by revealed preference, but their identity-lock and compliance costs keep them well short of the arbitrage end; secularized Jews sit high — targets whose mobility damps but does not erase the extraction the accounting performs on them; agunot sit nearest the full-target end — trapped, powerless, no exit inside the framework. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct structural relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as tangled_rope rather than snare or rope is what keeps both halves of the structure visible. The coordination function is genuine: a stateless, scattered minority did persist across two millennia and repeated catastrophe, and shared practice is a real mechanism of that persistence — a pure-extraction reading would erase the achievement and mispredict the arrangement's resilience. But the extraction is real and asymmetric: interpretive monopoly, confinement of agunot, invalidation of rival marriages and conversions, and a survival-accounting that feeds on its own losses — a pure-coordination reading would launder the monopoly. The identity_coordination typing carries a known gaming risk: identity narratives are the classic cover for extraction, so the coupling test matters here — and the diagnostic signal is present, since extraction concentrates on the least powerful seat (agunot) while the identity framing justifies it. On mandatrophy: the founding problem (catastrophe survival of a stateless people) is partially live — antisemitism and assimilation pressure persist — so the arrangement's mandate is contested rather than dead; the mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie flag, but the contest itself is the live question this story routes to its omegas. The arrangement has not outlived its function; it has outgrown its monopoly's justification — which is the tangled-rope signature, not the piton's.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_memory_survival kernel. What would the sibling readings change structurally if adopted as the operative account of what survival consists in?',
    'Comparative institutional analysis: trace what each reading, if institutionalized, would count as survival, whom it would charge, and whose authority it would warrant. The competence reading warrants custodians of practical knowledge with low extraction; the hybrid reading warrants dual-register institutions; this reading warrants interpretive monopoly with the present victim set.',
    'The victim set and epsilon are reading-constituted: under the competence reading the victims are holders of lost practical knowledge and extraction is low; under this reading the victims are secularized Jews and extraction is high. Cross-reading comparison must hold the referent (the standing arrangement) fixed while treating epsilon as reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is the symbol-survival reading of a three-reading kernel; sibling adoption would change the victim set and epsilon.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (family rupture, communal exclusion, state-enforced marriage jurisdiction) or internalized (identity fused with continuity, exit experienced as betraying the murdered)?',
    'Post-exit suppression trajectory of those who leave observance (the off-the-derech population): if identity crisis, guilt, and perceived betrayal persist after structural barriers are removed, a substantial share of suppression is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — leavers carry the arrangement with them — and the identity-lock component is stronger than enforcement records suggest, shifting classification pressure toward the extraction side of the tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in identity-locked observant exit.').

omega_variable(
    survival_causal_attribution,
    'Does practice-continuity causally explain Jewish survival across catastrophe, or does the arrangement retrospectively claim credit for persistence achieved by other means (dispersion, host-economy niches, literacy mandates, mutual-aid networks)?',
    'Comparative historical demography of observant versus secularized populations across catastrophe events; counterfactual analysis of secular cultural-continuity projects (Hebrew revival, Yiddish cultural movements) that maintained identity without the boundary machinery.',
    'If the causal attribution fails, the coordination justification weakens and a larger share of the measured extraction stands naked — classification pressure drifts toward pure extraction; if it holds, part of the extraction is the price of a real survival mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_causal_attribution, empirical, 'Whether the survival-equals-continuity equation is causal mechanism or retrospective institutional narrative.').

omega_variable(
    victim_set_indexicality,
    'Are secularized Jews victims of this arrangement — as its own accounting holds, counting them as transmission losses — or agents who exited a constraint they never valued, as their self-understanding holds?',
    'This is the kernel contest itself and does not resolve inside one reading; the resolvable structural core is the cost the arrangement imposes in shared jurisdictions (invalidated marriages and conversions, no civil marriage where the rabbinate holds state power), which can be measured independently of the accounting dispute.',
    'If victims only in the reading''s accounting, epsilon for this story is reading-indexed and the victim declaration must not be read as a neutral demographic fact; if the shared-jurisdiction costs dominate, the victim set is robust across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_indexicality, conceptual, 'Reading-constituted victim set: secularized Jews as losses-in-the-accounting versus agents-who-exited.').

omega_variable(
    jurisdictional_concentration,
    'Enforcement intensity varies sharply by jurisdiction — state-enforced marriage, divorce, and conversion jurisdiction in Israel versus diffuse social enforcement in the diaspora. Does the authored epsilon describe the concentrated instance, the diffuse one, or the transnational arrangement?',
    'Decomposition test: if enforcement context moves epsilon beyond the scope-scaling the engine already applies, author separate stories for the state-enforced and socially-enforced instances and link them; if the gradient is captured by scope and directionality, one story stands.',
    'If the Israeli instance is a distinct constraint, its epsilon is materially higher (state coercion) and its victim set narrower (those inside the jurisdiction); the present story''s epsilon would then describe the diaspora-weighted arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_concentration, empirical, 'Jurisdictional enforcement gradient: one transnational arrangement or two constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 1945, 2017).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(cata_tr_t1945, observed).
narrative_ontology:measurement(cata_tr_t1957, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1957, 0.28).
narrative_ontology:measurement_basis(cata_tr_t1957, observed).
narrative_ontology:measurement(cata_tr_t1969, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1969, 0.31).
narrative_ontology:measurement_basis(cata_tr_t1969, observed).
narrative_ontology:measurement(cata_tr_t1981, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1981, 0.34).
narrative_ontology:measurement_basis(cata_tr_t1981, observed).
narrative_ontology:measurement(cata_tr_t1993, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1993, 0.37).
narrative_ontology:measurement_basis(cata_tr_t1993, observed).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2005, 0.41).
narrative_ontology:measurement_basis(cata_tr_t2005, observed).
narrative_ontology:measurement(cata_tr_t2017, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement_basis(cata_tr_t2017, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement_basis(cata_be_t1945, observed).
narrative_ontology:measurement(cata_be_t1957, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1957, 0.68).
narrative_ontology:measurement_basis(cata_be_t1957, observed).
narrative_ontology:measurement(cata_be_t1969, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1969, 0.7).
narrative_ontology:measurement_basis(cata_be_t1969, observed).
narrative_ontology:measurement(cata_be_t1981, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1981, 0.71).
narrative_ontology:measurement_basis(cata_be_t1981, observed).
narrative_ontology:measurement(cata_be_t1993, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1993, 0.72).
narrative_ontology:measurement_basis(cata_be_t1993, observed).
narrative_ontology:measurement(cata_be_t2005, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement_basis(cata_be_t2005, observed).
narrative_ontology:measurement(cata_be_t2017, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2017, 0.75).
narrative_ontology:measurement_basis(cata_be_t2017, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement_basis(cata_su_t1945, observed).
narrative_ontology:measurement(cata_su_t1957, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1957, 0.55).
narrative_ontology:measurement_basis(cata_su_t1957, observed).
narrative_ontology:measurement(cata_su_t1969, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1969, 0.58).
narrative_ontology:measurement_basis(cata_su_t1969, observed).
narrative_ontology:measurement(cata_su_t1981, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1981, 0.61).
narrative_ontology:measurement_basis(cata_su_t1981, observed).
narrative_ontology:measurement(cata_su_t1993, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1993, 0.64).
narrative_ontology:measurement_basis(cata_su_t1993, observed).
narrative_ontology:measurement(cata_su_t2005, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2005, 0.67).
narrative_ontology:measurement_basis(cata_su_t2005, observed).
narrative_ontology:measurement(cata_su_t2017, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement_basis(cata_su_t2017, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'ritual preserved the Jews through catastrophe' conflates three structurally distinct claims with materially different epsilon. competence_transmission_reading (upstream, higher empirical confidence): ritual transmits practical survival knowledge — timing, resource management, family protocols; extraction negligible. hybrid_encoding_reading (mediating): dual registers, survival depends on both; moderate extraction. symbol_survival_reading (this file, downstream): survival is practice-continuity itself; the form-sufficiency claim is what rabbinic interpretive monopoly rides on, and it carries the family's highest epsilon with a reading-constituted victim set. The upstream claim is cited as evidence by the downstream ones; this file links both siblings and authors only the symbol-survival reading as a clean epsilon-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
