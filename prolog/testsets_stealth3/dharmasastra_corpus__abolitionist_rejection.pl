% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Abolitionist Rejection Reading: The Dharmasastric Order as Illegitimate Extractive Charter
 *   domain: religious law/textual interpretation/normative authority
 *
 * SUMMARY:
 *   The standing arrangement under contest is the dharmasastric normative
 *   order: the smrti/sutra corpus together with the varna-jati hierarchy it
 *   licenses and the interpreter class that certifies both. Across the
 *   interval it ran as operative law under colonial codification, was
 *   stripped of formal legal authority by the constitutional revolution and
 *   the Hindu Code Bills, and now operates through informal enforcement -
 *   caste councils, marriage-market closure, purity labor, family violence -
 *   while its ceremonial layer grows. This file instantiates ONLY the
 *   abolitionist_rejection reading of the dharmasastra_corpus kernel: the
 *   corpus holds zero legitimate normative authority and must be wholly
 *   abandoned together with the hierarchy. Per the epsilon-referent rule,
 *   epsilon is authored for the standing dharmasastric arrangement as this
 *   reading assesses it - not for the egalitarian order the reading endorses.
 *   The claim (snare) and the metrics are independently authored facts; the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - brahmin_interpreter_class: agenda-setting collector (institutional/identity_locked) - runs the legitimating machinery and takes its fees, endowments, and grants
 *   - dwija_caste_elites: primary beneficiary (powerful/identity_locked) - inherits the corpus's allocations of schooling, marriage capital, and deference
 *   - caste_patriarch_households: domestic beneficiary (organized/constrained) - converts the corpus's household provisions into control over women's labor and marriage
 *   - dalit_and_outcaste_communities: primary target (powerless/trapped) - bears the ranking's floor: polluting labor, exclusion, violence; stigma tracks them through conversion
 *   - shudra_laboring_castes: mass target (organized/constrained) - bore service obligations and study bans; now numerically dominant but status-blocked
 *   - all_caste_women: cross-cutting target (moderate/constrained) - bear the domestic provisions in every rank
 *   - anti_caste_movements: target-turned-counter-agenda-setter (organized/mobile) - built the exits and the replacement order
 *   - modern_constitutional_state: counter-agenda-setter (institutional/constrained) - retired the corpus from law while administering the society it built
 *   - orthodox_devotional_laypeople: excluded voice (moderate/identity_locked) - would object to abandonment; holds no seat in any deliberation
 *   - comparative_dharmasastra_scholars: analytical observer (institutional/analytical) - reconstructs the corpus's operation from outside the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.85).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.82).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Abolitionist Rejection Reading: The Dharmasastric Order as Illegitimate Extractive Charter").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious law/textual interpretation/normative authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, 'f2a4b880-3eae-4b61-86e2-e695d2cd205a').
narrative_ontology:cs_kernel_codification('f2a4b880-3eae-4b61-86e2-e695d2cd205a', fixed_text).
narrative_ontology:cs_authority_grounding('f2a4b880-3eae-4b61-86e2-e695d2cd205a', extraction).
narrative_ontology:cs_interpretation_layer_present('f2a4b880-3eae-4b61-86e2-e695d2cd205a').
narrative_ontology:cs_reading_relation('f2a4b880-3eae-4b61-86e2-e695d2cd205a', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('f2a4b880-3eae-4b61-86e2-e695d2cd205a', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('f2a4b880-3eae-4b61-86e2-e695d2cd205a', foundational, hierarchy_constituted_authority_is_illegitimate).
narrative_ontology:cs_axiom_status(hierarchy_constituted_authority_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('f2a4b880-3eae-4b61-86e2-e695d2cd205a', hierarchy_constituted_authority_is_illegitimate, deontological).
narrative_ontology:cs_axiom('f2a4b880-3eae-4b61-86e2-e695d2cd205a', foundational, wholesale_dismantling_over_reinterpretation).
narrative_ontology:cs_axiom_status(wholesale_dismantling_over_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('f2a4b880-3eae-4b61-86e2-e695d2cd205a', wholesale_dismantling_over_reinterpretation, instrumental).
narrative_ontology:cs_reference_frame('f2a4b880-3eae-4b61-86e2-e695d2cd205a', illegitimate_extractive_charter).
narrative_ontology:cs_drift_state('f2a4b880-3eae-4b61-86e2-e695d2cd205a', post_constitutional_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2a4b880-3eae-4b61-86e2-e695d2cd205a', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmin_interpreter_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dwija_caste_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, caste_patriarch_households).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_and_outcaste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laboring_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, all_caste_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, anti_caste_movements).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, varna_jati_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, scriptural_infallibility_claim).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, karma_theodicy_of_station).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorize, transmit, and adjudicate the corpus: pandits, gurus, and dharmadhikarins who certify ritual validity, fix marriage and inheritance rules, and collect dakshina fees, temple endowments, and land grants for doing so. Their livelihood, rank, and self-understanding are constituted by the transmission lineages they staff; renouncing the corpus would dissolve their office, income, and communal standing at once.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmin_interpreter_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, brahmin_interpreter_class, beneficiary).

% Upper-caste landholding and mercantile households who inherit the corpus's allocations without administering them: priority in schooling and employment during its legal ascendancy, preferential marriage markets, deference owed by those ranked below, and freedom from the service obligations the corpus assigns to others. Exit would mean surrendering inherited status and the kin networks that organize their property and marriages.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dwija_caste_elites, beneficiary,
    powerful, generational, identity_locked, national).

% Household heads who wield the corpus's domestic provisions - pativrata ideals, controlled female sexuality, arranged endogamous alliance - to direct the labor, mobility, and marriage choices of wives and daughters. They gain obedient households and alliance capital; their authority is legible only inside the framework that licenses it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, caste_patriarch_households, beneficiary,
    organized, biographical, constrained, national).

% Those the corpus ranks outside or beneath the varna order: assigned polluting labor such as scavenging, leatherwork, and corpse handling, barred from wells, schools, temples, and village interiors, and subject to violence when they transgress. Statutory prohibition changed the law books; landlords, employers, and neighbors still enforce the ranking, and conversion to other religions has repeatedly failed to outrun the stigma, which reconstitutes itself inside the new community.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_and_outcaste_communities, payer,
    powerless, generational, trapped, national).

% The corpus assigns them service to the twice-born and bars them from Vedic study; in practice they supplied agricultural and artisanal labor while financing the ritual economy through fees and gifts. Today they carry the largest demographic weight among the ranked, hold growing electoral power, and remain locked out of the top of the marriage and status order that their numbers cannot purchase.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laboring_castes, payer,
    organized, generational, constrained, national).

% Every ranked community disciplines its women through the corpus's domestic provisions: early and arranged marriage, wifely obedience ideals, widow austerity, restricted property and ritual roles. Statutory reforms shifted the legal ground, but family, caste councils, and marriage markets still price women's conformity, and nonconforming women bear the violence that enforcement relies on.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, all_caste_women, payer,
    moderate, biographical, constrained, national).

% Movements from Phule and Ambedkar to the Dalit Panthers and contemporary Dalit feminism: they bear the ranking's costs directly and have built the counter-order - conversion ceremonies, constitutional litigation, reservation politics, annihilationist critique. They sit on both sides: bearing the costs of the ranking they fight while setting the legal and symbolic agenda that replaces it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, anti_caste_movements, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, anti_caste_movements, agenda_setter).

% Administers the republic that formally repudiates the ranking - constitutional equality clauses, untouchability prohibition, criminal penalties for caste violence - while depending on caste enumeration to run reservations and on elected coalitions organized by caste. It enforces the corpus's retirement from law while managing the social order the corpus built, unable to ignore the question it outlawed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, modern_constitutional_state, agenda_setter,
    institutional, generational, constrained, national).

% Ordinary practitioners whose festivals, domestic rites, and life-cycle ceremonies run on the corpus's calendar and categories. Abolitionist politics reads their practice as complicity; reformist elites negotiate over their heads. They hold no seat in either deliberation, and abandoning the framework would cost them the ritual grammar of their families and their dead.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_devotional_laypeople, excluded,
    moderate, biographical, identity_locked, national).

% Historians and philologists who reconstruct how the corpus was composed, transmitted, and applied - including its internal disputes, commentarial reversals, and regional variation. They owe allegiance to neither the pulpit nor the movement and can watch the whole structure from outside, including the ways both its defenders and its abolishers simplify it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, comparative_dharmasastra_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahmin_interpreter_class).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a decentralized civilization a shared normative operating system: procedures for litigation, debt, inheritance, and marriage; a ritual calendar validating life-cycle transitions; a division of labor and jurisdiction between king, priest, and household; and a common vocabulary of duty that made conduct legible across a subcontinent without centralized enforcement.
% TRANSFER_FUNCTION: Moves labor, service, education, ritual access, property, and honor up the varna gradient - from those ranked Shudra, outcaste, or female to twice-born male households - and moves fees, endowments, and deference to the interpreter class that certifies the whole arrangement.
% ABSENT_VOICES: The people the corpus legislated over had no voice in its composition or adjudication: Shudra and outcaste objection survives only as the transgression the texts police, and women's voices arrive mediated through male interpreters. In the present dispute, orthodox lay practitioners whose devotional lives run through the framework would object to wholesale abandonment, but they hold no seat in abolitionist or reformist deliberation - their attachment is read as false consciousness from both sides.
% DISAPPEARANCE_RATIONALE: Marriage markets, village labor arrangements, ritual economies, guru lineages, status signaling, and residential segregation all organize around the ranking the corpus licenses; overnight removal would force simultaneous reorganization of kinship, labor, worship, and politics - nothing about the current arrangement survives on its own momentum.
% FOUNDING_PROBLEM: How to regulate conduct, ritual validity, and dispute resolution across a vast, decentralized population where no sovereign reaches every village - answered by codifying a divinely ordered division of labor and a portable procedural law.
% FOUNDING_PROBLEM_CORROBORATION: No party outside the dispute attests a neutral version: constitutional and legal historians, seated outside the interpreter class, attest the regulatory mandate is dead - statute and constitutional equality replaced it; Ambedkarite scholarship attests the hierarchy's persistence independent of textual belief; orthodox authorities attest the opposite - that dharma is eternal and the mandate live. The status is genuinely disputed across seats, and no corroborating source commands agreement among them.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.85: statutory dismantling removed the largest public extractions (legal untouchability, explicit study bans), but the core transfers persist - manual scavenging remains an occupied trade, occupational segregation and caste wage gaps are measured, endogamy prices marriage, and educational gaps track caste. Suppression is 0.82 and unscaled by scope or power in the engine's arithmetic: enforcement went informal rather than away - caste council edicts, violence against intercaste couples, ostracism of nonconforming women. Theater_ratio is 0.46 and rising: as legal function decayed, a ceremonial layer grew (guru authority displays, selective citation in culture wars, recitation detached from adjudication) while real enforcement continues underneath. Accessibility_collapse is 0.65: exits exist (conversion, urban migration, constitutional recourse) but stigma has followed converts across religions and generations, and economic life remains caste-structured. Resistance is 0.72: two centuries of organized movements, the constitutional revolution itself, mass conversion waves, and ongoing litigation. All three metric series share one time grid (1900-2025, eight points). The suppression series is U-shaped, not cyclical: colonial courts enforced shastric personal law (high), the 1950s constitutional strip-down cut formal enforcement capacity sharply, and informal enforcement then re-hardened decade over decade - no intermittent-reinforcement oscillator is identified. On the suppression-mechanism split: the larger share is structural (labor bondage, residential segregation, marriage-market closure), with a rising internalized share (Ambedkar's graded inequality - each rank investing in those below - and sanskritization), routed to the internalized_graded_inequality omega.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the interpreter-class seat the corpus is a sacred charge and a livelihood - the experience of staffing a coordination order. From the dalit, shudra, and women's seats the same corpus is enforced extraction with violent margins. The state's seat administers a transition it officially completed seventy-five years ago and cannot close. Orthodox laypeople experience abolitionism itself as erasure of their ritual grammar. These divergences follow from power and exit asymmetries (identity_locked beneficiaries versus trapped payers versus a constrained administrator), not from the authored claim; the engine computes them and the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the subsidy end: the interpreter class doubly so (it runs the machinery and collects from it - fees, endowments, agrahara-style grants); dwija elites inherit allocations without administering; patriarch households convert domestic provisions into household control. Targets cluster at the full-target end: dalit communities are trapped (stigma follows conversion, so exit does not clear the ledger); shudra castes are constrained (numbers without status); women are constrained by kinship economics in every rank. The constitutional state sits opposite the corpus - it spends resources dismantling the arrangement - and anti-caste movements combine target position with counter-agenda setting. No directionality_overrides are authored: the override array is keyed by power atom, and this story contains two institutional seats (interpreter class and constitutional state) at opposite ends of the directionality range, so any single-atom override would collide; the differentiation rides the stakeholder surface instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - regulating a decentralized social order - was real, and the corpus did once coordinate; the abolitionist reading does not deny that history, it dates the mandate's death: statute and constitutional equality now perform the regulatory function. What distinguishes this reading's snare verdict from a piton verdict is the maintenance structure: a piton persists by inertia without a concentrated maintainer, whereas here the interpreter class and dwija elites actively maintain enforcement because they collect from it - concentrated beneficiaries plus active enforcement is the snare signature, with the coordination story functioning as cover. The R5 mismatch check runs clean: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges raises no dead-mandate zombie flag, because the parties genuinely dispute whether the mandate is dead - that dispute is the live contest between this reading and its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates only the abolitionist_rejection reading of the dharmasastra_corpus kernel; the orthodox_literalist and reformist_contextual readings instantiate different constraints over the same corpus - which reading''s structure is correct is not settled by this file.',
    'Compare the three sibling stories'' structural data: victim sets, epsilon values, and claimed types. Convergence on victim identity (outcastes, shudras, women) with divergent epsilon would locate the disagreement in authority attribution rather than in harm identification.',
    'If the reformist reading''s separability thesis holds, this reading''s epsilon overstates by counting a salvageable ethical core as extraction; if the orthodox reading''s eternity claim held, this reading''s zero-authority premise would collapse entirely and the constraint would restructure around observance rather than abandonment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would restructure the constraint''s victim set and epsilon.').

omega_variable(
    coordination_cover_verdict,
    'Is the corpus''s civilizational coordination function (litigation procedure, ritual calendar, dispute resolution) genuinely inseparable from hierarchy maintenance - cover, as this reading claims - or partially separable as the reformist reading contends?',
    'Examine communities running dharma-derived ethics without textual hierarchy (Navayana Buddhist sanghas, egalitarian liturgical movements): if the ethical function persists while the hierarchy is dropped, separability gains support.',
    'If separable, part of the measured extraction belongs to a salvageable coordination function and the pure-cover verdict weakens toward a hybrid; if inseparable, the abolitionist verdict stands and reinterpretation strategies are confirmed futile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cover_verdict, conceptual, 'Whether the coordination story is cover or carries a separable residue - the crux dividing this reading from the reformist sibling.').

omega_variable(
    textual_authority_vs_material_persistence,
    'How much of the ranking''s present-day extraction runs through textual and interpretive authority versus material structures (land, labor markets, marriage economics) that would survive textual abandonment?',
    'Regional and cohort comparison where interpreter authority atrophied earliest and deepest - post-temple-entry Kerala, urban anonymous labor markets, converted communities across generations - measuring whether extraction fell proportionally with textual authority.',
    'If material structures sustain most extraction independently, abolishing the corpus removes legitimacy but not the transfers, and any successor arrangement needs economic remedies alongside symbolic ones; if textual authority is load-bearing, abandonment bites directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_vs_material_persistence, empirical, 'Load-bearing question behind the abolitionist bet: does removing the charter remove the transfers?').

omega_variable(
    internalized_graded_inequality,
    'Is the measured suppression predominantly structural (economic dependency, residential segregation, marriage-market closure) or internalized (graded inequality - each rank investing in those below - and sanskritization)?',
    'Post-exit trajectory analysis: track suppression markers in individuals and communities after structural barriers fall (urban migration, conversion, reservation-enabled mobility); persistence of the markers after barrier removal indicates internalized carriage.',
    'If substantially internalized, dismantling texts and statutes leaves suppression intact - the constraint''s effective suppression exceeds the structural measure and outlives its enforcement machinery, raising the true cost of the transition this reading calls for.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_graded_inequality, empirical, 'Structural versus internalized suppression mechanism in a hierarchy whose victims reproduce it downward.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1900, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1920, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1920, 0.17).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1940, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1960, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1980, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t2000, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t2012, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2012, 0.44).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t2025, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2025, 0.46).

% Extraction over time
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1900, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1920, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1920, 0.91).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1940, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1940, 0.9).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1960, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1960, 0.87).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1980, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1980, 0.86).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t2000, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t2012, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2012, 0.85).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t2025, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1900, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1920, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1920, 0.77).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1940, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1960, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1960, 0.54).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1980, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t2000, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t2012, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2012, 0.74).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t2025, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% The colloquial label 'Dharmasastra' conflates three structurally distinct commitments over one corpus-kernel: eternal binding prescription (orthodox_literalist), a separable ethical core (reformist_contextual), and zero legitimate authority requiring wholesale abandonment (this file). Each reading is authored as its own story with its own epsilon, victim set, and claimed type, linked here as a constraint family. This reading sits upstream-negative: abolitionist mobilization stripped the corpus of legal enforcement, and that stripped condition is precisely the operating environment in which the reformist interpretive project and the orthodox observance claims now proceed - the decomposition, not the label, is what makes the three epsilons stable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
