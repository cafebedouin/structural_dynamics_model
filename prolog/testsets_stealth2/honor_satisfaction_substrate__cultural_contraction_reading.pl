% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Culture-of-Honor Satisfaction Code (Cultural Contraction Reading)
 *   domain: historical sociology/cultural anthropology/legal history
 *
 * SUMMARY:
 *   For roughly a century and a half, European and American gentlemanly
 *   society ran on a satisfaction code: an insult obliged the challenged
 *   party to stake his life in a rule-bounded exchange, and refusal cost him
 *   his standing, his career, and in the officer professions his legal
 *   viability. This story authors that code as the standing arrangement under
 *   contest, assessed by the cultural_contraction_reading's own lights: a
 *   real normative order that carried genuine coordination value (bounding
 *   feud among armed elites) while extracting asymmetrically (its blood-price
 *   fell hardest on those with the fewest exits), and whose dissolution came
 *   from inside — the honor substrate itself contracted as cultures of honor
 *   gave way to cultures of dignity, until dueling exited the thinkable
 *   action-set altogether. On this reading the decline was mountain-erosion
 *   in mechanism though not in classification: no enforcer defeated the code,
 *   no victor collected from its fall; its constitutive preconditions (worth
 *   equals courage-display, satisfaction equals life-stake, a closed status
 *   class with weak legal recourse) simply stopped being believed, and the
 *   arrangement dissolved into theater and then into nothing. Per the
 *   epsilon-invariance discipline, the colloquial label 'why dueling died' is
 *   decomposed into three linked constraint stories — exogenous enforcement,
 *   endogenous contraction (this file), and their overdetermined composite —
 *   each with its own epsilon, suppression trajectory, and classification
 *   dynamics. KEY AGENTS (by structural relationship): -
 *   established_gentleman_elite: Agenda-setter and principal beneficiary
 *   (institutional/constrained) — administers the code, collects standing
 *   from it, cannot cheaply abstain - junior_officers_subaltern_gentlemen:
 *   Primary target (moderate/identity_locked) — bears the blood-price; exit
 *   is a self he does not have - poor_gentlemen_status_seekers: Primary
 *   target (powerless/trapped) — pays most in risk and treasure for least
 *   standing - widows_orphans_of_the_fallen: Silent cost-bearers
 *   (powerless/trapped) — no seat in the protocol that orphaned them -
 *   clergy_evangelical_reformers: Excluded voice (organized/constrained) —
 *   centuries of condemnation with zero standing inside the framework -
 *   dignity_culture_proponents: Excluded voice turned successor
 *   (organized/mobile) — dissolves the premise rather than answering it -
 *   state_legal_authorities: Impotent observer (institutional/analytical) —
 *   statutes without convictions - historical_sociologists_of_honor:
 *   Analytical observer (analytical/analytical) — assesses the causal claim
 *   from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.07).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.1).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Culture-of-Honor Satisfaction Code (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical sociology/cultural anthropology/legal history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'a4ed104a-7586-4da0-8975-17298c994b9c').
narrative_ontology:cs_kernel_codification('a4ed104a-7586-4da0-8975-17298c994b9c', distributed).
narrative_ontology:cs_authority_grounding('a4ed104a-7586-4da0-8975-17298c994b9c', practice).
narrative_ontology:cs_interpretation_layer_present('a4ed104a-7586-4da0-8975-17298c994b9c').
narrative_ontology:cs_reading_relation('a4ed104a-7586-4da0-8975-17298c994b9c', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('a4ed104a-7586-4da0-8975-17298c994b9c', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('a4ed104a-7586-4da0-8975-17298c994b9c', foundational, normative_unthinkability_over_external_ban).
narrative_ontology:cs_axiom_status(normative_unthinkability_over_external_ban, holdable).
narrative_ontology:cs_axiom_grounding('a4ed104a-7586-4da0-8975-17298c994b9c', normative_unthinkability_over_external_ban, empirically_contingent).
narrative_ontology:cs_axiom('a4ed104a-7586-4da0-8975-17298c994b9c', foundational, honor_dignity_mutual_exclusivity).
narrative_ontology:cs_axiom_status(honor_dignity_mutual_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('a4ed104a-7586-4da0-8975-17298c994b9c', honor_dignity_mutual_exclusivity, empirically_contingent).
narrative_ontology:cs_axiom('a4ed104a-7586-4da0-8975-17298c994b9c', secondary, substrate_erosion_needs_no_enforcer).
narrative_ontology:cs_axiom_status(substrate_erosion_needs_no_enforcer, holdable).
narrative_ontology:cs_axiom_grounding('a4ed104a-7586-4da0-8975-17298c994b9c', substrate_erosion_needs_no_enforcer, empirically_contingent).
narrative_ontology:cs_reference_frame('a4ed104a-7586-4da0-8975-17298c994b9c', honor_constitutive_consensus).
narrative_ontology:cs_drift_state('a4ed104a-7586-4da0-8975-17298c994b9c', post_wwi_dignity_settlement, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('a4ed104a-7586-4da0-8975-17298c994b9c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, established_gentleman_elite).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, junior_officers_subaltern_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, poor_gentlemen_status_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, widows_orphans_of_the_fallen).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, doctrine_of_satisfaction).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, point_of_honor_precedence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior officers, landed magnates, magistrates, and parliamentarians who wrote and administered the codes of honor, staffed the seconds' protocols, and policed polite society's boundaries. The code ordered their status competition and channeled officer violence into controllable forms; they collected standing and security from its operation. They were also exposed by it — Hamilton and Pushkin died inside their own institution — and unilateral abstention meant ceding precedence to rivals, so even the administrators could not cheaply exit. Their grip on the code depended on the surrounding honor consensus continuing to believe what they believed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, established_gentleman_elite, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, established_gentleman_elite, beneficiary).

% Commissioned officers and younger sons of gentry for whom the honor identity was constitutive of rank itself. A refused challenge meant courts-martial pressure, regimental ostracism, and the collapse of the self they had been trained to inhabit; accepting meant staking life for a word. They bore the code's blood-price disproportionately — they fought more often than generals, and their careers ended over slights their seniors traded casually. Exit was not a place but a self they did not have.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, junior_officers_subaltern_gentlemen, payer,
    moderate, biographical, identity_locked, national).

% Men of small fortune living on the edge of genteel recognition — physicians, attorneys, unendowed younger sons — for whom honor compliance was the toll of admission to polite standing. They could not refuse a challenge without confirming the inferiority they were trying to escape, could rarely afford the travel, weapons, and seconding a proper affair required, and had no institutional shield. The code took the most from them, in both risk and treasure, in exchange for the least standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, poor_gentlemen_status_seekers, payer,
    powerless, biographical, trapped, national).

% Families of men killed in affairs of honor. They bore the permanent costs — livelihood, protection, name — while possessing no seat whatsoever in the protocol that produced the loss; the code's entire conversational space ran between challenger, challenged, and seconds. Their grief entered the historical record only as reformers' testimony, never as adjudication.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, widows_orphans_of_the_fallen, payer,
    powerless, generational, trapped, national).

% Church courts, evangelical movements, and dissenting ministries that condemned dueling as sin from the seventeenth century onward. Inside the honor framework their objection carried no standing — piety was precisely what the code classified as non-genteel — so centuries of condemnation altered practice hardly at all. They preached to populations increasingly outside the honor consensus, which is where their words eventually found traction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, clergy_evangelical_reformers, excluded,
    organized, generational, constrained, continental).

% Commercial and professional middle classes, sentimental novelists, utilitarian reformers, and the expanding reading public building an alternative account of personal worth: character, self-command, and moral interiority rather than courage-display, with legal recourse replacing personal vengeance. They did not argue inside the code; they dissolved its premise by making a different life imaginable — one in which a man who declined a duel lost nothing that mattered. Their counter-world is the substrate into which the honor code collapsed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_proponents, excluded,
    organized, generational, mobile, continental).

% Sovereigns, legislatures, and courts that prohibited dueling repeatedly — edicts from the sixteenth century onward, statute books thick with penalties — and almost never convicted, because juries refused to hang gentlemen for what gentlemen all agreed was obligatory. Their instruments registered the code's existence without touching it; the gap between their statutes and their outcomes is the clearest recorded evidence that enforcement was not the operative variable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).

% Later analysts reconstructing the honor economy from correspondence, court records, regimental archives, and advice literature. They compare honor and dignity societies across space and time, trace which preconditions the transition required, and hold the seat from which this story's causal claim is ultimately assessed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_sociologists_of_honor, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, established_gentleman_elite).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converted potentially unbounded feud among armed gentlemen into rule-bounded single-exchange settlement: challenge protocol, seconds, negotiated conditions, apology ramps — violence with a stopping rule. Simultaneously allocated and defended membership in the honor class: willingness to stake life certified gentlemanly standing and policed the boundary between gentlemen and the rest.
% TRANSFER_FUNCTION: Moved lethal risk and social standing. Challengers transferred the burden of response onto challenged men; the code moved security-of-standing upward, to established elites whose position was insured by the system they administered, and risk downward, to junior officers and poor gentlemen whose refusal meant ruin. It also moved deference: public acknowledgment of courage from losers, refusers, and the polite world watching.
% ABSENT_VOICES: Widows and orphans of the fallen had no seat in the adjudication of any affair of honor; clergy condemned dueling from outside a framework that assigned their objection zero standing; the dignity-culture middle classes rejected the code's premise wholesale, but the code had no procedure for hearing 'the premise is wrong.' All three sat outside the challenge-and-second protocol that constituted the code's entire conversational space.
% DISAPPEARANCE_RATIONALE: Gentlemanly status allocation, officer careers, political conflict management, and the marriage market of respectable families all ran through satisfaction obligations. When the substrate contracted, all of these rearranged around dignity-based worth — self-possession rather than courage-display, legal recourse rather than personal vengeance, character rather than reputation-for-violence. Nothing filled the duel's slot because the demand side dissolved along with the supply: men no longer wanted what the duel delivered.
% FOUNDING_PROBLEM: Armed elites under a weak state monopoly on violence needed a way to answer insult without triggering unbounded vendetta: how to vindicate status, deter contempt, and stop escalation at one exchange. The honor code was built to bound violence that could not be prevented.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary anti-dueling reformers — clergy, Benthamite utilitarians, bereaved families testifying to inquiries — attested from outside the beneficiary set that the code's protective function was cover for status coercion. Modern historians of the duel and comparative anthropologists attest the founding problem was specific to honor substrates: honor-satisfaction logics return precisely where the dignity transition never completed, confirming the problem was substrate-dependent rather than perennial. No beneficiary-party source is relied upon.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).
:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scalar convention: base_properties report the interval-END state (the vestige), matching the final column of the shared measurement grid; the operative-era profile lives in the series (T0: epsilon 0.58, suppression 0.68, theater 0.12). Extraction at maturity was substantial but not total — the code delivered real coordination value (single-exchange settlement with apology ramps replaced unbounded vendetta) while compelling lethal risk asymmetrically; it decays monotonically as the dignity transition proceeds, reaching near-zero as the last ceremonial duels expire. Suppression_requirement FALLS across the whole interval — this is the reading's fingerprint in the data. A suppression-driven decline (the practice_decline sibling) requires a rising enforcement ratchet; the historical record shows the opposite: statutes accumulated for 150 years with conviction rates near zero, refusal penalties relaxed as elite consensus split, and the code's own enforcement machinery (challenge obligations, regimental sanction, social death) attrited before extinction rather than being overcome by superior force. Theater_ratio rises in mirror image: functional satisfaction duels gave way to bloodless Belle Epoque epee rituals and the German Mensur's deliberately sought scars — performance replacing function as the substrate emptied. Accessibility_collapse and resistance scalars describe the end state (alternatives fully open, no one bothering to resist a dead norm); across the arc they ran high (roughly 0.8, once the code was understood refusal meant social death) and moderate (roughly 0.35 at peak: anti-dueling societies, famous refusals, religious campaigns) respectively. Coalition note: the code's victims could not coalition — its identity-lock fragmented them across regiments and nations, and the only effective anti-dueling coalitions formed OUTSIDE the honor identity (clergy, utilitarians, bereaved civilians), which is itself evidence that the binding mechanism was internalized identity rather than enforceable structure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrator seat compute different constraints from the same protocol. From the established elite's position the code was the constitution of their world — the thing that made status legible, officer violence governable, and insult answerable; they experienced it as order they administered, and their own exposure (Hamilton, Pushkin) read as the price of the order, not extraction. From the junior officer's position the same protocol was a machine that consumed men of his rank for slights traded carelessly by his betters, with exit measured in selves rather than places. The poor gentleman experienced a toll booth at the border of a class he could not otherwise enter. The state observer saw statutes everyone honored and no one obeyed. The excluded voices saw the premise itself as the pathology — and had no procedural slot from which to say so. Same nominal institution, incompatible phenomenologies; the engine computes the divergence from power, exit, and role, and the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map straight onto the honor economy: the established elite collect standing and administrative control (beneficiary end, d near 0.0, damped further by their steering power over the rules); junior officers, poor gentlemen, and the bereaved families bear the transfer (target end, d pushed toward 1.0 — amplified for the officer class by identity lock, maximal for the trapped status-seekers). The excluded voices (clergy, dignity proponents) hold no declared beneficiary or victim position — they neither pay the code's toll nor collect its rents — so their directionality rides the structural fallback; their causal significance runs through the substrate, not through the receipt stream. State authorities sit near-symmetric and analytically distant: they touched the constraint only through instruments that failed. Scope note: the code operated at continental scale but verified locally — each affair adjudicated face-to-face inside a small elite — so scope-amplification of extraction stayed modest despite the wide spatial footprint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — bounding feud among armed gentlemen under a weak state monopoly on violence — died with its preconditions: states consolidated legitimate violence, and dignity ontologies decoupled worth from violent vindication. The arrangement outlived the mandate by two generations as pure theater (bloodless French duels, scar-collecting Mensuren) sustained by institutional inertia in closed corps — the dead-mandate/persistent-arrangement mismatch, resolving toward vestige rather than capture because no seat profits enough to maintain it deliberately; the German officer corps' late attachment was identity nostalgia, not rent collection. The classification prevents two symmetrical mislabelings: calling the whole arc a snare erases the genuine coordination function that kept the code self-sustaining for centuries without state enforcement; calling it a rope erases the asymmetric blood-price paid by the exit-less. Tangled rope at maturity, decaying through piton-shaped theater into extinction — and the decay mechanism itself, on this reading, was substrate erosion: the constraint was never defeated, it was abandoned by the world that had made it thinkable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_isolation,
    'This story instantiates the cultural_contraction_reading of the honor_satisfaction_substrate kernel; what structural features would differ under the sibling readings?',
    'Cross-read the sibling stories'' suppression trajectories against shared archival indicators: conviction rates and enforcement budgets (exogenous arm) versus sermon content, advice literature, obituary treatment of refusals, and the dating of ''unthinkable'' language (endogenous arm).',
    'If enforcement ratcheted while belief held steady, this story''s falling suppression series misattributes the mechanism and the practice_decline structure becomes the accurate account; if belief moved first everywhere enforcement was constant, the composite loses its exogenous arm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_isolation, empirical, 'Committer routing: one reading of a contested kernel; sibling readings instantiate rival causal structures over the same record.').

omega_variable(
    endogenous_delegitimation_primacy,
    'Did the honor substrate contract autonomously, or did accumulating legal and institutional pressure induce the cultural change?',
    'Timing analysis: statutory prohibitions predate the decline by 150-plus years with no measurable effect until belief-change indicators move; compare jurisdictions with identical statutes and divergent outcomes (post-statute persistence in the American South, earlier British extinction under similar law).',
    'If law shaped belief, the composite reading gains and this story''s epsilon-trajectory reads as law-induced rather than self-sustaining erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_delegitimation_primacy, empirical, 'Primacy of endogenous delegitimation over induced cultural change.').

omega_variable(
    dignity_transition_preconditions,
    'Was the honor-to-dignity transition driven by structural forces (commercial interdependence, print publics, state-building) or by deliberate reform agency — and was it therefore inevitable or contingent?',
    'Comparative timing of the transition against commercialization and literacy gradients across regions; counterfactual check on honor societies that industrialized without transitioning.',
    'If structural and near-inevitable, honor-substrate constraints behave as self-dissolving when preconditions vanish (erosion model confirmed); if contingent on agency, the contraction was reversible and the code''s persistence was a live possibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_transition_preconditions, conceptual, 'Driver and reversibility of the dignity transition.').

omega_variable(
    honor_pocket_persistence,
    'Why do honor-satisfaction logics persist in pockets (Caucasus and Mediterranean honor violence, gang honor codes, the German Mensur into the late twentieth century) if the substrate transformation was foundational?',
    'Test which dignity-transition preconditions (state monopoly on violence, commercial interdependence, print publics, exit mobility) are absent in each pocket; correlate pocket survival with precondition absence rather than enforcement presence.',
    'Pocket survival tracking missing preconditions confirms substrate-dependence; survival tracking enforcement gaps instead revives the suppression-model sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_pocket_persistence, empirical, 'Residual honor pockets as a discriminant between substrate-erosion and suppression accounts.').

omega_variable(
    suppression_internalization_split,
    'How much of the code''s hold on its targets was structural (courts-martial, legal disability, economic ruin) versus internalized (honor identity making refusal unthinkable)?',
    'Refusal-case outcomes: men who declined with social cover (wealth, foreign residence, clerical office) frequently survived; track whether penalties pursued refusers who escaped the identity frame.',
    'If the hold was mostly internalized, the constraint''s grip tracked identity infrastructure — explaining both the failure of legal remedy and the completeness of the dignity-shift dissolution; if mostly structural, the practice_decline sibling''s enforcement story strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized share of the honor code''s coercive grip.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1770, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1770, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1770, observed).
narrative_ontology:measurement(hono_tr_t1805, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1805, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1805, observed).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1840, 0.38).
narrative_ontology:measurement_basis(hono_tr_t1840, observed).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1870, 0.55).
narrative_ontology:measurement_basis(hono_tr_t1870, observed).
narrative_ontology:measurement(hono_tr_t1895, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1895, 0.72).
narrative_ontology:measurement_basis(hono_tr_t1895, observed).
narrative_ontology:measurement(hono_tr_t1920, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1920, 0.85).
narrative_ontology:measurement_basis(hono_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1770, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1770, 0.58).
narrative_ontology:measurement_basis(hono_be_t1770, observed).
narrative_ontology:measurement(hono_be_t1805, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1805, 0.55).
narrative_ontology:measurement_basis(hono_be_t1805, observed).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1840, 0.42).
narrative_ontology:measurement_basis(hono_be_t1840, observed).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1870, 0.28).
narrative_ontology:measurement_basis(hono_be_t1870, observed).
narrative_ontology:measurement(hono_be_t1895, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1895, 0.16).
narrative_ontology:measurement_basis(hono_be_t1895, observed).
narrative_ontology:measurement(hono_be_t1920, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1920, 0.07).
narrative_ontology:measurement_basis(hono_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1770, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1770, 0.68).
narrative_ontology:measurement_basis(hono_su_t1770, observed).
narrative_ontology:measurement(hono_su_t1805, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1805, 0.64).
narrative_ontology:measurement_basis(hono_su_t1805, observed).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1840, 0.48).
narrative_ontology:measurement_basis(hono_su_t1840, observed).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1870, 0.33).
narrative_ontology:measurement_basis(hono_su_t1870, observed).
narrative_ontology:measurement(hono_su_t1895, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1895, 0.22).
narrative_ontology:measurement_basis(hono_su_t1895, observed).
narrative_ontology:measurement(hono_su_t1920, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement_basis(hono_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why dueling died' decomposes into three structurally distinct causal constraints: exogenous enforcement (practice_decline_reading — suppression-driven, predicting rising enforcement ratchets), endogenous substrate transformation (this file — belief-driven, predicting enforcement decay and unthinkability), and their overdetermined composite. Each carries its own suppression trajectory and therefore its own classification dynamics; epsilon differs across the family because the referent arrangement's operative mechanism differs, not because any story measures one constraint two ways. Historiographic flow runs from the practice reading (earliest accounts emphasized bans) through this reading (which reframes the same record around belief change) to the composite (which subsumes both).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
