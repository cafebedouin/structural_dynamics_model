% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor-Violence Legitimacy (Drop Reading): Dueling as Structurally Valid but Practically Suppressed
 *   domain: legal_anthropology/historical_sociology
 *
 * SUMMARY:
 *   Honor-based dueling declined sharply across Western Europe and North
 *   America from the 17th through 19th centuries. This constraint captures
 *   ONE reading of that decline: dueling remained a structurally legitimate
 *   response to insult within the honor code, but became practically rare
 *   because external costs (state prosecution, ecclesiastical penalty, social
 *   stigma, legal jeopardy, execution) made participation increasingly
 *   expensive. In this reading, the honor code did not change; honor still
 *   required violent response to insult. What changed was the cost equation:
 *   as states criminalized dueling and enforced penalties, the material price
 *   of honoring rose beyond what most participants would rationally pay. The
 *   constraint thus operates as a tangled rope: it coordinates genuine
 *   reputation-enforcement (a real problem), but extracts via identity-lock
 *   (participants cannot exit the honor frame without ceasing to exist
 *   socially as gentlemen) and via state-imposed cost (participation means
 *   legal jeopardy). This reading is one of three siblings in the
 *   honor-violence-legitimacy kernel; the contraction reading argues that
 *   honor itself was redefined to exclude violence (making dueling
 *   unthinkable rather than merely expensive); the composite reading argues
 *   both mechanisms operated. This story instantiates only the drop reading.
 *
 * KEY AGENTS:
 *   - code_of_honor_society: maintains honor-code legitimacy through social enforcement and ritual
 *   - insulted_gentleman: faces identity-lock (honor-code defines legitimate response as violence) and rising material cost (legal penalty)
 *   - insulting_party: equally bound by honor-code (refusal to accept duel destroys honor)
 *   - state_enforcement_apparatus: criminalizes dueling, increases cost of participation through prosecution and penalty
 *   - ecclesiastical_authorities: condemn dueling as sin, offer alternative paths, but do not reframe honor itself
 *   - excluded_lower_orders: bear dueling's costs without honor-stake; permanently absent from legitimacy deliberation
 *   - analytical_observer: measures constraint frequency, cost, legitimacy-framing, and drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.41).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.38).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor-Violence Legitimacy (Drop Reading): Dueling as Structurally Valid but Practically Suppressed").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "legal_anthropology/historical_sociology").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '4d370082-d37a-403e-9200-a512527bdea5').
narrative_ontology:cs_kernel_codification('4d370082-d37a-403e-9200-a512527bdea5', fixed_text).
narrative_ontology:cs_authority_grounding('4d370082-d37a-403e-9200-a512527bdea5', lineage).
narrative_ontology:cs_interpretation_layer_present('4d370082-d37a-403e-9200-a512527bdea5').
narrative_ontology:cs_reading_relation('4d370082-d37a-403e-9200-a512527bdea5', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d370082-d37a-403e-9200-a512527bdea5', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('4d370082-d37a-403e-9200-a512527bdea5', foundational, honor_requires_violent_response).
narrative_ontology:cs_axiom_status(honor_requires_violent_response, holdable).
narrative_ontology:cs_axiom_grounding('4d370082-d37a-403e-9200-a512527bdea5', honor_requires_violent_response, conventional).
narrative_ontology:cs_axiom('4d370082-d37a-403e-9200-a512527bdea5', foundational, honor_code_legitimacy_stable).
narrative_ontology:cs_axiom_status(honor_code_legitimacy_stable, holdable).
narrative_ontology:cs_axiom_grounding('4d370082-d37a-403e-9200-a512527bdea5', honor_code_legitimacy_stable, empirically_contingent).
narrative_ontology:cs_reference_frame('4d370082-d37a-403e-9200-a512527bdea5', honor_violence_inseparability).
narrative_ontology:cs_drift_state('4d370082-d37a-403e-9200-a512527bdea5', high_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d370082-d37a-403e-9200-a512527bdea5', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_upheld_communities).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, involuntary_dueling_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, insulted_gentleman).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, insulted_gentleman).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, insulting_party).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, ecclesiastical_authorities).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, honor_requires_violent_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and enforces the code of honor that positions violence as the only legitimate response to insult, slander, or family dishonor. Adjudicates what offenses require satisfaction. Enforces the constraint by social censure, reputation destruction, and ritual participation in duels. Members organize seconds, witnesses, and consecrating figures. The constraint persists through their continuous legitimacy conferral.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, code_of_honor_society, agenda_setter,
    organized, generational, identity_locked, regional).

% Receives an insult or slander. The code declares that non-violent response (suit, public retraction, apology negotiation) is cowardly and destroys honor and marriageability. The gentleman faces a structural choice: accept social death through dishonor, or submit to mortal risk through dueling. Even if external costs rise (legal prosecution, ecclesiastical penalty, economic loss), the honor framework remains the legitimate definition of self-worth, making exit unthinkable despite rising material penalty.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, insulted_gentleman, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, insulted_gentleman, beneficiary).

% Commits an insult or slander. Once the offense is public, the honor code positions the insult-giver as bound to accept a duel if challenged. Refusal destroys their honor equivalently. Both parties face the same structural bind: the honor framework defines legitimate response as violence, and backing down (regardless of the material cost) triggers social annihilation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, insulting_party, payer,
    moderate, biographical, identity_locked, regional).

% Opposes dueling through law and prosecution. As dueling becomes criminalized and enforcement intensifies (severity of penalties rising, trials publicized, execution of duelers), the external cost of participation increases. However, the state's legal prohibition does not reframe honor — it merely makes honor-satisfaction costlier. The constraint remains structurally legitimate (the code says violence is honorable) while the state makes participation practically riskier.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_enforcement_apparatus, payer,
    institutional, generational, analytical, national).

% Condemn dueling as sinful and murder. Excommunicate duelers and seconds. Offer alternative paths to honor through public penance or ecclesiastical arbitration. Yet for participants embedded in the honor code, church condemnation is external noise rather than a redefinition of honor itself — dueling remains thinkable as honorable even if it is condemned as sinful.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, ecclesiastical_authorities, payer,
    organized, generational, analytical, regional).

% Are barred from the duel entirely — it is a gentlemen's constraint. They bear dueling's social costs (instability, loss of productive members, contagion of violence norms) without the honor stake that frames the constraint as legitimate. Their objection — that dueling is wasteful and unjust — is never heard in honor-code deliberation because they lack standing in the honor community.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, excluded_lower_orders, excluded,
    powerless, biographical, trapped, regional).

% Measures the constraint's operation: frequency (duels per capita per decade), cost profile (legal penalties, fatality rates, social disruption), conceptual status (is dueling still thinkable as honorable, or has honor been redefined to exclude violence), and legitimacy drift (do participants cite honor-obligation or mere custom/momentum).
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, code_of_honor_society).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code solves a trust and reputation problem: without a mechanism that publicly vindicates wronged parties and visibly punishes insults, reputation would become untethered from conduct, and agreements would lack credible enforcement. The duel establishes that insults are costly and dishonor is real, making reputation stakes meaningful in a pre-institutional-law society.
% TRANSFER_FUNCTION: Moves mortal risk and legal/ecclesiastical penalty from the insulted gentleman to both duelists. The constraint transfers death risk and legal jeopardy as the price of defending honor. As the state criminalizes dueling, the constraint also transfers risk of prosecution, imprisonment, and execution.
% ABSENT_VOICES: The excluded lower orders would object to dueling as wasteful and would advocate for alternative mechanisms (courts, arbitration, public compensation) — but they have no standing in honor-code deliberation. Women as stakeholders in gentlemen's honor (reputation determines marriageability) are also structurally absent from the decision to duel; they are beneficiaries (their family honor is defended) and victims (they absorb the social cost of the gentlemen's deaths) but never voice the trade-off.
% DISAPPEARANCE_RATIONALE: If dueling as an honor-legitimate mechanism disappeared overnight, the gentlemen would lose their primary legitimacy device for resolving insults. They would reorganize around alternative mechanisms: courts, public apologies, compensatory damages, or new honor codes that exclude violence. The structure of reputation would reorganize; trust would reattach to new mechanisms.
% FOUNDING_PROBLEM: In a society without centralized legal institutions, insults and slanders are unenforceable through law. A gentleman wronged by slander has no way to legally compel the slanderer to retract or compensate. Honor-based dueling establishes a private enforcement mechanism: the insult-giver knows that an insult may trigger a lethal challenge, which makes the insult costly and deters false slander.
% FOUNDING_PROBLEM_CORROBORATION: Honor-code participants and historians of early modern dueling attest that the founding problem is live: without dueling, there is no mechanism to enforce reputation and no cost to slander in weak-law contexts. State prosecutors and modern legal systems attest that the problem is dead: centralized courts, libel law, public trials, and reputation adjudication by judges have superseded the need for private duel-based enforcement. The contraction-reading tradition (those who hold that honor itself was redefined) attests that the problem is reframed rather than solved: honor now means responsiveness to legal remedy and public vindication through law, not through violence.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.25 at t0) because early dueling is mostly self-enforcing within the honor community; the constraint is genuinely coordinate. As state enforcement intensifies (t0–t150), extractiveness rises to 0.41 because the state begins imposing external cost, and the identity-lock deepens (participants cannot exit honor-frame). Theater ratio rises sharply (0.15→0.52) because the balance of activity shifts: early periods show functional duel-as-reputation-mechanism; later periods show increasing ritual performance (seconds arranging to avoid lethal outcomes, participants delaying or arranging amnesties while publicly honoring the code). Suppression rises from 0.12→0.38 as state enforcement intensifies but then plateaus, suggesting enforcement has reached saturation: further penalties add no marginal deterrent. The measurement grid is shared across all three metrics at every time point (one alignment rule).
 *
 * PERSPECTIVAL GAP:
 *   The honor-code society and the insulted gentleman perceive this constraint as legitimate even as it becomes costlier — honor remains the definition of self-worth, and the code remains the only legitimate mechanism. State apparatus perceives dueling as criminal and seeks to suppress it through cost (penalties, trials, execution). The insulting party and insulted gentleman both perceive themselves as trapped: honor-code defines honorable response, but state cost makes that response ruinous. Each seat computes a different type from the same structural data: the honor community reads it as coordination (rope); the state reads it as crime (snare); the trapped participants read it as extraction (tangled rope). The engine computes per-seat classification; the story's metrics support the tangled-rope reading from the participant seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The honor-code society is the beneficiary (collectivizes and legitimates reputation mechanism; membership confers standing). Insulted and insulting gentlemen are both payers (face mortal risk and legal jeopardy). The state is a payer (must enforce criminalization). Ecclesiastical authorities are payers (must condemn and excommunicate). The lower orders are excluded payers (bear social cost without honor-stake). The identity-lock on the insulted and insulting gentlemen is the core structural fact: they cannot exit the honor frame without ceasing to exist as gentlemen; the constraint owns their identity definition. This drives their d toward the full-target end despite their nominal power (moderate): they are trapped by self-conception, not external barriers alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insults are unenforceable in weak-law contexts; reputation is unanchored) is live in early periods (t0–t50) and contested by t150+. State prosecution and legal remedies (libel law, courts) offer alternative enforcement, but dueling persists because honor-codes treat legal remedy as insufficient — honor requires *private* violent satisfaction. The constraint does not become mandatrophy-resolved because the founding problem (need for honor-based reputation mechanism in weak-legal contexts) recedes as state legal institutions mature, but the honor-code persists as inertial legitimacy. The plateau in theater_ratio (0.52 at t300) indicates the constraint is not yet a pure piton: some functional reputation-enforcement remains alongside the theatrical performance. The reading distinguishes drop (legitimacy intact, frequency down from cost) from piton (legitimacy eroded, constraint is inertial). Mandatrophy is not yet present; the constraint remains a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_frequency_decoupling,
    'Does the drop in duel frequency represent a rejection of the honor-code legitimacy, or merely a rational response to rising external costs imposed by the state?',
    'Textual analysis of period correspondence and legal testimony: do participants cite honor-obligation and regret the necessity of avoiding duels (external cost framing), or do they cite changed understandings of honor and personal dignity (legitimacy redefinition)? Contraction-reading scholarship emphasizes changed honor-framing; drop-reading scholarship emphasizes cost-calculation by participants who still endorse honor-violence linkage.',
    'If the drop is cost-driven (drop reading), the constraint remains a tangled rope: legitimacy persists, frequency drops, theater ratio rises. If the drop reflects redefined legitimacy (contraction reading), the constraint becomes a piton: legitimacy has eroded, theater replaces function, the constraint is inertial. Classification depends on whether legitimacy itself shifted or merely the math of participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_frequency_decoupling, empirical, 'Whether duel decline reflects cost-rationality or legitimacy-erosion.').

omega_variable(
    honor_definition_stability,
    'Did the conceptual content of honor remain stable (honor = violent response to insult) while external costs rose, or did honor itself undergo redefinition to exclude violence?',
    'Comparative analysis of honor-code texts, judicial rhetoric, and literary representations at t0 vs. t_end. Drop reading holds that honor-definitions stayed constant (violence is still honorable, but cost-prohibitive) while contraction reading holds that honor was explicitly redefined (violence becomes dishonorable, honor relocates to legal/moral virtue). A core axiom divergence between the two readings.',
    'This is the boundary marker between drop reading and contraction reading. If honor-definition stayed stable, the constraint is a legitimate-but-costly tangled rope (drop). If honor-definition shifted, the constraint becomes a legitimacy-eroded piton (contraction). The two readings represent genuine alternatives; the evidence determines which is structurally accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_definition_stability, conceptual, 'Whether honor-code redefined violence as dishonorable, or merely became costly.').

omega_variable(
    state_enforcement_ratchet_vs_saturation,
    'Does the plateau in theater_ratio and suppression_requirement at t200+ indicate that enforcement has saturated (the maximum deterrent effect has been reached), or that the constraint is transitioning toward abandonment?',
    'Long-term trend continuation: if duel frequency continues declining toward zero despite flat enforcement, enforcement has saturated (further penalties add no marginal deterrent). If duels stabilize at a small positive frequency despite stable enforcement, the residual practice is identity-locked in subpopulations and will persist indefinitely unless legitimacy itself erodes (contraction-reading path).',
    'If saturation: the constraint is a stable tangled rope with a locked residual subpopulation. If abandonment trajectory: the constraint is transitioning to piton. The plateau in theater and suppression metrics suggests saturation; legitimacy-redefinition (contraction reading) would produce a different trajectory (theater spike upward as the mechanism becomes purely theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_ratchet_vs_saturation, empirical, 'Whether enforcement plateau indicates stabilization or transition to piton.').

omega_variable(
    reading_committer_ambiguity,
    'Is this constraint best understood under the drop reading (legitimacy stable, frequency drops from external cost) or the contraction reading (legitimacy eroded, honor redefined to exclude violence)?',
    'The two readings are coexisting positions held by different scholarly traditions and different participant cohorts. Drop reading is held by those emphasizing external cost as the explanation (state enforcement, legal liability, emigration opportunity); contraction reading is held by those emphasizing normative shift and legitimacy erosion (moral philosophy, psychological historians, those attending to honor-code rhetoric). Neither reading forecloses the other; both remain live. The measurement series authored here (plateau in theater_ratio and suppression, frequency decline with legitimacy framing intact) is consistent with drop reading; alternative measurements (spike in theater_ratio, explicit re-framing of honor-codes) would support contraction reading. A corpus with both readings linked via network.affects_constraints will enable the engine to measure which reading''s metrics align better with the historical record.',
    'The choice between readings determines the constraint''s terminal classification: drop reading → tangled rope; contraction reading → piton. This omega documents the irreducible interpretive choice that cannot be resolved by metrics alone; it belongs in the family network (both readings in one family, linked edges), not in a single constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'This constraint is one reading of a contested kernel; sibling readings coexist and are empirically non-distinguishable at the level of practice frequency alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__drop_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(hono_tr_t50, observed).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__drop_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(hono_tr_t100, observed).
narrative_ontology:measurement(hono_tr_t150, honor_violence_legitimacy__drop_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement_basis(hono_tr_t150, observed).
narrative_ontology:measurement(hono_tr_t200, honor_violence_legitimacy__drop_reading, theater_ratio, 200, 0.51).
narrative_ontology:measurement_basis(hono_tr_t200, observed).
narrative_ontology:measurement(hono_tr_t250, honor_violence_legitimacy__drop_reading, theater_ratio, 250, 0.52).
narrative_ontology:measurement_basis(hono_tr_t250, observed).
narrative_ontology:measurement(hono_tr_t300, honor_violence_legitimacy__drop_reading, theater_ratio, 300, 0.52).
narrative_ontology:measurement_basis(hono_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__drop_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement_basis(hono_be_t50, observed).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__drop_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(hono_be_t100, observed).
narrative_ontology:measurement(hono_be_t150, honor_violence_legitimacy__drop_reading, base_extractiveness, 150, 0.41).
narrative_ontology:measurement_basis(hono_be_t150, observed).
narrative_ontology:measurement(hono_be_t200, honor_violence_legitimacy__drop_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(hono_be_t200, observed).
narrative_ontology:measurement(hono_be_t250, honor_violence_legitimacy__drop_reading, base_extractiveness, 250, 0.41).
narrative_ontology:measurement_basis(hono_be_t250, observed).
narrative_ontology:measurement(hono_be_t300, honor_violence_legitimacy__drop_reading, base_extractiveness, 300, 0.41).
narrative_ontology:measurement_basis(hono_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__drop_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(hono_su_t50, observed).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__drop_reading, suppression_requirement, 100, 0.28).
narrative_ontology:measurement_basis(hono_su_t100, observed).
narrative_ontology:measurement(hono_su_t150, honor_violence_legitimacy__drop_reading, suppression_requirement, 150, 0.33).
narrative_ontology:measurement_basis(hono_su_t150, observed).
narrative_ontology:measurement(hono_su_t200, honor_violence_legitimacy__drop_reading, suppression_requirement, 200, 0.37).
narrative_ontology:measurement_basis(hono_su_t200, observed).
narrative_ontology:measurement(hono_su_t250, honor_violence_legitimacy__drop_reading, suppression_requirement, 250, 0.38).
narrative_ontology:measurement_basis(hono_su_t250, observed).
narrative_ontology:measurement(hono_su_t300, honor_violence_legitimacy__drop_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement_basis(hono_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor-violence-legitimacy kernel admits multiple readings. This constraint (drop reading) models dueling's persistence as driven by stable honor-code legitimacy and rising external costs. The contraction reading models persistence as driven by eroded legitimacy and theatrical inertia. The composite reading models both mechanisms. All three stories share the same historical domain but instantiate different structural causal explanations. The three readings coexist in scholarly discourse and in different participant cohorts' justifications; neither forecloses the other. Network edges enable the engine to measure which reading's metrics align better with the historical frequency and legitimacy-framing record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
