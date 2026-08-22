% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Satisfaction Code — Practice-Decline Reading (Exogenous Suppression of Dueling)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the practice_decline_reading of the
 *   honor_satisfaction_substrate kernel: the claim that the honor code's
 *   normative content did NOT foundationally transform, but rather persisted
 *   intact as a background substrate while dueling as a PRACTICE was
 *   suppressed by exogenous forces — criminal prohibition, civil liability,
 *   dueling-specific statutes, and the rising opportunity cost of risking
 *   death or imprisonment as commercial and professional life expanded. On
 *   this reading, the code survives recognizably in attenuated forms:
 *   military courts of honor, dueling-adjacent codes of conduct, and regional
 *   'cultures of honor' (notably in the American South) that retain
 *   heightened insult-sensitivity and retaliation expectations without the
 *   dueling mechanism itself. This is a rope-type failure under this reading,
 *   not mountain-style erosion: the underlying coordination problem (how do
 *   status-equals resolve reputational disputes without either
 *   courts-they-distrust or unregulated violence) was never solved by
 *   delegitimating the code's premises — it was suppressed by raising the
 *   cost of the traditional mechanism faster than an alternative could
 *   organically displace the norms that generated demand for it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.42).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Code — Practice-Decline Reading (Exogenous Suppression of Dueling)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '3f518d23-aa90-42a6-a7cc-0cfe0f638bbc').
narrative_ontology:cs_kernel_codification('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', distributed).
narrative_ontology:cs_authority_grounding('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', practice).
narrative_ontology:cs_interpretation_layer_present('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc').
narrative_ontology:cs_reading_relation('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', foundational, code_content_invariant_under_practice_suppression).
narrative_ontology:cs_axiom_status(code_content_invariant_under_practice_suppression, holdable).
narrative_ontology:cs_axiom_grounding('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', code_content_invariant_under_practice_suppression, empirically_contingent).
narrative_ontology:cs_axiom('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', foundational, exogenous_enforcement_sufficient_for_practice_decline).
narrative_ontology:cs_axiom_status(exogenous_enforcement_sufficient_for_practice_decline, holdable).
narrative_ontology:cs_axiom_grounding('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', exogenous_enforcement_sufficient_for_practice_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', elite_status_defense_via_ritualized_combat).
narrative_ontology:cs_drift_state('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', post_prohibition_institutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3f518d23-aa90-42a6-a7cc-0cfe0f638bbc', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, gentry_status_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_culture_regions).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, families_of_duel_casualties).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, socially_subordinate_men_excluded_from_satisfaction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically used the honor code and the threat of the duel to police status boundaries and settle disputes over reputation without appeal to courts they regarded as beneath their class. As dueling becomes legally and practically foreclosed, they retain the underlying code — the vocabulary of insult, satisfaction, and reputational stake — but discharge it through litigation, dueling-adjacent ritual (military codes of conduct, formal apology rituals), or simple avoidance, without abandoning the normative substrate itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, gentry_status_claimants, beneficiary,
    powerful, generational, constrained, national).

% Retains honor-code logic formally in codes of conduct, courts of honor, and expectations around dueling-adjacent conduct (challenges to duty, personal courage, willingness to answer insult) long after actual dueling is prohibited. The institution administers what remains of the code and could relax it further, but treats the residual honor substrate as functionally useful for esprit de corps and discipline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, agenda_setter).

% Individuals who, prior to and during the decline, faced genuine physical risk and legal jeopardy for participating in duels demanded by the honor code's logic — refusal meant social death, participation meant death or prosecution. As legal prohibition and opportunity cost mount, exit from an individual duel becomes easier, but exit from the code's judgment of them (as a coward, as socially diminished) remains harder; the code's persistence as substrate is what keeps this cost real even as the practice becomes rare.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, dueling_participants, payer,
    moderate, biographical, trapped, regional).

% Bore the direct, irreversible costs (death, injury, loss of a breadwinner) of duels fought under a code they typically had no voice in setting. Legal prohibition arrives too late for those already affected, and even after prohibition reduces incidence, the residual honor substrate continues to validate the underlying logic that produced their loss.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, families_of_duel_casualties, payer,
    powerless, biographical, trapped, local).

% Men outside the gentleman class were historically denied standing to demand or receive 'satisfaction' under the honor code at all — insults from social superiors were simply absorbed. The exogenous decline of dueling does not restore this exclusion's cost retroactively; it merely removes one mechanism (formal challenge) that was never available to them in the first place, while the honor code's status hierarchy persists in attenuated social judgment.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, socially_subordinate_men_excluded_from_satisfaction, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, socially_subordinate_men_excluded_from_satisfaction, excluded).

% Regional populations (e.g., the American South) where the honor code persists as a recognizable normative substrate — heightened sensitivity to insult, valorization of reputation defense, expectation of retaliatory response — long after the duel itself has vanished as a practice. Legal prohibition and urbanization removed the practice's viability; the underlying normative expectations persist as regional culture, identity-bound rather than practice-bound.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_culture_regions, beneficiary,
    organized, civilizational, identity_locked, regional).

% Criminalized dueling, prosecuted participants and seconds, and built alternative institutional channels (civil courts, professional grievance boards) for dispute resolution. This is the exogenous suppression mechanism this reading centers: legal prohibition drove practice decline directly, independent of whether the honor code's own legitimacy eroded.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Study the divergence between practice decline (measurable in dueling incident counts, legal records) and normative persistence (measurable in surviving codes of conduct, regional culture-of-honor studies). This reading is their analytical claim: the two are separable, and separation is evidenced by codes surviving where practice cannot.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinated dispute resolution and status verification among status-equals without recourse to courts perceived as illegitimate or beneath the parties' dignity; it substituted a mutually recognized ritual (challenge, seconds, terms, satisfaction) for either violence-without-rules or capitulation.
% TRANSFER_FUNCTION: Under the code, reputational capital and physical risk were transferred: a successful or merely willing challenger gained standing, a refuser lost it, and the risk of death or injury was moved onto the participants themselves (and, derivatively, onto their dependents) rather than being resolved through third-party arbitration.
% ABSENT_VOICES: Socially subordinate men who could never issue or receive a formal challenge, and the families of the dead and injured, had no voice in either the code's original design or in the timing/terms of its practical decline — the decline was driven by state actors and elite opportunity cost, not by these excluded parties gaining leverage.
% DISAPPEARANCE_RATIONALE: If the residual honor substrate vanished overnight, dueling itself would not return (it is already suppressed by law and opportunity cost, independent of the substrate) — so at the level of practice, the world is largely unchanged. But military codes of conduct, courts of honor, and regional culture-of-honor social dynamics would visibly rearrange: officer corps discipline mechanisms and Southern interpersonal norms around insult and retaliation would lose their organizing logic. The verdict is contested because the practice layer and the normative-substrate layer respond differently to the counterfactual.
% FOUNDING_PROBLEM: Pre-modern elites lacked a trusted, status-appropriate mechanism to resolve accusations of dishonesty, cowardice, or insult without either capitulating (losing status) or resorting to unregulated violence; the duel with its honor code provided a rule-bound, mutually recognized channel.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state prosecution records from the 19th century attest that alternative dispute-resolution channels (civil defamation law, criminal assault law, professional grievance mechanisms) fully supplanted the duel's original function; military historians studying courts of honor corroborate that the residual code now serves institutional discipline and identity maintenance rather than actual grievance resolution, a status attested independently of the honor-culture beneficiaries themselves.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, declining slowly over the interval, because on THIS reading the underlying normative machinery (status defense as legitimate demand) remains largely constant in its distributive logic even as its violent expression becomes rare — the code keeps extracting reputational conformity and risk-bearing expectation from participants even where the duel itself no longer occurs. Theater ratio rises sharply (0.15 to 0.55) because as practice becomes legally and practically foreclosed, an increasing share of what remains of the honor code is performative — courts of honor, ceremonial codes of conduct, regional rhetoric of honor — without the underlying life-and-death stakes that once made the code's demands consequential. Suppression rises through the interval (0.25 to 0.58) tracking increasing legal prohibition, prosecution, and institutional barriers (military discipline codes explicitly banning dueling, civil liability regimes) — this is the exogenous enforcement mechanism this reading claims is doing the causal work, not endogenous delegitimation of the code's premises.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry status claimants, the officer corps, and honor-culture regions are coded as beneficiaries because the code's persistence as substrate continues to serve their status-maintenance and institutional-discipline functions even after the duel itself declines — they experience low-to-moderate directionality toward this constraint (it subsidizes their social position). Dueling participants, families of casualties, and socially subordinate men excluded from satisfaction are coded as payers/victims: participants bore direct mortal risk under the code's logic, families of casualties bore irreversible loss, and excluded subordinate men bore a status-hierarchy cost that the code's decline as PRACTICE never addressed because they were never covered by the satisfaction mechanism to begin with. State legal authorities are the exogenous suppression agent central to this reading's causal claim — their prohibition activity is what drives decline on this account, independent of any change in the code's own legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a trusted, status-appropriate dispute-resolution channel outside courts perceived as illegitimate) is dead by 1900 on this reading's own terms — civil and criminal law fully displaced the duel's practical function. Yet the honor code substrate persists in courts of honor, military codes, and regional culture. This is not classified as pure mandatrophy resolution because the code was never a Mountain or a genuinely functionless Piton in this reading's account: it is a Rope whose coordination function was actively suppressed by external enforcement rather than organically obsolesced. The distinction matters for how the corpus should treat 'zombie' honor codes — this reading holds they are theater riding on a substrate that could, in principle, reactivate its practical form if suppression relaxed, which is structurally different from a code whose underlying legitimacy collapsed (the cultural_contraction_reading's claim) or from a code whose decline had no single dominant cause (the composite_overdetermined_reading's claim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_versus_residue_test,
    'Is the honor code that persists in military codes and Southern culture-of-honor regions a LIVE normative substrate capable of reactivating dueling-like practices if suppression relaxed, or is it inert residue that would not reconstitute the practice even absent legal prohibition?',
    'Compare regions/institutions where legal prohibition weakened or enforcement lapsed against regions where it remained strict, controlling for opportunity cost; if practice-adjacent violence (informal duels, honor killings, retaliatory violence) rebounds where suppression lapses independent of code-content change, the substrate is live and this reading is supported. If practice does not rebound even where suppression lapses, the cultural_contraction_reading''s delegitimation claim gains support instead.',
    'If the substrate is shown to be live, this reading''s rope classification (coordination function surviving, suppressed by exogenous force) holds. If the substrate is shown to be inert residue, the constraint is better classified as piton (a fossilized coordination mechanism with no reactivatable function) and the cultural_contraction_reading becomes the better-supported account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_versus_residue_test, empirical, 'Whether the surviving honor code is a reactivatable coordination function or dead residue.').

omega_variable(
    counterfactual_legalization_test,
    'If dueling prohibition and criminal liability were hypothetically repealed today in a jurisdiction with a strong residual honor culture (e.g., parts of the American South), would dueling practice meaningfully return?',
    'This is inherently counterfactual and cannot be directly tested, but proxy evidence exists in the persistence of informal ''honor violence'' (bar fights, retaliatory shootings framed in honor terms) in culture-of-honor regions even under current prohibition — suggesting partial reactivation already occurs at the margins where enforcement is weakest.',
    'Strong proxy evidence for reactivation at the margins would support this reading''s central causal claim (exogenous suppression, not endogenous delegitimation, is doing the work); weak or absent proxy evidence would favor sibling readings that locate the causal weight in the code''s own transformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_legalization_test, conceptual, 'Counterfactual test of whether legal suppression alone accounts for decline.').

omega_variable(
    which_reading_is_true_kernel_indeterminacy,
    'Is the decline of dueling structurally better explained by exogenous suppression alone (this reading), endogenous cultural transformation alone (cultural_contraction_reading), or an overdetermined combination with non-separable causal pathways (composite_overdetermined_reading)?',
    'This is the core kernel contest and is not resolvable within a single reading''s own framework — it requires comparative historical analysis across all three readings, examining whether legal prohibition timing precedes, follows, or is contemporaneous with measurable shifts in elite attitudes toward violence and status.',
    'Determines which of the three sibling constraint stories in this kernel family best represents the historical record; each reading commits to a different beneficiary/victim structure and a different classification (rope for this reading, mountain-erosion-adjacent for the cultural_contraction_reading, and an irreducibly mixed classification for the composite reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_true_kernel_indeterminacy, conceptual, 'The unresolved kernel-level dispute among the three readings of honor code decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1780, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1780, 0.15).
narrative_ontology:measurement(hono_tr_t1820, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1820, 0.22).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1860, 0.35).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(hono_tr_t1940, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1940, 0.5).
narrative_ontology:measurement(hono_tr_t1980, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1980, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1780, 0.55).
narrative_ontology:measurement(hono_be_t1820, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1820, 0.52).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1860, 0.48).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(hono_be_t1940, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1940, 0.43).
narrative_ontology:measurement(hono_be_t1980, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1980, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1780, 0.25).
narrative_ontology:measurement(hono_su_t1820, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1820, 0.4).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(hono_su_t1940, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement(hono_su_t1980, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1980, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the honor_satisfaction_substrate kernel. practice_decline_reading (this story) claims exogenous legal/institutional suppression drove decline while the code's normative content persisted essentially unchanged (rope classification: coordination function suppressed by external force). cultural_contraction_reading claims the code itself underwent foundational transformation, making dueling unthinkable rather than merely impractical. composite_overdetermined_reading claims both mechanisms operated simultaneously with non-separable causal pathways. All three share the same kernel (the honor-satisfaction normative complex and its relationship to the historical decline of dueling) but author different ε, different beneficiary/victim structures, and different classifications, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
