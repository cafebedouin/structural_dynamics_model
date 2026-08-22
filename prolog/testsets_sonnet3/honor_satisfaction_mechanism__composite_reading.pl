% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor-Satisfaction Duel as Composite-Mechanism Erosion (State Monopoly + Bourgeois Norms + Insurance + Category-Shift)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor-satisfaction duel — the socially legitimated, rule-bound combat
 *   between equals over questions of honor — did not vanish through any
 *   single cause. This reading holds that four structurally distinct
 *   mechanisms operated on it simultaneously and independently: (1) the
 *   centralizing state's claim to a monopoly on legitimate violence,
 *   criminalizing and prosecuting duels; (2) the rise of bourgeois
 *   professional norms that offered an alternative, non-lethal grammar of
 *   reputation defense (litigation, print, credit-worthiness) and simply
 *   out-competed the aristocratic code for social prestige; (3) the life
 *   insurance industry's actuarial repricing of dueling risk, which imposed a
 *   direct financial penalty on participation without any legal prohibition;
 *   and (4) a category-shift in legal and social description, whereby the
 *   duel was redescribed from 'honorable combat' to 'assault' or 'reckless
 *   homicide,' changing which institutions had jurisdiction over it at all.
 *   None of these four is individually sufficient to explain the near-total
 *   disappearance of dueling by the early twentieth century in the
 *   jurisdictions studied; the composite reading's distinguishing claim is
 *   that the interaction of independent pressures, not a single dominant
 *   cause, is the correct causal structure. As extraction rises across the
 *   interval, what is being measured is the compounding cost imposed on the
 *   dueling class by the accumulation of these four channels, not extraction
 *   by a single actor.
 *
 * KEY AGENTS:
 *   - centralizing_states: primary agenda-setter, absorbs adjudication authority
 *   - life_insurance_industry: independent economic mechanism, reprices dueling risk
 *   - bourgeois_professional_class: displaces the honor code's prestige function with an alternative norm-set
 *   - dueling_officer_class: primary payer, faces compounding pressure from all four mechanisms at once
 *   - aggrieved_parties_denied_traditional_recourse: bear the residual cost of an unreplaced grievance-processing function
 *   - legal_historians: analytical observers reconstructing the composite causal structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.61).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor-Satisfaction Duel as Composite-Mechanism Erosion (State Monopoly + Bourgeois Norms + Insurance + Category-Shift)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '05c5998b-4dc6-433c-a532-930bec3cc081').
narrative_ontology:cs_kernel_codification('05c5998b-4dc6-433c-a532-930bec3cc081', distributed).
narrative_ontology:cs_authority_grounding('05c5998b-4dc6-433c-a532-930bec3cc081', distributed).
narrative_ontology:cs_reading_relation('05c5998b-4dc6-433c-a532-930bec3cc081', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('05c5998b-4dc6-433c-a532-930bec3cc081', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('05c5998b-4dc6-433c-a532-930bec3cc081', foundational, causal_structure_is_irreducibly_plural).
narrative_ontology:cs_axiom_status(causal_structure_is_irreducibly_plural, holdable).
narrative_ontology:cs_axiom_grounding('05c5998b-4dc6-433c-a532-930bec3cc081', causal_structure_is_irreducibly_plural, empirically_contingent).
narrative_ontology:cs_axiom('05c5998b-4dc6-433c-a532-930bec3cc081', secondary, no_single_mechanism_is_individually_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_is_individually_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('05c5998b-4dc6-433c-a532-930bec3cc081', no_single_mechanism_is_individually_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('05c5998b-4dc6-433c-a532-930bec3cc081', aristocratic_equal_status_arbitration).
narrative_ontology:cs_drift_state('05c5998b-4dc6-433c-a532-930bec3cc081', early_twentieth_century_multi_jurisdiction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('05c5998b-4dc6-433c-a532-930bec3cc081', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, centralizing_states).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, life_insurance_industry).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, dueling_officer_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, military_honor_culture_adherents).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aggrieved_parties_denied_traditional_recourse).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, contractual_liability_as_replacement_for_honor_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalizes dueling and prosecutes participants (unevenly at first, then more consistently) as part of a broader project of claiming exclusive legitimate authority over lethal violence. Gains a monopoly on adjudicating disputes that previously bypassed courts entirely. Enforcement is one lever among several operating in this reading, not the sole cause.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, centralizing_states, agenda_setter,
    institutional, generational, analytical, national).

% Writes policies that exclude or heavily surcharge death-by-dueling, and later refuses to underwrite known duelists at all. This converts the honor code's core mechanism — willingness to risk death for satisfaction — into an actuarially priced, financially punished choice. Insurers collect premiums from the broader population while imposing a direct cost on the dueling class, reshaping incentives without ever writing a law.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, life_insurance_industry, beneficiary,
    organized, generational, arbitrage, national).

% Advances an alternative honor grammar — reputation defended through litigation, print polemic, professional standing, and commercial reliability — that displaces the aristocratic/officer code as the socially dominant model. This class does not need dueling and actively promotes its own norms (contracts, courts, credit-worthiness) as superior; their rise in status siphons prestige away from the dueling code without any single act of suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, agenda_setter).

% Faces the traditional code's demands (challenge or be dishonored) at the exact moment all four mechanisms compound: courts prosecute, insurers penalize, bourgeois rivals do not recognize the code's authority at all, and the entire category of 'honor requiring blood satisfaction' is being redescribed as criminal assault or reckless endangerment rather than a legitimate practice. Their traditional recourse is being closed from four directions simultaneously, and none of the four alone would have been decisive.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_officer_class, payer,
    powerful, biographical, constrained, national).

% Officers and cadets whose professional advancement and peer standing were historically bound to willingness to duel now face career discipline for participating, exclusion from insurance, and declining recognition from the civilian elite whose approval increasingly matters. They experience the erosion as several unrelated institutions each pulling support out from under the same practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, military_honor_culture_adherents, payer,
    moderate, biographical, trapped, regional).

% Individuals whose honor claims would formerly have been settled (however violently) through a socially legible ritual now have no equivalent mechanism — courts do not adjudicate 'insult,' insurance does not restore reputation, and the bourgeois alternative (litigation for slander) is slow, expensive, and requires resources they may lack. The old mechanism's disappearance leaves a genuine, unresolved grievance-processing gap for this group.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aggrieved_parties_denied_traditional_recourse, payer,
    powerless, immediate, constrained, local).

% Trace the interaction of criminal statutes, insurance underwriting records, etiquette and conduct literature, and shifting legal categorization (assault vs. duel) across multiple national jurisdictions, arguing that no single mechanism is sufficient to explain the timeline and that the composite interaction is itself the causal structure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its original form, the duel-of-honor coordinated status disputes among social equals by providing an agreed, rule-bound, mutually witnessed procedure that avoided endless private feud and mutual escalation. This reading holds that no single successor mechanism replaced that coordination function wholesale — instead four distinct institutional pressures independently reduced the scope in which the old procedure could operate.
% TRANSFER_FUNCTION: Legitimacy over the resolution of honor disputes is transferred away from the dueling class and toward: the state (which absorbs adjudication authority), the insurance industry (which absorbs and reprices the risk), and the bourgeois professional class (whose norms of contractual and reputational conduct absorb the social function). Aggrieved individuals without access to these substitute channels receive no equivalent transfer — the grievance-processing function is not fully replaced for them.
% ABSENT_VOICES: Aggrieved parties without money for litigation or standing before courts had no seat in any of the four displacing institutions — state prosecutors, insurers, and bourgeois arbiters of reputation all operated without consulting this group's actual need for a socially recognized satisfaction mechanism.
% DISAPPEARANCE_RATIONALE: If any single one of the four mechanisms (state prosecution, insurance penalty, bourgeois norm displacement, or legal recategorization) had not operated, the timeline and completeness of the duel's disappearance would differ measurably — the composite reading's central claim is that removing any one mechanism leaves residual dueling-code viability that the other three do not fully compensate for, so the arrangement of pressures genuinely matters to the outcome.
% FOUNDING_PROBLEM: The dueling code was originally built to give social equals (nobility, later officers and gentlemen) a legitimate, honor-preserving alternative to either submitting to insult or engaging in unregulated violence or feud.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians studying court records, insurance underwriting archives, and etiquette manuals across multiple jurisdictions independently corroborate that by the early twentieth century the social conditions the code addressed (aristocratic equals without state-backed dispute resolution) had been supplanted by state courts, actuarial risk-pricing, and bourgeois reputational institutions; no advocacy literature internal to the dueling class survives making a serious claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored moderate-to-substantial (0.61 by interval end) because this reading treats the erosion as extraction of social standing and functional recourse from the dueling class distributed across four separate institutional channels, none of which alone would be decisive — the sum is what closes off the practice. Suppression (0.58) reflects genuine state coercive capacity (prosecution) layered with non-coercive but equally effective displacement (insurance pricing, norm competition, redescription) — this is why suppression sits meaningfully below what a pure state-monopoly reading would claim, since three of the four mechanisms operate without direct coercion. Theater ratio rises over the interval (0.10 to 0.42) reflecting that late-stage anti-dueling laws in some jurisdictions were enforced more for symbolic/legitimacy reasons (the practice was already rare) than as the operative cause of decline — a genuinely mixed function/performance profile appropriate to a piton-adjacent read of the residual legal apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the dueling officer class's seat, this looks like an inexplicable, multi-front collapse of a previously stable status system — no single villain, just simultaneous defection by the state, the insurers, and the rising bourgeoisie. From the state's seat, this looks like successful consolidation of a violence monopoly it had wanted for other reasons entirely (dueling suppression was a byproduct of broader state-building, not the target). From the insurance industry's seat, this was never about honor at all — it was actuarial risk management that happened to have this social effect. The composite reading insists all three seats are correctly describing genuine partial mechanisms; none is the whole story, and none should be flattened into the others.
 *
 * DIRECTIONALITY LOGIC:
 *   Centralizing states, the insurance industry, and the bourgeois professional class are coded as beneficiaries/agenda-setters because each independently gains something structural from the duel's decline (state authority, insurance premium bases undisturbed by uninsurable risk-takers, bourgeois norm dominance) even though none of them coordinated with the others to produce the outcome. The dueling officer class and military honor-culture adherents are coded as targets because the compounding pressure specifically closes off their traditional recourse without providing an equivalent substitute at their power level. Aggrieved parties without resources for litigation are the clearest residual victims: the composite mechanisms replace the function for elites (who can litigate, insure, professionalize) but leave a gap for those without comparable resources — this is the strongest argument for real, not merely apparent, extraction in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a legitimate dispute-resolution ritual for social equals absent effective state courts) is dead by any of the composite mechanisms' own logic — state courts now exist, insurance and litigation offer alternative channels for elites, and the aristocratic status system the duel policed has itself been substantially displaced. Classifying this as piton rather than snare or mountain matters: no single beneficiary is extracting rents from a captured population (ruling out snare), and it is manifestly not a natural law (ruling out mountain) — but the residual legal apparatus criminalizing dueling persists today in most jurisdictions as an inert, rarely-invoked statute maintained mostly for legitimacy/completeness reasons (rising theater_ratio), while the underlying function (need for aristocratic status-equal dispute resolution) has genuinely disappeared. This resolves the mandatrophy question: the mandate has outlived the problem, and what remains is institutional inertia, not active extraction or active coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence_vs_common_cause,
    'Are the four mechanisms (state monopoly, bourgeois norms, insurance, category-shift) genuinely independent causal channels, or are they all downstream expressions of a single deeper cause (e.g., the general rise of bureaucratic-rational modernity) that would make the ''composite'' framing an artifact of granularity rather than a true multi-mechanism structure?',
    'Comparative historical analysis across jurisdictions where the four mechanisms arrived in different orders or at different speeds (e.g., a jurisdiction with early insurance markets but late state criminalization) — if the duel''s decline tracks the mechanisms'' actual arrival timing independently in each case, independence is supported; if decline timing tracks a single common variable regardless of which mechanisms are present, common-cause is supported.',
    'If the mechanisms are downstream of a single common cause, this composite_reading collapses toward the decline_reading or a modernity-monocausal reading, and the ''four independent mechanisms'' framing becomes descriptive rather than causally load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_vs_common_cause, conceptual, 'Whether the four named mechanisms are causally independent or expressions of one deeper process.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between composite_reading, decline_reading, and contraction_reading actually live — is it a disagreement about facts (what happened, in what order), about causal structure (how many independent mechanisms operated), or about the correct level of description (frequency counts vs. cognitive category-status vs. institutional mechanism inventory)?',
    'Explicit cross-reading comparison exercise: hold the same historical dataset (dueling frequency records, prosecution records, insurance archives, etiquette literature) constant and ask each reading''s proponents to identify which specific claims they would revise given the same evidence.',
    'If the disagreement is purely one of description level rather than fact or causal structure, the three readings may be less in tension than the kernel framing suggests, and could in principle be merged into a single multi-level story rather than three siblings — though per the ε-invariance principle this would still require justifying why they should share one ε rather than three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the sibling readings disagree about facts, causal structure, or descriptive level.').

omega_variable(
    residual_grievance_gap_severity,
    'How severe and how persistent was the gap in grievance-processing left for aggrieved parties without access to litigation, insurance, or bourgeois reputational institutions — did this population develop adequate substitute mechanisms (informal community sanction, changing norms of what counts as an actionable insult), or did the gap represent a genuine, unaddressed loss of recourse?',
    'Social history research into informal dispute resolution among non-elite populations in the same period and jurisdictions, examining whether alternative honor-restoration mechanisms (public shaming, community arbitration, changing insult norms) emerged to fill the gap.',
    'If adequate substitutes emerged, the victim coding for aggrieved_parties_denied_traditional_recourse should be softened; if no substitute emerged, this strengthens the claim that the composite mechanisms'' erosion of the duel was extractive with respect to this specific population, not merely neutral institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_grievance_gap_severity, empirical, 'Whether non-elite populations found adequate substitutes for the honor-satisfaction function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__composite_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__composite_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_mechanism__composite_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_mechanism__composite_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 100, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_satisfaction_mechanism kernel, each with its own ε, stakeholder structure, and classification per the ε-invariance principle. decline_reading treats the duel's disappearance as gradual frequency attrition (a Rope/Piton drift toward disuse); contraction_reading treats it as an abrupt category-level cognitive foreclosure (dueling became unthinkable rather than merely rare); this composite_reading treats it as the joint product of four analytically distinct mechanisms (state monopoly, bourgeois norms, insurance repricing, legal category-shift) whose interaction, not any single cause, produced the outcome. All three are linked via affects_constraints; none is the 'correct' reading and none averages the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
