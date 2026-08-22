% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor-Satisfaction Substrate — Composite Overdetermined Decline Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The historical decline of the aristocratic dueling code and its
 *   underlying honor-satisfaction substrate is read here as a case of genuine
 *   causal overdetermination: state legal suppression (criminalization,
 *   prosecution, loss of civil standing) and internal transformation of the
 *   honor code itself (the shift from honor cultures organized around
 *   willingness to duel toward dignity cultures organized around inherent
 *   worth and legal recourse) operated concurrently across the eighteenth
 *   through early twentieth centuries in Western Europe and the United
 *   States, and neither mechanism alone plausibly accounts for the observed
 *   decline curve or its timing across jurisdictions with different legal
 *   enforcement intensity.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class: primary agenda-setter and early beneficiary, later ambivalent — (powerful/identity_locked)
 *   - dueling_code_arbiters: administer the ritual, hollow into theater as substrate erodes — (organized/constrained)
 *   - state_legal_authorities: exogenous suppressor and secondary beneficiary of jurisdiction consolidation — (institutional/arbitrage)
 *   - dueling_participants: primary target bearing mortal and legal risk simultaneously — (moderate/trapped)
 *   - bourgeois_professional_class: payer and unwitting endogenous solvent of the honor substrate — (moderate/constrained)
 *   - women_and_dependents_of_duelists: fully excluded payer bearing derivative cost — (powerless/trapped)
 *   - social_historians: analytical observers reconstructing the entangled causal record — (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor-Satisfaction Substrate — Composite Overdetermined Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '87ca3737-4aa2-4e89-8b5c-5523705493c6').
narrative_ontology:cs_kernel_codification('87ca3737-4aa2-4e89-8b5c-5523705493c6', distributed).
narrative_ontology:cs_authority_grounding('87ca3737-4aa2-4e89-8b5c-5523705493c6', practice).
narrative_ontology:cs_interpretation_layer_present('87ca3737-4aa2-4e89-8b5c-5523705493c6').
narrative_ontology:cs_reading_relation('87ca3737-4aa2-4e89-8b5c-5523705493c6', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('87ca3737-4aa2-4e89-8b5c-5523705493c6', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('87ca3737-4aa2-4e89-8b5c-5523705493c6', foundational, causal_non_independence_of_suppression_and_delegitimation).
narrative_ontology:cs_axiom_status(causal_non_independence_of_suppression_and_delegitimation, holdable).
narrative_ontology:cs_axiom_grounding('87ca3737-4aa2-4e89-8b5c-5523705493c6', causal_non_independence_of_suppression_and_delegitimation, empirically_contingent).
narrative_ontology:cs_axiom('87ca3737-4aa2-4e89-8b5c-5523705493c6', secondary, single_mechanism_accounts_are_structurally_incomplete).
narrative_ontology:cs_axiom_status(single_mechanism_accounts_are_structurally_incomplete, holdable).
narrative_ontology:cs_axiom_grounding('87ca3737-4aa2-4e89-8b5c-5523705493c6', single_mechanism_accounts_are_structurally_incomplete, conventional).
narrative_ontology:cs_reference_frame('87ca3737-4aa2-4e89-8b5c-5523705493c6', elite_self_regulated_honor_tribunal_system).
narrative_ontology:cs_drift_state('87ca3737-4aa2-4e89-8b5c-5523705493c6', post_state_judicial_consolidation_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('87ca3737-4aa2-4e89-8b5c-5523705493c6', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_code_arbiters).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_authorities).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_professional_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, women_and_dependents_of_duelists).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, overdetermination_of_institutional_collapse).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, non_independence_of_causal_pathways_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and polices the honor code that makes dueling both meaningful and obligatory for members of the class. Their standing depends on being seen as willing to answer an insult with a challenge; the code is simultaneously a coordination mechanism (settling disputes without escalation to blood feud or lawsuit) and a status-extraction device that punishes any member who declines to participate. As the code's substrate erodes (honor increasingly indexed to dignity/rule-of-law norms rather than willingness to duel), the class's own investment in the practice becomes a liability rather than an asset.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class, agenda_setter,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class, beneficiary).

% Seconds, codifiers of dueling manuals, and informal tribunals of honor administer the rules that make dueling a regulated ritual rather than chaotic violence. Their function persists only as long as both the honor substrate and the absence of effective legal suppression hold; as courts and legislatures criminalize dueling and honor itself is redefined around legal/professional standing, the arbiters' role hollows into theater — presiding over an increasingly symbolic and rare event.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_code_arbiters, agenda_setter,
    organized, biographical, constrained, national).

% Legislatures and courts criminalize dueling, prosecute survivors, and strip honors from participants, asserting a monopoly on legitimate violence and offering statutory courts as the sanctioned venue for grievance resolution. They benefit doubly: consolidating state authority over interpersonal violence AND riding the wave of an independently occurring honor-code transformation that makes their prohibitions culturally credible rather than merely coercive.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_authorities, beneficiary).

% Individual men (nearly always men) compelled by the honor code to issue or accept challenges on pain of social death, bear the literal mortal risk of the ritual, and increasingly also bear legal risk (prosecution, loss of office, exile) once the state criminalizes the practice. Their exit is doubly foreclosed during the transition period: refusing a challenge invites social ruin under the still-operative honor code, while accepting invites prosecution under the newly assertive legal regime — the overdetermined double bind is felt most acutely here.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants, payer,
    moderate, immediate, trapped, local).

% Rising professional and commercial classes are pressured to adopt aristocratic honor codes to claim social parity, absorbing the costs (risk, time, reputational stakes) of a system built for a different class's economy of status, while having comparatively little influence over the code's rules or its eventual delegitimation. Their gradual embrace of dignity-based professional norms (contracts, courts, credentialing) is itself one of the endogenous solvents dissolving the honor substrate from within.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_professional_class, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_professional_class, excluded).

% Wives, children, and other dependents bear the material and emotional cost of a duelist's death or injury, and the reputational cost of a duelist's cowardice, without any voice in whether the challenge is issued or accepted. They have no standing in the honor code's adjudication and no standing before the courts that later criminalize the practice; their interests appear nowhere in either the coordination story or the suppression story.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, women_and_dependents_of_duelists, payer,
    powerless, biographical, trapped, local).

% Reconstruct the decline of dueling from statute books, honor-code manuals, court records, and dueling statistics, and debate whether legal suppression or cultural delegitimation was the primary driver — or, as this reading holds, whether the two were entangled rather than separable causal streams.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor-satisfaction code coordinated interpersonal violence away from unregulated blood feud and toward a bounded, rule-governed ritual with seconds, agreed weapons, and a recognized point of resolution — a genuine reduction in uncontrolled retaliatory violence relative to the absence of any code.
% TRANSFER_FUNCTION: The arrangement transfers physical risk and mortality onto individual duelists (disproportionately non-elite entrants seeking status parity), transfers unacknowledged cost onto dependents with no standing in the code, and transfers legitimacy and jurisdiction from informal honor tribunals to the state as legal suppression intensifies — with the state and the honor class each capturing different forms of authority from the same declining substrate.
% ABSENT_VOICES: Women and dependents of duelists are structurally absent from both the honor code's adjudicative machinery and the legislative debates that eventually suppress it; their interests are invoked rhetorically (protecting families, protecting widows) by both the honor class and the suppressing state without their direct participation.
% DISAPPEARANCE_RATIONALE: The honor-satisfaction substrate's disappearance did rearrange the world: elite dispute resolution shifted permanently to courts, libel law, and dueling's stigmatization made prior notions of masculine honor unavailable as social capital, and status competition among elites migrated to other symbolic markets (wealth display, professional credentialing, political office). The composite reading holds that this rearrangement was driven jointly by legal suppression and by the honor code's own internal transformation, and that removing either mechanism alone would not have produced the same outcome on the same timeline.
% FOUNDING_PROBLEM: The honor code and its dueling ritual were built to provide elites a bounded, socially legible mechanism for resolving insults to reputation without recourse to either chaotic private violence or a state judiciary elites did not fully trust or control.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining 19th-century court records and legislative debates attest that dueling prosecutions rose as state judicial capacity and legitimacy grew, corroborating from outside the honor class that the original coordination problem (absence of a trusted third-party forum) had been substantially solved by the state well before the code's final collapse; sociologists of honor culture (independently of legal historians) attest that surviving correspondence and etiquette literature show elites themselves describing dueling as archaic and socially embarrassing decades before its final legal suppression, corroborating the endogenous delegitimation thread from a source with no stake in the legal-suppression narrative.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.58: the arrangement genuinely coordinated elite dispute resolution (real coordination function) while extracting mortal risk from participants and total exclusion from dependents — a hybrid profile appropriate to tangled_rope rather than pure snare. Suppression rises over the interval (0.30 to 0.62) tracking the maturation of state legal machinery targeting dueling specifically, while theater_ratio also rises (0.10 to 0.40) reflecting that by the late period, surviving duels and honor tribunals had become increasingly ceremonial and socially marginal even where not yet formally illegal — consistent with the composite reading's claim that both mechanisms degraded the practice's substance simultaneously rather than one being a pure residue of the other.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the aristocratic honor class in the arrangement's early period, dueling reads as a live coordination mechanism defending real status stakes; from the same seat a generation later, the identical residual practice reads as embarrassing anachronism — a shift the composite reading attributes jointly to legal risk recalculation and to independent honor-to-dignity cultural drift, which is precisely the non-independence the kernel context specifies.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic honor class and dueling-code arbiters sit near the beneficiary end while the code retains status-conferring power, but their position degrades over the interval as the code's value collapses — this is a beneficiary group whose own directionality shifts within the story's timeframe, which the composite reading treats as evidence the two decline mechanisms are entangled rather than independent (their loss of benefit from the code is itself partly endogenous re-evaluation, partly a rational response to rising legal risk). Dueling participants and dependents sit at the target end throughout, bearing mortal and reputational risk under a code they did not design and, for dependents, could not contest at all. State legal authorities occupy an unusual dual position: institutional beneficiaries of the practice's suppression (consolidating jurisdiction) who simultaneously benefit from delegitimation happening independently of their own enforcement effort, since a culturally discredited practice is far cheaper to suppress than a culturally vital one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (a trusted, bounded mechanism for resolving reputational disputes among elites who did not trust state courts) is corroborated as dead by sources outside the honor class itself — legal historians and independent sociologists of honor culture both attest, from different evidentiary bases, that the problem had been substantially resolved by rising state judicial legitimacy well before the code's final collapse. This composite reading resists two mandatrophy mislabelings symmetrically: it does not let the practice_decline_reading's claim of a persisting honor substrate excuse the practice as still functionally necessary once suppressed, nor does it let the cultural_contraction_reading's claim of pure endogenous obsolescence obscure the real coercive machinery (criminal prosecution, civil disqualification) that was built and actively deployed against duelists. Overdetermination cuts against declaring either mechanism alone sufficient — a single-cause story in either direction would misclassify the arrangement's true entangled structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_non_independence_mechanism,
    'Through what specific mechanism did legal suppression and honor-code delegitimation reinforce rather than merely coincide with each other?',
    'Comparative jurisdictional analysis: compare decline timelines in regions with strong legal suppression but weak dignity-culture uptake (or vice versa) against regions with both; if decline speed tracks the interaction term rather than either factor alone, non-independence is empirically supported.',
    'If the mechanisms turn out to be independent and merely coincident, this composite reading collapses into a weighted average of the two sibling readings rather than a structurally distinct third reading; if genuinely entangled, the composite reading captures something the siblings individually miss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_non_independence_mechanism, empirical, 'Whether suppression and delegitimation were causally entangled or merely temporally coincident.').

omega_variable(
    cs_framing_kernel_or_institution,
    'Is the correct commitment-system kernel the honor code itself (a fixed, transmitted normative text/practice) or the state''s monopoly-on-violence claim that the honor code''s decline vindicates?',
    'Trace which authority structure''s own internal documents (dueling manuals vs. legislative debate records) more directly address the legitimacy of the OTHER framing — whichever framing is treated as the live contested ground by contemporaneous actors is the better-supported kernel referent.',
    'Framing the kernel as the honor code favors reading this constraint''s authority_grounding as practice/lineage (declining through internal drift); framing the kernel as state authority favors authority_grounding as extraction (the state capturing jurisdiction). This story adopts the honor-code framing as primary because the kernel contest as declared centers on dueling''s disappearance, not the state''s legitimacy — but the alternative framing would shift emphasis toward state-authority CS dynamics rather than honor-practice CS dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_or_institution, conceptual, 'Alternative kernel framing: honor-code-as-kernel versus state-authority-as-kernel, and which the story privileges.').

omega_variable(
    class_differential_delegitimation_speed,
    'Did the honor code delegitimate at the same rate across aristocratic and bourgeois adopters, or did the rising professional class''s earlier embrace of dignity/legal norms accelerate delegitimation among elites who initially had no independent reason to abandon the code?',
    'Compare dueling-frequency decline curves by class of participant within the same jurisdiction and period.',
    'If bourgeois adoption of dignity norms preceded and accelerated aristocratic abandonment, the ''endogenous'' delegitimation thread is itself partly downstream of a class dynamic exogenous to the honor class proper — deepening the non-independence claim in an unexpected direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_differential_delegitimation_speed, empirical, 'Whether cross-class diffusion of dignity norms was itself a driver of the honor class''s own delegitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_substrate kernel. practice_decline_reading treats the honor code as a persisting mountain-like substrate eroded purely by exogenous legal suppression (rope-breaking without mountain erosion). cultural_contraction_reading treats the honor code's internal transformation (honor culture to dignity culture) as sufficient on its own (mountain erosion without needing rope-breaking as an independent cause). This composite_overdetermined_reading claims neither sibling is independently sufficient: the two mechanisms are causally entangled such that removing either changes the timeline and character of the other. All three share the same referent arrangement (the aristocratic dueling/honor-satisfaction system) but author different causal-sufficiency claims and therefore different epsilon trajectories and different beneficiary/victim emphasis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
