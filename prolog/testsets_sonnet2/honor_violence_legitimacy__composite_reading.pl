% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor-Violence Legitimacy Norm (Composite Decline Reading: External Cost + Conceptual Contraction)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the COMPOSITE reading of the
 *   honor_violence_legitimacy kernel: dueling's decline as a legitimate
 *   institution resulted from two structurally distinct mechanisms operating
 *   simultaneously and jointly, neither sufficient alone. The 'drop'
 *   mechanism (rising external costs — criminal prosecution,
 *   insurance/financial ruin, career risk) made dueling practically rarer
 *   while leaving its legitimacy formally intact. The 'contraction' mechanism
 *   (semantic redefinition of honor itself, from a category that included
 *   violent vindication to one built on Christian/bourgeois self-restraint)
 *   made dueling conceptually unthinkable independent of its practical cost.
 *   This reading holds that the historical record — dueling declining
 *   unevenly across jurisdictions with different legal enforcement
 *   intensities and different definitional trajectories of honor — is only
 *   explained when both mechanisms are modeled together; the drop-only
 *   account cannot explain persistence in jurisdictions with weak enforcement
 *   but contracted honor discourse, and the contraction-only account cannot
 *   explain the sharp correlation between prosecution intensity and decline
 *   timing in others. ε is authored for the standing arrangement
 *   (honor-violence legitimacy as codified and enforced by arbiters and the
 *   state) as this composite reading sees it — moderate-high extraction
 *   accruing to arbiters and incumbents who retain reputational and
 *   adjudicative capital across both regimes, not for either sibling
 *   reading's endorsed alternative.
 *
 * KEY AGENTS:
 *   - dueling_code_arbiters: institutional beneficiary/agenda_setter, retains adjudicative role across both mechanisms
 *   - aristocratic_incumbents_with_reputational_capital: powerful beneficiary, captures reputational deference under both regimes without bearing the historical risk
 *   - dueling_participants_and_seconds: moderate-power payer, caught in transition between two overlapping constraint regimes
 *   - commoner_and_bourgeois_challengers: powerless payer, excluded by both rising cost and narrowing definition simultaneously
 *   - women_and_dependents_of_duelists: powerless payer, absent from adjudication under either mechanism
 *   - the_state_and_judiciary: institutional agenda_setter/beneficiary, drives drop mechanism and enables contraction mechanism jointly
 *   - social_and_legal_historians: analytical observer of the composite decline pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.58).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.61).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor-Violence Legitimacy Norm (Composite Decline Reading: External Cost + Conceptual Contraction)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'f545a975-65c3-4cc0-92e8-cb5eedbcd5a6').
narrative_ontology:cs_kernel_codification('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', distributed).
narrative_ontology:cs_authority_grounding('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', practice).
narrative_ontology:cs_interpretation_layer_present('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6').
narrative_ontology:cs_reading_relation('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', foundational, decline_requires_joint_mechanism_sufficiency).
narrative_ontology:cs_axiom_status(decline_requires_joint_mechanism_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', decline_requires_joint_mechanism_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', secondary, cost_mechanism_alone_underdetermines_historical_variance).
narrative_ontology:cs_axiom_status(cost_mechanism_alone_underdetermines_historical_variance, holdable).
narrative_ontology:cs_axiom_grounding('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', cost_mechanism_alone_underdetermines_historical_variance, empirically_contingent).
narrative_ontology:cs_reference_frame('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', aristocratic_violent_vindication_norm).
narrative_ontology:cs_drift_state('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', post_state_consolidation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f545a975-65c3-4cc0-92e8-cb5eedbcd5a6', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, dueling_code_arbiters).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, aristocratic_incumbents_with_reputational_capital).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, dueling_participants_and_seconds).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, commoner_and_bourgeois_challengers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_and_dependents_of_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, the_state_and_judiciary).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, gentlemanly_honor_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifiers of the codes of honor (seconds' manuals, dueling codes, aristocratic courts of honor) who administer what counts as an insult requiring satisfaction and what counts as adequate reparation short of violence. As external costs rise (legal prosecution, insurance against career/social ruin) and honor's semantic content narrows to exclude violent vindication, these arbiters retain their adjudicative role by redefining honor procedures around apology, litigation, and reputational management rather than combat — preserving their institutional position across both mechanisms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_code_arbiters, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, dueling_code_arbiters, beneficiary).

% Established elites whose reputational capital was historically defended by credible threat of violence. As dueling becomes both costlier (drop mechanism: prosecution, social insurance markets, loss of commissions) and conceptually illegitimate (contraction mechanism: honor redefined as self-restraint, Christian forbearance, bourgeois respectability), they retain the reputational deference the violent system once secured for them, now backed by legal and social status mechanisms instead. They benefit from both mechanisms without bearing the risks either once imposed.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_incumbents_with_reputational_capital, beneficiary,
    powerful, generational, mobile, national).

% Men whose social standing required participation in the honor economy: they bore death, injury, prosecution, and financial ruin under the older regime, and under the declining regime bear a different cost — the humiliation of being unable to vindicate insult by either violent or non-violent means as institutions no longer recognize either the old satisfaction procedure or a clear replacement. Caught in the transition, they pay costs under both the rising external-cost regime and the narrowing semantic regime.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_participants_and_seconds, payer,
    moderate, biographical, constrained, regional).

% Non-aristocratic men attempting to access the honor economy for social mobility (dueling as a marker of gentlemanly status) find the door closing from two directions simultaneously: external costs (legal, financial) make the practice newly punishable for those without aristocratic legal cover, and the contracting definition of honor increasingly excludes them by redefining true honor as inherently aristocratic self-restraint they are presumed incapable of. They lose the mobility channel without gaining protection under either mechanism.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, commoner_and_bourgeois_challengers, payer,
    powerless, biographical, trapped, regional).

% Wives, children, and other dependents bore the material and status consequences of a duelist's death, injury, or prosecution under the old regime, and continue to bear diffuse costs under the new regime as honor violence transforms into slower-burning reputational and legal warfare (defamation suits, social ostracism campaigns) that consume family resources over years rather than resolving in a single encounter. Neither mechanism gives them voice in the adjudication.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_and_dependents_of_duelists, payer,
    powerless, biographical, trapped, local).

% Courts, legislatures, and prosecutors drive the external-cost mechanism directly (criminalizing dueling, prosecuting survivors, refusing to recognize honor as mitigation) and indirectly enable the contraction mechanism by providing an alternative venue (defamation and libel litigation) that reframes what honor claims are made of. The state accrues legitimacy and monopoly-of-violence consolidation from both mechanisms operating together.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, the_state_and_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, the_state_and_judiciary, beneficiary).

% Study the decline of dueling and honor violence, debating whether external cost imposition or semantic redefinition of honor was the operative mechanism, or whether — as this reading holds — both operated jointly and neither alone is sufficient to explain the historical trajectory of decline across different national contexts and timescales.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, social_and_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code, in its declining form, still coordinates status competition among elites: it specifies how insult is recognized, how reparation is demanded, and who is authorized to adjudicate — reducing (in principle) unregulated private violence to a rule-governed procedure with known participants and known stakes.
% TRANSFER_FUNCTION: Moves reputational deference and social standing from challengers/losers to incumbents, and moves risk (legal, physical, financial) from institutional arbiters and state actors onto individual duelists and their dependents, under both the rising-external-cost and narrowing-definition mechanisms simultaneously.
% ABSENT_VOICES: Women and dependents of duelists are never parties to the honor code's adjudication despite bearing its material consequences under both mechanisms; commoner and bourgeois challengers are excluded from defining what honor means even as the redefinition determines whether they can ever participate legitimately.
% DISAPPEARANCE_RATIONALE: If the honor-violence legitimacy norm vanished overnight (rather than declining gradually under two overlapping mechanisms), the entire apparatus of seconds, courts of honor, dueling codes, and the associated social ranking of who has 'given satisfaction' would lose its referent immediately — reputational disputes would have to route entirely through law or informal social sanction with no intermediate violent-but-rule-governed option, which is in fact what happened as the composite decline completed.
% FOUNDING_PROBLEM: Pre-state and early-state societies lacked reliable centralized enforcement of reputational and status claims; dueling and honor violence emerged as a decentralized, rule-bound substitute for judicial remedy among a class (the nobility/gentry) that recognized no superior arbiter for matters of personal honor.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state-building scholars (outside the beneficiary class of aristocratic incumbents) attest that centralized judiciaries, defamation law, and professional/administrative status systems had, by the mechanisms' completion, fully displaced the function dueling once served — the founding problem of absent reputational remedy no longer exists in jurisdictions where the norm declined, which corroborates that the composite mechanism ran to completion rather than merely suppressing an ongoing need.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 to 0.58 across the interval because the two mechanisms compound rather than substitute: as external costs make violent vindication rarer (drop), arbiters and incumbents do not lose their adjudicative and reputational position — they migrate it into the newly available non-violent honor-management channels the contraction mechanism opens (litigation, formal apology, respectability performance), capturing rents under the new regime that a pure-drop account (frequency decline with legitimacy intact) would not predict. Theater ratio rises correspondingly (0.12 to 0.42) as the surviving honor apparatus becomes increasingly performative — courts of honor and codes of conduct persist as social ritual even as their original violent-adjudication function has been foreclosed by both cost and definition. Suppression rises (0.35 to 0.61) reflecting the joint tightening: legal suppression of dueling itself, plus discursive suppression of violent-honor claims as illegitimate speech acts within the redefined honor vocabulary. This is a genuinely different metric profile from either sibling reading: the drop reading would show flat legitimacy/definition metrics with only frequency declining; the contraction reading would show sharp discontinuous semantic collapse largely independent of enforcement intensity. The composite reading's metrics track a slower, compounding, mutually-reinforcing decline consistent with piton dynamics — theatrical residue outlasting function.
 *
 * DIRECTIONALITY LOGIC:
 *   Arbiters and incumbents sit near the beneficiary end of directionality under both mechanisms: institutional power, arbitrage-grade exit (they redefine the game rather than losing it), generational time horizon. Dueling participants and seconds sit in a transitional middle — moderate power but constrained exit, since neither the old violent-vindication path nor a clear non-violent substitute is fully available to them during the overlap period, which is exactly the 'overdetermined' bind this reading names. Commoner/bourgeois challengers and dependents sit at the full-target end: powerless, trapped, bearing costs from both mechanisms with no adjudicative voice in either.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absent centralized remedy for reputational injury among a class recognizing no superior arbiter) is dead — state judiciaries and administrative status systems have fully displaced it — yet the honor-adjudication apparatus persists in residual, theatrical form (courts of honor surviving as social ritual, honor vocabulary persisting in legal defamation discourse) captured by the same arbiter class that administered the original function. This is the mandatrophy signature: a mandate whose founding problem resolved via two independent mechanisms, while the administering class retained position by migrating across both. Classifying this as piton (rather than snare) matters because no single concentrated beneficiary captures new extraction from a discrete victim set through active coercion — instead, diffuse costs are borne by individuals caught in the compounding transition, and the arbiter class benefits from inertial institutional continuity rather than fresh extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence_or_conflation,
    'Are the drop mechanism (external cost) and contraction mechanism (semantic redefinition) genuinely independent causal processes that happened to co-occur, or is the contraction mechanism itself partly downstream of the drop mechanism (i.e., honor was redefined BECAUSE dueling had already become too costly to sustain, making ''contraction'' an ex-post rationalization of a cost-driven retreat)?',
    'Comparative historical analysis of jurisdictions where legal/financial cost rose sharply but honor vocabulary remained violence-inclusive for an extended period (or vice versa) would establish temporal and causal independence; if contraction consistently lags drop with no jurisdictions showing the reverse or simultaneous pattern, the composite reading collapses toward the drop_reading with contraction as epiphenomenal.',
    'If contraction is downstream of drop, this composite reading is not actually a third, independent reading but a more detailed drop_reading; the classification would likely shift toward whatever the drop_reading computes, since the semantic mechanism would carry no independent extractive weight. If mechanisms are shown temporally and causally independent, the composite reading''s higher extractiveness and piton classification (capturing compounding effects) is the structurally correct account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_or_conflation, empirical, 'Whether the two named mechanisms are causally independent or one is downstream of the other.').

omega_variable(
    kernel_referent_ambiguity,
    'Does ''honor violence legitimacy'' name a single kernel with one legitimacy claim that eroded via two channels, or are drop and contraction better modeled as evidence about two DIFFERENT kernels (a practical-permission kernel and a semantic-category kernel) that happen to share a label?',
    'Apply the epsilon-invariance test directly: if the extraction/victim profile attributable to ''external cost pressure alone, definition held fixed'' differs sharply from ''definitional exclusion alone, cost held fixed'' in ways that cannot be reconciled to one kernel''s drift, they may need decomposition into a fourth family member rather than three sibling readings of one kernel.',
    'If the kernel referent is genuinely singular, the three-reading family (drop/contraction/composite) is the correct decomposition. If the mechanisms pick out structurally distinct kernels, the composite reading may itself need splitting rather than standing as the synthesis of the other two.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_referent_ambiguity, conceptual, 'Whether the kernel this triplet is reading is genuinely one contested commitment or a conflation of two.').

omega_variable(
    arbiter_class_continuity_beneficiary_status,
    'Is the dueling_code_arbiters'' apparent beneficiary status under the composite mechanism a genuine capture of new rents, or merely the continuation of a pre-existing status advantage that would have persisted regardless of which decline mechanism operated?',
    'Track arbiter-class wealth, social standing, and adjudicative caseload before, during, and after the decline interval across multiple jurisdictions; a genuine capture signature would show the arbiter class''s relative position IMPROVING during the transition, not merely holding steady.',
    'If arbiter position merely held steady, the piton classification (inertial persistence, no concentrated fresh beneficiary) is well-supported. If arbiter relative position improved during the transition, a tangled_rope or snare classification capturing active rent-seeking during the decline itself would be more accurate than piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbiter_class_continuity_beneficiary_status, empirical, 'Whether the beneficiary class actively captured new advantage during decline or merely retained pre-existing position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__composite_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(hono_tr_t80, honor_violence_legitimacy__composite_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__composite_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__composite_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hono_be_t80, honor_violence_legitimacy__composite_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__composite_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__composite_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(hono_su_t80, honor_violence_legitimacy__composite_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__composite_reading, suppression_requirement, 100, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This composite_reading is one of three sibling stories reading the honor_violence_legitimacy kernel. drop_reading holds dueling remained legitimate but became practically rare from external cost; contraction_reading holds dueling became conceptually unthinkable via redefinition of honor independent of cost; composite_reading (this story) holds both mechanisms operated jointly with different victim sets and extractiveness profiles, and that the drop mechanism alone is insufficient without the contraction edge. Each reading is authored with its own stable epsilon and its own beneficiary/victim structure per the epsilon-invariance principle; they are linked here rather than merged into one parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
