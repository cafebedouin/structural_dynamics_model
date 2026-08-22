% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Dueling Legitimacy Under Composite Decline (External Cost + Conceptual Contraction)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the COMPOSITE reading of the
 *   honor_violence_legitimacy kernel: the claim that dueling's decline across
 *   the 18th–19th centuries cannot be explained by either external-cost
 *   escalation (prosecution, forfeiture, career penalty — the drop_reading)
 *   or conceptual redefinition (honor reconceived as incompatible with
 *   violence — the contraction_reading) operating alone. The composite
 *   reading holds that both mechanisms operated jointly and were mutually
 *   reinforcing: rising costs made the duel harder to justify AS
 *   honor-consistent even to its practitioners, while redefinition made the
 *   surviving costs easier for the state to impose without triggering the
 *   code's own resistance logic. This reading has a different victim set than
 *   either sibling alone — it names casualties of the TRANSITION PERIOD
 *   ITSELF (those caught between an eroding old code and a
 *   not-yet-consolidated new one), which neither single-mechanism account
 *   isolates cleanly.
 *
 * KEY AGENTS:
 *   - dueling_participants_and_seconds: bore both escalating legal cost and eroding social meaning simultaneously
 *   - widows_and_dependents_of_the_slain: paid material costs regardless of which mechanism dominated
 *   - state_legal_monopolists: administered both cost-escalation and redefinition levers, consolidating authority
 *   - professional_classes_seeking_status_alternatives: benefited from the joint collapse of practice and meaning
 *   - dueling_code_traditionalists: excluded voice disputing the neutrality of the joint decline
 *   - legal_and_social_historians: analytical observers reconstructing the composite mechanism from records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.58).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.62).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Dueling Legitimacy Under Composite Decline (External Cost + Conceptual Contraction)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c').
narrative_ontology:cs_kernel_codification('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', distributed).
narrative_ontology:cs_authority_grounding('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', distributed).
narrative_ontology:cs_reading_relation('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', foundational, mechanisms_are_jointly_necessary_not_individually_sufficient).
narrative_ontology:cs_axiom_status(mechanisms_are_jointly_necessary_not_individually_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', mechanisms_are_jointly_necessary_not_individually_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', secondary, joint_operation_forecloses_single_channel_resistance).
narrative_ontology:cs_axiom_status(joint_operation_forecloses_single_channel_resistance, holdable).
narrative_ontology:cs_axiom_grounding('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', joint_operation_forecloses_single_channel_resistance, empirically_contingent).
narrative_ontology:cs_reference_frame('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', aristocratic_honor_code_as_autonomous_repair_system).
narrative_ontology:cs_drift_state('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', consolidated_state_legal_monopoly_1900, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1deaa39e-faf6-4a5b-9ce5-d7d0d6219e3c', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, state_legal_monopolists).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, professional_classes_seeking_status_alternatives).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, dueling_participants_and_seconds).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, widows_and_dependents_of_the_slain).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, lower_status_men_barred_from_honor_repair).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, honor_as_reputational_rather_than_physical_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen and officers who, across the 18th–19th centuries, faced dueling as both a live legal risk (prosecution, exile, forfeiture) and, increasingly, as a socially unintelligible act as 'honor' was redefined around restraint. They bore the escalating cost of the code (legal jeopardy, injury, death) while simultaneously watching the social meaning of their actions curdle from vindication into criminality or absurdity — a double bind neither pure prohibition nor pure redefinition alone produced.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_participants_and_seconds, payer,
    moderate, biographical, constrained, national).

% Bore the material and status costs of duels fought to satisfy a code they had no part in authoring — loss of income, loss of protector, loss of standing — regardless of whether the duel's legitimacy was eroding through cost or through redefinition. Neither mechanism compensated them; both left them holding the externalized cost.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, widows_and_dependents_of_the_slain, payer,
    powerless, generational, trapped, local).

% Never had access to the duel as a repair mechanism in the first place (class-restricted) and then, as honor was redefined to exclude violence, found the redefinition arriving too late or too selectively applied to erase the residual stigma of being 'unable to answer' an insult under the old code. They pay a cost from the transition period itself, independent of which mechanism (cost or redefinition) dominates.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, lower_status_men_barred_from_honor_repair, payer,
    powerless, biographical, trapped, national).

% Courts, legislatures, and crowns that raised the practical cost of dueling (prosecution, forfeiture, career penalties) while simultaneously sponsoring or tolerating a redefinition of honor as compatible with legal submission. They administer both levers and collect the resulting monopoly on legitimate violence — the composite mechanism concentrates authority in their hands regardless of which lever any given case turns on.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_monopolists, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, state_legal_monopolists, agenda_setter).

% Rising commercial and professional elites who benefited from a redefined honor code that valorized reputation, credit, and self-restraint over physical risk — a code that let them compete for status without adopting the aristocratic dueling apparatus. They gained standing precisely because the composite decline made both the old code's practice AND its meaning collapse together, clearing space for a new status currency.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, professional_classes_seeking_status_alternatives, beneficiary,
    organized, generational, mobile, national).

% Aristocratic and military traditionalists who continued to regard the duel as the only legitimate honor-repair mechanism, and who are largely absent from the historiographical conversation that treats the decline as settled and overdetermined. They would object that the code was neither merely made costly nor merely redefined out of existence, but actively suppressed by an alliance of state and rising bourgeois interests — a reading the composite account tends to flatten into disinterested social evolution.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_code_traditionalists, excluded,
    moderate, biographical, identity_locked, regional).

% Reconstruct the decline from court records, dueling codes, conduct literature, and casualty statistics; debate whether cost-escalation or meaning-redefinition (or their joint operation) best explains the timing and unevenness of the decline across regions and classes.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, legal_and_social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, state_legal_monopolists).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite mechanism coordinates a transition of legitimate-violence authority from a decentralized aristocratic honor code to a centralized state/legal-professional order, using two levers together: raising the practical cost of dueling AND narrowing what 'honor' is understood to permit, so that neither lever alone has to carry the whole weight of delegitimation.
% TRANSFER_FUNCTION: Moves the authority to define and repair honor violations from individual gentlemen (via personal combat) to the state (via courts) and to reputational/credit markets (via professional and commercial status systems); moves the material costs of the transition onto participants, their dependents, and those excluded from either the old or new repair mechanisms.
% ABSENT_VOICES: Dueling code traditionalists, who held that the composite decline was neither natural nor neutral but an engineered displacement serving state consolidation and bourgeois status competition, are largely written out of accounts that treat the decline as an overdetermined, near-inevitable social evolution.
% DISAPPEARANCE_RATIONALE: If the composite explanation were shown false — if the decline were adequately explained by cost alone or redefinition alone — the historiographical consensus on dueling's decline would need substantial revision, and the victim/beneficiary attribution in this specific reading (which depends on BOTH mechanisms operating jointly) would dissolve into one of the sibling readings. Whether 'the world rearranges' depends on which single-mechanism account would absorb the explanatory work; parties disagree on this.
% FOUNDING_PROBLEM: Aristocratic and military honor codes needed a mechanism to repair reputational injury that predated and existed independently of centralized state legal authority; dueling was that mechanism. The composite decline addresses the later problem of how a centralizing state and an emerging professional class jointly displaced that mechanism without a single clean rupture.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary classes (independent of both the state apparatus and the professional classes that gained from the transition) corroborate via court records and dueling statistics that the practice became both practically costly and semantically excluded from 'honor' within overlapping timeframes across multiple European jurisdictions and the American South, supporting joint operation rather than either mechanism alone; dueling traditionalist voices from period conduct literature corroborate that both external cost and definitional narrowing were felt as simultaneous pressures, though they dispute the neutrality of that joint operation.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 — moderate-high — because the composite mechanism concentrates authority (over honor-repair and over legitimate violence) in state and professional hands via two simultaneous channels, which is a more efficient extraction of authority than either channel alone could achieve; this is higher than what a pure cost-story or pure redefinition-story would independently support, because the joint operation removes the traditionalist counter-move of treating cost as a mere tax to be paid (since redefinition simultaneously undermines the code's meaning) and removes the counter-move of treating redefinition as mere rhetoric (since cost makes continued practice materially punishing). The two mechanisms are mutually reinforcing rather than merely additive, which is the structural delta this reading claims relative to its siblings. Suppression rises across the interval (0.35 to 0.62) tracking the maturation of both state legal enforcement AND the social/discursive enforcement of the new honor concept — the alignment on one time grid lets both series be read together as jointly hardening.
 *
 * PERSPECTIVAL GAP:
 *   From the state/professional beneficiary seats, the composite decline reads as the natural, overdetermined triumph of civilized restraint over archaic violence — two independent forces happening to agree. From the payer seats (participants, dependents, the excluded), the same joint operation reads as a pincer: no single point of resistance could address it, because contesting the cost argument left the meaning argument standing, and vice versa. This is the structural asymmetry the composite reading is built to name — it is not visible from either single-mechanism account.
 *
 * DIRECTIONALITY LOGIC:
 *   State legal monopolists and rising professional classes are coded as beneficiaries (low d) because the joint mechanism transfers repair-authority and status-currency to them specifically. Dueling participants, widows/dependents, and lower-status excluded men are coded as victims (high d) because they bear the material and status costs of a transition that neither compensates them nor asks their consent. Dueling traditionalists are excluded rather than victimized outright — their voice is missing from the historiography, which is a distinct structural fact from bearing the cost directly, hence the separate excluded role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decentralized honor-repair predating centralized law) is authored as dead — courts now perform that function almost everywhere the composite reading applies — yet the disappearance_verdict is contested rather than world_unchanged, because the composite reading's specific claim (joint, mutually-reinforcing operation) would itself dissolve into a simpler single-mechanism account if either drop_reading or contraction_reading were shown sufficient alone. This is exactly the overdetermination question the kernel exists to hold open: the composite reading claims MORE structure (two reinforcing mechanisms) than either sibling, and that additional structural claim is the falsifiable content that distinguishes it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_single_mechanism,
    'Could either the cost-escalation mechanism or the conceptual-redefinition mechanism alone have produced the observed decline curve, or does the historical timing require joint operation?',
    'Comparative regional analysis: jurisdictions where legal cost rose sharply without significant redefinition of honor discourse, versus jurisdictions where redefinition occurred without significant cost escalation, compared against the composite regions'' decline curves. If single-mechanism regions show comparable decline rates, the composite claim is undermined.',
    'If a single mechanism is shown sufficient, this story''s claimed structural delta (joint, mutually-reinforcing operation with its own distinct victim set) collapses into one of the sibling readings, and this constraint''s victim/beneficiary attribution would need to be reassigned to whichever sibling absorbs the explanatory work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_single_mechanism, empirical, 'Whether the composite reading''s core claim of mechanism-interdependence survives comparative regional testing.').

omega_variable(
    state_interest_in_composite_framing,
    'Does the composite (''overdetermined, mutually reinforcing'') framing itself serve state and professional-class interests by making the decline appear natural and inevitable rather than the product of a contestable, interested campaign?',
    'Discourse analysis of period legislative debates and conduct literature: does the joint-mechanism framing appear in sources produced by the beneficiary classes themselves, suggesting it functions partly as legitimation rhetoric rather than purely as historians'' analytical device?',
    'If the composite framing originates substantially in beneficiary-class sources, part of what this story treats as ''the historical mechanism'' may itself be a beneficiary narrative absorbed into later historiography — raising the traditionalist-excluded voice''s objection from a minority dissent to a substantive methodological concern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_interest_in_composite_framing, conceptual, 'Whether the composite/overdetermined framing is itself partly an artifact of beneficiary-class self-narration.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Are the ''transition-period casualties'' this reading names (lower-status men barred from honor repair, those caught between codes) a genuinely distinct victim class, or are they better absorbed into the victim sets already named by drop_reading and contraction_reading separately?',
    'Cross-reference this story''s victim declarations against the sibling readings'' victim declarations once authored; check for non-overlapping harm mechanisms that only the joint operation produces.',
    'If the transition-period victim class is not genuinely distinct, this reading''s claim to structural novelty (relative to its siblings) weakens substantially, though the joint-mechanism extractiveness claim could still stand independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, conceptual, 'Whether the composite reading''s victim set is structurally novel or an artifact of combining the siblings'' victim sets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__composite_reading, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__composite_reading, theater_ratio, 1780, 0.18).
narrative_ontology:measurement(hono_tr_t1810, honor_violence_legitimacy__composite_reading, theater_ratio, 1810, 0.27).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__composite_reading, theater_ratio, 1840, 0.35).
narrative_ontology:measurement(hono_tr_t1870, honor_violence_legitimacy__composite_reading, theater_ratio, 1870, 0.4).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__composite_reading, theater_ratio, 1900, 0.44).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__composite_reading, base_extractiveness, 1750, 0.32).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__composite_reading, base_extractiveness, 1780, 0.38).
narrative_ontology:measurement(hono_be_t1810, honor_violence_legitimacy__composite_reading, base_extractiveness, 1810, 0.47).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__composite_reading, base_extractiveness, 1840, 0.53).
narrative_ontology:measurement(hono_be_t1870, honor_violence_legitimacy__composite_reading, base_extractiveness, 1870, 0.56).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__composite_reading, base_extractiveness, 1900, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__composite_reading, suppression_requirement, 1750, 0.35).
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__composite_reading, suppression_requirement, 1780, 0.42).
narrative_ontology:measurement(hono_su_t1810, honor_violence_legitimacy__composite_reading, suppression_requirement, 1810, 0.51).
narrative_ontology:measurement(hono_su_t1840, honor_violence_legitimacy__composite_reading, suppression_requirement, 1840, 0.58).
narrative_ontology:measurement(hono_su_t1870, honor_violence_legitimacy__composite_reading, suppression_requirement, 1870, 0.6).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__composite_reading, suppression_requirement, 1900, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.1).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language claim 'why dueling declined' (the honor_violence_legitimacy kernel). drop_reading treats external cost as sufficient; contraction_reading treats conceptual redefinition as sufficient; composite_reading (this story) treats both as jointly necessary and mutually reinforcing, with its own extractiveness value (0.58, higher than either single-mechanism claim would independently support) and its own victim set (transition-period casualties distinct from either sibling's). All three link to each other via affects_constraints per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
