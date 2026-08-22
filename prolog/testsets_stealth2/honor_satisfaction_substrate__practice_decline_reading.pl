% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor-Satisfaction Substrate — Practice-Decline Reading (Exogenous Enforcement)
 *   domain: historical sociology / cultural anthropology / legal history
 *
 * SUMMARY:
 *   The honor-satisfaction substrate is the normative system that, among
 *   status-bearing men, made the challenge-and-satisfaction protocol the
 *   sanctioned terminus of a serious dispute: an insult required apology or a
 *   bounded, ritualized violence event, and refusal carried social ruin. This
 *   story instantiates the practice-decline reading of that substrate: the
 *   code itself persisted as normative substrate while the practice of
 *   dueling declined because exogenous enforcement raised its price —
 *   anti-dueling statutes, forfeiture of military commissions,
 *   institution-building that routed disputes into honor courts and academies
 *   designed to prevent challenges, and rising opportunity cost as careers
 *   and courts offered better exits. On this reading dueling remained
 *   thinkable to the last — honored in the abstract, invoked in rhetoric —
 *   but became impractical; the substrate survived in attenuated carriers
 *   (military honor codes, the Southern culture of honor, community
 *   reputation systems) and still coordinates reputation disputes today. The
 *   ε referent is the standing arrangement as this reading holds it: an
 *   intact substrate whose satisfaction practice is legally suppressed. The
 *   claimed type is the reading's structural commitment — a coordination
 *   arrangement whose practice collapsed under legal pressure, not a natural
 *   law eroding and not an internal legitimacy collapse — while the metrics
 *   are authored independently from the arrangement's observed operation,
 *   including its residual extraction; where computed per-seat
 *   classifications diverge from the claim, that divergence is the
 *   measurement this corpus exists to take.
 *
 * KEY AGENTS:
 *   - - honor_gentry_class: Primary beneficiary and status apex (powerful/identity_locked) — the code maintains the deference hierarchy it sits atop; historically also paid in duel deaths
 *   - - military_officer_corps: Institutional carrier (institutional/constrained) — benefits from honor cohesion; paid in commissions and lives under anti-dueling enforcement
 *   - - southern_culture_of_honor_communities: Regional persistence carrier (organized/identity_locked) — where the substrate survives most strongly and most violently
 *   - - honor_culture_young_men: Primary cost-bearers (moderate/trapped) — socialized into reputation-defense expectations; bear the violence risk
 *   - - honor_code_refusers: Sanctioned dissenters (organized/constrained) — bore refusal sanctions; their organized exit pressure fed the exogenous enforcement coalition
 *   - - state_legal_apparatus: Exogenous enforcer and current agenda-setter (institutional/arbitrage) — statutes, courts-martial, and institutional barriers that made the practice impractical
 *   - - honor_community_women: Excluded seat (powerless/constrained) — the ostensible objects of defense with no standing in the code's adjudication
 *   - - historical_sociologists: Analytical observer (analytical/analytical) — sees the full substrate/practice/ceremony structure from outside any carrier community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.42).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.48).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor-Satisfaction Substrate — Practice-Decline Reading (Exogenous Enforcement)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical sociology / cultural anthropology / legal history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'dc2f65b4-1742-44b3-ab0b-972c16423a3e').
narrative_ontology:cs_kernel_codification('dc2f65b4-1742-44b3-ab0b-972c16423a3e', distributed).
narrative_ontology:cs_authority_grounding('dc2f65b4-1742-44b3-ab0b-972c16423a3e', practice).
narrative_ontology:cs_interpretation_layer_present('dc2f65b4-1742-44b3-ab0b-972c16423a3e').
narrative_ontology:cs_reading_relation('dc2f65b4-1742-44b3-ab0b-972c16423a3e', honor_satisfaction_substrate__cultural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('dc2f65b4-1742-44b3-ab0b-972c16423a3e', honor_satisfaction_substrate__composite_overdetermined_reading, forecloses).
narrative_ontology:cs_axiom('dc2f65b4-1742-44b3-ab0b-972c16423a3e', foundational, honor_substrate_continuity_under_suppression).
narrative_ontology:cs_axiom_status(honor_substrate_continuity_under_suppression, holdable).
narrative_ontology:cs_axiom_grounding('dc2f65b4-1742-44b3-ab0b-972c16423a3e', honor_substrate_continuity_under_suppression, empirically_contingent).
narrative_ontology:cs_axiom('dc2f65b4-1742-44b3-ab0b-972c16423a3e', foundational, exogenous_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(exogenous_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('dc2f65b4-1742-44b3-ab0b-972c16423a3e', exogenous_enforcement_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('dc2f65b4-1742-44b3-ab0b-972c16423a3e', secondary, dueling_thinkable_but_impractical).
narrative_ontology:cs_axiom_status(dueling_thinkable_but_impractical, holdable).
narrative_ontology:cs_axiom_grounding('dc2f65b4-1742-44b3-ab0b-972c16423a3e', dueling_thinkable_but_impractical, empirically_contingent).
narrative_ontology:cs_reference_frame('dc2f65b4-1742-44b3-ab0b-972c16423a3e', intact_honor_substrate_with_live_satisfaction_practice).
narrative_ontology:cs_drift_state('dc2f65b4-1742-44b3-ab0b-972c16423a3e', post_prohibition_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc2f65b4-1742-44b3-ab0b-972c16423a3e', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_gentry_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, southern_culture_of_honor_communities).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_culture_young_men).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_code_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_gentry_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, honor_code_coordination_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, reputation_sanction_currency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Status-bearing men — politicians, lawyers, landowners — whose serious disputes the code standardizes into challenge-and-satisfaction and whose standing the code protects. They accrue the deference hierarchy that the extracted compliance maintains, and historically they also paid: duels killed their peers, and under anti-dueling enforcement they risked commissions and prosecution. Their social self-concept is constituted through honor standing, so leaving the code means ceasing to be what they are.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_gentry_class, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, honor_gentry_class, payer).

% Carries the substrate's most formal attenuated form: written honor codes, academies, honor courts, and an accountability idiom organized around honor language. It benefits from the cohesion and discipline the code coordinates; it paid historically in duel deaths and, under enforcement, in courts-martial and forfeited commissions. Resignation is possible, but career investment and the total-institution character of officer life make exit costly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, payer).

% Regional communities — most studied in the American South — where reputation remains the operative sanctioning currency and the satisfaction logic survives in attenuated, often violent form. The community's internal order runs on the substrate; members' identities are formed inside it from childhood; exit means leaving kin, congregation, and standing networks at once.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, southern_culture_of_honor_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Young men socialized into reputation-defense expectations before they could choose. They bear the arrangement's violence risk: the culture-of-honor research literature ties the substrate's persistence to elevated lethal-violence rates among precisely this group. Moving away does not reliably shed the socialization, so geographic exit does not release them from the norm's grip.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_culture_young_men, payer,
    moderate, biographical, trapped, regional).

% Men who decline the reputation-defense protocol — on religious conviction, principle, or simple unwillingness. They bear the sanction the code exists to enforce: ridicule, exclusion from standing, the cowardice label. Their churches and reform networks gave partial shelter, and their organized refusal fed the exogenous enforcement coalition that ultimately made the practice impractical.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_refusers, payer,
    organized, biographical, constrained, national).

% Legislatures, courts, and military tribunals that criminalized the satisfaction practice, built the institutional barriers (commission forfeiture, academy design, honor courts that adjudicate without shots), and now administer the standing arrangement: practice prohibited, substrate tolerated. It can recalibrate enforcement at low cost and bears little of the substrate's costs or benefits directly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The ostensible objects whose honor men defended — and who had no standing anywhere in the code's adjudication: they could not issue, accept, or second a challenge, and the code's terms were set entirely by men. They bear the arrangement's downstream costs (violence, widowhood, enforced dependency) without a seat in its conversation, past or present.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_community_women, excluded,
    powerless, biographical, constrained, regional).

% Analytical seat: reconstructs the substrate/practice distinction from legal records, regimental courts-martial archives, press accounts, and the culture-of-honor literature; sees from outside any carrier community which parts of the arrangement coordinate, which extract, and which are ceremony.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, honor_gentry_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the resolution of status insults among honor-bearing men: a single bounded satisfaction event — challenge, negotiated terms, duel or ritualized apology — replaces open-ended feuding, and reputation, the community's sanctioning currency, is defended through a known protocol rather than private war. The attenuated carriers continue this work in institutional form: military honor codes coordinate integrity and accountability expectations; community honor norms coordinate reputation sanctioning.
% TRANSFER_FUNCTION: Moves compliance and risk from honor-community members — especially young men compelled to answer challenges — toward the maintenance of the community's standing hierarchy: reputation-defense labor, violence risk, and conformity flow upward and inward, consolidating the deference order at whose apex the gentry class sits.
% ABSENT_VOICES: Women of the honor communities — the ostensible objects whose honor men defended — had no standing in challenge adjudication and still have no seat where honor norms are maintained. The men compelled to fight, often the younger challenged party, had no seat where the code's terms were set. The dignity-culture mainstream that eventually displaced honor adjudication in civil society stood entirely outside the honor conversation. The code's internal unanimity was produced in a room its cost-bearers never entered.
% DISAPPEARANCE_RATIONALE: If the substrate vanished overnight, honor communities would lose their dispute-resolution protocol and revert to courts alone or to private feud; military institutions would lose a working cohesion and accountability idiom and would have to build a replacement; culture-of-honor communities would lose the reputation currency that organizes their sanctioning, and the violence expectations currently channeled by it would reorganize rather than disappear. The arrangements of millions of people still depend on the substrate's attenuated operation.
% FOUNDING_PROBLEM: Among armed status-bearing men, how to resolve insults and disputes without open-ended feuding that would destroy the class from within: the code of honor standardized one bounded violence event, and its ritual alternatives, as the terminus of a dispute.
% FOUNDING_PROBLEM_CORROBORATION: The dead-original-form reading has corroboration from outside the beneficiary set: historians of the state's violence monopoly and of the dignity transition, working from court and legislative records, attest that the armed-gentry feud problem the code was built for no longer exists. The live-descendant reading is attested mainly by the carriers themselves — military institutions and honor communities — whose attestation is discounted as beneficiary testimony. Evangelical and reform archives show the problem's status was already contested at the code's peak. Net: outside corroboration supports the dead-original-form claim; the live-descendant claim rests largely on insider attestation.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.42 at interval end, down from 0.65 at the Code-Duello-era peak): the arrangement's lethal compulsion layer — challenge-or-be-ruined, with death as the enforcement edge — was removed exogenously, so the standing arrangement's extraction is residual: reputation-defense pressure that still channels young men in honor cultures into violence risk, plus conformity costs inside institutional carriers. At the practice peak the arrangement was a hybrid — real coordination (feud suppression) with real extraction (compelled lethal risk) — and the exogenous enforcement stripped the extraction layer rather than the arrangement dissolving from within. Suppression (0.48) is authored as a raw structural property, unscaled by power or scope: community sanction and institutional discipline still enforce the code inside honor communities, while legal counter-pressure removed the practice's compulsion. Theater (0.40): a substantial share of surviving honor maintenance is ceremonial — oaths, honor rituals, heritage fencing — but culture-of-honor sanctioning and military accountability functions are live, so theater is substantial without being dominant. Accessibility collapse (0.45): alternatives persist, and the prohibition itself created the main one — courts as the honorable-enough channel; the substrate does not close exits the way a natural limit would. Resistance (0.55): the arrangement met organized resistance across its entire recorded span — refuser networks, evangelical campaigns, reform societies — and the dignity transition is that resistance's large-scale successor. All three tracked metric series run on one shared seven-point grid. The suppression_requirement series is authored because this story's subject is enforcement-capacity change: it ratchets to a mid-interval peak (honor communities policed refusal harder once the law offered refusers an exit) and then decays as the practice normalizes into attenuated form. The coercion grid renders the same history at levels: structural-level coercive force rises (0.15 to 0.75) as the state takes over enforcement while individual-level force decays (0.70 to 0.45) as socialization replaces sanction. The scalar tracks the constraint's own suppressive force over its members; the grid's structural level tracks the state's counter-force against the practice; the two are one account, not a contradiction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the honor_gentry_class seat the arrangement is inherited order: the code standardized disputes their class could not have survived as feuds, and its costs were paid in a currency — standing — that they hold abundantly. From the honor_culture_young_men seat the same structure is compelled risk: socialization before consent into a violence protocol they did not author and cannot cheaply shed. The military_officer_corps seat is genuinely dual — cohesion benefit and commission-and-casualty cost — and should compute near the boundary between coordination and extraction. The state_legal_apparatus seat experiences neither subsidy nor extraction: it administers the arrangement's suppression and bears little of the substrate itself. The honor_community_women seat never entered the conversation that produced the code's unanimity, so no seat-level perception from inside the arrangement represents them. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (honor_gentry_class, military_officer_corps, southern_culture_of_honor_communities) drive those seats toward the beneficiary end of d; the victim declarations (honor_culture_young_men, honor_code_refusers) drive those seats toward the target end, amplified by trapped and constrained exit — a young man's socialization does not shed with geography, and the refusers' organizational shelter is partial. Identity-locked exit for the gentry and the Southern communities keeps them near the beneficiary end but without arbitrage: their benefit is inseparable from the identity the code constitutes, so they cannot capture the benefit while exiting the cost structure. The state_legal_apparatus declares no beneficiary or victim position; it sits near symmetric — it neither collects from the substrate nor is extracted against by it. Scopes are regional to national, so the engine's scope amplification is moderate; the extraction that matters most (culture-of-honor violence) sits at regional scope where verification is comparatively easy, which should damp rather than amplify the effective extraction at that seat. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat the derivation needs to distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how armed status-bearing men resolve disputes without destroying themselves in feuds — is dead in its original form: the state's violence monopoly dissolved the armed-gentry class that needed the protocol. The substrate persists anyway, in attenuated carriers doing live descendant work: military cohesion and accountability idioms, community reputation sanctioning. The classification guards against two mislabels. Calling the arrangement a snare would read its violent history as present extraction and miss that the lethal compulsion layer was removed by exogenous enforcement rather than by the arrangement's own dynamics. Calling it a mountain — honor as simply how status societies work — would misread a maintained, enforced human arrangement as natural law; the practice fell to statutes and institutional design, which no natural law does. The mandatrophy question is genuinely open rather than resolved: the original mandate is gone, the descendant functions are live, and whether the substrate's persistence is coordination or residue is exactly what the omega set tracks. founding_problem_status is authored as contested because the parties dispute it, and the corroboration asymmetry (outside attestation supports the dead-original-form reading) is recorded in the R5 field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_across_readings,
    'Was dueling''s decline caused primarily by exogenous enforcement — statutes, institutional barriers, opportunity cost — as this reading holds, or did endogenous delegitimation of the honor code do the decisive work?',
    'Comparative timing analysis across jurisdictions: correlate practice-decline curves with statute enactment, enforcement intensity, and institutional barriers, controlling for honor-culture intensity; examine whether refusal sanctions decayed before, with, or after legal prohibition in matched communities.',
    'If endogenous delegitimation dominated, this reading''s substrate-continuity premise fails and the classification migrates toward the cultural-contraction family member; if exogenous enforcement dominated, the rope-under-legal-pressure classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_across_readings, empirical, 'Causal attribution of the dueling decline across the three readings of the honor-satisfaction kernel.').

omega_variable(
    substrate_practice_decomposition,
    'Is the honor-satisfaction substrate one constraint — a norm system whose extreme satisfaction practice was suppressed — or two constraints: the satisfaction protocol itself and the broader honor norm system that outlived it?',
    'ε-stability test across observables: measure the arrangement via military honor-code operation, culture-of-honor violence rates, and heritage or ceremonial practice; if ε is stable across these, one constraint; if it diverges by a wide margin, decompose into separate linked stories.',
    'Decomposition would split this story into a suppressed-practice constraint (snare-flavored at the enforcement peak) and a live-substrate constraint (rope-flavored), each with its own stakeholder set and network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_practice_decomposition, conceptual, 'Whether the kernel is a single ε-invariant constraint or a practice/substrate pair requiring decomposition.').

omega_variable(
    attenuated_form_extraction_share,
    'In the standing arrangement, what share of the surviving substrate''s operation is extraction — compelled reputation-defense and violence risk borne by honor_culture_young_men — versus coordination?',
    'Violence epidemiology across honor- and dignity-culture populations matched on income, age structure, firearm access, and urbanicity (the Nisbett–Cohen research program and its successors), plus survey evidence on the perceived costliness of refusing reputation defense.',
    'A large compelled-violence share pushes payer-seat classifications toward tangled-rope and snare flavors despite the rope claim; a small share stabilizes the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attenuated_form_extraction_share, empirical, 'Extraction versus coordination share in the substrate''s attenuated survivals.').

omega_variable(
    substrate_persistence_mechanism,
    'Does the substrate persist because it still solves a coordination problem communities need solved, or because institutional carriers — military honor codes, heritage organizations, community churches — artificially maintain it?',
    'Track substrate vitality where carriers withdrew: honor-norm persistence among Southern-descended populations after urban migration, and in militaries that reformed their honor institutions; decay following carrier withdrawal indicates institutional carriage.',
    'Institutional carriage would make parts of the standing arrangement scaffold- or piton-like (maintained form without fully live function); self-sustaining persistence supports the rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_persistence_mechanism, empirical, 'Whether substrate persistence reflects live coordination value or institutional life support.').

omega_variable(
    honor_suppression_internalization,
    'Is the standing arrangement''s suppression structural (community sanction, institutional discipline) or internalized (reputation-defense identity formed by socialization that persists after structural sanction is removed)?',
    'Post-exit suppression trajectory: honor-community members who migrate into dignity-culture settings — if reputation-defense behavior and violence propensity persist without community sanction, the suppression is substantially internalized.',
    'Internalized suppression raises the effective suppression above the structural measure and explains persistence without enforcement; structural-only suppression predicts decay wherever enforcement is withdrawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_suppression_internalization, empirical, 'Structural versus internalized suppression mechanism in the substrate''s operation.').

omega_variable(
    coercion_grid_level_inference,
    'The coercion grid''s level-resolved values are judgments assembled from heterogeneous records (court dockets, regimental courts-martial, press accounts, ethnography) that do not measure levels uniformly — how robust is the structural-rises/individual-falls gradient?',
    'Archive work that measures each level on its own instruments: statute enforcement rates for the structural level, courts-martial and regulation records for the organizational, sanction ethnography for the individual and class levels.',
    'If the structural-level rise is an artifact of better state record-keeping rather than rising coercive force, the exogenous-enforcement reading loses its central evidentiary gradient and the composite reading gains ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_grid_level_inference, empirical, 'Robustness of the level-resolved coercion gradient underwriting the exogenous-enforcement claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 143).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_practice_decline_tr_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(honor_practice_decline_tr_t24, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(honor_practice_decline_tr_t48, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 48, 0.19).
narrative_ontology:measurement(honor_practice_decline_tr_t71, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 71, 0.24).
narrative_ontology:measurement(honor_practice_decline_tr_t95, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 95, 0.3).
narrative_ontology:measurement(honor_practice_decline_tr_t119, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 119, 0.36).
narrative_ontology:measurement(honor_practice_decline_tr_t143, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 143, 0.4).

% Extraction over time
narrative_ontology:measurement(honor_practice_decline_be_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(honor_practice_decline_be_t24, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(honor_practice_decline_be_t48, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 48, 0.54).
narrative_ontology:measurement(honor_practice_decline_be_t71, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 71, 0.49).
narrative_ontology:measurement(honor_practice_decline_be_t95, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 95, 0.46).
narrative_ontology:measurement(honor_practice_decline_be_t119, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 119, 0.44).
narrative_ontology:measurement(honor_practice_decline_be_t143, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 143, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(honor_practice_decline_su_t0, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(honor_practice_decline_su_t24, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(honor_practice_decline_su_t48, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 48, 0.64).
narrative_ontology:measurement(honor_practice_decline_su_t71, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 71, 0.65).
narrative_ontology:measurement(honor_practice_decline_su_t95, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 95, 0.58).
narrative_ontology:measurement(honor_practice_decline_su_t119, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 119, 0.52).
narrative_ontology:measurement(honor_practice_decline_su_t143, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 143, 0.48).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=143
narrative_ontology:measurement(honor_practice_decline_grid_01, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 0, 0.6).
narrative_ontology:measurement(honor_practice_decline_grid_02, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 143, 0.35).
narrative_ontology:measurement(honor_practice_decline_grid_03, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(honor_practice_decline_grid_04, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 143, 0.4).
narrative_ontology:measurement(honor_practice_decline_grid_05, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(honor_practice_decline_grid_06, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 143, 0.5).
narrative_ontology:measurement(honor_practice_decline_grid_07, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 0, 0.3).
narrative_ontology:measurement(honor_practice_decline_grid_08, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 143, 0.7).
narrative_ontology:measurement(honor_practice_decline_grid_09, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 0, 0.35).
narrative_ontology:measurement(honor_practice_decline_grid_10, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 143, 0.55).
narrative_ontology:measurement(honor_practice_decline_grid_11, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 0, 0.3).
narrative_ontology:measurement(honor_practice_decline_grid_12, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 143, 0.35).
narrative_ontology:measurement(honor_practice_decline_grid_13, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 0, 0.25).
narrative_ontology:measurement(honor_practice_decline_grid_14, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 143, 0.4).
narrative_ontology:measurement(honor_practice_decline_grid_15, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 0, 0.2).
narrative_ontology:measurement(honor_practice_decline_grid_16, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 143, 0.7).
narrative_ontology:measurement(honor_practice_decline_grid_17, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 0, 0.6).
narrative_ontology:measurement(honor_practice_decline_grid_18, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 143, 0.4).
narrative_ontology:measurement(honor_practice_decline_grid_19, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement(honor_practice_decline_grid_20, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 143, 0.45).
narrative_ontology:measurement(honor_practice_decline_grid_21, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 0, 0.5).
narrative_ontology:measurement(honor_practice_decline_grid_22, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 143, 0.55).
narrative_ontology:measurement(honor_practice_decline_grid_23, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(honor_practice_decline_grid_24, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 143, 0.5).
narrative_ontology:measurement(honor_practice_decline_grid_25, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 0, 0.6).
narrative_ontology:measurement(honor_practice_decline_grid_26, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 143, 0.4).
narrative_ontology:measurement(honor_practice_decline_grid_27, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 0, 0.7).
narrative_ontology:measurement(honor_practice_decline_grid_28, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 143, 0.45).
narrative_ontology:measurement(honor_practice_decline_grid_29, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(honor_practice_decline_grid_30, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 143, 0.5).
narrative_ontology:measurement(honor_practice_decline_grid_31, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 0, 0.15).
narrative_ontology:measurement(honor_practice_decline_grid_32, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 143, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% One kernel (honor_satisfaction_substrate), three readings, three constraints: practice_decline (this file — substrate persists, decline exogenous, rope), cultural_contraction (substrate transformed, decline endogenous), composite_overdetermined (entangled hybrid). The ε differs across the family because each reading fixes a different standing arrangement as referent: intact-substrate-under-prohibition here, transformed-substrate in the contraction reading, entangled-hybrid in the composite. The documentary record this reading organizes — statutes, courts-martial registers, refusal-sanction cases — is the same record the siblings re-read, so evidence that degrades one reading's purity propagates to the others through these edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
