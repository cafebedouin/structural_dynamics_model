% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligations as Destructive Extraction Cycle
 *   domain: legal/political/economic
 *
 * SUMMARY:
 *   This constraint instantiates the extraction-cycle reading of the
 *   feud-obligation kernel. In this reading, blood-feud obligations enforced
 *   by royal legal doctrine and ecclesiastical authority extract productive
 *   capacity from kin groups and territorial occupants by mandating
 *   participation in violence, creating cycles of retaliation and
 *   compensation that deplete labor, wealth, and security while preventing
 *   rival political entities from consolidating alternative governance
 *   structures. The kernel itself—the binding commitment that kinship creates
 *   enforceable obligation to pursue or settle blood injuries—is
 *   reinterpreted here NOT as a coordination mechanism providing justice
 *   without centralized authority, but as a rent-extraction device that
 *   benefits centralizing powers by fragmenting territorial organization. The
 *   reading asserts that royals and ecclesiastical authorities enforced this
 *   obligation structure precisely because it prevented subordinate groups
 *   from consolidating independent authority, making them dependent on royal
 *   arbitration and taxation.
 *
 * KEY AGENTS:
 *   - kin_group_obligation_bearers: Bound by identity to participate in feuds; direct mortality and resource depletion burden; identity_locked exit
 *   - territorial_occupants: Powerless, trapped by geography; violence spillover and disrupted production; no standing in feud system
 *   - royal_centralizing_authority: Institutional, arbitrage exit; benefits from feud fragmentation preventing rival consolidation; administers obligation enforcement
 *   - ecclesiastical_authority: Institutional, arbitrage exit; monopolizes compensation and forgiveness; collects fees for mediation
 *   - powerful_war_leaders: Powerful, mobile exit; leverage obligatory kinship mobilizations into military followings without direct extraction
 *   - productive_agricultural_class: Powerless, constrained exit; loses labor to feud participation and security to violence spillover
 *   - rivals_to_royal_consolidation: Organized, trapped; would consolidate alternative authority but are legally prohibited by feud framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.82).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligations as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal/political/economic").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '914fa1a9-f354-451a-bd9d-811ca54000ed').
narrative_ontology:cs_kernel_codification('914fa1a9-f354-451a-bd9d-811ca54000ed', fixed_text).
narrative_ontology:cs_authority_grounding('914fa1a9-f354-451a-bd9d-811ca54000ed', extraction).
narrative_ontology:cs_interpretation_layer_present('914fa1a9-f354-451a-bd9d-811ca54000ed').
narrative_ontology:cs_reading_relation('914fa1a9-f354-451a-bd9d-811ca54000ed', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('914fa1a9-f354-451a-bd9d-811ca54000ed', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('914fa1a9-f354-451a-bd9d-811ca54000ed', foundational, feud_persistence_signals_extraction).
narrative_ontology:cs_axiom_status(feud_persistence_signals_extraction, holdable).
narrative_ontology:cs_axiom_grounding('914fa1a9-f354-451a-bd9d-811ca54000ed', feud_persistence_signals_extraction, empirically_contingent).
narrative_ontology:cs_axiom('914fa1a9-f354-451a-bd9d-811ca54000ed', foundational, mandatory_kinship_obligation_is_enforced).
narrative_ontology:cs_axiom_status(mandatory_kinship_obligation_is_enforced, holdable).
narrative_ontology:cs_axiom_grounding('914fa1a9-f354-451a-bd9d-811ca54000ed', mandatory_kinship_obligation_is_enforced, empirically_contingent).
narrative_ontology:cs_reference_frame('914fa1a9-f354-451a-bd9d-811ca54000ed', kinship_based_mandatory_obligation).
narrative_ontology:cs_drift_state('914fa1a9-f354-451a-bd9d-811ca54000ed', post_centralization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('914fa1a9-f354-451a-bd9d-811ca54000ed', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, centralizing_royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutional_power).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kin_group_members).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, territorial_occupants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, productive_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_centralizing_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, powerful_war_leaders).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kin_group_obligation_bearers).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, productive_agricultural_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship identity to participate in feuds triggered by injury or death to kin, regardless of personal involvement in the precipitating incident. Must invest time, resources, and risk of death in pursuit, settlement, or retaliation cycles. Exit is impossible without abandoning family identity and protection. Bear the mortality and resource depletion directly.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kin_group_obligation_bearers, payer,
    moderate, biographical, identity_locked, regional).

% Inhabit lands crossed by feuding kin groups or serve as collateral parties. Experience routine violence spillover, property destruction, and disrupted production. Have no formal claim to dispute resolution outside the feud mechanism itself. Cannot exit without abandoning livelihood and settlement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, territorial_occupants, payer,
    powerless, biographical, trapped, regional).

% Enforces the feud obligation framework through legal doctrine and violence monopoly claims. Benefits because blood-feud cycles prevent rival power centers from consolidating territorial authority independent of the crown; feud fragmentation legitimizes royal intervention as peacemaker and tax collector. Administers compensation scales, enforces participation in feuds mandated by law, and collects fees for arbitration, forgiveness, or exemption. Can exit enforcement at will by declaring new legal authority.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_centralizing_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_centralizing_authority, beneficiary).

% Gains institutional power by positioning itself as the exclusive authority on forgiveness, penance, and divine justice. Administers compensation (wergeld) schedules and hosts negotiations for feud settlement. Collects fees, donations, and lands as part of settlement infrastructure. Can declare new moral doctrine (e.g., pacification theology) to shift the obligation structure without constraint.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authority, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authority, agenda_setter).

% Use feud obligations to mobilize armed followings for territorial conquest and defense without direct resource extraction. Obligatory kinship mobilizations become military advantage; the feud framework legitimizes standing armies. Can exit by abandoning kinship networks and relocating, or by leveraging feud participation into higher institutional office.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, powerful_war_leaders, beneficiary,
    powerful, biographical, mobile, regional).

% Lose productive labor to feud participation (conscription by obligation holders), lose harvests to violence spillover and forced requisitions to fund feuds, and lose security for cultivation. Cannot refuse participation without social death. The feud cycle prevents territorial consolidation that would bring stable governance and protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, productive_agricultural_class, payer,
    powerless, biographical, constrained, regional).

% Would benefit from territorial consolidation under rival authority or federation of kin groups with independent justice. The feud obligation framework legally mandates their subordination to royal arbitration and prevents them from using consolidated violence or justice institutions to challenge crown authority. Their exclusion from redefining the feud framework is the enforcement object.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, rivals_to_royal_consolidation, excluded,
    organized, generational, trapped, national).

% Record feud cycles and their outcomes; can compare institutional readings and assess whether feuds served coordination or extraction. Neutral analytical seat; their assessments inform competing interpretations.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, historical_chroniclers, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None declared under this reading. The extraction-cycle reading denies that feuds provide genuine justice or deterrence; it claims the coordination function (if present) is incidental cover for extractive mechanisms that benefit centralizing authority and ecclesiastical power.
% TRANSFER_FUNCTION: Moves productive capacity (labor, land, harvests, accumulated wealth) and mortality risk from kin groups and territorial occupants to war leaders and centralizing authorities. The transfer mechanism is obligation-bearing identity (kinship) that cannot be exited without social death; the extraction is secured by royal legal doctrine declaring feud participation mandatory and by ecclesiastical institutions monopolizing compensation and forgiveness.
% ABSENT_VOICES: Territorial occupants without kin-group affiliation (slaves, non-kin servants, foreign merchants) would argue for security and production stability rather than endless feud cycles, but have no legal standing in the feud framework. Rival political authorities would advocate for alternative justice mechanisms (secular courts, federation structures) but are excluded from redefining the kernel.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight—if kinship no longer mandated violent response and royal authority ceased enforcing participation—productive capacity would consolidate rapidly, territorial governance would stabilize, and the peasantry would shift resources from violence and compensation to agriculture. Royal tax income would initially drop (feud infrastructure fees end) but would stabilize at higher levels through stable production. War leaders would lose the free mobilization mechanism and would need to hire or manage standing armies differently. The entire political landscape would reorganize.
% FOUNDING_PROBLEM: In early medieval polities without centralized enforcement institutions, private justice (blood feud) provided the only available deterrent against injury without legal recourse. Kin-group obligation ensured response and retaliation, creating fear of consequences for aggression.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical analysis (external to benefiting parties) establishes that by the consolidation period (9th–12th centuries), centralized royal authority had the capacity to provide alternative justice mechanisms. Contemporary chronicles from non-royal, non-ecclesiastical sources (merchant records, peasant revolts) document that feuds persisted NOT because no alternative existed, but because royal and ecclesiastical authorities enforced the obligation structure to maintain fragmentation and extraction. Legal historians attest that royal charters explicitly mandate feud participation and prohibit secular alternative justice.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 (early medieval period when feud obligation was less systematically enforced) to 0.82 (consolidation period when royal doctrine mandates participation and ecclesiastical compensation is institutionalized). The measurement trajectory reflects increasing capture: as royal authority crystallizes, the obligation structure becomes more thoroughly extracted for state benefit. Suppression rises from 0.52 to 0.78 over the interval, tracking the royal enforcement buildup—kinship-based enforcement alone cannot sustain the cycle against alternative justice frameworks; active royal legal suppression of alternatives is required. Theater ratio rises from 0.08 to 0.28, indicating that over time the justice/deterrence function claims grow theatrically more elaborate (elaborate compensation schedules, ecclesiastical mediation theater) while the actual mechanism is increasingly exposed as extraction—the constraint persists despite the founding problem (absence of alternatives) being solved. The plateau at t=40 indicates the constraint reaches its stable extractive state: equilibrium between suppression needed to maintain it and theater adequate to mask its function.
 *
 * PERSPECTIVAL GAP:
 *   The royal authority's analytical reading of this constraint would describe it as necessary peace-enforcement and state-building (preventing anarchy, taxing for public order). The kin-group bearer's reading describes it as mandatory participation in cycles that destroy their households. From the royal seat the constraint provides coordination benefits (peace, justice); from the kin-group seat it is pure extraction. The engine computes both per-seat types from the structural data without adjudicating which framing is correct. The measurement series showing rising extractiveness and theater ratio supports the extraction reading, but the claim/metric independence principle means the authored claim (snare) and the metrics (high extraction, high suppression) are independent authored facts, and their alignment is the diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   From the kin-group bearer's seat, d approaches 1.0 (full target): they participate involuntarily, bear mortality and resource cost, cannot exit without identity death, and benefit not at all. From the royal authority's seat, d approaches 0.0 (full beneficiary): they set rules, collect fees, benefit from fragmentation preventing rival consolidation, and can exit enforcement whenever advantageous. The engine derives these divergent d values from the structural data: identity_locked exit for kin-groups (d→1.0), arbitrage exit for royals (d→0.0). Ecclesiastical authority sits similarly beneficiary-positioned. War leaders occupy a complex middle position (powerful+mobile but locked into the obligation structure by need for military followings); their d sits around 0.3-0.4. Territorial occupants are pure targets (d→1.0) due to trapped exit. This heterogeneity within the 'victim' set reflects that some pay by obligation and some pay by geography; the engine computes these seat-by-seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the stateless-coordination reading, blood feuds solve the founding problem (providing deterrence without centralized authority). Under the extraction-cycle reading authored here, blood feuds PERSIST after the founding problem is solved because centralizing authorities enforce the obligation structure to prevent rival consolidation. The founding_problem_status is dead: alternative justice mechanisms existed by the consolidation period, yet feuds persisted. This mismatch between dead founding problem and persistent constraint is mandatrophy—the arrangement outlived its functional justification and became pure extraction. The measurements track mandatrophy accumulation: theater ratio rising while extractiveness plateaus indicates elaborate justification theater (wergeld schedules, ecclesiastical mediation) growing to maintain a constraint whose founding function is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforced_obligation_vs_genuine_deterrence,
    'Did kin groups participate in feuds because they genuinely deterred aggression and provided justice (stateless-coordination reading), or because royal law mandated participation and suppressed alternatives (extraction-cycle reading)?',
    'Comparative analysis of jurisdictions where royal enforcement weakened or where alternative justice mechanisms were explicitly permitted: if feuds declined absent enforcement, participation was mandated, not voluntary; if they persisted despite legal alternatives, coordination function was real.',
    'If participation was genuinely voluntary (coordination), the constraint is rope or tangled_rope with substantial beneficiary function; if enforced (extraction), the classification as snare with identity-locked suppression is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforced_obligation_vs_genuine_deterrence, empirical, 'Whether feud participation was enforced obligation or voluntary coordination.').

omega_variable(
    productive_capacity_depletion_mechanism,
    'Was productive capacity depletion a direct effect of feud cycles, or an incidental side effect of any justice mechanism (including coordination)?',
    'Economic analysis comparing productive output in high-feud vs. low-feud regions, controlling for alternative institutional variables (centralization, literacy, trade access). Comparison with later periods after feud suppression showing productivity gains.',
    'If depletion was unique to the extraction reading (i.e., avoided by stateless-coordination because deterrence is proportional), it distinguishes the reading empirically. If depletion occurs under any justice mechanism, it is a side effect, not an extraction signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productive_capacity_depletion_mechanism, empirical, 'Whether productive capacity depletion is specific to the extraction cycle or generic to all feud operation.').

omega_variable(
    alternative_institution_suppression,
    'Did royal authorities actively suppress non-feud justice mechanisms (merchant courts, monastic arbitration, federation justice), or did feuds persist because no alternatives were available?',
    'Royal charters, ecclesiastical records, and trade documentation showing prohibition or exemption patterns: if authorities granted exemptions and then revoked them, suppression was active; if they always prohibited alternatives, suppression is structural.',
    'Active suppression (demonstrated prohibition-then-revocation or chartered exemptions later withdrawn) confirms that the obligation structure was enforced against emerging alternatives, supporting the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institution_suppression, empirical, 'Whether feud obligation enforcement included active suppression of rival justice mechanisms.').

omega_variable(
    kinship_identity_lock_mechanism,
    'What specific mechanisms made kinship-based feud obligation impossible to exit? Was it social death (complete loss of protection and standing), legal execution, or economic ruin?',
    'Historical records of exit attempts, charter penalties for feud refusal, monastic records of refugees claiming exemption, and patterns of kinship abandonment.',
    'If exit carried severe identity penalties (social death), the identity_locked classification is correct. If exit was merely costly (economic ruin), the classification should be constrained. If exit was physically prevented (execution), identity is irrelevant and the constraint is trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kinship_identity_lock_mechanism, empirical, 'The mechanism by which kinship obligation became identity-locked.').

omega_variable(
    reading_contest_framing_ambiguity,
    'Is the contest between extraction-cycle and stateless-coordination readings rooted in empirical disagreement about what feuds actually did (deterred vs. extracted), or in normative disagreement about whether mandatory participation is legitimate?',
    'Separate empirical questions (did feuds deter aggression? did they deplete production?) from normative questions (should kinship mandate violence? should royals enforce mandatory participation?). An empirical disagreement routes to the empirical omegas above; a normative disagreement routes to the axiom_overriding drift.',
    'If the contest is empirical, resolution comes from historical evidence about deterrence and productivity. If normative, resolution comes from axiom shifts (e.g., ecclesiastical authority rejecting the legitimacy of mandatory kinship obligation). The extraction reading asserts a empirical-core disagreement: feuds EXTRACT because they persist after the founding problem is solved, independent of whether extracting is normatively legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_framing_ambiguity, conceptual, 'Whether the reading contest is empirical or normative disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(feud_tr_t5, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(feud_tr_t15, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(feud_tr_t30, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(feud_tr_t35, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(feud_be_t5, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(feud_be_t15, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(feud_be_t30, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(feud_be_t35, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(feud_su_t5, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(feud_su_t15, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(feud_su_t30, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(feud_su_t35, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The feud-obligation kernel admits three structurally distinct readings: (1) stateless_coordination_reading interprets blood feuds as self-enforcing deterrence and justice when centralized authority is absent; (2) christianized_pacification_reading interprets feuds as violations of divine law requiring ecclesiastical/royal replacement; (3) extraction_cycle_reading (this file) interprets feuds as enforced extraction mechanisms that fragment political authority and prevent territorial consolidation. Each reading has distinct ε, distinct beneficiary/victim structure, and distinct classification. The readings are linked via network.affects_constraints because each sibling reading contests the same kernel (the binding commitment that kinship creates obligation to pursue blood injury) and would foreclose or reframe the others if adopted as authoritative. The extraction-cycle reading influences both siblings: if extraction-cycle is confirmed (founding problem dead, active suppression of alternatives), stateless-coordination is undermined (not voluntary deterrence but enforced) and pacification-theology becomes necessary to displace the extraction (religious authority must override kinship obligation to break the cycle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
