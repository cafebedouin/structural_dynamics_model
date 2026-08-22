% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Expansive Humanitarian Floor (Human Rights Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes minimum
 *   humanitarian standards applicable to armed conflicts. This constraint
 *   models the expansive human-rights reading: CA3 applies automatically to
 *   ANY organized armed violence, regardless of how the state or parties
 *   classify the situation. Under this reading, attempts by states to evade
 *   humanitarian obligations through narrow definitions (calling violence
 *   'law enforcement' rather than 'armed conflict') fail—the constraint's
 *   scope is indexed to objective organized-violence criteria, not to state
 *   classification choices. The reading expands the victim set (all
 *   detainees, all affected populations) and transfers monitoring authority
 *   to international bodies. Beneficiaries are those protected by the floor;
 *   victims are those whose operational autonomy is constrained by external
 *   standards they do not control.
 *
 * KEY AGENTS:
 *   - State security forces: institutional actor bound by CA3 minimum standards; constrained exit (cannot withdraw from armed operations)
 *   - Non-state armed groups: organized actors bound equivalent to state forces; trapped (organized violence automatically triggers obligation)
 *   - Affected civilian populations: powerless beneficiaries; identity-locked (cannot exit civilian status to escape protection)
 *   - Humanitarian monitoring bodies (ICRC, UN): agenda-setters whose legitimacy expands with the expansive reading; arbitrage exit (can navigate between state classification and international standards)
 *   - State governments: institutional payers; constrained (treaty-bound; cannot withdraw without cost to sovereignty)
 *   - International Criminal Court: agenda-setter applying the expansive reading to jurisdiction; analytical exit (derivative from state treaty acceptance)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.71).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Expansive Humanitarian Floor (Human Rights Reading)").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '36073592-4e94-4b30-b155-ab82b2f01aaf').
narrative_ontology:cs_kernel_codification('36073592-4e94-4b30-b155-ab82b2f01aaf', fixed_text).
narrative_ontology:cs_authority_grounding('36073592-4e94-4b30-b155-ab82b2f01aaf', extraction).
narrative_ontology:cs_interpretation_layer_present('36073592-4e94-4b30-b155-ab82b2f01aaf').
narrative_ontology:cs_reading_relation('36073592-4e94-4b30-b155-ab82b2f01aaf', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('36073592-4e94-4b30-b155-ab82b2f01aaf', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('36073592-4e94-4b30-b155-ab82b2f01aaf', foundational, humanitarian_floor_independent_of_state_classification).
narrative_ontology:cs_axiom_status(humanitarian_floor_independent_of_state_classification, holdable).
narrative_ontology:cs_axiom_grounding('36073592-4e94-4b30-b155-ab82b2f01aaf', humanitarian_floor_independent_of_state_classification, deontological).
narrative_ontology:cs_axiom('36073592-4e94-4b30-b155-ab82b2f01aaf', foundational, organized_violence_objective_criteria_governs_scope).
narrative_ontology:cs_axiom_status(organized_violence_objective_criteria_governs_scope, holdable).
narrative_ontology:cs_axiom_grounding('36073592-4e94-4b30-b155-ab82b2f01aaf', organized_violence_objective_criteria_governs_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('36073592-4e94-4b30-b155-ab82b2f01aaf', universal_humanitarian_protection_standard).
narrative_ontology:cs_drift_state('36073592-4e94-4b30-b155-ab82b2f01aaf', contemporary_state_practice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36073592-4e94-4b30-b155-ab82b2f01aaf', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees_and_prisoners).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, humanitarian_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, military_command_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed forces and interior security apparatus subject to CA3 minimum standards in any organized armed violence context, regardless of internal conflict classification. Must provide humane treatment, medical care, and due process to detainees. Face potential international scrutiny, ICC referral, and individual criminal liability if CA3 protections are violated. Constrained because withdrawing from armed operations is not a viable exit; only compliance or violation are the real choices.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, generational, constrained, national).

% Bound by CA3 minimum standards including humane treatment of captives, no torture, no summary execution. International law regime treats them as duty-bearers equivalent to state forces under this reading. Trapped because armed organizations cannot exit the constraint without disbanding entirely; the constraint applies the moment organized violence begins, independent of their political status or the state's conflict classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, regional).

% Entitled to protection from arbitrary violence, detention without charge, torture, and enforced disappearance under CA3 minimum floor, regardless of whether the state labels the situation 'armed conflict,' 'counterinsurgency,' 'public order emergency,' or something else. Protected by an external standard that cannot be suspended by state declaration. Identity-locked because exiting civilian status to escape the constraint is not a real option (one cannot choose to become a combatant to escape civilian protection).
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations, beneficiary,
    powerless, biographical, identity_locked, national).

% All persons captured or detained in organized armed violence contexts receive CA3 protections—medical treatment, humane conditions, due process—without exception based on detaining authority's classification. Trapped because detention status itself removes mobility; the constraint defines what that detention must look like.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees_and_prisoners, beneficiary,
    powerless, biographical, trapped, national).

% ICRC, UN special rapporteurs, and human rights NGOs acquire standing to monitor and report on CA3 compliance in ANY armed violence context under this reading. Empowered to conduct prison visits, interview detainees, publish findings, and petition for investigations. Their legitimacy rests on the reading's expansive scope: the broader the constraint's reach, the broader their mandate. Can arbitrage between state classification frameworks and international standards.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, humanitarian_monitoring_bodies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, humanitarian_monitoring_bodies, agenda_setter).

% Retain administrative discretion to define the security situation internally (conflict, insurgency, crime, emergency), but under this reading that discretion cannot lower the floor of CA3 protections—any organized armed violence triggers the standard automatically. Constrained because treaty signature binds them; withdrawal from CA3 would cost sovereignty credibility and international standing. The constraint trades state classification autonomy for externally auditable minimum standards.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Under this reading, CA3 violations become automatic triggers for investigation and prosecution regardless of the state's declared classification of the conflict. The expansive reading expands ICC jurisdiction by removing the state's ability to narrow the trigger condition through definitional choices. Analytical exit because the court's role is interpretive and derivative from state treaty acceptance, but their reading of CA3 scope shapes enforcement globally.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% Policymakers designing counterinsurgency, counterterrorism, or public order responses would prefer to operate below CA3 thresholds by keeping the conflict 'law enforcement' rather than 'armed conflict.' Under this reading, that choice is removed: once violence reaches organized-armed status (independently of the state's label), CA3 applies. Excluded from the formal constraint negotiation but their preferences drive resistance to the reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_policy_makers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, state_security_policy_makers, excluded).

% Insurgent, rebel, or separatist groups face international law duties they did not negotiate and over which they have no voice in the constraint's evolution. Their exclusion from standard-setting, combined with their binding obligation, is part of the constraint's asymmetry. Trapped because the constraint applies upon their organized action, not by choice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, opposing_armed_factions, excluded,
    organized, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, humanitarian_monitoring_bodies).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal minimum floor of humane treatment that applies across all armed violence contexts, preventing any party from escaping humanitarian obligations through definitional manipulation. Solves the collective-action problem of states using narrow conflict classifications to justify unlimited force against detention, torture, and summary execution.
% TRANSFER_FUNCTION: Transfers accountability and monitoring authority from states alone (who can classify their own conflicts) to an international regime (ICRC, UN bodies, potentially ICC) that monitors CA3 compliance regardless of the state's classification choice. Also transfers restrictions on permissible state violence from the domain of state discretion to the domain of international law.
% ABSENT_VOICES: States that prefer low-threshold definitions of armed conflict (treating organized violence as 'law enforcement' or 'counterterrorism' rather than IAC/NIAC) are excluded from the constraint-setting process but bound by it; they would argue the expansive reading imposes external definitions that violate sovereignty. Non-state armed groups are similarly bound without voice—their exclusion from standard-setting is structural.
% DISAPPEARANCE_RATIONALE: If the expansive reading and its enforcement vanished, states would immediately narrow their conflict classifications to exclude their security operations from CA3 scope, detention practices would revert to state-law-only standards (varying widely by jurisdiction and regime type), and the international monitoring regime would lose standing to investigate or report on torture and extrajudicial execution in contexts the state calls 'law enforcement.' The humanitarian floor would collapse to the lowest common denominator of state practice.
% FOUNDING_PROBLEM: The Geneva Conventions framework left a critical gap: states could evade minimum humanitarian obligations by refusing to classify their armed violence as 'conflict' subject to IHL, calling it instead 'law enforcement,' 'counterterrorism,' or 'public order emergency,' and treating detainees under domestic law (which many states fail to enforce). Thousands died without protection because the state's classification choice removed international standards from application.
% FOUNDING_PROBLEM_CORROBORATION: Documented in UN fact-finding missions on counterterrorism operations (Yemen, Philippines, Sri Lanka); ICRC operational reports on detention practices in non-international armed conflicts; ICC prosecutor briefs on gaps in protection for persons detained in organized violence contexts that states classify below conflict threshold. Corroboration from humanitarian bodies and international courts independent of the human-rights advocacy sector.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the constraint transfers operational discretion from state actors to international standards. States cannot classify violence downward to escape obligations; humanitarian bodies gain monitoring standing; individuals in state custody acquire rights they cannot be denied. Suppression (0.71) is high because the constraint persists only through active international enforcement—ICRC prison visits, ICC investigations, UN reporting—and through state treaty compliance (a treaty is a form of collective enforced commitment). State security forces, despite their institutional power, cannot unilaterally evade the constraint; they must either comply or violate with international consequences. Theater ratio (0.22) is relatively low because the constraint has genuine protective function: detainees do receive medical care, torture does decrease (measuring by compliance audits), and the monitoring regime is substantive. The measurement series tracks extractiveness rising initially as the reading's scope expands and enforcement mechanisms consolidate (t0-t13), then plateauing as the constraint reaches steady-state enforcement (t13-t26). Suppression requirement rises similarly: enforcement machinery must be maintained to keep state actors compliant, but the intensity of suppression stabilizes once the standard becomes accepted practice.
 *
 * PERSPECTIVAL GAP:
 *   From the state security forces' seat, this is a Snare: they are the targets of external standards, enforcement is coercive (ICC prosecution threat), and they cannot exit or negotiate the terms. From the humanitarian monitoring bodies' seat, this is a Rope: they coordinate protection of vulnerable populations, extract no material benefit, and their role depends on genuine protective function. From state governments' seat, it is a Tangled Rope: they benefit from the predictability and legitimacy of a universal humanitarian standard (avoiding ad-hoc humanitarian crises, protecting their own populations), but they pay the cost of operational constraint (cannot conduct security operations at will). The engine computes these divergent readings from the structural data—the beneficiary/victim declarations and the exit options differentiate power atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary group: affected populations and detainees (protected by CA3 floor), humanitarian monitoring bodies (empowered to monitor and report). These agents benefit without running the system; they are beneficiary-seated. Victim group: state security forces and non-state armed groups (constrained by external standard); military command structures (lose operational discretion). These are payer-seated. State governments sit dual: they pay (constrained, must comply) and benefit (universal standard protects their own populations, provides legitimacy). Exit options differentiate: state forces are constrained (bound by treaty obligation); humanitarian bodies are arbitrage-mobile (can navigate between state and international standards); civilians are identity-locked (cannot exit civilian status). These feed directionality: beneficiaries with mobile or arbitrage exits sit lower d; constrained targets sit higher d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy: the founding problem (states evading humanitarian obligations through narrow classification) remains live; the constraint persists because the coordination function (universal floor preventing classification evasion) remains necessary; and the founding problem's status is witnessed by independent humanitarian bodies (ICRC operational data, UN fact-finding missions). The constraint is not a vestige. However, there is theater growth: over the interval, states increasingly perform CA3 compliance (training programs, detention-review mechanisms) while operational practices sometimes diverge from formal standards—theater_ratio rises from 0.14 to 0.22. This suggests not mandatrophy (the function is real) but compliance-theater divergence: states comply performatively more than substantively, and the suppression requirement rises (more enforcement machinery needed to maintain the appearance of compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organized_violence_threshold_ambiguity,
    'What level of organization, coordination, and intensity in armed violence triggers CA3''s application under the expansive reading? Is it any group with structured command? Any sustained violence involving weapons? Any coordination at all?',
    'Case law from ICC, ad hoc tribunals, and ICRC operational practice: examine which fact patterns triggered CA3 application and which were excluded. Look for convergence on an operative definition.',
    'A narrow threshold (strict organization + sustained operations) preserves some state discretion to exclude isolated incidents; a broad threshold (any coordinated violence) maximizes constraint scope and minimizes state evasion room. The measured extractiveness depends on where the threshold sits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_violence_threshold_ambiguity, empirical, 'The factual trigger for CA3 scope under the expansive reading.').

omega_variable(
    state_classification_vs_objective_criteria,
    'Is the expansive reading truly independent of state classification, or does it implicitly defer to state findings of fact (the state finds violence was organized, therefore CA3 applies)?',
    'Examine cases where international bodies rejected a state''s classification as ''law enforcement'' and independently applied CA3. Document frequency of override.',
    'If independence is genuine, the constraint truly extracts state discretion; if the reading defers to state factual findings, the constraint is more performative—states retain gatekeeping power by controlling the evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_classification_vs_objective_criteria, empirical, 'Whether the expansive reading removes state definitional power or only harmonizes standards given shared factual premises.').

omega_variable(
    enforcement_capacity_constraint,
    'The expansive reading depends on international enforcement (ICC, ICRC monitoring, UN reporting). How much of the apparent constraint is the reading itself, and how much is contingent on enforcement infrastructure that may be overextended, politically compromised, or resourced inadequately?',
    'Compare actual enforcement (number of investigations, prosecutions, documented visits, published reports) to the scope of potential violations across all organized armed violence contexts. Measure enforcement-to-violation ratio.',
    'If enforcement capacity is substantially below potential violations, the constraint is more Piton than Tangled Rope—the floor exists formally but enforcement is theater, and beneficiaries (detainees, affected populations) receive real protection only where enforcement is active.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_constraint, empirical, 'Whether the expansive reading''s coordination function is limited by enforcement capacity.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the expansive reading''s core premise (CA3 applies regardless of state classification) logically foreclose the state_centric reading''s core premise (CA3 applies only to conflicts meeting defined thresholds), or do they represent different emphasis/strategy choices that could coexist in principle?',
    'Examine whether a single actor could hold both premises without internal contradiction. State-centric: CA3 applies only when violence meets thresholds. Expansive: CA3 applies when violence meets thresholds (objective criteria) regardless of state label. The premises could coexist if ''threshold'' is objective—but if threshold is state-discretionary, they foreclose each other.',
    'If foreclosure is real, the two readings cannot be reconciled in one framework. If they coexist, they are policy alternatives held by different parties rather than contradictory epistemologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the expansive and state-centric readings are logically incompatible or represent different policy preferences.').

omega_variable(
    human_rights_vs_humanitarian_framing,
    'Does the expansive reading''s grounding in universal human dignity (the human-rights framing) produce a different scope or enforcement mechanism than a purely humanitarian (minimalist protection) framing?',
    'Compare human-rights interpretations of CA3 (dignity, autonomy, right to trial) with humanitarian interpretations (suffering reduction, medical treatment access). Do they produce different victim sets or remedies?',
    'If framing choice produces different scope, then what appears to be one reading (expansive) is actually two. If framing produces the same victim set but different rhetorical ground, then the readings are one constraint with alternative justifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_vs_humanitarian_framing, conceptual, 'Whether the human-rights vs humanitarian framing difference marks distinct constraints or one constraint with alternative justifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(comm_tr_t4, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(comm_tr_t8, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(comm_tr_t13, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 13, 0.21).
narrative_ontology:measurement(comm_tr_t18, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(comm_tr_t26, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 26, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(comm_be_t4, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(comm_be_t8, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(comm_be_t13, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 13, 0.67).
narrative_ontology:measurement(comm_be_t18, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(comm_be_t26, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 26, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(comm_su_t4, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(comm_su_t8, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(comm_su_t13, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 13, 0.7).
narrative_ontology:measurement(comm_su_t18, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(comm_su_t26, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 26, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__expansive_human_rights_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, icc_jurisdiction__armed_conflict_threshold).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, torture_prohibition__absolute_vs_state_exception).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Common Article 3 scope kernel. The state_centric_reading (narrower scope, state classification controls application) and icrc_customary_reading (scope determined by state practice evolution) are separate constraint stories. All three share the same kernel text but produce different victim sets, beneficiary structures, and enforcement regimes. The expansive reading maximizes external monitoring standing and minimizes state evasion room; the state_centric reading preserves state sovereignty over conflict classification; the ICRC reading anchors scope to observed state behavior. These three stories form a constraint family linked by network.affects_constraints. This story (expansive_human_rights_reading) forecloses the state_centric reading's core premise (state discretion over classification) while coexisting with the ICRC reading (which produces a middle-ground scope through practice tracking).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
