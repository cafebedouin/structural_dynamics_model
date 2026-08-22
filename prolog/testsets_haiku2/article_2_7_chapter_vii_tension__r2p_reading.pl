% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: R2P Reading: Sovereignty Conditional on Population Protection
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) reading of Article 2(7) and Chapter
 *   VII of the UN Charter conditions state sovereignty on the state's
 *   capacity and willingness to protect its populations from systematic
 *   atrocity. When that protection fails catastrophically, the reading
 *   invokes international responsibility to intervene, overriding the
 *   traditional non-intervention principle. This is the dominant
 *   humanitarian-law reading, endorsed by the UN General Assembly, human
 *   rights bodies, and liberal-democratic coalitions. It competes with a
 *   sovereignty-first reading that treats Article 2(7) as a near-absolute bar
 *   to intervention absent interstate aggression. The R2P reading establishes
 *   high extractiveness (0.68): it legitimizes intervention that targets
 *   states bear as costs, transfers jurisdiction to intervening coalitions,
 *   and erodes the foundational sovereignty norm. The measurement series
 *   shows extractiveness rising from 0.52 to 0.68 (t0 to t20) then
 *   plateauing—the reading's institutional weight solidified by the mid-2010s
 *   and has stabilized despite contested application (Libya, Syria, South
 *   Sudan). Theater ratio rises to 0.41 by t15 and stays flat—indicating that
 *   rhetoric about protecting populations increasingly obscures divergent
 *   interests in intervention (geopolitical advantage, regional power
 *   assertion, military resource deployment). Suppression requirement rises
 *   steadily (0.54 to 0.72), reflecting that maintaining the R2P norm
 *   requires ongoing rhetorical and institutional suppression of the
 *   sovereignty-first reading—states that reject R2P are excluded from the
 *   conversation, their legal arguments marginalized, their veto capacity
 *   circumscribed by coalition action outside the Security Council.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.72).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "R2P Reading: Sovereignty Conditional on Population Protection").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '3c9e1842-d576-4778-84a4-d9045c7335d5').
narrative_ontology:cs_kernel_codification('3c9e1842-d576-4778-84a4-d9045c7335d5', fixed_text).
narrative_ontology:cs_authority_grounding('3c9e1842-d576-4778-84a4-d9045c7335d5', lineage).
narrative_ontology:cs_interpretation_layer_present('3c9e1842-d576-4778-84a4-d9045c7335d5').
narrative_ontology:cs_reading_relation('3c9e1842-d576-4778-84a4-d9045c7335d5', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('3c9e1842-d576-4778-84a4-d9045c7335d5', foundational, sovereignty_conditional_on_population_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_population_protection, holdable).
narrative_ontology:cs_axiom_grounding('3c9e1842-d576-4778-84a4-d9045c7335d5', sovereignty_conditional_on_population_protection, deontological).
narrative_ontology:cs_axiom('3c9e1842-d576-4778-84a4-d9045c7335d5', foundational, systematic_atrocity_triggers_collective_responsibility).
narrative_ontology:cs_axiom_status(systematic_atrocity_triggers_collective_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('3c9e1842-d576-4778-84a4-d9045c7335d5', systematic_atrocity_triggers_collective_responsibility, deontological).
narrative_ontology:cs_reference_frame('3c9e1842-d576-4778-84a4-d9045c7335d5', universal_human_rights_over_state_sovereignty).
narrative_ontology:cs_drift_state('3c9e1842-d576-4778-84a4-d9045c7335d5', contemporary_post_libya_backlash_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c9e1842-d576-4778-84a4-d9045c7335d5', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_community_humanitarian_coalition).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_sovereignty).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_intervening_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_coalition).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, un_security_council_permanent_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity—mass killing, ethnic cleansing, genocide, crimes against humanity. Under the R2P reading, their protection overrides the targeted state's sovereignty claim. They have no exit; their survival depends on whether the international community recognizes the atrocity threshold and intervenes. They are the moral and legal beneficiaries of the constraint's operation, yet they control neither its invocation nor its timing.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% Loses the foundational claim to exclusive jurisdiction and non-intervention. Under the R2P reading, sovereignty is conditional—it persists only insofar as the state protects its populations. If systematic atrocity occurs, the state's right to refuse international intervention evaporates. The state bears the cost of erosion of the non-intervention norm and the reputational, economic, and military consequences of intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_sovereignty, payer,
    institutional, generational, trapped, national).

% Set the agenda for recognizing atrocity thresholds, authorizing intervention (via UN Security Council or claimed responsibility), and determining the scope and timing of action. They justify intervention as humanitarian duty and legal obligation under the R2P framework. Their decisions directly trigger enforcement of the constraint and allocation of intervention costs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, agenda_setter,
    powerful, biographical, mobile, global).

% Bear diffuse costs from interventions they do not authorize or support: destabilization of regions, refugee flows, economic disruption, and diplomatic friction. They are implicitly bound by the R2P reading even when they oppose intervention in specific cases. Exit options are constrained—withdrawing from international institutions is costly, but so is bearing intervention costs without control.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_intervening_states, payer,
    organized, generational, constrained, global).

% Control formal authorization of coercive action under Chapter VII. Under the R2P reading, they face pressure to approve humanitarian intervention when atrocities occur, but they also retain veto power—they can block intervention, accept it, or demand conditions. Their position is structurally dual: they set the agenda for authorization while also bearing costs (military commitment, regional consequences, precedent implications).
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, un_security_council_permanent_members, payer).

% Includes human rights organizations, advocacy networks, and liberal-democratic states that champion the R2P principle. They benefit from the elevation of population protection over state sovereignty: it legitimizes their advocacy, enables institutional action, and provides a legal framework for humanitarian intervention. They actively push for recognition of atrocity thresholds and intervention authorization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_coalition, beneficiary,
    organized, biographical, mobile, global).

% Reject the R2P reading and defend absolute state sovereignty. They would argue that intervention requires explicit consent or inter-state aggression (not intra-state atrocity) to justify Chapter VII action. Their exclusion from the conversation is enforced by the majority reading and the institutional weight of the UN General Assembly and regional bodies that have endorsed R2P.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_states, excluded,
    powerful, generational, constrained, global).

% Interprets and adjudicates the relationship between sovereignty and human rights. The International Court of Justice, International Criminal Court, and regional human rights courts refine the atrocity threshold, determine state responsibility, and assess whether intervention was lawful. They observe the constraint's operation and provide post-hoc legal legitimation or contestation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_court_system, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared international norm and enforcement mechanism: when systematic atrocities reach a defined threshold, the international community recognizes a collective obligation to protect populations, overriding the targeted state's sovereignty claim. This solves the free-rider problem of humanitarian intervention—without a binding norm, states defer to others or remain silent; with R2P, the obligation becomes collective and enforceable.
% TRANSFER_FUNCTION: Transfers the right to exclusive jurisdiction from the targeted state to the intervening coalition and the international community. The targeted state loses the capacity to refuse intervention when atrocities occur. Intervening states transfer military, financial, and diplomatic resources to enforce the intervention. Non-intervening states transfer sovereignty over regional affairs to the intervening coalition.
% ABSENT_VOICES: States that reject the R2P reading—particularly authoritarian regimes, regional powers invested in sovereignty-first frameworks, and nations skeptical of Western-led intervention—are structurally excluded from the conversation. They are not represented in the decision to intervene; their sovereignty and strategic interests are overridden by the majority reading. They would argue that R2P is a disguise for neo-colonial intervention and hegemonic power assertion.
% DISAPPEARANCE_RATIONALE: If the R2P reading and its enforcement vanished, targeted states would regain absolute sovereignty over internal affairs, intervening coalitions would lose the legal framework for humanitarian intervention, and persecuted populations would face atrocities without international rescue. The international legal order would reorganize around state sovereignty as foundational—the sovereignty-first reading would become the default. Humanitarian intervention would persist but would lack the legitimacy the R2P norm provides.
% FOUNDING_PROBLEM: Systematic atrocities within state borders—genocide, ethnic cleansing, crimes against humanity—occurred with international impunity. The Rwandan genocide (1994), Srebrenica (1995), and other mass atrocities exposed the inadequacy of the non-intervention norm for protecting populations at catastrophic risk. States claimed sovereignty prevented international rescue; the international community lacked a legal framework to override that claim.
% FOUNDING_PROBLEM_CORROBORATION: The humanitarian coalition and the UN General Assembly (who endorsed R2P in 2005) attest the founding problem was urgent and the response necessary. Targeted states, sovereignty-first powers, and scholars of international law outside the humanitarian advocacy sphere contest both the severity and the solution—they argue the founding problem was real but the R2P response overreaches, creating new injustices (interventionism, regime change, destabilization) while claiming to solve the old one. Independent analysis of post-R2P interventions (Libya, Syria, South Sudan) documents both humanitarian gains and unintended harms, validating the contested status.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the R2P reading systematically transfers jurisdiction and overrides state consent. Suppression is high (0.72) because enforcement requires active suppression of alternative readings and the veto power of sovereignty-first states. Theater is moderate (0.41) because humanitarian rhetoric is genuine (the underlying coordination problem—how to rescue populations from atrocity—is real) but increasingly serves as cover for strategic intervention by powerful states. Accessibility collapse is moderate (0.58) because alternatives (sovereignty-first reading, non-intervention) remain intellectually live and institutionally defended by major powers, even as they are excluded from operational decision-making. Resistance is high (0.74) because targeted states, sovereignty-first powers, and skeptical scholars mount sustained resistance to the R2P framework. The measurement trajectory shows the constraint hardening over time: extractiveness and suppression requirement both rise and plateau, indicating the reading moved from contested proposal (2005 General Assembly endorsement) to institutional establishment (Libya intervention 2011 as exemplar; subsequent decline in invocation as backlash mounted). Theater ratio's rise suggests the gap between humanitarian justification and strategic implementation widened as the reading matured.
 *
 * PERSPECTIVAL GAP:
 *   From the humanitarian coalition's seat, the R2P reading is genuine coordination: it solves the atrocity-protection problem and is justified by universal human rights. From the targeted state's seat, it is enforced extraction: a powerful coalition overrides sovereignty and imposes costly intervention. From the sovereignty-first state's seat, it is norm-erosion: the reading delegitimizes the foundational non-intervention principle and creates precedent for hegemonic intervention. From the persecuted population's seat, the reading is aspirational but unreliable—it promises protection but delivery depends on geopolitical factors (whether powerful states view intervention as in their interest) unrelated to the severity of the atrocity. The engine computes these perspectives from the structural data (power, exit, beneficiary/victim status); the authored claim (tangled rope) reflects that the constraint both solves a real coordination problem (how to respond to mass atrocity) and functions as extraction (powerful states override others' sovereignty for strategic gain). The perspective gaps are the differential experience of the same constraint from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations (d ≈ 0.0) are nominal beneficiaries but have zero agency—their benefit is conditional on others' decisions. Intervening coalition states (d ≈ 0.2-0.4) benefit from the norm's legitimacy and the absence of veto constraints; they are agenda-setters, not pure targets. Non-intervening states (d ≈ 0.6) bear costs (destabilization, refugee flows, friction) without control. Targeted states (d ≈ 0.9-1.0) are the primary targets—they lose sovereignty and control over internal affairs. Sovereignty-first states (d ≈ 0.7-0.8) bear reputational and strategic costs from being excluded and overridden. The UN Security Council permanent members sit at dual positions (d ≈ 0.4-0.6): they set the authorization agenda but also bear military and diplomatic costs, and they face pressure to override their veto power when humanitarian sentiment is high. The power differentials and exit options drive these directionalities: powerless populations and trapped states have no exit; institutional players with mobile or arbitrage options (intervening coalitions, P5 members with exit paths) sit lower on the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy. The founding problem (systematic atrocity without international remedy) remains live—Libya, Syria, South Sudan, and Myanmar atrocities after 2005 validate ongoing need. The founding-problem status is contested: the humanitarian coalition attest the problem is live and R2P is the solution; sovereignty-first powers attest the problem was real but the solution creates worse problems (intervention wars, regime change, destabilization). The disappearance verdict is world_rearranges—without R2P, targeted states would regain sovereignty and interventions would lose legitimacy. The discrepancy between the contested-status founding problem and the world_rearranges disappearance verdict indicates the constraint is institutionally entrenched (disappearing would cause reorganization) even though its justification remains disputed. No mandatrophy flag is warranted: the constraint's function (protecting persecuted populations) remains contested but live; it is not a vestigial norm persisting past its utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_ambiguity,
    'What constitutes the atrocity threshold that triggers R2P intervention? Is it purely mass killing (genocide), or does it extend to ethnic cleansing, crimes against humanity, and systematic oppression?',
    'Post-atrocity institutional review: which atrocities triggered intervention (Libya, Syria, South Sudan, Myanmar, etc.), and what criteria did intervening coalitions apply? Contrast against atrocities that did not trigger intervention to identify the implicit threshold.',
    'A narrow threshold (genocide only) limits intervention scope and reduces extraction; a broad threshold (any systematic human rights abuse) expands intervention authority and increases state sovereignty erosion. The threshold determines the constraint''s effective scope and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_ambiguity, empirical, 'The operative definition of systematic atrocity that justifies intervention.').

omega_variable(
    geopolitical_selectivity_vs_principle,
    'Does the R2P reading operate as a principle (applied consistently across cases regardless of geopolitical interest) or as a selective tool (applied when powerful states benefit from intervention, ignored when intervention would be costly)?',
    'Empirical audit: compare cases where atrocities occurred (Rwanda, Syria, Myanmar, Cambodia, Darfur, etc.) and trace which triggered intervention and which did not. Analyze the relationship between intervention and the geopolitical interests of permanent Security Council members.',
    'If the reading operates as principle, it is a genuine coordination mechanism (high legitimacy, moderate extraction). If selective, it is cover for strategic extraction by powerful states (high extraction, high theater ratio). Selectivity raises the constraint from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_selectivity_vs_principle, empirical, 'Whether R2P operates as principle or strategic tool.').

omega_variable(
    sovereignty_conditionality_reversibility,
    'If a state protects its populations from atrocity and thus regains the full shield of sovereignty, can it later lose that protection again? Is the conditionality reversible, or does sovereignty regain permanence once demonstrated?',
    'Textual analysis of R2P doctrine and institutional practice: does the framework establish a once-and-for-all sovereignty recovery, or a continuous condition subject to revocation if atrocities recur?',
    'Reversibility makes the constraint a continuous external audit of state behavior (higher extraction, higher suppression). Non-reversibility allows states to recover full sovereignty after demonstrating capacity (lower extraction, more stable). The choice affects whether the R2P reading is extractive or coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_conditionality_reversibility, conceptual, 'Whether sovereignty recovery from R2P intervention is permanent or conditional.').

omega_variable(
    consent_requirement_in_r2p,
    'Does the R2P reading permit intervention against the explicit refusal of the targeted state? Or does it require that intervention occur with the state''s ultimate consent (even if coerced)?',
    'Doctrinal and operational analysis: in cases of invoked R2P (Libya, Syria interventions), did the reading claim to proceed against state refusal, or did it frame intervention as state-initiated-under-pressure?',
    'If R2P permits intervention against refusal, the targeted state''s extraction is maximal (d → 1.0), extraction is high, suppression is high. If R2P requires (coerced) consent, the framing is more coordinative (lower d for the targeted state, moderating the extraction profile). The distinction traces the boundary between coordination and pure coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_requirement_in_r2p, conceptual, 'Whether R2P intervention requires state consent or can override it.').

omega_variable(
    intervention_mandate_scope,
    'Once intervention is authorized under R2P, what is the scope of permissible action? Is it limited to protecting populations from immediate threat (narrow humanitarian scope), or does it extend to regime change, political transformation, and state-building?',
    'Operational audit of R2P interventions: what mandates were authorized and what operations were executed? Did intervention scope creep from population protection to political objectives?',
    'Narrow scope (population protection) moderates extraction and frames the constraint as coordinative. Broad scope (regime change, state-building) increases extraction, increases the constraint''s function as cover for strategic interests, and raises theater ratio. Libya''s trajectory (NATO intervention authorized for civilian protection, executed as regime change) exemplifies this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_mandate_scope, empirical, 'The operational scope and mission creep dynamics of R2P interventions.').

omega_variable(
    kernel_reading_contest,
    'Is the R2P reading a defensible interpretation of the UN Charter''s text, or is it an extra-textual innovation that requires reading new meaning into Article 2(7) and Chapter VII?',
    'Comparative jurisprudence: textual exegesis of the Charter (what it explicitly says about intervention triggers and sovereignty limits), historical intent (what drafters intended in 1945), and subsequent state practice. Compare against the sovereignty-first reading''s textual arguments.',
    'If R2P is a defensible interpretation, the constraint''s legitimacy is textual and institutional. If extra-textual, it is a reading imposed by powerful states and humanitarian networks—extraction is reframed as norm-innovation rather than coordination. This distinction affects whether the reading is justifiable to sovereignty-first states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether R2P is a Charter-grounded reading or an extra-textual innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t5, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(arti_tr_t25, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(arti_be_t5, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(arti_be_t25, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(arti_su_t5, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(arti_su_t25, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.15).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% The article_2_7_chapter_vii_tension kernel constrains two structurally distinct readings: the r2p_reading (this file) instantiates the humanitarian-intervention branch, treating sovereignty as conditional on population protection. The sovereignty_first_reading instantiates the alternative branch, treating sovereignty and non-intervention as foundational. Each reading has its own constraint_id, ε, beneficiary/victim structure, and classification. The readings coexist in institutional practice—different coalitions and state factions hold each reading; neither foreclosed the other, but each influences the other through the contested interpretation of the Charter. The two stories are linked via network.affects_constraints to reflect their structural kinship: changes to one reading's institutional weight or credibility affect the operational constraints on the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
