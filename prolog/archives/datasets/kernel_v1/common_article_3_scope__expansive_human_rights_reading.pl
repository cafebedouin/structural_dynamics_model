% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: CA3 Expansive Scope: Humanitarian Standards Applied to All Armed Violence
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   The expansive human rights reading of Common Article 3 of the 1949 Geneva
 *   Conventions interprets the text as applying minimum humanitarian
 *   standards to ALL organized armed violence, regardless of the
 *   belligerent's status as a state, recognized combatant, or non-state armed
 *   group. This reading treats CA3 protections (prohibition of torture,
 *   inhuman treatment, summary execution, and denial of due process) as
 *   universal thresholds that bind all parties equally. The constraint
 *   exhibits tangled_rope structure: it genuinely coordinates humanitarian
 *   outcomes (reducing torture, enabling systematic detention processing,
 *   providing monitoring frameworks) while simultaneously extracting autonomy
 *   from state and NSG actors through external accountability claims and
 *   prosecution threats. The extractiveness trajectory (0.38 → 0.62 over 30
 *   years) reflects the constraint's strengthening as judicial interpretation
 *   hardens around the broad scope and prosecution capacity builds through
 *   the ICC and national courts. Suppression increases (0.55 → 0.68) as
 *   enforcement mechanisms become more credible and non-compliance costs
 *   rise. Theater ratio decreases (0.62 → 0.54) as the monitoring and
 *   accountability infrastructure becomes more functionally mature — less
 *   performative ritual, more actual accountability.
 *
 * KEY AGENTS:
 *   - Civilian Populations in Conflict: Primary victims (powerless/trapped) — trapped between combatant groups; subject to violence by both; cannot invoke protections without targeting
 *   - Detained Combatants and POWs: Secondary victims and beneficiaries (moderate/constrained) — experience protection gains but also legal liability and institutional extraction
 *   - International Humanitarian Monitoring Bodies (ICRC, UN): Organized beneficiaries (organized/constrained) — gain mandate expansion and coordination function; also experience resource constraints and access barriers
 *   - Human Rights Institutions and Advocacy: Institutional beneficiaries (institutional/arbitrage) — gain mission legitimacy, funding, and enforcement leverage; arbitrage exit available
 *   - State Military Command Authorities: Primary victims (powerful/mobile) — lose operational autonomy; face prosecution threats; can exit through treaty withdrawal at high diplomatic cost
 *   - Non-State Armed Groups: Primary victims (organized/constrained) — face extraction of legitimacy and liability without parity protections; constrained exit
 *   - Progressive Legal Reform Movements: Strategic agent (institutional/constrained) — use expansive reading as platform toward stronger universal standards; sunset structure implicit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.62).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.68).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "CA3 Expansive Scope: Humanitarian Standards Applied to All Armed Violence").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '1c18ecb2-27c3-4cf1-b12b-99e15ef15769').
narrative_ontology:cs_kernel_codification('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', fixed_text).
narrative_ontology:cs_authority_grounding('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', lineage).
narrative_ontology:cs_interpretation_layer_present('1c18ecb2-27c3-4cf1-b12b-99e15ef15769').
narrative_ontology:cs_reading_relation('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', foundational, universal_humanitarian_protection_threshold).
narrative_ontology:cs_axiom_status(universal_humanitarian_protection_threshold, holdable).
narrative_ontology:cs_axiom_grounding('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', universal_humanitarian_protection_threshold, deontological).
narrative_ontology:cs_axiom('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', foundational, symmetrical_accountability_requirement).
narrative_ontology:cs_axiom_status(symmetrical_accountability_requirement, holdable).
narrative_ontology:cs_axiom_grounding('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', symmetrical_accountability_requirement, deontological).
narrative_ontology:cs_reference_frame('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', universal_humanitarian_minimum).
narrative_ontology:cs_created_at('1c18ecb2-27c3-4cf1-b12b-99e15ef15769', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_military_command_autonomy).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups_operational_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped within active conflict; cannot exit or refuse subjection to either state or non-state armed actors. Trapped between extractive claims of both combatant groups. Maximum experienced extraction — no exit option, no ability to invoke protections without being targeted as collaborators by opposing forces. Suppression mediated through threat of violence.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DETAINED COMBATANTS (TANGLED ROPE) — Constrained by physical detention and legal liability, but the CA3 expansive reading provides genuine coordination function: humane treatment standards enable systematic processing, accountability, and reduction of extrajudicial killing. Also bears extraction: states and non-state groups benefit from legal cover for detention operations. Mixed experience — genuine protection alongside institutional extraction.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANITARIAN MONITORING (TANGLED ROPE) — Organized actors (ICRC, UN bodies, regional commissions) experience dual function: genuine coordination gain through standardized inspection protocols and accountability mechanisms, AND extraction as the monitoring mandate expands monitoring capacity without corresponding enforcement power or resources. Constrained by state and NSG access restrictions; benefit from treaty framework legitimacy.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RIGHTS INSTITUTIONS (ROPE) — Benefit from the expansive CA3 reading as a coordination standard; institutions gain mission legitimacy, funding, and enforcement leverage through broad scope application. Low extraction experienced because these institutions are structural beneficiaries. Arbitrage exit available: can redirect resources to other campaigns if CA3 enforcement becomes too resource-intensive.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL REFORM (SCAFFOLD) — See the expansive reading as a transitional platform toward stronger universal standards. The constraint functions as a coordination mechanism with built-in sunset: as state practice evolves and custom hardens around broader protections, the temporary expansive reading becomes the baseline, and further escalation occurs through treaty amendment or new custom. Theater ratio low (0.54) because the mechanism is genuinely functional — CA3 drives actual accountability change.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE MILITARY (SNARE) — Experiences the expansive CA3 reading as extraction of command autonomy through external monitoring and potential prosecution for acts classified as violations under the broad scope. Can technically exit by withdrawing from Geneva Convention ratification (mobile exit), but exit cost is diplomatic/economic sanctions. High suppression: non-compliance triggers international accountability mechanisms. Asymmetric: the constraint extracts autonomy from state actors while providing coordination benefits to humanitarian bodies.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: NON-STATE ARMED GROUPS (SNARE) — Organized groups face the expansive reading as pure extraction: accountability claims without recognition as combatants or fighters, monitoring claims without diplomatic status, and prosecution without prisoners-of-war protections in many jurisdictions. Constrained exit: disband or fight on while bearing legal liability. Suppression very high — threat of prosecution, loss of international legitimacy, targeting by state counterinsurgency.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, human dignity and prohibition on torture are fundamental principles inherent to any coherent ethical system governing armed conflict. The expansive CA3 reading appears as the inevitable recognition of a pre-existing natural law: minimum standards apply to all armed violence regardless of classification or political status. However, the structural data contradicts this — identifiable beneficiaries (human rights institutions) and clear state/NSG victims exist. False summit detection will flag this as naturalization of a politically contingent legal interpretation.
constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_article_3_scope__expansive_human_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high, rising. The constraint extracts autonomy from state and NSG actors through accountability claims while providing genuine protection coordination benefits. Base extractiveness is sustained by the asymmetry between the broad scope (applies to all armed groups) and unequal enforcement (systematically biased toward weaker actors). The rise from 0.38 to 0.62 over 30 years reflects hardening of the interpretation through case law and increasing credibility of enforcement mechanisms. Suppression (0.68): High, rising. Non-compliance triggers international accountability mechanisms, prosecution, and sanctions. Suppression is structural (external monitoring, legal liability) rather than performative. The rise from 0.55 to 0.68 reflects maturation of enforcement infrastructure: more courts, more prosecutions, higher threat credibility. Theater ratio (0.54): Moderate, declining. The expansive CA3 reading has moved from aspirational (high theater in early advocacy phase, 0.62 at t0) toward functionally operational (0.54 at t30) as monitoring protocols standardize and accountability mechanisms produce measurable outcomes. The decline indicates the constraint is becoming genuinely functional rather than performative — monitoring bodies have real inspection capacity, prosecutions are substantive rather than symbolic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classic tangled_rope structure with significant perspectival gaps. State military command sees pure extraction (snare from perspective 6: loss of autonomy without coordination benefit). Non-state armed groups see snare with added delegitimation (perspective 7: accountability claims without combatant status). Detained combatants and POWs see mixed constraint (perspective 2: genuine protection gains paired with institutional extraction). Civilian populations see snare (perspective 1: trapped with no recourse). Human rights institutions see rope (perspective 4: pure coordination benefit and mission legitimacy). The expansive human rights reading itself becomes visible as a perspectival choice when compared to the state-centric reading (sibling constraint): a state-centric reading would narrow victim sets and weaken monitoring scope, shifting perspectives 6-7 toward rope and perspective 2 toward mountain (naturalized state prerogative).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the structural power asymmetry in the expansive reading. State actors with global power appear to have mobile exit (can withdraw from Geneva Conventions) but face high d values because they are targets of the constraint — the expansive reading specifically constrains state operational autonomy. NSG actors have constrained exit (cannot exit without dissolving) and also target status, producing even higher d values and experiencing higher f(d) → higher χ. Humanitarian monitoring bodies have institutional power and arbitrage exit (can redirect resources), positioning them as beneficiaries with low d values. Human rights advocacy institutions have arbitrage exit and clear beneficiary status (mission expansion), producing negative χ — the constraint subsidizes their operations. The engine's derivation chain produces higher χ for victims (states, NSGs) and lower/negative χ for beneficiaries (humanitarian and rights institutions), creating a perspectival gap that reveals the asymmetric nature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive CA3 reading resolves the mandatrophy (threat of mislabeling coordination as extraction) by declaring both genuine coordination functions AND identifiable beneficiaries/victims. The constraint genuinely coordinates humanitarian outcomes (torture reduction, systematic detention processing, accountability frameworks) while asymmetrically extracting autonomy from state and NSG actors. The declaration that both coordination AND extraction are present prevents the false choice between 'this is pure law (mountain)' and 'this is pure oppression (snare).' The tangled_rope classification captures the actual structure: legal constraints on armed violence ARE coordination mechanisms (they make war more systematizable, more transparent, less arbitrary) AND they ARE extraction mechanisms (they impose costs on state/NSG autonomy and create asymmetric liability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_state_actor_combatant_status,
    'Does the expansive CA3 reading grant combatant immunity and prisoner-of-war status to non-state armed group members, or only humanitarian protections without legal combatancy recognition?',
    'Case law analysis: how ICC, national courts, and state practice treat captured NSG members under expansive CA3 interpretation; examination of state practice in granting or withholding POW status to organized NSG members',
    'If combatant status granted: NSGs experience moderate constraint (tangled_rope); extraction is balanced by protection benefits and formal recognition. If humanitarian protections without combatancy: NSGs experience pure extraction (snare); they bear liability without parity protections.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_combatant_status, empirical, 'Whether expansive CA3 reading grants combatant immunity to non-state actors').

omega_variable(
    enforcement_capacity_vs_mandate,
    'Can monitoring bodies and accountability mechanisms actually enforce the expansive CA3 reading against powerful state and NSG actors, or is the constraint performative (high theater) despite low stated theater_ratio?',
    'Compliance tracking: ratio of substantive prosecutions to alleged violations; post-investigation enforcement rates; state and NSG behavioral change following investigation or indictment; comparison of threat credibility before and after landmark convictions',
    'If enforcement credible: tangled_rope classification holds; extraction is real but balanced by genuine protection gains. If enforcement performative: theater_ratio should rise above 0.54; constraint reclassifies toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_mandate, empirical, 'Whether CA3 monitoring and accountability mechanisms have actual enforcement capacity').

omega_variable(
    selective_prosecution_bias,
    'Does the expansive CA3 reading apply equally to all parties, or does enforcement systematically privilege powerful state actors and burden NSGs and weaker states?',
    'Prosecution database analysis: rate of ICC indictments by actor type (state vs NSG vs powerful state official); acquittal rates by defendant nationality/power; comparison of severity of alleged violations prosecuted (torture by state vs by NSG; same conduct prosecuted asymmetrically)',
    'If selective: the expansive reading functions as extraction of legitimacy from weaker actors while permitting powerful states to capture the interpretation. Reclassifies from tangled_rope to snare for NSGs and weak states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_prosecution_bias, empirical, 'Whether enforcement of expansive CA3 reading is applied equally across state and non-state actors').

omega_variable(
    reading_foreclosure_condition,
    'Does this expansive human rights reading foreclose the state-centric reading, or do they coexist as different institutional interpretations within different jurisdictions?',
    'Doctrinal analysis: whether state-centric interpretation is logically compatible with expansive reading within a single legal framework; jurisdictional mapping of which states and legal systems adopt which reading; whether a state can simultaneously hold both positions in different contexts',
    'If forecloses: only one reading can be legitimate; conflict is zero-sum. If coexists: both readings are live positions held by different parties; constraint is fundamentally contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_condition, conceptual, 'Whether expansive CA3 reading forecloses or coexists with state-centric reading').

omega_variable(
    beneficiary_specification_ambiguity,
    'Who are the primary beneficiaries of the expansive CA3 reading: the affected populations (coordination function), the human rights institutions (mission legitimacy and resource flows), or the progressive legal reform movement (using it as a platform for further expansion)?',
    'Historical analysis of who advocated for expansive interpretation; funding and institutional growth tracking of human rights bodies following CA3 expansions; behavioral outcome comparison (did expansive reading produce better civilian protection outcomes, or primarily more human rights institution activity)',
    'If primary beneficiary is humanitarian institutions: the constraint is partly extractive (institutions benefit from expanded mandate without proportional enforcement capacity). If primary beneficiary is affected populations: coordination function dominates; constraint is legitimate tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_specification_ambiguity, empirical, 'Specification of primary beneficiaries of expansive CA3 reading').

omega_variable(
    kernel_interpretation_authority,
    'What authority legitimately interprets the Common Article 3 kernel? Is it the ICRC, state practice, tribunals, consensus, or some combination?',
    'Doctrinal genealogy: which body has made key interpretive rulings on scope; how much weight states assign to different interpreters; evolution of authoritative interpretation over time',
    'If ICRC or tribunals have primary authority: expansive interpretation can be institutionalized and enforced. If state practice is authority: expansive reading requires broad state consensus to become binding custom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_authority, conceptual, 'Authority grounding for CA3 interpretation in commitment-system framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_exp_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(ca3_exp_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(ca3_exp_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.54).

% Extraction over time
narrative_ontology:measurement(ca3_exp_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ca3_exp_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(ca3_exp_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ca3_exp_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ca3_exp_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(ca3_exp_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, icc_prosecutorial_bias).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, armed_group_humanitarian_accountability).

% DUAL FORMULATION NOTE:
% The expansive CA3 reading is one constraint family member within common_article_3_scope kernel. The state-centric reading and ICRC customary reading are separate constraint stories with their own ε values, perspectives, and classification types. They are linked via network.affects_constraints because they compete for interpretive authority over the same legal kernel. The expansive reading influences (and constrains) the state-centric reading by expanding the victim set unilaterally; the state-centric reading influences the expansive reading by providing high-power resistance to broad scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, powerful, 0.78).
constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, organized, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
