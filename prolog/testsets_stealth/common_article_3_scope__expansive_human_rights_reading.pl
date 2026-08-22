% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Common Article 3 Expansive Reading: Minimum Humanitarian Floor for Any Organized Armed Violence
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets minimum humanitarian
 *   standards for non-international armed conflict. This story instantiates
 *   the reading under which the floor applies to any organized armed violence
 *   regardless of formal conflict classification: the protective scope is
 *   fixed normatively, not by threshold findings. Under this reading, every
 *   detainee in the hands of state forces or armed groups during organized
 *   violence holds enforceable minimum-treatment entitlements, humanitarian
 *   organizations hold access mandates, and state security operations —
 *   counterinsurgency, counterterrorism, internal security — fall under
 *   external monitoring and prosecution exposure. The reading consolidated
 *   through the International Court of Justice's jurisprudence, ad hoc
 *   tribunal case law, the Rome Statute's war-crimes provisions for internal
 *   conflict, and the ICRC's customary-law work. Note on vocabulary: the
 *   reading's legal 'victim set' (protected persons) maps in this schema to
 *   structural BENEFICIARIES — detainees and civilians receive protection;
 *   the structural victims (cost-bearers) are the state and non-state seats
 *   whose operations the floor constrains and whose members face prosecution.
 *   The claimed type and the metrics are authored independently: the
 *   arrangement is claimed as a hybrid of genuine humanitarian coordination
 *   and asymmetric cost imposition, and the metrics describe its observed
 *   operation without being tuned to any classification target.
 *
 * KEY AGENTS:
 *   - state_security_establishments: Primary target (institutional/trapped) — bears operational constraint, external monitoring, and commander prosecution exposure across every operation involving organized violence
 *   - detainees_in_organized_violence: Primary beneficiary (powerless/trapped) — holds enforceable minimum-treatment entitlements in any custodian's hands
 *   - civilians_in_armed_violence_zones: Primary beneficiary (powerless/constrained) — protected from violence to life and dignity; humanitarian access mandated
 *   - nonstate_armed_groups: Dual-positioned (organized/constrained) — bound by the floor and prosecutable, while its wounded and captured members receive the same protections
 *   - international_criminal_tribunals: Agenda-setter and institutional beneficiary (institutional/identity_locked) — adjudicates internal-conflict atrocities; jurisdiction and caseload scale with the floor's scope
 *   - icrc_and_humanitarian_agencies: Beneficiary and norm custodian (institutional/identity_locked) — claims access on the floor's strength and authors interpretations feeding its consolidation
 *   - human_rights_monitoring_bodies: Secondary beneficiary (institutional/constrained) — extends review over internal security operations through human rights machinery
 *   - state_political_branches: Sovereignty-cost bearer (powerful/constrained) — directs security policy, bears external review of operations the state deems internal, resists through classification contest rather than exit
 *   - populations_dependent_on_security_operations: Excluded voice (powerless/trapped) — bears contested second-order costs of constrained operations; no seat in the forums where scope is argued
 *   - ihl_doctrine_and_commentary: Analytical observer (analytical/analytical) — maps the floor's application across jurisdictions and sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.52).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.62).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Expansive Reading: Minimum Humanitarian Floor for Any Organized Armed Violence").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'ebd021be-43d1-4a4d-a5b8-2285d3083d75').
narrative_ontology:cs_kernel_codification('ebd021be-43d1-4a4d-a5b8-2285d3083d75', fixed_text).
narrative_ontology:cs_authority_grounding('ebd021be-43d1-4a4d-a5b8-2285d3083d75', lineage).
narrative_ontology:cs_interpretation_layer_present('ebd021be-43d1-4a4d-a5b8-2285d3083d75').
narrative_ontology:cs_reading_relation('ebd021be-43d1-4a4d-a5b8-2285d3083d75', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('ebd021be-43d1-4a4d-a5b8-2285d3083d75', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('ebd021be-43d1-4a4d-a5b8-2285d3083d75', foundational, minimum_standards_regardless_of_classification).
narrative_ontology:cs_axiom_status(minimum_standards_regardless_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('ebd021be-43d1-4a4d-a5b8-2285d3083d75', minimum_standards_regardless_of_classification, deontological).
narrative_ontology:cs_axiom('ebd021be-43d1-4a4d-a5b8-2285d3083d75', foundational, protection_independent_of_state_consent).
narrative_ontology:cs_axiom_status(protection_independent_of_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('ebd021be-43d1-4a4d-a5b8-2285d3083d75', protection_independent_of_state_consent, deontological).
narrative_ontology:cs_reference_frame('ebd021be-43d1-4a4d-a5b8-2285d3083d75', object_and_purpose_humanitarian_floor).
narrative_ontology:cs_drift_state('ebd021be-43d1-4a4d-a5b8-2285d3083d75', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ebd021be-43d1-4a4d-a5b8-2285d3083d75', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees_in_organized_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilians_in_armed_violence_zones).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, icrc_and_humanitarian_agencies).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_establishments).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_political_branches).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, object_and_purpose_interpretive_method).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, niac_war_crimes_jurisdiction_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct counterinsurgency, counterterrorism, and internal security operations. Under this reading the minimum-treatment floor binds them in every operation involving organized armed violence, whatever the state calls the situation: detention practices, interrogation methods, and treatment of captured or wounded adversaries are reviewable against the standards, and commanders face prosecution exposure before international tribunals and foreign courts applying universal jurisdiction. Exit is effectively closed: the Geneva Conventions are universally ratified, the floor is claimed as customary law binding even non-parties, and no practical denunciation path exists.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_establishments, payer,
    institutional, generational, trapped, global).

% Persons held in custody by state forces or armed groups during organized violence. The floor guarantees humane treatment regardless of how the detaining power classifies the conflict: violence to life, hostage-taking, and execution without judgment are prohibited, and humanitarian access can be claimed. They hold no power over their treatment; their protection depends entirely on external enforcement they cannot invoke directly.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees_in_organized_violence, beneficiary,
    powerless, biographical, trapped, global).

% Populations living where organized armed violence occurs. They gain protection from violence to life and outrages upon personal dignity, and humanitarian organizations gain a mandate to reach them with relief. Their exit is flight, which is costly and often impossible; their protection is delivered by institutions they do not control.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilians_in_armed_violence_zones, beneficiary,
    powerless, biographical, constrained, regional).

% Organized armed groups fighting state forces. The floor binds them symmetrically: their commanders and fighters can be prosecuted before the same tribunals that try state personnel, while their wounded, captured, and detained members receive the same minimum protections, and the framework treats them as parties to the violence rather than mere criminal objects. No exit exists — customary-law claims bind groups that never signed anything.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups, beneficiary).

% Ad hoc tribunals and the permanent international criminal court adjudicate atrocities in internal armed violence, with jurisdiction over internal-conflict conduct resting on the floor's application regardless of classification. Caseload, budget, and doctrinal authority scale with the floor's scope, and the tribunals author the case law through which the reading consolidates. They cannot abandon this jurisdiction without dissolving their own mandate.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, beneficiary).

% The ICRC and partner agencies claim access to detainees and conflict zones on the strength of the floor, and the ICRC acts as custodian and promoter of humanitarian law, publishing the interpretations and customary-law studies that feed the reading's consolidation. Mandate and funding scale with the norm's reach; the organization's identity is fused with the mandate itself.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_and_humanitarian_agencies, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, icrc_and_humanitarian_agencies, agenda_setter).

% United Nations treaty bodies, regional human rights courts, and commissions of inquiry apply the floor's standards to state security operations through human rights machinery. Each expansion of the floor's scope extends their review jurisdiction over internal security matters; they document violations, publish findings, and press for prosecution.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, beneficiary,
    institutional, generational, constrained, global).

% Executives and legislatures that direct security policy and bear the sovereignty cost of the reading: external bodies review operations the state designates as internal, and universal ratification forecloses formal withdrawal from the framework. Their resistance runs through contesting classification and scope — arguing thresholds are unmet, operations are law enforcement, or application was never consented to — rather than through exit.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_political_branches, payer,
    powerful, generational, constrained, national).

% Civilians whose physical security depends on the effectiveness of state operations against insurgents and armed groups. Operational limits, detention restrictions, and commander prosecution exposure can lengthen campaigns or narrow the tools available to their protectors; these populations bear any such second-order costs yet have no seat in the tribunals, diplomatic conferences, or expert forums where the floor's scope is argued. Their objection — that protection for detainees can trade against protection for them — goes unrepresented in the arrangement's administration.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, populations_dependent_on_security_operations, excluded,
    powerless, biographical, trapped, regional).

% Academic commentators, military lawyers, and legal advisers who map the floor's application across jurisdictions and conflicts. They see the whole structure: where enforcement bites, where endorsement outruns delivery, and which seats bear or collect what. They hold no stake in the arrangement's operation beyond professional standing.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, ihl_doctrine_and_commentary, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a common minimum standard for treatment of persons in the hands of adversaries during organized internal violence, solving the reciprocity failure of civil wars — where no party expects restraint from the other absent a binding floor — and giving humanitarian organizations a recognized mandate to reach detainees and civilians.
% TRANSFER_FUNCTION: Transfers adjudicatory authority over internal security operations from national institutions to international tribunals and monitoring bodies; transfers enforceable protection entitlements to detainees and civilian populations; moves operational discretion and legal exposure from state and non-state warring parties to the enforcement apparatus.
% ABSENT_VOICES: Non-state armed groups are bound by the floor but had no seat in the jurisprudence and commentary through which this reading consolidated; populations whose security depends on state operations bear second-order costs of constrained operations but are absent from the tribunals, diplomatic conferences, and expert forums where scope is argued; the counterinsurgency states most exposed to prosecution risk were rarely parties to the proceedings that authored the reading. All three sit outside the doctrinal forums where the arrangement's terms are set.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, internal conflicts would lose their minimum-standards anchor: detainees in state and non-state custody would lose the strongest legal barrier against torture, hostage-taking, and summary execution; international tribunals would lose jurisdiction over internal-conflict atrocities; humanitarian access mandates would weaken; and the retaliation cycles the floor interrupts would resume. State security establishments would regain operational freedom and shed prosecution exposure.
% FOUNDING_PROBLEM: Civil wars fought in a legal vacuum: before 1949, non-international conflicts were governed by no treaty law, detainees of internal enemies had no protection, and internal violence was historically conducted with total brutality — the problem the Geneva drafters built Common Article 3 to solve.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the military doctrine manuals of states that contest this reading's scope incorporate the floor as operative law for internal violence; United Nations commissions of inquiry and Security Council atrocity documentation attest that unrestrained internal violence remains a live problem; even threshold-gate states accept the floor's application to conflicts clearing their own thresholds — they contest the reading's breadth, not the problem's existence.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.52 at interval end). The floor solves a real collective-action problem — the reciprocity failure that makes internal violence total — and protected persons are its net beneficiaries; but its expansion transfers operational discretion and legal exposure from state seats to an enforcement apparatus that accrues jurisdiction, caseload, and mandate with each scope extension. The late-interval plateau (0.53 to 0.52) models state-practice pushback contesting application in counterterrorism-era operations. Suppression (0.62) is a raw structural property, unscaled by power or scope: exit is effectively closed (universal ratification, customary-law claims binding non-parties, no practical denunciation path), and the arrangement's persistence depends on active enforcement machinery rather than participant preference. Theater ratio (0.36) records a real but uneven protective record — the floor demonstrably protects where enforced, while a growing share of activity is rhetorical endorsement unaccompanied by access or prosecution. Accessibility collapse (0.50) is mid-range because the scope criterion itself remains contestable: states maintain live alternative framings rather than confronting a collapsed option set. Resistance (0.60) is sustained and organized — classification contests, monitoring refusals, jurisdiction rejections — mounted by precisely the seats that bear the costs. All three metric series run on one shared time grid (t = 0, 10, 20, 30, 40, 50, mapping 1975 to 2025), so every tracked metric is authored at every examined point. Suppression_requirement is tracked deliberately: this story's dynamic is enforcement-capacity change — the reading's consolidation was the build-out of prosecutorial and monitoring machinery — so the enforcement trajectory is authored rather than left to the static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the enforcement seats compute different arrangements from the same text. From the state security and political-branch seats, the floor operates as an external review regime imposed on security functions the state designates as internal — costs are concentrated, visible, and personally borne by commanders facing prosecution exposure. From the detainee and civilian seats, the same text is the only enforceable barrier between them and their custodians. From the tribunal, monitoring, and humanitarian seats, the floor is mandate: each scope extension is institutional gain. The engine computes these per-seat classifications from the structural data; the divergence between the payer-seat experience (imposition) and the beneficiary-seat experience (protection) is the arrangement's central perspectival fact, and it is why the same norm is defended as humanitarian law and resisted as sovereignty infringement simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (detainees, civilians, humanitarian agencies, tribunals, monitoring bodies) drive those seats toward the beneficiary end of directionality; victim declarations (state security establishments, political branches, non-state armed groups) drive those seats toward the target end. Exit structure modulates within each group: detainees are trapped and powerless, sitting nearest the full-beneficiary end; state seats are trapped by universal ratification and customary-law claims, sitting nearest the full-target end. One override is declared: nonstate_armed_groups (power atom 'organized') at d = 0.55, because the near-full-target value that the victim role plus constrained exit would derive overstates its position — the group seat is genuinely dual, its commanders prosecutable while its wounded and captured members receive the same minimum protections, and the framework treats it as a party to the violence rather than a mere object of it. The override is keyed to the 'organized' atom, which only this stakeholder holds, so no other seat is affected.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are prevented. Classifying the arrangement as pure coordination would erase the asymmetric cost structure the metrics record: state seats bear concentrated, non-consented costs, and the enforcement apparatus accrues jurisdiction with each scope extension — receipt of the arrangement's gains lands demonstrably on the tribunal seat. Classifying it as pure extraction would erase the coordination function: the floor solves the reciprocity failure that makes internal violence total, protected persons are net beneficiaries, and the founding problem remains live. The hybrid classification holds both facts at once. Mandatrophy is not resolved: the founding problem — internal violence conducted in a legal vacuum — is attested live by sources outside the beneficiary set, including the military doctrines of states that contest this reading's scope and United Nations atrocity documentation. The R5 mismatch check (live founding-problem status crossed with a world_rearranges disappearance verdict) raises no zombie flag: the arrangement has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_delta_state_centric_reading,
    'This constraint instantiates the expansive_human_rights_reading of kernel common_article_3_scope. If the state_centric_reading governed instead — application only when intensity and organization thresholds are met, excluding low-level violence and law enforcement — which structural elements of this story change?',
    'Classify a fixed panel of internal operations (counterterrorism detentions, low-intensity insurgencies, policing-adjacent violence, spillover conflicts) under each reading''s scope criterion and compare which detainees and populations fall inside the protective set and which operations face external review.',
    'Under the threshold-gated sibling, the beneficiary set contracts to conflicts clearing the thresholds, most state security operations exit external monitoring and prosecution exposure, the enforcement seats'' jurisdiction shrinks, and this story''s victim declarations and extractiveness would not describe the resulting arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_state_centric_reading, conceptual, 'Committer delta against the threshold-gated sibling reading of the Common Article 3 kernel.').

omega_variable(
    sibling_delta_icrc_customary_reading,
    'If the icrc_customary_reading governed — scope determined by evolving state practice and opinio juris rather than fixed normatively — what would this story''s structural data become?',
    'Track state practice and opinio juris on the floor''s application (military manuals, reservations, protests, acquiescence, national prosecutions) and compare the practice-derived scope against this reading''s normative scope.',
    'If practice-derived scope is narrower, this reading''s beneficiary declarations are overstated and its state-cost declarations overbroad; if practice has consolidated at the expansive scope the readings converge. Note the asymmetry: this reading''s jurisprudential gains feed the practice record the sibling tracks, so the sibling partially absorbs this reading''s wins rather than competing head-on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_icrc_customary_reading, empirical, 'Committer delta against the practice-tracking sibling reading; the readings are linked by this reading''s influence on the practice record.').

omega_variable(
    organized_violence_operationalization,
    'Can ''organized armed violence'' be operationalized as the scope trigger without reintroducing classification thresholds through the back door?',
    'Comparative analysis of how tribunals and monitoring bodies actually gate application: intensity indicators, organizational criteria, duration and territorial-control tests in case law and monitoring practice.',
    'If application practice requires de facto threshold findings, this reading''s classification-independent scope is nominal and its effective operation converges on the threshold-gated alternative; if application tracks the mere existence of organized violence, the broad scope is real and the state-cost declarations bind widely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_violence_operationalization, conceptual, 'Whether the reading''s scope criterion is classification-independent in operational practice.').

omega_variable(
    doctrine_compliance_gap,
    'Does the reading''s doctrinal consolidation convert into actual protection for detainees and civilians, or does the gap between normative reach and state compliance leave the expansion substantially declaratory?',
    'Compare detention-monitoring access rates, prosecution counts for internal-conflict violations, and documented violation rates against the reading''s scope claims, across jurisdictions and across the interval.',
    'A widening gap would raise the performative share of the arrangement''s activity and push its effective operation toward maintained appearance rather than delivery; convergence would confirm that the protective function scales with scope and support the beneficiary declarations at their authored strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_compliance_gap, empirical, 'Whether normative scope expansion converts into protective delivery or remains largely declaratory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_expansive_reading_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t0, observed).
narrative_ontology:measurement(ca3_expansive_reading_tr_t10, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t10, observed).
narrative_ontology:measurement(ca3_expansive_reading_tr_t20, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t20, observed).
narrative_ontology:measurement(ca3_expansive_reading_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t30, observed).
narrative_ontology:measurement(ca3_expansive_reading_tr_t40, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t40, observed).
narrative_ontology:measurement(ca3_expansive_reading_tr_t50, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(ca3_expansive_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(ca3_expansive_reading_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t0, observed).
narrative_ontology:measurement(ca3_expansive_reading_be_t10, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t10, observed).
narrative_ontology:measurement(ca3_expansive_reading_be_t20, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t20, observed).
narrative_ontology:measurement(ca3_expansive_reading_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t30, observed).
narrative_ontology:measurement(ca3_expansive_reading_be_t40, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t40, observed).
narrative_ontology:measurement(ca3_expansive_reading_be_t50, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(ca3_expansive_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(ca3_expansive_reading_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t0, observed).
narrative_ontology:measurement(ca3_expansive_reading_su_t10, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t10, observed).
narrative_ontology:measurement(ca3_expansive_reading_su_t20, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t20, observed).
narrative_ontology:measurement(ca3_expansive_reading_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t30, observed).
narrative_ontology:measurement(ca3_expansive_reading_su_t40, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t40, observed).
narrative_ontology:measurement(ca3_expansive_reading_su_t50, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(ca3_expansive_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The kernel common_article_3_scope — the fixed text of Common Article 3 — decomposes into three structurally distinct scope readings, each a separate constraint story with its own epsilon, beneficiary/victim structure, and classification: this expansive human-rights reading (scope fixed normatively, any organized armed violence), the state-centric reading (scope gated on intensity and organization thresholds), and the ICRC customary reading (scope tracked through evolving state practice and opinio juris). They are linked rather than merged because the scope criterion determines the protected set and the state-cost surface: merging them would produce an epsilon that shifts with the observable used, violating epsilon invariance. This reading sits downstream of the fixed text and upstream of the practice record the ICRC customary reading tracks — its jurisprudential gains (tribunal case law, the Rome Statute's internal-conflict provisions) become data in the sibling's scope determination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
