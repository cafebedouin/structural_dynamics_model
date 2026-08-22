% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine Covenant as Territorial Sovereignty Mandate
 *   domain: political philosophy / religious nationalism / territorial conflict
 *
 * SUMMARY:
 *   This story authors the religious-covenant reading of the Jewish
 *   self-determination kernel: the claim that Jewish sovereignty over the
 *   land is a religious obligation grounded in divine covenant, standing
 *   independent of and prior to secular political frameworks, treaties, or
 *   negotiated settlements. Within the reading's own theological framework,
 *   the covenant is treated as immutable — a mountain: no party 'grants' it,
 *   its authority does not depend on enforcement, and its content is fixed by
 *   scripture and tradition. But when this claim is operationalized into
 *   state policy — settlement expansion, legal exemptions, coalition politics
 *   — it becomes deeply entangled with instruments of secular state power
 *   (land allocation, security forces, legal exemptions for settlements),
 *   which is where it functions as a tangled_rope: coordinating a
 *   religious-national identity project while extracting political veto power
 *   from the secular negotiating apparatus and imposing material costs on
 *   Palestinian residents who have no standing within the framework. The
 *   claimed_type is authored as tangled_rope, not mountain, because although
 *   the theological content presents itself as immutable, its political
 *   operation is precisely NOT self-sustaining independent of enforcement —
 *   it requires active political mobilization, legal argumentation, and state
 *   action to translate scriptural claim into territorial fact. This is the
 *   expected structural delta: mountain-as-theology,
 *   tangled_rope-as-operationalized-policy. The ε value (0.71) reflects the
 *   high effective extraction produced when a claim asserted as absolute
 *   within one framework is deployed as a trump card within a contested
 *   political framework that has not agreed to its premises.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: primary agenda-setter and beneficiary — treats territorial retention as religious commandment
 *   - settlement_enterprise_institutions: material beneficiary — receives land, subsidy, legal protection
 *   - palestinian_residents_of_contested_territories: primary victims — bear displacement and restriction with no standing in the framework
 *   - secular_israeli_negotiating_framework: institutional victim — loses negotiating flexibility to religious veto
 *   - non_orthodox_jewish_denominations: secondary victims — theological and reputational marginalization
 *   - diaspora_jewish_communities: excluded — bear associational consequences without a voice in operationalization
 *   - international_legal_and_diplomatic_bodies: analytical observer — cannot engage the claim on its own theological terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.71).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant as Territorial Sovereignty Mandate").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political philosophy / religious nationalism / territorial conflict").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '39b60996-6f93-48f2-a934-fc073ea4218a').
narrative_ontology:cs_kernel_codification('39b60996-6f93-48f2-a934-fc073ea4218a', fixed_text).
narrative_ontology:cs_authority_grounding('39b60996-6f93-48f2-a934-fc073ea4218a', lineage).
narrative_ontology:cs_interpretation_layer_present('39b60996-6f93-48f2-a934-fc073ea4218a').
narrative_ontology:cs_reading_relation('39b60996-6f93-48f2-a934-fc073ea4218a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('39b60996-6f93-48f2-a934-fc073ea4218a', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('39b60996-6f93-48f2-a934-fc073ea4218a', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('39b60996-6f93-48f2-a934-fc073ea4218a', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('39b60996-6f93-48f2-a934-fc073ea4218a', foundational, territorial_sovereignty_is_divine_commandment).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_divine_commandment, holdable).
narrative_ontology:cs_axiom_grounding('39b60996-6f93-48f2-a934-fc073ea4218a', territorial_sovereignty_is_divine_commandment, theological).
narrative_ontology:cs_axiom('39b60996-6f93-48f2-a934-fc073ea4218a', foundational, covenant_claim_independent_of_secular_political_consent).
narrative_ontology:cs_axiom_status(covenant_claim_independent_of_secular_political_consent, holdable).
narrative_ontology:cs_axiom_grounding('39b60996-6f93-48f2-a934-fc073ea4218a', covenant_claim_independent_of_secular_political_consent, theological).
narrative_ontology:cs_axiom('39b60996-6f93-48f2-a934-fc073ea4218a', secondary, territorial_retention_overrides_diplomatic_negotiation).
narrative_ontology:cs_axiom_status(territorial_retention_overrides_diplomatic_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('39b60996-6f93-48f2-a934-fc073ea4218a', territorial_retention_overrides_diplomatic_negotiation, deontological).
narrative_ontology:cs_reference_frame('39b60996-6f93-48f2-a934-fc073ea4218a', abrahamic_land_grant_covenant).
narrative_ontology:cs_drift_state('39b60996-6f93-48f2-a934-fc073ea4218a', post_1967_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39b60996-6f93-48f2-a934-fc073ea4218a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_political_parties).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territories).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_negotiating_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, non_orthodox_jewish_denominations).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, divine_land_grant_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, religious_obligation_precedes_secular_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes political parties, yeshivot, and settlement councils around the premise that possession of the land is a religious commandment (mitzvat yishuv ha'aretz) rather than a negotiable policy outcome. Sets the interpretive agenda for what counts as legitimate territorial compromise by framing withdrawal as sin, not strategy. Draws funding, political leverage, and demographic growth in settlements from this framing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Receives state infrastructure, subsidized land, and legal cover for expansion in the West Bank on the strength of the covenant claim's political traction. Its continued growth depends on the religious framing remaining politically potent enough to override competing claims from security or diplomatic considerations.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions, beneficiary,
    institutional, generational, constrained, regional).

% Live under a territorial claim they had no part in authoring, framed as absolute and non-negotiable by religious warrant. Experience land expropriation, movement restriction, and settlement expansion justified by a doctrine that treats their residence as an obstacle to a divine mandate rather than a competing political claim. Have no standing within the covenant framework itself to contest it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territories, payer,
    powerless, generational, trapped, regional).

% Israeli governments attempting territorial compromise (land-for-peace formulas, withdrawal proposals) must operate against a domestic veto point: any deal perceived to cede covenanted land triggers coalition collapse, mass mobilization, or violent resistance from religious nationalist constituencies. The secular state's capacity to trade territory for security or diplomatic gain is structurally narrowed by a claim it cannot out-argue on its own terms.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_negotiating_framework, payer,
    institutional, biographical, constrained, national).

% Reform, Conservative, secular, and diasporic Jewish communities who hold competing theological or political understandings of covenant and land find their interpretations marginalized in Israeli religious-state institutions that privilege Orthodox and religious-Zionist readings. Bear reputational costs internationally when the covenant claim is publicly identified with Jewish identity as such.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, non_orthodox_jewish_denominations, payer,
    moderate, generational, constrained, global).

% Many diaspora Jews reject or are ambivalent about a covenant framing that ties Jewish identity to a specific territorial-sovereignty project; some actively advocate diasporist alternatives. Largely excluded from the internal Israeli religious-political debate that decides how the covenant claim is operationalized in law and settlement policy, despite bearing associational consequences.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities, excluded,
    organized, generational, mobile, global).

% UN bodies, international courts, and foreign governments assess the territorial claim using secular international law (occupation law, self-determination norms) that has no mechanism for adjudicating a divine warrant. They can document effects (settlement expansion, displacement) but cannot engage the covenant claim on its own terms, producing persistent talking-past between religious and secular-legal registers.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_legal_and_diplomatic_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the religious-Zionist framework, the covenant claim coordinates a highly dispersed, historically stateless people around a single, stable, non-negotiable territorial referent — solving the problem of what would otherwise be an endlessly renegotiable, faction-fractured claim to land by grounding it in a fixed textual and theological source treated as beyond human amendment.
% TRANSFER_FUNCTION: Moves political veto power, land, state subsidy, and legal protection toward religious-Zionist settlement institutions and away from secular negotiating flexibility and from Palestinian residents of the contested territories, by converting a policy question (where should borders run) into a theological one (what has God granted) that secular actors cannot bargain over without appearing to commit sacrilege.
% ABSENT_VOICES: Palestinian residents have no standing inside the covenant framework to contest it — the claim's internal logic does not recognize their residence as a competing right, only as an obstacle. Non-Orthodox and diasporist Jewish voices proposing alternative theological or political readings of covenant are marginalized within Israeli religious-legal institutions. Both groups would object that the claim forecloses exactly the negotiations that could otherwise settle the underlying dispute.
% DISAPPEARANCE_RATIONALE: If the covenant claim's political operative force disappeared overnight — i.e., if religious warrant ceased to function as a domestic veto on territorial policy — Israeli governments would regain negotiating flexibility currently foreclosed by coalition dependence on religious-nationalist parties, settlement expansion would lose its primary legal-theological justification, and territorial questions would revert to being adjudicated on security, demographic, and diplomatic grounds rather than treated as non-negotiable religious obligation. Settlement enterprise institutions and religious parties would lose their principal source of political leverage.
% FOUNDING_PROBLEM: The claim was built to answer: on what basis can a historically stateless, dispersed people assert an unbreakable, legitimate connection to a specific territory that cannot be dissolved by conquest, exile, or changing political fashion? The covenant supplies a warrant that predates and outlasts any particular state or treaty.
% FOUNDING_PROBLEM_CORROBORATION: Religious-Zionist rabbinical authorities and settlement movement leaders attest the covenant obligation remains fully live and binding regardless of political circumstance. Secular Israeli historians, non-Orthodox denominational leaders, and international legal scholars — all outside the religious-Zionist beneficiary set — attest that the claim's practical function has shifted from theological self-understanding to an operative veto on territorial compromise, and that this shift is the source of its contemporary political extraction rather than evidence the founding theological problem itself remains unresolved.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 to reflect that although the covenant claim, taken purely as theology, need extract nothing (a belief costs no one anything on its own), its operationalization as a political veto over Israeli territorial policy transfers real costs: land, movement, legal standing, and negotiating flexibility, from Palestinian residents and the secular state apparatus to religious-nationalist institutions. Suppression (0.68) is authored high because the claim's political force depends on foreclosing certain forms of dissent — proposing territorial compromise perceived as violating covenant carries severe domestic political and sometimes violent consequences (e.g., historical assassination of a Prime Minister following a peace agreement framed by opponents as covenant violation). Theater ratio (0.42) reflects a genuine and substantial theological core (this is not merely cynical instrumentalization — many adherents hold the belief with total sincerity) alongside a growing performative layer where covenant language is invoked instrumentally in political coalition-building independent of theological conviction. Accessibility collapse (0.62) and resistance (0.74) are both authored moderately-high: alternatives (secular nationalist, diasporist, or negotiated frameworks) remain visible and actively argued, but the covenant claim's entrenchment in law, settlement fact-on-the-ground, and coalition politics makes reversal costly, hence real active resistance continues from multiple directions rather than the claim resting unchallenged.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-Zionist movement and settlement institutions sit at the beneficiary end: the claim's political operation transfers land, funding, and veto power to them, and their exit options are identity_locked or institutional (this is not a preference they could easily abandon without abandoning constitutive religious-national identity). Palestinian residents sit at the full-target end: trapped exit options, no standing within the framework, and direct material costs (displacement, restriction). The secular Israeli negotiating framework is a structural victim at the institutional level: it bears diminished capacity to trade territory, constrained not by external adversaries but by the domestic political cost of appearing to violate a claim it does not fully control. Non-Orthodox denominations and diaspora communities are victims/excluded at lower intensity — theological and reputational costs rather than material displacement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding theological problem (how does a stateless, dispersed people ground a legitimate, unbreakable territorial claim) may remain sincerely live for many adherents — this is not authored as a cynical fabrication. But its status is authored as contested because outside observers (secular historians, non-Orthodox theologians, international legal scholars) document that the claim's operative political function has shifted: it now functions less as a private theological self-understanding and more as a public veto instrument constraining a democratic state's negotiating options and materially affecting non-consenting third parties. Classifying this as tangled_rope rather than mountain or snare prevents two mislabeling errors: (1) treating a highly contested, actively-enforced political claim as an unchangeable natural fact beyond critique (the mountain-mislabeling this reading's own framework invites), and (2) treating deeply and sincerely held religious conviction as pure cynical extraction with no genuine coordination function (erasing the real identity-coordination the covenant provides for its adherents). The tangled_rope classification holds both: real coordination for believers, real extraction from non-consenting others, bound together by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_versus_political_instrumentalization,
    'Is the covenant claim, as it functions in contemporary Israeli politics, primarily a sincere theological conviction that happens to have political consequences, or primarily a political instrument that uses theological language for legitimation and mobilization?',
    'Comparative analysis of religious-Zionist voting and settlement behavior against periods of high versus low political salience of the territorial question; survey data on the relative weight adherents place on theological versus security/nationalist justifications; historical tracing of when covenant language intensifies relative to coalition-formation needs.',
    'If primarily sincere theology, the constraint is better modeled with a larger mountain-like component regardless of its political effects (genuine belief, contingent political consequence). If primarily instrumentalized, the tangled_rope/snare balance shifts further toward snare, since the theological framing would function mainly as cover for material and political extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theology_versus_political_instrumentalization, conceptual, 'Whether covenant claims are sincere theology with political spillover or instrumentalized political tools using theological cover.').

omega_variable(
    internal_religious_pluralism_on_covenant_meaning,
    'Does ''the'' religious covenant claim actually admit of a single reading, or are there significant internal religious traditions (including within Orthodoxy) that interpret covenant obligation as compatible with territorial compromise or with prioritizing peace/life-preservation (pikuach nefesh) over territorial retention?',
    'Examination of rabbinic legal (halakhic) literature and religious-Zionist internal debates on territorial compromise; identification of religious authorities who have ruled in favor of land-for-peace exchanges on religious grounds.',
    'If significant internal pluralism exists, this constraint''s beneficiary set should be narrowed to specific factions (not ''religious Zionism'' monolithically), and the claim of a single immutable religious mandate is itself contested from within the tradition, not just from outside it — weakening the mountain-like immutability claim at its theological root, not just its political operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_religious_pluralism_on_covenant_meaning, empirical, 'Whether the covenant claim is theologically monolithic or internally contested within Jewish religious tradition itself.').

omega_variable(
    kernel_framing_alternative_legitimacy_versus_institution,
    'Should this story''s kernel-reading structure be framed around the covenant CLAIM itself (a legitimacy assertion) or around the STATE INSTITUTIONS (religious councils, settlement authorities, coalition arrangements) that operationalize it? The obvious framing treats the covenant claim as the constraint; a less obvious framing would treat the accumulated institutional apparatus built to enact it as the true site of extraction, with the covenant claim itself functioning only as legitimating narrative layered above.',
    'Trace whether removing the theological justification (while leaving settlement institutions, legal structures, and coalition dependencies intact) would materially change extraction levels — if institutions persist and continue extracting under a purely secular-nationalist justification, the institutional framing is doing more causal work than the covenant claim itself.',
    'Under the covenant-claim framing (adopted here), this story is a reading of the jewish_self_determination kernel with tangled_rope classification. Under the institutional framing, this constraint might instead be modeled as downstream of a separate institutional-apparatus constraint, with the religious claim relegated to a vindicated_proposition rather than the primary structural driver — this would not change ε much but would shift where the coordination/extraction boundary is located structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_legitimacy_versus_institution, conceptual, 'Alternative framing: covenant-claim-as-constraint versus institutional-apparatus-as-constraint, with the covenant serving as legitimating narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewi_tr_t1977, jewish_self_determination__religious_covenant_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.33).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__religious_covenant_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(jewi_tr_t2015, jewish_self_determination__religious_covenant_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.28).
narrative_ontology:measurement(jewi_be_t1977, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(jewi_be_t2015, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(jewi_su_t1977, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1977, 0.48).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(jewi_su_t2015, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the jewish_self_determination kernel. Each reading is authored as a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and claimed_type, per the ε-invariance principle: the label 'Jewish claim to the land' covers at least five non-equivalent structural claims (theological-covenantal, liberal-national, indigenous-continuity, settler-colonial, and diasporist-pluralist), which cannot share one ε without averaging over incompatible normative and empirical premises. The religious_covenant_reading is distinguished from liberal_nationalist_reading by grounding the claim in a source (divine covenant) that its own logic holds prior to and independent of secular political consent — this is why it forecloses (not merely coexists with) diasporist_reading, whose core premise (territorial sovereignty is a dangerous deviation from proper Jewish flourishing) is directly negated by a framework in which territorial sovereignty is a binding religious commandment. It coexists_with liberal_nationalist, indigenous_return, and settler_colonial readings because those readings operate on secular-political or historical-empirical registers that a covenant-holder can hold as separately true or false without internal contradiction to the theological claim (a religious Zionist can also believe the liberal-national argument, or contest the settler-colonial characterization, without those beliefs bearing on covenant validity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
