% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Patriarchal Interpretation of Qur'anic Gender Verses (Contextual-Egalitarian Reading)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This constraint story models the standing patriarchal interpretation of
 *   Qur'anic gender verses (4:11 inheritance, 2:282 testimony, 4:34
 *   guardianship) as assessed by the contextual-egalitarian reading. That
 *   reading holds the verses were historically situated progressive steps for
 *   7th-century Arabia, requiring reinterpretation under overarching Qur'anic
 *   equity principles (maqasid al-shari'a: justice, human dignity, public
 *   welfare). The patriarchal interpretation persists through state
 *   codification, judicial monopoly, and scholarly gatekeeping — coordinating
 *   family law across the Muslim world while extracting interpretive
 *   authority and material rights from women. The contextual-egalitarian
 *   reading sees moderate base extractiveness (0.48) declining from a
 *   colonial-era peak (0.65) as reformist pressure mounts, but suppression
 *   remains significant (0.62) because alternative readings are excluded from
 *   official institutions. Theater ratio (0.38) reflects performative
 *   adherence to classical texts while actual practice in many courts quietly
 *   adopts equitable outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.48).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.62).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.48).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Patriarchal Interpretation of Qur'anic Gender Verses (Contextual-Egalitarian Reading)").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '783517ec-f89a-438f-a784-85b2b7449a28').
narrative_ontology:cs_kernel_codification('783517ec-f89a-438f-a784-85b2b7449a28', fixed_text).
narrative_ontology:cs_authority_grounding('783517ec-f89a-438f-a784-85b2b7449a28', extraction).
narrative_ontology:cs_interpretation_layer_present('783517ec-f89a-438f-a784-85b2b7449a28').
narrative_ontology:cs_reading_relation('783517ec-f89a-438f-a784-85b2b7449a28', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('783517ec-f89a-438f-a784-85b2b7449a28', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('783517ec-f89a-438f-a784-85b2b7449a28', foundational, verses_are_historically_situated_progressive_steps).
narrative_ontology:cs_axiom_status(verses_are_historically_situated_progressive_steps, holdable).
narrative_ontology:cs_axiom_grounding('783517ec-f89a-438f-a784-85b2b7449a28', verses_are_historically_situated_progressive_steps, empirically_contingent).
narrative_ontology:cs_axiom('783517ec-f89a-438f-a784-85b2b7449a28', foundational, maqasid_principles_entail_gender_equality).
narrative_ontology:cs_axiom_status(maqasid_principles_entail_gender_equality, holdable).
narrative_ontology:cs_axiom_grounding('783517ec-f89a-438f-a784-85b2b7449a28', maqasid_principles_entail_gender_equality, conventional).
narrative_ontology:cs_reference_frame('783517ec-f89a-438f-a784-85b2b7449a28', classical_fiqh_patriarchal_consensus).
narrative_ontology:cs_drift_state('783517ec-f89a-438f-a784-85b2b7449a28', contemporary_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('783517ec-f89a-438f-a784-85b2b7449a28', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, patriarchal_elites).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, traditional_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, conservative_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, muslim_women).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, female_witnesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, female_heirs).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, male_guardianship_as_divine_ordinance).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, differentiated_inheritance_as_fixed_shares).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, male_testimony_weight_as_superior).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to differentiated inheritance shares (half of male), testimony weight (half of male), and guardianship requirements in marriage/travel. Religious identity fuses with communal belonging, making exit from the interpretive framework existentially costly. Reformist scholarship offers alternative readings but carries social ostracism risk.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, muslim_women, payer,
    moderate, biographical, identity_locked, global).

% Receive fixed inheritance shares under current fiqh — a material benefit over pre-Islamic exclusion — but systematically half the male share. The benefit is real but structurally subordinate; challenging the ratio risks delegitimizing the entire inheritance framework they depend on.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, female_heirs, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, female_heirs, beneficiary).

% Testimony counted at half weight in financial contracts and hudud cases. In practice, many modern courts accept equal testimony, but the doctrinal ceiling remains. Exit means accepting secular courts or facing communal pressure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, female_witnesses, payer,
    moderate, biographical, constrained, global).

% Control family law codification, judicial appointments, and fatwa councils across Muslim-majority states. Extract interpretive authority and material loyalty from maintaining the patriarchal reading. Can shift between state and religious institutional roles; exit options include secular power structures.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_elites, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, patriarchal_elites, beneficiary).

% Administer personal status law (marriage, divorce, inheritance, child custody) using classical fiqh. Institutional survival depends on monopoly over authoritative interpretation. Reform threatens jurisdiction and legitimacy; exit means absorption into secular judiciary with loss of distinct authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, traditional_courts, beneficiary).

% Hold interpretive authority through madrasa networks, fatwa bodies, and state appointments. Their epistemic capital is bound to the classical corpus; maqasid-based reinterpretation devalues their specialized training. Professional identity is fused with the textual tradition they defend.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, conservative_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Produce maqasid-based, historical-critical readings arguing for gender-equitable reinterpretation. Excluded from official fatwa councils and judicial appointments in most jurisdictions. Operate through universities, NGOs, and digital platforms; can move between academic and advocacy roles.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, excluded,
    organized, biographical, mobile, global).

% Muslim women's rights organizations (e.g., Musawah, Sisters in Islam) litigate, lobby, and produce counter-interpretations. Funded transnationally; excluded from state religious institutions. Exit options include UN mechanisms, domestic courts, and public advocacy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_ngos, excluded,
    organized, biographical, mobile, global).

% Experience the constraint through family law outcomes, Friday sermons, and communal norms. Divided between traditional loyalty and reformist sympathy. Intra-community conflict over legitimacy creates social friction; exit means either secularization or migration to more congenial interpretive communities.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, general_muslim_public, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, patriarchal_elites).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The patriarchal interpretation coordinates family law, inheritance distribution, and testimony rules across diverse Muslim societies, providing a stable, textually anchored legal framework that resolves disputes without requiring continuous renegotiation of gender roles.
% TRANSFER_FUNCTION: Moves interpretive authority and legal discretion to male elites and traditional courts; moves material rights (full inheritance shares, equal testimony weight, autonomous legal capacity) from women to men; moves communal legitimacy from plural interpretive traditions to a single authorized reading.
% ABSENT_VOICES: Women directly affected by inheritance and testimony rules in rural and conservative communities; reformist scholars and rights NGOs excluded from official fatwa councils and judicial appointments; queer and trans Muslims whose gendered legal status is unaddressed by the binary framework — all structurally absent from the authoritative interpretive circle.
% DISAPPEARANCE_RATIONALE: If the patriarchal interpretation vanished overnight, personal status codes across 40+ Muslim-majority jurisdictions would require immediate revision; courts would lose their primary interpretive basis for gender-differentiated rules; inheritance and testimony laws would need new legislative or juristic foundations; the authority of traditional courts and conservative scholars would collapse.
% FOUNDING_PROBLEM: 7th-century Arabia needed stable family and tribal structures to replace pre-Islamic customs that excluded women entirely from inheritance, treated them as property, and allowed unlimited polygyny. The Qur'anic verses introduced progressive improvements: fixed inheritance shares for women, testimony procedures, limits on polygyny, and contractual marriage rights.
% FOUNDING_PROBLEM_CORROBORATION: Classical and modern Islamic historians (Wael Hallaq, Kecia Ali, Asma Barlas, Amina Wadud) confirm the verses were progressive relative to 7th-century jahiliyya customs. No non-beneficiary source corroborates that the specific patriarchal reading (as opposed to the verses' original progressive function) remains necessary for social stability today. The corroboration comes from outside the beneficiary set (patriarchal elites and conservative scholars).
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because the constraint transfers concrete material rights (inheritance, testimony weight, legal autonomy) from women to men and interpretive authority from plural traditions to a single authorized reading. Suppression is higher than extractiveness because the constraint's persistence depends on actively excluding reformist scholars from fatwa councils, blocking NGO participation in law reform, and criminalizing 'unauthorized' interpretation in several jurisdictions. Theater ratio is substantial: many judges apply equitable outcomes (e.g., equal testimony in practice) while citing classical texts, creating a performative layer. Accessibility collapse is partial — reformist readings exist and circulate digitally — but identity-locked exit options keep most women within the framework. Resistance is significant and growing through transnational advocacy networks.
 *
 * PERSPECTIVAL GAP:
 *   From the patriarchal elite seat, the arrangement is genuine coordination (rope-like): it provides stable, textually grounded family law across diverse societies. From the Muslim woman seat, the same structure operates as enforced extraction (snare-like): differentiated rights are maintained by excluding her voice from interpretation. From the reformist scholar seat, it is a tangled rope: the coordination function (stable family law) is real but the extraction (gender hierarchy) is not necessary for it — maqasid reinterpretation could preserve coordination while eliminating extraction. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarchal elites and traditional courts are structural beneficiaries (d near 0.0-0.2): they collect interpretive authority, institutional jurisdiction, and communal loyalty. Conservative scholars are beneficiaries with identity-locked exit (d ~0.15): their professional capital depends on the classical corpus. Muslim women, female heirs, and female witnesses are structural targets (d near 0.8-0.95): they bear the material and symbolic costs with identity-locked or constrained exit. Reformist scholars and rights NGOs are excluded (d not computed — they are outside the constraint's direct operation but structurally pressured by it). The general Muslim public sits near symmetric (d ~0.5): subject to the law but also invested in its legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century tribal stabilization) is dead — the historical context that made the verses progressive is gone. Yet the arrangement persists and has expanded through colonial-era codification into modern nation-state personal status codes. This is classic mandatrophy: the mandate (provide stable gender-equitable law for its time) has been replaced by a self-perpetuating institution (preserve the specific patriarchal reading as divine ordinance). The constraint is not a piton — it is actively enforced and its beneficiaries (patriarchal elites, traditional courts) profit substantially from maintaining it. It is a tangled rope because the coordination function (stable personal status law) is genuine but the extraction (gender hierarchy) is asymmetric and requires active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the contextual-egalitarian reading a distinct constraint from the standing patriarchal arrangement it assesses, or does it constitute a competing constraint over the same kernel?',
    'Trace whether the reading''s proponents seek to replace the patriarchal interpretation within existing institutions (competing constraint) or to create parallel interpretive authorities (distinct constraint). Institutional reform vs. parallel structure.',
    'If competing constraint, the two readings are in direct structural contention over the same institutional seats. If distinct, they operate in parallel with different beneficiary/victim sets. Affects network.affects_constraints linkage and contamination analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether this reading instantiates a separate constraint or contests the same constraint seats.').

omega_variable(
    historical_progressiveness_evidence,
    'Were the Qur''anic gender verses genuinely progressive relative to 7th-century Arabian customs, or does this claim project modern egalitarian values onto the text?',
    'Comparative analysis of pre-Islamic Arabian inheritance, marriage, and testimony customs against the Qur''anic innovations. Archaeological and textual evidence from the period.',
    'If historically progressive, the contextual-egalitarian reading''s founding problem claim is empirically grounded (empirically_contingent axiom). If not, the reading''s foundational axiom loses its historical warrant and becomes purely conventional or theological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_progressiveness_evidence, empirical, 'Empirical verification of the reading''s historical claim about the verses'' original function.').

omega_variable(
    maqasid_derivation_scope,
    'Do maqasid al-shari''a principles (preservation of life, religion, intellect, lineage, property) necessarily entail full gender equality in inheritance, testimony, and guardianship, or can they be satisfied by differentiated but complementary roles?',
    'Survey of classical and contemporary maqasid scholarship (al-Shatibi, al-Raysuni, Auda, Hashim Kamali) on whether gender equality is a necessary implication of the five necessities (daruriyyat).',
    'If maqasid necessitates equality, the reading''s second foundational axiom is conventionally grounded within the tradition. If maqasid permits differentiated roles, the axiom is contested even within the reading''s own methodological framework, weakening its internal coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maqasid_derivation_scope, conceptual, 'Whether the reading''s core methodological move (maqasid -> gender equality) is internally coherent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement, judicial monopoly, criminalization of dissent) or internalized (women''s belief in the religious necessity of differentiated roles, identity fusion with communal norms)?',
    'Post-reform suppression trajectory: in jurisdictions that have reformed personal status law (e.g., Tunisia 1956, Morocco 2004), does women''s subjective sense of constraint persist? Longitudinal studies of attitudes after legal change.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after legal exit. If primarily structural, legal reform alone reduces effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the patriarchal interpretation''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_ce_tr_t1900, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(qgv_ce_tr_t1930, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(qgv_ce_tr_t1960, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(qgv_ce_tr_t1980, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(qgv_ce_tr_t2000, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(qgv_ce_tr_t2024, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(qgv_ce_be_t1900, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(qgv_ce_be_t1930, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement(qgv_ce_be_t1960, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(qgv_ce_be_t1980, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(qgv_ce_be_t2000, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(qgv_ce_be_t2024, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qgv_ce_su_t1900, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(qgv_ce_su_t1930, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(qgv_ce_su_t1960, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(qgv_ce_su_t1980, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(qgv_ce_su_t2000, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(qgv_ce_su_t2024, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, muslim_personal_status_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, transnational_islamic_fatwa_networks).

% DUAL FORMULATION NOTE:
% This constraint story is one of three in the quranic_gender_verses kernel family. The contextual_egalitarian reading sees the standing patriarchal arrangement as a tangled rope (moderate extraction, genuine coordination, active enforcement). The literal_hierarchical reading sees it as a mountain (negligible extraction, natural divine law). The progressive_abrogation reading sees it as a snare (pure extraction, superseded by later revelation). The three stories share the same referent (the Qur'anic text as kernel) but author different ε, beneficiaries, and claimed types per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, institutional, 0.1).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
