% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses Under Progressive Abrogation Reading
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This constraint story instantiates the PROGRESSIVE ABROGATION reading of
 *   contested Qur'anic gender verses. The reading asserts that later
 *   egalitarian verses (49:13: universal human dignity regardless of gender;
 *   33:35: parallel spiritual obligations) supersede earlier gender-specific
 *   rules (4:11 differential inheritance; 4:34 guardianship; 2:282 testimony)
 *   via the Islamic jurisprudence principle of naskh (abrogation). This
 *   reading claims to resolve textual contradiction by establishing a
 *   hermeneutical hierarchy where egalitarian principles are the normative
 *   endpoint. Under this reading, women gain formal legal equality in
 *   inheritance, testimony, and guardianship status. The constraint extracts
 *   heavily from traditional literal-reading jurisprudence communities and
 *   scholars, whose institutional authority and theological frameworks are
 *   delegitimized. The reading is itself extractive in the sense that it
 *   transfers interpretive authority to scholars who endorse abrogation and
 *   away from scholars who defend literal hierarchy. The ε value reflects the
 *   complete normative reversal the reading instantiates—not merely
 *   reinterpretation but abrogation of foundational rules. The claim/metric
 *   independence is deliberate: this reading is CLAIMED as tangled_rope
 *   (genuine coordination of contradictory texts + extraction of authority
 *   from literal-reading institutions) while the authored metrics reflect
 *   substantive extractiveness (high ε, suppression of literal-reading
 *   alternatives, theater ratio for theological justification work). The
 *   engine will measure whether this classification holds across seats.
 *
 * KEY AGENTS:
 *   - progressive_jurisprudence_scholars — institutional agenda-setters; control naskh principle interpretation and establish which verses supersede which; mobile exit (can be retained in academia regardless of institutional Islamic law authority)
 *   - women_under_abrogation_reading — powerless beneficiaries; gain formal legal equality if reading becomes authoritative; exit constrained by identity-fusion with Islamic tradition and institutional dependency on jurisprudence institutions for legal recognition
 *   - literal_reading_communities — organized payers; lose theological and legal foundation for gender hierarchy; exit identity-locked (rejection requires abandoning core community identity)
 *   - traditional_scholars_committed_to_literal_hierarchy — institutional payers; career and institutional standing built on defending literal hierarchy; exit identity-locked (adoption of abrogation reading requires public recantation)
 *   - institutional_islamic_law_authorities — institutional agenda-setters and payers; currently embed literal-reading rules into family law; face institutional restructuring if abrogation reading becomes authoritative; exit constrained by constitutional frameworks requiring explicit legislative reform
 *   - women_in_literal_communities — excluded powerless agents; their legal status is determined by the reading contest but they have no voice in it; trapped exit (excluded by institutional design)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.89).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.76).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.89).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses Under Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '5b5363e1-9cd4-42fb-a36f-1b880fd50872').
narrative_ontology:cs_kernel_codification('5b5363e1-9cd4-42fb-a36f-1b880fd50872', fixed_text).
narrative_ontology:cs_authority_grounding('5b5363e1-9cd4-42fb-a36f-1b880fd50872', lineage).
narrative_ontology:cs_interpretation_layer_present('5b5363e1-9cd4-42fb-a36f-1b880fd50872').
narrative_ontology:cs_reading_relation('5b5363e1-9cd4-42fb-a36f-1b880fd50872', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('5b5363e1-9cd4-42fb-a36f-1b880fd50872', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('5b5363e1-9cd4-42fb-a36f-1b880fd50872', foundational, naskh_applies_to_gender_hierarchy).
narrative_ontology:cs_axiom_status(naskh_applies_to_gender_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('5b5363e1-9cd4-42fb-a36f-1b880fd50872', naskh_applies_to_gender_hierarchy, deontological).
narrative_ontology:cs_axiom('5b5363e1-9cd4-42fb-a36f-1b880fd50872', foundational, later_egalitarian_verses_supersede_hierarchy).
narrative_ontology:cs_axiom_status(later_egalitarian_verses_supersede_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('5b5363e1-9cd4-42fb-a36f-1b880fd50872', later_egalitarian_verses_supersede_hierarchy, deontological).
narrative_ontology:cs_reference_frame('5b5363e1-9cd4-42fb-a36f-1b880fd50872', quranic_teleology_toward_universal_equity).
narrative_ontology:cs_drift_state('5b5363e1-9cd4-42fb-a36f-1b880fd50872', contemporary_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b5363e1-9cd4-42fb-a36f-1b880fd50872', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_under_abrogation_reading).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, egalitarian_jurisprudence_tradition).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literal_reading_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_scholars_committed_to_literal_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, institutional_islamic_law_authorities).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, naskh_principle_validity).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, quranic_trajectory_toward_equity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and theologians who adopt and teach the progressive abrogation reading. They set the hermeneutical frame by selecting which verses supersede which, justify the naskh principle application, and authoritatively interpret 49:13 and related later verses as the normative endpoint. They control Islamic jurisprudence institutions, publish theological work, and influence Islamic law reform in multiple jurisdictions. They present this reading as the only legitimate integration of Qur'anic equity principles.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_jurisprudence_scholars, agenda_setter,
    institutional, generational, mobile, global).

% Women whose legal status, inheritance rights, testimony weight, and guardianship requirements would be fully equalized under the progressive abrogation reading. They gain formal legal parity, equal inheritance, equal testimony, and removal of male guardianship requirements. Their exit is constrained by institutional dependency on jurisprudence institutions for legal recognition and by identity-fusion with the Islamic tradition itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_under_abrogation_reading, beneficiary,
    powerless, generational, constrained, global).

% The intellectual and institutional tradition that gains vindication and authority when the progressive abrogation reading is adopted. This reading legitimizes the tradition's methods, textual interpretations, and policy recommendations. As a non-agent entity, it collects no material benefit but gains institutional standing and epistemic authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, egalitarian_jurisprudence_tradition, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(quranic_gender_verses__progressive_abrogation, egalitarian_jurisprudence_tradition).

% Muslim communities, scholars, and institutions whose theological identity and legal frameworks are built on the literal interpretation of gender verses (4:11, 4:34, 2:282 as binding divine ordinance). Under the abrogation reading, these verses are downgraded from operative law to historically contextual steps—a fundamental delegitimization of their interpretive tradition. Their legal institutions, family law, inheritance practices, and gender norms lose textual foundation. Exit is identity-locked: rejecting the literal reading requires abandoning a core identity commitment.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literal_reading_communities, payer,
    organized, generational, identity_locked, global).

% Scholars of Islamic jurisprudence who have built careers, institutional standing, and theological reputations on defending the literal interpretation of gender verses as divine hierarchy. The progressive abrogation reading directly contradicts their scholarly life-work and institutional authority. Adopting the reading would require public recantation, career disruption, and loss of standing within traditional jurisprudence institutions. Their exit is identity-locked: their professional identity is constituted through the literal-hierarchy framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_scholars_committed_to_literal_hierarchy, payer,
    institutional, biographical, identity_locked, global).

% State-level Islamic courts, family law authorities, and Ministry of Religious Affairs offices in Muslim-majority jurisdictions. These institutions currently embed literal-reading gender rules into family law, inheritance law, and testimony standards. If the progressive abrogation reading becomes authoritative, they face institutional restructuring, legal reform requirements, and loss of the literal-text justification for their current practices. Their exit is constrained by constitutional and legal frameworks that would require explicit legislative change.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, institutional_islamic_law_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, institutional_islamic_law_authorities, payer).

% International human rights bodies, legal scholars, and NGOs that monitor Islamic law implementations. They observe the reading contest and its consequences for women's legal status. They have no standing to adjudicate the theological reading but have external incentive structures (human rights frameworks) that align them with outcomes favoring gender equality.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, western_secular_authorities, observer,
    institutional, generational, analytical, global).

% Scholars of comparative law, religious studies, and Islamic jurisprudence who lack standing to interpret the Qur'an within the Islamic tradition itself. They conduct external analysis but are structurally excluded from the hermeneutical authority that determines which reading prevails within Islamic jurisprudence. Their exclusion is jurisdictional, not contested.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, non_muslim_jurists_and_scholars, excluded,
    analytical, generational, analytical, global).

% Women whose legal status, family arrangements, and inheritance rights are governed by literal-reading family law codes in Muslim-majority jurisdictions. They are excluded from the theological debate over which reading is correct. If the progressive abrogation reading becomes authoritative in their jurisdiction, they gain legal reforms they did not author; if it remains minoritized, they remain under literal-hierarchy law they did not consent to adopt. Their voice in the reading contest is absent by institutional design.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_in_literal_communities, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, progressive_jurisprudence_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates scattered gender-specific rulings (4:11 inheritance, 4:34 guardianship, 2:282 testimony) into a coherent normative system where later egalitarian verses (49:13 universal dignity) take hermeneutical priority via the naskh principle. Solves the textual coherence problem: the Qur'an contains both hierarchical and egalitarian principles; the abrogation reading coordinates them by establishing a principle of supersession.
% TRANSFER_FUNCTION: Moves theological authority and institutional standing from traditional literal-hierarchy jurisprudence schools to progressive abrogation jurisprudence schools. Transfers women's legal status from differential rights to formal equality. The constraint itself is a transfer of interpretive power: who controls naskh principle application controls which verses are operative and which are historically situated.
% ABSENT_VOICES: Women in literal-reading communities are excluded from the theological debate that determines their legal status. They are neither consulted on whether the abrogation reading should prevail nor empowered to contest it if it is imposed. Literal-reading scholars are formally present but their voice is reframed as error rather than legitimate difference. Non-Muslim legal experts and secular human rights advocates observe but have no hermeneutical standing to influence the outcome.
% DISAPPEARANCE_RATIONALE: If the progressive abrogation reading were to disappear—if the hermeneutical principle suddenly became inaccessible or universally rejected—traditional literal-reading jurisprudence would reestablish full institutional authority over Islamic law and family law across Muslim-majority jurisdictions. Women's legal status would revert to differential rights frameworks. Millions of people would lose legal claims (full inheritance, independent testimony) that depend on the abrogation reading's authority. Multiple Islamic jurisprudence schools, scholarly institutions, and law reform initiatives would be unmade.
% FOUNDING_PROBLEM: The Qur'an appears to contain both gender-hierarchical rules (verses 4:11, 4:34, 2:282) and universalist equity principles (49:13: 'no preference except in piety'; 33:35 parallel spiritual obligations). Early Islamic jurisprudence schools did not systematically resolve this contradiction. The founding problem is: how to read the Qur'an as a coherent legal source when it contains prima facie contradictory provisions on gender? The progressive abrogation reading solves this by treating later egalitarian verses as abrogating earlier hierarchical rules.
% FOUNDING_PROBLEM_CORROBORATION: Progressive abrogation scholars attests the problem is live and their reading solves it (internal authority). Traditionalist scholars contest that there is a contradiction or that the naskh principle applies to gender verses (internal authority, opposed). External corroboration comes from: (a) comparative religious studies scholars who document the textual tension as a historical fact; (b) women's rights advocates who note that interpretive choices have documented legal consequences for women's status; (c) Islamic law historians who trace the development of naskh jurisprudence. The founding problem's existence is corroborated independently; its solution status remains internally contested.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89 at interval end) because the reading accomplishes complete normative reversal: fundamental legal categories (inheritance rights, testimony weight, guardianship authority) are redefined. This is not incremental reinterpretation but wholesale abrogation of operative legal rules. The reading extracts authority from traditional institutions through the hermeneutical mechanism itself—by establishing naskh as the controlling principle, progressive scholars delegitimize literal-reading jurisprudence schools as failing to apply the principle correctly. Suppression is high (0.76) because the abrogation reading's persistence requires actively defending the naskh principle against literalist challenge, denying literal-reading scholars' hermeneutical standing, and managing the institutional consequences (resistive traditional institutions, women in literal communities objecting to sudden legal changes or mourning lost identity coherence). Theater_ratio is moderate (0.42) because the theological justification work (demonstrating why 49:13 abrogates 4:34, how coherence is achieved) is genuine hermeneutics, but a growing share of effort post-adoption is managing the institutional and identity damage inflicted on literal-reading communities. The measurement series traces a trajectory from initial abrogation-reading adoption (lower extractiveness, active suppression as resistance mounts) toward a stabilized state (higher extractiveness, theater rises as performance replaces contestation, suppression persists). The grid is shared across all three metrics at every time point, honoring the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive scholars' seat, the reading solves a genuine textual coherence problem and establishes equity as the Qur'an's trajectory. From the literal-reading communities' seat, the reading is hermeneutical violence—the delegitimization of their entire jurisprudence tradition and identity framework. From women under literal-reading law, the reading offers liberation but at the cost of either accepting that their entire legal world was error, or bearing the identity rupture of legal change imposed without their voice. From institutional Islamic law authorities, the reading threatens jurisdictional authority and constitutional stability. The engine should compute a vastly different effective extraction χ for each seat: progressive scholars experience low/negative χ (authority gain), literal communities experience high χ (authority and identity loss), women experience asymmetric χ (formal gain, identity cost), institutional authorities experience constrained χ (threatened jurisdiction). This asymmetry is the structural core of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: progressive scholars and women under the reading are listed as beneficiaries because the reading's operation benefits them—scholars gain authority, women gain formal equality. However, directionality differs sharply. Progressive scholars derive d near 0.0 (full beneficiary) because they control the reading, set its terms, and experience institutional authority gain. Women derive d near 0.5-0.6 (mixed) because they gain formal equality but lose identity coherence, face exit barriers (identity-locked to Islamic tradition), and have no seat at the hermeneutical table that determines their legal status. Victim derivation: literal-reading communities and traditional scholars are victims because the reading's operation degrades their theological authority and institutional standing. Literal communities derive d near 0.9 (near-full target) because they are powerless, exit-locked (identity-fusion), and have no mobility to adapt. Traditional scholars derive d near 0.85 (high target) because they are identity-locked (career built on literal hierarchy) even though they are institutional actors. The directionality hierarchy exposes the reading's asymmetric character: powerful progressive scholars extract from trapped literal communities via hermeneutical authority delegation. No override is needed; the structural data produce the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint demonstrates how mandatrophy can hide inside coordination claims. The founding problem is genuine: the Qur'an does contain contradictory provisions on gender. The naskh principle is a real hermeneutical tool that Islamic jurisprudence uses for other textual tensions. However, the founding problem is CONTESTED—traditionalist scholars argue there is no contradiction (verses can be read harmoniously), that naskh does not apply to ethical-universal principles (only to specific historical rulings), or that both verses are eternally valid in different contexts. The reading's persistence does not depend on consensus that the problem exists or that the solution is correct. It depends on institutional authority: progressive scholars have gained standing in international Islamic jurisprudence bodies, have been adopted by reformist states seeking gender law modernization, and have been incorporated into academic Islamic studies. This institutionalization can create the appearance of solving the problem (mandatrophy) when in fact the problem is still contested among Islamic jurisprudence communities. The high theater_ratio (rising from 0.28 to 0.42) reflects increasing performative work to maintain the reading's authority despite ongoing literalist scholarship and community resistance. The measurement series shows base_extractiveness rising and stabilizing as progressive scholars consolidate institutional authority and suppress competing hermeneutics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_principle_theological_validity,
    'Is the naskh (abrogation) principle a legitimate hermeneutical tool for gender verses, or does its application to gender violate the Qur''an''s self-presented ethical-universality claim?',
    'Textual analysis of how naskh operates in Islamic jurisprudence for other verse pairs, comparison of gender verses'' theological weight and positioning to verses where naskh is uncontested, examination of whether gender equity is presented as an ethical principle (subject to abrogation) or a foundational divine ordinance (exempt from abrogation). Post-institutional analysis: if progressive abrogation scholars cannot defend their principle application under scrutiny from literalist jurisprudence expertise, the principle''s legitimacy collapses.',
    'If naskh does not validly apply to gender verses, the entire progressive abrogation reading is hermeneutically incoherent—the constraint evaporates as merely false interpretation, not as an alternative reading. If naskh is valid but contested, the reading remains a live alternative but loses claim to uniquely correct interpretation. This directly affects whether the reading can sustain institutional authority and whether its extractiveness is justified by hermeneutical legitimacy or operates as pure authority seizure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_principle_theological_validity, conceptual, 'Whether the naskh principle application is hermeneutically sound or represents category error.').

omega_variable(
    identity_fusion_suppression_mechanism,
    'For literal-reading communities and traditional scholars marked identity-locked, is the suppression of their alternative reading structural (external institutional barriers to alternative interpretation) or internalized (the reading has fused with their self-concept such that questioning it destabilizes identity)?',
    'Post-exit trajectory analysis: if scholars and community members who leave literal-reading institutions or adopt progressive abrogation spontaneously report identity rupture, grief, or continued adherence to literal interpretations despite rejection of institutional structures, the suppression is partially internalized. If they rapidly adopt egalitarian frameworks after institutional exit, suppression is primarily structural. Longitudinal studies of generation gaps (do children raised in progressive institutions naturally adopt abrogation, or do they inherit the literal-reading identity commitment despite institutional change?) provide evidence of internalization depth.',
    'If suppression is primarily structural, removing institutional barriers (allowing literal-reading scholarship, permitting institutional pluralism) would enable exit and allow alternatives to compete. If suppression is substantially internalized, the constraint carries its coercive force across institutional boundaries and into individual cognition. Internalized suppression requires therapeutic or identity-work interventions, not merely institutional reform. The effective suppression value is higher when internalized because it persists across structural changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_suppression_mechanism, empirical, 'Whether suppression of literal-reading alternatives is structural or internalized.').

omega_variable(
    women_agency_in_abrogation_adoption,
    'Do women in communities adopting the progressive abrogation reading genuinely prefer the legal equality outcome, or does their apparent acceptance reflect coercion by progressive authorities who present the reading as inevitable?',
    'Pre-reform and post-reform surveys asking women about their preferences for legal status independent of which reading is official, whether they understood the reading change as chosen or imposed, whether they experience the legal equality as liberation or as loss of cultural coherence. Comparison of post-reform patterns: if divorce rates spike, property litigation rises, or women report distress disproportionate to legal gains, the reform imposed costs (identity rupture, institutional instability) that offset formal legal gains.',
    'If women genuinely prefer abrogation-reading legal status, the reading''s extractiveness from women-in-literal-communities is justified by their benefit. If women experience it as imposed, the reading becomes additionally extractive from women themselves—it extracts consent to new identity and legal framework without consulting those affected. The beneficiary classification would shift: women might not be genuine beneficiaries but co-targets of extraction (alongside literal-reading scholars). This would reclassify the constraint from tangled_rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_agency_in_abrogation_adoption, empirical, 'Whether women in adopting communities genuinely prefer abrogation-reading legal status or experience it as imposed.').

omega_variable(
    kernel_reading_foreclosure_impossibility,
    'Is foreclosure actually structurally impossible for this kernel—that is, can all three readings (literal, contextual, progressive) coexist indefinitely as live theological alternatives within Islamic jurisprudence, or does adoption of one reading logically eliminate the others?',
    'Logical analysis: do the three readings'' foundational axioms directly contradict (foreclosure possible) or merely prioritize differently (coexistence possible)? Institutional analysis: do progressive, contextual, and literal jurisprudence schools currently coexist in living Islamic scholarship, or does one domination require the suppression of others? Historical analysis: has the Islamic tradition sustained multiple gender-reading schools for extended periods, or does theological authority eventually consolidate around one reading?',
    'If foreclosure is impossible (true coexistence), the constraint is misclassified: it is not a zero-sum rewriting but a contentious landscape of alternatives. Classification would shift from tangled_rope toward piton (performative maintenance of contested superiority, not functional coordination). If foreclosure is logically possible (readings do mutually exclude), understanding WHICH reading forecloses which affects the reading_relations classification: progressive vs. literal may be genuine foreclosure (both claim universality and absoluteness), while progressive vs. contextual may be mere coexistence (both deny literal hierarchy but differ on mechanism). Current authored relations reflect true coexistence (all three marked coexists_with); if foreclosure is discovered, the relations field must be revised.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_impossibility, conceptual, 'Whether kernel readings can genuinely coexist or whether one adoption necessarily forecloses others.').

omega_variable(
    epistemic_violence_cost_quantification,
    'The expected structural delta mentions ''risk of epistemic violence against communities whose identity is bound to literal reading.'' What is the magnitude of this cost, and how is it measured against the benefit of women''s legal equality?',
    'Develop a framework for measuring epistemic violence: loss of hermeneutical standing, delegitimization of scholarly tradition, institutional exclusion, identity rupture. Compare pre/post-adoption mental health data, community cohesion measures, rates of theological crisis or faith abandonment in literal-reading communities. Weigh epistemic violence cost against women''s legal autonomy gains. If epistemically violent reform produces legal equality but requires destroying the identity coherence of half the affected population, is the constraint justified, or does it transform into a zero-sum zero-consent reform?',
    'If epistemic violence costs are quantified as substantial, the classification may shift: the constraint would be revealed as purely extractive from literal-reading communities (their loss is not offset by gain), raising the possibility that the reading is a snare, not tangled_rope. If costs are small, the reading''s extraction is offset by genuine women-beneficiary gain, preserving tangled_rope classification. High epistemic violence cost would argue for institutional pluralism (allowing literal and progressive schools to coexist) over abrogation-reading dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_cost_quantification, preference, 'Measurement of epistemic violence cost against women''s legal equality benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t7, quranic_gender_verses__progressive_abrogation, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(qura_tr_t7, observed).
narrative_ontology:measurement(qura_tr_t14, quranic_gender_verses__progressive_abrogation, theater_ratio, 14, 0.35).
narrative_ontology:measurement_basis(qura_tr_t14, observed).
narrative_ontology:measurement(qura_tr_t21, quranic_gender_verses__progressive_abrogation, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(qura_tr_t21, observed).
narrative_ontology:measurement(qura_tr_t28, quranic_gender_verses__progressive_abrogation, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(qura_tr_t28, observed).
narrative_ontology:measurement(qura_tr_t35, quranic_gender_verses__progressive_abrogation, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(qura_tr_t35, projected).
narrative_ontology:measurement(qura_tr_t42, quranic_gender_verses__progressive_abrogation, theater_ratio, 42, 0.42).
narrative_ontology:measurement_basis(qura_tr_t42, projected).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(qura_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t7, quranic_gender_verses__progressive_abrogation, base_extractiveness, 7, 0.76).
narrative_ontology:measurement_basis(qura_be_t7, observed).
narrative_ontology:measurement(qura_be_t14, quranic_gender_verses__progressive_abrogation, base_extractiveness, 14, 0.81).
narrative_ontology:measurement_basis(qura_be_t14, observed).
narrative_ontology:measurement(qura_be_t21, quranic_gender_verses__progressive_abrogation, base_extractiveness, 21, 0.85).
narrative_ontology:measurement_basis(qura_be_t21, observed).
narrative_ontology:measurement(qura_be_t28, quranic_gender_verses__progressive_abrogation, base_extractiveness, 28, 0.87).
narrative_ontology:measurement_basis(qura_be_t28, observed).
narrative_ontology:measurement(qura_be_t35, quranic_gender_verses__progressive_abrogation, base_extractiveness, 35, 0.88).
narrative_ontology:measurement_basis(qura_be_t35, projected).
narrative_ontology:measurement(qura_be_t42, quranic_gender_verses__progressive_abrogation, base_extractiveness, 42, 0.89).
narrative_ontology:measurement_basis(qura_be_t42, projected).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.89).
narrative_ontology:measurement_basis(qura_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t7, quranic_gender_verses__progressive_abrogation, suppression_requirement, 7, 0.68).
narrative_ontology:measurement_basis(qura_su_t7, observed).
narrative_ontology:measurement(qura_su_t14, quranic_gender_verses__progressive_abrogation, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(qura_su_t14, observed).
narrative_ontology:measurement(qura_su_t21, quranic_gender_verses__progressive_abrogation, suppression_requirement, 21, 0.73).
narrative_ontology:measurement_basis(qura_su_t21, observed).
narrative_ontology:measurement(qura_su_t28, quranic_gender_verses__progressive_abrogation, suppression_requirement, 28, 0.75).
narrative_ontology:measurement_basis(qura_su_t28, observed).
narrative_ontology:measurement(qura_su_t35, quranic_gender_verses__progressive_abrogation, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(qura_su_t35, projected).
narrative_ontology:measurement(qura_su_t42, quranic_gender_verses__progressive_abrogation, suppression_requirement, 42, 0.76).
narrative_ontology:measurement_basis(qura_su_t42, projected).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(qura_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, islamic_jurisprudence_authority_structure).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, women_legal_status_islamic_jurisdictions).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family: quranic_gender_verses. Three structurally distinct readings decomposed per DP-001 (ε-invariance): (1) literal_hierarchical — ε ~0.15, gender hierarchy is divine ordinance, constrains women's legal status; (2) contextual_egalitarian — ε ~0.25, verses are historically situated, require reinterpretation under equity principles, moderate extraction; (3) progressive_abrogation — ε ~0.89 (this reading), later verses abrogates earlier hierarchy, complete normative reversal, very high extraction from literal-reading communities. Each reading emerges from the same Qur'anic text but applies different hermeneutical principles (literalism vs. contextualization vs. abrogation), producing radically different operative law and gender status outcomes. The three are linked: progressive_abrogation affects both sibling readings by delegitimizing literal hierarchy and offering an alternative to contextual reinterpretation. This story focuses only on the progressive_abrogation reading; the others are separate constraint stories with their own ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
