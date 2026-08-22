% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses (Literal Hierarchical Reading): Male Guardianship & Differentiated Rights
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   Verses 4:11, 2:282, 4:34 of the Qur'an establish gender-differentiated
 *   legal rights as divine ordinance in Islamic jurisprudence. The
 *   literal-hierarchical reading interprets these verses as timeless,
 *   non-contextual legal constraints: verse 4:11 assigns inheritance to males
 *   at twice the female share; verse 2:282 weights female testimony at half
 *   that of male testimony in financial disputes; verse 4:34 establishes male
 *   guardianship (qawwamun) and authority over wives and daughters. This
 *   reading is institutionalized in Islamic family courts across many
 *   jurisdictions, enforced through jurisprudential tradition, and defended
 *   by male religious scholars and male household heads as divine law. The
 *   constraint simultaneously coordinates Islamic family law (establishing
 *   stable property and authority rules) and extracts through gender
 *   hierarchy (concentrating property and decision-making authority in male
 *   hands while constraining women's legal autonomy). The measured
 *   extractiveness (0.82) and suppression (0.76) reflect the structural
 *   asymmetry: male beneficiaries derive uncontested authority; female
 *   targets face identity-locked suppression where exit (apostasy, family
 *   rupture) carries spiritual and social death costs. Theater is low (0.22)
 *   because the constraint's function is transparent—no performance masks the
 *   hierarchical structure; what exists is defensive theological
 *   argumentation that the hierarchy is divinely mandated and therefore
 *   justified.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiaries; hold guardianship authority and larger inheritance shares; structurally arbitrary power over wives and daughters
 *   - male_religious_jurists: Institutional beneficiaries; administer the literal reading through Islamic courts and scholarly authority; their legitimacy is tied to maintaining the reading's authority
 *   - women_as_legal_subjects: Primary target; subject to guardianship, reduced inheritance, half-weight testimony; powerless with identity-locked exit
 *   - unmarried_women: Secondary target; guardianship by father/male relative; cannot marry autonomously; constrained property and work rights
 *   - contextual_egalitarian_interpreters: Excluded voices; would reframe these verses as 7th-century progress requiring modern reinterpretation through equity principles
 *   - progressive_abrogation_interpreters: Excluded voices; argue later egalitarian verses supersede gender-specific rules via naskh doctrine
 *   - women_seeking_legal_autonomy: Excluded from formal jurisprudence; internally contest guardianship but remain identity-fused to Islamic frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.76).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses (Literal Hierarchical Reading): Male Guardianship & Differentiated Rights").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '5849a7bd-b001-4943-98e9-b57c2db5a7c0').
narrative_ontology:cs_kernel_codification('5849a7bd-b001-4943-98e9-b57c2db5a7c0', fixed_text).
narrative_ontology:cs_authority_grounding('5849a7bd-b001-4943-98e9-b57c2db5a7c0', lineage).
narrative_ontology:cs_interpretation_layer_present('5849a7bd-b001-4943-98e9-b57c2db5a7c0').
narrative_ontology:cs_reading_relation('5849a7bd-b001-4943-98e9-b57c2db5a7c0', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('5849a7bd-b001-4943-98e9-b57c2db5a7c0', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('5849a7bd-b001-4943-98e9-b57c2db5a7c0', foundational, quranic_verses_timeless_divine_ordinance).
narrative_ontology:cs_axiom_status(quranic_verses_timeless_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('5849a7bd-b001-4943-98e9-b57c2db5a7c0', quranic_verses_timeless_divine_ordinance, theological).
narrative_ontology:cs_axiom('5849a7bd-b001-4943-98e9-b57c2db5a7c0', foundational, male_guardianship_divinely_mandated).
narrative_ontology:cs_axiom_status(male_guardianship_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('5849a7bd-b001-4943-98e9-b57c2db5a7c0', male_guardianship_divinely_mandated, deontological).
narrative_ontology:cs_reference_frame('5849a7bd-b001-4943-98e9-b57c2db5a7c0', divine_gender_hierarchy_framework).
narrative_ontology:cs_drift_state('5849a7bd-b001-4943-98e9-b57c2db5a7c0', contemporary_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5849a7bd-b001-4943-98e9-b57c2db5a7c0', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_religious_jurists).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_as_legal_subjects).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, unmarried_women).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_gender_hierarchy).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, quranic_immutability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold guardianship (wilayah) over wives and daughters under verses 4:34 and related jurisprudence. Control household property, marriage contract terms, divorce proceedings, and women's exit from the home. Receive larger inheritance shares under verse 4:11 (male = 2x female). Actively maintain this authority through family enforcement, religious interpretation, and legal advocacy in Islamic courts.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Adjudicate and transmit the literal reading of gender verses through fatwas, jurisprudential schools (madhabs), and Islamic legal scholarship. Their interpretive authority is grounded in the literal immutability of these verses. They administer Islamic family courts, issue rulings on marriage, divorce, inheritance, and women's legal capacity. The literal reading is foundational to their institutional legitimacy and scholarly tradition.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_religious_jurists, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Subject to male guardianship (qawwamun) over financial and personal decisions under verse 4:34. Receive half inheritance of male siblings under verse 4:11. Testimony in court carries half the weight of male testimony under verse 2:282. Cannot marry without wali (male guardian), often cannot divorce unilaterally, cannot travel or work without male permission in strict literal applications. Identity fusion: Islamic faith, family identity, and cultural belonging are inseparably bound to this legal framework for many women. Exit (apostasy, family rupture) carries severe social and spiritual costs.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_as_legal_subjects, payer,
    powerless, biographical, identity_locked, global).

% Under guardianship of father or closest male relative; marriage guardian (wali) requirement under verse 4:34 framework prevents autonomous marriage contracting. Inheritance rights reduced by half relative to brothers. If widowed or divorced, return to male guardianship. Limited capacity to work or own property independently in strict literal jurisdictions. Legal autonomy and exit options are severely constrained. Cultural/religious identity fusion makes exit from the framework equivalent to apostasy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, unmarried_women, payer,
    powerless, biographical, identity_locked, global).

% Islamic scholars and activists who argue these verses were historical-contextual progress for 7th-century Arabia and must be reinterpreted through overarching equity principles (maqasid al-sharia). Their arguments are structurally excluded from Islamic family courts that apply the literal reading in many jurisdictions. They would advocate for equal inheritance, autonomous marriage, equal testimony weight, and abolition of qawwamun guardianship if their framing were admitted.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_interpreters, excluded,
    organized, generational, trapped, global).

% Scholars and movements arguing that gender-specific verses are superseded by later Qur'anic principles of universal dignity (verse 49:13) via the doctrine of naskh (abrogation). Their interpretive framework would produce more egalitarian law if adopted. They are marginalized in most Islamic legal institutions that treat the gender verses as timeless and non-abrogable.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_abrogation_interpreters, excluded,
    organized, generational, trapped, global).

% Women who internally contest male guardianship but remain inside Islamic frameworks, seeking reform. Their objections are not formally part of the jurisprudential conversation in courts applying the literal reading. They are constrained by identity fusion (apostasy would mean spiritual death) and social structure (family, community rupture). Their voices are heard in activism, scholarship, and civil society but excluded from formal legal authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_seeking_legal_autonomy, excluded,
    powerless, biographical, identity_locked, global).

% International human rights bodies and secular courts in Muslim-majority and diaspora contexts assess these verses and their jurisprudential application against equality and non-discrimination norms. They cannot mandate Islamic law but document its effects and advocate for reform. They sit outside the Islamic jurisprudential system and cannot change its internal logic.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_courts_and_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes Islamic household law and family relations: specifies authority structures for marriage, divorce, inheritance, and property management; coordinates gender roles within Islamic ethical and legal frameworks; provides stable rules for contract formation and dispute resolution in Islamic family law.
% TRANSFER_FUNCTION: Transfers property (via reduced inheritance shares), labor (obedience to guardians), legal authority (half-weight testimony), and decision-making autonomy from women to male household heads and male religious jurists. The transfer is the mechanism that sustains male structural authority and female legal subordination.
% ABSENT_VOICES: Contextual-egalitarian and progressive-abrogation interpreters are structurally excluded from Islamic family courts in jurisdictions applying the literal reading. Women whose consent to guardianship is coerced by identity-fusion are not freely in the conversation. Secular women's rights advocates and international human rights bodies are excluded from jurisprudential authority but document the effects.
% DISAPPEARANCE_RATIONALE: If these verses were deemed non-binding or reinterpreted as historical-contextual rather than timeless, Islamic family law across jurisdictions applying them would reorganize fundamentally: inheritance would become equal, guardianship would dissolve, women's testimony would carry equal weight, autonomous marriage and divorce rights would emerge. Millions of women's legal status would shift, household property distributions would change, and the institutional authority of traditional Islamic jurisprudence would be substantially diminished. The reorganization would be total.
% FOUNDING_PROBLEM: Establish stable, divinely-sanctioned family structure and property inheritance rules in early Islamic Arabia; assign clear authority roles for household decision-making and resource distribution; protect women through male guardianship understood as protective obligation (qayyam carries responsibility connotation); regulate marriage and divorce within sacred law rather than pre-Islamic custom.
% FOUNDING_PROBLEM_CORROBORATION: The literal-hierarchical reading attests these verses directly solve foundational problems of family stability and inheritance clarity—the problems are understood as permanently live. Contextual-egalitarian and progressive-abrogation interpreters attest that the founding problems (7th-century Arabian social chaos, pre-Islamic infanticide of daughters) are dead or superseded; the modern founding problem is equality and women's human dignity, which these verses do not solve. Women's rights advocates and human rights bodies attest the founding problems are reframed—contemporary problems are gender-based discrimination and restricted autonomy, which this reading perpetuates. No single external corroboration exists; the three interpretive communities produce entirely opposed verdicts.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because male beneficiaries derive stable, substantial advantages (guardianship authority, doubled inheritance, full testimony weight) and these advantages are sustained independent of female consent or preference. Suppression is also high (0.76) because the constraint's persistence depends on active enforcement mechanisms: family law courts apply it, religious scholars defend it against reformist critique, and women's exit is suppressed not just by external barriers (legal disenfranchisement, economic dependency) but by internalized identity-fusion (Islamic identity becomes inseparable from acceptance of guardianship). Theater is low (0.22) because there is minimal performative masking—the hierarchy is defended as divinely mandated, not as incidental to coordination. The real coordination function (stable family law) is modest relative to the extractive work the constraint performs. Measurements track near-stability across the 35-year interval: extractiveness holds steady around 0.80–0.82, suggesting the constraint's intensity has not decayed nor been significantly challenged at the institutional level. Suppression requirement remains elevated and stable (0.72–0.76), indicating that maintaining the literal reading requires sustained active enforcement as alternative interpretations gain scholarly attention. The modest increase in theater ratio over 35 years (0.18 to 0.22) reflects increased defensive theological and jurisprudential argumentation against reform movements, not a shift in function—the performance is the scholarly defense of immutability against progressive reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary-seat perspective (male household head or jurist) experiences the constraint as divine law that protects women through guardianship, organizes stable property inheritance, and maintains family order. The extractiveness and suppression metrics appear as low from this perspective because the benefits (authority, property, institutional power) are felt as legitimate and the costs (discipline, authority maintenance) are rationalized as duty. The target-seat perspective (woman under guardianship) experiences the constraint as coercive hierarchy that restricts autonomy, reduces property rights, and enforces obedience through family and social structure. The extractiveness appears as high because the costs (guardianship, constrained inheritance, reduced testimony) are directly borne; the suppression appears as high because exit is identity-suicide. The measured metrics (extractiveness 0.82, suppression 0.76) describe the target-seat experience—they encode the asymmetry that someone loses what someone else gains. The beneficiary seat may experience these numbers as excessive or distorted; the target seat may experience them as inadequate to capture the internalized identity-lock. This is the perspectival gap the per-seat computation should surface: one architectural view showing coordination and stability; another showing extraction and coercion. Both are true; the gap is structural, not illusory.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and male religious jurists are structural beneficiaries with d near 0.0 (full beneficiary): the constraint directly grants them authority, property advantages, and institutional power; they have arbitrage-level exit (can reinterpret or migrate the reading, are not trapped by it). Women as legal subjects and unmarried women are structural targets with d approaching 1.0 (full target): the constraint directly restricts their autonomy, property rights, and decision-making; they are identity-locked (exit means apostasy/family rupture) and powerless, amplifying their vulnerability. Contextual-egalitarian and progressive-abrogation interpreters are excluded (d not computed; they sit outside the formal legal system). From the beneficiary seat (male jurist perspective), the constraint appears as legitimate coordination grounded in divine law, its extractive character is not apparent, and the measured suppression appears as necessary discipline. From the target seat (woman under guardianship perspective), the constraint appears as coercive hierarchy sustained by theological narrative, and the measured suppression is experienced as internalized identity-lock. The per-seat computation should reveal this asymmetry: the beneficiary seat computes a higher coordination benefit / lower extraction ratio; the target seat computes higher extraction / lower coordination benefit. Neither computation is wrong; they are different structural positions viewing the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establish stable, divinely-sanctioned family law) is live in the beneficiary seats and organizational structures, but dead or transformed in women's lived experience. Women face not a family-stability problem but a legal-subordination problem; the constraint that solved the original problem now perpetuates a different problem. The constraint's mandate has outlived its justification for the target seats. However, mandatrophy is contested because the beneficiary seats and the institutional religious hierarchy maintain that the founding problem is permanently live—gender hierarchy is not understood as a means to family stability but as an end in itself, divinely mandated. This is precisely the contestation that the three readings embody: the literal-hierarchical reading holds the mandate is live and timeless; the contextual-egalitarian reading holds the mandate was specific to 7th-century Arabia; the progressive-abrogation reading holds the mandate has been superseded by later principles. Mandatrophy resolution is omega-dependent—it requires external assessment of whether the founding problem is actually live, which the three readings answer completely differently. The constraint persists despite mandate-contestation because institutional authority (courts, scholars, families) continues to enforce the literal reading; the persistence is institutional inertia rather than widespread belief that the founding problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_hierarchy,
    'Is gender hierarchy in these verses a natural law (unchangeable structure of reality) or a constructed constraint (a choice to interpret and enforce the text this way)?',
    'The question is not empirically resolvable; it depends on theological commitments about divine inspiration and textual immutability. Resolution requires external assessment: do people hold these verses as divine precisely because they experience the hierarchy as natural, or do they construct naturality narratives around the hierarchy to defend institutional power? Post-exit suppression trajectories (women who leave the framework and are no longer under guardianship—do they experience previously internalized suppression as external imposition?) provide indirect evidence but cannot definitively resolve.',
    'If the hierarchy is truly natural law, the constraint would classify as mountain with near-zero resistance and near-universal accessibility collapse. The measured high suppression (0.76) and moderate resistance (0.64) would instead indicate the constraint''s constructed character, supporting tangled_rope or snare classification. FSM analysis (false-summit detection) would trigger, reclassifying the constraint away from mountain toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturality_vs_constructed_hierarchy, conceptual, 'Whether verses encode natural law or constructed gender hierarchy defended as natural law.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.76) primarily structural (external legal barriers, economic dependency, lack of alternatives) or internalized (identity-fusion where women internalize the constraint as legitimate)?',
    'Observe trajectories of women who exit the constraint (apostasy, secular law adoption, migration to secular jurisdictions): do they report suppression persisting after structural barriers are removed? If suppression persists (continued difficulty with autonomous decision-making, belief they should defer to men despite legal equality), the suppression is internalized. If suppression dissolves quickly after structural removal, it was primarily structural. Combined survey evidence from women in strict literal jurisdictions vs. women who exited, plus qualitative interviews on identity-lock phenomenology.',
    'If suppression is primarily structural, the constraint could be weakened by changing laws and economic conditions. If suppression is substantially internalized, the constraint is carried by affected parties themselves; weakening it would require identity reconstruction that is existentially costly. High internalization amplifies effective suppression and increases exit costs, potentially supporting snare classification over tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'What portion of the constraint''s suppressive force is external structure vs. internalized identity-fusion.').

omega_variable(
    foundational_problem_liveness_contest,
    'Is the founding problem (establishing stable, divinely-sanctioned family law) actually live, or has it been replaced by a different foundational problem (protecting women''s equality and autonomy)?',
    'Compare three contexts: (1) Muslim-majority jurisdictions applying the literal reading—do they report stable, functioning family law as the primary benefit, or do they report inequality and women''s legal subordination as the primary problem? (2) Women''s lived experience surveys: do women in strict literal jurisdictions report stable, protected family structures as benefits, or coercion and constraint as harms? (3) Reform advocacy: what problem do contextual-egalitarian and progressive-abrogation movements frame as primary? Consensus across these contexts would resolve liveness.',
    'If the founding problem is dead (family instability is not the live problem; gender equality is), the constraint has entered mandatrophy: it persists institutionally even though its original justification no longer applies. This supports reclassification away from rope (genuine coordination for a live problem) toward snare or piton (extraction or inertia). If the founding problem is contested (different communities experience it as live vs. dead), the classification should reflect that ambiguity—the constraint solves a problem for beneficiary seats but fails/perpetuates a problem for target seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_problem_liveness_contest, empirical, 'Whether the founding problem persists or has been replaced by a different problem that the constraint now perpetuates.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the three readings of the gender-verses kernel foreclose each other logically (no framework could hold two simultaneously) or coexist as live positions held by different institutional actors?',
    'Test whether a single coherent Islamic theological framework can incorporate two readings without contradiction. For example: can a jurist hold both ''these verses are timeless AND they are historically contextualized''? Can they hold ''these verses are binding AND they are superseded by later principles''? Examine actual jurisprudential literature for frameworks attempting to integrate readings; survey Islamic scholars on whether integration is logically possible or whether choosing one reading requires rejecting the other.',
    'If readings foreclose each other, the relation is forecloses; the constraint''s classification would be determined by which reading wins institutional authority. If readings coexist (each is coherent within its own framework, and different communities adopt different frameworks), the relation is coexists_with; the classification reflects the winner''s position locally, but globally multiple classifications apply to the same kernel. This affects corpus-wide analysis: does the gender-verses domain have one canonical constraint type or multiple coexistent ones?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the three readings logically foreclose each other or remain live coexistent positions.').

omega_variable(
    kernel_reading_vs_ordinary_constraint,
    'Is this story accurately framed as ONE reading of a contested kernel, or is it a standalone constraint that happens to be contested?',
    'Kernel reading requires: (1) a fixed textual or formal commitment (the kernel) that (2) multiple coherent readings produce from it (at least two readings that share the kernel but differ in interpretation). For gender-verses: the kernel is the Qur''anic text (4:11, 2:282, 4:34); the readings are literal-hierarchical, contextual-egalitarian, and progressive-abrogation. This meets the definition. The alternative would be: the constraint is a standalone interpretation of ambiguous source material, not a reading of a fixed kernel—in which case the three interpretations are just disagreement, not readings of a kernel. The distinction matters for cs_structure: the reading frame requires axioms, reference_frame, drift_state; the standalone frame would not.',
    'If this is accurately a kernel reading, cs_structure must include reading_relations, axioms, reference_frame, and drift_state. If it''s a standalone constraint, cs_structure can be simpler or omitted. The current story authors it as a reading (kernel_context is present, cs_structure includes reading_relations and axioms), which is correct IF the readings are distinguishable interpretations of a fixed text, not independent constraints on the same topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_ordinary_constraint, conceptual, 'Whether this constraint is accurately framed as a kernel reading or as a standalone interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t5, quranic_gender_verses__literal_hierarchical, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__literal_hierarchical, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quranic_gender_verses__literal_hierarchical, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__literal_hierarchical, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t25, quranic_gender_verses__literal_hierarchical, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(qura_tr_t25, observed).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__literal_hierarchical, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t35, quranic_gender_verses__literal_hierarchical, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(qura_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t5, quranic_gender_verses__literal_hierarchical, base_extractiveness, 5, 0.79).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__literal_hierarchical, base_extractiveness, 10, 0.8).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quranic_gender_verses__literal_hierarchical, base_extractiveness, 15, 0.81).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__literal_hierarchical, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t25, quranic_gender_verses__literal_hierarchical, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(qura_be_t25, observed).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__literal_hierarchical, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t35, quranic_gender_verses__literal_hierarchical, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(qura_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t5, quranic_gender_verses__literal_hierarchical, suppression_requirement, 5, 0.73).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__literal_hierarchical, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quranic_gender_verses__literal_hierarchical, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__literal_hierarchical, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t25, quranic_gender_verses__literal_hierarchical, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(qura_su_t25, observed).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__literal_hierarchical, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t35, quranic_gender_verses__literal_hierarchical, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(qura_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel decomposes into three constraint stories, one per reading. This story (literal_hierarchical) is linked to two siblings: contextual_egalitarian and progressive_abrogation. They share the same textual kernel (Qur'an 4:11, 2:282, 4:34) but produce different ε values, different beneficiary/victim structures, and different constraint types because the readings differ on the referent (timeless divine law vs. historical context vs. abrogated principle). Each reading is a coherent constraint story; the three together form a constraint family encoding the institutional contestation over Islamic gender law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
