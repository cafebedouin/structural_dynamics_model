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
 *   human_readable: Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   Qur'anic verses 4:11 (inheritance fractions), 2:282 (testimony weight in
 *   financial matters), and 4:34 (male guardianship/qawwamun status) are
 *   interpreted in the literal_hierarchical reading as direct, timeless
 *   divine ordinances establishing male authority over household and legal
 *   matters and differentiated property/testimony rights for women. This
 *   reading frames the hierarchy as natural law—divinely mandated, not
 *   historically contingent. Under this reading, male household heads and
 *   religious jurists are structural beneficiaries (they gain authority and
 *   interpretive power); women enter the victim set (constrained legal
 *   capacity, halved testimony, halved inheritance, identity-locked exit).
 *   The constraint is classified as tangled_rope: it solves a genuine
 *   coordination problem (household authority, property inheritance clarity)
 *   while asymmetrically extracting from women to male guardians. The
 *   contradiction between the literal reading's claim (natural divine
 *   ordinance) and the high extractiveness/active enforcement metrics is the
 *   point: the engine measures whether the structural data supports the
 *   naturalness claim; divergence is how false summits are detected.
 *
 * KEY AGENTS:
 *   - Male household heads: beneficiaries; structural authority over female guardianship, inheritance distribution, and legal representation.
 *   - Male religious jurists: agenda-setters; interpret and codify the literal reading; their institutional credibility depends on the constraint's persistence.
 *   - Women under guardianship: primary victims; constrained legal autonomy, halved inheritance, halved testimony weight; identity-locked exit.
 *   - Female testifiers: secondary victims; testimony systematically discounted in financial and sexual disputes; trapped exit.
 *   - Reform-minded Muslim scholars: excluded voices; would argue for contextual or progressive readings but face institutional marginalization.
 *   - Secular Muslim-majority states: observers; many have superseded the literal reading in civil law but tolerate it in personal/religious law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.79).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'd9a75c5e-d543-4677-ad1f-9440e4d171bf').
narrative_ontology:cs_kernel_codification('d9a75c5e-d543-4677-ad1f-9440e4d171bf', fixed_text).
narrative_ontology:cs_authority_grounding('d9a75c5e-d543-4677-ad1f-9440e4d171bf', lineage).
narrative_ontology:cs_interpretation_layer_present('d9a75c5e-d543-4677-ad1f-9440e4d171bf').
narrative_ontology:cs_reading_relation('d9a75c5e-d543-4677-ad1f-9440e4d171bf', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('d9a75c5e-d543-4677-ad1f-9440e4d171bf', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('d9a75c5e-d543-4677-ad1f-9440e4d171bf', foundational, verses_literal_timeless_ordinance).
narrative_ontology:cs_axiom_status(verses_literal_timeless_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('d9a75c5e-d543-4677-ad1f-9440e4d171bf', verses_literal_timeless_ordinance, theological).
narrative_ontology:cs_axiom('d9a75c5e-d543-4677-ad1f-9440e4d171bf', foundational, male_guardianship_divinely_mandated).
narrative_ontology:cs_axiom_status(male_guardianship_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('d9a75c5e-d543-4677-ad1f-9440e4d171bf', male_guardianship_divinely_mandated, deontological).
narrative_ontology:cs_reference_frame('d9a75c5e-d543-4677-ad1f-9440e4d171bf', quranic_hierarchy_as_final_ordinance).
narrative_ontology:cs_drift_state('d9a75c5e-d543-4677-ad1f-9440e4d171bf', contemporary_secularization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9a75c5e-d543-4677-ad1f-9440e4d171bf', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_religious_jurists).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_testifiers).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, male_guardianship_ordinance).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, differentiated_legal_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Serve as qawwamun (maintainers/guardians) of wives and daughters under the literal reading of 4:34. Exercise authority over female family members' property, marriage, and legal representation in both Islamic law and personal status law systems that enforce the literal reading. Collect rents in the form of property control, decision-making authority, and legal privilege. Can exit by adopting alternative theological readings or secular legal frameworks, but at moderate social cost within traditional communities.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary).

% Interpret, codify, and enforce the literal reading through formal jurisprudence (fiqh), institutional fatwas, and religious court authority. Their scholarly credibility and institutional power depend on the literal reading's legitimacy. Defend the reading against reform interpretations. Can theoretically exit by reinterpreting the verses, but doing so would fundamentally delegitimize their credentials as guardians of the textual tradition and would require institutional recalibration of their entire jurisprudential framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_religious_jurists, agenda_setter,
    institutional, generational, constrained, universal).

% Live under male guardianship authority in household, property, and legal matters under the literal reading. Their inheritance share is half that of male siblings (2:282). Their testamentary capacity, marriage autonomy, and independent financial decision-making are constrained by guardianship requirement. Exit from the constraint requires apostasy (rejecting Islam), which carries severe social, economic, family, and legal consequences—the identity frame ('Muslim woman') is constituted through the hierarchical relationship, making exit identity-rending rather than merely costful.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_under_guardianship, payer,
    powerless, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, women_under_guardianship, excluded).

% In financial and sexual disputes, their legal testimony carries half the evidentiary weight of male testimony (2:282: 'two women...or a woman and two men'). This testamentary asymmetry embeds them in a legal hierarchy where their voice is structurally worth less. They cannot exit by choosing a different legal system (the legal system itself instantiates the asymmetry); they cannot exit by apostasy without abandoning their entire legal/religious identity and family. Their position is the most constrained: the barrier is not an external rule they could choose to bypass, but the definition of their legal personhood itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_testifiers, payer,
    powerless, biographical, trapped, universal).

% Propose contextual, maqasid-based, or progressive-abrogation readings that reinterpret or supersede the literal hierarchical constraint. They are structurally excluded from traditional jurisprudential authority; their alternative framings are treated as revisionist or heterodox within literalist institutional frameworks. They bear reputational cost (delegitimization within traditional scholarship) and institutional marginalization (excluded from state-sanctioned fatwa bodies, religious courts, and conservative educational institutions). They cannot exit the Islamic tradition without abandoning their identity and intellectual heritage, so their dissent is constrained-exit costly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reform_minded_muslim_scholars, excluded,
    organized, biographical, constrained, universal).

% Many have codified civil law that contradicts the literal reading (equal inheritance, equal testimony weight, autonomous marriage and property rights for women). They observe the constraint's operation in parallel legal systems (personal status law, Shari'a courts) and have institutional leverage to choose which legal framework applies to which matters. They face political pressure from religious constituencies to preserve the literal reading in personal law while maintaining civil equality in commercial/public law.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_muslim_majority_states, observer,
    institutional, generational, arbitrage, national).

% CEDAW, ICCPR, and related treaty frameworks recognize equal legal capacity and non-discrimination on gender. They generate a structural mismatch with the literal hierarchical reading (which assigns women half-weight testimony and half inheritance). These frameworks are not directly enforceable on internal Islamic jurisprudence but create legitimacy contestation and interpretive pressure. They represent an alternative analytical seat measuring the constraint from outside its own authority structure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, international_human_rights_frameworks, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes explicit rules for household authority, property inheritance, and legal representation in the absence of alternative institutional arrangements (modern probate law, equal capacity regimes, joint property frameworks). The literal reading frames male guardianship as resolving the 7th-century coordination problem: who is responsible for widows, orphans, and dependents? Who holds testamentary authority? Who bears financial liability? The verses provide an answer: male guardians bear responsibility and hold authority. This coordination function is genuine for its time and place.
% TRANSFER_FUNCTION: Transfers property rights, inheritance entitlements, testamentary authority, and legal voice from women to male guardians and to male-dominated religious jurists (who enforce the rules). In concrete terms: women's inheritance share is half of men's share (2:282); women's testimony in financial and sexual matters counts as half (2:282); women's autonomy in marriage and property requires male guardian consent (4:34); religious courts enforce these asymmetries. The transfer is unidirectional: women do not gain reciprocal authority over male guardians.
% ABSENT_VOICES: Women scholars who would argue for alternative interpretations, secular legal reformers who have superseded the literal reading in civil law, Islamic reform scholars who argue for contextual or progressive readings, and international human rights advocates are structurally excluded from the literal reading's authority claim. They are not consulted in traditional fiqh interpretation; their alternative framings are treated as external to the divine ordinance. This exclusion is structural: the literal reading's authority depends on claiming the verses are self-evident divine law, not subject to contextual reinterpretation—inviting alternative voices would undermine that claim.
% DISAPPEARANCE_RATIONALE: If the literal hierarchical reading vanished overnight—replaced by equal inheritance, equal testimony weight, and female autonomous legal capacity—inheritance distributions would shift property flows (women would inherit equal shares), marriage contracts would operate under different consent terms (female autonomous choice), legal disputes would weight female testimony equally, and religious courts would function under different authority principles. Entire legal systems, family property structures, and judicial authority hierarchies built on this constraint would require recalibration. The world would not stay roughly the same—it would rearrange.
% FOUNDING_PROBLEM: In 7th-century Arabia, inheritance rules were ambiguous and contested—widows and daughters had uncertain property claims, creditors and debtors lacked clear rules for financial liability, and household authority was ill-defined. Dependent care (widows, orphans, the elderly) was a genuine coordination problem without clear institutional solutions. Verses 2:282 (inheritance fractions), 4:11 (testimony weight), and 4:34 (guardianship) provided explicit rules: male guardians bear financial responsibility and hold testamentary authority; women inherit at specified fractions; male testimony carries specified weight. These rules solved an ambiguity coordination problem by providing clarity.
% FOUNDING_PROBLEM_CORROBORATION: Literalist scholars attest the founding problem remains live: women still need guardianship protection, household financial authority still requires clarity, inheritance ambiguity still endangers dependents. Contextual scholars, secular Muslim-majority states, and Islamic reform scholars attest the founding problem is substantially dead—modern property law, equal legal capacity regimes, and alternative inheritance models (spousal property regimes, probate law) solve the stated problems without gendered hierarchy. Economic historians and legal anthropologists outside the benefiting parties support this assessment: the specific 7th-century coordination problem has been superseded. The contest is real: literalists claim the founding problem is eternal (women always need guardianship); reformers claim it was historically specific and has been solved. The corroboration comes from outside the beneficiary parties (secular legal systems, international frameworks, reform scholarship) and contradicts the literalist claim.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.82) because the constraint transfers significant property rights, legal voice, and autonomous decision-making from women to male guardians—the flow is uncompensated and asymmetric. The literal reading frames this as necessary (coordinating household authority, clarifying inheritance), but the asymmetry (why women but not men are subject to guardianship, why women's testimony is halved) is not explained by the coordination function alone; it serves the beneficiary interest. Suppression is high (0.79) because the constraint's persistence is defended not primarily by voluntary coordination but by legal enforcement, family sanction, institutional power (religious courts, state personal law), and identity-lock (apostasy/family rupture costs for exit). Theater ratio is moderate (0.28): the coordination rhetoric is real (the verses do address household authority and inheritance), but institutional energy increasingly goes to defending the male privilege and testimony asymmetry rather than to solving the founding coordination problem. Accessibility collapse is high (0.88) because once the constraint is understood as divinely ordained, alternatives are treated as apostasy/heresy—the interpretive space appears to close. Resistance is moderate (0.42) because reform readings exist and secular states have adopted alternatives, but the literal reading maintains institutional power in religious courts and in Salafist/traditional jurisprudence. The measurement series shows stability: extractiveness, suppression, and theater ratio have drifted slightly upward (0.78→0.82, 0.75→0.79, 0.22→0.28) as institutional literalism has hardened in some contexts and been challenged more actively in others, producing modest intensification on both dimensions but no collapse or radical shift.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat (male household heads, religious jurists), the constraint is coordination plus authority: the founding problem (inheritance ambiguity, household authority clarity) is real, the solution (male guardianship) is divinely mandated and morally binding, and enforcement is legitimate. From the payer's seat (women under guardianship), the same structure operates as illegitimate extraction: their testimony is arbitrarily halved, their inheritance is arbitrary, their autonomy is unjustly constrained, and enforcement is coercive. The engine computes this perspectival divergence from the power/exit/beneficiary data: male household heads compute as low-d (beneficiaries with arbitrage exit), women compute as high-d (powerless, identity-locked targets). No single institutional seat can hold both perspectives—they are locked in structural asymmetry. This asymmetry is the signature of tangled_rope: genuine coordination function (inheritance, guardianship clarity) entangled with asymmetric extraction (male privilege in property, testimony, autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads: d ≈ 0.15 (full beneficiary pole). They gain authority, property control, and legal privilege; their exit options are high (they can adopt alternative readings at modest institutional cost); their power is institutional. Directionality derivation: beneficiary role + institutional power + mobile exit = low d = subsidy/benefit. Male religious jurists: d ≈ 0.20 (beneficiary pole, constrained by institutional identity). They benefit from interpretive authority and institutional power; exit is constrained (abandoning the literal reading would delegitimize their scholarly credentials); power is institutional. Derivation: beneficiary role + institutional power + constrained exit (identity-locked on interpretive authority) = low-moderate d. Women under guardianship: d ≈ 0.88 (target pole). They bear the constraint asymmetrically; they are subject to guardianship but men are not; exit is identity-locked (apostasy/family rupture); power is powerless. Derivation: victim role + powerless + identity-locked exit = high d = full target. Female testifiers: d ≈ 0.92 (target pole). Their testimony is systematically discounted; they have no direct beneficiary claim; exit is trapped (legal system itself is the barrier, not a choice they can evade); power is powerless. Derivation: victim role + powerless + trapped exit = maximal target. Reform scholars: d ≈ 0.65 (partial target). They are excluded from authoritative interpretation; they bear reputational cost for proposing alternatives; power is organized; exit is constrained (abandoning Islamic framework entirely). Derivation: payer role (they pay institutional cost) + organized power + constrained exit = moderate target. Secular states: d ≈ 0.50 (symmetric). They observe the constraint's operation, they can choose whether to adopt or supersede it, they have arbitrage options (civil law instead of religious law); power is institutional. Derivation: observer role + institutional power + arbitrage exit = symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inheritance ambiguity, household authority clarity in 7th-century Arabia) is classified as 'contested' status at interval end. Literalist scholars attest the problem remains live (guardianship still necessary, household authority still contested). Contextual and secular observers attest the problem is substantially dead (modern property law, equal capacity frameworks, and alternative inheritance models solve the stated coordination problem; the literal reading persists as institutional tradition and religious authority, not as solution to a live functional problem). This status divergence triggers the mandatrophy mismatch: the constraint's classification as tangled_rope depends on the coordination function remaining live and asymmetric extraction remaining intentional corollary to coordination. If the founding problem is dead (authority clarity solved by secular law, inheritance ambiguity solved by equal capacity regimes), the constraint's persistence becomes pure extraction defended as natural—classification would shift to snare, and institutional maintenance becomes theater. The omega documenting this (literal_vs_contextual_hermeneutics) is exactly where the ambiguity lives: if the literal reading is one selected interpretation among valid Islamic alternatives (not the unique correct transmission), then persistence is institutional choice to defend male privilege, not coordination necessity. Mandatrophy_resolved should be flagged for review: the constraint's classification is sensitive to the hermeneutic question, and the hermeneutic question is irreducibly contested within Islamic scholarship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_contextual_hermeneutics,
    'Is the literal hierarchical reading a faithful transmission of the Qur''anic ordinance''s timeless meaning, or is it a historically specific interpretation selected from interpretive plurality within the Islamic tradition?',
    'Hermeneutic analysis of competing tafsir traditions, classical and contemporary Islamic jurisprudence, and linguistic/philological scholarship on the verses'' possible meanings. Examination of whether pre-modern Islamic scholars recognized multiple valid readings.',
    'If the literal reading is one selected reading among multiple valid Islamic alternatives, the constraint''s classification shifts from natural ordinance to constructed institutional choice—extractiveness remains high, but the beneficiary structure becomes agents defending a chosen reading rather than transmitting a fixed divine law. Classification may shift from tangled_rope (coordination + enforcement) to snare (pure extraction defended as natural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_vs_contextual_hermeneutics, conceptual, 'Whether literal reading is unique transmission or selected interpretation within Islamic hermeneutic pluralism.').

omega_variable(
    male_guardianship_necessity_empirical,
    'Is male guardianship structurally necessary to prevent social chaos, protect dependents, or manage household finance—or do alternative institutional arrangements (female legal capacity, joint property regimes, independent female testimony) achieve the same protective ends without gendered hierarchy?',
    'Comparative legal analysis across Muslim-majority jurisdictions: do states that grant women equal legal capacity, equal inheritance, and equal testimony show worse outcomes in property protection, dependent care, or household stability than those enforcing literal hierarchy? Economic and sociological evidence on causal mechanisms.',
    'If alternative arrangements achieve protective ends equivalently, the constraint''s extractiveness classification is confirmed: the hierarchy serves beneficiary interests (male authority, legal privilege) rather than functional necessity. If the hierarchy proves causally necessary for stated protective functions, part of the measured extraction becomes coordination cost rather than pure rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_guardianship_necessity_empirical, empirical, 'Whether male guardianship is functionally necessary or instrumentally chosen for authority preservation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) structural—legal barriers, economic dependency, family sanctions—or substantially internalized—Muslim women internalizing the hierarchical frame as legitimate, divinely ordained, and resistance-unthinkable?',
    'Post-exit suppression trajectory analysis: when women exit the constraint''s jurisdiction (migrate, adopt secular law, convert or apostasy), does suppression persist? Do women in secular legal jurisdictions report internalized acceptance of the hierarchy, or do suppressive patterns dissolve? Qualitative and quantitative evidence on identity fusion and theological internalization.',
    'If suppression is primarily structural, the constraint''s persistence depends on active enforcement and could shift if enforcement weakens. If substantially internalized, exit costs are carried with the agent—the constraint''s effective reach extends beyond its institutional scope, and classification may shift to snare (extraction defended by victim internalization). Identity-lock exits become identity-rending, not merely constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural barrier or internalized theological identity-fusion.').

omega_variable(
    kernel_reading_alternative_framings,
    'This story instantiates the literal_hierarchical reading of the quranic_gender_verses kernel. The contextual_egalitarian reading would reinterpret 4:11, 2:282, 4:34 as historically progressive steps within 7th-century Arabia, situating them under overarching maqasid (purposes: justice, protection of vulnerable persons) that yield egalitarian modern application. The progressive_abrogation reading holds that later Qur''anic principles (e.g., 49:13: no superiority except piety) supersede the earlier gender-specific rules via naskh (abrogation). Which reading—if any—is the correct transmission of divine intent?',
    'This is an irreducible hermeneutic question within Islamic theology. No empirical resolution exists—resolution is purely interpretive within the Islamic scholarly tradition. The question is not a deficiency but the point: the kernel (4:11, 2:282, 4:34 as revealed text) admits three structurally distinct constraint framings, yielding different beneficiary structures, extractiveness profiles, and victim sets. The corpus measures all three and lets their patterns diverge.',
    'This omega documents that THIS constraint (literal_hierarchical) and its sibling readings (contextual_egalitarian, progressive_abrogation) share a kernel but instantiate different structural constraints with different ε values and victim/beneficiary configurations. The corpus does not adjudicate which reading is theologically correct—it measures the structural consequences of each. This is a CONCEPTUAL omega; it is unresolvable by design, and that unresolvability IS the signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Irreducible hermeneutic pluralism: which reading (literal, contextual, progressive-abrogation) correctly transmits divine intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t3, quranic_gender_verses__literal_hierarchical, theater_ratio, 3, 0.24).
narrative_ontology:measurement_basis(qura_tr_t3, observed).
narrative_ontology:measurement(qura_tr_t6, quranic_gender_verses__literal_hierarchical, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(qura_tr_t6, observed).
narrative_ontology:measurement(qura_tr_t12, quranic_gender_verses__literal_hierarchical, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(qura_tr_t12, observed).
narrative_ontology:measurement(qura_tr_t18, quranic_gender_verses__literal_hierarchical, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(qura_tr_t18, observed).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__literal_hierarchical, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(qura_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t3, quranic_gender_verses__literal_hierarchical, base_extractiveness, 3, 0.79).
narrative_ontology:measurement_basis(qura_be_t3, observed).
narrative_ontology:measurement(qura_be_t6, quranic_gender_verses__literal_hierarchical, base_extractiveness, 6, 0.8).
narrative_ontology:measurement_basis(qura_be_t6, observed).
narrative_ontology:measurement(qura_be_t12, quranic_gender_verses__literal_hierarchical, base_extractiveness, 12, 0.81).
narrative_ontology:measurement_basis(qura_be_t12, observed).
narrative_ontology:measurement(qura_be_t18, quranic_gender_verses__literal_hierarchical, base_extractiveness, 18, 0.82).
narrative_ontology:measurement_basis(qura_be_t18, observed).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__literal_hierarchical, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(qura_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t3, quranic_gender_verses__literal_hierarchical, suppression_requirement, 3, 0.76).
narrative_ontology:measurement_basis(qura_su_t3, observed).
narrative_ontology:measurement(qura_su_t6, quranic_gender_verses__literal_hierarchical, suppression_requirement, 6, 0.77).
narrative_ontology:measurement_basis(qura_su_t6, observed).
narrative_ontology:measurement(qura_su_t12, quranic_gender_verses__literal_hierarchical, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(qura_su_t12, observed).
narrative_ontology:measurement(qura_su_t18, quranic_gender_verses__literal_hierarchical, suppression_requirement, 18, 0.79).
narrative_ontology:measurement_basis(qura_su_t18, observed).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__literal_hierarchical, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(qura_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, attachment_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This story is part of the quranic_gender_verses kernel family. The literal_hierarchical reading, contextual_egalitarian reading, and progressive_abrogation reading share the same scriptural source (Qur'an 4:11, 2:282, 4:34) but instantiate three structurally distinct constraints with different extractiveness profiles, victim/beneficiary structures, and classifications. The ε-invariance principle requires decomposition: a single Qur'anic reference cannot simultaneously yield high extractiveness (literal reading: hierarchy is divinely mandated, women's constrained capacity is extractive) and low extractiveness (contextual reading: verses are progressive steps, now reinterpreted under justice principles) and zero extractiveness (progressive reading: verses are abrogated by higher principles). These are three different constraints sharing a kernel. Each story links to its siblings via network.affects_constraints to establish the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
