% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Qur'anic Gender Verses: Contextual-Egalitarian Reading
 *   domain: legal/religious/gender
 *
 * SUMMARY:
 *   This constraint is one reading of a contested Islamic hermeneutical
 *   kernel: the gender verses of the Qur'an (primarily 4:11 on inheritance,
 *   2:282 on witnesses, 4:34 on marital guardianship, and 49:13 on universal
 *   dignity). The contextual-egalitarian reading holds that these verses
 *   represent historically situated legal responses to 7th-century Arabian
 *   conditions—not timeless, universal ordinances—and must be reinterpreted
 *   through the overarching Islamic equity principles (maqasid al-shariah:
 *   protection of life, intellect, lineage, property, and dignity). Under
 *   this reading, women exit the structural victim position assigned by the
 *   literal hierarchical interpretation and become claimants with scriptural
 *   warrant for equal inheritance, testimony, and freedom from patriarchal
 *   guardianship. Reformist scholars and rights-based organizations gain
 *   interpretive authority; patriarchal elites and traditional court systems
 *   lose their institutional monopoly on Islamic legal interpretation. The
 *   constraint is tangled_rope because it coordinates a unified hermeneutical
 *   framework (beneficiaries: reformist scholars, rights-based NGOs, and
 *   women claimants all gain from a single interpretive system) while
 *   simultaneously extracting from patriarchal elites and traditional courts
 *   by stripping them of interpretive authority. The enforcement is active
 *   because the literal reading retains institutional power in many
 *   Muslim-majority states, and defending the contextual-egalitarian reading
 *   requires sustained scholarly and legal work.
 *
 * KEY AGENTS:
 *   - Reformist scholars (institutional power, mobile exit): Tariq Ramadan, Khaled Abou El Fadl, Asma Barlas, and emerging academic cohorts trained in modern hermeneutics, gender studies, and Islamic jurisprudence. They gain authority and platform from the reading's rise.
 *   - Women claimants (moderate power, constrained exit): Inherit equally, testify equally, claim freedom from forced guardianship. They transition from victim to beneficiary under this reading but bear intra-community costs and remain constrained by family/legal system embeddedness.
 *   - Patriarchal elites (powerful, constrained exit): Male family heads, judges, and traditional scholars who lose interpretive monopoly. They are trapped because rejecting the literal reading means abandoning their professional/familial authority, yet defending it becomes increasingly untenable against maqasid-based argumentation.
 *   - Traditional courts (institutional, trapped exit): Qadi systems and family law courts that must adjudicate under the literal reading. They bear the cost of defending increasingly indefensible positions and are trapped in legal systems built on the literal interpretation.
 *   - Rights-based NGOs (organized, mobile exit): International and regional women's rights organizations that adopt this reading as the Qur'anically grounded basis for equality advocacy. They benefit by gaining Islamic legitimacy and establishing themselves as authoritative interpreters.
 *   - Conservative communities (excluded): Numerically and institutionally powerful but excluded from this reading's hermeneutical space. They would object loudly if seated, but the reading progressively marginalizes them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.38).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.52).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.38).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Qur'anic Gender Verses: Contextual-Egalitarian Reading").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "legal/religious/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '2b50b510-efb7-4dce-af8a-09b447a5388b').
narrative_ontology:cs_kernel_codification('2b50b510-efb7-4dce-af8a-09b447a5388b', fixed_text).
narrative_ontology:cs_authority_grounding('2b50b510-efb7-4dce-af8a-09b447a5388b', lineage).
narrative_ontology:cs_interpretation_layer_present('2b50b510-efb7-4dce-af8a-09b447a5388b').
narrative_ontology:cs_reading_relation('2b50b510-efb7-4dce-af8a-09b447a5388b', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('2b50b510-efb7-4dce-af8a-09b447a5388b', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('2b50b510-efb7-4dce-af8a-09b447a5388b', foundational, verses_historically_contextual).
narrative_ontology:cs_axiom_status(verses_historically_contextual, holdable).
narrative_ontology:cs_axiom_grounding('2b50b510-efb7-4dce-af8a-09b447a5388b', verses_historically_contextual, empirically_contingent).
narrative_ontology:cs_axiom('2b50b510-efb7-4dce-af8a-09b447a5388b', foundational, maqasid_override_textual_particulars).
narrative_ontology:cs_axiom_status(maqasid_override_textual_particulars, holdable).
narrative_ontology:cs_axiom_grounding('2b50b510-efb7-4dce-af8a-09b447a5388b', maqasid_override_textual_particulars, deontological).
narrative_ontology:cs_reference_frame('2b50b510-efb7-4dce-af8a-09b447a5388b', quranic_equity_framework).
narrative_ontology:cs_drift_state('2b50b510-efb7-4dce-af8a-09b447a5388b', contemporary_gender_equality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b50b510-efb7-4dce-af8a-09b447a5388b', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_claimants).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_elites).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_court_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, women_claimants).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, quranic_equity_principles_maqasid).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, universal_human_dignity_49_13).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in modern hermeneutics, Islamic law, gender studies, and linguistics who reinterpret gender verses through historical context and overarching maqasid principles. They produce scholarly commentary, train new generations, establish interpretive authority in universities and legal reform movements. Their reading gains legitimacy from Qur'anic principles (equity, justice, human dignity) and historical understanding of 7th-century Arabia. They face institutional resistance from traditional establishments but gain platform and funding from progressive Muslim communities and international rights organizations.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    institutional, generational, mobile, global).

% Women who invoke this reading to claim equal inheritance shares, equal testimony weight in court, and freedom from guardianship requirements. Under this reading, they move from the victim set of literal hierarchical interpretations to claimants with scriptural warrant for equality. They bear the cost of intra-community conflict, potential social ostracism, and the burden of constant re-argumentation in communities where the literal reading dominates. Their exit from patriarchal interpretation is constrained by family ties, community embeddedness, and legal systems still organized under the literal reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_claimants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, women_claimants, payer).

% Religious scholars, judges, and male family heads who hold power under the literal hierarchical reading. This reading strips them of their interpretive monopoly and the scriptural warrant for male guardianship and differentiated inheritance. They bear the cost of lost institutional authority, diminished control over family property and decision-making, and erosion of their position as sole interpreters of Islamic law. Their response—asserting the literal reading is eternal and inviolable—becomes increasingly difficult to defend against maqasid-based argumentation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_elites, payer,
    powerful, generational, constrained, national).

% Qadis and court systems that adjudicate family law under the literal reading. They must defend differentiated inheritance, guardianship rules, and lower testimony weight for women against claims grounded in this reading. The constraint on them is the burden of defending a reading that becomes increasingly untenable in the face of maqasid-based legal reasoning. They are trapped because abandoning the literal reading requires structural reform of entire court systems and contradicts their claimed Islamic legitimacy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_court_operators, payer,
    institutional, generational, trapped, national).

% International and regional organizations dedicated to women's rights who adopt this reading as the Qur'anically grounded alternative to patriarchal interpretation. They fund scholarship, produce materials, train advocates, and amplify reformist voices in policy contexts. They benefit by gaining Islamic legitimacy for gender equality claims (avoiding the charge that rights-based advocacy is Western imperialism) and by establishing themselves as authoritative interpreters in global governance. They have high exit optionality—they operate across jurisdictions and can shift focus or strategy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, generational, mobile, global).

% Scholars trained in classical Islamic jurisprudence who maintain the literal hierarchical reading. They are excluded from this narrative not because they have nothing to say but because this reading's institutional rise (reformist scholars gaining authority) displaces them from the center of Islamic legal interpretation. Their identity is fused with the classical interpretive tradition; rejecting it means rejecting their entire professional formation. They would object loudly if they were seated at the table, but the reading's logic progressively marginalizes them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_scholars, excluded,
    institutional, generational, identity_locked, national).

% UN mechanisms, regional human rights commissions, and treaty bodies that evaluate gender equality compliance. They observe the contest over Islamic interpretation and recognize the contextual-egalitarian reading as support for legal reforms aligning Islamic jurisprudence with international equality standards. They have no direct stake in the outcome but exert influence through conditional aid, reputational pressure, and forum for rights-based claims.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of Islamic sacred texts in a way that reconciles scriptural authority with gender equality principles. Solves the problem: how can Muslims honor the Qur'an's comprehensive applicability while acknowledging that 7th-century Arabia's patriarchal norms do not bind 21st-century law? Creates a unified hermeneutical framework (maqasid-based) that modern Muslim communities can adopt without claiming the Qur'an itself is false or outdated.
% TRANSFER_FUNCTION: Redistributes interpretive authority from traditional court-based scholars and judges to reformist academics, rights-based organizations, and women's movement intellectuals. Moves women from the structural position of legal subordinates (under literal reading) to legal claimants with scriptural warrant for equality. Transfers decision-making power from patriarchal family heads and qadis to reformed legal systems and egalitarian community frameworks. The 'extraction' here is the cost borne by patriarchal elites and traditional courts who lose monopoly authority over Islamic law.
% ABSENT_VOICES: Conservative and traditionalist communities who interpret the Qur'an literally and view contextual reinterpretation as dilution of divine law. Scholars in classical Islamic jurisprudence traditions who were trained to read these verses as eternal and unchanging. Women in conservative communities who have internalized the literal reading and do not perceive themselves as victimized by differentiated rights. These voices are structurally excluded from the reformist interpretive space, though they remain numerically and institutionally powerful in many Muslim-majority states.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight—if the maqasid-based hermeneutical framework lost all institutional support and the literal hierarchical reading reclaimed hegemony—legal and family systems in Muslim-majority states would revert to explicit guardianship rules, differentiated inheritance, and reduced female testimony. Women's property rights, inheritance claims, and legal standing in family courts would contract. The institutional shift would be severe; the reading's existence sustains a possibility space for reform that its disappearance would collapse.
% FOUNDING_PROBLEM: The founding problem is the 7th-century contextuality of gender verses: how can Muslims in radically different historical and technological conditions live under rules formulated for tribal Arabian society without claiming the Qur'an itself is outdated or false? The contextual-egalitarian reading answers: by distinguishing the underlying equity principle (maqasid—justice, protection of rights, prevention of harm) from its 7th-century instantiation, the reading reconciles scriptural authority with historical change.
% FOUNDING_PROBLEM_CORROBORATION: Reformist Islamic scholars (Tariq Ramadan, Jamal al-Banna, Khaled Abou El Fadl, Asma Barlas) attest the founding problem is live: Muslim societies face sustained pressure to reconcile scriptural authority with modern legal systems, and the literal reading creates indefensible gender hierarchies in contemporary law. Women's rights organizations and secular legal scholars attest that patriarchal family law in Muslim-majority states creates documented harms (forced marriage, unequal inheritance, reduced legal capacity). International human rights bodies corroborate that the problem is live—countries cite 'Islamic law' as justification for differentiated gender rights, and reform advocates point to alternative readings as the path to compatibility with equality standards. No part of the corroboration comes exclusively from the reading's own beneficiaries; the problem statement is cross-verified across multiple independent seats.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects moderate asymmetry: the reading genuinely solves a coordination problem (how to interpret sacred text across historical change) and produces benefits for beneficiaries (authority, rights claims). However, these benefits accrue asymmetrically—reform intellectuals and rights-based organizations gain platform and funding; women gain legal claims but also bear social costs; patriarchal elites lose authority. The extractiveness is not high because the reading does not create a zero-sum system; traditional courts and conservative scholars retain institutional power in many jurisdictions, and the constraint's operation remains contested. Suppression (0.52) reflects the active enforcement required to sustain this reading: classical jurisprudential reinterpretation must be continuously defended against literal reading adherents, women's equality claims must be reasserted in family law proceedings, and reformist authority must be maintained in educational and legal institutions. Theater (0.28) is moderate-low because the maqasid-based framework is genuinely coherent—it is not primarily performative—but as the reading gains institutional power, some activity becomes theatrical: universities and NGOs may invoke maqasid rhetoric without substantially shifting resource allocation or decision-making authority. Accessibility collapse (0.41) is relatively low because alternatives to this reading remain institutionally viable in many contexts; the literal reading has not become inaccessible. Resistance (0.72) is high because powerful constituencies (patriarchal elites, traditional courts, conservative communities) actively resist the reading despite its growing institutional foothold. The measurement series shows extractiveness and suppression rising steeply from t=0 to t=15 (as the reading gains institutional space and faces resistance) then plateauing from t=15 onward (as the reading reaches an equilibrium where it is institutionally established in academic and rights-based spaces but remains contested in traditional courts and conservative communities).
 *
 * PERSPECTIVAL GAP:
 *   The contextual-egalitarian reading should compute differently depending on the seat. From the reformist scholar's seat, the arrangement is genuine coordination it builds and sustains through scholarship, teaching, and institutional development—the extraction cost is borne by others, not by them. From the woman claimant's seat, the reading provides access to scriptural resources for equality claims, but she also bears intra-community costs (family rupture, social ostracism, legal vulnerability in mixed-jurisdiction contexts) that reduce her net benefit. From the patriarchal elite's seat, the reading appears as pure extraction: their authority is stripped, their position defended, their institutional power diminished. From the traditional court's seat, the reading imposes compliance costs without corresponding benefit—they must defend an increasingly difficult position or abandon institutional legitimacy. The engine computes these divergent directionalities from the structural data: beneficiaries (reformist scholars, rights-based NGOs) sit near d=0.0 (full subsidy); payers (patriarchal elites, traditional courts) sit near d=1.0 (full target); women claimants sit near d=0.5 (they benefit and pay). This divergence is expected and captures the true structural dynamics of a constraint that coordinates interpretation while extracting from traditional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (reformist scholars, rights-based NGOs): These agents benefit from the reading's rise through increased interpretive authority, institutional platform, international recognition, and funding flows. They have mobile exit options (can pursue careers in different contexts if the reading loses power) and are not structurally trapped. Their directionality is low (near 0.0), indicating they are subsidized by the constraint. Women claimants: They gain scriptural warrant for equality claims and legal standing, but they also bear costs (social pressure, family rupture, vulnerability in still-patriarchal contexts). They have constrained exit (embedded in communities, families, legal systems). Their directionality is near symmetric (0.5) because benefits and costs are roughly balanced, though the balance varies by jurisdiction and family structure. Patriarchal elites and traditional courts: They lose interpretive authority, decision-making power, and institutional legitimacy. They have constrained exit (trapped in positions that depend on the literal reading). Their directionality is high (near 1.0), indicating they are targeted for extraction. The extraction is not money but authority, decision-making power, and institutional recognition—precisely what they lose as the contextual-egalitarian reading gains ground. The directionality override for powerful agents (d=0.85) reflects that patriarchal elites, despite their nominal institutional power, are being extracted from in the specific structural relationship of this constraint: their power does not insulate them from the reading's authority-stripping effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: Muslim societies face sustained pressure to reconcile scriptural authority with historical change and gender equality standards. The contextual-egalitarian reading directly addresses this problem by providing a framework for historical reinterpretation grounded in Islamic principles. The constraint is not mandatrophic in the sense that its original function (solving the interpretation problem) remains vital and the reading continues to perform this function. However, the reading is institutionally contested: it is dominant in academic circles and international rights organizations but remains marginal or absent in many traditional courts and conservative communities. This means the constraint's effectiveness varies by jurisdiction and community, and in some contexts, it may be approaching mandatrophy if institutional adoption is stalling. The reading's persistence depends on continued scholarly work and institutional maintenance (universities, training programs, legal advocacy) rather than on passive acceptance or natural equilibrium. The measured suppression (0.52) reflects this: the reading must be actively defended against the literal reading's institutional power, which suggests it is not yet dominant enough to be self-sustaining. Mandatrophy is not yet present, but the reading's evolution from reformist margin to institutional center is not inevitable; it requires sustained coordination among beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authority_divergence,
    'Which reading (contextual-egalitarian vs. literal-hierarchical vs. progressive-abrogation) holds authority within a single Islamic jurisprudential framework?',
    'Cross-analysis of foundational Islamic legal principles (usul al-fiqh) to determine which reading survives rigorous scrutiny within classical methodology; examination of which reading is endorsed by the plurality of contemporary Islamic legal scholars and reform movements.',
    'If the contextual-egalitarian reading can be shown to rest on stronger classical foundations (e.g., maqasid-based reasoning predates and supersedes literal textualism in Islamic jurisprudence), its authority claim is strengthened. If the literal reading can be shown to be the only defensible classical reading, this reading''s authority becomes primarily contemporary and reformist rather than rooted in classical tradition. This determines whether the reading is a recovery of classical principle or a modern innovation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_authority_divergence, conceptual, 'Whether maqasid-based hermeneutics is a recovery of classical Islamic jurisprudence or a modern reinterpretation—affects the reading''s claim to be internally Islamic rather than externally imposed.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.52) predominantly structural (enforcement machinery actively defending the literal reading) or internalized (women and reform-minded communities have internalized patriarchal interpretations and resist the egalitarian reading)?',
    'Post-legal-reform observation: if suppression drops when the literal reading loses institutional enforcement (e.g., family law reform adopts egalitarian interpretation), suppression was predominantly structural. If suppression persists (communities continue to resist despite legal change), suppression has internalized components and may be partially identity-locked or culturally embedded.',
    'If structural: the constraint''s persistence depends on maintaining enforcement machinery; legal reform can shift the interpretation equilibrium. If internalized: women and communities may continue to invoke the literal reading even when institutional pressure is removed, and the constraint''s effective suppression is higher than structural measures suggest. This affects the reform strategy''s probability of success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of the egalitarian reading is externally enforced or internally adopted.').

omega_variable(
    kernel_committer_framing_ambiguity,
    'Is the contested kernel ''gender verses in the Qur''an'' a single kernel with multiple readings, or do the three readings instantiate fundamentally different kernels (different referents, different texts being interpreted)?',
    'Structural analysis: do all three readings claim to be interpretations of the same scriptural text (4:11, 2:282, 4:34, 49:13)? Do they dispute how to read the same text, or do they disagree about which text is authoritative? If the literal reading treats classical jurisprudential elaboration as part of the kernel while the contextual reading treats only the Qur''an itself as kernel, they may be reading different objects.',
    'If a single kernel: the readings are genuinely alternative interpretations and the engine''s frame applies cleanly. If multiple kernels: the appearance of disagreement masks structural kernel-switching, and the ''contest'' is not resolvable within a single framework because the parties are not interpreting the same thing. This affects whether reform advocacy can persuade within Islamic jurisprudence or whether it is advocating for a different jurisprudence entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_framing_ambiguity, conceptual, 'Whether all three readings share a single kernel or instantiate different kernels masked by terminological unity.').

omega_variable(
    women_beneficiary_vs_payer_asymmetry,
    'Women are authored with role=beneficiary + secondary_role=payer. Is this dual role stable, or do women exit the payer position as the reading gains institutional ground?',
    'Temporal observation: track women''s social costs (ostracism, family rupture, legal vulnerability) in jurisdictions where the contextual-egalitarian reading gains institutional authority. If costs decline, women''s dual role shifts toward pure beneficiary; if costs persist or rise (due to backlash), the dual role remains stable and may intensify.',
    'If women''s payer status declines: the reading''s success is marked by women''s exit from costs. If payer status persists: the reading''s victory is incomplete—women gain rights claims but remain socially pressured, suggesting the constraint is not fully resolved and may remain tangled_rope indefinitely. This affects the reading''s potential to achieve snare→rope transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(women_beneficiary_vs_payer_asymmetry, empirical, 'Whether women''s transition from victim to beneficiary is complete or partial as institutional adoption progresses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t5, quranic_gender_verses__contextual_egalitarian, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__contextual_egalitarian, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quranic_gender_verses__contextual_egalitarian, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__contextual_egalitarian, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t25, quranic_gender_verses__contextual_egalitarian, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(qura_tr_t25, observed).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__contextual_egalitarian, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(qura_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t5, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t25, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(qura_be_t25, observed).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(qura_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t5, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t25, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(qura_su_t25, observed).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(qura_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel decomposes into three constraint stories, each representing a different reading with distinct beneficiary/victim structures and extracted authority relations. The contextual-egalitarian reading is one of these three; the other readings (literal_hierarchical, progressive_abrogation) are separate constraint stories linked by this network.affects_constraints edge. All three readings interpret the same scriptural text (Qur'an 4:11, 2:282, 4:34, 49:13) but derive different legal implications and different distributions of interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
