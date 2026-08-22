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
 *   human_readable: Maqasid-Based Contextual-Egalitarian Reading of Qur'anic Gender Verses
 *   domain: Islamic Jurisprudence / Legal Hermeneutics / Gender Studies
 *
 * SUMMARY:
 *   This constraint captures one specific reading of the contested kernel
 *   over Qur'anic gender verses (4:11 inheritance, 2:282 testimony, 4:34
 *   guardianship): the contextual-egalitarian reading, which treats these
 *   verses as historically situated progressive steps for 7th-century Arabia
 *   rather than as timeless universal legal codes, and subordinates their
 *   literal application to the Qur'an's overarching maqasid (higher
 *   objectives, chiefly equity and justice). This reading is generated as its
 *   own clean constraint, independent of the literal-hierarchical and
 *   progressive-abrogation siblings, which are separate constraint stories in
 *   this kernel family. Under this reading, reformist scholars and
 *   rights-based NGOs gain interpretive authority and institutional standing;
 *   women pursuing equal inheritance and testimony claims exit the victim set
 *   of the literal reading and gain a textually-grounded claim to equal
 *   treatment; patriarchal religious elites and traditional qadi courts lose
 *   discretionary interpretive monopoly; and intra-community legitimacy
 *   conflict is the visible cost of the shift.
 *
 * KEY AGENTS:
 *   - reformist_scholars: agenda-setters who construct and disseminate the maqasid-based reading, gaining interpretive authority
 *   - womens_rights_ngos: organized beneficiaries who deploy the reading in advocacy and litigation
 *   - women_seeking_equal_inheritance_testimony: direct material beneficiaries where the reading is legally recognized
 *   - patriarchal_religious_elites: institutional payers who lose interpretive monopoly and some legitimacy
 *   - traditional_qadi_courts: institutional payers facing costly recodification and re-litigation pressure
 *   - conservative_male_heirs: individual payers facing reduced inheritance shares where the reading prevails
 *   - ordinary_believers: excluded lay population whose practice is shaped by whichever reading their local authority adopts
 *   - comparative_religion_scholars: analytical observers of the broader reinterpretation pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.42).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.38).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Maqasid-Based Contextual-Egalitarian Reading of Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "Islamic Jurisprudence / Legal Hermeneutics / Gender Studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '18835a5a-ae17-43c2-84ed-7fc5003a30ca').
narrative_ontology:cs_kernel_codification('18835a5a-ae17-43c2-84ed-7fc5003a30ca', fixed_text).
narrative_ontology:cs_authority_grounding('18835a5a-ae17-43c2-84ed-7fc5003a30ca', expertise).
narrative_ontology:cs_interpretation_layer_present('18835a5a-ae17-43c2-84ed-7fc5003a30ca').
narrative_ontology:cs_reading_relation('18835a5a-ae17-43c2-84ed-7fc5003a30ca', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('18835a5a-ae17-43c2-84ed-7fc5003a30ca', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('18835a5a-ae17-43c2-84ed-7fc5003a30ca', foundational, verses_are_historically_contextual_not_timeless_command).
narrative_ontology:cs_axiom_status(verses_are_historically_contextual_not_timeless_command, holdable).
narrative_ontology:cs_axiom_grounding('18835a5a-ae17-43c2-84ed-7fc5003a30ca', verses_are_historically_contextual_not_timeless_command, conventional).
narrative_ontology:cs_axiom('18835a5a-ae17-43c2-84ed-7fc5003a30ca', foundational, maqasid_equity_principle_governs_specific_legal_verses).
narrative_ontology:cs_axiom_status(maqasid_equity_principle_governs_specific_legal_verses, holdable).
narrative_ontology:cs_axiom_grounding('18835a5a-ae17-43c2-84ed-7fc5003a30ca', maqasid_equity_principle_governs_specific_legal_verses, instrumental).
narrative_ontology:cs_created_at('18835a5a-ae17-43c2-84ed-7fc5003a30ca', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_inheritance_testimony).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_religious_elites).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_qadi_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, conservative_male_heirs).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_al_sharia_as_overarching_equity_principle).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, historical_contextualization_hermeneutic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and disseminate the maqasid-based reinterpretation, positioning historical-context verses (4:11, 2:282, 4:34) as time-bound applications of a deeper equity principle rather than fixed rules. Gain interpretive authority, academic standing, and institutional platforms (universities, reform councils, transnational fiqh forums) previously monopolized by traditional jurists. Can publish, teach, and litigate this reading across jurisdictions with relative freedom of movement between supportive institutions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, reformist_scholars, beneficiary).

% Cite the contextual-egalitarian reading in advocacy for equal inheritance and testimony law reform, using it to argue that gender-differentiated rules are pedagogical scaffolding rather than eternal command. Receive donor funding and legal standing tied to this hermeneutic's credibility; can shift funding and campaign focus across countries as political openings appear or close.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos, beneficiary,
    organized, generational, mobile, global).

% Gain a textually grounded religious argument for equal inheritance shares and testimony weight where family or state law is negotiable. Exit this reading's protection is constrained by which courts, muftis, or family councils recognize maqasid-based reasoning as legitimate — in jurisdictions dominated by literalist courts, the benefit is nominal rather than enforceable.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_inheritance_testimony, beneficiary,
    moderate, biographical, constrained, national).

% Lose the discretionary authority to declare the literal gender-differentiated rulings as settled, uncontestable divine law. Their institutional legitimacy rested partly on being the sole gatekeepers of correct interpretation; the contextual reading opens interpretive competition they cannot fully suppress without appearing to reject reform outright, which costs them standing with younger or diaspora constituencies.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_religious_elites, payer,
    institutional, generational, constrained, national).

% Apply codified personal-status law built on the literal readings (fixed male-double inheritance shares, testimony-weighting rules). A maqasid-based reinterpretation, if adopted into statute or accepted as valid legal precedent, forces costly recodification, retraining of judges, and re-litigation of settled precedent. They cannot simply exit the debate because their rulings' legitimacy depends on being seen as faithful to correctly interpreted revelation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_qadi_courts, payer,
    institutional, generational, trapped, national).

% Stand to receive a smaller inheritance share, or have testimony given equal evidentiary weight to their own, if courts or family arbitration adopt the contextual-egalitarian reading. Their material interest is directly tied to the literal reading remaining dominant in the jurisdiction where their estate or dispute is adjudicated; they have little individual power to resist a shift in the interpretive consensus.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, conservative_male_heirs, payer,
    moderate, biographical, constrained, national).

% Most lay Muslims encounter this dispute only through sermons, family elders, or media summaries, without access to the classical Arabic philological and usul al-fiqh training needed to independently evaluate either reading's textual claims. Their lived practice is shaped by whichever authority (mosque, family, state) prevails locally, but their own view is rarely solicited by either camp.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, ordinary_believers, excluded,
    powerless, biographical, constrained, local).

% Study the hermeneutic dispute as a case of scriptural reinterpretation under social change pressure, drawing parallels to similar reform movements in other textual traditions. Take testimony and texts from all camps without a stake in which reading prevails within the community itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textually and methodologically grounded framework (maqasid al-sharia) for reconciling specific 7th-century legal verses with contemporary claims of gender equity, allowing believing communities to update personal-status practice without declaring the Qur'an itself mistaken or abandoning scriptural authority altogether.
% TRANSFER_FUNCTION: Moves interpretive authority from traditional male jurist gatekeepers toward reformist scholars and rights advocates; moves material claims (inheritance shares, evidentiary weight) from conservative male heirs and parties favored by literal readings toward women asserting equal claims, wherever this reading is accepted by the adjudicating authority.
% ABSENT_VOICES: Ordinary lay believers, especially women without access to either classical training or NGO advocacy networks, are largely absent from the scholarly and legal debate that determines which reading governs their actual inheritance or testimony outcomes; their preferences are inferred by both camps rather than solicited directly.
% DISAPPEARANCE_RATIONALE: If this specific reading disappeared, reformist legal victories built on maqasid reasoning (e.g., statutory equal-inheritance provisions in some jurisdictions, testimony-weight reforms) would face renewed legal and theological challenge, and NGOs would lose a key scripturally-grounded advocacy tool — a real rearrangement for those relying on it. But traditional courts and patriarchal elites would say the underlying religious obligation was never actually altered by a reading they regard as illegitimate, so from their side nothing changes. The verdict is genuinely disputed between the parties, not merely a difference of tone.
% FOUNDING_PROBLEM: The founding problem this reading addresses is the tension between static verses regulating inheritance, testimony, and marital authority (revealed in a specific 7th-century Arabian social context) and a modern moral and legal environment in which gender-differentiated legal treatment is widely regarded as inequitable; the reading offers a way to retain scriptural fidelity while updating practice.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars (Amina Wadud, Khaled Abou El Fadl, and affiliated fiqh councils) attest the founding problem is live and the maqasid framework is the correct resolution. Independent corroboration from outside the immediate beneficiary set comes from comparative religion scholars and legal historians studying analogous reform movements in Jewish and Christian legal traditions, who document that historically-situated reinterpretation is a recurring and structurally similar response to changing social norms across scriptural traditions — though these scholars take no position on whether THIS particular reinterpretation is theologically correct. Traditional jurists dispute that the founding problem exists at all, holding the verses were never merely contextual.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, contested).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.42 at interval end) because this reading does redistribute real material and interpretive resources — inheritance shares, testimony weight, gatekeeping authority — away from parties favored by the literal reading, but it does so through argument, scholarship, and legal advocacy rather than coercive seizure; the 'extraction' is a contested redistribution, not predation. Suppression is moderate-low and DECLINING over the interval (0.50 → 0.38): as maqasid-based reasoning gains broader scholarly acceptance and institutional footholds (reform fiqh councils, some national courts), the reading requires less active argumentative and institutional effort to sustain itself against dismissal as illegitimate — early on it needed vigorous defense against charges of theological innovation (bid'a); over time it has become an established, if still contested, position within Islamic legal scholarship. Theater ratio is low and rising slowly (0.15 → 0.28), reflecting some performative citation of maqasid language in venues that do not substantively change legal outcomes (symbolic gestures toward reform without corresponding statutory change), a pattern that grows modestly as the reading's vocabulary is absorbed into official rhetoric faster than into codified law.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and NGOs sit near the beneficiary end: they gain interpretive authority, institutional platforms, and advocacy tools with mobile exit options across supportive jurisdictions. Women seeking equal inheritance/testimony are moderate beneficiaries whose actual gain is bounded by their constrained exit — they cannot simply choose a friendlier jurisdiction for their own inheritance dispute. Patriarchal elites and traditional courts sit toward the target end: they bear a real loss of discretionary authority and institutional legitimacy, though their institutional power gives them some capacity to resist (hence 'payer' rather than pure 'victim' framing, and constrained rather than trapped exit for the elites specifically). Conservative male heirs are the clearest direct material payers, with the least power to resist a jurisdiction-level interpretive shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scripturally-grounded gender-differentiated legal treatment with a contemporary equity commitment — is genuinely contested rather than resolved: reformist scholars hold it is being actively and appropriately addressed; traditionalists hold no such problem exists because the verses were never merely time-bound. Classifying this as tangled_rope (rather than pure rope or pure snare) prevents two mislabeling errors: treating the reading as costless pure coordination (ignoring that real institutional and material losses accrue to identifiable parties) and treating it as pure extraction with no genuine coordination function (ignoring that it does solve a real reconciliation problem for believers who want both scriptural fidelity and contemporary equity, and does so through open scholarly argument rather than concealment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_legitimacy_authority,
    'Does interpretive authority to override literal verse meaning via maqasid properly rest with credentialed usul al-fiqh scholars, with the broader Muslim community''s evolving moral consensus, or with no single legitimate authority at all — making this an irreducibly contested hermeneutic claim rather than a resolvable jurisprudential question?',
    'No empirical resolution mechanism exists internal to the tradition; this is a live question of religious epistemology and communal authority that different Islamic legal schools answer differently. External resolution would require either broad ijma (consensus) that has not emerged, or acceptance that the question remains permanently open (ikhtilaf).',
    'If maqasid-based reinterpretation authority is broadly recognized as legitimate, this reading''s classification moves toward rope (genuine, accepted coordination mechanism updating practice); if rejected as illegitimate innovation by the wider scholarly consensus, it functions more as a snare from the traditionalist seat — an unauthorized redistribution dressed as scholarship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_legitimacy_authority, conceptual, 'Who holds legitimate authority to reinterpret specific verses via maqasid is itself unsettled.').

omega_variable(
    kernel_reading_coexistence_or_foreclosure,
    'Can the contextual-egalitarian reading and the literal-hierarchical reading genuinely coexist as live options within a single legal system (e.g., through legal pluralism or judicial discretion), or does adopting one as binding law necessarily foreclose the other for that jurisdiction''s population?',
    'Comparative study of jurisdictions that have formally codified maqasid-based family law reform (e.g., Morocco''s Moudawana, Tunisia''s Code of Personal Status) versus those retaining literal codification, tracking whether both readings persist as live minority positions or whether codification effectively forecloses the alternative for affected populations.',
    'If codification forecloses the sibling reading in practice (even while both remain theologically arguable), the reading_relations declared as coexists_with may understate the practical displacement this reading produces once translated into binding statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_or_foreclosure, empirical, 'Whether legal codification of this reading practically forecloses, rather than merely coexists with, the literal reading.').

omega_variable(
    extraction_referent_ambiguity,
    'Is the extractiveness authored here (0.42) measuring extraction FROM the literal reading''s beneficiaries (patriarchal elites, favored male heirs) as this reading''s own advocates would describe it, or should it instead be read as measuring extraction from the STANDING arrangement (literal reading as currently codified in most jurisdictions) as this reading''s proponents experience resistance to it?',
    'Per the ε-referent rule for kernel readings, ε is authored for the standing arrangement under contest, by this reading''s own lights — clarify in future revisions whether ''standing arrangement'' should be read as the literal-reading status quo ante or as this reading''s own emerging practice being resisted.',
    'Does not change the classification but affects how the extractiveness figure should be narratively glossed in cross-reading comparison.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'Clarifying which arrangement ε is measured against, consistent with the kernel-reading ε-referent rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__contextual_egalitarian, theater_ratio, 8, 0.18).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__contextual_egalitarian, theater_ratio, 16, 0.21).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__contextual_egalitarian, theater_ratio, 24, 0.24).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__contextual_egalitarian, theater_ratio, 32, 0.26).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.1).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quranic_gender_verses kernel, each authored as an independent, ε-invariant constraint story per the ε-invariance principle. contextual_egalitarian (this file) treats the verses as time-bound progressive steps subordinate to maqasid; literal_hierarchical treats them as timeless direct ordinance; progressive_abrogation treats later universalist verses as superseding the earlier ones via naskh. The three share the same textual kernel but differ in ε, beneficiary/victim structure, and claimed type, reflecting genuinely distinct structural claims about interpretive authority and legal consequence, not merely differing observables on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
