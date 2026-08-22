% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid Obligation (Classical Madhhab Authority)
 *   domain: religious_legal_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the traditionalist_taqlid reading of
 *   the quran_hadith_substrate kernel. The reading asserts that classical
 *   fiqh schools (madhahib) embody an authoritative consensus (ijma) binding
 *   on all subsequent generations, and that contemporary Muslims are
 *   religiously obligated to follow established madhhab rulings through
 *   taqlid (deference to scholarly authority) rather than independent
 *   reasoning (ijtihad). This reading operates as the default legal theology
 *   in traditionalist-dominant contexts (e.g., Saudi Arabia's Hanbali
 *   establishment, Al-Azhar's institutional stance, Deobandi networks,
 *   Twelver Shi'i marja'iyya) and informs personal status codes across the
 *   Muslim world. The constraint coordinates communal legal identity but
 *   extracts compliance from women, minorities, and reformist voices through
 *   institutionalized enforcement — state courts, fatwa bureaucracies, social
 *   policing, and the structural exclusion of alternative hermeneutics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.72).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.78).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid Obligation (Classical Madhhab Authority)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious_legal_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'cb595392-f483-4862-b50d-b1e18765ea57').
narrative_ontology:cs_kernel_codification('cb595392-f483-4862-b50d-b1e18765ea57', fixed_text).
narrative_ontology:cs_authority_grounding('cb595392-f483-4862-b50d-b1e18765ea57', lineage).
narrative_ontology:cs_interpretation_layer_present('cb595392-f483-4862-b50d-b1e18765ea57').
narrative_ontology:cs_reading_relation('cb595392-f483-4862-b50d-b1e18765ea57', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('cb595392-f483-4862-b50d-b1e18765ea57', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('cb595392-f483-4862-b50d-b1e18765ea57', foundational, classical_ijma_binding_on_all_generations).
narrative_ontology:cs_axiom_status(classical_ijma_binding_on_all_generations, holdable).
narrative_ontology:cs_axiom_grounding('cb595392-f483-4862-b50d-b1e18765ea57', classical_ijma_binding_on_all_generations, deontological).
narrative_ontology:cs_axiom('cb595392-f483-4862-b50d-b1e18765ea57', foundational, taqlid_as_religious_obligation_for_non_mujtahids).
narrative_ontology:cs_axiom_status(taqlid_as_religious_obligation_for_non_mujtahids, holdable).
narrative_ontology:cs_axiom_grounding('cb595392-f483-4862-b50d-b1e18765ea57', taqlid_as_religious_obligation_for_non_mujtahids, deontological).
narrative_ontology:cs_axiom('cb595392-f483-4862-b50d-b1e18765ea57', secondary, madhhab_preservation_as_communal_obligation).
narrative_ontology:cs_axiom_status(madhhab_preservation_as_communal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cb595392-f483-4862-b50d-b1e18765ea57', madhhab_preservation_as_communal_obligation, conventional).
narrative_ontology:cs_reference_frame('cb595392-f483-4862-b50d-b1e18765ea57', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('cb595392-f483-4862-b50d-b1e18765ea57', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb595392-f483-4862-b50d-b1e18765ea57', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_councils).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, religious_endowment_administrators).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, classical_ijma_authority).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, taqlid_as_obligation).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, madhhab_infallibility_in_core).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain curricula, fatwa bodies, and judicial appointments across madhhab-affiliated seminaries and courts. Authorize who speaks for the tradition. Collect social authority, state recognition, and material resources (endowments, state salaries) from administering the taqlid framework. Can redirect institutional resources to defend the arrangement.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Formal bodies (e.g., Al-Azhar, Deoband, Qom seminaries) that certify scholars, issue collective fatwas, and define the boundaries of acceptable interpretation. Their institutional identity is fused with the madhhab system — exit would dissolve their raison d'etre. Benefit from gatekeeping authority and state patronage.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_councils, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, madhhab_councils, agenda_setter).

% Local enforcement layer: appoint imams, control Friday sermon content, manage community dispute resolution through sharia councils. Derive legitimacy and funding from representing 'authentic' tradition. Exit means losing community trust and institutional position.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, biographical, constrained, regional).

% Manage waqf properties whose deeds specify madhhab affiliation. Material interest in preserving the legal framework that validates their administration. Exit would require legal restructuring of endowments — costly and politically contested.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_endowment_administrators, beneficiary,
    moderate, generational, constrained, national).

% Seek to practice Islam in ways that align with contemporary ethics (gender equality, LGBTQ+ inclusion, religious freedom). Bear costs: social ostracism, loss of community, accusations of apostasy, legal penalties in some jurisdictions. Exit options: leave the community (high identity cost), migrate (material cost), or conform (epistemic cost).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Subject to classical fiqh rulings on marriage, divorce, inheritance, testimony, and custody that systematically disadvantage women. In many jurisdictions these rulings are codified in personal status law with no opt-out. Exit is legally blocked (cannot choose secular courts for family matters) and socially punished.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status, payer,
    powerless, biographical, trapped, national).

% Governed by classical dhimmi rules (jizya, dress codes, building restrictions, testimony inequality) where traditionalist taqlid informs state law. No recognized exit from the legal category; conversion is the only formal exit but carries death penalty in some jurisdictions. Bear extraction without representation in the authority structure.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, generational, trapped, national).

% Scholars advocating contextual ijtihad, maslaha-based reasoning, or Quran-centric hermeneutics. Structurally excluded from official fatwa bodies, seminary appointments, and state religious institutions. Face professional blacklisting, fatwas of deviation, travel bans. Their exclusion is the enforcement mechanism that maintains taqlid's monopoly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, payer).

% Constitutional courts, human rights bodies, international legal scholars analyzing the conflict between classical fiqh codifications and constitutional equality guarantees. See the full structure: how taqlid operates as both coordination (communal cohesion) and extraction (gender/minority subordination).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, secular_legal_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative legal framework for Muslim communities across time and space: resolves interpretive disputes through recognized schools, ensures communal cohesion via shared ritual and transactional norms, preserves textual continuity with the formative period.
% TRANSFER_FUNCTION: Moves interpretive authority, social legitimacy, material resources (endowments, state funding), and legal decision-making power from the governed (lay Muslims, women, minorities, reformist voices) to the governing (ulama institutions, madhhab councils, state religious bureaucracies). The governed receive communal belonging and ritual validity; the governors collect rents of authority.
% ABSENT_VOICES: Muslims in diaspora communities without access to alternative religious authorities; queer Muslims whose existence is foreclosed by the textual framework; ex-Muslims in traditionalist-dominant societies who face capital punishment for apostasy; feminist scholars working within the tradition but denied institutional platforms. They are absent because the taqlid structure treats their perspectives as categorically illegitimate — not merely wrong, but outside the bounds of the conversation.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation vanished overnight: classical madhhab authority would lose its coercive monopoly; reformist ijtihad would become the default mode for legal reasoning; personal status laws would face immediate constitutional challenge; women's testimony and inheritance rights would be renegotiated; religious minorities would demand equal citizenship; endowment administrations would face legal restructuring. The Muslim world's legal architecture would reorganize around contestation rather than consensus.
% FOUNDING_PROBLEM: Post-formative fragmentation: after the Prophet's death and the closure of revelation, the expanding Muslim empire faced chaotic legal pluralism — every region, every scholar, every tribe interpreting Quran and Hadith differently. Taqlid and madhhab consensus emerged to stabilize law, prevent anarchy, and preserve communal unity across vast territories.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist scholars attest the problem is live: they cite ongoing sectarian fragmentation, modernist confusion, and the necessity of authoritative guidance for lay Muslims. Reformist scholars, historians of Islamic law (e.g., Wael Hallaq, Knut Vikør), and constitutional courts in Muslim-majority countries attest the founding problem is substantially solved: the chaos of early expansion is gone; modern states have codified law; the madhhab system now functions as a barrier to necessary adaptation. The corroboration is split along the same authority lines the constraint enforces.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the taqlid obligation transfers interpretive sovereignty from the subject to the scholar-class, with no accountability mechanism and no exit for those disadvantaged by classical rulings. Suppression (0.78) is high because the constraint's persistence depends on active enforcement: criminalizing apostasy/blasphemy, barring reformist scholars from platforms, codifying classical fiqh in state law, and treating dissent as deviation rather than interpretation. Theater ratio (0.45) reflects that the coordination function (communal legal unity) is real but increasingly performative — the madhhab system no longer resolves novel problems (bioethics, finance, human rights) but maintains the ritual of consultation. Accessibility collapse (0.68) is substantial: once one accepts the premise that classical consensus is binding, alternative readings (reformist, Quran-centric, contextual) appear not as valid options but as heresy. Resistance (0.55) is moderate: reformist movements exist but face severe structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (traditional ulama) experiences this constraint as genuine coordination: it solves the problem of legal anarchy, preserves the tradition, and enables communal cohesion. The payer seats (women, minorities, progressives) experience it as enforced extraction: the same structure that 'coordinates' the community does so by freezing patriarchal and hierarchical rulings that systematically disadvantage them. The engine computes this divergence from the structural data — the declared beneficiaries/victims, power levels, and exit options produce different effective extraction (chi) per seat. The claimed type (tangled_rope) captures the structural reality: both coordination AND extraction are present, requiring active enforcement to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama institutions and madhhab councils are structural beneficiaries (d ~ 0.15): they collect authority, resources, and legitimacy from administering the taqlid framework. Their exit is arbitrage/identity-locked — they could theoretically reform but their institutional identity is fused with the system. Mosque hierarchies and endowment administrators are secondary beneficiaries (d ~ 0.25-0.35): they derive local legitimacy and material interests but have less agenda-setting power. Progressive Muslims, women seeking equal status, and religious minorities are structural targets (d ~ 0.85-0.95): they bear the extractive costs (legal disability, social exclusion, epistemic marginalization) with trapped or constrained exit. Reformist scholars are excluded (d ~ 0.9): their exclusion is the enforcement mechanism. Secular legal observers sit at analytical (d ~ 0.5): they see the full structure but bear no direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-revelation legal fragmentation) was genuinely solved by the madhhab system — but that solution has outlived its function. Modern states have legislatures, constitutions, and codified laws; the chaotic pluralism of the 8th-10th centuries is gone. Yet the taqlid obligation persists and has hardened, because the authority structures that administer it (ulama institutions, madhhab councils, state religious bureaucracies) extract substantial benefits from its continuation. This is mandatrophy: a coordination mechanism that solved its founding problem but was captured by its administrators and repurposed as an extraction mechanism. The constraint now persists not because it coordinates, but because its beneficiaries have the power to enforce it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taqlid_ijtihad_boundary,
    'Is the traditionalist claim that the ''gates of ijtihad are closed'' a genuine doctrinal consensus (ijma) or a constructed closure that serves institutional interests?',
    'Historical analysis of classical usul al-fiqh texts: did the major schools actually declare ijtihad closed, or did they define conditions for ijtihad that became practically unattainable? Correlate with the rise of madhhab institutionalization and state patronage.',
    'If the closure was constructed, the taqlid obligation lacks its claimed doctrinal foundation and is revealed as institutional self-preservation — supporting reclassification toward snare. If genuine ijma, the coordination function has stronger epistemic warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_ijtihad_boundary, conceptual, 'Whether the closure of ijtihad is doctrinal fact or institutional construction.').

omega_variable(
    coordination_extraction_separability,
    'Can the communal coordination function of madhhab affiliation (shared ritual, marriage validity, inheritance certainty) be separated from the extractive function (gender hierarchy, minority subordination, reformist exclusion)?',
    'Comparative study of Muslim communities that have retained madhhab identity while reforming gender/minority rulings (e.g., some Malaysian states, Tunisian personal status code, progressive mosques in the West). Assess whether communal cohesion survives doctrinal adaptation.',
    'If separable, the extraction is not the price of coordination but a separable layer — the constraint is more snare-like. If inseparable, part of the measured extraction is the genuine cost of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    state_enforcement_dependency,
    'To what degree does the traditionalist taqlid constraint depend on state enforcement vs. voluntary communal adherence in the contemporary period?',
    'Jurisdiction-level analysis: compare enforcement mechanisms and compliance rates in states with codified sharia personal status law (Saudi Arabia, Iran, Pakistan, UAE) vs. states with secular family law but traditionalist civil society (Turkey, Indonesia, Senegal, Western diasporas).',
    'If state enforcement is the primary driver, the constraint is more snare-like (coercion-dependent). If voluntary adherence dominates, the coordination function is more genuine. This affects the suppression metric''s structural vs. internalized composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_dependency, empirical, 'State vs. civil society as the primary enforcement vector for taqlid.').

omega_variable(
    kernel_reading_framing,
    'Does the quran_hadith_substrate kernel admit the traditionalist_taqlid reading as the only coherent framing, or is the kernel itself under-specified such that multiple readings are equally warranted by the textual evidence?',
    'Analyze the kernel''s structural properties: does the Quran-Hadith corpus contain internal criteria for adjudicating between taqlid and ijtihad as authoritative modes? Or is the choice of reading determined by extra-textual commitments (institutional, political, ethical)?',
    'If the kernel is under-specified (distributed kernel_codification), the traditionalist reading''s claim to exclusive authority is a power move, not a textual necessity. This supports the extraction diagnosis. If the kernel formally specifies taqlid (formalized/fixed_text), the reading has stronger coordination warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel''s structure determines the reading or the reading imposes structure on the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (state law, institutional blacklisting, legal penalties) or internalized (psychological internalization of inferiority, identity fusion with the tradition, fear of divine punishment)?',
    'Post-exit trajectory study: track individuals who leave traditionalist communities — does suppression persist after structural barriers are removed? Compare diaspora Muslims in secular legal environments: do they still self-censor on gender/minority issues?',
    'If substantially internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the subject. This would increase the extraction burden on payer seats and support a snare classification. If primarily structural, exit (migration, legal reform) genuinely reduces suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in traditionalist taqlid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1750, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1750, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(qura_tr_t1800, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(qura_tr_t1850, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(qura_tr_t1900, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(qura_tr_t1950, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1950, 0.42).
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(qura_tr_t2025, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(qura_be_t1750, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1750, 0.45).
narrative_ontology:measurement(qura_be_t1800, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement(qura_be_t1850, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1850, 0.58).
narrative_ontology:measurement(qura_be_t1900, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement(qura_be_t1950, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(qura_be_t2025, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1750, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(qura_su_t1800, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1800, 0.62).
narrative_ontology:measurement(qura_su_t1850, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement(qura_su_t1900, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement(qura_su_t1950, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(qura_su_t2025, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, muslim_personal_status_codes).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, apostasy_blasphemy_laws).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, religious_endowment_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the quran_hadith_substrate kernel. The traditionalist_taqlid reading asserts binding classical consensus and obligatory deference. The reformist_ijtihad reading asserts contextual independent reasoning prioritizing the Quran's ethical trajectory. The state_hybrid reading asserts political sovereignty as the legitimating ground for selective adoption. The three readings share the same textual substrate but instantiate different constraints with different beneficiary/victim structures and extraction profiles. This decomposition follows the epsilon-invariance principle: the label 'Islamic law' conflates structurally distinct claims that must be analyzed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, institutional, 0.15).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, organized, 0.25).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, moderate, 0.65).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
