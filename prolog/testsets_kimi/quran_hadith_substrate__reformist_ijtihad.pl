% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Contextual Ijtihad Prioritizing Quranic Ethical Trajectory
 *   domain: religious/jurisprudential
 *
 * SUMMARY:
 *   This constraint story instantiates the reformist_ijtihad reading of the
 *   contested kernel quran_hadith_substrate. It models a jurisprudential
 *   methodology that mandates contextual ijtihad when classical fiqh rulings
 *   conflict with contemporary ethics, human rights norms, or public interest
 *   (maslaha). The Quran's ethical trajectory is treated as the hermeneutic
 *   north star, overriding literalist hadith application and classical
 *   madhhab consensus. The constraint operates as a tangled rope: it
 *   genuinely coordinates modern Muslim identity with evolving ethical
 *   standards (solving an existential collective-action problem for
 *   progressive Muslims), while simultaneously extracting interpretive
 *   authority and institutional legitimacy from traditionalist clergy who
 *   depend on a monopoly over classical texts. The claim/metric independence
 *   is maintained: the claimed type is tangled_rope, while the metrics
 *   honestly reflect moderate-to-high extraction (0.45), substantial
 *   resistance (0.70), and moderate suppression (0.50) that is vulnerable to
 *   counter-mobilization.
 *
 * KEY AGENTS:
 *   - reformist_jurists: Agenda-setter (powerful/generational) â construct and enforce the contextual hermeneutic
 *   - progressive_muslims: Primary beneficiary (organized/biographical) â gain theological legitimacy for modern ethics
 *   - muslim_women: Primary beneficiary (powerless/biographical) â gain internally-Islamic arguments for autonomy
 *   - lgbtq_muslims: Primary beneficiary (powerless/identity_locked) â gain reconciled religious belonging
 *   - religious_minorities: Secondary beneficiary (moderate/generational) â gain doctrinal equality over dhimmi status
 *   - traditionalist_clergy: Primary target/payer (institutional/trapped) â lose interpretive monopoly and institutional rents
 *   - human_rights_observers: Analytical observer (institutional/generational) â monitors outcomes without theological stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.5).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Contextual Ijtihad Prioritizing Quranic Ethical Trajectory").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/jurisprudential").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '2b027c99-1d33-4582-bbae-4b6f56ae0367').
narrative_ontology:cs_kernel_codification('2b027c99-1d33-4582-bbae-4b6f56ae0367', fixed_text).
narrative_ontology:cs_authority_grounding('2b027c99-1d33-4582-bbae-4b6f56ae0367', lineage).
narrative_ontology:cs_interpretation_layer_present('2b027c99-1d33-4582-bbae-4b6f56ae0367').
narrative_ontology:cs_reading_relation('2b027c99-1d33-4582-bbae-4b6f56ae0367', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('2b027c99-1d33-4582-bbae-4b6f56ae0367', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('2b027c99-1d33-4582-bbae-4b6f56ae0367', foundational, quran_ethical_trajectory_priority).
narrative_ontology:cs_axiom_status(quran_ethical_trajectory_priority, holdable).
narrative_ontology:cs_axiom_grounding('2b027c99-1d33-4582-bbae-4b6f56ae0367', quran_ethical_trajectory_priority, deontological).
narrative_ontology:cs_axiom('2b027c99-1d33-4582-bbae-4b6f56ae0367', foundational, maslaha_over_classical_rulings).
narrative_ontology:cs_axiom_status(maslaha_over_classical_rulings, holdable).
narrative_ontology:cs_axiom_grounding('2b027c99-1d33-4582-bbae-4b6f56ae0367', maslaha_over_classical_rulings, instrumental).
narrative_ontology:cs_reference_frame('2b027c99-1d33-4582-bbae-4b6f56ae0367', quranic_egalitarian_ethic).
narrative_ontology:cs_drift_state('2b027c99-1d33-4582-bbae-4b6f56ae0367', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b027c99-1d33-4582-bbae-4b6f56ae0367', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_clergy).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, quranic_ethical_trajectory_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maslaha_public_interest_principle).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, contextual_hadith_criticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and advocate for contextual hermeneutic methods that prioritize the Quran's ethical trajectory over literalist hadith and classical fiqh rulings. They publish alternative fatwas, teach in reformist seminaries, and advise states or NGOs on family law reform. Their authority depends on scholarly credibility and institutional backing; they can relocate to liberal academic or policy institutions if traditionalist hostility intensifies.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_jurists, agenda_setter,
    powerful, generational, mobile, global).

% Seek to practice Islam in ways compatible with modern ethics, gender equality, and pluralism. They benefit from theological legitimacy provided by reformist ijtihad, which allows them to remain within the tradition while rejecting patriarchal or exclusivist classical rulings. Exit is constrained because leaving the faith community carries high social and familial costs.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    organized, biographical, constrained, national).

% Are primary beneficiaries of reformist re-readings of family law, inheritance, and bodily autonomy. The constraint provides them with internally Islamic arguments for rights that classical fiqh often restricts. Their exit options are constrained by economic dependency and family structures, making internal reform often the only viable path to religious dignity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women, beneficiary,
    powerless, biographical, constrained, national).

% Require theological frameworks that reconcile their sexual or gender identity with Muslim belonging. Reformist ijtihad offers scarce but vital legitimacy by distinguishing eternal ethical principles from time-bound classical rulings. Their exit is identity-locked: abandoning Islam is psychologically and communally traumatic, so the availability of a reformist reading is often experienced as existential rather than optional.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, national).

% Benefit from reformist readings that reject dhimmi-style classical hierarchies in favor of Quranic pluralism and citizenship equality. This reduces the doctrinal basis for discriminatory personal-status codes. Their exit is constrained by citizenship and geographic location within Muslim-majority states.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    moderate, generational, constrained, national).

% Derive institutional authority and material support from their role as gatekeepers of classical fiqh and hadith literalism. Reformist ijtihad directly undermines their interpretive monopoly by delegitimizing taqlid and reopening questions they consider settled. They are trapped because their professional identity, educational capital, and community standing are fused with the classical system; they cannot adopt reformism without dissolving their own authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_clergy, payer,
    institutional, generational, trapped, global).

% Monitor the compatibility of religious legal systems with international human rights standards. They document whether reformist ijtihad produces tangible legal improvements for women and minorities or remains theological theater. They have no stake in the intra-Islamic legitimacy contest but track its outcomes for policy and advocacy purposes.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a methodological bridge between Islamic scriptural sources and modern ethical norms, allowing Muslims to maintain religious identity and communal belonging while accepting gender equality, human rights, and pluralism without requiring mass apostasy or secular abandonment of the tradition.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from classical madhhab institutions and literalist hadith scholars to contextualist reformist jurists; simultaneously transfers rights, dignity, and communal inclusion to women, LGBTQ+ individuals, and religious minorities who were excluded by classical rulings.
% ABSENT_VOICES: Traditionalist muftis and salafi literalists are structurally excluded from the reformist interpretive framework because their methodological premises are rejected at the root; conservative state actors who prefer the state_hybrid reading are also absent from the reformist theological conversation, as they ground legitimacy in sovereignty rather than hermeneutic renewal.
% DISAPPEARANCE_RATIONALE: Without the reformist ijtihad constraint, progressive Muslims would lose the theological vocabulary to advocate for gender equality and LGBTQ+ inclusion from within the tradition; women and minorities would face re-enclosure under classical fiqh rulings; and the intellectual and legal space for Islamic human rights discourse would collapse, forcing reform-oriented populations toward either silent hypocrisy or open exit from the community.
% FOUNDING_PROBLEM: Classical fiqh rulings produced legal and ethical outcomes (slavery, gender hierarchy, dhimmi subordination, punitive apostasy) that became increasingly incompatible with modern nation-state citizenship, international human rights norms, and the ethical expectations of educated Muslims, generating a crisis of religious legitimacy in the modern period.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial legal historians and Muslim feminist scholars outside the traditionalist clergy corroborate the crisis of legitimacy; international human rights bodies document the gap between classical personal-status codes and universal rights standards. Traditionalist clergy deny that any incompatibility exists, asserting classical rulings remain eternally valid.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.45 because the constraint systematically transfers authority from traditionalist institutions to reformist scholars and inclusion to marginalized groups, imposing a real cost on the former. Suppression is 0.50: the constraint actively suppresses literalist/traditionalist exclusivity where it holds institutional power, but this suppression is incomplete and vulnerable to backlash. Theater_ratio at 0.35 captures the performative dimension of some state-backed reform that gestures toward equality while preserving patriarchal structures in practice. Accessibility_collapse (0.55) reflects that within reformist institutional spheres, classical alternatives become delegitimized and hard to articulate. Resistance (0.70) is high because traditionalist clergy and their constituencies mount sustained theological, political, and legal opposition. The temporal measurements show extraction rising from 0.25 to 0.48 as institutional backing grows, theater rising as reformist discourse is partially co-opted, and suppression fluctuating with cycles of traditionalist counter-mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (reformist jurists) experiences the constraint as genuine jurisprudential recovery and ethical progress. The payer seat (traditionalist clergy) experiences it as hermeneutical violence and institutional dispossession. The beneficiary seats (marginalized groups) experience it as survival and inclusion. The engine computes these divergent classifications from the same structural data: low exit options and identity-lock amplify effective extraction for trapped traditionalists, while coordination benefits damp extraction for constrained beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist_jurists sit near the beneficiary end (d â 0.25) because they accrue scholarly authority and institutional status from the new hermeneutic, though they are not the primary material beneficiaries. Progressive Muslims, women, LGBTQ+ Muslims, and religious minorities sit at the symmetric-to-beneficiary end (d â 0.3â0.4) because they receive coordination goods (inclusion, legitimacy) without paying extractive costs. Traditionalist_clergy sit at the full-target end (d â 0.9) because the constraint directly dissolves their interpretive monopoly and doctrinal authority; they are trapped in a role that the constraint renders obsolete. Human_rights_observers are analytical (d â 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as either pure rope (which would ignore the real cost imposed on traditionalist authority structures and the active enforcement required to displace classical fiqh) or pure snare (which would deny the genuine coordination function for millions of Muslims who would otherwise face an impossible choice between their faith and their ethical commitments). The founding problem â classical fiqh's modern legitimacy crisis â is contested but corroborated by external historians and human rights bodies, satisfying the R5 genealogy requirement and preventing mandatrophy by grounding persistence in a live or contested problem rather than inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_backing_source,
    'Does the constraint''s efficacy depend on genuine scholarly consensus and grassroots community uptake, or on state coercion and elite institutional capture?',
    'Comparative analysis across jurisdictions: where reformist ijtihad thrives under civil-society institutions without state backing, it is scholarly; where it persists only through state-family-law codification and collapses when the state withdraws, it is captured.',
    'If state-dependent, the constraint''s coordination function is weaker and its extraction from traditionalists is more akin to political domination than theological persuasion, potentially shifting classification toward snare in authoritarian contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_backing_source, empirical, 'Whether reformist ijtihad is organically scholarly or state-captured').

omega_variable(
    quranic_trajectory_objectivity,
    'Is the ''Quranic ethical trajectory'' a recoverable objective hermeneutic, or a modernist projection constructed to justify predetermined ethical conclusions?',
    'Inter-temporal consistency check: do reformist jurists working independently across different languages and polities converge on similar ethical-trajectory readings, or do their readings track their local political contexts?',
    'If the trajectory is largely constructed, the constraint''s coordination function is ideological cover rather than genuine jurisprudential recovery, raising theater_ratio and potentially collapsing the coordination half of the tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quranic_trajectory_objectivity, conceptual, 'Whether the Quranic ethical trajectory is discovered or constructed').

omega_variable(
    traditionalist_counter_mobilization_vulnerability,
    'Will traditionalist counter-mobilization systematically reverse reformist gains, indicating that the constraint''s suppression capacity is weaker than its institutional presence suggests?',
    'Track legislative and judicial rollbacks in Muslim-majority states over a 20-year window; measure whether reformist family-law codes survive changes of government or are routinely dismantled by incoming traditionalist coalitions.',
    'If reformist constraints are routinely reversed, the high resistance metric is structurally dominant and the constraint functions more like a transient scaffold than a stable tangled rope; low persistence would imply piton-like features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_counter_mobilization_vulnerability, empirical, 'Whether traditionalist backlash undermines constraint persistence').

omega_variable(
    kernel_reading_boundary,
    'Does the reformist ijtihad reading decompose into multiple structurally distinct constraints depending on whether the prioritization mechanism is Quranic textual, maslaha-based, or human-rights-based?',
    'Separate analysis of constraints where Quranic hermeneutics alone drives reform versus those where international human rights norms are the primary engine; check if epsilon and victim/beneficiary structures diverge.',
    'If the three justifications produce different epsilon values and different beneficiary/victim distributions, the current story conflates multiple constraints under one label and should be split per the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether reformist ijtihad is one constraint or a family of methods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.2).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.28).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 30, 0.3).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.32).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The natural-language concept 'quran_hadith_substrate' conflates three structurally distinct readings: reformist_ijtihad (this file), traditionalist_taqlid, and state_hybrid. Each reading has a different epsilon, different beneficiary/victim structure, and different authority grounding. They are modeled as a constraint family linked by affects_constraints rather than as a single story with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
