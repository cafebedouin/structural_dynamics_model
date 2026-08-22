% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Madhhab Taqlid Obligation (Traditionalist Reading)
 *   domain: religious/legal-authority
 *
 * SUMMARY:
 *   The arrangement under contest is the taqlid obligation as the
 *   traditionalist reading instantiates it: the classical schools of fiqh are
 *   treated as embodiments of authoritative consensus (ijma), and
 *   contemporary Muslims are obligated to take settled rulings from
 *   accredited scholars rather than derive law directly from the Quran and
 *   hadith. This file is ONE reading of the quran_hadith_substrate kernel;
 *   the reformist_ijtihad and state_hybrid readings are separate constraints
 *   with their own epsilon values, victim sets, and classifications, linked
 *   through the network block and not averaged into this one. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing taqlid
 *   arrangement itself — never for the reformist alternative this reading
 *   opposes — and the value is reading-indexed: a story authored from the
 *   reformist seat over the same referent would score the same arrangement
 *   differently. The claimed type and the metric values were authored
 *   independently: the claim asserts a genuine coordination function joined
 *   to asymmetric extraction held together by active enforcement; the metrics
 *   describe observed operation. KEY AGENTS (by structural relationship):
 *   ulama_scholarly_establishment (primary beneficiary and agenda setter,
 *   institutional/identity_locked); madhhab_institutions (beneficiary,
 *   institutional/civilizational); mosque_hierarchies (secondary beneficiary
 *   and local enforcement arm, organized/constrained); personal_status_courts
 *   (enforcement administrator, institutional/constrained);
 *   lay_taqlid_followers (dual-positioned: coordination beneficiary, indirect
 *   payer, moderate/constrained); progressive_muslims (target,
 *   moderate/constrained); women_seeking_equal_legal_status (target,
 *   powerless/trapped); religious_minorities_under_dhimmi_frameworks (target,
 *   powerless/trapped); reformist_scholars (excluded voice,
 *   moderate/constrained); comparative_jurisprudence_observers (analytical
 *   observer).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.7).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.76).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Madhhab Taqlid Obligation (Traditionalist Reading)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal-authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '8e747fcd-7e92-456e-b7fd-b62d9f0c2c86').
narrative_ontology:cs_kernel_codification('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', fixed_text).
narrative_ontology:cs_authority_grounding('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', lineage).
narrative_ontology:cs_interpretation_layer_present('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86').
narrative_ontology:cs_reading_relation('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', foundational, classical_ijma_binding_on_later_generations).
narrative_ontology:cs_axiom_status(classical_ijma_binding_on_later_generations, holdable).
narrative_ontology:cs_axiom_grounding('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', classical_ijma_binding_on_later_generations, theological).
narrative_ontology:cs_axiom('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', foundational, taqlid_obligatory_for_unqualified_believers).
narrative_ontology:cs_axiom_status(taqlid_obligatory_for_unqualified_believers, holdable).
narrative_ontology:cs_axiom_grounding('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', taqlid_obligatory_for_unqualified_believers, instrumental).
narrative_ontology:cs_reference_frame('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', classical_ijma_closure).
narrative_ontology:cs_drift_state('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', contemporary_mass_literacy_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8e747fcd-7e92-456e-b7fd-b62d9f0c2c86', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_establishment).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, lay_taqlid_followers).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_taqlid_followers).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, classical_ijma_infallibility_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, isnad_transmission_chain_reliability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train in one or more madhhab curricula, receive authorization (ijaza) through teacher chains reaching the classical imams, and issue rulings and fatwas citing the school's received doctrine. Collect stipends, endowment income, students, and social deference; their standing depends on the continued binding force of the schools' consensus. Leaving the role means forfeiting livelihood, learned standing, and a community identity built over decades of formation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Endowed seminaries, fatwa councils, and scholarly lineages that hold the schools' curricula, libraries, and certification authority. Accumulate endowments, enrollment, and prestige across generations; their continuity is constituted by transmitting the schools intact, so doctrinal revision threatens the very asset they exist to preserve.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, identity_locked, global).

% Imams and mosque boards who teach the schools' positions in sermons and classes and handle marriages, divorces, burials, and local disputes according to received doctrine. Draw salaries and community standing from the arrangement and spend real effort addressing deviation in their congregations, since unlicensed reinterpretation nearby undermines their own standing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, biographical, constrained, regional).

% State courts in traditionalist-dominant jurisdictions that apply codified madhhab rules to marriage, divorce, custody, and inheritance. Judges and registries administer the enforcement machinery; their careers and procedural legitimacy ride on the codified doctrine remaining stable.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, personal_status_courts, agenda_setter,
    institutional, generational, constrained, national).

% Ordinary believers who ask recognized scholars for rulings instead of deriving law themselves, gaining quick, standardized answers for prayer, fasting, finance, and family matters. Also absorb the arrangement's costs indirectly: inheritance shares, divorce procedure, and gender-role rules reach them as settled facts, and switching to self-study costs years of training and exposes them to accusations of arrogance.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, lay_taqlid_followers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, lay_taqlid_followers, payer).

% Believers who read the sources through contemporary ethical commitments and press for revised positions on gender, apostasy, punishment, and interfaith relations. Face refutation treatises, pulpit denunciation, exclusion from teaching posts and media platforms, and in some jurisdictions formal charges; staying inside the community while dissenting is the costly path most of them take.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Women contesting unequal inheritance shares, unilateral divorce powers, polygyny rules, and testimony weight as applied by family courts and mosque arbitration. Their options run through the very tribunals applying the doctrine; individual opt-out is unavailable because the rules attach to marriage and estates regardless of personal conviction.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status, payer,
    powerless, biographical, trapped, regional).

% Non-Muslim citizens and residents in traditionalist-dominant societies whose status, worship permissions, and property relations are discussed in classical subordinate-status terms by preachers and occasionally codified. Cannot vote the doctrine away; emigration is the main exit, at the price of homeland, livelihood, and family.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, generational, trapped, regional).

% Jurists trained in the same curricula who argue for reopening direct source engagement and weighting the Quran's ethical aims over inherited applications. Publish, broadcast, and organize parallel study circles after being denied pulpits, council seats, and certification channels controlled by the establishment.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, continental).

% Academic historians and legal anthropologists of Islamic law who study the formation of the schools, the closure debates, and contemporary enforcement without holding a confessional stake. Document the arrangement's history and mechanics from outside its authority economy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, comparative_jurisprudence_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_establishment).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediates revelation for a community in which competent derivation of law from the Arabic sources requires years of specialized training: the madhhab system supplies settled answers, standardizes worship and transactions across regions and generations, and preserves a vetted chain of interpretation so that each believer need not re-derive the law.
% TRANSFER_FUNCTION: Moves interpretive authority and its material support — stipends, endowments, students, deference — from the general body of believers to the scholarly establishment, and moves compliance with classical rulings onto all believers, with the heaviest costs landing on those whose status the classical rulings subordinate: women in family law, dissenting interpreters facing sanction, and non-Muslims discussed under subordinate-status frameworks.
% ABSENT_VOICES: Reformist jurists, Muslim feminist scholars, and non-Muslim citizens of traditionalist-dominant societies would object that the arrangement settles questions they live under without their participation; they stand outside the fatwa councils, curriculum committees, and court consultative bodies where the doctrine is maintained, and their objections are pre-labeled innovation or foreign import before being heard.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation vanished overnight, every mosque and court would need a replacement source of authoritative answers: mass direct engagement with the sources, competing interpreter networks, or state codification filling the vacuum. Religious authority incomes, seminary enrollments, and the family-law doctrines applied to millions would all be renegotiated within a generation.
% FOUNDING_PROBLEM: After the Prophet's death the community faced novel cases with no recorded prophetic answer; by the classical period the pressing problem was methodological chaos — unqualified men issuing contradictory rulings and weak reports circulating as law — and the schools formed to discipline interpretation through accredited methods and transmitted doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Islamic law outside the benefiting parties corroborate the founding problem's reality (documented diversity and conflict of early rulings, the schools' methodological consolidation) while dating the effective closure of independent ijtihad to later institutional development rather than original design; reformist jurists outside the establishment attest that mass literacy, printed and translated source corpora, and modern legal training have changed the epistemic conditions the arrangement presupposes. No attestation from outside the beneficiary set supports the claim that the problem remains live in its original gatekeeping form.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon sits at 0.70 because the arrangement transfers real goods — interpretive authority, income, legal control over family life — from a broad population to a concentrated establishment, while retaining a genuine service (accessible, standardized rulings) that partially offsets the transfer. Suppression (0.76) is authored as a raw structural property, unscaled by power or scope: enforcement combines institutional instruments (denial of pulpits, certification, teaching posts, and in some jurisdictions criminal process) with social instruments (takfir accusation, marriage and burial obstruction) and reaches into family law where individual exit is unavailable. Theater_ratio (0.35) reflects a real but partially ceremonialized function: fatwa production and seminary instruction still resolve live disputes, while a growing share of establishment activity defends the arrangement's own authority — refutation literature against anti-madhhabism, ritualized citation chains — as state codes absorb actual adjudication. Accessibility_collapse (0.45) is moderate: alternatives (reformist ijtihad, anti-madhhab literalism, state civil law) remain reachable, so understanding the arrangement does not close the option set. Resistance (0.55) records sustained organized pushback — reformist publishing, feminist legal activism, dissident preaching — including coalition formation among the victim classes. All three tracked series share one time grid (1850, 1900, 1950, 1975, 2000, 2026) so no metric row is sampled against another metric's scalar substitute; the trajectories show extraction accumulation and enforcement intensification alongside functional atrophy in adjudicating domains.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the ulama seat the arrangement is a sacred trust: they experience the schools as preserved, verified law and experience dissent as ingratitude toward fourteen centuries of scholarly verification. From the women and minority seats the same structure arrives as family-court doctrine and sermon rhetoric that fixes their status without consultation. From the lay seat it is mostly a convenience with occasional sharp costs — an inheritance share, a divorce procedure, a daughter's marriage terms. The engine derives these divergent per-seat classifications from the power, exit, and role data; nothing in the claimed type adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (ulama, madhhab institutions, mosque hierarchies) derive low directionality — the arrangement subsidizes them — amplified by identity_locked exit: their professional and confessional selves are constituted by the transmission chains the arrangement preserves, so even formal freedom to defect would not loosen the bond. Declared victims derive high directionality, with trapped exit (women under family-law attachment, minorities under status frameworks) pushing them toward the full-target end and constrained exit (progressive Muslims, who can emigrate or disaffiliate at real cost) sitting somewhat nearer the middle. Lay believers carry a dual declaration (beneficiary with payer secondary role) and land near symmetric. Personal-status courts administer enforcement without capturing the principal gains, so their derived directionality reflects administrative exposure rather than receipt. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disciplining interpretation after the loss of direct prophetic guidance — is live in its general form (novel cases keep arriving) but contested in its gatekeeping form (whether accreditation must route through the classical schools). Because founding_problem_status is contested rather than dead, the status-by-verdict mismatch consumer finds no dead-mandate zombie signature despite verdict world_rearranges. The mislabeling risks run in both directions: calling the whole arrangement pure extraction would erase the coordination leg that keeps most lay believers voluntary participants; calling it pure coordination would erase the enforced subordination the victim seats document. The tangled-rope claim holds both legs apart. Partial obsolescence is real and localized: where state codes absorbed adjudication, the schools' judicial function atrophied into advisory and ceremonial roles — visible in the rising theater_ratio series — with no sunset clause ever declared and no mechanism for winding down the domains the state took over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the traditionalist_taqlid reading of the quran_hadith_substrate kernel; would the reformist_ijtihad or state_hybrid readings of the same substrate produce a different victim set, enforcement profile, and epsilon over the same referent?',
    'Comparative authoring of the sibling stories over the identical referent; observational leverage from jurisdictions where each reading dominates (personal-status law regimes, ijtihad-pluralist communities, hybrid state codes).',
    'If a sibling reading prevailed institutionally, the taqlid-specific extraction (enforced deference, sanctioned reinterpretation) would transfer to a different arrangement with a different victim set; this story''s epsilon is valid only for the traditionalist instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression of dissent structural (institutional discipline, takfir accusation, family-law compulsion) or internalized (believers trained to experience questioning as sin or arrogance)?',
    'Post-exit trajectory tracking: survey and ethnographic data on believers who move to non-traditionalist settings; if deference reflexes persist after external barriers drop, the internalized share is high.',
    'If largely internalized, effective suppression exceeds the structural measure and persists even where formal enforcement weakens; the scalar suppression value understates the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural vs internalized suppression mechanism split for the deference obligation.').

omega_variable(
    epistemic_gap_permanence,
    'Is the competence asymmetry that justifies taqlid (lay believers cannot derive law from the Arabic sources) a permanent feature of religious knowledge or an artifact of restricted access to training?',
    'Track outcomes as mass religious education, printed and translated source corpora, and searchable digital archives spread: if competent independent evaluation diffuses faster than institutional gatekeeping adapts, the gap narrows.',
    'If the gap is artifactual and closing, the obligation''s justification decays toward transitional-support logic and persistence increasingly rides on enforcement rather than epistemic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_gap_permanence, empirical, 'Whether the epistemic division of labor grounding taqlid is permanent or historically contingent.').

omega_variable(
    dhimmi_provisions_operative_status,
    'Are classical dhimmi provisions an operative component of the living taqlid arrangement, or dormant doctrine cited selectively?',
    'Audit contemporary fatwa corpora, seminary curricula, and personal-status codes for active invocation of dhimmi rules versus purely historical citation.',
    'If operative, the victim set includes resident non-Muslim populations as a standing class and epsilon rises; if dormant, the minority-victim count shrinks and the measured extraction drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimmi_provisions_operative_status, empirical, 'Operative status of classical subordinate-status frameworks within the living arrangement.').

omega_variable(
    lay_net_benefit_valence,
    'Do lay believers net-benefit from the taqlid arrangement (certainty, standardized practice, accessible dispute resolution) once the costs passed through family law and suppressed reinterpretation are counted?',
    'Survey and welfare comparison of observance satisfaction and material outcomes across communities under taqlid-dominant versus interpretively pluralist regimes.',
    'If lay net benefit is strongly positive, the coordination leg is robust and extraction concentrates on the three victim classes; if negative, the beneficiary structure collapses toward extraction maintained by enforcement alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_net_benefit_valence, conceptual, 'Net valence of the lay-believer position under the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1850, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1850, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(qura_tr_t1900, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1900, 0.24).
narrative_ontology:measurement(qura_tr_t1950, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(qura_tr_t1975, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1975, 0.31).
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(qura_tr_t2026, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(qura_be_t1850, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1850, 0.58).
narrative_ontology:measurement(qura_be_t1900, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(qura_be_t1950, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1950, 0.63).
narrative_ontology:measurement(qura_be_t1975, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1975, 0.66).
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2000, 0.69).
narrative_ontology:measurement(qura_be_t2026, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1850, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1850, 0.62).
narrative_ontology:measurement(qura_su_t1900, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1900, 0.66).
narrative_ontology:measurement(qura_su_t1950, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(qura_su_t1975, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1975, 0.73).
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(qura_su_t2026, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic legal authority' decomposes, per the epsilon-invariance principle, into at least three structurally distinct arrangements corresponding to the three readings of the quran_hadith_substrate kernel: enforced deference to classical consensus (this file), mandated contextual ijtihad (reformist_ijtihad), and sovereignty-indexed selective adoption (state_hybrid). Each carries its own epsilon, victim set, and enforcement profile; measuring 'Islamic legal authority' with a single observable would conflate them. Edges run in both directions: this reading's ijma doctrine is cited as evidence against the reformist reopening, and traditionalist social pressure constrains which classical rules hybrid states retain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
