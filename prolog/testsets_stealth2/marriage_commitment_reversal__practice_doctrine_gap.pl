% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Post-Manifesto Doctrine-Practice Gap in Plural-Marriage Commitment (1890-1935)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   Between the 1890 Manifesto and the Second Manifesto of 1904, the LDS
 *   governing hierarchy suspended new plural marriages in public compliance
 *   with federal law while leaving Section 132 — the canonized revelation
 *   underwriting the practice — untouched in the Doctrine and Covenants. The
 *   resulting doctrine-practice gap was not idle: it authorized a dual track
 *   in which the institution denied the practice in public while apostles
 *   performed roughly two hundred post-Manifesto sealings in Chihuahua,
 *   Alberta, and aboard international waters, in jurisdictions claimed as
 *   legal. When the Reed Smoot hearings forced the issue, the Second
 *   Manifesto closed the public gap at the cost of apostolic resignations
 *   (John W. Taylor, Matthias F. Cowley), stranded interim-married families,
 *   and — because the canon itself was never revised — a fundamentalist
 *   movement that organized around the preserved section and schismed by
 *   1929-1935. This story authors the gap-as-structure: one reading of the
 *   marriage_commitment_reversal kernel, decomposed per the
 *   epsilon-invariance principle from its causal siblings (see
 *   network.dual_formulation_note). KEY AGENTS (by structural relationship):
 *   - governing_priesthood_hierarchy: agenda-setting collector
 *   (institutional/arbitrage) — issues both manifestos, controls sealings and
 *   discipline, receives continuity - federal_prosecutorial_authority:
 *   external enforcer turned examiner (institutional/mobile) — created the
 *   compliance demand, then audited it - rank_and_file_membership: diffuse
 *   bearer (moderate/identity_locked) — sustains both lines, bears the
 *   whiplash, keeps the community - post_manifesto_couples: concentrated
 *   bearers (moderate/trapped) — sealed in the gap window, stranded by its
 *   closure - performing_apostles: administrator-turned-bearer
 *   (powerful/identity_locked) — performed interim sealings, surrendered
 *   seats in 1905-1906 - plural_family_women: absent voice
 *   (powerless/trapped) — pre-1890 households in lasting limbo, no seat in
 *   any decision - fundamentalist_dissenters: principled remnant
 *   (powerless/identity_locked) — take the preserved canon literally, absorb
 *   schism and prosecution - religious_history_analysts: analytical observer
 *   (analytical/analytical) — reconstruct the two-track structure from
 *   archives
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.84).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.8).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.84).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Post-Manifesto Doctrine-Practice Gap in Plural-Marriage Commitment (1890-1935)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '266e6687-4300-4782-b0f1-023857d46b66').
narrative_ontology:cs_kernel_codification('266e6687-4300-4782-b0f1-023857d46b66', fixed_text).
narrative_ontology:cs_authority_grounding('266e6687-4300-4782-b0f1-023857d46b66', lineage).
narrative_ontology:cs_interpretation_layer_present('266e6687-4300-4782-b0f1-023857d46b66').
narrative_ontology:cs_reading_relation('266e6687-4300-4782-b0f1-023857d46b66', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('266e6687-4300-4782-b0f1-023857d46b66', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('266e6687-4300-4782-b0f1-023857d46b66', foundational, canon_survives_administrative_suspension).
narrative_ontology:cs_axiom_status(canon_survives_administrative_suspension, holdable).
narrative_ontology:cs_axiom_grounding('266e6687-4300-4782-b0f1-023857d46b66', canon_survives_administrative_suspension, conventional).
narrative_ontology:cs_axiom('266e6687-4300-4782-b0f1-023857d46b66', foundational, compliance_announcement_lacks_canonical_authority).
narrative_ontology:cs_axiom_status(compliance_announcement_lacks_canonical_authority, holdable).
narrative_ontology:cs_axiom_grounding('266e6687-4300-4782-b0f1-023857d46b66', compliance_announcement_lacks_canonical_authority, conventional).
narrative_ontology:cs_reference_frame('266e6687-4300-4782-b0f1-023857d46b66', preserved_principle_managed_compliance).
narrative_ontology:cs_drift_state('266e6687-4300-4782-b0f1-023857d46b66', fundamentalist_schism_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('266e6687-4300-4782-b0f1-023857d46b66', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, governing_priesthood_hierarchy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, rank_and_file_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_couples).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, rank_and_file_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, performing_apostles).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, canon_immunity_to_administrative_revision).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_discretion_in_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve announce policy, decide which sealings are authorized and where, issue both manifestos, and control temple recommends, membership standing, and disciplinary councils. Continuity flows to them: the corporate charter survived, confiscated property was largely restored, Utah statehood arrived in 1896, and a Latter-day Saint sat in the Senate from 1907. They could shift marriage administration to the Mexican and Canadian colonies when enforcement in Utah tightened, and they wrote the public record describing the practice as ended.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, governing_priesthood_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, governing_priesthood_hierarchy, beneficiary).

% Congress, the Justice Department, and the Utah Commission pursued cessation of plural marriage through confiscation statutes, disfranchisement, and cohabitation prosecutions. After 1890 they accepted formal compliance, then probed its completeness — the Reed Smoot hearings (1904-1907) put post-Manifesto marriages under Senate examination. They could escalate enforcement or stand down at will, and their verification standard shaped what the public record had to say.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_prosecutorial_authority, agenda_setter,
    institutional, generational, mobile, national).

% Ordinary members sustained both manifestos at general conference without prior consultation, were taught that the marriage revelation remained eternal scripture while new plural marriages had ceased, and lived with the whiplash when marriages performed in the interim were later repudiated. They kept their wards, temples, and community throughout, which is what most of them wanted preserved. Leaving meant severing eternal-family theology, tithing-funded social infrastructure, and kin networks across the Intermountain West and the colonies.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, rank_and_file_membership, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, rank_and_file_membership, beneficiary).

% Couples sealed between 1890 and 1904 under apostolic authority in Chihuahua, Alberta, or aboard ship were assured the practice carried sanction. After 1904 the same marriages became liabilities: some participants were asked to dissolve or conceal unions, the leaders who performed them lost their positions, and the families' standing in temple records stayed ambiguous for a generation. The sealing ordinance itself bound them; walking away meant spiritual rupture, and staying meant living under a cloud.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_couples, payer,
    moderate, generational, trapped, continental).

% John W. Taylor, Matthias F. Cowley, George Teasdale, Marriner W. Merrill, Anthony W. Ivins, and colleagues performed or authorized post-Manifesto sealings, some with the First Presidency's knowledge. When the Second Manifesto landed, they were required to abandon plural wives or surrender quorum seats; Taylor and Cowley resigned in 1905-1906 and lived out their lives in partial eclipse. They administered the interim policy and then absorbed the cost of ending it.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, performing_apostles, payer,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, performing_apostles, agenda_setter).

% Wives in households formed before 1890 had no voice in either manifesto or the 1904 policy. Their existing marriages were neither dissolved nor publicly defended; they carried stigma, economic precarity, and their husbands' legal exposure vicariously, and watched the institution negotiate away the principle their family structures rested on. Their households remained in limbo for decades.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, plural_family_women, excluded,
    powerless, generational, trapped, continental).

% Believers who read the preserved revelation as requiring continued practice organized study circles in the 1910s and 1920s, formed a Council of Friends in 1929, and consolidated settlements such as Short Creek by the mid-1930s. Excommunicated for practicing or advocating what the canon still contained, they later faced criminal prosecution. Their identity fused with the principle the institution preserved in print and disowned in policy; leaving the principle meant leaving their understanding of salvation itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters, payer,
    powerless, generational, identity_locked, regional).

% Historians and archivists reconstructing authorization chains, marriage counts, and council deliberations from diaries, minutes, and hearing transcripts. They see the full two-track structure across the entire period and publish outside any ecclesiastical chain of command.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, religious_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, governing_priesthood_hierarchy).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a real reconciliation problem: an unrevisable canonized revelation stood against non-negotiable federal law. The gap let one organization honor both simultaneously — public compliance satisfied the state; preserved doctrine satisfied the tradition's internal continuity claims — keeping temples open, the corporation alive, and the colonization economy functioning while the crisis passed.
% TRANSFER_FUNCTION: Moved interpretive authority and risk downward and inward: marriage-legitimacy determinations shifted from stable published revelation to discretionary presidential policy; the costs of doctrinal contradiction moved from the institutional center to dispersed members (interim couples first, then dissenters), while continuity goods — charter, property, temples, political respectability — accrued to the governing center.
% ABSENT_VOICES: Plural-family women and interim-married wives had no seat in either announcement or the 1904 closure; rank-and-file ratified retroactively at conference; fundamentalist-minded believers were heard only at the moment of expulsion. Federal examiners wanted verified cessation and got testimony instead — the Smoot hearings record shows the verification demand answered with denial rather than disclosure.
% DISAPPEARANCE_RATIONALE: Without the gap, the institution faces an impossible choice in 1890 — repudiate its founding revelation or lose the corporation — so either the church reorganizes around an amended canon or fragments earlier; the interim marriages never happen, the 1904 repudiation never lands, and the fundamentalist churches that define themselves by the preserved section never come into existence. Communities on all sides currently arrange themselves around the gap's residue.
% FOUNDING_PROBLEM: An existential legal crisis: the 1887 Edmunds-Tucker Act dissolved the church's corporation and placed its property in receivership, cohabitation prosecutions filled prisons, and statehood was conditioned on cessation. The arrangement was built to satisfy federal law without formally repudiating Section 132, the revelation underwriting prophetic authority and eternal-family theology.
% FOUNDING_PROBLEM_CORROBORATION: The legal crisis itself is corroborated from outside the benefiting parties by federal court dockets, confiscation records, and Senate hearing transcripts. Whether the underlying doctrinal problem is resolved is disputed: the governing hierarchy attests settlement (harmony between canon and practice), while fundamentalist publications and outside historians (D. Michael Quinn, B. Carmon Hardy, Kathryn Daynes) document the unresolved dual structure — corroboration for the 'contested' verdict comes from sources with no stake in institutional continuity.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored high (0.84 at interval end) because the arrangement's operative function was to keep two contradictory commitments load-bearing at once, and the costs of the contradiction were displaced onto members: couples sealed under interim authorization were repudiated by the same authority that permitted them, and believers who trusted the preserved canon were expelled for trusting it. Suppression (0.80) is authored as a raw structural property and is not scaled by power or scope; its series traces enforcement-capacity buildup — from the passive compliance posture of 1890 through the Second Manifesto's disciplinary machinery to the excommunications of the 1920s — which is precisely the dynamic this story tracks, so a suppression_requirement series is warranted alongside the scalar. Theater (0.65) rises as public-compliance performance (denials, hearing testimony, harmony narratives) substitutes for substantive resolution of the canon-practice contradiction; the 1904 jump marks the Smoot-hearing testimony phase. Accessibility_collapse (0.60): alternatives did not vanish — fundamentalist exit and private disbelief persisted — but mainstream exit collapsed into identity loss. Resistance (0.55): apostolic refusal, member disillusionment, and the fundamentalist organization itself. Claim and metrics are independent authored facts: the claimed type states what the structure is (a working coordination shell carrying asymmetric extraction under active enforcement); the metrics describe how it actually operated. All three series share one ten-point grid (1890-1935) so no metric row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the hierarchy's chair the arrangement is stewardship: a necessary bridge that saved the corporation, the temples, and the community every seat depends on. From the interim couples' chair it is a promise made and withdrawn — authorization given, then repudiated. From the fundamentalist chair it is worse than betrayal: the canon they share is affirmed in print and disowned in policy, so the institution itself becomes the obstacle to its own scripture. From the federal chair the episode reads as compliance achieved and then discovered incomplete. Same events, four incompatible experiences of the arrangement; the engine derives these from power, exit, and directional position rather than from anyone's testimony.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The hierarchy sits nearest the beneficiary pole (declared beneficiary, agenda_setter, arbitrage-grade mobility across jurisdictions). Rank-and-file members are declared victims but retain an incidental coordination benefit — the preserved community — damping their position below the couples'. Interim couples and fundamentalist dissenters sit nearest the target pole: trapped or identity_locked, generational stakes, no compensating flow. Performing apostles are dual-positioned — they administered the interim policy and then paid its closure costs — placing them mid-field. Plural-family women, excluded and trapped, sit high. The federal authority has no beneficiary/victim declaration; it received the formal compliance it demanded, placing it mildly beneficiary-side of symmetric. Continental scope amplifies effective extraction modestly for the target-side seats, since verification across Utah, Chihuahua, Alberta, and the high seas was genuinely hard. No directionality overrides are authored: the structural declarations already separate the seats, and overrides key by power atom, which would collide here (two institutional seats with opposite relationships).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — surviving the Edmunds-Tucker crisis without canon suicide — was substantially solved by 1907: statehood in 1896, property largely restored, a Latter-day Saint seated in the Senate. The arrangement outlived that mandate, and the genealogy interview locates why: closing the gap outright required either canon revision (unthinkable — it would concede the founding revelation was provisional) or consistent enforcement (delivered in 1904 at the price of apostolic resignations and, eventually, schism). The classification prevents mislabeling in both directions: a pure-extraction reading misses the real coordination good (every seat materially depended on institutional continuity), while a pure-coordination reading erases the stranded couples and the expelled believers who paid for the bridge. The tangled-rope claim holds both faces together. Founding_problem_status is authored 'contested' rather than 'dead' because the doctrinal residue is genuinely disputed — the hierarchy attests settlement, the fundamentalist movement attests the opposite, and outside historiography corroborates the dispute rather than either settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the marriage_commitment_reversal kernel best models what happened — external override, internal revelation, or a deliberately maintained structure-gap? This file instantiates the practice_doctrine_gap reading; the sibling readings would assign different causes and different victim attributions.',
    'Cross-reading comparison once the sibling stories are compiled: contrast each reading''s epsilon referent, victim set, and computed per-seat classifications against the same archival record (Woodruff diaries, council minutes, authorization chains).',
    'If the exogenous reading dominates, the arrangement is coercion-shaped and the hierarchy''s agency shrinks; if the endogenous reading dominates, the gap dissolves into harmonious supersession and measured extraction drops; as authored, the gap reading keeps the hierarchy''s management of ambiguity as the operative structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame routing: this story is one reading of a three-reading kernel; classification may shift under sibling readings.').

omega_variable(
    dual_track_intentionality,
    'Was the doctrine-practice gap a designed dual track from 1890 (public line plus privately authorized sealings) or a control failure in which post-Manifesto marriages accumulated until 1904 forced cleanup?',
    'Date and trace authorization for each known post-Manifesto marriage: whose authority, what knowledge at the First Presidency, when disclosed.',
    'A designed dual track raises the hierarchy''s share of responsibility and supports the hybrid coordination-plus-extraction reading; rogue accumulation recasts 1890-1904 as enforcement decay and shifts attribution away from the center.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_track_intentionality, empirical, 'Whether the ambiguity was engineered or emergent.').

omega_variable(
    membership_betrayal_intensity,
    'How widely did the rank-and-file register the gap as betrayal or bewilderment, versus experiencing it as a distant administrative matter concerning a practice most had never entered?',
    'Diaries, correspondence, ward and stake meeting minutes, and newspaper exchanges from 1890-1910 across Utah and the colonies.',
    'Wide diffusion enlarges the victim set and raises the membership seat''s position toward the target pole; narrow diffusion concentrates the harm in interim couples and dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_betrayal_intensity, empirical, 'Diffusion breadth of the betrayal experience among ordinary members.').

omega_variable(
    colony_jurisdiction_legality,
    'Were the post-Manifesto marriages performed in Chihuahua and Alberta actually lawful where performed, as the claimed-legal-jurisdictions framing asserts, or did they violate host-jurisdiction law as well?',
    'Mexican civil-code and Canadian marriage-law analysis plus colonial civil records: registration practice, local official tolerance, prosecutorial history.',
    'If lawful locally, the marriages were jurisdictional arbitrage against US law alone and the gap reads as evasion management; if unlawful everywhere, the dual track manufactured illegality twice over and the extraction characterization hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colony_jurisdiction_legality, empirical, 'Legal status of colony-performed marriages under host-jurisdiction law.').

omega_variable(
    exit_suppression_mechanism,
    'Was member exit suppressed structurally (economic embeddedness, geographic concentration, kin networks) or internalized (testimony-based identity in which leaving equals spiritual self-destruction), and in what proportion?',
    'Post-exit trajectories: compare leavers who relocated outside the Intermountain corridor with fundamentalists who exited into schism while retaining the principle — persistence of distress after physical exit indicates internalized carryover.',
    'Internalized dominance raises effective suppression above the structural measure and strengthens identity-locked coding across the member seats; structural dominance makes suppression responsive to economic diversification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_suppression_mechanism, empirical, 'Structural versus internalized composition of exit suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(marr_tr_t1893, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1893, 0.24).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1896, 0.28).
narrative_ontology:measurement(marr_tr_t1899, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1899, 0.33).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.38).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.46).
narrative_ontology:measurement(marr_tr_t1911, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1911, 0.52).
narrative_ontology:measurement(marr_tr_t1922, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1922, 0.58).
narrative_ontology:measurement(marr_tr_t1929, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1929, 0.62).
narrative_ontology:measurement(marr_tr_t1935, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1935, 0.65).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1893, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1893, 0.6).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1896, 0.63).
narrative_ontology:measurement(marr_be_t1899, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1899, 0.66).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.7).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.74).
narrative_ontology:measurement(marr_be_t1911, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1911, 0.78).
narrative_ontology:measurement(marr_be_t1922, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1922, 0.8).
narrative_ontology:measurement(marr_be_t1929, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1929, 0.82).
narrative_ontology:measurement(marr_be_t1935, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1935, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement(marr_su_t1893, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1893, 0.4).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1896, 0.45).
narrative_ontology:measurement(marr_su_t1899, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1899, 0.52).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.58).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.68).
narrative_ontology:measurement(marr_su_t1911, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1911, 0.72).
narrative_ontology:measurement(marr_su_t1922, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1922, 0.75).
narrative_ontology:measurement(marr_su_t1929, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1929, 0.78).
narrative_ontology:measurement(marr_su_t1935, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1935, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 reversal' conflates three structurally distinct claims (epsilon-invariance principle): what caused the reversal (exogenous coercion vs. internal revelation) and what structure the reversal produced (a durable doctrine-practice gap). Each claim gets its own story with its own epsilon, beneficiaries, and victims; this file authors the structure claim. Family links run through network.affects_constraints; the causal readings sit upstream of this structural reading because all three cite the same events as evidence, and the gap reading changes what each causal reading must explain (a durable dual track, not a one-time suspension).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
