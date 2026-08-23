% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Madhhab Taqlid Obligation (Traditionalist Reading of the Quran-Hadith Substrate)
 *   domain: religious/legal-authority
 *
 * SUMMARY:
 *   The traditionalist reading holds that the Quran and hadith substrate
 *   reached authoritative settlement in the classical fiqh schools: their
 *   converged rulings constitute binding consensus (ijma), and contemporary
 *   Muslims who are not themselves qualified jurists fulfill their
 *   obligations by following (taqlid) an established madhhab rather than
 *   reasoning independently. The arrangement solves a real problem at scale —
 *   nearly two billion believers receive consistent answers on ritual,
 *   family, commerce, and worship without a central church — and the same
 *   structure concentrates interpretive authority in a credentialed scholarly
 *   class whose position, income, and institutional continuity depend on the
 *   obligation's persistence. Costs fall unevenly: progressive Muslims face
 *   sanction and exclusion for rereading, women encounter classical
 *   family-law rulings on testimony, divorce, guardianship, and inheritance
 *   defended as divinely fixed, and minorities live under dhimmi-derived
 *   frameworks they have no seat in maintaining. Enforcement is active:
 *   heresy-adjacent accusation, pulpit and institutional exclusion, and — in
 *   traditionalist-dominant states — codified personal-status law
 *   administered by religious courts. This file instantiates ONE reading of
 *   the quran_hadith_substrate kernel; reformist_ijtihad and state_hybrid are
 *   separate constraints with their own beneficiary/victim structures and
 *   their own burden levels, linked through the network section. The claim
 *   and the metrics are independent authored facts: claimed_type is stated
 *   from structural analysis (real coordination function entangled with
 *   asymmetric, actively enforced extraction), and the metric values describe
 *   the arrangement's observed operation without being tuned to any predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - senior_madhhab_muftis: agenda-setting enforcers (institutional / identity_locked) — head fatwa councils, certify who may teach, declare deviation; the office is constituted entirely within the system
 *   - ulama_scholarly_class: primary beneficiary with administering duties (organized / identity_locked) — collects deference, stipends, teaching and arbitration income through the taqlid economy
 *   - madhhab_institutions: institutional beneficiaries (institutional / identity_locked) — seminaries, juridical academies, fatwa councils whose enrollment and funding ride on the obligation
 *   - mosque_endowment_hierarchies: institutional beneficiaries (institutional / constrained) — waqf boards and mosque networks administering flows routed through scholarly oversight
 *   - traditional_family_patriarchs: household-level beneficiaries (moderate / constrained) — classical rulings allocate family decision rights to senior male kin
 *   - ordinary_practicing_believers: dual-positioned mass seat (moderate / identity_locked) — receive ready-made authoritative answers; pay deference and diffuse support
 *   - progressive_muslims: primary targets (moderate / trapped) — sanctioned for rereading; exit means community and kinship rupture
 *   - women_under_classical_family_rulings: primary targets (moderate / constrained) — bear the family-law rulings' costs, jurisdiction-dependent from codified law to communal pressure
 *   - dhimmi_framework_minorities: primary targets (powerless / trapped) — bear differential intercommunal burdens with no seat in the councils maintaining the rules
 *   - reformist_jurists: excluded voices (moderate / mobile) — trained in legal theory, barred from official councils, speaking from outside the establishment
 *   - state_religious_courts: enforcing administrators in traditionalist-dominant states (institutional / constrained) — administer codified classical family law; jurisdiction depends on the arrangement's persistence
 *   - comparative_jurisprudence_scholars: analytical observers (analytical / analytical) — document the closure narrative's formation and cross-era school practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.78).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Madhhab Taqlid Obligation (Traditionalist Reading of the Quran-Hadith Substrate)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal-authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'c73b9457-b933-4b25-816d-a952c902863a').
narrative_ontology:cs_kernel_codification('c73b9457-b933-4b25-816d-a952c902863a', fixed_text).
narrative_ontology:cs_authority_grounding('c73b9457-b933-4b25-816d-a952c902863a', lineage).
narrative_ontology:cs_interpretation_layer_present('c73b9457-b933-4b25-816d-a952c902863a').
narrative_ontology:cs_reading_relation('c73b9457-b933-4b25-816d-a952c902863a', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('c73b9457-b933-4b25-816d-a952c902863a', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('c73b9457-b933-4b25-816d-a952c902863a', foundational, ijma_closure_confers_binding_authority).
narrative_ontology:cs_axiom_status(ijma_closure_confers_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('c73b9457-b933-4b25-816d-a952c902863a', ijma_closure_confers_binding_authority, theological).
narrative_ontology:cs_axiom('c73b9457-b933-4b25-816d-a952c902863a', foundational, taqlid_obligatory_for_non_mujtahids).
narrative_ontology:cs_axiom_status(taqlid_obligatory_for_non_mujtahids, holdable).
narrative_ontology:cs_axiom_grounding('c73b9457-b933-4b25-816d-a952c902863a', taqlid_obligatory_for_non_mujtahids, instrumental).
narrative_ontology:cs_reference_frame('c73b9457-b933-4b25-816d-a952c902863a', classical_madhhab_consensus_settlement).
narrative_ontology:cs_drift_state('c73b9457-b933-4b25-816d-a952c902863a', contemporary_postcolonial_mass_education_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c73b9457-b933-4b25-816d-a952c902863a', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_endowment_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_family_patriarchs).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_under_classical_family_rulings).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, dhimmi_framework_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ordinary_practicing_believers).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, state_religious_courts).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, ordinary_practicing_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Head the fatwa councils and madhhab academies, certify who may teach law, and pronounce on deviation from settled doctrine. Their moral authority, office, and biography exist entirely inside the system they administer; stepping outside it would annul the standing that makes their voice count.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, senior_madhhab_muftis, agenda_setter,
    institutional, generational, identity_locked, global).

% Teach, issue rulings, arbitrate disputes, and staff the institutions of learning. Deference, stipends, endowment income, teaching and arbitration fees, and social standing all flow to them through the obligation of lay following. Leaving the role would cost livelihood, community position, and religious identity at once.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class, agenda_setter).

% Seminaries, juridical academies, and fatwa councils whose enrollment, endowments, and continuity depend on the obligation remaining in force. Over centuries each institution has become identified with its school's doctrine; dissolving the obligation would dissolve the institution's reason to exist.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Waqf boards and mosque networks that administer charitable and endowment flows under scholarly oversight. Their revenues and staffing are tied to continued doctrinal gatekeeping; they defend the arrangement because their operating budget rides on it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_endowment_hierarchies, beneficiary,
    institutional, generational, constrained, continental).

% Senior male kin in households governed by classical family rulings on guardianship, consent, divorce initiative, and inheritance shares. The rulings allocate household decision rights to them; they uphold the rulings as divinely fixed order and resist revision as family dishonor.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_family_patriarchs, beneficiary,
    moderate, generational, constrained, regional).

% Receive ready-made, authoritative answers for daily ritual, diet, finance, and family questions — genuine convenience and certainty they did not have to earn through years of legal training. They pay in deference to scholarly authority and diffuse material support for the institutions, and their faith identity makes leaving the framework unthinkable regardless of specific grievances.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ordinary_practicing_believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, ordinary_practicing_believers, payer).

% Believers who seek ethically grounded rereading of inherited rulings. They face accusations of innovation or disbelief, exclusion from pulpits, teaching posts, and marriage networks, and rupture with family. Staying silent costs them conviction; speaking costs them community; formally leaving costs them identity, kinship, and in some places physical safety.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, trapped, global).

% Live under rulings on testimony weight, initiation of divorce, marital guardianship, and inheritance shares. Where states codify these into personal-status law the costs are legal and enforced by courts; elsewhere they are communal and enforced by family and congregation. Exit ranges from costly litigation to unavailable, and internal voice is limited by the same rulings that assign them lesser procedural standing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_under_classical_family_rulings, payer,
    moderate, biographical, constrained, global).

% Religious minorities living where classical frameworks govern intercommunal relations — historically poll-tax and restriction regimes, and their contemporary analogues in parts of the traditionalist-dominant world. They bear differential legal burdens and have no seat in the councils that maintain the rules applying to them.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, dhimmi_framework_minorities, payer,
    powerless, generational, trapped, regional).

% Scholars trained in the same legal theory who argue for reopening independent reasoning where inherited rulings conflict with contemporary ethical knowledge. They are barred from official councils, labeled deviant from the establishment's platforms, and publish through universities, media, and civic organizations outside it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_jurists, excluded,
    moderate, biographical, mobile, global).

% In traditionalist-dominant states, religious courts and muftiates administer codified classical family and personal-status law. Their jurisdiction, budgets, and staffing exist because the arrangement holds; officials who questioned its foundations would forfeit their posts.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_religious_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, state_religious_courts, beneficiary).

% Academic historians and legal theorists who document how the closure narrative formed, compare pre-modern school practice across eras, and map where formal doctrine and lived practice diverge. They hold no stake in the outcome and serve as the outside corroborating seat for the genealogy interview.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, comparative_jurisprudence_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains interpretive unity and legal coherence for a geographically dispersed community of nearly two billion without a central church: standardized answers to recurring ritual, family, commercial, and worship questions; validated transmission of law across generations through certified teacher chains; dispute resolution by reference to settled school doctrine instead of ad hoc individual judgment.
% TRANSFER_FUNCTION: Moves interpretive authority and deference from lay believers to the credentialed scholarly class; moves material support (endowment income, stipends, fees, state salaries in traditionalist-dominant states) toward madhhab institutions and their networks; moves household decision rights in marriage, divorce, guardianship, and inheritance toward senior male kin and scholarly arbiters.
% ABSENT_VOICES: Reformist jurists, Muslim feminist jurisprudence collectives, and lay believers who chafe at specific rulings are present only as objects of ruling, never as co-authors of doctrine; minority communities subject to dhimmi-derived rules have no seat in the councils that maintain those rules. They speak from universities, media, and civic organizations outside the establishment — which is precisely where the enforcement boundary places them.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation vanished overnight, daily practice across the community would lose its adjudication channel — prayer, fasting, finance, and family questions would fragment among competing interpretive entrepreneurs; the scholarly class's livelihood and status would collapse; family-law regimes in several states would lose their doctrinal grounding and face immediate constitutional renegotiation; and the excluded voices would move from the margin to the center of a wide-open interpretive field.
% FOUNDING_PROBLEM: After the Prophet's death the community faced an unbounded stream of novel situations with no living legislator: which reports of his practice were authentic, how to derive rulings from the revealed texts, and how to prevent arbitrary private judgment from fragmenting the law. The madhhab system and the consensus doctrine were built to validate and transmit revealed law across generations without a central church.
% FOUNDING_PROBLEM_CORROBORATION: The validation problem's original reality is attested from outside the benefiting parties by academic Islamic-studies scholarship, which documents both the genuine early need and the later construction of the closure narrative; reformist jurists attest from outside the establishment that the problem remains open and the closure claim false; state legal-modernization commissions in several countries attest partial obsolescence by having already moved commercial and criminal law to non-madhhab codes. No corroboration exists for the strong claim that consensus was completed and the reasoning door shut — that claim is asserted only by the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68: interpretive authority and family-law decision rights are concentrated in a hereditary-credential class, and the rate of deference owed is decoupled from any measurable service delivered to the payer seats — the signature of an arrangement operating above its coordination cost. Suppression is authored at 0.78 as a RAW STRUCTURAL PROPERTY, unscaled by power or scope: the arrangement's persistence depends on actively foreclosing rival readings (accusation of innovation or disbelief, exclusion from pulpits and councils, and codified enforcement in traditionalist-dominant states), not on voluntary preference alone. Theater ratio is 0.30: the majority of activity is functional (teaching, fatwa issuance, adjudication, ritual standardization), while a growing minority is performative — invoking consensus ceremonially in settings where the schools internally disagree, and rehearsing closure narratives that historical practice contradicts. Accessibility collapse is 0.60, deliberately below mountain-range values: alternatives persist and are reachable — Salafi currents that reject madhhab binding altogether, reformist seminaries, digital fatwa pluralism — but each carries heavy social cost inside traditionalist communities. Resistance is 0.55: organized reform movements, feminist jurisprudence collectives, and state modernization programs actively contest the arrangement, and notably the strongest internal challenge to madhhab binding comes from ultra-traditionalist quarters, so resistance is not a single ideological flank. The measurement series run on ONE shared grid (points 0, 20, 40, 60, 80, 100 of a century-scale interval) with all three tracked metrics authored at every point; trajectories are monotonic, not cyclical — colonial-era codification stripped commercial and criminal jurisdiction early (lower starting burden), after which the surviving family-law and personal-status domain became the concentrated site of authority defense, enforcement hardened as literacy and broadcast media multiplied rival voices, and ceremonial consensus-invocation crept upward.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the senior mufti seat, the arrangement is a sacred trust faithfully transmitted: the obligation protects laypeople from error and the law from fragmentation, and enforcement is pastoral care. From the ordinary believer seat, it is mostly benign coordination — ready answers, ritual certainty, belonging — with diffuse costs rarely itemized. From the woman-under-family-rulings seat, the reformer seat, and the minority seat, the same structure operates as enforced subordination and exclusion, with exit priced in kinship, community, and sometimes physical safety. The engine computes these per-seat classifications from the authored power, exit, and role data; nothing in the claimed_type adjudicates between them, and the divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the ulama class, madhhab institutions, and endowment hierarchies sit near the full-beneficiary end (low d), with identity_locked exit amplifying their stake — their professional and religious selves are constituted by the arrangement, so effective burden on them inverts toward subsidy. Traditional family patriarchs sit low-to-moderate: they collect decision rights without administering the system. Ordinary practicing believers carry a dual declaration (beneficiary with secondary payer position) and should derive near-symmetric: genuine coordination benefit tempered by diffuse cost and total identity lock-in. The payer seats derive high: progressive Muslims (trapped exit pushes toward the full-target end), women under classical family rulings (constrained exit, jurisdiction-dependent), and dhimmi-framework minorities (powerless and trapped — maximal effective burden). Reformist jurists are authored as excluded rather than payer: they stand outside the conversation the arrangement permits, and per the R3 ruling their absence is commentary-grade evidence, never a classification override. No directionality overrides are declared: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — validating revealed law for novel cases after the death of the living legislator, without a central church — remains partially live: new situations (biomedicine, finance, digital life) continually arrive and the schools do issue rulings on them. What is contested is the closure claim: whether consensus was ever completed such that the reasoning door shut. Because founding_problem_status is authored 'contested' and disappearance_verdict 'world_rearranges', the mismatch consumer finds no dead-problem-plus-rearranged-world signature; mandatrophy is NOT resolved and the arrangement is not a zombie — it still performs its function daily at scale. The tangled_rope classification is what prevents mislabeling in both directions: a pure-extraction reading would erase the real coordination good that billions of believers consume willingly and that no rival mechanism currently delivers at comparable scale; a pure-coordination reading would erase the documented, actively enforced costs borne by women, minorities, and reformers. Coalition potential among payer seats is real and growing — progressive movements and women's jurisprudence collectives have begun coordinating across borders — which is the structural path by which the payer side's effective power could rise and the current asymmetry narrow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (traditionalist_taqlid) of the quran_hadith_substrate kernel; would instantiating the sibling readings (reformist_ijtihad, state_hybrid) change the structural classification?',
    'Cross-reading comparison of victim sets, beneficiary sets, and enforcement surfaces across the three sibling stories: the disagreement is located in whether interpretive closure occurred, who may derive rulings, and what grounds legitimacy (doctrinal fidelity versus political sovereignty).',
    'Under reformist_ijtihad the victim set contracts sharply (women and minorities gain standing, the ulama lose gatekeeping position) and measured burden falls well below this reading''s level; under state_hybrid enforcement becomes selective and political rather than doctrinal, dispersing the beneficiary seat toward state organs. This file authors only the traditionalist reading; the deltas belong to the sibling files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: kernel membership, sibling readings, and location of the structural disagreement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissenting readings structural (takfir-adjacent sanction, institutional exclusion, state court enforcement) or internalized (believers'' trained self-conception that independent questioning signals weak faith)?',
    'Post-exit suppression trajectory: track whether doubt and reinterpretation remain costly for people who have physically left traditionalist-dominant communities; if the cost persists after enforcement contact ends, a substantial share is internalized.',
    'If internalization carries a large share, effective suppression exceeds the structural measure and outlasts any institutional relaxation; enforcement decay alone would not reopen interpretive space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in religious-identity enforcement.').

omega_variable(
    ijma_closure_historicity,
    'Was interpretive closure ever actually achieved as a matter of historical practice (a genuine completed consensus), or is the closure narrative a later construction laid over schools that continued internal reasoning?',
    'Historiographic analysis of pre-modern legal practice: documented post-classical ijtihad within the schools, differing school positions presented as equally orthodox, and the genealogy of the closure claim in legal-theoretical literature.',
    'If closure is constructed, the taqlid obligation rests on a false historical premise, which raises the arrangement''s effective burden above what a genuine-consensus framing would imply and strengthens the reformist sibling''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_closure_historicity, empirical, 'Historicity of the consensus-closure premise underlying the taqlid obligation.').

omega_variable(
    gain_capture_concentration,
    'Do the gains of the arrangement concentrate in the ulama class and its institutions, or diffuse across the wider traditionalist social order?',
    'Trace material and status flows: endowment income, state salaries, teaching and arbitration fees, and deference goods, against the distribution of costs across payer seats.',
    'Concentrated capture in a named seat supports a capture-dominated reading of the arrangement; genuinely diffuse gains would shift the picture toward a broadly-shared coordination order with incidental asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_concentration, empirical, 'Whether receipt of the arrangement''s gains is concentrated or diffuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.22).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.24).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.26).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.28).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.29).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(qura_be_t100, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal authority' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of the quran_hadith_substrate kernel — traditionalist_taqlid (this file), reformist_ijtihad, and state_hybrid. Each has its own beneficiary/victim structure, its own enforcement surface, and its own stable burden level; forcing one story to span all three would make the measured burden observer-relative, which the framework forbids. The upstream/downstream structure runs through enforcement competition: this reading's institutional dominance shapes the legitimacy conditions under which the sibling readings operate (reformist jurists are excluded by this reading's gatekeeping; state hybrids seek this reading's endorsement for the classical rulings they retain), so this file links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
