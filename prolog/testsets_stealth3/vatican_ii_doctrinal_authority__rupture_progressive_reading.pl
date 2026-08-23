% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Spirit-of-the-Council Authorization (Progressive Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   After the Second Vatican Council (1962-1965), a governing authorization
 *   took shape in Catholic institutional life: the Council's 'spirit' — its
 *   discerned intent — was held to license reform beyond what the sixteen
 *   promulgated texts explicitly mandate. Under this authorization, doctrinal
 *   developments such as the Declaration on Religious Freedom were received
 *   as legitimate reversals of prior magisterial positions (notably condemned
 *   propositions of the Syllabus of Errors), deliberately ambiguous passages
 *   were read as intentional openings for further development, and
 *   post-conciliar implementation was treated as the authentic realization of
 *   conciliar intent rather than as fallible administrative choice. The
 *   arrangement coordinated a global church through a period of
 *   self-described rupture with its own recent past: it supplied a unifying
 *   narrative, licensed wholesale liturgical change, opened ecumenical
 *   engagement, and redistributed governing discretion toward bishops'
 *   conferences and theological advisors. It also imposed heavy, actively
 *   enforced costs on those attached to the pre-conciliar forms — suppressed
 *   liturgical usage, reordered religious institutes, and disciplined dissent
 *   in both the traditionalist and the radical-reform directions. The
 *   claim/metric independence rule applies: claimed_type records my
 *   structural judgment; the metrics record the arrangement's actual
 *   operation as this reading assesses it. KEY AGENTS (by structural
 *   relationship): - roman_magisterium_offices: Agenda-setting center
 *   (institutional/constrained) — adjudicates authorized reform, draws the
 *   enforcement boundary in both directions, collects interpretive authority
 *   - progressive_theological_establishment: Primary beneficiary
 *   (institutional/identity_locked) — careers and institutions fused with the
 *   beyond-text authorization - diocesan_episcopal_conferences: Administrator
 *   and secondary beneficiary (institutional/constrained) — converts the
 *   authorization into local policy, absorbs pressure from both sides -
 *   traditionalist_clergy_and_laity: Primary target (organized/constrained) —
 *   bears suppression of their liturgical and doctrinal forms -
 *   pre_conciliar_religious_orders: Target (moderate/identity_locked) —
 *   charisms dissolved by enforced self-reform - radical_reform_theologians:
 *   Secondary target (moderate/constrained) — disciplined for overdrawing the
 *   same authorization - vernacular_liturgy_laity: Near-symmetric participant
 *   (moderate/mobile) — receives the reforms, bears diffuse costs -
 *   ecumenical_partner_churches: External beneficiary (organized/mobile) -
 *   minority_council_fathers: Excluded voice (institutional/trapped) —
 *   predicted the exploitation of ambiguity, held no post-conciliar office -
 *   academic_ecclesiologists: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.74).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Spirit-of-the-Council Authorization (Progressive Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '34738a31-7eda-41b4-ac6f-8abc782d3156').
narrative_ontology:cs_kernel_codification('34738a31-7eda-41b4-ac6f-8abc782d3156', fixed_text).
narrative_ontology:cs_authority_grounding('34738a31-7eda-41b4-ac6f-8abc782d3156', lineage).
narrative_ontology:cs_interpretation_layer_present('34738a31-7eda-41b4-ac6f-8abc782d3156').
narrative_ontology:cs_reading_relation('34738a31-7eda-41b4-ac6f-8abc782d3156', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('34738a31-7eda-41b4-ac6f-8abc782d3156', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('34738a31-7eda-41b4-ac6f-8abc782d3156', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('34738a31-7eda-41b4-ac6f-8abc782d3156', foundational, conciliar_intent_exceeds_textual_letter).
narrative_ontology:cs_axiom_status(conciliar_intent_exceeds_textual_letter, holdable).
narrative_ontology:cs_axiom_grounding('34738a31-7eda-41b4-ac6f-8abc782d3156', conciliar_intent_exceeds_textual_letter, theological).
narrative_ontology:cs_axiom('34738a31-7eda-41b4-ac6f-8abc782d3156', foundational, post_conciliar_implementation_realizes_authentic_intent).
narrative_ontology:cs_axiom_status(post_conciliar_implementation_realizes_authentic_intent, holdable).
narrative_ontology:cs_axiom_grounding('34738a31-7eda-41b4-ac6f-8abc782d3156', post_conciliar_implementation_realizes_authentic_intent, conventional).
narrative_ontology:cs_reference_frame('34738a31-7eda-41b4-ac6f-8abc782d3156', conciliar_event_as_normative_new_beginning).
narrative_ontology:cs_drift_state('34738a31-7eda-41b4-ac6f-8abc782d3156', contemporary_post_traditionis_custodes, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('34738a31-7eda-41b4-ac6f-8abc782d3156', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, diocesan_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, vernacular_liturgy_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partner_churches).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, radical_reform_theologians).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, spirit_of_council_hermeneutic).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, aggiornamento_principle).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, living_tradition_open_ended_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope and the doctrinal and liturgical congregations adjudicate what the Council's intent authorizes, draw the boundary of permitted reform in both directions, and grant or withdraw canonical arrangements for older forms of worship. Through that adjudication the office accumulates interpretive authority; its occupants cannot exit short of death or abdication.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_magisterium_offices, agenda_setter,
    institutional, generational, constrained, global).

% Translate the authorization into local liturgical, catechetical, and disciplinary policy. They gained substantial discretion over worship and governance after the Council, and they also absorb pressure from Rome above them and from disaffected clergy and faithful below them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, diocesan_episcopal_conferences, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, diocesan_episcopal_conferences, beneficiary).

% Theologians, liturgists, periti, and advisors whose chairs, journals, and commissions were built on the conciliar-renewal project. Their professional standing is fused with the claim that the Council's meaning exceeds its letter; relinquishing that claim would dissolve the institutional identity their careers are made of.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Attend reformed liturgy in local languages, serve in lay ministries created after the Council, and received the reforms as gifts rather than as their own imposition. They are free to attend or leave, and across the decades large numbers did both.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, vernacular_liturgy_laity, beneficiary,
    moderate, biographical, mobile, global).

% Protestant and Orthodox bodies that gained dialogue channels, common prayer, and doctrinal rapprochement unavailable before the Council. Their engagement continues on the strength of the conciliar opening, and they can withdraw from it without canonical consequence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partner_churches, beneficiary,
    organized, generational, mobile, global).

% Priests and faithful attached to the pre-conciliar liturgy and doctrinal corpus. They lost regular access to their forms of worship, saw institutes suppressed or restructured, and can secure those forms only through canonical arrangements granted or withdrawn at the center's discretion. Leaving communion entirely forfeits the sacramental order they recognize; remaining means accepting the terms offered.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity, payer,
    organized, generational, constrained, global).

% Religious institutes whose constitutions, habits, and apostolates were reformed, sometimes against their own statutes, under the implementation decrees. Their charisms were constituted by the very forms the reform displaced: refusal meant dissolution, compliance meant becoming a different community than the one their founders established.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_orders, payer,
    moderate, generational, identity_locked, global).

% Theologians who took the authorization as license without limit and pressed reform beyond what the magisterium would sanction. They were censured, silenced, or stripped of teaching faculties when their proposals outran the boundary the center drew around its own authorization.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, radical_reform_theologians, payer,
    moderate, biographical, constrained, global).

% Council fathers of the minority bloc who voted against key schemas and warned during the debates that deliberately ambiguous phrasing would be exploited after the Council. Once the documents were promulgated they held no standing office in interpretation, and several were sidelined when they resisted implementation. As consecrated bishops they could not leave communion to escape the outcome.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, minority_council_fathers, excluded,
    institutional, generational, trapped, global).

% Historians and scholars of the Council who work from the drafting archives, voting records, and reception history. They attest what the texts and their drafting process involved without holding any office in the arrangement they study.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, academic_ecclesiologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_magisterium_offices).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single hermeneutic by which a global institution could implement sixteen internally contested documents coherently across thousands of dioceses: ambiguous passages are resolved into actionable policy by appeal to discerned conciliar intent, giving clergy and laity one shared narrative of renewal during a period of rapid, self-described rupture with the immediate past.
% TRANSFER_FUNCTION: Moves interpretive authority and governing discretion from fixed textual norms to the living magisterium and its accredited interpreters; moves liturgical and doctrinal forms out of the hands of their traditional holders; and transfers career security, institutional standing, and canonical legitimacy to those aligned with the invoked spirit of the Council.
% ABSENT_VOICES: The minority council fathers who predicted that ambiguous phrasing would be exploited held no office in post-conciliar interpretation, and their warnings were never given equal institutional standing. Lay faithful attached to the traditional liturgy had no consultative seat when the liturgical reform was designed by expert-clerical bodies; the people whose worship was replaced learned of the change by decree.
% DISAPPEARANCE_RATIONALE: If the spirit-authorization vanished overnight, implementation would collapse back to adjudication of the texts as written: liturgical arrangements would lose their legitimating narrative, episcopal discretion would contract to what the documents explicitly permit, the progressive establishment's mandate would evaporate, and traditionalist communities would renegotiate their position from a fundamentally different baseline. Six decades of institutional practice are organized around the authorization; its removal rearranges all of them.
% FOUNDING_PROBLEM: The Council had to pass deeply contested schemas over a divided episcopate: drafters engineered supermajorities by leaving disputed questions (religious liberty, revelation, liturgy) in deliberately ambiguous phrasing, and the resulting texts could not carry unambiguous mandates without splitting the Council. The authorization was built to solve the problem of how to implement texts whose clarity had been sacrificed to passage.
% FOUNDING_PROBLEM_CORROBORATION: The consensus-securing problem ended when the Council closed in 1965; what persists is the authorization as a standing interpretive license. Council historians working from the drafting archives (the Alberigo and O'Malley schools) document the deliberate-ambiguity strategy from outside every benefiting seat; senior figures within the hierarchy itself have publicly diagnosed the gap between the texts and the invoked spirit; and the communities that bore the implementation's costs attest the burden from their own position. No corroboration for the problem's continued liveness comes from outside the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the authorization decouples doctrinal and liturgical authority from the fixed texts: whatever the enforcement boundary happens to be on a given day, the mechanism that sets it is the adjudicating center's appeal to intent rather than the governed parties' appeal to the documents, and the costs of each redrawing land on those attached to the displaced forms. Suppression is higher still (0.74) because the arrangement's persistence depends on active canonical enforcement — prohibition of liturgical usage, restructuring of institutes against their statutes, censure of theologians in both directions — not on voluntary uptake. Theater ratio is moderate-low (0.31): the reform activity was and is substantially real (vernacular liturgy, ecumenical dialogue, episcopal collegiality all operate), but a recurring share of 'conciliar renewal' discourse functions as ritual affirmation detached from further change, rising when rhetoric outruns practice and falling when practice is intense. Accessibility collapse is low-moderate (0.38): text-bound implementation and the traditional forms remain visibly available and practiced, so understanding the authorization does not close off alternatives. Resistance is high (0.66): organized traditionalist communities, recalcitrant institutes, and scholarly contestation have persisted for six decades and periodically force retrenchment. The referent of epsilon is the standing authorization arrangement itself, assessed by this reading's own lights: this reading endorses the break as necessary and still measures the arrangement as having drawn heavily from those who bore the transition — endorsement of the rupture and acknowledgment of its costs are compatible positions, and the magnitude, not the justification, is what the metric records. Suppression is authored as a raw structural property and is not scaled by context; extractiveness is scaled by the engine from directionality and scope, and the arrangement's global scope amplifies effective extraction by making enforcement outcomes hard to verify locally. The temporal series run on one shared ten-point grid (all three metrics authored at every point) and show a full oscillation: an implementation surge peaking in the mid-1970s, a long accommodation trough through the John Paul II era (partial indult arrangements, enforcement redirected toward radical overdraw), and a renewed ascent culminating in the revocation of liturgical accommodations in 2021 and after. The cycle tracks pontificate alternation; whether the oscillation is personalistic or structural to the authorization's design is carried as an omega below.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat compute differently from the same structure. From the magisterium's position the authorization is necessary governance: without a living interpreter the contested texts could not be administered at all, and policing the boundary in both directions is fidelity, not coercion. From the traditionalist and old-form institute seats the same structure operates as enforced displacement: their forms were not argued out of existence but proscribed, and the terms of their partial restoration are grants revocable at will. The identity-lock dynamics differ by seat: for the progressive establishment the lock is professional (careers, chairs, and journals constituted by the beyond-text claim — if the frame breaks, the credentialing structure built on it breaks); for the reformed institutes it is charismatic-institutional (the community's constitution WAS the pre-conciliar form, so compliance and dissolution of identity were the same act). A third seat diverges quietly: radical reform theologians accepted the authorization's premise and were still censured — from their position the enforcement proves the license was always bounded by the licensor's discretion, not by the spirit's own logic. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Traditionalist clergy and laity and the pre-conciliar institutes sit near the full-target end: they bear the transfer and their exits are constrained or identity-bound, which pins them toward maximal effective extraction. The progressive establishment sits near the beneficiary end with identity_lock amplifying its investment in the arrangement's persistence. Episcopal conferences derive low-to-moderate d from their beneficiary declaration, tempered by their administrator position between Rome and the disaffected. Vernacular-liturgy laity sit near symmetric: genuine received benefit, diffuse indirect costs, mobile exit. Ecumenical partners are beneficiaries with arbitrage-grade exit (they can disengage without canonical loss), placing them nearest the subsidized end. The magisterium is not declared a beneficiary — it is the agenda-setter — but the gains demonstrably accrue to it: interpretive authority is the arrangement's principal product and it lodges in the adjudicating office, which is why gain_flow names that seat rather than 'diffuse'. No directionality overrides are used: the derivation from declarations plus exit options already separates the seats correctly, and the override mechanism keys on power atoms, which would cross-contaminate seats sharing a power level (the moderate-power victims and the moderate-power laity would be forced onto one d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing passage for contested texts and then legitimating their implementation — was solved by 1965-1970. The authorization persists as a standing interpretive license, which is precisely the mandatrophy signature: the arrangement's founding_problem_status is dead while its disappearance verdict is world_rearranges, a mismatch the engine's consumption rule reads as a capture/zombie flag cross-checked against the computed theater path. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure coordination (a rope) erases the identifiable victims — suppressed rites, dissolved charisms, censured theologians — and the enforcement machinery their suppression requires; reading it as pure extraction (a snare) erases the genuine coordination achievement — a global institution implemented internally contested texts coherently, opened ecumenical engagement, and avoided schism on its reforming flank during the largest liturgical change in its history. Both facts are structural; the hybrid category holds them together. Whether the authorization can be retired now that its founding function is spent, or whether the institution has become structurally dependent on open-ended interpretive authority, is the live question the omega variables carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the rupture_progressive_reading of kernel vatican_ii_doctrinal_authority. How much of the measured extraction profile is contingent on that hermeneutic choice rather than on the conciliar settlement itself — what would a sibling reading instantiate differently over the same referent?',
    'Author and compile the sibling reading stories over the same kernel and interval; compare epsilon, victim sets, enforcement profiles, and seat structures across the kernel family.',
    'If sibling readings yield materially different epsilon over the same referent, the kernel''s classification is indexical to hermeneutic choice and no single type can be assigned to ''the Council''s doctrinal authority'' as such; if the profiles converge, the extraction is robust across readings and the kernel itself carries the measured structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame contingency: extraction profile may be a property of this reading, not of the kernel.').

omega_variable(
    deliberate_ambiguity_genealogy,
    'Were the conciliar texts'' ambiguities intentionally crafted openings for development (as this reading''s authorization presumes) or artifacts of diplomatic compromise reached to secure supermajorities?',
    'Archival study of the drafting commissions: relatio, successive schemata, recorded modi and vote tallies, and commission minutes comparing drafting intent against post-conciliar interpretive use.',
    'If compromise-artifacts, the beyond-text authorization rests on a misdescription of drafting intent and its legitimating genealogy fails; if intentional openings, the authorization has stronger textual warrant and part of the measured extraction is attributable to the drafters'' own design choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_ambiguity_genealogy, empirical, 'Whether the textual openness the authorization rides on was designed or incidental.').

omega_variable(
    enforcement_symmetry,
    'Does the enforcement machinery police dissent symmetrically (traditionalist retention AND radical overdraw) or asymmetrically?',
    'Tabulate censures, faculty withdrawals, and liturgical prohibitions by target class across the interval; compare rates and severity against the target classes'' sizes.',
    'If enforcement concentrates on one class, extraction concentrates accordingly and the arrangement trends toward the pure-extraction profile; if symmetric, the enforcement is better described as boundary maintenance around a discretionary license, and the victim set is genuinely bidirectional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry, empirical, 'Whether the enforcement boundary is drawn evenly around the authorization.').

omega_variable(
    authorization_cycle_driver,
    'Is the observed oscillation in extraction and suppression driven by pontificate alternation (personalistic) or by a structural alternation inherent to the authorization''s design?',
    'Compare cycle phase against pontificate boundaries versus against internal institutional variables (seminary cohorts, curial personnel continuity, financial pressures) independent of the occupant of the office.',
    'If structural, the oscillation itself is part of the mechanism — intermittent reinforcement of accommodations followed by revocation deepens dependence on the center''s discretion and should weight the classification; if personalistic, the arrangement''s baseline is flatter than the series suggests and the endpoints overstate the steady-state profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_cycle_driver, empirical, 'Source of the cyclical drift in the temporal measurements.').

omega_variable(
    spirit_text_separability,
    'Is the coordination function — coherent global implementation of internally contested texts — separable from the beyond-text authorization, or does implementation coherence require the open-ended license?',
    'Compare dioceses and institutes that implemented strictly text-bound against those implementing by invoked intent: measure implementation coherence, conflict rates, and retention of communion across the two regimes.',
    'If separable, the authorization component is removable overhead riding on genuine coordination and the arrangement could be repaired by returning to the texts; if inseparable, part of the measured extraction is the irreducible price of the coordination the arrangement performs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spirit_text_separability, conceptual, 'Whether the coordination and the discretionary-license components can be structurally separated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vati_tr_t1969, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1969, 0.22).
narrative_ontology:measurement(vati_tr_t1976, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1976, 0.28).
narrative_ontology:measurement(vati_tr_t1983, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1983, 0.34).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(vati_tr_t1997, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1997, 0.42).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2013, 0.36).
narrative_ontology:measurement(vati_tr_t2019, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2019, 0.33).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1962, 0.25).
narrative_ontology:measurement(vati_be_t1969, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1969, 0.55).
narrative_ontology:measurement(vati_be_t1976, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1976, 0.68).
narrative_ontology:measurement(vati_be_t1983, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1983, 0.62).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(vati_be_t1997, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(vati_be_t2019, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(vati_su_t1969, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1969, 0.55).
narrative_ontology:measurement(vati_su_t1976, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1976, 0.72).
narrative_ontology:measurement(vati_su_t1983, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1983, 0.68).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(vati_su_t1997, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1997, 0.58).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(vati_su_t2019, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% 'Vatican II's doctrinal authority' is a single contested kernel — the Council event and its sixteen fixed texts as the locus of interpretive legitimacy — that decomposes into distinct constraint stories per the epsilon-invariance principle: each hermeneutic reading instantiates a different arrangement with its own epsilon, beneficiary/victim structure, and enforcement profile. This file authors ONLY the rupture_progressive_reading (spirit authorizes reform beyond textual limits; implementation as realized intent; high measured extraction from holders of the pre-conciliar forms). The continuity reading (organic development, no genuine novelty) and the rupture-traditionalist reading (rupture as infidelity, ambiguities as defects) instantiate different victim sets and different enforcement objects over the same texts, and the composite-overdetermination reading denies the unified-shift framing altogether. Family members link via network.affects_constraints; upstream/downstream citation pressure runs from whichever reading holds institutional interpretive office in a given period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
