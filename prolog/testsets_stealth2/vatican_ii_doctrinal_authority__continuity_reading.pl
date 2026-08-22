% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity — Conciliar Reception Discipline (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story authors the continuity_reading of Vatican II doctrinal
 *   authority as a single, epsilon-invariant constraint: the interpretive
 *   discipline requiring that every apparent novelty in the conciliar corpus
 *   be received as an explication of implicitly contained prior teaching,
 *   with residual discontinuities attributed to implementation error rather
 *   than conciliar intent. The standing arrangement under contest — the
 *   post-conciliar settlement as governed by this hermeneutic — is assessed
 *   by the reading's own lights: doctrinally near-costless (the reading
 *   concedes no doctrinal loss, so acceptance costs there are low) but
 *   liturgically and pastorally costly (the reading itself registers that the
 *   reformed rites displaced inherited forms whose loss fell on identifiable
 *   communities). The constraint coordinates a global communion around one
 *   account of its own history while enforcing that account through canonical
 *   machinery; it therefore carries both a genuine coordination function and
 *   asymmetric extraction, maintained by active enforcement. ASSUMPTIONS:
 *   interval t=0 maps to 1965 (Council close) and t=60 to 2025; sibling
 *   constraint IDs are assumed to follow the kernel__reading pattern used in
 *   this file's constraint_id. FAMILY NOTE: this file is one member of a
 *   four-story constraint family decomposing the colloquial label 'what
 *   Vatican II did'; the sibling files (rupture_progressive,
 *   rupture_traditionalist, composite_overdetermination) author different
 *   epsilon over the same referent and are linked via
 *   network.affects_constraints — the decomposition follows the
 *   epsilon-invariance rule, since each reading yields a stable, distinct
 *   extraction profile.
 *
 * KEY AGENTS:
 *   - roman_curial_doctrine_offices: Agenda-setter and collector (institutional/identity_locked) — administers interpretive discipline, issues notifications and approvals, collects interpretive jurisdiction over what counts as authentic tradition
 *   - roman_pontiff: Supreme agenda-setter (institutional/identity_locked) — ratifies or reverses the hermeneutic's application; his policy swings reset enforcement intensity for everyone else
 *   - residential_episcopate: Beneficiary with local enforcement duties (institutional/identity_locked) — receives centralized backing for disciplining dissent, exercises delegated interpretive authority in dioceses
 *   - postconciliar_liturgical_establishment: Beneficiary (organized/constrained) — translation bodies, liturgical commissions, and faculties whose work product the continuity designation protects
 *   - catholic_mainstream_laity: Beneficiary with diffuse costs (moderate/identity_locked) — receive a stable identity narrative and functioning parishes; carry catechetical discontinuity across generations
 *   - preconciliar_rite_laity: Primary target (powerless/trapped) — attached to inherited liturgical forms; access depends on revocable permission; their rupture-perception is officially classified as error
 *   - traditionalist_institutes_of_consecrated_life: Primary target (organized/trapped) — canonical existence, property, and seminaries hostage to continued recognition
 *   - progressive_rupture_theologians: Secondary target (moderate/constrained) — academics disciplined for reading the Council as authorizing reform beyond the texts
 *   - irregular_traditionalist_clergy: Excluded voice (organized/trapped) — outside the official interpretive conversation by mutual refusal and canonical penalty
 *   - professional_ecclesiastical_historians: Analytical observer (moderate/analytical) — document continuity and discontinuity with source-critical methods; hold no adjudicative power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.66).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.64).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Hermeneutic of Continuity — Conciliar Reception Discipline (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '29808a68-48da-4ccd-a152-57eb3854caec').
narrative_ontology:cs_kernel_codification('29808a68-48da-4ccd-a152-57eb3854caec', fixed_text).
narrative_ontology:cs_authority_grounding('29808a68-48da-4ccd-a152-57eb3854caec', lineage).
narrative_ontology:cs_interpretation_layer_present('29808a68-48da-4ccd-a152-57eb3854caec').
narrative_ontology:cs_reading_relation('29808a68-48da-4ccd-a152-57eb3854caec', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('29808a68-48da-4ccd-a152-57eb3854caec', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('29808a68-48da-4ccd-a152-57eb3854caec', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('29808a68-48da-4ccd-a152-57eb3854caec', foundational, conciliar_novelties_are_explications_of_prior_teaching).
narrative_ontology:cs_axiom_status(conciliar_novelties_are_explications_of_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('29808a68-48da-4ccd-a152-57eb3854caec', conciliar_novelties_are_explications_of_prior_teaching, empirically_contingent).
narrative_ontology:cs_axiom('29808a68-48da-4ccd-a152-57eb3854caec', foundational, spirit_guided_development_precludes_real_reversal).
narrative_ontology:cs_axiom_status(spirit_guided_development_precludes_real_reversal, holdable).
narrative_ontology:cs_axiom_grounding('29808a68-48da-4ccd-a152-57eb3854caec', spirit_guided_development_precludes_real_reversal, theological).
narrative_ontology:cs_axiom('29808a68-48da-4ccd-a152-57eb3854caec', secondary, interpretation_bounded_by_demonstrated_continuity).
narrative_ontology:cs_axiom_status(interpretation_bounded_by_demonstrated_continuity, holdable).
narrative_ontology:cs_axiom_grounding('29808a68-48da-4ccd-a152-57eb3854caec', interpretation_bounded_by_demonstrated_continuity, conventional).
narrative_ontology:cs_reference_frame('29808a68-48da-4ccd-a152-57eb3854caec', inviolable_deposit_organic_explication).
narrative_ontology:cs_drift_state('29808a68-48da-4ccd-a152-57eb3854caec', contemporary_postconciliar_reception, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29808a68-48da-4ccd-a152-57eb3854caec', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, roman_curial_doctrine_offices).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, residential_episcopate).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, postconciliar_liturgical_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, catholic_mainstream_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, preconciliar_rite_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_institutes_of_consecrated_life).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_rupture_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, catholic_mainstream_laity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, newmanian_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the dicasteries that adjudicate doctrinal and liturgical conformity: they issue notifications, grant or withdraw recognitions, approve catechetical materials, and investigate theologians whose readings depart from the continuity account. Their careers, canonical standing, and personal identities are constituted by service to the magisterial office; exit means abandoning clerical ministry entirely. They collect interpretive jurisdiction — the practical authority to determine what the tradition contains — as the direct return on administering the hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, roman_curial_doctrine_offices, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, roman_curial_doctrine_offices, beneficiary).

% Ratifies, modulates, or reverses the hermeneutic's application: successive pontificates have widened rite access, articulated the continuity formula in programmatic addresses, and later restricted rite access again. Each intervention resets enforcement intensity for every other seat. The office is lifelong, its identity total; there is no exit short of resignation, and resignation does not release the officeholder from the role's symbolic weight.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, roman_pontiff, agenda_setter,
    institutional, biographical, identity_locked, global).

% Diocesan bishops receive centralized backing when they discipline liturgical abuse or doctrinal dissent locally, and exercise delegated interpretive authority over catechesis and worship in their territories. Consecration is irreversible and membership in the episcopal college is effectively inescapable; a bishop who rejects the continuity framework loses jurisdiction rather than gaining independence. They benefit from the framework's backing while absorbing local implementation friction.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, residential_episcopate, beneficiary,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, residential_episcopate, agenda_setter).

% Translation bodies, liturgical commissions, seminary liturgy faculties, and liturgical publishers whose work product is the reformed rites and their supporting literature. The continuity designation protects decades of accumulated institutional investment: if the reformed rites were reclassified as rupture, the establishment's output would require justification rather than enjoy presumption. Exit would strand careers, translations, and published inventories.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, postconciliar_liturgical_establishment, beneficiary,
    organized, biographical, constrained, global).

% Receive a stable identity narrative — the faith is unchanged, the Council changed nothing essential — together with functioning parishes and a common catechetical grammar. They also carry diffuse costs: inherited devotions and musical forms disappeared within a single generation, producing catechetical discontinuity between grandparents and grandchildren. For the devout, exit means leaving the sacramental community in which their identity and family transmission are embedded, which is experienced as self-dissolution rather than option-taking.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, catholic_mainstream_laity, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, catholic_mainstream_laity, payer).

% Attached to the pre-conciliar liturgy by formation, devotion, and family practice. Their access to the inherited rites depends on successive grants of permission and their revocation; celebrating communities are sparse and geographically fixed, so relocation is often impossible without abandoning the rite altogether. Their perception that the liturgical change constitutes a rupture is officially classified as error, which forecloses the grievance channel rather than opening it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, preconciliar_rite_laity, payer,
    powerless, biographical, trapped, global).

% Religious orders, fraternities, and societies whose charisms center the pre-conciliar forms. Their canonical existence depends on recognition that can be conditioned or withdrawn; their seminaries, properties, and pipelines of vocations are hostage to continued regularization. Organization gives them collective voice, but the object of their collective life is precisely what the permission structure controls, so organization cannot convert into exit.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_institutes_of_consecrated_life, payer,
    organized, generational, trapped, global).

% Academic theologians who read the Council as authorizing reform beyond the texts and who treat the continuity account as a defensive fiction. They are subject to investigation, notification, review of their canonical mission to teach, and loss of ecclesiastical mandate. University chairs, confessional affiliation, and vocational identity tie them to the institution whose interpretive regime they contest; some have absorbed discipline, some have migrated to secular institutions, none can take the magisterial audience with them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_rupture_theologians, payer,
    moderate, biographical, constrained, global).

% Clergy in irregular canonical status who reject the conciliar settlement wholesale and operate parallel chapels, seminaries, and jurisdictions. They are excluded from the official interpretive conversation by mutual refusal — they decline the framework's authority, and the framework declines theirs — so their objections circulate only in polemical literature and parallel networks. Their position is self-trapping: conceding the framework's authority to re-enter the conversation would dissolve the grounds of their separation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, irregular_traditionalist_clergy, excluded,
    organized, generational, trapped, global).

% Scholars inside and outside confessional institutions who document both continuity and discontinuity in the conciliar corpus using source-critical and comparative methods. They produce the evidentiary record that every reading appeals to, but hold no adjudicative power over which reading prevails; their findings enter the framework only after passing through the offices they study.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, professional_ecclesiastical_historians, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, roman_curial_doctrine_offices).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative account of the Church's doctrinal history that a global communion of clergy, catechists, and laity can teach identically: it solves the problem of how a community whose self-understanding rests on an unchanging deposit can incorporate a major legislative event without fragmenting into rival accounts of its own past.
% TRANSFER_FUNCTION: Moves interpretive authority and the deference it commands toward the magisterial center — papal and curial offices, backed episcopate — and moves compliance costs outward: liturgical practice is transferred from inherited to reformed forms, with the burden of the transfer falling hardest on communities attached to the inherited forms and on theologians whose readings deviate in either direction.
% ABSENT_VOICES: Irregular traditionalist clergy (excluded by canonical status and mutual refusal), sedevacantist groups, non-Catholic ecumenical partners — especially the Orthodox, who assess Latin doctrinal development under different premises about what counts as organic growth — and the now-largely-deceased generation of laity formed before the Council. They stand outside synodal and curial processes; their objections survive in polemical literature, archival records, and parallel chapel networks rather than in the rooms where the hermeneutic is applied.
% DISAPPEARANCE_RATIONALE: Overnight removal would force every diocese, seminary, institute, and catechetical publisher to choose among rival accounts of the Council within months: catechesis would lose its common grammar, liturgical governance would fracture along rite lines, formalization of at least one schismatic jurisdiction becomes likely, and ecumenical dialogues calibrated to the continuity account would require renegotiation from their foundations.
% FOUNDING_PROBLEM: When the Council closed in 1965, the Church needed to bind a global communion to documents that changed worship, religious liberty, ecumenical posture, and ecclesial self-understanding, while preserving the teaching that the deposit of faith is inviolable — how to receive real change without admitting rupture.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular historians of modern Catholicism document the 1965–1975 reception crisis as a genuine governance problem; Orthodox and Protestant ecumenical interlocutors attested the scale of the changes at the time; and the reading's sharpest critics on both flanks dispute its solution precisely because they accept that the integration problem existed. No serious party denies the founding problem was real; the contest is over whether it has been solved.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   CLAIM/METRIC INDEPENDENCE: claimed_type=tangled_rope is my structural judgment — the constraint possesses a genuine coordination function (a single teachable account of the Church's doctrinal history for 1.4 billion members), asymmetric extraction (identifiable payers: rite-attached communities, disciplined theologians), and active enforcement (canonical penalties, rite-access restriction, censorial machinery). The metrics are authored independently as descriptive facts. EPSILON (0.66 end-state): deliberately bimodal — approximately 0.2 in the doctrinal domain (the reading concedes no doctrinal change, so extraction there is limited to acceptance-cost) and approximately 0.75 in the liturgical/pastoral domain (enforced displacement of inherited forms, precarity of rite-dependent communities); the composite weights toward the liturgical side because that is where enforcement actually bites. SUPPRESSION (0.64): raw structural property, unscaled by power or scope — canonical visitations, withdrawal of rite permissions, notifications against theologians, and conditionated recognition of institutes. THEATER (0.38): genuine scholarly labor (historical demonstration of continuity) coexists with a growing share of ritualized assertion (anniversary framings, ceremonial reaffirmations) as documentary discontinuities accumulate; the dip at t=40 reflects a period when the case was argued rather than asserted. ACCESSIBILITY_COLLAPSE (0.55): alternatives are foreclosed inside the framework (once the explication rule is granted, rupture readings become unsayable) but persist externally in irregular jurisdictions and academic theology. RESISTANCE (0.62): sixty years of organized two-flank resistance; the constraint has never been received without remainder. TRAJECTORY: enforcement buildout through the 1970s–80s, managed equilibrium under the indult regime, an accommodation-driven dip at t=40, and a sharp intensification at t=60 when rite access was restricted — policy step-changes, not oscillation; the series run on one shared time grid with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute different types from identical structural data. From the curial and episcopal seats, the constraint is the necessary condition of ecclesial unity: without a single authoritative account of the Council, the communion fragments into rival histories, and these seats experience enforcement as custodial care. From the preconciliar-rite and disciplined-theologian seats, the same structure operates as enforced extraction: their perception of discontinuity is not engaged but reclassified as error, and their exit options (trapped, constrained) mean the extraction cannot be avoided by leaving the framework short of leaving the communion itself. The engine computes this per-seat divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Roman curial offices (agenda_setter + beneficiary, identity_locked) sit nearest the beneficiary end — the constraint subsidizes them with interpretive jurisdiction and they cannot exit without dissolving their vocational identity. The residential episcopate (beneficiary + local enforcement) sits nearby but slightly toward symmetric, since they absorb local implementation friction. The postconciliar liturgical establishment (beneficiary, constrained exit) derives low d tempered by career lock-in. Mainstream laity (beneficiary with secondary payer position) sit near symmetric: real coordination benefit, diffuse indirect cost. The three victim groups derive high d: preconciliar rite laity (payer, powerless, trapped) near the full-target end; traditionalist institutes (payer, organized but trapped — organization raises power, trap raises effective extraction); progressive rupture theologians (payer, moderate, constrained). No directionality overrides were needed: the structural declarations plus exit options reproduce the actual relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — receiving real conciliar change without admitting rupture — remains live, regenerating with each controversy cycle, so the constraint has not outlived its function. The classification discipline cuts both ways here. Labeling the constraint a rope would erase the documented extraction from rite-attached communities and disciplined theologians; labeling it a snare would erase the genuine coordination achievement — a global communion that can still teach one account of its own history, which no sibling reading provides at comparable scale. Tangled_rope keeps both facts visible. Watch-item: if the explication-redescription apparatus proves unfalsifiable (see omega explication_redescription_falsifiability), the interpretive-domain extraction trend bends upward and the balance shifts toward snare; conversely, if implementation-error attribution succeeds across domains (omega implementation_error_attribution), extraction falls back toward rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (continuity_reading) of the contested kernel vatican_ii_doctrinal_authority; sibling readings (rupture_progressive_reading, rupture_traditionalist_reading, composite_overdetermination_reading) instantiate different constraints over the same referent — what would adopting a sibling change structurally?',
    'No in-framework resolution exists: the disagreement is located in whether the Council effected real doctrinal change and who holds authority to say so. Resolution is per-party adoption; corpus-level comparison across the four sibling story files is the only cross-reading measurement available.',
    'Under a rupture_traditionalist adoption, doctrinal extraction rises sharply and the conciliar texts themselves become the extraction object; under rupture_progressive adoption, liturgical extraction is reframed as liberation and the victim set shifts toward institutional conservatives; under composite_overdetermination, the unified-continuity account dissolves into separately-classified changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file authors one reading of a four-reading kernel; epsilon is reading-indexed over a shared referent.').

omega_variable(
    doctrinal_liturgical_extraction_split,
    'The continuity reading carries low extraction on doctrinal change and high extraction on liturgical/pastoral practice. Is the composite epsilon masking two structurally separable constraints that should be decomposed into separate stories per the epsilon-invariance principle?',
    'Test whether enforcement machinery, enforcer seats, and victim sets differ by domain: if liturgical enforcement runs through distinct offices (worship congregations, rite-access permissions) with distinct victims than doctrinal discipline (doctrinal dicastery, theologian investigations), split into a low-epsilon doctrinal story and a high-epsilon liturgical-enforcement story linked by network.affects_constraints.',
    'Decomposition would yield a near-rope doctrinal-hermeneutic constraint and a substantially extractive liturgical-enforcement constraint; the composite tangled_rope verdict would be replaced by divergent per-domain classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_liturgical_extraction_split, conceptual, 'Whether the reading''s bimodal extraction profile is one constraint with heterogeneous incidence or two constraints sharing a label.').

omega_variable(
    explication_redescription_falsifiability,
    'Can any conceivable datum refute the claim that apparent novelties are explications of implicit prior teaching, given that the redescription apparatus absorbs every candidate counterexample either as explication or as implementation error — and if the apparatus is unfalsifiable, is its coordination function inseparable from its evidential immunity?',
    'Specify in advance what observational finding would count as a genuine doctrinal reversal (e.g., direct contradiction of a defined dogma with no recoverable antecedent), then test whether the redescription apparatus would absorb even that case; survey whether any proponent has ever conceded a counterexample.',
    'If unfalsifiable, accessibility_collapse is understated for agents inside the framework, the interpretive-domain extraction trend bends upward, and the constraint''s balance shifts from tangled_rope toward snare in the hermeneutic domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explication_redescription_falsifiability, conceptual, 'Whether the explication claim is a testable historiographical thesis or a closed redescription loop.').

omega_variable(
    implementation_error_attribution,
    'Are post-conciliar excesses genuinely implementation errors diverging from conciliar intent (as this reading holds), or are they constitutive of the conciliar settlement itself?',
    'Compare the conciliar texts'' explicit provisions with implemented practice across jurisdictions and decades: where deviation is uniform across independent implementers and persists across personnel turnover, attribution to implementation error weakens; where deviation tracks local discretion, it strengthens.',
    'If excesses are constitutive rather than incidental, the reading''s low-doctrinal-extraction assessment becomes untenable, composite epsilon rises, and the victim set expands to include all parties governed by the excesses rather than only rite-attached communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_error_attribution, empirical, 'Attribution of post-conciliar discontinuities to implementation versus conciliar intent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (canonical penalties, rite-access restriction, censorial notifications) or internalized (clerical and lay identity fusion making rupture-perceiving dissent psychologically unavailable even where enforcement is lax)?',
    'Post-exit suppression trajectory: track clergy and laity who leave ministry or migrate to irregular jurisdictions — if rupture-affirming speech and practice persist at full strength after canonical pressure is removed, the structural share dominates; if dissent capacity attenuates, an internalized component is carrying the suppression.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists independently of enforcement policy swings (such as the t=40 accommodation and its t=60 reversal); the constraint''s persistence becomes less dependent on active enforcement than the suppression_requirement series implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of interpretive conformity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Vatican II doctrinal authority' decomposes into four readings of one kernel, each a separate story with its own stable epsilon over the shared referent (the post-conciliar settlement). This file instantiates the continuity_reading (low doctrinal epsilon, high liturgical/pastoral epsilon, ambiguities as prudential adaptations, excesses as implementation error). The upstream/downstream structure runs through the continuity reading because both rupture siblings define themselves AGAINST its account: the traditionalist sibling cites the same documented discontinuities as evidence of rupture, and the progressive sibling cites the continuity apparatus itself as the enforcement object. Sibling IDs assume the kernel__reading naming convention established by this file's constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
