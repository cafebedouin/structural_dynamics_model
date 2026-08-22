% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity — Vatican II as Organic Development (Official Interpretive Regime)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-65) closed with documents whose ambiguities —
 *   collegiality, religious liberty, the liturgy, relations with
 *   non-Christian religions — admitted both rupture and continuity readings.
 *   The continuity account became the official hermeneutic: the Council
 *   changed nothing essential; apparent novelties explicate implicit prior
 *   teaching; ambiguities are prudential adaptations rather than doctrinal
 *   shifts; post-conciliar excesses are implementation errors rather than
 *   conciliar intent. This story authors that account as an enforced
 *   interpretive arrangement — the standing post-conciliar regime under
 *   contest — assessed by the reading's own lights, per the kernel-reading
 *   epsilon referent rule (the referent is the existing arrangement, never
 *   the arrangement this reading would prefer). The account performs real
 *   coordination work (it let a global institution receive a turbulent
 *   council without adjudicating rupture and risking schism) and imposes real
 *   asymmetric costs (it disciplines the traditionalist and progressive
 *   flanks from opposite directions, and imposed a liturgical settlement
 *   under a nothing-changed banner). This is ONE reading of a contested
 *   kernel; the sibling readings are separate constraints with their own
 *   epsilon, beneficiary structures, and classifications.
 *
 * KEY AGENTS:
 *   - roman_curia_magisterium: Agenda-setter (institutional/arbitrage) — issues the official interpretation, adjudicates which readings are legitimate, collects the stability and authority the account preserves
 *   - traditionalist_clergy_laity: Primary target (organized/trapped) — experiences the settlement as discontinuity, bears conformity or marginalization costs, liturgically cornered after Traditionis Custodes (2021)
 *   - progressive_theologians: Secondary target (organized/constrained) — bears the doctrinal ceiling and doctrinal-congregation discipline from the opposite flank
 *   - diocesan_clergy_postconciliar: Beneficiary (moderate/identity_locked) — vocational identity constituted by the account; administers the settlement
 *   - laity_in_pews: Mixed beneficiary-payer (powerless/mobile) — received stability and disruption together; exit by disaffiliation
 *   - ecumenical_partners: Secondary beneficiary (organized/mobile) — dialogue architecture underwritten by the continuity claim
 *   - sedevacantist_communities: Excluded (organized/trapped) — concluded the account fails and left the conversation; hold no seat in the process their departure defines
 *   - academic_historians: Analytical observer (analytical/analytical) — documents the composite causation and the gap between texts and implementation; adjudicates nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.6).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Hermeneutic of Continuity — Vatican II as Organic Development (Official Interpretive Regime)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '084a1561-74dc-474b-8b9e-401bb99f8220').
narrative_ontology:cs_kernel_codification('084a1561-74dc-474b-8b9e-401bb99f8220', fixed_text).
narrative_ontology:cs_authority_grounding('084a1561-74dc-474b-8b9e-401bb99f8220', lineage).
narrative_ontology:cs_interpretation_layer_present('084a1561-74dc-474b-8b9e-401bb99f8220').
narrative_ontology:cs_reading_relation('084a1561-74dc-474b-8b9e-401bb99f8220', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('084a1561-74dc-474b-8b9e-401bb99f8220', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('084a1561-74dc-474b-8b9e-401bb99f8220', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('084a1561-74dc-474b-8b9e-401bb99f8220', foundational, apparent_novelties_explicate_implicit_teaching).
narrative_ontology:cs_axiom_status(apparent_novelties_explicate_implicit_teaching, holdable).
narrative_ontology:cs_axiom_grounding('084a1561-74dc-474b-8b9e-401bb99f8220', apparent_novelties_explicate_implicit_teaching, theological).
narrative_ontology:cs_axiom('084a1561-74dc-474b-8b9e-401bb99f8220', secondary, magisterium_sole_authorized_council_interpreter).
narrative_ontology:cs_axiom_status(magisterium_sole_authorized_council_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('084a1561-74dc-474b-8b9e-401bb99f8220', magisterium_sole_authorized_council_interpreter, conventional).
narrative_ontology:cs_reference_frame('084a1561-74dc-474b-8b9e-401bb99f8220', hermeneutic_of_reform_in_continuity).
narrative_ontology:cs_drift_state('084a1561-74dc-474b-8b9e-401bb99f8220', contemporary_postconciliar_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('084a1561-74dc-474b-8b9e-401bb99f8220', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, roman_curia_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy_postconciliar).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, laity_in_pews).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_clergy_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, laity_in_pews).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, newmanian_doctrinal_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the official interpretation of the Council through curial documents, doctrinal-congregation interventions, and papal addresses, and adjudicates which readings of the conciliar texts are legitimate. The stability of the Church's post-conciliar self-understanding and the authority to settle interpretive disputes accrue to this seat. It can reframe the hermeneutic (as when Benedict XVI named it a 'hermeneutic of reform in continuity' in 2005) but cannot abandon the Council's reception without institutional rupture, so it arbitrates among framings rather than exiting.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, roman_curia_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Ordained and formed after the Council; their identity as priests of the post-conciliar Church is constituted by the continuity account — rejecting it would unravel their vocational self-understanding. They celebrate the reformed liturgy, teach the reformed catechesis, and receive the legitimacy the official account confers on their ministry. Leaving would mean losing vocation, livelihood, and community at once.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy_postconciliar, beneficiary,
    moderate, biographical, identity_locked, global).

% Experience the liturgical and pastoral settlement as discontinuity with the tradition they were formed in. Those who conformed bear the cost of celebrating forms they read as foreign; those who resisted bear marginalization, canonical irregularity (the SSPX case), and — after Traditionis Custodes in 2021 — sharply restricted access to the pre-conciliar Mass even in full communion. Organized in fraternities, institutes, and communities, but with no canonical route out that preserves their position.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_clergy_laity, payer,
    organized, biographical, trapped, global).

% Read the Council as authorizing reform beyond the documents' letter and press the 'spirit of the Council' against the official account. When they press far, they meet doctrinal-congregation investigations, silencing, and removal from Catholic teaching posts (the pattern of the liberation-theology era and after). Their employment, standing, and audience sit inside Catholic institutions, so exit is costly; their reading is as illegitimate inside the official frame as the traditionalist one — the account disciplines both flanks.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians, payer,
    organized, biographical, constrained, global).

% Received a new Mass, new catechesis, and a revised self-understanding within a single generation. They receive the sacramental stability and institutional continuity the official account secures, and they absorbed the practical disruption that the same account describes as mere adaptation. Their realistic exit is disaffiliation, which tens of millions took; those who remain mostly hold the official account as simply what the Church teaches.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, laity_in_pews, beneficiary,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, laity_in_pews, payer).

% Protestant and Orthodox dialogue partners who structured decades of bilateral dialogue around the Catholic claim that the Council changed nothing essential. The continuity account underwrites the validity of agreements reached across the Council and the Catholic claim to be the same interlocutor throughout. If the account collapsed, the dialogue architecture would need renegotiation — a cost they have an interest in avoiding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_partners, beneficiary,
    organized, generational, mobile, global).

% Concluded that the continuity account fails, that the conciliar settlement is a genuine discontinuity, and that the post-conciliar authorities therefore lack legitimacy; they left the conversation rather than argue inside it. No official document engages their position except as a deviation to be avoided; they hold no seat in the interpretive process their departure defines.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, sedevacantist_communities, excluded,
    organized, biographical, trapped, global).

% Study the Council's genesis, documents, and reception from the archives and diaries with no adjudicating role. They document the composite causation behind the settlement, the drafting ambiguities, and the gap between the texts and later official readings; their accounts are cited and used by every flank.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, academic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, roman_curia_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the reception problem every council with contested documents creates: it gives clergy, laity, and ecumenical partners a single authorized account of what changed (nothing essential) so that a global institution can absorb a turbulent council without adjudicating rupture and risking schism.
% TRANSFER_FUNCTION: Moves interpretive assent and compliance from clergy and laity to the magisterium, and moves the practical costs of liturgical and pastoral change onto practitioners — while the nothing-changed framing ensures no doctrinal concession is recorded in exchange.
% ABSENT_VOICES: Sedevacantist communities and the hardest traditionalist line concluded the continuity claim fails and left the conversation entirely; disciplined progressive theologians are present but muted. Both would object that the hermeneutic is unfalsifiable in its strong form and that the liturgical change exceeded prudential adaptation; neither seat sits inside the official interpretive process.
% DISAPPEARANCE_RATIONALE: Without the continuity hermeneutic, the Church must adjudicate the rupture question directly: the traditionalist reading (the Council erred) and the progressive reading (the Council authorizes ongoing reform) cannot both be accommodated, and the institution's post-conciliar coherence — the ecumenical agreements, the identity of the ordained, the liturgical settlement — was built on the continuity account and would reorganize under whichever rupture reading prevailed.
% FOUNDING_PROBLEM: The Council closed with documents containing genuine ambiguities (collegiality, religious liberty, the liturgy, relations with non-Christian religions) that could plausibly be read as doctrinal rupture; the institution needed an authorized account under which it could receive the Council whole — without schismatic exit on the traditionalist flank or unbounded expansion on the progressive flank.
% FOUNDING_PROBLEM_CORROBORATION: The interpretive contest's persistence corroborates the founding problem: both rupture movements arose immediately at the Council's close and remain active six decades on, and contemporary historians of the Council — working from the archives, outside the benefiting parties — document the hermeneutical battle as real rather than manufactured. No party outside the beneficiary set attests that the problem is solved; the beneficiaries' claim that it is solved is self-attestation.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Metrics are authored independently of the claim; the engine computes each seat's type from the structural data. Extractiveness (0.60) sits where this reading itself locates the burden: near-zero on the doctrinal layer (the account holds nothing essential changed, so assent is cheap) and high on the liturgical/pastoral layer (traditionalist clergy bear an imposed liturgical settlement plus the post-2021 restriction of the pre-conciliar Mass; progressive theologians bear a doctrinal ceiling enforced by doctrinal-congregation investigation and silencing). Suppression (0.62) is authored as the raw structural property it is — it is NOT scaled by power or scope; only extractiveness is scaled in the engine's computation — and reflects real enforcement machinery (canonical penalties, investigations, liturgical restriction) coexisting with genuine space for theological work inside the frame. Theater (0.35): the repeated public assertion that nothing changed, while practice changed substantially, is partly functional (it is the operative official account) and partly performative (maintaining the denial against six decades of accumulated discontinuity in practice). Accessibility collapse (0.50): alternative readings persist and are held by organized communities, but the official frame raises their cost sharply without eliminating them. Resistance (0.60): two-flank resistance sustained across the entire interval. The measurement series run on one shared time grid (T=0 is the Council's close, 1965; one unit is one year; T=60 is the present). Suppression_requirement is tracked because this story specifically traces enforcement-capacity change — build-up during the implementation decades, relaxation during the Benedictine détente, hardening after Traditionis Custodes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the curial seat the arrangement is guardianship: the same deposit, better articulated. From the traditionalist seat it is enforced denial of experienced rupture — told that what they lived did not happen. From the progressive seat it is a ceiling: the Council authorized more than the account admits, and the account is enforced against them. From the laity seat it is stability mixed with unexplained disruption. A structural feature worth registering: the two victim wings cannot coalesce. Each wing's claim requires the other's defeat — the traditionalist needs the rupture to be error, the progressive needs it to be authorization — so the account disciplines two flanks that cannot ally. That mutual foreclosure of coalition is part of what holds the arrangement stable despite sustained, organized resistance on both sides.
 *
 * DIRECTIONALITY LOGIC:
 *   The curia is the structural beneficiary and the seat the gains accrue to (d near the beneficiary end): interpretive authority and institutional stability flow to it, and it controls the framework. Post-conciliar clergy benefit through identity and legitimacy, with the identity lock amplifying the benefit side of their position. Ecumenical partners benefit externally: the continuity claim underwrites their dialogue architecture. The laity are declared beneficiaries (sacramental stability, institutional continuity) but absorbed the practical change directly; the derivation from the beneficiary declaration alone would place them near the full-beneficiary end, so a directionality override moves the powerless atom to 0.45 — near symmetric — reflecting the mixed position. Traditionalist clergy/laity and progressive theologians are the targets (d near the target end): both bear the costs, and their exit positions (trapped, constrained) amplify effective extraction. Beneficiaries get low derived d; trapped or identity-locked targets sit nearer the full-target end; the scope is global, which the engine accounts for in scaling extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — receiving an ambiguously drafted council without adjudicating rupture — remains live after six decades: both rupture movements arose at the Council's close and persist, and the interpretive contest is active. No mandatrophy is declared; the arrangement has not outlived its function. The tangled_rope classification guards against two mislabelings. A pure-coordination framing would erase the two-front burden the account itself concedes on the liturgical/pastoral layer. A pure-extraction framing would erase the genuine reception function that even the resisting flanks partly rely on — the traditionalist claim to be the true continuity requires the official framework to exist as the thing being betrayed. If the interpretive contest ever resolves (one rupture reading winning, or the dispute dissolving into irrelevance), the hermeneutic would atrophy toward performance and the classification should be revisited; the theater_ratio series is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_reading_of_kernel,
    'This constraint is the continuity_reading instantiation of kernel vatican_ii_doctrinal_authority — what would each sibling reading (rupture_progressive_reading, rupture_traditionalist_reading, composite_overdetermination_reading) change structurally, and where exactly is the disagreement located?',
    'Cross-reading comparison: compile all four readings of the kernel and locate the structural element on which they diverge — the doctrinal status of the conciliar novelties (explication of implicit teaching vs. authorization for reform beyond the texts vs. error enabling heterodox implementation vs. plurality of distinct shifts).',
    'If a rupture reading is structurally right, this reading''s low doctrinal extraction is misattributed — the doctrinal layer carries the burden and the arrangement moves toward the snare end. If the composite reading is right, the single epsilon here is an averaging artifact and this story decomposes into several linked stories with distinct beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_reading_of_kernel, conceptual, 'Committer structure: one reading of a four-reading kernel; the disagreement is located in the doctrinal status of conciliar novelty.').

omega_variable(
    implicit_teaching_identifiability,
    'Is ''implicit prior teaching'' an identifiable standard that could in principle fail to fit a novelty, or is the continuity hermeneutic unfalsifiable in its strong form — any novelty redescribable after the fact as explication?',
    'Test against cases where the magisterium itself rejected a proposed development (e.g., ordination of women, despite development-style arguments advanced for it): if the hermeneutic contains a principled stopping rule, some novelties must fail it on the hermeneutic''s own terms; if every rejected development fails for reasons imported from outside the hermeneutic, the standard is unfalsifiable.',
    'If unfalsifiable, the account''s coordination function is partly cover — the frame makes the interpretive authority unrefutable, and the burden borne on the liturgical/pastoral layer reads better as enforcement of an unaccountable interpretive monopoly (toward snare). If a principled stopping rule exists, the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_teaching_identifiability, conceptual, 'Whether the continuity standard is falsifiable or retroactively all-fitting.').

omega_variable(
    implementation_error_attribution_sustainability,
    'Can post-conciliar excesses remain attributable to implementation error rather than conciliar intent, given that the excesses were systematic across cultures, decades, and levels of hierarchy?',
    'Compare the conciliar texts with the implementation norms issued by the same authorities that approved the texts: if the implementing authorities consistently read the texts the ''excessive'' way, the error attribution becomes untenable and the intent question reopens.',
    'If the attribution fails, this reading''s epsilon profile is wrong — the doctrinal layer carries the burden after all, and this reading converges structurally toward the traditionalist sibling. If it holds, the reading''s separation of doctrine from practice is stable and the low doctrinal extraction stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_error_attribution_sustainability, empirical, 'Whether the implementation-error attribution can be sustained against the historical record.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of rupture readings structural (canonical penalties, doctrinal-congregation investigations, liturgical restriction) or internalized (clergy formed to treat the continuity account as identical with fidelity itself)?',
    'Post-exit trajectory: track clergy who leave or are removed — does the pressure to read through the continuity frame persist in their new communities, or does it dissolve with the structural apparatus?',
    'If substantially internalized, effective suppression exceeds the structural measure — the account persists in formed clergy even where enforcement relaxes, as suggested by the Benedictine détente (Summorum Pontificum era), when enforcement eased but the vast majority of clergy did not revert their reading or practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression of rupture readings among formed clergy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a four-reading constraint family over kernel vatican_ii_doctrinal_authority. The colloquial label 'what Vatican II was' covers four structurally distinct claims with different epsilon profiles and different beneficiary structures. This continuity reading carries moderate extraction concentrated on liturgical/pastoral practice with low doctrinal extraction; the rupture readings locate the burden differently; the composite reading would decompose this single story into several. Per the epsilon-invariance principle, each reading is authored as its own constraint with its own epsilon, beneficiaries, victims, and classification; the family is linked through affects_constraints. The upstream claim (the official continuity account) supplies the legitimacy conditions under which the sibling readings operate as dissent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
