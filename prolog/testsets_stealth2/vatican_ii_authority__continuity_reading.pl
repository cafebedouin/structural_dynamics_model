% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Hermeneutic of Reform-in-Continuity: Authorized Vatican II Reception Regime
 *   domain: theology/ecclesiology/religious authority
 *
 * SUMMARY:
 *   This story instantiates the continuity_reading of the
 *   vatican_ii_authority kernel: the claim that Vatican II represents organic
 *   doctrinal development in continuity with tradition, that all sixteen
 *   documents are valid, and that post-conciliar reforms are legitimate
 *   expressions of the unchanging deposit of faith, with apparent ambiguities
 *   resolvable through traditional hermeneutics. The standing arrangement
 *   under contest — and therefore the epsilon referent — is the
 *   post-conciliar reception regime itself: the authorized hermeneutic, the
 *   enforcement machinery that backs it, and the distribution of standing it
 *   produces. Assessed by this reading's own lights, the arrangement is
 *   legitimate development carrying real enforcement overhead, hence the
 *   moderate reading-indexed extractiveness (0.46); the sibling files
 *   instantiate the same referent under different readings — rupture_reading
 *   authors high epsilon for the identical arrangement (the tradition as
 *   taken), and composite_overdetermination_reading authors indeterminate
 *   epsilon (the event as unmeasurable as one thing). Per the
 *   epsilon-invariance principle these are separate stories linked by
 *   network.affects_constraints, not one story with a measurement parameter.
 *   The claim/metric relationship is deliberate: the reading CLAIMS the
 *   arrangement is development-with-overhead (tangled_rope: genuine
 *   coordination plus asymmetric bearing of cost), while the metrics describe
 *   the actual operation — active enforcement, sustained two-flank
 *   resistance, alternatives that survive suppression — and the engine
 *   computes each seat's type from the structural data. KEY AGENTS (by
 *   structural relationship): - holy_see_magisterium: Agenda setter
 *   (institutional/identity_locked) — defines the authorized hermeneutic,
 *   disciplines dissent; its post-conciliar credibility depends on the
 *   council having taught truly - conciliar_reform_bishops: Primary
 *   beneficiary (institutional/identity_locked) — episcopal implementers
 *   whose collegial authority exists only because the council is accepted -
 *   progressive_reform_theologians: Beneficiary (organized/constrained) —
 *   academics whose post-conciliar work is certified as authentic development
 *   - catholic_laity_majority: Beneficiary with diffuse costs
 *   (moderate/mobile) — receives the reforms' fruits; carries disruption and
 *   disaffiliation - traditionalist_catholics: Primary target
 *   (organized/identity_locked) — attached to pre-conciliar forms; their
 *   reading ruled out of bounds; bear canonical and liturgical costs -
 *   radical_reform_theologians: Secondary target (moderate/constrained) —
 *   pressed for deeper change than the settlement enacted; censured when they
 *   exceeded it - sedevacantist_clergy: Excluded voice (powerless/trapped) —
 *   deny the post-conciliar order's validity entirely; hold no seat in the
 *   conversation - academic_ecclesiologists: Analytical observer
 *   (analytical/analytical) — historians documenting the reception conflict
 *   without a seat in it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.46).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.6).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Hermeneutic of Reform-in-Continuity: Authorized Vatican II Reception Regime").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd').
narrative_ontology:cs_kernel_codification('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', fixed_text).
narrative_ontology:cs_authority_grounding('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', lineage).
narrative_ontology:cs_interpretation_layer_present('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd').
narrative_ontology:cs_reading_relation('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', foundational, vatican_ii_organic_development_of_deposit).
narrative_ontology:cs_axiom_status(vatican_ii_organic_development_of_deposit, holdable).
narrative_ontology:cs_axiom_grounding('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', vatican_ii_organic_development_of_deposit, theological).
narrative_ontology:cs_axiom('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', foundational, apparent_conciliar_ambiguities_resolvable_in_tradition).
narrative_ontology:cs_axiom_status(apparent_conciliar_ambiguities_resolvable_in_tradition, holdable).
narrative_ontology:cs_axiom_grounding('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', apparent_conciliar_ambiguities_resolvable_in_tradition, conventional).
narrative_ontology:cs_reference_frame('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', organic_continuity_of_deposit).
narrative_ontology:cs_drift_state('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', contemporary_liturgy_wars, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('34310b4e-dbb7-4a4e-93ad-0ba3ccf24cdd', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, holy_see_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_reform_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reform_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, catholic_laity_majority).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, radical_reform_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, catholic_laity_majority).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope and the doctrine dicastery define which reading of the sixteen conciliar documents is operative, issue authoritative interpretations, approve or withhold liturgical permissions, and discipline teachers who depart from the authorized line. Since the council closed, the See's own credibility as teacher rests on the council having taught truly; the office both administers the settlement and draws standing from it. Exiting this position would mean repudiating the office's own decades of teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, holy_see_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, holy_see_magisterium, beneficiary).

% Bishops of the generation that implemented the council — liturgy commissions, synod secretariats, national conference presidencies — exercise authority through collegial structures, vernacular worship oversight, and new consultative bodies that exist only because the council is accepted as legitimate. Their institutional inheritance is bound to the settlement holding.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, conciliar_reform_bishops, beneficiary,
    institutional, generational, identity_locked, global).

% Theologians and liturgists whose post-conciliar work in ecumenism, interreligious dialogue, and historical-critical method is certified as authentic development of the deposit. Their mandates, faculties, and publishing channels depend on the continuity certification; a rupture verdict would strand their life's work as error. Leaving the Catholic academic ecosystem would mean losing the audience their work addresses.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reform_theologians, beneficiary,
    organized, biographical, constrained, continental).

% Around a billion baptized members who receive the settlement's fruits — vernacular liturgy, revised catechesis, expanded lay roles — and who carry its diffuse costs: parish disruption, generational catechetical gaps, and the disaffiliation of children who found the reformed forms thin. Most hold no strong view on the hermeneutic dispute; their practical exit is simply walking away, which large numbers have done.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, catholic_laity_majority, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, catholic_laity_majority, payer).

% Clergy, religious institutes, and lay communities attached to the pre-conciliar liturgy and doctrinal synthesis. They read the council's texts as containing novelties that cannot be harmonized with prior teaching, and the authorized hermeneutic rules their reading out of bounds. Costs they bear: canonical irregularity or supervision, restriction of access to the older rites, exclusion from teaching offices, and the charge of schism whenever they organize. Leaving the church entirely would surrender the sacramental life their identity is built around; staying means living under a reading they reject.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_catholics, payer,
    organized, generational, identity_locked, global).

% Theologians who judged the enacted reforms insufficient and pressed for deeper revision on contraception, ordination, and authority structures. The same settlement that certifies moderate reform as development ruled their further program out of bounds: censures, withdrawn mandates, silenced publications, and suppressed orders followed. Their exit was partial — some left the academy or the church; others continued under censure.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, radical_reform_theologians, payer,
    moderate, biographical, constrained, continental).

% Clergy who concluded that the post-conciliar popes are not true popes and the reformed sacraments are doubtfully valid. They hold no recognized seat anywhere in the conversation their conclusion is about; they publish from the margins and are answered, when at all, as a pathology of the fringe rather than as a party.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, sedevacantist_clergy, excluded,
    powerless, generational, trapped, global).

% Historians and systematic theologians who study the council and its reception without holding office in the dispute. They document the drafting history, the vote counts, and the hermeneutic debates; they grant the arrangement no legitimacy and suffer no penalties from it.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, academic_ecclesiologists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, holy_see_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authorized reading under which all sixteen conciliar documents bind the whole church at once: one hermeneutic lets a global institution absorb a sweeping council without fragmenting into mutually anathematizing schools, and lets parishes, seminaries, and tribunals know which interpretation of the texts is operative.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy inward: compliance with the authorized hermeneutic — in teaching chairs, liturgical practice, and canonical standing — flows from the edges to the center, while offices, mandates, and platforms flow outward to those who teach the authorized line. Dissenting readers on both flanks transfer standing, career security, and liturgical access to the center's discretion.
% ABSENT_VOICES: Sedevacantist clergy would object that the entire post-conciliar order lacks validity; they sit wholly outside the conversation, with no recognized seat since the 1988 separations. The Eastern Orthodox observers' ecclesiological reservations about the conciliar process never received a seat in the Western reception debate. And the pre-conciliar magisterial voice itself — the anti-modernist framework the council displaced — has no living advocate with institutional standing.
% DISAPPEARANCE_RATIONALE: If the continuity settlement vanished overnight — if the church formally conceded rupture or irreconcilable contradiction — the post-conciliar order would rearrange immediately: the reform program's legitimacy collapses, ordinations and confirmations conferred under the reformed rites become canonically doubtful at scale, the episcopal conference system loses its charter, and the traditionalist and progressive wings would formalize into competing jurisdictions rather than remaining tolerated fringes.
% FOUNDING_PROBLEM: The reception crisis of Vatican II itself: the sixteen documents contained real tensions — religious liberty against prior condemnations of indifferentism, collegiality against the defined universal primacy, a reformed liturgy against four centuries of settled practice — and the church needed a way to bind them as authentic teaching without splitting.
% FOUNDING_PROBLEM_CORROBORATION: Church historians outside the benefiting parties corroborate that the reception problem was real and remains unresolved: the multi-volume History of Vatican II project and subsequent reception studies document the tensions and the contested hermeneutic from an academic seat, and sociologists of religion independently document the persistence of the liturgy conflict. On status, the corroboration splits — historians of the conciliar event tend to judge the tensions structural rather than superficial, while the magisterium attests they are resolvable; no neutral arbiter exists.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 reading-indexed: the continuity seat sees the arrangement as legitimate development whose costs are largely service-priced (unity, doctrinal integration), while acknowledging that interpretive compliance is collected from dissenters on both flanks. Suppression is authored at 0.60 as a raw structural property — unscaled by power or scope — because the settlement's persistence demonstrably depends on active machinery: dicastery interventions, withdrawn mandates, canonical supervision of traditionalist institutes, and the 2021 restriction of the older rites. Theater_ratio 0.36: the doctrinal and liturgical work is substantially real, but a growing share of activity is ritual invocation of continuity (anniversary addresses, hermeneutic formulae deployed to justify reversals) rather than textual engagement. Accessibility_collapse 0.40: alternatives do not collapse — the rupture reading survives in academies and chapels, the traditionalist practice survives irregularly — but each alternative operates at a permanent institutional discount. Resistance 0.55: sustained from both flanks simultaneously, which is the arrangement's distinctive burden — it must police excess on two fronts at once, against those who want the council reversed and those who want it deepened. The temporal series share one grid (ten points, 1965-2026) and show a full enforcement cycle: buildup through the disciplinary era peaking at the 1988 separations, detente through the reconciliation decade bottoming around 2012, re-hardening after 2021. The oscillation is carried as a candidate intermittent-reinforcement mechanism (omega enforcement_oscillation_function); base_properties reflect the current re-hardened phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the magisterial seat the arrangement is pastoral governance: discipline is medicine, the hermeneutic is fidelity, and the flanks are convalescents. From the traditionalist seat the same machinery operates as enforced loss: a reading they hold in good faith is ruled out of bounds, and the price of belonging is assent they cannot give. From the radical-reformer seat the settlement operates as a ceiling: it certifies exactly as much change as was enacted and no more. The laity sit near-symmetric — genuine fruits, diffuse costs, and a mobile exit the clerical seats lack. The engine computes these divergences from power, exit, and directional data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the magisterium, the reform bishops, and the progressive theologians — the settlement subsidizes their standing — with the magisterium nearest the beneficiary end because it both administers and collects. The laity derive mildly beneficiary, with the payer secondary role registering their diffuse costs. The two payer seats derive high directionality, amplified by exit character: traditionalist_catholics are identity_locked (their Catholic identity is constituted by the patrimony the settlement discounts), which pushes them toward the full-target end; radical_reform_theologians are constrained but less locked — some exited the ecosystem entirely. Sedevacantist clergy fall outside the derivation: they neither pay into nor collect from the arrangement they reject, which is why they are authored excluded rather than payer. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the intended spread without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. Against pure-extraction mislabeling: the coordination function is genuine and load-bearing — a billion-member institution absorbed a sweeping council without formal schism precisely because one authorized reading existed; a snare verdict would erase the unity work that is real. Against pure-coordination mislabeling: the settlement's benefits concentrate (standing, offices, mandates flow to the center and the reform establishment) while its costs land on identifiable flanks, and it holds by enforcement rather than consent — a rope verdict would erase the asymmetry. Tangled_rope holds both truths. Mandatrophy is not declared: the founding problem (reception of the council) is contested-live, not dead — the arrangement's function has not outlived its origin, so the zombie configuration (dead founding problem paired with a world_rearranges verdict) does not obtain. The reading's own delta prediction ('victim: none') diverges from the authored structure, which declares two payer seats; that divergence is carried as omega victimless_delta_vs_authored_structure rather than reconciled away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates only the continuity_reading of the vatican_ii_authority kernel — what would the sibling readings change structurally?',
    'Not resolvable by data: framing-dependent. Read the sibling stories (vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading) and compare authored epsilon, victim sets, and computed types across the family.',
    'Under rupture_reading the same referent authors high epsilon with the inherited tradition itself among the injured; under composite_overdetermination_reading epsilon becomes indeterminate and the clean coordination half of the arrangement dissolves. Classification of every seat shifts with the adopted reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one kernel, three readings; this file is the continuity reading only.').

omega_variable(
    victimless_delta_vs_authored_structure,
    'The reading''s own structural delta predicts ''victim: none — reforms are cost-free development,'' but the authored structure declares two payer seats. Which is true of the standing arrangement?',
    'Comparative cost accounting: do the costs borne by traditionalist_catholics and radical_reform_theologians track the marginal service the arrangement provides (unity, doctrinal integration) or exceed it in ways that concentrate authority-protection rents?',
    'If costs track service, the arrangement settles toward coordination-with-overhead and the reading''s self-description stands; if costs exceed service and protect the reform establishment''s authority, effective extraction rises and the extraction-dominant flank strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victimless_delta_vs_authored_structure, empirical, 'Whether the continuity regime''s costs are service-priced or rent-bearing.').

omega_variable(
    enforcement_oscillation_function,
    'Is the suppression cycle (buildup 1965-1988, detente 1988-2012, re-hardening 2012-2026) an intermittent-reinforcement mechanism that stabilizes compliance, or sincere pastoral course-correction?',
    'Correlate enforcement phases with compliance and dissent indicators across the cycle; test whether relaxation phases precede renewed extraction demands (reinforcement signature) or coincide with genuine grievance redress.',
    'If reinforcement, the oscillation itself is part of the compliance machinery and effective suppression exceeds any single-phase reading; if correction, the cycle dampens rather than produces extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_oscillation_function, empirical, 'Function of the enforcement cycle in the arrangement''s persistence.').

omega_variable(
    hermeneutic_resolution_genuineness,
    'Does the traditional hermeneutic genuinely resolve the documents'' internal tensions (religious liberty against prior condemnations of indifferentism, collegiality against the defined universal primacy), or does it manage them rhetorically?',
    'Scholarly adjudication: compare the hermeneutic''s proposed resolutions against the historical-record objections raised by both flanks; a resolution that satisfies neither flank on the merits is management, not resolution.',
    'Genuine resolution supports the coordination half of the tangled_rope reading; rhetorical management shifts weight toward the extraction half and strengthens the composite_reading''s case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_resolution_genuineness, empirical, 'Whether the authorized hermeneutic resolves or merely manages the conciliar tensions.').

omega_variable(
    traditionalist_exit_composition,
    'How much of traditionalist_catholics'' inability to exit is identity fusion versus canonical and sacramental trapping?',
    'Post-departure trajectories: track communities and individuals who left for Orthodoxy, sedevacantism, or secularity — if attachment to the pre-conciliar patrimony persists after canonical barriers are removed, fusion dominates.',
    'Identity-fusion dominance raises their effective extraction (full-target behavior that travels with them); canonical-trapping dominance locates the barrier structurally and makes it legislatively fixable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_exit_composition, empirical, 'Composition of the traditionalist exit barrier: identity fusion versus structural trapping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(vati_tr_t1965, observed).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_authority__continuity_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(vati_tr_t1970, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_authority__continuity_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement_basis(vati_tr_t1980, observed).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_authority__continuity_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement_basis(vati_tr_t1988, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(vati_tr_t2005, observed).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_authority__continuity_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement_basis(vati_tr_t2012, observed).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_authority__continuity_reading, theater_ratio, 2021, 0.34).
narrative_ontology:measurement_basis(vati_tr_t2021, observed).
narrative_ontology:measurement(vati_tr_t2026, vatican_ii_authority__continuity_reading, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(vati_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement_basis(vati_be_t1965, observed).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_authority__continuity_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement_basis(vati_be_t1970, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_authority__continuity_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(vati_be_t1980, observed).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_authority__continuity_reading, base_extractiveness, 1988, 0.48).
narrative_ontology:measurement_basis(vati_be_t1988, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(vati_be_t2005, observed).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_authority__continuity_reading, base_extractiveness, 2012, 0.36).
narrative_ontology:measurement_basis(vati_be_t2012, observed).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_authority__continuity_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement_basis(vati_be_t2021, observed).
narrative_ontology:measurement(vati_be_t2026, vatican_ii_authority__continuity_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(vati_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement_basis(vati_su_t1965, observed).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_authority__continuity_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement_basis(vati_su_t1970, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__continuity_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_authority__continuity_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement_basis(vati_su_t1980, observed).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_authority__continuity_reading, suppression_requirement, 1988, 0.62).
narrative_ontology:measurement_basis(vati_su_t1988, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__continuity_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__continuity_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement_basis(vati_su_t2005, observed).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_authority__continuity_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement_basis(vati_su_t2012, observed).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_authority__continuity_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(vati_su_t2021, observed).
narrative_ontology:measurement(vati_su_t2026, vatican_ii_authority__continuity_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(vati_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Vatican II's authority' decomposes into three readings of one kernel (vatican_ii_authority). This file instantiates continuity_reading, which authors epsilon 0.46 for the standing post-conciliar arrangement — it sees legitimate development carrying enforcement overhead. The sibling files instantiate rupture_reading (same referent, high epsilon: the arrangement as dispossession of the inherited tradition) and composite_overdetermination_reading (epsilon indeterminate: the event resists measurement as one thing). The continuity reading is upstream institutionally: it controls the interpretive resources (faculties, mandates, liturgical permissions) through which the siblings' adherents operate, so its edges run outward to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
