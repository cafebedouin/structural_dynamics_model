% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Reading: Contextual Reasoning over Classical Ruling Fidelity
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This constraint instantiates the reformist ijtihad reading of the
 *   quran_hadith_substrate kernel: a contested claim about how Islamic legal
 *   authority should be exercised when classical fiqh rulings conflict with
 *   contemporary ethics, international human rights norms, or maslaha (public
 *   interest). Under this reading, the Quran's overarching ethical trajectory
 *   takes interpretive priority over literalist application of hadith-derived
 *   classical rulings, and contextual ijtihad is treated as an ongoing
 *   obligation rather than a closed historical practice. The reading has real
 *   institutional backing in some reform-oriented seminaries, transnational
 *   Muslim networks, and select state fatwa councils, but remains a minority
 *   position relative to traditionalist taqlid in most jurisdictions, and is
 *   vulnerable to sustained counter-mobilization by traditionalist
 *   authorities who characterize it as an illegitimate rupture with ijma.
 *
 * KEY AGENTS:
 *   - progressive_muslim_scholars: agenda_setter (moderate/constrained) — administers the reasoning method but lacks state enforcement power
 *   - muslim_women_seeking_reform, lgbtq_muslims, religious_minorities_under_muslim_majority_law: primary beneficiaries (powerless/constrained-trapped) — benefit only where the reading has institutional footing
 *   - traditional_madhhab_authorities, hadith_literalist_clergy: primary payers (institutional-organized/identity_locked) — their authority claim is directly eroded
 *   - state_religious_bureaucracies_tied_to_classical_fiqh: secondary payer/observer — hedges via selective adoption (the state_hybrid sibling reading)
 *   - human_rights_and_international_bodies: analytical observer — cites the reading without bearing its local costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.4).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.32).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Reading: Contextual Reasoning over Classical Ruling Fidelity").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '7e0ad008-3676-43d9-a524-c3cad045e157').
narrative_ontology:cs_kernel_codification('7e0ad008-3676-43d9-a524-c3cad045e157', distributed).
narrative_ontology:cs_authority_grounding('7e0ad008-3676-43d9-a524-c3cad045e157', expertise).
narrative_ontology:cs_interpretation_layer_present('7e0ad008-3676-43d9-a524-c3cad045e157').
narrative_ontology:cs_reading_relation('7e0ad008-3676-43d9-a524-c3cad045e157', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('7e0ad008-3676-43d9-a524-c3cad045e157', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('7e0ad008-3676-43d9-a524-c3cad045e157', foundational, quranic_ethical_trajectory_overrides_literalist_hadith).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_overrides_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('7e0ad008-3676-43d9-a524-c3cad045e157', quranic_ethical_trajectory_overrides_literalist_hadith, deontological).
narrative_ontology:cs_axiom('7e0ad008-3676-43d9-a524-c3cad045e157', foundational, ijtihad_gate_never_closed).
narrative_ontology:cs_axiom_status(ijtihad_gate_never_closed, holdable).
narrative_ontology:cs_axiom_grounding('7e0ad008-3676-43d9-a524-c3cad045e157', ijtihad_gate_never_closed, conventional).
narrative_ontology:cs_axiom('7e0ad008-3676-43d9-a524-c3cad045e157', secondary, maslaha_binds_when_classical_ruling_conflicts_with_contemporary_ethics).
narrative_ontology:cs_axiom_status(maslaha_binds_when_classical_ruling_conflicts_with_contemporary_ethics, holdable).
narrative_ontology:cs_axiom_grounding('7e0ad008-3676-43d9-a524-c3cad045e157', maslaha_binds_when_classical_ruling_conflicts_with_contemporary_ethics, instrumental).
narrative_ontology:cs_reference_frame('7e0ad008-3676-43d9-a524-c3cad045e157', classical_madhhab_consensus_framework).
narrative_ontology:cs_drift_state('7e0ad008-3676-43d9-a524-c3cad045e157', post_human_rights_era_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7e0ad008-3676-43d9-a524-c3cad045e157', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_reform).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_muslim_majority_law).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reform_oriented_institutions).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_madhhab_authorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, hadith_literalist_clergy).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, state_religious_bureaucracies_tied_to_classical_fiqh).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, quranic_ethical_trajectory_supremacy).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maslaha_as_binding_interpretive_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and circulate ijtihad-based rulings that reinterpret classical fiqh in light of contemporary ethics, human rights instruments, and maslaha. They administer the reasoning method itself — deciding which classical rulings are open to revision — but hold no state enforcement power and depend on institutional platforms (universities, reform councils, transnational networks) that can be withdrawn or delegitimized by traditionalist pushback.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    moderate, generational, constrained, global).

% Benefit directly when reformist rulings loosen classical restrictions on marriage, divorce, inheritance, and guardianship. Their exit from unfavorable classical rulings depends entirely on whether a reformist reading has any institutional foothold in their jurisdiction; where it does not, they remain bound by traditionalist application regardless of the reading's theoretical availability.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_reform, beneficiary,
    powerless, biographical, constrained, national).

% Stand to benefit most from a maslaha-and-ethical-trajectory framework that could re-read classical prohibitions, but in nearly every jurisdiction reformist readings on this specific question remain marginal or actively suppressed by both traditionalist and state authorities; their situation is the sharpest test of whether the reading's lower suppression is real or aspirational.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, trapped, global).

% Benefit where reformist ijtihad reframes classical dhimmi-status jurisprudence toward contemporary equal-citizenship norms. Their capacity to invoke this benefit depends on courts or legislatures adopting the reformist reading rather than treating it as a minority scholarly opinion.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_muslim_majority_law, beneficiary,
    powerless, generational, constrained, national).

% Universities, transnational Muslim reform networks, and some state-linked fatwa councils that adopt reformist methodology gain intellectual legitimacy, funding, and international standing (including from human-rights-aligned donors and governments). They can relocate their platforms across jurisdictions more easily than individual believers can relocate their legal status.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reform_oriented_institutions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, reform_oriented_institutions, agenda_setter).

% Their authority rests on claiming that classical fiqh represents settled ijma requiring taqlid, not continuous reinterpretation. Reformist ijtihad directly erodes this claim by asserting that classical rulings are revisable whenever they conflict with contemporary ethics or maslaha — a standard these authorities cannot control or bound. Their institutional identity is fused to the interpretive monopoly itself, making exit from the dispute equivalent to abandoning their institutional function.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_madhhab_authorities, payer,
    institutional, civilizational, identity_locked, global).

% Derive authority and livelihood from applying hadith-based rulings literally. The reformist framework subordinates their entire methodology to a prioritized Quranic ethical trajectory, which they experience as a direct attack on the legitimacy of their scholarly tradition and its authority to bind believers.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, hadith_literalist_clergy, payer,
    organized, generational, identity_locked, national).

% Ministries of religious affairs and state fatwa councils that have codified classical fiqh into family and personal-status law face political and administrative cost if reformist readings gain enough traction to force legal revision. Some hedge by selectively absorbing reformist language (see state_hybrid reading) while resisting full adoption.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_religious_bureaucracies_tied_to_classical_fiqh, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_religious_bureaucracies_tied_to_classical_fiqh, observer).

% Most lay believers are not party to the scholarly contest between reformist and traditionalist methodology; they receive whichever ruling their local mosque, court, or community imam applies, largely without a voice in which interpretive framework governs their lives.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, ordinary_practicing_muslims, excluded,
    powerless, biographical, constrained, local).

% Cite reformist ijtihad as evidence that Islamic jurisprudence is compatible with international human rights norms, sometimes fund or platform reformist scholarship, and use its existence in diplomatic and legal arguments — without bearing the costs or risks that scholars and lay beneficiaries bear inside contested jurisdictions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_and_international_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a methodological mechanism for Islamic legal reasoning to remain responsive to changed circumstances, rather than freezing rulings issued for premodern social conditions — the classical concept of ijtihad's 'gate' being reopened to address maslaha, changed context, and ethical trajectory rather than literalist hadith application in isolation.
% TRANSFER_FUNCTION: Moves interpretive authority away from institutions whose legitimacy rests on unbroken transmission of classical madhhab consensus (ijma) and taqlid, toward individual and institutional reasoners applying contextual ijtihad — and correspondingly moves practical legal and social outcomes (marriage, inheritance, minority status, gender roles) toward positions more aligned with contemporary human rights norms, at the direct expense of traditionalist authorities' claim to sole legitimate interpretation.
% ABSENT_VOICES: Ordinary practicing Muslims in jurisdictions where neither reformist nor state-hybrid frameworks have institutional traction are not represented in the scholarly contest; they experience whichever reading their local authorities apply without input. Lay LGBTQ+ Muslims and women in traditionalist-dominant regions are especially absent from the rooms where the reformist reading is debated, despite being named beneficiaries.
% DISAPPEARANCE_RATIONALE: Reformist scholars and beneficiary groups would say the world rearranges sharply: without the reformist methodology, avenues for revising classical family law, minority status, and gender rulings close, and legal reform initiatives lose their doctrinal grounding. Traditionalist authorities would say the world is largely unchanged or improved: taqlid and ijma continue to function as they always have, since in their own account the reformist reading was never authoritative to begin with. The verdict depends on which reading's account of authority is accepted, which is exactly the kernel-level dispute this story is one reading of.
% FOUNDING_PROBLEM: Classical fiqh rulings, codified centuries ago under specific historical, social, and political conditions, increasingly produce outcomes — regarding gender equality, minority citizenship, criminal punishment, and individual rights — that a significant body of Muslim scholars and laypeople judge to conflict with the Quran's own stated ethical aims and with contemporary human rights consensus. Reformist ijtihad was developed to resolve this gap without abandoning Quranic authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated partially from outside the reformist camp: comparative legal scholars and some traditionalist critics agree the gap between classical rulings and contemporary norms is real, even though they disagree sharply about the correct remedy (traditionalists favor renewed taqlid discipline or state-managed selective adoption rather than expanded ijtihad). International human rights bodies and academic Islamic studies scholars outside any single Muslim reform faction also document the gap independently. No fully neutral corroboration exists free of all doctrinal stake, since even 'outside' academic observers often have normative sympathies toward one reading or another.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, contested).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.4, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.40 (moderate band per the expected structural delta) reflecting that the reading substantively redistributes interpretive legitimacy away from traditional authorities without eliminating their institutional standing outright — it displaces rather than annihilates. Suppression is authored lower than a traditionalist reading would show (0.32 at interval end, falling from 0.45) because the reformist reading's own structural commitment is to open plural interpretation rather than enforced uniformity; the declining suppression trajectory models growing institutional acceptance in specific reform-friendly jurisdictions over the interval, though this acceptance remains partial and geographically uneven. Theater ratio stays low (0.22) because the scholarly and legal work of ijtihad is substantively performed, not merely gestured at. Resistance is authored high (0.68) because traditionalist and state-conservative pushback against reformist rulings is intense and organized wherever the reading gains visibility — this is a reading under active contest, not one that has settled into unopposed operation.
 *
 * PERSPECTIVAL GAP:
 *   From progressive scholars' seat, this reading is coordination: a shared methodology solving a genuine problem (rulings misaligned with the Quran's own ethical aims and with contemporary norms) via a defensible, textually-grounded process. From traditional madhhab authorities' seat, the same reading computes as extraction of their legitimacy — displacing centuries of accumulated interpretive authority via a standard (maslaha, ethical trajectory) that has no fixed boundary and can be invoked to override any ruling they consider settled. Both seats are looking at the identical structural facts; the divergence is exactly what the engine is built to compute, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars and reform institutions sit near the agenda-setting end but with only moderate power and constrained-to-mobile exit, since they lack coercive enforcement capacity and depend on institutional hosts that can withdraw support. Beneficiary groups (women, LGBTQ+ Muslims, religious minorities) are powerless with constrained-to-trapped exit — the theoretical availability of a reformist ruling does not translate into practical benefit unless local institutions adopt it, so their directionality toward full-beneficiary status is heavily discounted by exit reality. Traditional authorities and literalist clergy sit at the target end: their institutional legitimacy is the specific thing the reading's transfer function erodes, and their exit is identity-locked because their function IS the interpretive monopoly being challenged.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical rulings producing outcomes in tension with the Quran's own ethical aims and with contemporary rights norms) is authored as live, not dead — this blocks a mandatrophy verdict of pure inertial persistence. The reading is not a scaffold with a declared sunset; it presents itself as a permanent methodological correction, not a transitional measure. Because both a real coordination function (responsive legal reasoning) and asymmetric cost to identifiable payers (traditional authorities) are present, alongside active enforcement contestation, this reading is authored as tangled_rope rather than pure rope — it is not simply neutral coordination, since its operation structurally requires displacing an existing interpretive monopoly to succeed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijtihad_boundary_indeterminacy,
    'Is there a principled, bounded criterion for when ''conflict with contemporary ethics or maslaha'' licenses departure from a classical ruling, or is the boundary set case-by-case by whichever scholarly authority currently holds institutional platforms?',
    'Comparative analysis of reformist fiqh councils'' published methodological criteria across jurisdictions, checked against whether outcomes are predictable in advance or only justified after the fact.',
    'If the boundary is unprincipled, traditionalist authorities'' charge that reformist ijtihad is unconstrained extraction of interpretive authority gains structural support, pushing this reading''s classification toward snare in traditionalist-contested jurisdictions. If a stable, publicly checkable criterion exists, the tangled_rope coordination function is on firmer ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijtihad_boundary_indeterminacy, conceptual, 'Whether the reformist standard is principled or ad hoc.').

omega_variable(
    beneficiary_reach_gap,
    'For beneficiary groups named in this reading (women, LGBTQ+ Muslims, religious minorities), how large is the gap between the reading''s theoretical availability and its practical application in the jurisdictions where they actually live?',
    'Jurisdiction-by-jurisdiction survey of family court rulings, fatwa council outputs, and legal reform outcomes citing reformist ijtihad methodology versus jurisdictions where it exists only in academic literature.',
    'A wide gap would mean the authored moderate extractiveness and beneficiary declarations overstate real-world benefit capture for most named beneficiaries, and the reading functions mostly as elite/institutional discourse rather than lived legal change for the powerless beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_reach_gap, empirical, 'Gap between the reading''s theoretical reach and its practical legal application.').

omega_variable(
    kernel_framing_alternative,
    'Should this constraint be framed around the interpretive METHODOLOGY (ijtihad as a legal-theoretic procedure) or around the specific SUBSTANTIVE OUTCOMES it is invoked to reach (gender equality rulings, minority status, LGBTQ+ questions)? These could be two different constraints with different ε.',
    'Decompose further if evidence shows the methodology commands broad acceptance (even among some traditionalists, in narrow domains) while specific substantive applications (e.g., LGBTQ+ questions) remain far more contested — in which case a separate, higher-ε story for the specific contested substantive application would be warranted per the ε-invariance principle.',
    'If methodology and specific substantive outcomes diverge sharply in acceptance, this single story may be conflating a moderately-accepted procedural claim with a highly contested substantive one, understating suppression for the most contested applications (e.g., LGBTQ+ rulings) while overstating it for less contested ones (e.g., some inheritance or commercial law reforms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether methodology-level and outcome-level claims should be split into separate constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 8, 0.14).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 16, 0.17).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 32, 0.33).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_hadith_substrate kernel. traditionalist_taqlid holds classical madhhab consensus as binding and authoritative; state_hybrid has state sovereignty selectively adopt classical rulings in some domains (family/criminal law) while applying reformist or secular frameworks elsewhere. Each reading has its own ε, its own beneficiary/victim structure, and its own classification — reformist_ijtihad here is authored as tangled_rope with moderate ε (~0.40) and declining suppression; the traditionalist reading would be expected to show higher suppression of alternative readings and a different victim set (progressive scholars and reform beneficiaries as victims rather than beneficiaries); the state_hybrid reading would show a distinct extraction pattern rooted in political sovereignty rather than doctrinal fidelity. Do not average or reconcile ε across these three files — they are structurally distinct constraints sharing one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
