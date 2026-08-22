% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses — Progressive Abrogation Reading (Naskh Supersession of Gender-Specific Rules)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   A constraint family surrounds the Qur'an's gender-specific legal verses
 *   (4:11 inheritance shares, 2:282 testimony weighting, 4:34 marital
 *   authority), which classical jurisprudence administers as binding family
 *   law across much of the Muslim world. This file authors ONE reading of
 *   that kernel — progressive_abrogation: the claim that the verses were an
 *   incomplete trajectory whose later egalitarian principles (49:13 universal
 *   human dignity) supersede the gender-specific rules via naskh. Per the
 *   ε-invariance principle, ε's referent is the standing arrangement under
 *   contest — the gender-differentiated legal regime as actually administered
 *   — assessed by this reading's own lights, in which the regime's justifying
 *   verses are already superseded and its persistence is therefore extraction
 *   without live justification. The sibling readings (literal_hierarchical,
 *   contextual_egalitarian) are separate constraint stories with their own ε
 *   values and victim structures; they are not folded into this one. The
 *   claim and the metrics are independent authored facts:
 *   claimed_type=scaffold is this reading's structural verdict (the
 *   arrangement was transitional support whose sunset the canon itself
 *   supplies, and the sunset has fired), while the metrics describe the
 *   standing arrangement's actual operation as this reading assesses it. The
 *   engine computes per-seat classifications from the structural data; where
 *   a computed seat-type diverges from this claim, that divergence is data,
 *   not error. KEY AGENTS (by structural relationship): -
 *   muslim_women_under_classical_law: primary target
 *   (powerless/identity_locked) — bears the property, standing, and authority
 *   differential - male_kin_and_guardians: primary material beneficiary
 *   (moderate/mobile) — collects the differential shares and guardianship -
 *   traditional_juristic_class: agenda-setter and authority beneficiary
 *   (institutional/identity_locked) — administers the rules and captures
 *   interpretive authority - muslim_family_law_courts: secondary
 *   administrator (institutional/constrained) — applies codified rules
 *   without originating them - reformist_scholars: suppressed contesting seat
 *   (moderate/constrained) — advances this reading and pays for it -
 *   secular_hermeneutics_observers: analytical observer
 *   (analytical/analytical) — maps the contest without holding a seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, scaffold).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses — Progressive Abrogation Reading (Naskh Supersession of Gender-Specific Rules)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).
narrative_ontology:has_sunset_clause(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '1a9fca79-5da0-4318-97c8-db563a9234b1').
narrative_ontology:cs_kernel_codification('1a9fca79-5da0-4318-97c8-db563a9234b1', fixed_text).
narrative_ontology:cs_authority_grounding('1a9fca79-5da0-4318-97c8-db563a9234b1', lineage).
narrative_ontology:cs_interpretation_layer_present('1a9fca79-5da0-4318-97c8-db563a9234b1').
narrative_ontology:cs_reading_relation('1a9fca79-5da0-4318-97c8-db563a9234b1', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('1a9fca79-5da0-4318-97c8-db563a9234b1', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('1a9fca79-5da0-4318-97c8-db563a9234b1', foundational, egalitarian_principles_abrogate_gender_rules).
narrative_ontology:cs_axiom_status(egalitarian_principles_abrogate_gender_rules, holdable).
narrative_ontology:cs_axiom_grounding('1a9fca79-5da0-4318-97c8-db563a9234b1', egalitarian_principles_abrogate_gender_rules, theological).
narrative_ontology:cs_axiom('1a9fca79-5da0-4318-97c8-db563a9234b1', foundational, naskh_operates_at_principle_level).
narrative_ontology:cs_axiom_status(naskh_operates_at_principle_level, holdable).
narrative_ontology:cs_axiom_grounding('1a9fca79-5da0-4318-97c8-db563a9234b1', naskh_operates_at_principle_level, conventional).
narrative_ontology:cs_reference_frame('1a9fca79-5da0-4318-97c8-db563a9234b1', completed_egalitarian_trajectory).
narrative_ontology:cs_drift_state('1a9fca79-5da0-4318-97c8-db563a9234b1', contemporary_muslim_family_law, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a9fca79-5da0-4318-97c8-db563a9234b1', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, male_kin_and_guardians).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, traditional_juristic_class).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, muslim_women_under_classical_law).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, reformist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the gender-differentiated family-law rules: receive half the inheritance share of male counterparts in the same kinship line, give testimony weighted below a man's in financial matters, and fall under marital-authority and discipline provisions administered by family courts and juristic councils. Leaving the arrangement would mean leaving the faith community itself — losing family ties, marriage validity, community standing, and in some jurisdictions legal recognition — so the rules travel with religious identity rather than being escapable by relocation or role renunciation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_women_under_classical_law, payer,
    powerless, biographical, identity_locked, global).

% Receive double inheritance shares relative to sisters in the same kinship line, act as marriage guardians, and hold default household authority under the rules. The arrangement channels property and standing to them at no administrative burden of their own; declining it would mean forgoing shares the courts will otherwise award, so they have every incentive to remain as they are.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, male_kin_and_guardians, beneficiary,
    moderate, biographical, mobile, global).

% Canonize, teach, and adjudicate the rules through madhhab seminaries, mufti councils, and endowed teaching posts. Their interpretive office is constituted by the rules' continuing force as divine ordinance: a scholar who declares the gender rules superseded dissolves the textual foundation of his own authority. The class staffs the boundary-keeping — charges of innovation, denial of teaching posts, exclusion of rival hermeneutics from official canon — that keeps egalitarian readings out of authoritative seats, and its standing rises with every successful defense of the rules' continuing force.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_juristic_class, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, traditional_juristic_class, beneficiary).

% Apply codified family law in inheritance, testimony, marriage, and divorce cases across Muslim-majority jurisdictions. Bound by statute and by the juristic class's doctrine; an individual judge who deviates from the differentiated rules faces reversal on appeal and career cost. They administer the rules without originating them and collect no revenue from them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_family_law_courts, agenda_setter,
    institutional, generational, constrained, national).

% Advance the abrogation reading and neighboring egalitarian hermeneutics from university chairs, research institutes, and marginal seminary positions. They are excluded from official interpretive councils and pay for the position in lost institutional standing, accusations of bid'ah, and in some jurisdictions security attention. Exit into purely secular academia is available but abandons the intra-traditional audience the project addresses, so most stay and absorb the costs.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% Academic specialists in Islamic law, usul al-fiqh, and Qur'anic studies who map the interpretive contest without holding a seat in it. They document the textual premises, chronological claims, and institutional dynamics on which each reading depends, and their journals and monographs are the nearest available neutral record of the dispute.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_hermeneutics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, traditional_juristic_class).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its founding context the gender-specific rules solved a real coordination problem: a society in which women had no property rights or legal standing needed uniform rules assigning inheritance shares, testimony participation, and marital obligations. The verses provided the first codified property entitlements for women in the Arabian context and a single family-law framework for an expanding community that had previously regulated these matters by custom alone.
% TRANSFER_FUNCTION: Moves inheritance shares (a 2:1 differential to male kin), courtroom standing (testimony weighted by sex), and household authority from women to male kin and husbands; and moves interpretive authority over the rules to the juristic class that canonizes and administers them.
% ABSENT_VOICES: Women are absent from the seats that authored, canonized, and adjudicate the rules: classical tafsir and fiqh were produced almost entirely by men whose own shares the rules allocate, and no consultative mechanism carries governed women's testimony into the interpretive councils. Also absent: non-literalist hermeneutical traditions excluded from madhhab canon, and lay believers whose family law is fixed without recourse.
% DISAPPEARANCE_RATIONALE: If the arrangement's force vanished overnight, inheritance distribution across Muslim-majority jurisdictions would shift to equal shares, testimony weighting and marital-authority doctrines would lose their applied vehicle, family-court procedure would reorganize around the reformed or secular codes already on the books in several states, and the juristic class would lose the administrative domain through which its authority is exercised — the family-law order of roughly a fifth of the world's population would rearrange.
% FOUNDING_PROBLEM: Seventh-century Arabia: women had no inheritance rights, female infanticide was practiced, and marriage and divorce were regulated entirely against women's interests. The verses secured minimal property and legal standing for women within the existing social order — shares where there had been none, testimony where there had been silence, regulated treatment where there had been discretion.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship on pre-Islamic Arabian inheritance and marriage practice corroborates the founding problem's reality from outside the beneficiary set. Legislative practice in Muslim-majority reform states corroborates that the rules' force is contingent rather than timeless: Tunisia (1956) and Morocco (2004 Mudawana) abrogated or reweighted the differentiated provisions by statute without doctrinal or social collapse. No source outside the beneficiary juristic class attests the rules' timelessness — that attestation is the class's own, issued from the office the rules' continuing force constitutes.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the standing arrangement transfers inheritance shares, courtroom standing, and household authority from women to male kin while this reading can locate no live justification for the transfer in the canon — the justifying verses are, on this reading, already superseded, so the transfer persists as bare differential. Suppression (0.78) is authored as a raw structural value, unscaled by power or scope: it measures the enforcement machinery — charges of innovation (bid'ah), denial of teaching posts, exclusion of rival hermeneutics from official councils, social and in places legal sanction — that keeps egalitarian readings out of authoritative seats; only extractiveness is scaled downstream by directionality and scope. Theater ratio (0.45 and rising) reflects maintenance increasingly devoted to performing the rules' divine timelessness rather than administering a live need: the founding emergency (women with no standing at all) is historically resolved, so a growing share of the arrangement's activity defends authority over spent function. Accessibility collapse (0.5): alternatives do not fully seal — reform legislation (Tunisia 1956, Morocco 2004) and contextual readings remain articulable and partially operative, which is what keeps this a spent scaffold rather than a closed system. Resistance (0.6) is substantial and organized. The measurement series run on one shared grid — t=0..100 indexing 1920–2020 CE, the window in which the egalitarian alternative became articulable and post-sunset persistence became measurable — with every tracked metric authored at every point; all points are observed. Coalition note: the payer seats do not fuse into enforcement-breaking power — women under the rules are identity-locked to the very community the rules structure, and reformist scholars are institutionally marginal — so the numerical majority governed by the rules converts poorly into coalition pressure, which is why suppression holds at moderate-high rather than decaying under majority weight.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute different types from one structure. From the juristic seat the arrangement is the canon faithfully administered: the rules are divine ordinance, the abrogation reading is innovation, and enforcement is fidelity. From the women-under-the-rules seat the same structure operates as a transfer of property and standing enforced by the very authority it enriches. The reformist scholar occupies a third position: she accepts the canon's authority (unlike a secular critic) and rejects the class's reading of it — which is why her exit is constrained rather than trapped; she can leave the institutions but not the tradition her claim addresses. Same-nominal-level institutional actors diverge on exit: family-law courts are statute-bound (constrained), the juristic class is office-constituted (identity_locked — the madhhab has become its function, so abandoning the rules' force dissolves the office itself), and reformists are audience-bound (constrained). Identity-lock here is institutional and religious fusion, not economic trap: the classification would change if the juristic office could be re-founded on the egalitarian strata of the same canon — which is precisely this reading's wager.
 *
 * DIRECTIONALITY LOGIC:
 *   Male kin and guardians are structural beneficiaries: the rules subsidize their shares and standing at no cost to them, placing them near the beneficiary end of d; their exit would cost them only the subsidy, so they stay. The juristic class is agenda_setter with a beneficiary secondary role and identity_locked exit — derivation places it low-d, though not at zero: the arrangement collects maintenance labor and reputational risk from the class even as it pays it in authority. Women under classical law sit near the full-target end: they bear the transfer, and identity_lock (exit means leaving the faith community, family, and in places legal recognition) is what holds them there — the lock, not distance, is the binding constraint. Reformist scholars bear the suppression machinery directly (high d) while contesting from inside the tradition. Family-law courts administer without capturing: near-symmetric. No directionality_overrides were needed — the beneficiary/victim declarations plus exit atoms produce the correct relationship for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim with has_sunset_clause=true encodes this reading's central move and guards against misclassification in both directions. It does not let the arrangement's genuine historical coordination function be read as eternal pure extraction — the reading affirms that the verses solved a real 7th-century problem (women with no property or legal standing), which is why this is not a snare claim. And it does not let the coordination story launder the post-sunset persistence: the arrangement's justification was the transition toward the canon's own egalitarian endpoint, the sunset mechanism (naskh) is internal to the canon rather than imposed from outside, and persistence past the fired sunset is maintained by identity-locked authority rather than by need. mandatrophy_resolved is declared true: the mandate outlived its function. The R5 interview records the mismatch signature this reading asserts — founding_problem_status=dead against disappearance_verdict=world_rearranges: an arrangement whose founding problem is spent and whose disappearance would still rearrange family law across jurisdictions is exactly the zombie structure the mismatch consumer is designed to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel quranic_gender_verses — the progressive_abrogation reading. What would the sibling readings (literal_hierarchical, contextual_egalitarian) change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling constraint stories; the disagreement is located in two structural elements: whether naskh can operate between legal verses on the basis of general principles rather than explicit later rulings, and whether the gender-specific verses retain binding legal force at all.',
    'Under literal_hierarchical the victim set becomes permanent and the arrangement claims timeless ordinance, shifting the family toward mountain-claim dynamics with declared beneficiaries; under contextual_egalitarian the extraction is partial and remediable by interpretation rather than declaratively void, lowering measured extractiveness. This file''s high-epsilon scaffold verdict is valid only within this reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    naskh_principle_level_scope,
    'Does classical usul al-fiqh permit abrogation of explicit legal rulings by general moral principles (such as 49:13''s universal dignity), rather than only by explicit later rulings?',
    'Systematic usul analysis and a survey of how the naskh doctrine''s own classical exponents bounded its operation — the explicit-text requirement, the same-period objection, and the legal-vs-exhortative distinction.',
    'If principle-level naskh is rejected within the tradition''s own methodological rules, this reading collapses toward contextual_egalitarian and the sunset claim fails; if accepted, the gender-specific rules lose legal force outright and the arrangement''s persistence becomes indefensible on textual grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_principle_level_scope, conceptual, 'Whether the reading''s abrogation mechanism is available inside the interpretive tradition''s own rules.').

omega_variable(
    chronological_ordering_premise,
    'Is the egalitarian material (49:13 and the universal-dignity strata) chronologically later than the gender-specific legal verses (4:11, 2:282, 4:34), as the abrogation mechanism requires?',
    'Chronological reconstruction via the sira-maghhazi tradition, verse-stratification scholarship, and intra-Qur''anic cross-reference analysis.',
    'The abrogation mechanism is ordering-dependent: if the ordering fails or is undecidable, supersession-by-naskh loses its textual warrant and this reading''s distinctiveness from contextual_egalitarian collapses — both would then rest on reinterpretation rather than abrogation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronological_ordering_premise, empirical, 'The empirical chronological premise on which this reading''s mechanism rides.').

omega_variable(
    epistemic_cost_to_literal_communities,
    'Does adopting this reading impose its own costs on communities whose religious identity is bound to the literal reading — an epistemic price the reading''s self-account does not charge against itself?',
    'Ethnographic study of communities where the reading is introduced: identity disruption, trust in religious institutions, and who bears the cost of the normative reversal the reading demands.',
    'If the reversal''s costs concentrate on identity-bound believers rather than on the arrangement''s beneficiaries, the constraint family''s total cost structure is more symmetric than this reading''s account, and the reading''s own seat carries non-trivial directionality toward the family it proposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_to_literal_communities, conceptual, 'Whether the reading''s demanded reversal extracts from the communities it would transform.').

omega_variable(
    enforcement_site_ambiguity,
    'Is the standing arrangement''s enforcement juristic (identity-locked scholarly authority) or statutory (state codified family law), and does the agenda-setting seat differ across jurisdictions?',
    'Comparative family-law analysis across Muslim-majority jurisdictions distinguishing doctrine-driven enforcement (seminary networks, mufti councils) from statute-driven enforcement (codified personal-status codes).',
    'Where enforcement is statutory, the effective agenda_setter is the legislature — constrained exit, reformable by ordinary politics — and the cost of fixing drops; where juristic, the agenda_setter is office-constituted and identity-locked, and the cost of fixing remains prohibitive for the only seat that could effect it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_site_ambiguity, empirical, 'Which institutional site actually holds the arrangement''s enforcement, with consequences for fixing cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_prog_abrog_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(qgv_prog_abrog_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.28).
narrative_ontology:measurement(qgv_prog_abrog_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.33).
narrative_ontology:measurement(qgv_prog_abrog_tr_t60, quranic_gender_verses__progressive_abrogation, theater_ratio, 60, 0.38).
narrative_ontology:measurement(qgv_prog_abrog_tr_t80, quranic_gender_verses__progressive_abrogation, theater_ratio, 80, 0.42).
narrative_ontology:measurement(qgv_prog_abrog_tr_t100, quranic_gender_verses__progressive_abrogation, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(qgv_prog_abrog_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(qgv_prog_abrog_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(qgv_prog_abrog_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(qgv_prog_abrog_be_t60, quranic_gender_verses__progressive_abrogation, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(qgv_prog_abrog_be_t80, quranic_gender_verses__progressive_abrogation, base_extractiveness, 80, 0.86).
narrative_ontology:measurement(qgv_prog_abrog_be_t100, quranic_gender_verses__progressive_abrogation, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qgv_prog_abrog_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qgv_prog_abrog_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(qgv_prog_abrog_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(qgv_prog_abrog_su_t60, quranic_gender_verses__progressive_abrogation, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(qgv_prog_abrog_su_t80, quranic_gender_verses__progressive_abrogation, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(qgv_prog_abrog_su_t100, quranic_gender_verses__progressive_abrogation, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, resource_allocation).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, naskh_doctrine_in_usul_al_fiqh).

% DUAL FORMULATION NOTE:
% This story is one member of a three-reading constraint family decomposing the kernel quranic_gender_verses per the ε-invariance principle: literal_hierarchical (rules as timeless ordinance — mountain-claim dynamics with declared beneficiaries), contextual_egalitarian (rules as situated steps requiring maqasid reinterpretation — moderate, remediable extraction), and progressive_abrogation (this file — rules superseded by later egalitarian principles; very high extraction from a spent scaffold). The readings share a referent (the standing gender-differentiated arrangement) and differ in ε and victim structure; they are linked as siblings rather than merged because each instantiates a distinct constraint with a distinct beneficiary/victim structure. The upstream dependency naskh_doctrine_in_usul_al_fiqh supplies this reading's mechanism: if the doctrine cannot operate at principle level, this reading collapses into its contextual sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
