% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)
 *   domain: religious/theological
 *
 * SUMMARY:
 *   This constraint isolates the symbolic-confessional reading of the Nicene
 *   Creed's authority: the creed as a historically contingent, communally
 *   negotiated witness whose authority rests on ongoing community discernment
 *   and the believer's personal faith rather than on binding propositional
 *   metaphysics enforced by hierarchy. This reading emerged with force
 *   alongside historical-critical scholarship and ecumenical/liturgical
 *   renewal movements from the mid-twentieth century onward (Vatican II's
 *   aggiornamento currents, mainline Protestant historical theology, some
 *   strands of post-liberal and revisionist theology), and has settled into a
 *   stable, low-extraction equilibrium in communities that adopt it. It is
 *   one of three structurally distinct constraints sharing the label 'the
 *   Nicene Creed's authority' — the strict orthodox reading (binding
 *   metaphysical assent, sanctionable heresy) and the liturgical habituation
 *   reading (identity marker through performance, independent of cognitive
 *   assent) are separate constraints with their own ε and stakeholder
 *   structure, linked here via network.affects_constraints. Per the
 *   ε-invariance principle, these are not three measurements of one
 *   constraint but three constraints sharing a kernel.
 *
 * KEY AGENTS:
 *   - local_congregations: primary beneficiary (moderate/mobile) — gain interpretive latitude
 *   - progressive_theologians: beneficiary (organized/mobile) — gain scholarly and pastoral freedom
 *   - interfaith_dialogue_partners: beneficiary (moderate/mobile) — gain a non-absolutist basis for engagement
 *   - lay_believers_seeking_personal_faith: beneficiary (powerless/mobile) — gain room for doubt and idiosyncratic belief
 *   - centralized_denominational_authorities: primary payer (institutional/constrained) — lose disciplinary leverage
 *   - creedal_subscription_boards: payer (organized/constrained) — lose examination rationale
 *   - historical_theologians: analytical observer — documents the councils' contingent formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.19).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.14).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.19).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "religious/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, '64eefb0d-59b7-4a40-9505-dcb50bcc97e9').
narrative_ontology:cs_kernel_codification('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', fixed_text).
narrative_ontology:cs_authority_grounding('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', practice).
narrative_ontology:cs_interpretation_layer_present('64eefb0d-59b7-4a40-9505-dcb50bcc97e9').
narrative_ontology:cs_reading_relation('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', nicene_creed_authority__strict_orthodox_reading, forecloses).
narrative_ontology:cs_reading_relation('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', foundational, creedal_authority_grounded_in_community_discernment).
narrative_ontology:cs_axiom_status(creedal_authority_grounded_in_community_discernment, holdable).
narrative_ontology:cs_axiom_grounding('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', creedal_authority_grounded_in_community_discernment, conventional).
narrative_ontology:cs_axiom('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', foundational, doctrinal_formulations_are_historically_conditioned_and_revisable).
narrative_ontology:cs_axiom_status(doctrinal_formulations_are_historically_conditioned_and_revisable, holdable).
narrative_ontology:cs_axiom_grounding('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', doctrinal_formulations_are_historically_conditioned_and_revisable, empirically_contingent).
narrative_ontology:cs_reference_frame('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', conciliar_negotiated_consensus_325_381).
narrative_ontology:cs_drift_state('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', post_historical_critical_scholarship_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('64eefb0d-59b7-4a40-9505-dcb50bcc97e9', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, lay_believers_seeking_personal_faith).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_denominational_authorities).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, creedal_subscription_boards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite or reference the creed as a shared historical touchstone and worship resource rather than a binding metaphysical test. They retain latitude to interpret its clauses symbolically, poetically, or provisionally, and can adapt liturgical use to local pastoral needs without appeal to a higher tribunal.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    moderate, generational, mobile, regional).

% Use the creed's historically contingent status to argue for doctrinal development, reinterpretation of terms like 'begotten' or 'consubstantial' in light of modern philosophy of language, and continued theological work without fear of heresy charges attaching to their scholarship.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, progressive_theologians, beneficiary,
    organized, generational, mobile, national).

% Engage Christian communities in dialogue where the creed is presented as a culturally situated witness rather than an absolute claim excluding other traditions' truth-claims; this reading lowers the barrier to mutual recognition and joint action.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners, beneficiary,
    moderate, biographical, mobile, global).

% Hold personal doubts or idiosyncratic understandings of the Trinity or Incarnation without being required to affirm precise ontological formulations as a condition of belonging or participation in worship life.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, lay_believers_seeking_personal_faith, beneficiary,
    powerless, biographical, mobile, local).

% Lose the disciplinary leverage the creed once provided: subscription requirements, ordination exams, and heresy trials built on strict propositional assent become harder to enforce when the creed is treated as symbolic witness. Their historic role as guarantors of doctrinal uniformity is structurally weakened by this reading's spread.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_denominational_authorities, payer,
    institutional, civilizational, constrained, global).

% Administer subscription and examination processes for clergy that presuppose the creed states settled metaphysical fact. When candidates or sitting clergy adopt the symbolic-confessional reading, the boards' examination criteria lose their evidentiary force and their institutional purpose is contested from within.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, creedal_subscription_boards, payer,
    organized, generational, constrained, national).

% Study the fourth-century councils, the political and linguistic contingencies (homoousios vs. homoiousios, imperial involvement at Nicaea and Constantinople) that produced the creed's specific wording, and document how later communities have read authority into a text whose formation was itself contested and negotiated.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__symbolic_confessional_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__symbolic_confessional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared historical and liturgical touchstone that lets diverse congregations recognize continuity with the ancient church and with one another, without requiring uniform metaphysical assent as the price of fellowship.
% TRANSFER_FUNCTION: Moves interpretive authority away from centralized doctrinal tribunals and toward local congregations and individual conscience; what is 'transferred' is legitimacy and discretion, not material resources — congregations and theologians gain latitude that subscription boards and denominational hierarchies previously held exclusively.
% ABSENT_VOICES: Adherents of the strict orthodox reading would object that this reading empties the creed of binding force and effectively abandons the fourth-century church's intent; they are present in the wider kernel contest but excluded from this particular reading's own framework by construction, since the two readings rest on incompatible premises about what kind of claim the creed makes.
% DISAPPEARANCE_RATIONALE: If this reading vanished, congregations and theologians who rely on it to hold personal faith and pursue theological development without institutional censure would lose protective cover, and interfaith engagement premised on non-absolutist framing would become harder; denominational authorities would regain disciplinary leverage. Whether this counts as 'the world rearranging' depends on which seat is asked — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The historical councils (Nicaea 325, Constantinople 381) were convened to resolve a live, unsettled christological and trinitarian dispute (Arianism vs. proto-orthodox positions) that threatened ecclesial and imperial unity; the creed was a negotiated formula meant to end that specific controversy.
% FOUNDING_PROBLEM_CORROBORATION: Historical theologians and historians of late antiquity (outside any confessional beneficiary group) corroborate that the councils were politically brokered, linguistically contested events rather than simple transcriptions of settled apostolic teaching — this is the standard critical-historical account. Strict orthodox authorities dispute this framing and hold the founding problem to be permanently and correctly resolved by conciliar authority; centralized denominational bodies that depend on the creed's binding force have not independently corroborated the symbolic-confessional account and would not be expected to.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, contested).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.19, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.19, within the expected 0.11-0.30 band) because this reading's own structure minimizes coercive extraction: no one is compelled to affirm a specific metaphysical content as a condition of belonging, and no material or status transfer flows from payer to beneficiary beyond a reallocation of interpretive legitimacy. Suppression is correspondingly low (0.14) — the reading's whole point is to reduce coercive enforcement of assent. Theater ratio is moderate (0.28) because the creed is still recited liturgically in many communities holding this reading, and some of that recitation is now performative/heritage-preserving rather than doctrinally load-bearing, which the reading itself acknowledges rather than conceals. Accessibility collapse is low (0.22): alternative theological framings remain genuinely available and are actively pursued. Resistance is moderate (0.35): strict orthodox and traditionalist actors within the same institutions actively contest this reading, which is precisely the kernel dispute this story is one reading of.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (local congregations, progressive theologians, interfaith partners, lay believers) sit near the beneficiary end of directionality because the reading removes a cost (compulsory assent, risk of censure) that would otherwise fall on them under a stricter reading — this is the inverted topology named in the expected structural delta. Victims (centralized denominational authorities, creedal subscription boards) sit nearer the target end not because material extraction is taken from them, but because their institutional function and legitimacy depend on a form of creedal authority this reading structurally erodes; the 'cost' they bear is loss of disciplinary capacity and existential institutional purpose, which the engine's directionality derivation should register as asymmetric structural effect even absent a monetary transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving a specific fourth-century christological dispute to preserve ecclesial and imperial unity) is genealogically distinct from the ongoing function some institutions now assign the creed (perpetual metaphysical boundary-policing). This reading explicitly treats the founding problem as substantially resolved/historically bounded rather than perpetually live, which is why founding_problem_status is authored as contested rather than live: the reading itself holds the problem was time-bound, while institutions built on stricter readings treat it as permanently unresolved and requiring continuous vigilance. Classifying this as a low-extraction rope (rather than collapsing it into a single verdict about 'the creed') prevents mislabeling either (a) all creedal use as extractive control, or (b) all resistance to reinterpretation as illegitimate rent-seeking by threatened authorities — the classification is reading-specific, not creed-specific.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_criterion,
    'What determines which reading of the creed''s authority a given community or theologian adopts — is it prior commitment to historical-critical method, denominational tradition, personal religious experience, or some combination, and is that selection itself principled or arbitrary relative to the kernel text?',
    'Comparative ethnographic and historical-theological study of communities that have shifted between readings (e.g., mainline Protestant denominations post-1960s), tracing what triggered the shift and whether it correlates with institutional incentives, theological education patterns, or independent doctrinal reasoning.',
    'If reading-selection tracks institutional self-interest (e.g., authorities adopt whichever reading currently maximizes their control) rather than principled theological reasoning, that would suggest the kernel dispute is partly a proxy for institutional power contests rather than purely doctrinal disagreement — relevant to interpreting the victim-seat''s resistance as principled versus self-interested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether adoption of this reading tracks principled theological reasoning or institutional interest.').

omega_variable(
    historical_contingency_vs_revealed_truth,
    'Is the creed''s formation being ''historically contingent'' (shaped by fourth-century politics, language, and controversy) compatible with it also expressing a truth that transcends that contingency, or does establishing contingency of form necessarily weaken any claim to binding content?',
    'This is a philosophical/theological question not resolvable by historical evidence alone — it depends on prior commitments about revelation, doctrine development, and the relationship between historical process and theological truth-claims (analogous to debates in philosophy of science about theory-ladenness not undermining truth-aptness).',
    'If contingency of formation is compatible with binding truth-content, the symbolic-confessional reading''s inference from ''historically contingent'' to ''authority derives from discernment/faith rather than binding assent'' is a further normative step, not an entailment — weakening this reading''s claim to be simply reading off what the historical facts show and revealing it as itself a theological position competing with the strict orthodox reading on the same contested ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_contingency_vs_revealed_truth, conceptual, 'Whether historical contingency of the creed''s formation entails non-binding authority, or is a separate normative inference.').

omega_variable(
    institutional_survival_pressure,
    'Do centralized denominational authorities and subscription boards experience this reading''s spread as an existential threat requiring active resistance, or have most already accommodated it through informal non-enforcement while retaining formal subscription requirements as theater?',
    'Survey of actual heresy/discipline proceedings and subscription enforcement rates across denominations claiming Nicene authority, compared against rates of clergy privately holding non-strict interpretations.',
    'If formal requirements persist mostly as unenforced theater, the authored theater_ratio (0.28) may be understated and the victim seat''s structural cost may be lower than modeled, shifting the constraint closer to a piton-adjacent profile for the authority seat specifically even while remaining a rope overall for the beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_survival_pressure, empirical, 'Whether institutional resistance to this reading is substantively enforced or largely performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t1965, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1965, 0.33).
narrative_ontology:measurement(nice_tr_t1975, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1975, 0.31).
narrative_ontology:measurement(nice_tr_t1985, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(nice_tr_t1995, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1995, 0.29).
narrative_ontology:measurement(nice_tr_t2005, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(nice_tr_t2015, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t1965, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1965, 0.26).
narrative_ontology:measurement(nice_be_t1975, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1975, 0.24).
narrative_ontology:measurement(nice_be_t1985, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(nice_be_t1995, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement(nice_be_t2005, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2005, 0.19).
narrative_ontology:measurement(nice_be_t2015, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2015, 0.19).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2025, 0.19).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__symbolic_confessional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Nicene Creed's authority' per the ε-invariance principle. strict_orthodox_reading authors high extraction with centralized authorities as beneficiary and dissenting clergy/laity as victims — the inverse topology of this story. liturgical_habituation_reading authors a distinct low-to-moderate extraction constraint keyed to ritual participation and identity-boundary maintenance rather than either metaphysical assent or historical-critical discernment, with its own beneficiary/victim structure centered on liturgical communities versus non-conforming worshippers. All three share the same underlying text (the 325/381 conciliar formula) but instantiate structurally distinct claims about where its authority comes from and who it binds; none is a 'view' of the others requiring averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
