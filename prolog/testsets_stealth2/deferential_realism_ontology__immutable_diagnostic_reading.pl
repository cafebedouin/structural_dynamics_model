% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Immutable Diagnostic Reading: the Constraint Typology as Observational Instrument
 *   domain: epistemological/normative/institutional
 *
 * SUMMARY:
 *   This story instantiates the immutable_diagnostic_reading of the
 *   deferential_realism_ontology kernel: the institutional arrangement in
 *   which the constraint typology is wielded as an observational instrument
 *   with fixed referents, misclassification is treated as error correctable
 *   through better observation, and epsilon values are treated as discovered
 *   rather than constructed. The arrangement solves a genuine coordination
 *   problem — fixed referents make classifications comparable, cumulative,
 *   and correctable — and extracts real costs through the same structure:
 *   classified parties lose the right to contest on normative grounds, rival
 *   framings are recoded as observational errors, and governed communities
 *   enter the process only as data. Claim and metrics are authored
 *   independently: claimed_type=mountain records the arrangement's own
 *   self-presentation (its referent-structure claimed as discovered fact,
 *   naturality asserted via emerges_naturally — this is deliberate
 *   false-summit authoring, since the arrangement presents itself as 'not an
 *   arrangement at all, just correct methodology'); the metrics record the
 *   operation as descriptively assessed; the divergence between the claim and
 *   the engine's per-seat computation is the datum this story exists to take.
 *   The sibling readings are separate constraint stories linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - classification_authorities: agenda-setter and primary beneficiary (institutional power, identity_locked exit) — issue verdicts and adjudicate disputes by metric appeal; the observational frame constitutes their authority, so abandoning it dissolves their role
 *   - favorably_classified_arrangements: beneficiary (powerful, arbitrage exit) — collect naturalized verdicts that present their arrangements as inevitable or as mere coordination cost
 *   - adversely_classified_parties: primary target (powerful, constrained exit) — bear the condemnatory verdicts and may contest only on metric terrain the operator controls
 *   - rival_reading_theorists: secondary target (moderate, constrained exit) — hold the sibling readings; their framings are recoded as their own observational errors
 *   - governed_communities: deepest target (powerless, trapped) — enter the process only as metric data; no seat from which their objection is hearable as an objection
 *   - legitimacy_theorists: excluded (moderate, mobile exit) — their founding question (whose benefits make a classification authoritative?) is ill-formed inside the frame
 *   - meta_analytical_reviewers: analytical observer (institutional, analytical exit) — track the gap between the apparatus's reading-index rulings and the discovery-framed practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.63).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.75).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable Diagnostic Reading: the Constraint Typology as Observational Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemological/normative/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).
domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '8c349a95-8803-440c-943f-00cfa4f67bd8').
narrative_ontology:cs_kernel_codification('8c349a95-8803-440c-943f-00cfa4f67bd8', formalized).
narrative_ontology:cs_authority_grounding('8c349a95-8803-440c-943f-00cfa4f67bd8', extraction).
narrative_ontology:cs_interpretation_layer_present('8c349a95-8803-440c-943f-00cfa4f67bd8').
narrative_ontology:cs_reading_relation('8c349a95-8803-440c-943f-00cfa4f67bd8', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('8c349a95-8803-440c-943f-00cfa4f67bd8', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('8c349a95-8803-440c-943f-00cfa4f67bd8', foundational, classification_verdicts_are_discoveries).
narrative_ontology:cs_axiom_status(classification_verdicts_are_discoveries, holdable).
narrative_ontology:cs_axiom_grounding('8c349a95-8803-440c-943f-00cfa4f67bd8', classification_verdicts_are_discoveries, empirically_contingent).
narrative_ontology:cs_axiom('8c349a95-8803-440c-943f-00cfa4f67bd8', foundational, misclassification_is_observational_error).
narrative_ontology:cs_axiom_status(misclassification_is_observational_error, holdable).
narrative_ontology:cs_axiom_grounding('8c349a95-8803-440c-943f-00cfa4f67bd8', misclassification_is_observational_error, empirically_contingent).
narrative_ontology:cs_reference_frame('8c349a95-8803-440c-943f-00cfa4f67bd8', fixed_referent_observational_instrument).
narrative_ontology:cs_drift_state('8c349a95-8803-440c-943f-00cfa4f67bd8', post_reading_index_ruling, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8c349a95-8803-440c-943f-00cfa4f67bd8', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, classification_authorities).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, favorably_classified_arrangements).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, adversely_classified_parties).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, rival_reading_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, governed_communities).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, observational_neutrality_doctrine).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_discoverability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the classification instrument: issue verdicts on submitted constraint stories, adjudicate classification disputes by appealing to the observable metrics, and maintain the recoding rule under which a framing objection re-enters the process as a candidate measurement error. Their verdicts are received by the surrounding practice as reports rather than judgments. The authority of their role is constituted by the observational frame; if the frame were abandoned and verdicts became openly contestable commitments, the role's standing would dissolve with it, so leaving the frame is not a live option from inside.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, classification_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, classification_authorities, beneficiary).

% Institutions and arrangements whose operations receive the instrument's benign verdicts — the physical-invariant category and the pure-coordination category. A verdict of that kind presents their arrangement as inevitable or as simple coordination cost, and reform pressure deflates accordingly. They need not defend the instrument, rarely engage the dispute protocol at all, and can exit the discourse without material cost whenever it suits them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, favorably_classified_arrangements, beneficiary,
    powerful, biographical, arbitrage, global).

% Parties whose arrangements receive the instrument's condemnatory verdicts — the categories denoting coercive, victim-producing mechanisms. The verdict carries the framework's full normative force, but the frame admits no normative defense: arguing that their beneficiaries are legitimate is out of order, because within the fixed referents the condemnatory categories are defined by measurable harm rather than by beneficiary standing. Their only admissible move is litigating the metrics, on terrain the instrument's operator controls, with the operator adjudicating.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, adversely_classified_parties, payer,
    powerful, biographical, constrained, global).

% Theorists who hold the sibling readings of the typology — that its categories are declared normative vocabulary, or that its periphery is normatively contested. Inside this practice their positions are not engaged as alternative commitments; they are recoded as observational errors attributable to the theorist, which converts their disagreement into a personal defect. Exiting the discourse costs them their standing within constraint analysis, so they remain inside a process structured to receive their work as error reports.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rival_reading_theorists, payer,
    moderate, biographical, constrained, global).

% Communities living under the arrangements the instrument classifies. Their first-person testimony about the arrangements that govern them enters the classification process only as raw observational data — a reading on a metric — never as normative claim about what is being done to them. They cannot exit the arrangements under classification, and the epistemic protocol that adjudicates those arrangements has no seat from which their objection could be heard as an objection.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, governed_communities, payer,
    powerless, generational, trapped, national).

% Theorists who hold that a classification's authority over its subject requires normative adjudication of whose benefits count as legitimate. Under the observational frame their founding question is ill-formed — there is no step in the dispute protocol at which it could be asked — so they stand entirely outside the conversation, with no procedural route back in short of reframing the instrument itself.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, legitimacy_theorists, excluded,
    moderate, biographical, mobile, global).

% The apparatus's methodological layer: they track cross-cohort and cross-reading divergence in the corpus and maintain the ruling that epsilon is a property of a reading rather than a topic. They observe the gap between that ruling and a classification practice that still proceeds as if verdicts were discoveries, but they issue no classifications of their own and hold no enforcement seat.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, meta_analytical_reviewers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, classification_authorities).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the referents of a shared classificatory vocabulary so that constraint analyses can be compared, accumulated, and corrected across analysts and cohorts: with the invariant and coercive-mechanism categories fixed as physical invariants and measurable harm respectively, a classification dispute becomes a tractable question about measurement rather than an unbounded renegotiation of terms.
% TRANSFER_FUNCTION: Moves framing authority and contest rights from classified parties, rival-reading theorists, and governed communities to the classification authority: once a verdict issues as an observation, it can be contested only on observational grounds, on terrain the instrument-holder controls, and normative objection re-enters the process recoded as error.
% ABSENT_VOICES: Legitimacy theorists are absent — under the observational frame their question (whose benefits make a classification authoritative?) is ill-formed and has no seat in the dispute protocol. Governed communities are present only as metric data: their first-person normative testimony about the arrangements that govern them is inadmissible as claim. Rival-reading theorists are present but recoded — their objections enter as candidate errors, never as alternative commitments.
% DISAPPEARANCE_RATIONALE: If classifications ceased to be received as observations overnight, every settled verdict would reopen as a contestable framing commitment: adversely classified parties would litigate beneficiary legitimacy rather than metrics, favorably classified arrangements would lose the naturalized shield, the authority structure would have to renegotiate its legitimacy explicitly, and the rhetorical and hybrid readings would compete in the open rather than surviving as recoded errors.
% FOUNDING_PROBLEM: Early constraint classification suffered from unconstrained framing: verdicts that tracked the classifier's preferences, referents renegotiated mid-dispute, and disagreements that could never terminate because every party could redefine what the categories denoted. The immutable-diagnostic arrangement was built to make classifications falsifiable and disputes terminable — fix the referents, treat divergence as measurement error, and let an error-correction loop operate.
% FOUNDING_PROBLEM_CORROBORATION: The reality of the founding problem is corroborated from outside the benefiting parties by the philosophy-of-science record on operationalism and theory-ladenness (unconstrained framing is a standing, externally documented failure mode) and by the apparatus's own methodological history, which produced the epsilon-invariance principle precisely because unconstrained framing generated incoherent classifications. The status is contested: the apparatus's reading-index rulings attest from within the methodological layer that the discovery-framed solution overreaches, while adversely classified parties and rival-reading theorists attest from outside that the arrangement now functions as authority insulation rather than error correction.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, ExtMetricName, E),
    domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is mountain because that is the arrangement's own claim about itself: the immutable-diagnostic reading presents the typology's referent-structure as discovered fact and the classification protocol as the natural form of correct classification rather than one constructed arrangement among alternatives. My independent structural assessment, stated here and encoded in the honestly-authored metrics, is that the arrangement is a tangled rope — a genuine referential-discipline core carrying asymmetric extraction under active enforcement — and the engine's computation tests that against the claim. Extractiveness 0.63: contest rights, framing authority, and normative voice move to the authority seat while riding on a real coordination function. Suppression 0.75, raw and unscaled by power or scope: the recoding rule (rival framings enter only as candidate errors), protocol and venue gatekeeping, and fail-closed enforcement machinery. Theater ratio 0.42: metric appeals partly measure and partly launder antecedent framing commitments. Accessibility collapse 0.48: the sibling readings remain live — the kernel itself declares them — but are unsayable as commitments inside the practice. Resistance 0.62: sibling holdings and the apparatus's own reading-index rulings press against the discovery premise. The measurement series share one grid (0, 4, 8, 12, 16, 20, 24); all three tracked metrics rise monotonically — enforcement machinery hardened over the interval, which is why suppression_requirement is tracked as a series rather than left to the scalar. fixing_cost is prohibitive for the seat that could fix it: abandoning the discovery frame reopens every settled classification and dissolves the authority the fixer's role is constituted by, while the gains from fixing accrue to other seats.
 *
 * PERSPECTIVAL GAP:
 *   From the classification-authority seat the arrangement computes as a well-functioning measurement protocol — referential discipline, genuine error correction, minimal coercion. From the adversely classified and rival-reading seats the same structure computes as enforced extraction of contest rights. From the governed-community seat — trapped, voiceless except as data — extraction is deepest. The engine computes this per-seat divergence from the structural data; the authored mountain claim does not adjudicate it. The powerless seat's coalition route — governed communities allying with rival-reading theorists to build an alternative dispute protocol — is real but is itself caught by the recoding rule, which renders such coalitions legible only as error-clusters; that circularity is the suppression operating at the level where coalition power would otherwise form.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: classification_authorities collect framing authority and insulation from normative contest; their identity_locked exit (the observational frame constitutes their role's authority) keeps them inside and sustaining the arrangement. favorably_classified_arrangements collect naturalized verdicts that shield them from reform pressure; their arbitrage-grade exit places them nearest the beneficiary end. Targets: adversely_classified_parties may contest only on metric terrain the instrument-holder controls (constrained exit, high d); rival_reading_theorists have their framings recoded as their own observational errors (constrained, high d); governed_communities enter only as data and cannot exit the arrangements under classification (trapped, highest d). The epsilon-as-discovered commitment is the extraction mechanism itself: it converts normative contest into observational error, routing all dispute traffic onto terrain where the beneficiary seat holds the home advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained framing, non-terminating classification disputes — was real and is corroborated from outside the beneficiary set. The arrangement solved it, and the solution's extraction grew as the apparatus matured: extraction accumulation on a structure claimed as natural is the false-summit temporal signature. The mandatrophy question is live, not settled: referential discipline remains necessary (the founding problem is not dead), but the discovery-framed form of the solution is contested by the apparatus's own reading-index rulings. The per-seat computation prevents the two symmetrical mislabels: reading the arrangement as a genuine mountain (which would immunize the extraction as methodological necessity) or as a pure snare (which would discard the referential discipline even the sibling readings concede). founding_problem_status=contested with disappearance_verdict=world_rearranges is a coherent pairing, not a mismatch flag: the problem is partly live, and the world's classification practice does depend on the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This story instantiates the immutable_diagnostic_reading of the deferential_realism_ontology kernel; how would the structural classification change under the sibling readings (rhetorical_scaffold_reading, hybrid_pragmatic_reading) of the same kernel?',
    'Author the sibling stories over the shared referent and compare computed types and directionality structures across the constraint family.',
    'Under the rhetorical reading the extraction is overt (declaration is an open act of framing authority) and the coordination claim weakens, likely shifting the computed type toward pure extraction; under the hybrid reading the periphery''s normative contestability is admitted and part of the enforcement structure legitimates, likely softening the fixed core toward pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Classification is contingent on which reading of the typology kernel this story instantiates.').

omega_variable(
    epsilon_discoverability_dispute,
    'Is epsilon a discovered property of the classified arrangement (this reading''s commitment) or a property of a reading, constructed per seat (the apparatus''s settled reading-index ruling)?',
    'Cross-seat and cross-cohort convergence analysis: if analysts at the same seat converge on epsilon without shared framing commitments, discovery is supported; if epsilon tracks framing, construction is supported.',
    'If discovered, the metric-appeal dispute protocol is genuine error correction and the measured suppression is methodological discipline; if constructed, the protocol launders framing authority, measured extraction rises, and the arrangement''s coordination claim covers less of its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discoverability_dispute, conceptual, 'The discoverability of epsilon is the precise structural element on which the kernel''s readings disagree.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (dispute-protocol design, venue and review gatekeeping, the recoding rule) or internalized (analysts so formed inside the observational frame that rival framings no longer occur to them as options)?',
    'Post-exit suppression trajectory: track analysts who leave the classification-authority orbit — if they spontaneously generate rival framings, internalization was binding; if they continue to defer to metric authority, the structure was binding.',
    'If internalized, effective suppression exceeds the structural measure and persists after any protocol reform — deregating the dispute protocol would not restore framing pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative framings.').

omega_variable(
    dispute_termination_genuineness,
    'Does resolution-by-metric-appeal actually terminate classification disputes, or relocate them to the next metric layer?',
    'Dispute-history analysis: track whether metric-appeal resolutions hold durably or recur at a finer metric grain.',
    'If disputes recur, the theater ratio is understated, the error-correction loop is largely performative, and the arrangement drifts toward theatrical maintenance; if they terminate, the coordination function is stronger than the extraction reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_termination_genuineness, empirical, 'Whether the error-correction loop corrects or merely relocates.').

omega_variable(
    referent_fixation_naturalness,
    'Is the fixed-referent structure (the invariant category as physical invariant, the coercive-mechanism category as measurable harm) a discovered joint of the epistemic domain, or a constructed stabilization — fixed by a coordination event among the framework''s founders — that has hardened into seeming naturalness and now benefits the agents who administer it?',
    'Genealogy of the referent-fixation: archival analysis of how the categories were operationalized, who participated, and what alternatives were set aside at fixation; comparison with classification practices in adjacent disciplines.',
    'If constructed, the mountain claim fails on its own terms, the false-summit reclassification proceeds, and the arrangement''s extraction becomes legible as extraction rather than methodological necessity; if discovered, part of the measured suppression is warranted referential discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referent_fixation_naturalness, conceptual, 'Naturality versus constructedness of the fixed referents — the natural-law-versus-constructed ambiguity the mountain claim carries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the deferential_realism_ontology kernel decomposes into three reading-stories over one shared referent (the constraint typology as actually wielded), each with its own reading-indexed epsilon per the apparatus's reading-index rulings: this story (immutable_diagnostic_reading — the typology as observational instrument, epsilon discovered), rhetorical_scaffold_reading (the typology as normative vocabulary, coercive-category membership declared), and hybrid_pragmatic_reading (fixed core, normatively contested periphery). Edges run from this story to both siblings because this reading's enforcement structure — the recoding of rival framings as observational error — sets the operating conditions under which the siblings can be held; the upstream discovery-claim is what the siblings' holders must argue against. The epsilon values differ by reading while the referent stays fixed: the family is the corpus's standing test of the claim that epsilon is a property of a reading, not a topic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
