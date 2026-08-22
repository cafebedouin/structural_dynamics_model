% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity: National Primacy Reading
 *   domain: international/legal/political
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity: the ICC has
 *   jurisdiction only when national courts are 'unwilling or unable' to
 *   prosecute. This constraint story instantiates the national primacy
 *   reading — the interpretation that places the burden on the ICC prosecutor
 *   to affirmatively prove inadequacy, grants presumptive deference to
 *   national proceedings even when they are slow or selective, and
 *   prioritizes state sovereignty over victim access to international
 *   justice. The sibling reading (international_oversight_reading) interprets
 *   the same text as a trigger for ICC intervention whenever national
 *   proceedings are indifferent to victim protection or elite accountability.
 *   Both readings cite the same Rome Statute text; they differ in what
 *   'unwilling or unable' means operationally and who bears the burden of
 *   proof.
 *
 * KEY AGENTS:
 *   - national_judiciaries: Presumptive primary actors, benefit from deference
 *   - sovereignty_maximizing_states: Beneficiaries of high inadmissibility threshold
 *   - ICC prosecutor: Pays the burden to overcome presumption
 *   - victims_in_weak_state_proceedings: Trapped outside ICC reach unless sham is proven
 *   - transnational_accountability_advocates: Structurally disadvantaged by the constraint's design
 *   - rome_statute_framers: Authorial seat (original intent supports national primacy reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.72).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity: National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international/legal/political").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '4cf60503-36de-464e-aa18-da55bfb96fc5').
narrative_ontology:cs_kernel_codification('4cf60503-36de-464e-aa18-da55bfb96fc5', fixed_text).
narrative_ontology:cs_authority_grounding('4cf60503-36de-464e-aa18-da55bfb96fc5', lineage).
narrative_ontology:cs_interpretation_layer_present('4cf60503-36de-464e-aa18-da55bfb96fc5').
narrative_ontology:cs_reading_relation('4cf60503-36de-464e-aa18-da55bfb96fc5', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('4cf60503-36de-464e-aa18-da55bfb96fc5', foundational, state_sovereignty_primacy_in_justice).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_justice, holdable).
narrative_ontology:cs_axiom_grounding('4cf60503-36de-464e-aa18-da55bfb96fc5', state_sovereignty_primacy_in_justice, deontological).
narrative_ontology:cs_axiom('4cf60503-36de-464e-aa18-da55bfb96fc5', foundational, presumption_of_national_adequacy).
narrative_ontology:cs_axiom_status(presumption_of_national_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('4cf60503-36de-464e-aa18-da55bfb96fc5', presumption_of_national_adequacy, conventional).
narrative_ontology:cs_reference_frame('4cf60503-36de-464e-aa18-da55bfb96fc5', state_sovereign_jurisdiction_primacy).
narrative_ontology:cs_drift_state('4cf60503-36de-464e-aa18-da55bfb96fc5', contemporary_accountability_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cf60503-36de-464e-aa18-da55bfb96fc5', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_state_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, transnational_accountability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, powerful_states_with_weak_courts).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, post_conflict_national_judiciaries).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_prosecutor).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, post_conflict_national_judiciaries).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, state_sovereignty_primacy_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_principle_in_international_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary jurisdiction over crimes committed on their territory and by their nationals. Under this reading, national courts receive presumptive deference so long as proceedings exist and show minimal signs of unwillingness or sham. They set the operational threshold for what constitutes 'genuine' proceedings and what 'unwilling or unable' means in practice. They benefit from insulation from international oversight even where proceedings are dilatory, selective, or elite-protective.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% Maintain control over internal accountability mechanisms and resist external judicial intrusion into what they define as domestic affairs. The high ICC burden to demonstrate inadmissibility shields them from international prosecution of state officials and military personnel. They benefit from the constraint's gatekeeping effect: only states with complete judicial collapse or manifest sham proceedings face ICC intervention.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, global).

% Operates under a demanding standard: must affirmatively prove that national courts are unwilling (refusing to prosecute) or unable (lacking capacity). The burden is structural and high. Must gather evidence that national proceedings are shams — a costly, time-consuming, politically fraught investigation that often requires cooperation from the very state whose courts are being challenged. The constraint extracts investigative resources and constrains case selection.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, payer,
    institutional, biographical, constrained, global).

% Seek accountability in post-conflict or transitional states where courts exist but function poorly: slow trials, elite immunity, political pressure on judges, inadequate victim participation. Under this reading, they fall outside ICC reach unless their home state's proceedings are proven sham — a high bar. Their alternative is domestic justice systems they do not trust, or no justice at all. They cannot petition the ICC directly; they depend on state referral or the prosecutor's affirmative inadmissibility determination.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_state_proceedings, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, victims_in_weak_state_proceedings, excluded).

% Push for broad ICC jurisdiction and aggressive application of complementarity to close impunity gaps. They argue that genuine proceedings require substantive fairness, timely resolution, and victim participation — not mere process existence. The constraint extracts legitimacy from their advocacy: the prosecutor must invoke their evidence and moral framing to overcome the presumption in favor of national courts, yet the constraint's design systematically disadvantages their position.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, transnational_accountability_advocates, payer,
    organized, generational, constrained, global).

% Hold the sibling reading: that complementarity should interpret 'unwilling or unable' broadly, making the ICC an active guardian against impunity. They are excluded from the operational definition of what counts as adequate national proceedings under the national primacy reading. Their reading would permit ICC intervention in cases of selective prosecution or victim exclusion, which this reading does not.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_oversight_advocates, excluded,
    organized, generational, constrained, global).

% Derive asymmetric protection: they have political leverage to ensure their national proceedings are deemed genuine and adequate, even where selective prosecution or political bias is evident. Their power buys them the benefit of the doubt under the presumption; weaker states' proceedings are scrutinized more heavily. They benefit from the constraint while their nationals enjoy effective immunity through controlled prosecutions.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, powerful_states_with_weak_courts, beneficiary,
    powerful, generational, mobile, national).

% Attempt to build transitional justice mechanisms in fragile states emerging from conflict. They benefit from the presumption of adequacy (legitimacy, avoiding ICC shadow jurisdiction) but pay through resource constraints, political pressure to shield elites, and victim dissatisfaction. They are caught between the constraint's requirement to maintain genuine proceedings and domestic political incentives to grant amnesties or selective immunity. Their identity as 'national court' commits them to the institutional form even where capacity is inadequate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, post_conflict_national_judiciaries, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, post_conflict_national_judiciaries, beneficiary).

% Authored the Rome Statute's complementarity principle as a political compromise protecting state sovereignty while creating an international backstop. This reading instantiates their intended weight on state primacy; the sibling reading inverts their hierarchy. The framing texts and negotiation records support the national primacy reading, though subsequent practice and jurisprudence have drifted.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, rome_statute_framers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for international criminal accountability between national and international systems: national courts handle day-to-day prosecutions; ICC operates only when national systems genuinely fail or refuse. Solves the coordination problem of preventing both impunity (ICC backstop) and forum-shopping (national primacy presumption) in a two-tier system.
% TRANSFER_FUNCTION: Transfers jurisdiction and legitimacy FROM the ICC TO national courts by default. The constraint moves the burden of proof: unless proven otherwise, cases belong in national systems, not international ones. It transfers legitimacy cost to the ICC prosecutor, who must expend political capital and investigative resources to overcome the presumption. It transfers immunity benefit to states with functioning (even if imperfect) national proceedings.
% ABSENT_VOICES: Victims in weak-but-genuine national proceedings are institutionally excluded: they cannot petition the ICC directly and their home state rarely refers cases to it. Transnational accountability advocates are excluded from the operational definition of 'adequate proceedings': their arguments for victim participation, timely justice, and non-selective prosecution do not alter the presumption. Post-conflict judicial reformers are partially excluded: their voices advocate for international support and ICC presence as a legitimacy device, not its exclusion.
% DISAPPEARANCE_RATIONALE: If the presumption of national primacy disappeared and complementarity became actively prosecutorial, the ICC docket would expand dramatically; states would face deterrent pressure to restructure national proceedings or restrict ICC jurisdiction; South-South cooperation on referrals would shift; and post-conflict states would experience either increased ICC presence (altering sovereignty calculus) or reduced accountability (if they weakened national courts to avoid ICC intervention). State consent models would reorganize around de-coupling from ICC jurisdiction.
% FOUNDING_PROBLEM: Early post-WWII international law lacked legitimacy to prosecute state officials in their own countries. The Rome Statute's negotiators wanted to preserve state sovereignty while creating an international backstop for mass atrocity. Complementarity was the institutional design answer: presume national courts CAN and WILL handle accountability; only intervene when they demonstrably cannot or will not. This reading emphasizes the first half — the presumption.
% FOUNDING_PROBLEM_CORROBORATION: Rome Statute negotiators and state parties attest the founding problem was state sovereignty protection. ICC prosecutors, victim advocates, and human rights organizations attest the founding problem is accountability — that the presumption of national adequacy is a cover story permitting elite impunity in weak states. Academic commentary (from legal scholars, judges, prosecutors, and independent accountability analysts) is split: originalists support the national primacy reading; accountability-maximizers support the international oversight reading.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint extracts accountability access from victims in states with weak or politically biased national proceedings. The constraint is not purely extractive (there is a real coordination function: allocating responsibility between national and international systems), but it extracts by setting a high threshold for ICC intervention and placing the burden affirmatively on international actors. Suppression (0.72) is higher because the constraint's persistence depends on actively insulating national courts from external review — even weak courts receive presumptive deference, and the burden of proof is designed to be difficult and resource-consuming to overcome. Theater (0.41) is moderate: the constraint maintains the appearance of complementarity and state cooperation, but a growing proportion of ICC prosecutorial activity involves overcoming or contesting the presumption rather than applying it, indicating the constraint's functional basis has shifted while its formal structure persists. Accessibility_collapse (0.45) is moderate because victims retain formal pathways (state referral, prosecutor initiation) even if the burden is high; alternatives are not completely closed. Resistance (0.58) is substantial because transnational advocates, victim organizations, and some ICC prosecutors actively resist the presumption and push for broader interpretation — the constraint meets continuous pressure but persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the state sovereignty seat, the constraint is genuine coordination: a workable two-tier system that respects state autonomy while preserving international accountability. From the victim seat in a weak-but-genuine prosecution, the constraint operates as exclusionary extraction: formal access without substantive reach. From the ICC prosecutor's seat, it is a resource-extractive gate: the presumption requires affirmative, costly proof of inadequacy. From the Rome Statute framers' analytical seat, the constraint preserves their original intent. From the accountability advocate seat, the constraint is a cover story for impunity. The engine computes these different types from the structural data; the authored claim (tangled_rope) reflects the real presence of both coordination (two-tier allocation) and asymmetric extraction (high barrier for international intervention).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint distributes directionality asymmetrically. National judiciaries as an institutional class benefit from presumption (d~0.15), but post-conflict judiciaries in weak states also pay through impossible institutional demands (d~0.65 via identity_locked exit). Powerful states' instrumentalities benefit (d~0.10); weaker states' instrumentalities pay (d~0.75). The ICC prosecutor pays a high burden (d~0.85 via constrained exit). Victims pay access cost (d~0.90 via trapped exit). This is captured in the base directionality but the asymmetry within the 'national judiciaries' stakeholder class warrants commentary: the constraint protects Northern courts and institutionally strong Southern courts differently than it affects fragile transitional courts.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was state sovereignty protection in a post-WWII context where international legal authority lacked legitimacy. The founding problem status is now contested: states continue to defend the presumption; accountability advocates argue it is dead — the problem it solved (international overreach) has not materialized, but the constraint persists to solve a different problem (elite immunity). The disappearance_verdict (world_rearranges) coupled with founding_problem_status=contested is the classic mandatrophy signature: the arrangement persists despite disagreement about whether it still solves the original problem. The theater ratio is rising (0.35→0.41) because the constraint's formal structure (complementarity, deference) is increasingly separated from its function (controlling ICC docket and victim access). The extractiveness trajectory is flattening (0.58→0.68 early, then plateau), indicating the constraint has stabilized in its utility for state protection — it is doing what it does, consistently, without new escalation. This pattern is consistent with a tangled_rope that has matured into its extractive role: coordination still present (two-tier system works), extraction entrenched (high burden on ICC prosecutor), but the coordination component's justification is increasingly ceremonial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_vs_unable_definition,
    'What threshold of national judicial dysfunction or selectivity triggers ICC intervention under ''unable'' vs ''unwilling''? Does a weak court with genuine efforts but limited capacity qualify as ''unable''? Does a selective but functional court qualify as ''unwilling''?',
    'ICC case law and prosecutor practice data: track which state proceedings the prosecutor has challenged as inadequate, and what specific deficiencies triggered intervention. Compare to an external benchmark of actual judicial capacity and prosecutorial selectivity.',
    'If the threshold is high (near-total collapse required), the constraint extracts maximum victim access cost; if moderate (demonstrated neglect or systemic bias), the constraint is closer to rope than snare. The boundary between ''genuine but weak'' and ''sham'' is where the classification lives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_vs_unable_definition, empirical, 'Operational interpretation of the unwilling/unable standard drives the constraint''s effective gatekeeping.').

omega_variable(
    sovereignty_vs_accountability_incommensurability,
    'Is state sovereignty and victim accountability fundamentally in tension at the complementarity threshold, or can both be served by the same institutional design?',
    'This is a conceptual ambiguity rooted in conflicting axioms held by the two readings (national_primacy_reading vs international_oversight_reading). Resolution would require one reading to formally yield, or a new reading that reframes the tension as false (e.g., a ''genuine_subsidiarity'' reading that decouples sovereignty from immunity).',
    'If the tension is fundamental, the constraint will persist in contested form — neither reading can prevail without the other receding from the Rome Statute framework. If the tension can be reframed, the constraint could evolve toward a new reading that holds both values without zero-sum trade-off.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_accountability_incommensurability, conceptual, 'Whether the two readings'' axioms are logically incompatible or differently emphatic about the same values.').

omega_variable(
    powerful_state_asymmetry_in_presumption,
    'Do powerful states enjoy de facto different application of the complementarity presumption than weak states, based on their capacity to signal state cooperation and control of their national courts?',
    'Comparative analysis of which states'' proceedings have been challenged as inadequate by the ICC prosecutor, controlling for actual judicial capacity and prosecutorial selectivity. Statistical evidence of correlation between state power (military, economic, diplomatic) and prosecutor forbearance.',
    'If powerful states enjoy asymmetric protection, the constraint operates as a mechanism of structural inequality — it coordinates accountability in the formal design but extracts differentially based on power. This would indicate a snare-like feature within the tangled_rope: the coordination component is real, but the extraction component is captured by power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(powerful_state_asymmetry_in_presumption, empirical, 'Whether the presumption of national adequacy is applied uniformly or modulated by state power.').

omega_variable(
    victim_participation_as_adequacy_criterion,
    'Does a national proceeding''s failure to provide meaningful victim participation and remedy constitute ''unwillingness'' to prosecute, or only its failure to prosecute at all?',
    'Compare the prosecutor''s practice: does the prosecutor challenge national proceedings on grounds of victim exclusion alone, or only on grounds of prosecutorial refusal/incapacity? Track how victim organizations'' evidence is weighted in inadmissibility determinations.',
    'A narrow reading (prosecution alone determines adequacy) keeps the ICC prosecutor''s burden very high and favors the national primacy reading. A broad reading (victim access determines adequacy) shifts the constraint toward the international oversight reading and increases victim access to ICC proceedings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_participation_as_adequacy_criterion, empirical, 'Whether the adequacy standard includes substantive victim justice or only prosecutorial diligence.').

omega_variable(
    reading_contest_foreclosure,
    'Can the national_primacy_reading and international_oversight_reading coexist as live options within a single Rome Statute framework, or do their core axioms logically foreclose one another?',
    'Examine whether a court (the International Court of Justice or a future Rome Statute amendment conference) could adopt formal legal positions supporting both readings without internal contradiction. If the texts can support both interpretations without formal revision, they coexist; if one interpretation requires textual amendment that contradicts the other, they foreclose.',
    'This is the kernel-level question driving the committer frame. If the readings foreclose, the constraint story (national_primacy_reading) is in mortal competition with the sibling (international_oversight_reading) and one will eventually be formalized out. If they coexist, the constraint persists in contested form indefinitely, and the divergence between the two readings'' classifications becomes the measurement the corpus takes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the Rome Statute''s language supports both readings or permits only one normative hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__national_primacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__national_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__national_primacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.14).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% Article 17 complementarity is a contested kernel instantiated by two distinct constraint stories. The national_primacy_reading (this story) emphasizes state sovereignty and presumes national adequacy; the international_oversight_reading (sibling) emphasizes accountability and presumes ICC oversight when states fail. Both read the same Rome Statute text; they differ in the operational burden of proof and the hierarchy of values. Linking through network.affects_constraints records their institutional coupling: they are in zero-sum competition for the same jurisdictional space. The engine computes each reading's classification from its own structural data; the divergence is the measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
