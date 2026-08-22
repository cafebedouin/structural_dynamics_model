% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Complementarity Jurisdiction (Hybrid Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute established the International Criminal Court (ICC) with
 *   jurisdiction over genocide, crimes against humanity, and war crimes. The
 *   statute embeds a complementarity mechanism: the ICC defers to national
 *   judicial systems when they are willing and able to prosecute atrocities
 *   domestically. This reading interprets complementarity as a hybrid
 *   structure balancing universal aspiration (all atrocities are
 *   internationally accountable) with sovereign primacy (states retain the
 *   first right to try their own nationals). The constraint is claimed as a
 *   coordination mechanism (tangled_rope) because it both coordinates
 *   international criminal justice AND asymmetrically extracts sovereignty
 *   from signatory states while protecting non-signatories and Security
 *   Council members. The measurement series tracks the rising burden of
 *   complementarity deference on weak states and affected populations
 *   (extractiveness rising from 0.48 to 0.62), the increasing performative
 *   character of complementarity review (theater rising from 0.22 to 0.41),
 *   and the suppressive force required to maintain state deference despite
 *   ICC pressure (suppression rising from 0.42 to 0.58).
 *
 * KEY AGENTS:
 *   - International Criminal Court: administrator of complementarity doctrine; claims universal jurisdiction but operationally defers to state cooperation
 *   - Signatory states cooperating: maintain prosecution power domestically while benefiting from Rome Statute legitimacy and avoiding ICC oversight
 *   - Non-signatory states (US, Russia, China, India): formally outside treaty but subject to Security Council referral; controlled by veto-holding powers
 *   - Weak/captured domestic judiciaries: benefit nominally from complementarity deference but are often unable/unwilling to prosecute, delaying justice
 *   - Affected populations in conflict zones: depend on ICC when domestic systems fail but suffer extraction through lengthy complementarity procedures
 *   - International justice advocates: beneficiaries of the Rome Statute framework; defend complementarity as institutional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.58).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Complementarity Jurisdiction (Hybrid Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '66ebc05c-5909-4178-9517-0306f368101b').
narrative_ontology:cs_kernel_codification('66ebc05c-5909-4178-9517-0306f368101b', fixed_text).
narrative_ontology:cs_authority_grounding('66ebc05c-5909-4178-9517-0306f368101b', lineage).
narrative_ontology:cs_interpretation_layer_present('66ebc05c-5909-4178-9517-0306f368101b').
narrative_ontology:cs_reading_relation('66ebc05c-5909-4178-9517-0306f368101b', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('66ebc05c-5909-4178-9517-0306f368101b', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('66ebc05c-5909-4178-9517-0306f368101b', foundational, complementarity_is_genuine_subsidiarity).
narrative_ontology:cs_axiom_status(complementarity_is_genuine_subsidiarity, holdable).
narrative_ontology:cs_axiom_grounding('66ebc05c-5909-4178-9517-0306f368101b', complementarity_is_genuine_subsidiarity, conventional).
narrative_ontology:cs_axiom('66ebc05c-5909-4178-9517-0306f368101b', foundational, universal_jurisdiction_residual_not_primary).
narrative_ontology:cs_axiom_status(universal_jurisdiction_residual_not_primary, holdable).
narrative_ontology:cs_axiom_grounding('66ebc05c-5909-4178-9517-0306f368101b', universal_jurisdiction_residual_not_primary, deontological).
narrative_ontology:cs_reference_frame('66ebc05c-5909-4178-9517-0306f368101b', rome_statute_hybrid_balance).
narrative_ontology:cs_drift_state('66ebc05c-5909-4178-9517-0306f368101b', contemporary_icc_practice_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66ebc05c-5909-4178-9517-0306f368101b', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, affected_populations_accessing_justice).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_resisting_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_cooperating).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_resisting_intervention).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, weak_and_captured_domestic_judiciaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the complementarity doctrine: claims universal jurisdiction over genocide, crimes against humanity, and war crimes, but defers to state prosecution when domestic systems are willing and able. The deference is framed as respect for subsidiarity but operationally functions as a constraint on the Court's enforcement power. The Court interprets complementarity to preserve its own authority while appearing to honor state sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% States that have domesticated international criminal law and prosecute their own atrocity crimes maintain control over their judicial proceedings while benefiting from the legitimacy conferred by the Rome Statute framework. They avoid ICC intervention while appearing to comply with universal justice norms. Cooperation is voluntary but carries prestige and diplomatic recognition.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_cooperating, beneficiary,
    institutional, generational, mobile, global).

% States not party to the Rome Statute—including the United States, Russia, China, and India—are formally outside the ICC's jurisdiction, yet remain subject to Security Council referral. They bear the risk of ICC intervention without formal consent to the treaty framework. Their nationals can be prosecuted for crimes committed on the territory of signatory states or through Security Council action.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states, payer,
    institutional, generational, trapped, global).

% States that have signed the Rome Statute but resist ICC intervention in their own atrocities invoke complementarity to claim domestic handling is underway, or withdraw from the treaty when prosecution appears imminent. They carry the cost of compliance pressure and reputational risk while attempting to use the complementarity mechanism to defer or avoid ICC oversight. Withdrawal is possible but diplomatically expensive.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_resisting_intervention, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states_resisting_intervention, beneficiary).

% Survivors and families of atrocity victims in countries with weak or captured judiciaries depend on ICC prosecution for accountability when domestic systems are unwilling or unable. They benefit from the existence of universal jurisdiction but suffer extraction through lengthy complementarity procedures that defer to domestic systems that may be complicit in atrocities.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, affected_populations_accessing_justice, beneficiary,
    powerless, biographical, trapped, global).

% State judicial systems in post-conflict settings or under authoritarian control are often unable or unwilling to prosecute their own leaders for atrocities. Complementarity doctrine defers to these systems even when they lack genuine independence or capacity, extracting time and delaying justice while nominally respecting state autonomy. Reform or independence would dissolve the constraint, but institutional capture makes exit difficult.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, weak_and_captured_domestic_judiciaries, payer,
    moderate, biographical, identity_locked, national).

% The United States, Russia, China, France, and the UK control Security Council referral authority and can selectively refer situations to the ICC or block referral of situations involving their allies. This structural ability to manipulate jurisdiction is the enforcement asymmetry the complementarity reading obscures. Excluded from formal participation in complementarity debate while holding veto power over its application.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, permanent_security_council_members, excluded,
    institutional, generational, arbitrage, global).

% Human rights organizations and international justice networks benefit from the Rome Statute framework's existence and the legitimacy of universal jurisdiction principles. They advocate for robust ICC enforcement while defending complementarity as a necessary institutional compromise. Their advocacy shapes how the doctrine is interpreted and applied.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% International legal scholars, treaty negotiators, and institutional analysts assess whether complementarity functions as genuine subsidiarity or as a mechanism that constrains universal justice. They interpret the Rome Statute's text and historical record to adjudicate competing readings.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, treaty_interpreters, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for prosecuting atrocity crimes: creates universal jurisdiction over genocide, crimes against humanity, and war crimes while respecting state capacity and legitimacy by deferring to effective domestic prosecution. Solves the coordination problem of defining what constitutes atrocity and who has legitimate authority to adjudicate it when a state's own courts will not.
% TRANSFER_FUNCTION: Transfers authority from traditional state-centric jurisdiction to a hybrid model: a supranational court (ICC) gains the right to prosecute atrocity crimes, while states nominally retain primary responsibility and courts retain operational control over investigations and trials. Transfers legitimacy and accountability prestige to signatory states that cooperate, while extracting sovereignty constraints from non-signatories and states resisting intervention.
% ABSENT_VOICES: Non-signatory powers (US, Russia, China, India) are structurally excluded from Rome Statute decision-making yet subject to Security Council referral, which gives them veto power but no formal voice in treaty interpretation. Weak states dependent on ICC protection lack negotiating capacity in treaty evolution. Armed non-state groups and victims of crimes by powerful states have no seat at treaty amendment.
% DISAPPEARANCE_RATIONALE: If complementarity doctrine disappeared and the ICC exercised pure universal jurisdiction, the institutional balance would shift from state-constrained to supranational: signatory states would face immediate prosecution risk for all atrocities, non-signatories would lose the Security Council arbitrage mechanism, and the Court's case load and enforcement challenges would multiply. States would either strengthen the Rome Statute's enforcement mechanisms or withdraw wholesale, reorganizing international criminal law along consent-only or regional lines.
% FOUNDING_PROBLEM: The post-WWII international system lacked a mechanism for prosecuting atrocity crimes when perpetrators held state power or refuge in non-cooperating jurisdictions. The ICTY and ICTR ad hoc tribunals demonstrated both the necessity and the institutional fragility of international justice. The Rome Statute was designed to create a permanent institution that balanced universal aspiration (all atrocities are subject to international scrutiny) with state sovereignty (no signatory loses the right to prosecute its own nationals domestically first).
% FOUNDING_PROBLEM_CORROBORATION: International justice advocates attest the founding problem remains live: atrocity crimes persist and domestic systems remain unable or unwilling to prosecute. Sovereigntist states (US, Russia, India) attest that state-level protection is the founding value and complementarity correctly prioritizes it. Scholars of international criminal law note the founding problem was the institutional gap after ICTY/ICTR mandate expiration, and the ICC's actual performance on complementarity deference shows the founding aspiration (universal justice) has been substantially compromised. Non-signatory powers and African Union critiques provide corroboration that complementarity functions asymmetrically, protecting powerful states while constraining weaker ones.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects that complementarity operationally constrains ICC prosecution of atrocities in weaker states while protecting powerful non-signatories and permitting selective state-level justice depending on political will and capacity. Suppression at 0.58 reflects the ongoing enforcement machinery required to maintain state deference to complementarity: diplomatic pressure, funding conditionality, and threat of ICC intervention create active suppressive force against states that would prefer to ignore atrocities. Theater at 0.41 reflects rising performative character: complementarity review procedures are framed as respecting state capacity and judicial independence, but operate to defer justice in contexts where domestic systems are demonstrably compromised. Rising trajectories on all three metrics over the 2002–2026 interval suggest the constraint is accumulating extraction as state resistance hardens (South Africa and Philippines withdrawals, AU bloc critique, African states threatening mass withdrawal) and the ICC responds with more intensive complementarity proceduralism rather than direct prosecution. Accessibility_collapse at 0.68 reflects that once complementarity doctrine is understood, weak states and affected populations have collapsed exit options: they are nominally protected by the ICC yet practically dependent on domestic systems that may be compromised. Resistance at 0.72 reflects growing pushback from Global South states, African Union bloc, and non-signatories who view complementarity as masking ICC selectivity and protecting Northern actors.
 *
 * PERSPECTIVAL GAP:
 *   The core divergence surfaces in the Security Council veto asymmetry: the Rome Statute text describes universal jurisdiction and complementarity as bidirectional (states and ICC cooperate to prosecute atrocities), but the practice shows one-directional constraint on non-signatories and weak signatories while veto-holding powers enjoy de facto immunity. The constraint as written does not acknowledge this asymmetry; the constraint as practiced embeds it. Commentary on mandatrophy: the founding problem was the post-ICTY/ICTR institutional gap and the need for permanent international criminal justice. Complementarity solves that gap but at the cost of extracting sovereignty from states that cannot protect their own judiciaries. As weak-state withdrawal risk has risen (AU bloc 2016–2022, Philippines 2019, South Africa 2021), the ICC has intensified complementarity procedures rather than prosecuting directly, suggesting the founding problem (institutional gap) is being replaced by a secondary problem (regime protection). This is the extracted cost: vulnerable populations depend on complementarity deferring to their captured judiciaries, which persists because the ICC's institutional interest in state cooperation outweighs its mandate to prosecute atrocities.
 *
 * DIRECTIONALITY LOGIC:
 *   International Criminal Court (agenda_setter, institutional power): sets the complementarity doctrine, decides when to defer or intervene, controls interpretation of 'willing and able.' Directionality near 0.2 (strong beneficiary) — the Court's institutional prestige and case docket both depend on selective enforcement that respects powerful states' interests while appearing to pursue universal justice. Signatory states cooperating (beneficiary, institutional power): retain prosecution authority while benefiting from Rome Statute legitimacy. Directionality near 0.3 — they benefit from the coordination function and from complementarity's deference mechanism. Non-signatory states (payer, trapped in Security Council referral): bear jurisdiction risk without treaty consent. Directionality near 0.85 (target) — trapped exit, global scope, institutional power means their nationals face prosecution risk while they control no complementarity doctrine. Signatory states resisting (payer + beneficiary, institutional power, constrained exit): the secondary role reflects that they can use complementarity to defer intervention but pay reputational cost and withdrawal risk. Directionality near 0.65 — the constraint is partially extractive (sovereignty costs) but they can exit the treaty or cooperate to avoid ICC intervention. Affected populations (powerless, trapped, biographical horizon): depend on ICC prosecution when domestic systems fail but suffer extraction through complementarity delays. Directionality near 0.88 (strong target) — powerless, trapped, dependent on an institution controlled by others. Weak domestic judiciaries (identity_locked): institutional capture makes exit difficult despite complementarity doctrine nominally protecting them. Directionality near 0.75 (target) — the constraint maintains their constrained position by deferring to them formally while they lack capacity to act on that deference.
 *
 * MANDATROPHY ANALYSIS:
 *   The Rome Statute's mandate was to establish permanent international criminal justice transcending state consent (universal aspiration). Complementarity was inserted as an institutional compromise: defer to state prosecution when possible, retain ICC authority when states are unwilling or unable. The hybrid_complementarity_reading claims this balance is legitimate — it is a tangled_rope that coordinates justice while extracting sovereignty costs. The evidence for mandatrophy: (1) founding problem status is contested: advocates claim atrocity crimes persist (problem live); sovereigntists claim state protection is the founding value (problem reframed); (2) disappearance verdict is world_rearranges: if complementarity vanished, states would reorganize along either stronger ICC enforcement or stronger consent-based opt-in, suggesting the constraint is institutional rather than natural; (3) theater rising from 0.22 to 0.41 over 24 years suggests complementarity reviews increasingly perform respect for state capacity while deferring justice, rather than enforcing universal accountability; (4) extractiveness rising from 0.48 to 0.62 suggests the constraint accumulates extraction (state withdrawal risk, affected population delays) without corresponding prosecution increases. The mandatrophy verdict depends on whether complementarity's deference to weak judiciaries is genuine subsidiarity (a tangled_rope) or performative protection of state interests (a snare). The rising theater and resistance metrics, combined with AU and Global South bloc critiques, suggest the mandate (universal justice) has been substantially compromised by the institutional constraint (state cooperation). This is mandatrophy: the founding problem persists but the constraint's function has shifted from solving it to managing the institutional backlash against ICC selectivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_as_subsidiarity_vs_selectivity,
    'Does complementarity doctrine function as genuine subsidiarity (respecting state capacity and legitimacy to prosecute domestically) or as selective enforcement (protecting powerful states and constraining prosecutions in weak states)?',
    'Comparative analysis of ICC prosecution patterns across state strength, Security Council alignment, and Global South membership; audit of complementarity findings vs. actual state prosecution outcomes; longitudinal study of case outcomes and deferral timing.',
    'If genuine subsidiarity: the constraint is a tangled_rope (coordination + sovereignty extraction). If selective enforcement: the constraint is a snare (pure extraction masked by subsidiarity rhetoric). This divergence is the most consequential for classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_subsidiarity_vs_selectivity, empirical, 'Whether complementarity embeds genuine subsidiarity or performative selectivity').

omega_variable(
    security_council_asymmetry_in_rome_statute_design,
    'Is the Security Council''s veto power over ICC referral an intended feature of the Rome Statute complementarity mechanism or an unintended structural effect of Article 13(b)?',
    'Historical analysis of Rome Statute negotiation records; legal scholarship on treaty intent; state practice and pleadings in ICC admissibility cases invoking Council referral.',
    'If intended: the statute is sovereigntist-accommodating (great powers reserved veto); the hybrid reading must account for veto-power asymmetry as central to complementarity balance. If unintended: the statute contains a structural contradiction (universal jurisdiction + veto power) that the hybrid reading obscures. Either resolution strengthens the omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_council_asymmetry_in_rome_statute_design, empirical, 'Whether Rome Statute veto asymmetry reflects treaty intent or structural accident').

omega_variable(
    weak_state_capture_vs_respect_for_sovereignty,
    'When complementarity defers to a weak or captured domestic judiciary, is the constraint respecting that state''s sovereignty or extracting consent to injustice from a state lacking capacity to resist?',
    'Post-ICC intervention studies: when weak states are passed to domestic systems, do trials proceed? When the ICC directly prosecutes, do weak states benefit? Does complementarity''s deference correlate with higher or lower atrocity prosecution rates in weak-state contexts?',
    'High correlation of complementarity deferral with lower prosecution rates would establish that deference to weak judiciaries is extraction (constraining justice), not subsidiarity. This would shift the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_state_capture_vs_respect_for_sovereignty, empirical, 'Whether complementarity deference to weak states respects sovereignty or extracts injustice tolerance').

omega_variable(
    hybrid_reading_vs_sovereigntist_reading_boundary,
    'At what point does respect for complementarity deference to domestic systems cross from institutional accommodation (hybrid reading) to legal primacy of state consent (sovereigntist reading)?',
    'Interpretive analysis of Rome Statute text (Article 17 ''admissibility'' standard); ICC jurisprudence on complementarity thresholds; state practice on deference timing and withdrawal threats.',
    'If the boundary is permissive (easily satisfied by nominal domestic investigation), the constraint trends sovereigntist. If the boundary is strict (ICC can override complementarity on demand), the constraint trends universalist. Current practice suggests a blurred boundary dependent on great-power pressure, which is the hybrid reading''s core claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_vs_sovereigntist_reading_boundary, conceptual, 'Where the dividing line lies between hybrid balance and sovereigntist primacy in complementarity doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.22).
narrative_ontology:measurement_basis(rome_tr_t2002, observed).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement_basis(rome_tr_t2008, observed).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement_basis(rome_tr_t2014, observed).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement_basis(rome_tr_t2020, observed).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(rome_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.48).
narrative_ontology:measurement_basis(rome_be_t2002, observed).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement_basis(rome_be_t2008, observed).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement_basis(rome_be_t2014, observed).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(rome_be_t2020, observed).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(rome_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.42).
narrative_ontology:measurement_basis(rome_su_t2002, observed).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement_basis(rome_su_t2008, observed).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement_basis(rome_su_t2014, observed).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(rome_su_t2020, observed).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(rome_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice__selectivity_bias).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, security_council_referral__veto_asymmetry).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction kernel decomposes into three structurally distinct readings. The hybrid_complementarity_reading (this story) claims the statute balances universal aspiration with sovereign primacy through operational deference to domestic systems. The sovereigntist_reading (sibling constraint) emphasizes that complementarity is legally constitutive — states retain primary jurisdiction and the ICC is residual. The universalist_reading (sibling constraint) argues complementarity is descriptive cover for universal mandate — all atrocities are ultimately ICC-accountable regardless of state cooperation. These three readings have different ε values (this reading: 0.62; sovereigntist reading: ~0.35; universalist reading: ~0.78) and different structural beneficiary/victim sets. They are linked through this network field because interpreting the Rome Statute text differently produces different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.22).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerless, 0.88).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
