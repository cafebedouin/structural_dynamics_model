% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV Revisability via Textual-Critical Authority
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This constraint instantiates the 'revisable translation' reading of the
 *   KJV text kernel. The reading asserts that English Bible translation
 *   authority derives from manuscript evidence and historical-linguistic
 *   method, not tradition; therefore the KJV, though valuable, is subject to
 *   improvement via scholarly revision. This reading is ONE of three
 *   interpretations of the same kernel (the 1611 KJV text as an authority
 *   source). The reading does NOT claim the KJV was wrong in its entirety or
 *   that all alternatives are superior—it claims the proper frame for
 *   evaluating translations is textual-critical methodology, which permits
 *   systematic revision when manuscript or linguistic evidence justifies it.
 *   This reading coexists with the 'exclusive inspiration' reading (KJV is
 *   inerrant, revision is corruption) and the 'functional equivalence'
 *   reading (multiple translations serve valid purposes without claiming
 *   superiority). The constraint models the institutional machinery that
 *   enforces the revisability frame's authority claim.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholars: Institutional power, agenda-setting role. Control the scholarly consensus that manuscript evidence and linguistics justify revision. Derive authority and resource allocation from this frame.
 *   - modern_translation_publishers: Institutional power, beneficiary role. Publish modern translations validated by the revisability reading. Capture market share from KJV through the framing that newer = more accurate.
 *   - kjv_tradition_custodians: Organized power, payer role. Bear the cost of having their translation's claim to authority systematically undermined by scholarly consensus. Resistance is organized but structurally weaker than institutional scholarly power.
 *   - congregations_resisting_revision: Moderate to powerless power, identity-locked exit. Experience translation choice as identity-disruption. Trapped because they cannot independently evaluate scholarly claims.
 *   - lay_readers_without_technical_training: Powerless, trapped exit. Cannot assess manuscript evidence independently; must defer to scholarly authority for legitimacy. Proliferation of translation choices fragments their reading community.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.58).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.41).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV Revisability via Textual-Critical Authority").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious_studies/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, 'dbc0bccc-6fa8-4664-9f7d-402e704cb76e').
narrative_ontology:cs_kernel_codification('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', fixed_text).
narrative_ontology:cs_authority_grounding('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', extraction).
narrative_ontology:cs_interpretation_layer_present('dbc0bccc-6fa8-4664-9f7d-402e704cb76e').
narrative_ontology:cs_reading_relation('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', foundational, manuscript_evidence_determines_superiority).
narrative_ontology:cs_axiom_status(manuscript_evidence_determines_superiority, holdable).
narrative_ontology:cs_axiom_grounding('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', manuscript_evidence_determines_superiority, empirically_contingent).
narrative_ontology:cs_axiom('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', foundational, translation_improvement_justifies_revision).
narrative_ontology:cs_axiom_status(translation_improvement_justifies_revision, holdable).
narrative_ontology:cs_axiom_grounding('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', translation_improvement_justifies_revision, instrumental).
narrative_ontology:cs_reference_frame('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', text_improvement_via_manuscript_recovery).
narrative_ontology:cs_drift_state('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', contemporary_institutional_equilibrium, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbc0bccc-6fa8-4664-9f7d-402e704cb76e', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, methodological_textual_criticism).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_tradition_custodians).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, congregations_resisting_revision).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, lay_readers_without_technical_training).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, manuscript_evidence_principle).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, historical_linguistic_method).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, translation_improvement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the scholarly standard that translation authority derives from manuscript evidence and linguistic knowledge, not tradition. Control peer-review gatekeeping, academic publishing, seminary curricula. Benefit by defining 'correctness' in terms of their methodology; carry minimal cost if traditional readers reject their outputs.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).

% Publish modern translations (NIV, ESV, NRSV, etc.) justified by this reading's premise: better manuscripts, better linguistics, better accessibility. Capture the expanding market of educated readers who accept the revisability frame. Carry modest enforcement cost (marketing the legitimacy of newer translations against tradition).
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, modern_translation_publishers, beneficiary,
    institutional, generational, mobile, global).

% Defend the KJV's literary, liturgical, and spiritual authority. Bear the cost of having their translation's superiority claim systematically undermined by scholarly consensus that frames it as 'historically valuable but linguistically obsolete.' Cannot easily exit the conversation without abandoning their interpretive tradition; organized resistance is their primary option.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_tradition_custodians, payer,
    organized, generational, constrained, national).

% Maintain KJV in congregational use and personal devotion, often for reasons of identity and familiarity. Bear the cost of social/institutional pressure to adopt 'more scholarly' translations. Identity-locked because translation choice becomes intertwined with faith identity and community belonging; exit means changing congregations or facing persistent marginal status.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, congregations_resisting_revision, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, congregations_resisting_revision, excluded).

% Navigate a proliferation of translation choices, none of which they are equipped to evaluate. The revisability reading fragments their reading community and requires them to defer to scholarly authority for translation legitimacy. Trapped because they cannot independently assess manuscript evidence; their exit—choosing a translation they can defend—is foreclosed by the authority structure's vertical dependence on technical expertise.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, lay_readers_without_technical_training, payer,
    powerless, biographical, trapped, local).

% The reading vindicates manuscript evidence and historical-linguistic method as the legitimate basis for translation authority. This methodological framework gains institutional authority and resource allocation in seminaries and universities. As a non-agent, it collects no rents but anchors the legitimacy claim that drives the constraint.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, textual_critical_methodology, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__revisable_translation_reading, textual_critical_methodology).

% Navigate translation choice for evangelical audiences. The revisability reading creates market pressure to adopt 'scholarly' modern translations while maintaining evangelical identity. They observe the constraint without full agenda-setting power; their translation choices influence which reading gains practical dominance.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, evangelical_publishers_and_ministries, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns English-language Bible translation with contemporary manuscript evidence and linguistic science. Solves the coordination problem of which translation is legitimate by grounding legitimacy in reproducible textual-critical method rather than historical convention. Enables translators to improve clarity and accuracy when new manuscript discoveries or linguistic insights emerge.
% TRANSFER_FUNCTION: Moves interpretive authority from tradition-based custodianship (KJV defenders, liturgical communities) to academic scholars and modern publishers. The reading subordinates familiarity and continuity to methodological correctness; institutional authority over translation choice flows to those who control manuscript interpretation and linguistic expertise.
% ABSENT_VOICES: Congregations that experience translation choice as identity-disruption are partially excluded; their objections are heard as 'resistance to scholarship' rather than legitimate costs. Indigenous or non-Western Bible communities using KJV translations are absent from the core scholarly conversation (scholarship is globally published English-dominant, but adoption decisions in smaller language groups are not centered). The reading's universal claim to better methodology obscures context-specific translation needs.
% DISAPPEARANCE_RATIONALE: If the revisability reading and its enforcement apparatus vanished (if textual-critical methodology lost institutional authority in seminaries and publishing), congregations would retain KJV or adopt a single stable modern translation without constant revision pressure; publishers would market stability over improvement; Bible reading would re-stabilize around familiar texts rather than chase manuscript updates. The constraint's absence would permit translation stability at the cost of blocking methodological improvement.
% FOUNDING_PROBLEM: Early 20th-century biblical scholarship discovered older manuscripts (Dead Sea Scrolls, earlier Greek texts) and refined linguistic understanding of Koine Greek, revealing that the KJV (based on later, inferior manuscript tradition) contained translation errors and interpretive choices unsupported by the oldest sources.
% FOUNDING_PROBLEM_CORROBORATION: Paleographic and textual-critical consensus, attested by academic scholars outside fundamentalist traditions (Metzger, Ehrman, Comfort, and the international scholarly community). Dead Sea Scrolls discoveries (1940s–1950s) and continuous papyri finds provide material evidence independent of KJV-defender interpretations. Counterattested by conservative scholars (Burgon, Pickering traditions) who argue the manuscript base is corrupted rather than improved, but their dissent is minority within academic institutions.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint subordinates tradition-based authority to academic expertise; institutional publishers benefit while congregations and lay readers bear the cost of translation instability and deference to technical authority. Suppression is moderate (0.41) because the constraint operates largely through institutional gatekeeping (seminary curricula, peer review, publishing standards) rather than explicit coercion; resistance to the revisability frame is substantial (0.72) from KJV-defending communities, indicating the frame is not naturalized. Theater ratio is low-moderate (0.28) because the scholarly activity is genuine—textual criticism is a real discipline with real methods—but an increasing share of the constraint's enforcement serves to maintain the authority hierarchy itself rather than to improve translations (once scholarly consensus is established, further revision becomes incremental and marginal to the core goal). The measurement trajectory shows initial rise in extractiveness (t=0 to t=20, as institutional authority solidifies) followed by stabilization (t=20 to t=40), with suppression declining slightly (as the constraint becomes more normalized in academic institutions). Theater ratio rises early (t=0 to t=25, as the machinery for enforcing the frame proliferates) then stabilizes, indicating the constraint reaches a steady-state equilibrium where performance and function are roughly balanced.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (academic scholars) experiences this constraint as the legitimate application of scientific method to historical texts—they see it as correcting error, not extracting. The payer seats (KJV defenders, congregations) experience it as institutional power subordinating their interpretive tradition to academic authority. From the scholar's position, resistance is irrationality; from the tradition-defending position, the constraint is intellectual imperialism justified by methodological rhetoric. The engine computes these divergent positions from the structural data: the scholar's low directionality (beneficiary) produces low effective extraction from their seat, while the congregation's high directionality (victim, identity-locked, powerless) produces high effective extraction from their seat. The measured tangled-rope type reflects exactly this asymmetry: genuine coordination function (better manuscripts, better translations) coupled with asymmetric authority transfer (tradition → methodology → scholarship → publishing).
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars are beneficiaries with institutional power and arbitrage-grade exit options (they control the standard, face minimal cost if their translations are rejected, can migrate to other academic domains). Directionality near 0.15–0.25. Modern publishers are beneficiaries with institutional power and mobile exit (they publish what sells; if the revisability frame fails, they publish whatever does). Directionality near 0.10–0.20. KJV custodians are victims with organized power but constrained exit (they can organize resistance, but cannot exit the conversation without abandoning their tradition). Directionality near 0.70–0.80. Congregations resisting revision are victims with moderate-to-powerless power and identity-locked exit (they cannot easily leave without changing congregations or losing community identity). Directionality near 0.80–0.95. Lay readers are victims with powerless power and trapped exit (they have no technical basis for independent evaluation). Directionality near 0.95. No directionality overrides are needed; the structural derivation from beneficiary/victim + power + exit captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The revisability reading faces a mandatrophy threat: the founding problem (KJV rests on inferior manuscript tradition) is academically live and empirically grounded, but the institutional machinery enforcing the reading increasingly pursues translation stability and publisher market capture rather than manuscript improvement. Once the major modern translations (NIV, ESV, NRSV) solidified their market positions, the incentive to continue manuscript-based revision declined—the constraint now functions more to maintain the authority hierarchy (scholars > publishers > congregations) than to improve translations. This is not yet mandatrophy (the founding problem is still academically defended), but the measurement series shows extraction stabilizing while theater ratio plateaus, suggesting the constraint's function may be drifting toward institutional self-maintenance. Commentary on this drift belongs to the omega variable addressing the role of institutional gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_sufficiency_vs_methodological_closure,
    'Does the revisability reading commit to the claim that manuscript evidence will eventually produce a single, final, superior translation, or is the claim only that manuscript-based revision is always potentially legitimate?',
    'Examine whether scholarly practice treats manuscript improvement as asymptotically convergent (moving toward a true text) or as perpetually open (each generation reinterprets available evidence). Compare claims in textual-critical methodology texts with actual patterns of translation revision over multiple decades.',
    'If convergent, the reading promises eventual closure and makes sense of institutional investment in revision cycles; if perpetually open, the reading justifies infinite revision and becomes a mechanism for perpetual institutional authority-claiming. The constraint''s mandatrophy risk depends on which interpretation the institutional machinery actually operates under.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_sufficiency_vs_methodological_closure, conceptual, 'Whether revisability implies eventual manuscript consensus or perpetual methodological revision.').

omega_variable(
    scholarly_authority_independence,
    'To what extent does the revisability reading''s claim that ''scholarly method justifies revision'' depend on the claim that scholars are independent arbiters, versus the institutional fact that scholars are embedded in universities, publishing networks, and funded research hierarchies that benefit from perpetual revision cycles?',
    'Historical analysis of funding flows for translation projects, comparison of translation revision rates in institutional versus independent contexts, examination of how often scholars reject revisions suggested by publisher marketing pressures versus manuscript evidence.',
    'If scholarly authority is substantially independent, the constraint is a legitimate application of method; if substantially captured by publishing incentives or institutional reward structures, the constraint is an extraction mechanism dressed in methodological language—a false summit candidate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_authority_independence, empirical, 'Whether academic textual criticism operates independently of institutional resource capture.').

omega_variable(
    exclusivity_vs_pluralism_in_the_reading,
    'Does the revisable_translation reading entail that scholarship-based revision is the ONLY legitimate approach to translation authority, or does it permit other frames (tradition-based, user-preference-based, functional-equivalence-based) to remain valid within different communities?',
    'Examine how the reading''s defenders respond to the exclusive_inspiration and functional_equivalence readings: do they claim those readings are incoherent, or do they claim their own reading is simply superior for certain audiences? Compare institutional practice (do seminaries require adoption of the reading, or permit students to select their reading?) with theoretical claims.',
    'If the reading entails exclusivity, it is not a coexistent sibling reading but a foreclosing claim; if it permits pluralism, the constraint operates as persuasion + institutional gatekeeping, not as logical foreclosure. The distinction affects whether the reading''s authority is justified by method or by power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_vs_pluralism_in_the_reading, conceptual, 'Whether the revisability reading requires exclusive authority or permits coexistence of other translation frames.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.41 at interval end) achieved through structural barriers (institutional gatekeeping, publishing economics, seminary curriculum control) or through internalization of the scholarly frame as authoritative by congregational leaders and lay readers?',
    'Examine whether resistance to the revisability reading persists among lay readers who exit institutional gatekeeping (independent congregations, online Bible communities, translations chosen outside seminary networks). If resistance persists without institutional pressure, suppression is substantially internalized; if it dissolves when institutional pressure is removed, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—communities carry the deferral to scholarly authority even after exit. If structural, removing institutional gatekeeping would substantially reduce the constraint''s hold. This affects whether the constraint would persist if institutional power were redistributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative translation frames is structural (gatekeeping) or internalized (belief in scholarly authority).').

omega_variable(
    committer_ambiguity_sibling_reading_identity,
    'Is the distinction between the revisable_translation reading and the functional_equivalence reading a genuine difference in core premises (about whether manuscripts determine superiority) or a difference in institutional positioning (about who gets to claim authority)?',
    'Compare the two readings'' claims about translation: does functional_equivalence deny that better manuscripts justify revision, or does it simply claim that multiple valid translations can coexist? If the latter, the readings differ more in policy (pluralism vs. hierarchy) than in method (both could accept manuscript-based revision; they disagree on its authority status).',
    'If the readings differ primarily in policy rather than method, the sibling relation should be retyped from coexists_with to influences—the revisable reading creates pressure toward hierarchy that the functional reading accommodates by accepting revised translations as an additional valid option. This would suggest the two readings are less opposed than parallel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_ambiguity_sibling_reading_identity, conceptual, 'Whether the functional_equivalence sibling reading is a genuine alternative to revisability or an institutional accommodation of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_revisable_tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(kjv_revisable_tr_t0, observed).
narrative_ontology:measurement(kjv_revisable_tr_t5, kjv_text_1611__revisable_translation_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(kjv_revisable_tr_t5, observed).
narrative_ontology:measurement(kjv_revisable_tr_t10, kjv_text_1611__revisable_translation_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(kjv_revisable_tr_t10, observed).
narrative_ontology:measurement(kjv_revisable_tr_t15, kjv_text_1611__revisable_translation_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(kjv_revisable_tr_t15, observed).
narrative_ontology:measurement(kjv_revisable_tr_t20, kjv_text_1611__revisable_translation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(kjv_revisable_tr_t20, observed).
narrative_ontology:measurement(kjv_revisable_tr_t25, kjv_text_1611__revisable_translation_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(kjv_revisable_tr_t25, observed).
narrative_ontology:measurement(kjv_revisable_tr_t30, kjv_text_1611__revisable_translation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(kjv_revisable_tr_t30, observed).
narrative_ontology:measurement(kjv_revisable_tr_t40, kjv_text_1611__revisable_translation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(kjv_revisable_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(kjv_revisable_be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(kjv_revisable_be_t0, observed).
narrative_ontology:measurement(kjv_revisable_be_t5, kjv_text_1611__revisable_translation_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(kjv_revisable_be_t5, observed).
narrative_ontology:measurement(kjv_revisable_be_t10, kjv_text_1611__revisable_translation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(kjv_revisable_be_t10, observed).
narrative_ontology:measurement(kjv_revisable_be_t15, kjv_text_1611__revisable_translation_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(kjv_revisable_be_t15, observed).
narrative_ontology:measurement(kjv_revisable_be_t20, kjv_text_1611__revisable_translation_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(kjv_revisable_be_t20, observed).
narrative_ontology:measurement(kjv_revisable_be_t25, kjv_text_1611__revisable_translation_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(kjv_revisable_be_t25, observed).
narrative_ontology:measurement(kjv_revisable_be_t30, kjv_text_1611__revisable_translation_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(kjv_revisable_be_t30, observed).
narrative_ontology:measurement(kjv_revisable_be_t40, kjv_text_1611__revisable_translation_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(kjv_revisable_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv_revisable_su_t0, kjv_text_1611__revisable_translation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(kjv_revisable_su_t0, observed).
narrative_ontology:measurement(kjv_revisable_su_t5, kjv_text_1611__revisable_translation_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(kjv_revisable_su_t5, observed).
narrative_ontology:measurement(kjv_revisable_su_t10, kjv_text_1611__revisable_translation_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(kjv_revisable_su_t10, observed).
narrative_ontology:measurement(kjv_revisable_su_t15, kjv_text_1611__revisable_translation_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(kjv_revisable_su_t15, observed).
narrative_ontology:measurement(kjv_revisable_su_t20, kjv_text_1611__revisable_translation_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(kjv_revisable_su_t20, observed).
narrative_ontology:measurement(kjv_revisable_su_t25, kjv_text_1611__revisable_translation_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(kjv_revisable_su_t25, observed).
narrative_ontology:measurement(kjv_revisable_su_t30, kjv_text_1611__revisable_translation_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(kjv_revisable_su_t30, observed).
narrative_ontology:measurement(kjv_revisable_su_t40, kjv_text_1611__revisable_translation_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(kjv_revisable_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__revisable_translation_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, modern_translation_publishing_ecology).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, seminary_curriculum_standardization).

% DUAL FORMULATION NOTE:
% The KJV kernel (kjv_text_1611) decomposes into three structurally distinct constraints representing incompatible readings of the same persisting text. The revisable_translation_reading asserts that manuscript evidence and linguistics justify revision; it coexists with the exclusive_inspiration_reading (KJV is inerrant, revision is corruption) and the functional_equivalence_reading (multiple translations serve valid complementary purposes). Each reading has different ε, different victims and beneficiaries, and different suppression profiles. This story models the revisable reading's institutional machinery; sibling stories model the other readings. The readings are related through the network: each affects how the others function institutionally (the revisable reading creates pressure toward scholarly hierarchy that the functional reading accommodates; the exclusive reading resists the revisable reading's authority claims). The three stories should be read as a constraint family decomposing the contested kernel per the ε-invariance principle: one kernel, three incompatible readings, three stories, one network linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
