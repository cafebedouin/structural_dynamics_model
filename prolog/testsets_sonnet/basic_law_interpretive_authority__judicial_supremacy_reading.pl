% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the basic law
 *   interpretive authority kernel: courts hold final, terminal authority over
 *   constitutional meaning, justified by specialized legal expertise and
 *   electoral insulation. This is one of three structurally distinct readings
 *   of the same underlying kernel (who gets to say what the constitution
 *   means). The parliamentary sovereignty reading and the popular
 *   constitutionalism reading are separate constraint stories with different
 *   beneficiary/victim structures and different epsilon values — they are not
 *   alternate measurements of this constraint, they are different
 *   constraints. Under this reading specifically, the judiciary and the
 *   specialized constitutional bar enter the beneficiary set for
 *   institutional authority and professional rents; the legislature and
 *   electoral majorities enter the victim set when review nullifies enacted
 *   preferences, and gridlock/redrafting costs land on the legislative
 *   process.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda_setter/beneficiary (institutional/arbitrage) — holds terminal interpretive authority, faces no direct electoral check
 *   - specialized_legal_profession: beneficiary (organized/mobile) — professional rents scale with judicial centrality
 *   - elected_legislature: payer (powerful/constrained) — bears gridlock and redrafting costs when statutes are nullified
 *   - electoral_majorities: payer (powerless/trapped) — enacted preferences overridden with only generational or amendment-threshold recourse
 *   - minority_rights_claimants: beneficiary (moderate/constrained) — the genuine coordination case, protected against transient majorities
 *   - comparative_constitutional_scholars: observer (analytical/global) — sees the cross-national pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.51).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, 'ba495f2a-04b8-45f0-aa11-e6bb9d006780').
narrative_ontology:cs_kernel_codification('ba495f2a-04b8-45f0-aa11-e6bb9d006780', formalized).
narrative_ontology:cs_authority_grounding('ba495f2a-04b8-45f0-aa11-e6bb9d006780', expertise).
narrative_ontology:cs_interpretation_layer_present('ba495f2a-04b8-45f0-aa11-e6bb9d006780').
narrative_ontology:cs_reading_relation('ba495f2a-04b8-45f0-aa11-e6bb9d006780', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('ba495f2a-04b8-45f0-aa11-e6bb9d006780', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('ba495f2a-04b8-45f0-aa11-e6bb9d006780', foundational, specialized_expertise_warrants_terminal_authority).
narrative_ontology:cs_axiom_status(specialized_expertise_warrants_terminal_authority, holdable).
narrative_ontology:cs_axiom_grounding('ba495f2a-04b8-45f0-aa11-e6bb9d006780', specialized_expertise_warrants_terminal_authority, instrumental).
narrative_ontology:cs_axiom('ba495f2a-04b8-45f0-aa11-e6bb9d006780', foundational, electoral_insulation_improves_constitutional_fidelity).
narrative_ontology:cs_axiom_status(electoral_insulation_improves_constitutional_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('ba495f2a-04b8-45f0-aa11-e6bb9d006780', electoral_insulation_improves_constitutional_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('ba495f2a-04b8-45f0-aa11-e6bb9d006780', judicial_review_as_constitutional_settlement).
narrative_ontology:cs_drift_state('ba495f2a-04b8-45f0-aa11-e6bb9d006780', contemporary_polarized_appointments_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba495f2a-04b8-45f0-aa11-e6bb9d006780', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, specialized_legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_claimants).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_safeguard_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits as final arbiter of what the constitutional text means, striking down or upholding legislation through judicial review. Justifies this role through claims of specialized legal training and insulation from electoral pressure. Its rulings are binding until it reverses itself or an amendment supermajority overrides it; it faces no direct electoral accountability and controls the doctrine that defines the scope of its own authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Constitutional litigators, clerks, and academics whose professional standing and income depend on the judiciary retaining terminal interpretive authority. The more contested constitutional questions are resolved by courts rather than legislatures or referenda, the more this profession's specialized expertise is the scarce, monetizable resource.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, specialized_legal_profession, beneficiary,
    organized, generational, mobile, national).

% Passes statutes that can be nullified after the fact by judicial review on constitutional grounds it did not anticipate or does not accept. Bears the costs of gridlock when legislation is struck down and must be redrafted, litigated again, or abandoned. Cannot simply overrule the court's reading without a constitutional amendment process that is deliberately made difficult; its only other lever is slow appointment politics over the composition of the bench.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Vote for representatives and referenda outcomes that can be voided by unelected judges applying a constitutional interpretation the majority never ratified through any vote. Their only recourse is generational: electing officials who will eventually appoint different judges, or pursuing the high-threshold amendment process. In the interval between a court ruling and any correction, majoritarian preferences are structurally overridden.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    powerless, biographical, trapped, national).

% The formal kernel both sides claim to interpret faithfully. Not an actor; listed for completeness as the object of interpretive dispute, not a party that benefits or pays.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_text_and_founders_intent, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_text_and_founders_intent).

% Groups whose rights would likely be voted down by a transient electoral majority sometimes obtain protection precisely because courts are insulated from that majority. This is the strongest genuine coordination case for the reading: an independent judiciary can protect rights the ballot box would not, which is the coordination function riding alongside the extraction this story tracks.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Study cross-national patterns of judicial review to assess whether courts systematically expand their own jurisdiction over time and whether counter-majoritarian outcomes track genuine rights protection or judicial policy preference. Not a party to the dispute; produces evidence other seats cite selectively.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, technically expert, non-majoritarian forum to adjudicate contested constitutional questions and protect entrenched rights against transient political majorities and legislative overreach — a genuine coordination problem (someone must resolve constitutional disputes with finality) that this reading assigns to courts specifically.
% TRANSFER_FUNCTION: Moves final interpretive authority — and the practical power to nullify legislative and electoral outcomes — from elected, accountable bodies to appointed, tenured judges and the specialized legal profession that services constitutional litigation; gridlock costs and redrafting burdens are transferred onto the legislative process.
% ABSENT_VOICES: Electoral majorities whose enacted preferences are struck down have no seat in the litigation that overturns them; they participate only through the distant, high-threshold amendment process or multi-cycle appointment politics. Popular constitutionalist advocates who would prefer ongoing democratic contestation over terminal adjudication are institutionally excluded from the forum where the kernel is actually applied.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight and final interpretive authority reverted to the legislature or dispersed into popular contestation, entrenched constitutional protections currently enforced against majority preference would become renegotiable through ordinary politics, the specialized constitutional bar's leverage would collapse, and legislatures would need new mechanisms to resolve constitutional disputes among themselves — a substantial institutional rearrangement, not a null change.
% FOUNDING_PROBLEM: Early constitutional designers worried that majoritarian legislatures would erode entrenched rights and constitutional limits over time unless some body without direct electoral exposure could hold the line — judicial review was built to solve the problem of self-interested majorities amending away their own constraints through ordinary statute.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and constitutional law faculties attest the problem remains live, citing recent legislative attempts to curtail minority protections. Political scientists studying legislative behavior and comparative constitutionalists studying court-packing and doctrinal drift attest, from outside the judiciary and the legal profession, that the mechanism has substantially shifted toward courts adjudicating ordinary policy disagreements dressed as constitutional questions — corroboration exists on both sides of the contest, and no single outside authority resolves it.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.51, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.51) and suppression (0.58) are set at moderate-high, not extreme, because this reading carries a real coordination function (rights protection against majoritarian erosion, evidenced by the minority_rights_claimants seat) alongside the extraction (professional and institutional rents accruing to an unaccountable body, evidenced by the legislature/electoral majority victim seats). Theater ratio is modest and rising (0.16 to 0.28) reflecting a gradual increase in doctrinal apparatus that performs neutrality while doctrine itself expands judicial jurisdiction over policy questions. Accessibility collapse is fairly high (0.62) because once a court's constitutional ruling is issued, the practical alternatives collapse to slow appointment politics or supermajority amendment — genuinely difficult exits. Resistance (0.55) reflects ongoing political contestation over court composition, jurisdiction-stripping proposals, and court-packing debates — this constraint is actively fought over, not quietly accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is principled counter-majoritarian coordination protecting constitutional commitments from erosion. From the electoral majority's seat, the same structure is an unaccountable veto exercised by appointees who were never on any ballot. The engine should compute these divergently from the shared structural data — the claim (tangled_rope, acknowledging both functions) does not resolve which seat's experience is more 'true'; both are real.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits at the low-d, high-institutional-power end: it sets the rule, is not electorally exposed, and can expand its own doctrine. The specialized legal profession is a secondary beneficiary whose livelihood tracks judicial centrality. Electoral majorities sit at the high-d, trapped end: they cannot exit the jurisdiction, cannot easily reverse a ruling, and bear the cost of having enacted preferences voided. The legislature sits closer to the target end than the judiciary but retains some leverage through appointments and amendment initiation — hence powerful/constrained rather than powerless/trapped. Minority rights claimants are a genuine beneficiary group whose presence is exactly why this is authored as tangled_rope rather than snare: the coordination function is real, not merely claimed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting entrenched constitutional commitments from majoritarian erosion — is genuinely contested as live vs. dead: rights erosion attempts still occur (supporting 'live'), but comparative scholarship also documents courts expanding into ordinary policy adjudication well beyond the original rights-protection mandate (supporting 'dead, function drifted'). This tangled_rope classification, rather than a clean rope or snare, is precisely what prevents mislabeling: a pure rope framing would erase the electoral majority's real cost; a pure snare framing would erase the minority rights protection function that is genuinely operative in some fraction of cases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is judicial supremacy the structurally correct reading of the basic_law_interpretive_authority kernel, or is it one contested reading among parliamentary sovereignty and popular constitutionalism, each internally coherent within its own institutional tradition?',
    'No empirical resolution exists — this is a live normative and institutional-design dispute rather than a fact about the world. Comparative institutional performance data (rights outcomes, gridlock frequency, democratic responsiveness under each regime type) can inform but not settle which reading a polity ought to adopt.',
    'If a polity''s constitutional order genuinely operates under parliamentary sovereignty or popular constitutionalism instead, this story''s entire beneficiary/victim structure is inapplicable to that polity — the judiciary would not occupy the agenda_setter/beneficiary seat, and the extraction this story measures would not exist under the sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three sibling readings of the interpretive-authority kernel actually governs a given constitutional order is contested and not settled by this story.').

omega_variable(
    coordination_extraction_ratio_uncertainty,
    'What fraction of judicial review activity under this reading is genuine counter-majoritarian rights protection (minority_rights_claimants beneficiary case) versus judicial policy preference dressed as constitutional interpretation?',
    'Longitudinal comparative analysis of case outcomes coded by whether the constitutional question was genuinely textually indeterminate versus whether the ruling substituted judicial policy judgment for a plausible legislative reading; cross-national comparison against jurisdictions using sibling readings.',
    'A high genuine-rights-protection fraction would support classifying the coordination function as substantial and push the type toward rope; a low fraction (mostly policy substitution) would push toward snare, since the coordination story would be largely cover for institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_uncertainty, empirical, 'Whether judicial review activity under this reading is mostly genuine rights protection or mostly policy substitution.').

omega_variable(
    founding_intent_naturalness_ambiguity,
    'Is judicial supremacy a naturally emergent feature of any written constitution with an independent judiciary, or a specific, contestable institutional choice that could have been designed differently (e.g., with a legislative override mechanism)?',
    'Comparative constitutional design analysis: jurisdictions with written constitutions and independent judiciaries that nonetheless retained legislative override (e.g., notwithstanding clauses) demonstrate the choice is not forced by the mere existence of a written constitution.',
    'If judicial supremacy is a specific contestable design choice rather than an inevitable consequence of constitutionalism, the reading''s claim to neutrality is weaker and the beneficiary structure (judiciary, legal profession) looks more like an engineered outcome than a natural feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_intent_naturalness_ambiguity, conceptual, 'Whether judicial supremacy is inherent to written constitutionalism or a specific, alterable institutional design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(basi_tr_t36, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 36, 0.24).
narrative_ontology:measurement(basi_tr_t48, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 48, 0.26).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 24, 0.43).
narrative_ontology:measurement(basi_be_t36, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 36, 0.47).
narrative_ontology:measurement(basi_be_t48, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 48, 0.49).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(basi_su_t36, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 36, 0.53).
narrative_ontology:measurement(basi_su_t48, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 48, 0.56).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'who holds final constitutional interpretive authority.' Each sibling reading (judicial_supremacy_reading here; parliamentary_sovereignty_reading; popular_constitutionalism_reading) has its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. They are linked as a constraint family rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
