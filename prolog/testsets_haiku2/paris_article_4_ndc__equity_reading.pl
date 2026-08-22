% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Framework (Equity Reading)
 *   domain: international/climate/political_economy
 *
 * SUMMARY:
 *   Article 4 of the Paris Agreement obligates parties to pursue mitigation
 *   efforts to limit warming to 1.5–2°C; Article 13 establishes transparency
 *   mechanisms; the Nationally Determined Contribution (NDC) is the vehicle
 *   for pledges. The equity reading interprets these provisions through the
 *   lens of Common But Differentiated Responsibilities (CBDR), a principle
 *   embedded in the UNFCCC but contested in its application to NDCs. Under
 *   this reading, the Paris framework structurally recognizes that developed
 *   states (historical emitters, high per-capita income, technology
 *   capability) bear asymmetric obligations: binding, ratcheting targets,
 *   verified progress, finance transfers. Developing states retain policy
 *   space to balance climate action with development: their NDCs may be
 *   conditional on finance and technology, lower-ambition commitments are
 *   consistent with equity, and supranational enforcement is constrained by
 *   equity principles. This reading stands in structured opposition to
 *   sovereigntist readings (NDCs are purely voluntary national pledges, no
 *   differentiation) and supranational readings (NDCs are binding commitments
 *   toward global net-zero, identical accountability across states). The
 *   equity reading does NOT assert that developed states' extraction of value
 *   from developing states is acceptable — rather, it asserts that
 *   recognizing asymmetric obligation is the structural price of holding the
 *   global coalition together and directing finance toward those facing the
 *   greatest impacts.
 *
 * KEY AGENTS:
 *   - Equity coalitions (AOSIS, LDC group, African Union): secure structural recognition of differentiation and veto power over enforcement
 *   - Developed state treasuries: bear binding obligations, finance commitments, technology transfer — the material payers
 *   - Developing state constituencies: retain policy space and legitimacy for development-first framing; also bear transition costs
 *   - Supranational enforcement bodies (UNFCCC, Article 15 committee): constrained to preserve equity frame or lose legitimacy
 *   - Sovereigntist interpreters (energy exporters, sovereignty-first states): excluded from consensus, carry veto in formal negotiation
 *   - Climate science community: provides empirical warrant (1.5°C pathway) that all readings cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.58).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.42).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Framework (Equity Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international/climate/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'd73aa98e-d858-4e99-960e-4e6490979cd6').
narrative_ontology:cs_kernel_codification('d73aa98e-d858-4e99-960e-4e6490979cd6', formalized).
narrative_ontology:cs_authority_grounding('d73aa98e-d858-4e99-960e-4e6490979cd6', extraction).
narrative_ontology:cs_interpretation_layer_present('d73aa98e-d858-4e99-960e-4e6490979cd6').
narrative_ontology:cs_reading_relation('d73aa98e-d858-4e99-960e-4e6490979cd6', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d73aa98e-d858-4e99-960e-4e6490979cd6', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('d73aa98e-d858-4e99-960e-4e6490979cd6', foundational, differentiated_responsibility_structural_necessity).
narrative_ontology:cs_axiom_status(differentiated_responsibility_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d73aa98e-d858-4e99-960e-4e6490979cd6', differentiated_responsibility_structural_necessity, deontological).
narrative_ontology:cs_axiom('d73aa98e-d858-4e99-960e-4e6490979cd6', foundational, development_sovereignty_conditional_on_climate_finance).
narrative_ontology:cs_axiom_status(development_sovereignty_conditional_on_climate_finance, holdable).
narrative_ontology:cs_axiom_grounding('d73aa98e-d858-4e99-960e-4e6490979cd6', development_sovereignty_conditional_on_climate_finance, instrumental).
narrative_ontology:cs_reference_frame('d73aa98e-d858-4e99-960e-4e6490979cd6', cbdr_differentiated_ndc_obligations).
narrative_ontology:cs_drift_state('d73aa98e-d858-4e99-960e-4e6490979cd6', contemporary_2024_finance_shortfall, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d73aa98e-d858-4e99-960e-4e6490979cd6', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_constituencies).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_treasuries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developing_state_constituencies).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalition of Small Island Developing States, Least Developed Countries, and African Union members. Leverage NDC interpretation to secure structural recognition of historical responsibility distinctions, access to loss-and-damage funds, and veto power over unilateral enforcement mechanisms. The equity reading permits them to claim differential obligations as a structural feature of the framework, not as charitable exception.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions_vulnerable_states, beneficiary,
    organized, generational, constrained, global).

% Face binding climate finance commitments, technology transfer obligations, and loss-and-damage contributions as the structural price of the equity reading's legitimacy. Their NDCs are interpreted under higher scrutiny (binding, ratcheting, verified); their commitments to support developing states are non-negotiable under this reading's framing, not discretionary aid.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_treasuries, payer,
    institutional, biographical, constrained, national).

% Retain policy space to prioritize development needs within their NDCs under the equity reading: energy access, poverty reduction, infrastructure buildout are legitimate claims on their climate action. They also bear transition costs and climate impacts. The equity reading grants them structural recognition as distinct from developed states in obligation design, even as they participate in global mitigation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_constituencies, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_constituencies, payer).

% UNFCCC secretariat, Article 6 mechanisms, Article 15 committee (Enhanced Transparency Framework). Under the equity reading, their enforcement authority is constrained by equity principles: they cannot impose identical standards across developed and developing states, and their decisions must preserve differentiation. Their legitimacy depends on maintaining the equity frame.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_enforcement_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Domestic constituencies in wealthy nations bear the cost of aggressive climate action domestically (energy transition, subsidy transfers, trade adjustment) and finance obligations internationally. They resist binding interpretation of NDCs when it increases their state's obligations relative to developing peers. Their exit options are constrained by democratic accountability and WTO/trade dependencies.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_constituencies, payer,
    organized, biographical, constrained, national).

% States that resist the equity reading in favor of full national sovereignty framing (e.g., some energy-exporting states, states prioritizing development over climate). The equity reading's structural differentiation is interpreted by them as pressure to commit further. They are excluded from the consensus on equity-as-framework but carry veto power in formal negotiation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, sovereigntist_interpreters, excluded,
    institutional, biographical, trapped, national).

% IPCC and research institutions provide the empirical referent (1.5°C pathway, emissions budgets) against which all NDC readings are measured. They remain analytically outside the equity/sovereignty/supranational debate but their conclusions about feasibility and cost-distribution feed all three readings' legitimacy claims.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_science_community, observer,
    powerful, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, developed_state_treasuries).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for nationally-determined climate pledges structured by recognition of differentiated historical responsibility, current capability, and vulnerability. Solves the collective-action problem of global mitigation by permitting heterogeneous commitments and obligations that vary by development status, rather than imposing uniform standards.
% TRANSFER_FUNCTION: Moves climate finance, technology, and capacity-building resources from developed to developing states, justified by historical emissions responsibility and current capability asymmetry. Also moves legitimacy: the equity reading grants developing states narrative authority to claim they are *not* free-riders but structurally-recognized partners with distinct obligations.
% ABSENT_VOICES: Energy-exporting developing states that resist climate ambition; domestic constituencies in developing states that oppose the development-versus-climate framing; countries that view the equity reading as a constraint on unilateral climate action (e.g., states pursuing unilateral net-zero pledges independent of the framework). Sovereigntist and supranational readings represent these positions but are structurally excluded when the equity reading dominates the interpretation.
% DISAPPEARANCE_RATIONALE: If the equity reading's structural differentiation vanished overnight and NDCs were reframed as identical binding commitments across all states, developing nations would withdraw from the framework (loss of policy space and finance legitimacy), climate finance commitments would evaporate as obligation rather than equity-grounded transfer, and the global coalition holding Paris together would collapse. Alternatively, a shift to pure sovereigntist reading would eliminate supranational accountability and finance obligations entirely.
% FOUNDING_PROBLEM: The Paris Agreement had to solve the paradox of binding international climate action without coercive enforcement capacity: how to get commitments from actors with vastly different historical responsibility, current capacity, and development needs without either (a) imposing impossible cost on developing states or (b) allowing developed states to free-ride. The equity reading solves this by structurally distinguishing what counts as a 'binding commitment' based on development status.
% FOUNDING_PROBLEM_CORROBORATION: The UNFCCC, equity coalitions (AOSIS, LDC group, African Union), and development NGOs all attest the problem remains live: developed states have not met finance commitments, developing states face severe climate impacts while bearing transition costs, and the differentiation framework is the only structure preventing agreement collapse. Sovereigntist and supranational readings dispute whether the equity frame is the right solution, but both concede the founding problem exists.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because developed states' finance commitments are structurally non-negotiable under this reading, even as compliance remains weak and contested. The reading extracts legitimacy and resource obligation from wealthy nations in the name of equity. Suppression is lower (0.42) than a snare would be because the equity frame is not held in place primarily by coercion — it is held by coalition consensus and the threat of framework collapse if developing states defect. Developing states are not trapped into accepting this reading; they actively defend it as their structural leverage. Theater is moderate (0.28) and stable: some enforcement activity is genuinely about transparency and accountability (real theater), but a growing share is performative climate-finance pledging without delivery (false theater). The measurement series tracks rising extractiveness (finance pressure on developed states increases over the interval) and stable suppression (the coalition holds but does not tighten the screws further). The equity reading's stability through 2030 projects to a plateau: the tension is no longer whether the reading exists, but whether it delivers material transfers.
 *
 * PERSPECTIVAL GAP:
 *   Developed states experience this reading as a constraint on their sovereignty; developing states experience it as protection of their policy space; supranational bodies experience it as the boundary of their authority. The core asymmetry is structural: the reading extracts obligation from wealthy actors to justify non-binding commitments from developing actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states are the targets of asymmetric obligation: they pay climate finance, transfer technology, accept higher accountability — directionality near 1.0 (full targets). Equity coalitions are beneficiaries of the structural recognition that differentiation is legitimate and coalition veto is operative: directionality near 0.0 (beneficiaries). Developing state constituencies sit near 0.5: they benefit from policy space but also bear transition costs and climate impacts. Sovereigntist interpreters are neither beneficiaries nor payers under this reading; they are excluded and constrained. The supranational agenda-setter position is mobile and dependent on equity consensus: they maintain authority only by preserving the equity frame, so their directionality is constrained by the beneficiary coalition's will, moderately extracted (around 0.4–0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (binding global mitigation without coercive enforcement, across vastly unequal states) remains LIVE, not dead. The equity reading IS the mechanism designed to solve it: asymmetric obligations are the structural price of holding the coalition. Mandatrophy would trigger if the reading persisted after the problem was solved (e.g., if developed states had achieved zero emissions and developing states had decoupled development from emissions, making historical-responsibility distinctions irrelevant) — but climate impacts are accelerating, development needs persist, and finance commitments remain unmet. The constraint is not yet a zombie. However, the theater_ratio measurement (growing performative finance pledging without delivery) signals latent mandatrophy risk: if the equity reading survives while the material transfer function atrophies, it will become a pure extraction mechanism dressed in equity language. The measurement trajectory should flag for review circa 2027.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    differentiation_necessity,
    'Is structural differentiation between developed and developing states necessary for the Paris coalition to hold, or could a supranational reading enforce identical standards while preserving overall participation?',
    'Counterfactual: what happens if Article 15 committee shifts interpretation toward identical accountability? Observable outcome: do developing states maintain NDC compliance and finance participation, or do they withdraw/reduce ambition?',
    'If differentiation is necessary for coalition stability, the equity reading is genuine coordination (Rope-ish). If identical standards could be enforced without defection, differentiation is pure extraction cover (Snare-ish). The separation hinges on whether developing states'' participation is coalition-conditional or independent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_necessity, empirical, 'Whether equity-based differentiation is a structural requirement for Paris coalition viability or a disguise for asymmetric extraction.').

omega_variable(
    developed_state_compliance,
    'Do developed states genuinely experience their finance commitments as binding structural obligation under the equity reading, or as discretionary aid that can be deferred/negotiated downward?',
    'Track developed state behavior in missed finance deadlines, conditions attached to transfers, and willingness to enforce peer accountability. Observe whether they treat finance as non-negotiable treaty obligation or as budget item subject to domestic political reversal.',
    'True binding (extractive for developed states) → the equity reading has enforcement teeth. Treated as discretionary → the reading is theatrical, benefiting equity coalitions in narrative only, not material transfer. This shapes whether the constraint is Tangled Rope or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developed_state_compliance, empirical, 'Whether developed states treat equity-based finance obligations as legally binding or politically negotiable.').

omega_variable(
    sovereignty_versus_differentiation,
    'Does the equity reading''s structural differentiation represent genuine recognition of distinct circumstances, or does it inscribe a power hierarchy that preserves developed-state leverage while claiming equity language?',
    'Compare the equity reading''s prescriptions with what developing states would choose if unrestricted: do they retain genuine policy space, or does the differentiation framework constrain them more than an identical-standards framework would (by permitting lower ambition, but also permitting lower finance access)?',
    'If equity differentiation genuinely expands developing-state policy space and material receipt, it is Tangled Rope benefiting them. If it preserves developed-state control over the pace/terms of global mitigation while permitting lower commitment from developing states, it is a more subtle Snare using equity language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_versus_differentiation, conceptual, 'Whether the equity reading''s differentiation framework reflects genuine asymmetry or disguised power preservation.').

omega_variable(
    enforcement_asymmetry,
    'Why is supranational enforcement asymmetrically applied: developed states face transparent reporting and peer review, while developing states'' reporting is conditional on finance access (i.e., they are not penalized for non-compliance the same way)? Is this equity, or differential enforcement of identical rules?',
    'Article 15 committee produces data on compliance-gap patterns: does enforcement disparity track development status, or is it artifact of finance conditionality (which itself would be the equity mechanism)?',
    'If enforcement asymmetry IS the equity mechanism (different accountability standards because different circumstances), the reading is coherent. If enforcement is identical in principle but differentially applied because of finance leverage, the reading disguises an enforcement hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether enforcement asymmetry is equity differentiation or covert unequal application of identical rules.').

omega_variable(
    kernel_reading_contention,
    'The Paris kernel text is subject to three readings: equity, sovereigntist, and supranational. Which reading does the kernel''s literal text support, or is the kernel structurally ambiguous such that all three readings are valid instantiations?',
    'Textual analysis of Article 4 and Article 13 (UNFCCC archives, negotiation records, preparatory documents). Does the text privilege one reading or explicitly preserve ambiguity to achieve consensus?',
    'If equity is the text''s clear meaning, supranational and sovereigntist readings are misreadings. If the text is ambiguous, all three are valid readings with equal structural claim. If the text privileges supranational language but equity consensus was the negotiation settlement, the kernel is a surface compromise masking deep contention (high mandatrophy risk).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention, empirical, 'Whether the Paris kernel text privileges the equity reading or preserves structural ambiguity across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__equity_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(pari_tr_t2018, observed).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement_basis(pari_tr_t2021, observed).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2024, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__equity_reading, theater_ratio, 2030, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__equity_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement_basis(pari_be_t2018, observed).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement_basis(pari_be_t2021, observed).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement_basis(pari_be_t2024, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.58).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__equity_reading, base_extractiveness, 2030, 0.58).
narrative_ontology:measurement_basis(pari_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__equity_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement_basis(pari_su_t2018, observed).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.41).
narrative_ontology:measurement_basis(pari_su_t2021, observed).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(pari_su_t2024, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.42).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__equity_reading, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement_basis(pari_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.18).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, unfccc_loss_and_damage_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, climate_finance_green_climate_fund).

% DUAL FORMULATION NOTE:
% The paris_article_4_ndc kernel admits three structurally distinct readings: equity_reading (this constraint), sovereigntist_reading (voluntary pledges, no differentiation, national sovereignty paramount), and supranational_reading (binding commitments, identical accountability, supranational enforcement). Each reading instantiates different epsilon (equity: 0.58 moderate extractiveness; sovereigntist: lower epsilon because no transfer function; supranational: higher epsilon because supranational authority is more coercive). The readings coexist in the global climate governance space, with equity coalition and developing states defending equity, some developed and energy-exporting states defending sovereigntist, and supranational bodies and some wealthy-state constituencies pushing supranational. This constraint (equity_reading) is upstream to the other two in that equity coalition consensus constrains both. The network edge indicates structural influence, not causal determination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
