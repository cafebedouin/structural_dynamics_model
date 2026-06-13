% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause Expansive Federal Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The expansive federal reading of the Commerce Clause treats 'interstate
 *   commerce' as encompassing all economic activity with substantial
 *   aggregate effects on national markets. Under this reading, the federal
 *   government claims regulatory authority over virtually every sector of the
 *   economy: agriculture, manufacturing, labor, environmental quality,
 *   consumer protection, insurance, banking, and telecommunications. This
 *   reading emerged from judicial deference after 1937 and has grown more
 *   elaborate through doctrinal refinement. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination function to solve interstate
 *   regulatory fragmentation, paired with extraction of state authority and
 *   federal dominance) while the authored metrics show moderate to
 *   substantial extraction (0.68) and active enforcement (suppression 0.42,
 *   rising over time). The measurement series show extractiveness stabilizing
 *   in the post-1970 era (plateauing at 0.68) while theater ratio grows (0.12
 *   → 0.28), suggesting that increasingly elaborate justifications maintain a
 *   doctrine whose extractive core has stabilized.
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: institutional beneficiary with control over interpretation boundaries
 *   - national_policy_coherence_advocates: organized beneficiary with real coordination interests
 *   - state_governments: organized victim with constrained exit (cannot unilaterally reclaim authority)
 *   - interstate_commerce_operators: powerful beneficiary gaining uniform compliance regime
 *   - originalist_judicial_coalition: institutional payer/excluded voice subordinated in legitimacy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.42).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause Expansive Federal Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '7010a294-3308-4ed6-9489-5ecfb7056e87').
narrative_ontology:cs_kernel_codification('7010a294-3308-4ed6-9489-5ecfb7056e87', fixed_text).
narrative_ontology:cs_authority_grounding('7010a294-3308-4ed6-9489-5ecfb7056e87', lineage).
narrative_ontology:cs_interpretation_layer_present('7010a294-3308-4ed6-9489-5ecfb7056e87').
narrative_ontology:cs_reading_relation('7010a294-3308-4ed6-9489-5ecfb7056e87', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('7010a294-3308-4ed6-9489-5ecfb7056e87', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('7010a294-3308-4ed6-9489-5ecfb7056e87', foundational, federal_regulatory_authority_plenary_over_substantial_market_effects).
narrative_ontology:cs_axiom_status(federal_regulatory_authority_plenary_over_substantial_market_effects, holdable).
narrative_ontology:cs_axiom_grounding('7010a294-3308-4ed6-9489-5ecfb7056e87', federal_regulatory_authority_plenary_over_substantial_market_effects, empirically_contingent).
narrative_ontology:cs_axiom('7010a294-3308-4ed6-9489-5ecfb7056e87', foundational, state_police_power_subordinate_to_national_market_coherence).
narrative_ontology:cs_axiom_status(state_police_power_subordinate_to_national_market_coherence, holdable).
narrative_ontology:cs_axiom_grounding('7010a294-3308-4ed6-9489-5ecfb7056e87', state_police_power_subordinate_to_national_market_coherence, conventional).
narrative_ontology:cs_reference_frame('7010a294-3308-4ed6-9489-5ecfb7056e87', post_1937_judicial_deference).
narrative_ontology:cs_drift_state('7010a294-3308-4ed6-9489-5ecfb7056e87', contemporary_federalism_revival, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('7010a294-3308-4ed6-9489-5ecfb7056e87', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_regulatory_variation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, interstate_commerce_operators).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, originalist_judicial_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Commerce Clause under the expansive reading, claiming authority over any economic activity deemed to have substantial aggregate effects on interstate commerce. Administer environmental, labor, safety, and consumer protection regimes across all states. Justify the scope as preventing destructive regulatory races and ensuring national market coherence. Set the interpretive boundaries themselves via administrative practice and judicial deference.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from uniform national standards in environmental protection, workplace safety, consumer rights, and financial regulation. Include public health advocates, civil rights organizations, and consumer protection networks. Their benefit is real coordination: fragmented state standards create compliance chaos for national firms and leave exposed populations unprotected. Exit is available through legislative action or constitutional amendment.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the subordination of state regulatory authority to federal override. Lose the ability to set tailored local economic and social policy. Exit is identity-locked: constitutional amendment requires super-majority consensus; the state's core claim to sovereign regulatory authority is directly contradicted by the expansive reading, making exit psychologically and institutionally impossible. The reading treats virtually all state regulation as potentially reviewable for federal preemption.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    organized, generational, identity_locked, national).

% Large national and multinational firms benefit from uniform regulation: single compliance regime rather than fifty state variants. They can arbitrage the boundary (operate in more permissive states for some functions) but the baseline expectation is a unified field. Their benefit is real coordination, though asymmetrically distributed—national firms gain more than local competitors.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, interstate_commerce_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Opposes this reading as a violation of the text's original meaning and the Tenth Amendment's federalism constraint. Their exit is constrained: constitutional interpretation is not unilaterally revocable; doctrine persists until overruled by the same institution. They are partly excluded from the legitimacy-grounding conversation that treats the expansive reading as axiomatic.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_judicial_coalition, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, originalist_judicial_coalition, excluded).

% State and local policymakers who would design tailored regulatory approaches for their populations are excluded from meaningful authority. They are present in the conversation but not positioned as authoritative interpreters of economic regulation; their proposals face federal preemption review and often lose under the expansive doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, regional_policy_experimenters, excluded,
    moderate, biographical, constrained, regional).

% Interprets the Commerce Clause and could revise the expansive reading via doctrine change. Observational position: the court is the seat whose decisions instantiate or alter this constraint, but at any given moment the court system is analytically external to the question of whether the reading is constitutionally correct.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court_institutional_branch, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents destructive regulatory competition among states and establishes a unified national marketplace for goods, services, and labor. Solves the problem that states acting independently might adopt protectionist or race-to-the-bottom policies that fragment the market and harm interstate commerce. Provides a single substantive domain for nationwide firms to navigate labor, safety, environmental, and consumer standards.
% TRANSFER_FUNCTION: Transfers regulatory authority from state and local governments to the federal government and federal administrative agencies. The 'payment' is the loss of jurisdictional autonomy and the inability to implement state-specific or locally-adapted policies. The 'benefit' accrues to national policy advocates (uniform standards) and federal administrative capacity (expanded jurisdiction and budgetary scope).
% ABSENT_VOICES: Originalist constitutional scholars and federalism-protective institutions (tenth-amendment advocates, state attorney generals opposing federal overreach, local democratic bodies) are excluded from the core legitimacy-grounding conversation. They object to the reading but their objection is framed as 'losing' a constitutional dispute, not as a voice legitimately present in the dialogue.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished and the Commerce Clause reverted to originalist limits, federal regulatory reach would shrink dramatically: environmental, labor, civil rights, and consumer protection regimes operating under Commerce Clause authority would lose their federal foundation. States would reassert regulatory authority; national markets would fragment or require interstate treaties; compliance costs for national firms would rise sharply. The entire post-1940s federal regulatory state depends structurally on this reading.
% FOUNDING_PROBLEM: In the 1930s, the Supreme Court struck down New Deal regulatory programs as exceeding federal power, paralyzing national economic recovery during the Great Depression. The expansive reading emerged from judicial deference (post-1937) that repositioned the Commerce Clause as a broad grant of federal authority over all economic activity affecting interstate commerce.
% FOUNDING_PROBLEM_CORROBORATION: The federal administrative state and national policy advocates attest the founding problem is live: economic fragmentation and lack of uniform standards create destructive races to the bottom. Originalist scholars and state attorneys general attest the founding problem was a judicial error (striking down constitutional regulation was wrong, but the fix was to respect state authority, not to abandon the Constitution's text). Independent constitutional historians (e.g., Randy Barnett, Keith Whittington, Jack Balkin) provide corroboration from outside the benefiting parties for BOTH interpretations—the historical record supports that the founding problem was real AND that the expansive reading is a substantial departure from the text's original meaning.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the reading transfers regulatory authority from state and local seats to the federal government, and the federal government's scope grew substantially over the interval (1937–2026). The coordination function is real: uniform national standards solve genuine problems of market fragmentation and races to the bottom. But the measurement captures the asymmetry: the beneficiaries (federal agencies, national firms, policy advocates) gain scope and influence; the victims (state governments, local variation) lose authority and flexibility. Suppression is lower than extractiveness (0.42) because the constraint is legitimated through constitutional text interpretation and sustained by institutional momentum rather than overt coercion—the state governments must comply with federal preemption doctrine, but the compliance is enforced through judicial review, not police power. Theater grows over time (0.12 → 0.28) suggesting that as the original founding problem (preventing Depression-era paralysis) receded, increasingly elaborate doctrinal justifications (substantial effects test, even-if-rationally-related test) maintained the constraint's reach. Accessibility collapse (0.71) reflects that states cannot easily exit the constraint: constitutional amendment requires supermajority consensus, and the constraint's operation is embedded in decades of federal statute and administrative practice. Resistance (0.58) captures ongoing constitutional objections from federalism advocates, but resistance remains at the doctrinal and political level rather than operational defiance.
 *
 * PERSPECTIVAL GAP:
 *   From the federal regulatory seat and the national policy advocate seat, this reading is genuine coordination solving a real collective-action problem—states left to themselves would fragment the market. From the state government and local regulator seats, the same structure operates as federal dominance and the loss of democratic authority over local economic affairs. The engine computes these seat divergences from the structural data: federal agencies as institutional beneficiaries with arbitrary power to define 'substantial effects' sit at the extractive end; state governments as organized victims with constrained exit sit at the target end. The claimed type (tangled_rope) asserts both coordination and extraction are present; the metrics quantify the balance at 0.68 extraction with real but subordinated coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies benefit from expanded jurisdiction and budgetary scope (d near 0.0 on the beneficiary end). National policy advocates benefit from uniform standards and reduced regulatory uncertainty (d near 0.1–0.2). State governments are victims: they bear the loss of regulatory authority and must navigate federal preemption; they have constrained exit (identity_locked at institutional level—the state's core claim to sovereign authority is directly contradicted, making exit psychologically and constitutionally impossible) (d near 0.9). Interstate commerce operators benefit from uniform regime (d near 0.1). Originalist judicial coalitions are constrained payers: they lose interpretive authority over their own constitutional theory (d near 0.7–0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy because the founding problem (preventing economic paralysis through fragmented state regulation) remains live—national firms still depend on uniform standards, and state regulatory races continue to be a concern. The theater ratio growth (0.12 → 0.28) and the extractiveness plateau (stabilizing at 0.68) suggest the doctrine's justifications are becoming more elaborate while the core extraction persists unchanged, but this is characteristic of a mature tangled_rope, not a piton. A piton reading would show the founding problem status as 'dead' and the theater ratio approaching 0.7+; neither condition holds here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_vs_tradition_ambiguity,
    'Is the authoritative reading of the Commerce Clause determined by the text''s original public meaning in 1787, by the text''s semantic evolution through subsequent constitutional practice, or by the practical consequences of each reading?',
    'This is a foundational methodological question in constitutional interpretation. Resolution depends on endorsing a hermeneutic theory (originalism, living constitutionalism, pragmatism) that is itself contested. Evidence of settled practice (uniform national regulation for 80+ years) might ground the expansive reading, but originalist scholars argue settled practice cannot override constitutional text.',
    'If original meaning controls, the originalist_narrow_reading becomes the structurally true constraint and this reading (expansive_federal) becomes a false summit masquerading as constitutional law. If practice and consequences control, this reading is legitimately constitutive. If the methodological question is genuinely undecidable from within constitutional discourse, all three readings remain live and mutually foreclosing is inappropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_vs_tradition_ambiguity, conceptual, 'Whether the constitutive method for reading the Commerce Clause is text/original meaning, evolved practice, or pragmatic consequence.').

omega_variable(
    substantial_effects_boundary_vagueness,
    'What counts as ''substantial'' effects on interstate commerce? Is growing wheat for personal consumption ''substantial''? What about intrastate pollution with spillover air/water effects? What about discrimination in a local business affecting interstate firms'' ability to do business?',
    'Judicial doctrine has answered these questions inconsistently—compare Wickard v. Filburn (wheat for personal use is substantial) to United States v. Morrison (regulating non-economic intrastate violence is not substantial). The vagueness is structural: no metric for ''substantiality'' exists independent of the court''s determination. Natural experiment: federal courts'' application of the doctrinal test across circuits and over time would show whether ''substantial effects'' has a stable empirical referent or operates as a post-hoc cover for the court''s desired result.',
    'High vagueness would suggest the ''substantial effects'' test functions as doctrinal theater—the real boundary is wherever the court decides it should be, and the test merely provides legitimation language. This feeds the theater ratio growth visible in measurements. Lower vagueness (a stable, predictable boundary) would support that the test is a genuine limiting principle even if its derivation is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effects_boundary_vagueness, empirical, 'Whether ''substantial effects'' operates as a stable limiting principle or as doctrinal theater concealing discretionary boundary-setting.').

omega_variable(
    coordination_vs_dominance_boundary,
    'Is the expansive reading REQUIRED to solve the coordination problem of interstate regulatory fragmentation, or could a narrower reading (originalist or substantial-effects-with-nexus) achieve the same coordination while preserving more state autonomy?',
    'Counterfactual: if the originalist reading controlled, would states collectively arrive at uniform standards through interstate compacts, reciprocal agreements, or federal-state negotiation? Empirical check: Do countries with more federal systems and narrower central authority face worse regulatory fragmentation than the U.S. under the expansive reading?',
    'If narrower readings could achieve coordination, the expansive reading extracts more state authority than strictly necessary—it is a snare disguised as tangled_rope. If narrower readings cannot achieve coordination (state race-to-the-bottom dynamics prevent voluntary coordination), the extraction is justified as coordination cost and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_dominance_boundary, empirical, 'Whether federal dominance is structurally necessary for interstate commerce coordination or whether subordinate readings could achieve the same coordination with less state authority extraction.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Do this reading (expansive federal) and the originalist_narrow_reading mutually foreclose each other—such that no single constitutional framework could hold both—or do they coexist as live positions held by different institutional and political factions?',
    'Institutional test: both readings have been endorsed by credible Supreme Court Justices and constitutional scholars in contemporary discourse. Neither reading has been formally repudiated by the Court as incoherent or logically impossible. This suggests coexistence rather than foreclosure. Foreclosure would require one reading to logically entail the negation of the other''s core premise—but both readings claim to faithfully interpret the same text, suggesting they are competing interpretations, not logically incompatible.',
    'Coexistence means both readings remain defensible within constitutional law and the choice between them is a matter of institutional politics and hermeneutic methodology. Foreclosure would mean this reading''s truth makes originalism impossible. The committer frame should treat this as coexistence: both readings are live, the constraint-generation task is to instantiate each as a separate, ε-invariant constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the expansive and originalist readings logically foreclose each other or coexist as live constitutional positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_text__expansive_federal_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_text__expansive_federal_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t45, commerce_clause_text__expansive_federal_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(comm_tr_t45, observed).
narrative_ontology:measurement(comm_tr_t60, commerce_clause_text__expansive_federal_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(comm_tr_t60, observed).
narrative_ontology:measurement(comm_tr_t75, commerce_clause_text__expansive_federal_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement_basis(comm_tr_t75, observed).
narrative_ontology:measurement(comm_tr_t90, commerce_clause_text__expansive_federal_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(comm_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t15, commerce_clause_text__expansive_federal_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t30, commerce_clause_text__expansive_federal_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t45, commerce_clause_text__expansive_federal_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement_basis(comm_be_t45, observed).
narrative_ontology:measurement(comm_be_t60, commerce_clause_text__expansive_federal_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement_basis(comm_be_t60, observed).
narrative_ontology:measurement(comm_be_t75, commerce_clause_text__expansive_federal_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(comm_be_t75, observed).
narrative_ontology:measurement(comm_be_t90, commerce_clause_text__expansive_federal_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(comm_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t15, commerce_clause_text__expansive_federal_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t30, commerce_clause_text__expansive_federal_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t45, commerce_clause_text__expansive_federal_reading, suppression_requirement, 45, 0.39).
narrative_ontology:measurement_basis(comm_su_t45, observed).
narrative_ontology:measurement(comm_su_t60, commerce_clause_text__expansive_federal_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(comm_su_t60, observed).
narrative_ontology:measurement(comm_su_t75, commerce_clause_text__expansive_federal_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(comm_su_t75, observed).
narrative_ontology:measurement(comm_su_t90, commerce_clause_text__expansive_federal_reading, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(comm_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% The expansive federal reading is one of three structurally distinct readings of the same constitutional kernel (commerce_clause_text). Each reading instantiates a different constraint because they measure the Commerce Clause's scope using different interpretive methodologies (original public meaning vs. living Constitution vs. practical necessity) and thus yield different ε values. The three readings form a constraint family: expansive_federal is the most regulatory/coordinative/extractive; originalist_narrow is the most libertarian/limiting/non-extractive; substantial_effects_limited positions itself between. All three readings share the founding kernel (the constitutional text) but differ in how they parse it. Link all three via network.affects_constraints to enable contamination and foreclosure analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
