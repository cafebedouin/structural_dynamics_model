% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Reform via Literacy Maximization (Instrumentalist Reading)
 *   domain: political/linguistic/commitment-system
 *
 * SUMMARY:
 *   A state undertakes orthographic reform justified through literacy
 *   statistics and administrative efficiency metrics. The instrumentalist
 *   reading frames script choice as a pragmatic tool: the new orthography is
 *   phonetically transparent, pedagogically efficient, and measurable by
 *   literacy rates. This reading claims the reform solves a genuine
 *   coordination problem (unified script standardization) and benefits newly
 *   literate populations and state administrators. The structural asymmetry:
 *   Arabic-literate elites and traditional clergy lose professional authority
 *   as their previous skills are devalued; they are victims not because of
 *   explicit coercion but because their human capital becomes structurally
 *   obsolete. The reading's legitimacy rests on efficiency metrics and
 *   literacy statistics, not on civilizational narrative or identity
 *   claims—distinguishing it sharply from the continuity reading (which
 *   emphasizes preserving access to tradition) and the modernist reading
 *   (which emphasizes Western rupture). This is ONE READING of the
 *   orthographic legitimacy kernel; sibling readings would produce different
 *   ε values, different beneficiary/victim maps, and different type
 *   classifications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Reform via Literacy Maximization (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/commitment-system").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '950fe568-a06b-42bc-addc-29c63f00c63a').
narrative_ontology:cs_kernel_codification('950fe568-a06b-42bc-addc-29c63f00c63a', fixed_text).
narrative_ontology:cs_authority_grounding('950fe568-a06b-42bc-addc-29c63f00c63a', lineage).
narrative_ontology:cs_interpretation_layer_present('950fe568-a06b-42bc-addc-29c63f00c63a').
narrative_ontology:cs_reading_relation('950fe568-a06b-42bc-addc-29c63f00c63a', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('950fe568-a06b-42bc-addc-29c63f00c63a', orthographic_legitimacy_kernel__modernist_reading, influences).
narrative_ontology:cs_axiom('950fe568-a06b-42bc-addc-29c63f00c63a', foundational, script_choice_is_instrumental_tool).
narrative_ontology:cs_axiom_status(script_choice_is_instrumental_tool, holdable).
narrative_ontology:cs_axiom_grounding('950fe568-a06b-42bc-addc-29c63f00c63a', script_choice_is_instrumental_tool, empirically_contingent).
narrative_ontology:cs_axiom('950fe568-a06b-42bc-addc-29c63f00c63a', foundational, legitimacy_derives_from_literacy_maximization).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_literacy_maximization, holdable).
narrative_ontology:cs_axiom_grounding('950fe568-a06b-42bc-addc-29c63f00c63a', legitimacy_derives_from_literacy_maximization, instrumental).
narrative_ontology:cs_reference_frame('950fe568-a06b-42bc-addc-29c63f00c63a', efficiency_based_script_standardization).
narrative_ontology:cs_drift_state('950fe568-a06b-42bc-addc-29c63f00c63a', contemporary_post_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('950fe568-a06b-42bc-addc-29c63f00c63a', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_researchers).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, international_development_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains access to written language, primary education, and state administrative positions through script reform. The new orthography is designed to be phonetically transparent and pedagogically efficient, reducing time to basic literacy. Receives direct benefit from expanded educational opportunity and labor market access in the state bureaucracy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    organized, generational, mobile, national).

% Designs, enforces, and standardizes the new orthography through education policy, official correspondence, and legal documents. Justifies the reform by citing literacy statistics, administrative efficiency gains, and reduced record-keeping complexity. Benefits from faster training of clerks and reduced encoding/decoding errors in bureaucratic communication.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary).

% Possesses specialized training in the previous orthography (often Arabic script or classical forms) that becomes devalued under the reform. Their accumulated cultural and economic capital—based on linguistic expertise—loses market value as the state transitions education systems. They must either retrain (expensive, identity-threatening) or watch their professional privilege erode.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, biographical, constrained, national).

% Historically maintained legitimacy through custody of sacred texts in the previous orthography. Script reform threatens access to religious authority: a new generation trained only in the reformed script cannot read classical religious texts without additional specialized training. Their identity is fused with the previous orthography; exit means doctrinal apostasy in their own self-understanding.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_clergy, payer,
    powerful, generational, identity_locked, national).

% Propose alternative orthographies (different romanization systems, phonetic variants, or preservation of classical forms) that would serve literacy and efficiency equally well but are blocked by the state's standardization mandate. Would participate in the design process if admitted; their exclusion is enforced through education policy and official documentation monopoly.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, rival_script_advocates, excluded,
    moderate, biographical, constrained, national).

% Produce the empirical data—literacy statistics, reading-speed comparisons, cognitive load studies—that justify the reform. Benefit professionally from the reform's status as state policy and from continued funding for literacy measurement. Are positioned as objective observers but are embedded in the legitimacy apparatus.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_researchers, beneficiary,
    analytical, biographical, arbitrage, national).

% Provide technical assistance, funding, and international validation for the reform. Frame orthographic standardization as a best practice in developmental literacy policy. Benefit from the state adopting their frameworks and from the state's reported literacy gains, which validate the development model.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, international_development_agencies, beneficiary,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of script standardization: a shared orthography reduces transaction costs in education, administration, and commerce. Without standardization, mixed writing systems impose encoding/decoding overhead on every bureaucratic transaction. The reform achieves genuine coordination by solving this cost problem.
% TRANSFER_FUNCTION: Transfers cultural authority and professional privilege from Arabic-literate elites and traditional clergy to newly literate populations and state administrators. Moves social status from classical-text expertise to literacy-statistics-validated competence. Moves educational resources from elite religious schools to state primary education systems.
% ABSENT_VOICES: Rival orthography designers, continuity advocates who would preserve classical forms alongside new ones, and cultural traditionalists are excluded from the design process. They would argue for polyglotism, gradual transition, or preservation of classical access, but are kept out of official deliberation by the state's monopoly on education policy and the instrumental efficiency frame that treats alternatives as inherently inferior.
% DISAPPEARANCE_RATIONALE: If the instrumentalist reform vanished overnight, the state would face immediate administrative friction: mixed writing systems would return, education would fragment into competing pedagogies, and literacy gains would plateau. The coordination function is real—without a shared standard, bureaucratic systems degrade. The world would reorganize around some competing standard (perhaps the restored classical form, perhaps a rival romanization system), not remain unchanged.
% FOUNDING_PROBLEM: The state inherited a patchwork of scripts—classical Arabic, local variants, early romanization experiments—that prevented unified education and created efficiency losses in administration. Training different cohorts in different systems was slow, error-prone, and expensive.
% FOUNDING_PROBLEM_CORROBORATION: International literacy researchers, state planners, and independent educational economists attest the founding efficiency problem is real and persistent. The orchestration of the reform was documented by state ministries and reported by outside development agencies. Testimony from populations newly literate under the system and from administrative analysts confirms the coordination gains in practice.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The instrumentalist reading measures extractiveness at 0.58 (end of interval) because the reform does solve a genuine coordination problem (high beneficiary function) but imposes asymmetric costs on Arabic-literate elites and clergy whose skills are devalued—a real extraction, but not purely predatory. Suppression (0.62) is moderate-high: the state must actively suppress rival orthographies and limit alternative education pathways to enforce the standard, but this suppression serves the coordination function, not pure rent-collection. Theater (0.28) is low-moderate: the efficiency justification is substantive (literacy statistics are real data), but the intensity of enforcement rises over time as the elite resistance hardens, suggesting increasing proportion of suppression is devoted to defending the extraction itself rather than maintaining coordination. The measurement series trace this drift: extractiveness rises early (t=0 to t=20) as the elite losses compound, then plateaus (t=20 onward) as new cohorts are fully trained and the reform is normalized. Suppression peaks around t=15–20 (maximum resistance from clergy and elites seeking to preserve classical access), then declines slightly as normalized education structures reduce active resistance. Theater rises steadily (t=0 to t=25) as efficiency rhetoric increasingly covers the extraction of elite authority, then stabilizes. This pattern is consistent with a rope-type constraint whose coordination function is real but whose extraction of elite privilege is substantial and sustained by active suppression—not a pure snare (coordination is genuine), not a mountain (suppression is high and extractiveness is substantial), but a rope that carries significant asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The state administrative apparatus and newly literate beneficiaries perceive this as coordination (genuine efficiency gain, expanded opportunity). From their seats, the constraint solves a real problem and benefits are widely shared. The Arabic-literate elite and clergy perceive extraction: their previous authority is dismantled, their skills are devalued, and they have constrained exit options (traditional clergy are identity-locked; elites can retrain but at substantial cost). The engine computes per-seat classification from this structural asymmetry: the beneficiary seats should classify the constraint as rope (coordination), while the payer seats should classify it as snare or tangled_rope (extraction). The divergence is the measurement the system exists to take—it emerges from the authored beneficiary/victim data and exit option differentials, not from tuning either the claim or the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrators are near the beneficiary end (d ≈ 0.25): they set the rules, benefit from efficiency gains, and have arbitrage-grade exit (they could reverse the reform if costs exceeded benefits). Newly literate populations are beneficiaries with moderate d (≈ 0.35): they benefit substantially from expanded literacy access but have less direct control over the rules and constrained exit (the reform is already embedded in the education system). Arabic-literate elites are targets with high d (≈ 0.75): their skills are devalued, they have powerful institutional resources but constrained exit (retraining is expensive, maintaining the old orthography is increasingly illegal), and they cannot credibly threaten system collapse. Traditional clergy are near-full targets (d ≈ 0.85): their identity is fused with the previous orthography (identity_locked exit), they bear real costs to religious authority, and they have organized power but trapped ability to exit without doctrinal apostasy. International development agencies sit near beneficiary (d ≈ 0.20): they benefit from the reform being adopted as best practice, but their exit is arbitrage (they can fund other reforms elsewhere). These directionality values are derived from the authored beneficiary/victim declarations and exit options; they feed the engine's effective extraction computation. No overrides are needed; the structural data produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The instrumentalist reading declares founding_problem_status = live: the state and international observers attest the coordination problem (mixed scripts, administrative friction) remains live and is solved by the reform. This classification prevents false mandatrophy labeling—the constraint is not a zombie preserving a dead founding function. However, there is a secondary mandatrophy risk: the constraint may accumulate into a snare over time if the extraction of elite authority becomes the actual function (theater ratio rising, orthography used increasingly as a marker of state-approved identity rather than administrative efficiency). The measurement series show this risk is real: theater rises from 0.12 to 0.30, and extraction plateaus at high levels (0.58–0.59) despite literacy gains already saturating. This suggests the constraint's function may be shifting from coordination to control. A mandatrophy verdict would be premature at the current interval (t=40), but the trajectory warrants an omega variable addressing the long-term function drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_devaluation_vs_genuine_coordination,
    'Is the devaluation of Arabic literacy genuine coordination cost (the new script is objectively more efficient), or does the reform extract elite authority under cover of efficiency metrics?',
    'Comparative study: measure literacy gains and administrative efficiency under the new script against a counterfactual where classical and new scripts coexist (if such a jurisdiction exists). If gains are equivalent under polyglotism, the extraction is not coordination cost. If gains require exclusion of the classical form, the extraction is necessary coordination cost.',
    'If devaluation is genuine coordination cost, the constraint remains rope (moderate asymmetry justified by real function). If devaluation is extractive cover, the constraint is snare (classical script could coexist; its suppression is pure rent-collection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_devaluation_vs_genuine_coordination, empirical, 'Whether elite devaluation is coordination cost or elite capture.').

omega_variable(
    theater_ratio_drift_and_function_shift,
    'Does the rising theater ratio (0.12 to 0.30 over the interval) indicate the constraint''s function is shifting from coordination (efficient administration) to control (state identity enforcement)?',
    'Temporal analysis of enforcement focus: track what proportion of suppression activity is devoted to blocking rival orthographies (coordination function) vs. suppressing classical-text access and alternative pedagogies (control function). If control proportion rises above 0.5 by t=35, function shift is underway.',
    'Function shift would reclassify the constraint from rope (coordination with asymmetry) toward tangled_rope (coordination + control) or snare (control masquerading as coordination). It would trigger mandatrophy detection: a founding problem (efficiency coordination) that becomes obsolete as the real function shifts to identity enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_drift_and_function_shift, empirical, 'Whether the constraint''s coordination function is atrophying and control function is ascending.').

omega_variable(
    reading_boundary_ambiguity,
    'Is the instrumentalist reading distinct from the modernist reading, or does the instrumentalist justification (efficiency, literacy statistics) collapse into modernist legitimacy (Western alignment, rupture from Ottoman past) in practice?',
    'Textual analysis of reform justifications: does the state''s official narrative emphasize efficiency and measurable literacy gains (instrumentalist) or emphasize alignment with Western orthographic standards and rupture from Islamic tradition (modernist)? If modernist framing dominates, the two readings are conflating in policy reality.',
    'If readings conflate in practice, the instrumentalist constraint story underestimates the constraint''s extraction (modernist reading carries higher ε from the explicit rupture narrative). If readings remain distinct, the ε value (0.58) is accurate for the instrumentalist reading in isolation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the instrumentalist and modernist readings are empirically separable or conflated in state practice.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) structural (state blocking rival scripts, limiting classical education) or internalized (newly literate cohorts actively reject classical literacy, assimilate rejection as self-understanding)?',
    'Post-reform intergenerational study: after state enforcement relaxes (or in jurisdictions where state suppression was lighter), do newly literate cohorts spontaneously maintain the reformed orthography, or do they seek classical literacy access? If they maintain the new script spontaneously, suppression has become internalized identity; if they seek classical access, suppression was primarily structural.',
    'If suppression is internalized, effective suppression is higher than the 0.62 measurement suggests—the constraint carries psychological lock-in. If structural, the 0.62 reflects active state enforcement and would decline if suppression apparatus weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of classical literacy is structural or internalized.').

omega_variable(
    kernel_reading_contest_status,
    'Are the three sibling readings (instrumentalist, continuity, modernist) genuinely live positions held by different parties, or does one reading dominate state policy, pushing others into purely oppositional stances?',
    'Survey of state policy-makers, traditional clergy, and international advisors: what legitimacy basis does each group explicitly endorse? If all three readings find adherents within different institutional seats, the contest is live. If one reading dominates and others are pure opposition, the contest has collapsed into binary.',
    'If the contest is live (coexists_with relations are accurate), the three constraints are modeling a genuine three-way dispute. If the contest has collapsed, one reading has foreclosed or suppressed the others, and the network topology (affects_constraints relationships) needs revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, conceptual, 'Whether the orthographic legitimacy kernel remains actively contested across readings or has collapsed into binary opposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(orth_tr_t15, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(orth_tr_t25, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(orth_tr_t30, projected).
narrative_ontology:measurement(orth_tr_t35, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement_basis(orth_tr_t35, projected).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(orth_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(orth_be_t15, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(orth_be_t25, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(orth_be_t30, projected).
narrative_ontology:measurement(orth_be_t35, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement_basis(orth_be_t35, projected).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(orth_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(orth_su_t15, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(orth_su_t25, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(orth_su_t30, projected).
narrative_ontology:measurement(orth_su_t35, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(orth_su_t35, projected).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(orth_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic_legitimacy_kernel is decomposed into three structurally distinct constraint stories, one per reading: instrumentalist_reading (this story, ε=0.58, rope-type, coordination with asymmetric extraction), continuity_reading (ε≈0.35–0.45, rope-type or piton, symmetric negotiation over access), and modernist_reading (ε≈0.65–0.75, snare-type, explicit rupture narrative masking elite capture). The readings coexist as live positions held by different institutional seats (state administrators favor instrumentalist, traditional clergy favor continuity, Western-aligned modernizers favor modernist). Each reading instantiates a different constraint because their ε values differ substantially and their beneficiary/victim maps are structurally distinct. The kernel contest is the three-way dispute over what grounds orthographic legitimacy; the three constraints model the structural reality of that dispute. All three stories link bidirectionally via affects_constraints to enable contamination analysis: if one reading's legitimacy erodes, the others become more salient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
