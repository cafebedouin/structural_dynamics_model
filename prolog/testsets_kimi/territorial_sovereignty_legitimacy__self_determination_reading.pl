% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy â Self-Determination Reading
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the self-determination reading of the
 *   territorial sovereignty legitimacy kernel. It holds that sovereignty over
 *   the territory derives from the modern principle of national
 *   self-determination applied to the Arab population that held demographic
 *   majority and continuous residence during the 19th-20th centuries. Under
 *   this reading, the 1947 partition and subsequent Israeli statehood are
 *   framed as unjust colonial impositions by external powers, and the right
 *   of return is treated as restoration of the status quo ante rather than
 *   demographic threat. The constraint coordinates anti-colonial legal and
 *   political solidarity while asymmetrically extracting legitimacy from
 *   Israeli and Zionist territorial claims. It is claimed as a scaffold
 *   toward justice but operates as a tangled rope: the coordination function
 *   is real (mobilizing international legal frameworks for a stateless
 *   population), yet the same structure enforces a totalizing
 *   delegitimization of the rival polity that exceeds pure coordination.
 *
 * KEY AGENTS:
 *   - palestinian_arab_population: Primary beneficiary (moderate/constrained) â receives sovereignty legitimacy and return rights
 *   - israeli_state: Primary target (institutional/constrained) â sovereignty delegitimized as colonial imposition
 *   - jewish_immigrant_settler_society: Secondary target (organized/identity_locked) â framed as illegitimate colonial presence
 *   - anti_colonial_solidarity_networks: Secondary beneficiary (organized/mobile) â derives political coherence from the framing
 *   - international_legal_institutions: Agenda setter (institutional/analytical) â administers the self-determination norm
 *   - independent_historians: Analytical observer (analytical/analytical) â evaluates empirical claims about demographic continuity and coloniality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.76).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy â Self-Determination Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09').
narrative_ontology:cs_kernel_codification('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', formalized).
narrative_ontology:cs_authority_grounding('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', lineage).
narrative_ontology:cs_interpretation_layer_present('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09').
narrative_ontology:cs_reading_relation('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', foundational, modern_demographic_self_determination).
narrative_ontology:cs_axiom_status(modern_demographic_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', modern_demographic_self_determination, conventional).
narrative_ontology:cs_axiom('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', foundational, colonial_imposition_voids_territorial_claims).
narrative_ontology:cs_axiom_status(colonial_imposition_voids_territorial_claims, holdable).
narrative_ontology:cs_axiom_grounding('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', colonial_imposition_voids_territorial_claims, deontological).
narrative_ontology:cs_reference_frame('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', decolonization_legal_order).
narrative_ontology:cs_drift_state('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', contemporary_post_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a78cca7-fc75-4f8b-a7c4-ed1e64a5aa09', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_solidarity_networks).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_immigrant_settler_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims sovereignty and return rights based on having been the demographic majority with continuous residence during the modern period. Their political identity and international legal standing are constituted through the self-determination frame; exit from the constraint would mean abandoning the territorial claim and refugee right of return.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population, beneficiary,
    moderate, generational, constrained, national).

% Its sovereignty claims are structurally delegitimized by this reading, framed as a colonial imposition lacking legitimate basis. Must continuously defend its existence against the self-determination framework in international legal and diplomatic forums.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, payer,
    institutional, generational, constrained, national).

% Framed as an illegitimate colonial presence whose modern immigration and settlement lack standing under the continuous-residence criterion. Their identity is fused with territorial residence and the Zionist project; exit from the constraint's logic would require abandoning the ideological and physical settlement framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_immigrant_settler_society, payer,
    organized, biographical, identity_locked, national).

% Derive political coherence, funding, and moral purpose from applying anti-colonial and self-determination frameworks to the territory. The reading provides principled clarity that sustains global campaigns, academic boycott movements, and diplomatic pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_solidarity_networks, beneficiary,
    organized, biographical, mobile, global).

% Administer and enforce the self-determination norm through UN resolutions, ICJ advisory opinions, and human rights treaty bodies. Their institutional authority is partly constituted by upholding decolonization principles, yet they remain analytically capable of adopting alternative frameworks.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Evaluate empirical claims about demographic continuity, the colonial character of the mandate and partition, and the modern-period cutoff. They are situated outside the beneficiary-victim polarity but their findings are frequently recruited by both sides.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, independent_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national claims and international anti-colonial solidarity by providing a unified legal-moral framework that aligns diverse state and non-state actors against partition and in favor of territorial restoration and refugee return.
% TRANSFER_FUNCTION: Moves sovereignty legitimacy, territorial entitlement, and international legal recognition from the Jewish immigrant-settler society to the Arab demographic majority population that held continuous residence during the modern period; also transfers diplomatic and advocacy resources toward the anti-colonial framing and away from the Israeli state.
% ABSENT_VOICES: Jewish diaspora communities holding covenant-based and continuous-presence territorial claims, and pragmatic two-state advocates who accept partition as a legitimate compromise, are structurally marginalized in forums operating under this reading; their objections are classified as colonial apologetics rather than legitimate counter-claims.
% DISAPPEARANCE_RATIONALE: If the self-determination reading vanished overnight, Palestinian territorial claims would lose their primary international legal framework, anti-colonial solidarity networks would lose their principled coherence, and Israeli state legitimacy would face a radically different normative environment; the international diplomatic landscape would rearrange around alternative legitimacy axes such as covenant continuity or existential necessity.
% FOUNDING_PROBLEM: The colonial partition of the Middle East by European powers during the late Ottoman and mandatory periods, and the consequent displacement of the Arab demographic majority from the territory.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial historians and the UN Special Committee on Palestine corroborate the colonial partition framing from outside the immediate Palestinian beneficiary circle; Israeli state institutions, Zionist historiography, and critical international relations scholars contest this characterization, arguing the problem was competing national movements rather than colonial imposition. Corroboration is thus partial and politically situated.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading reallocates sovereignty legitimacy entirely from one national population to another, framing the existing state as void ab initio. Suppression is substantial (0.76) because the reading must actively suppress counter-narratives of indigenous Jewish continuity, covenant-based claims, and the legitimacy of international partition. Theater ratio is moderate (0.48): UN resolutions, ICJ proceedings, and solidarity campaigns perform genuine coordination, yet a growing share of activity defends a rhetorical position that has not altered territorial control in decades. Accessibility collapse is very high (0.85): once the colonial frame is adopted, Zionist legitimacy claims collapse conceptually and alternatives such as two-state partition become difficult to articulate within the frame. Resistance is high (0.78) because the Israeli state and its allies actively contest the framing in every available forum.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian seat, the constraint is experienced as restorative justice and the only available legal framework for redressing displacement; from the Israeli seat, the same structure is experienced as an existential delegitimization mechanism that denies any territorial entitlement regardless of boundaries or history. The engine computes this divergence from the structural data: beneficiaries with constrained exit experience low effective extraction, while institutional victims with identity-locked populations experience high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian Arab population and anti-colonial solidarity networks are declared beneficiaries, pushing their directionality toward the subsidized end. The Israeli state and Jewish immigrant-settler society are declared victims, pushing directionality toward the target end. International legal institutions occupy a mediating position as agenda-setters with analytical exit options, yielding near-symmetric directionality. No overrides are necessary: the structural derivation chain produces accurate directionalities from beneficiary/victim declarations combined with exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the problem of colonial displacement and the lack of a juridical framework for Palestinian national restoration. That founding problem remains live for refugees and stateless populations. However, the constraint has accumulated extractive functions that may exceed its coordinating purpose: it treats any Israeli sovereignty as illegitimate, not merely the occupation of 1967, and frames partition per se as colonial imposition. If the coordination function (self-determination for Palestinians) is used to justify unlimited extraction (permanent denial of Israeli legitimacy irrespective of borders), the reading risks mandatrophyâits justification becomes the transition to justice, yet it increasingly operates as a steady-state delegitimization engine. The temporal measurements show monotonically rising extractiveness and theater ratio, suggesting accumulation rather than transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_determination_universality,
    'Is the self-determination principle applied here a universal legal norm, or a historically specific anti-colonial construct whose scope is limited to delegitimizing particular state formations?',
    'Comparative analysis of self-determination claims across post-colonial contexts to test consistency of application; examination of whether the principle is invoked symmetrically for all stateless demographic majorities or selectively against specific targets.',
    'If specific rather than universal, the constraint''s coordination function is cover for targeted extraction, pushing classification toward snare. If universal, the coordination function is genuine and the classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_universality, conceptual, 'Universality vs specificity of the self-determination principle').

omega_variable(
    demographic_cutoff_exclusion,
    'Does the continuous residence criterion''s restriction to the modern period function as a neutral temporal boundary, or as a post-hoc device to exclude Jewish communities with documented pre-modern residence?',
    'Historical demographic analysis of Jewish continuous residence in the territory; examination of whether the modern-period cutoff is justified by the legal sources the reading claims or is constructed to produce a specific beneficiary set.',
    'If the cutoff is constructed to exclude, the constraint''s victim structure is internally inconsistent and the extraction is more asymmetric than the narrative admits, amplifying effective extraction for the excluded population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_cutoff_exclusion, empirical, 'Neutrality of the modern-period demographic cutoff').

omega_variable(
    partition_permanence,
    'Does the self-determination reading permit any permanent territorial partition, or does it foreclose all two-state outcomes as inherently unjust colonial impositions?',
    'Analysis of official Palestinian political positions, PLO charter history, and the reading''s textual logic to determine whether ''restoration of status quo ante'' is a maximalist horizon or a negotiable position.',
    'If the reading forecloses all partition, its coordination function is narrower than presented and its extractive character is more absolute. If it permits negotiated partition, the coordination function is broader and the rope less tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_permanence, conceptual, 'Compatibility of the reading with permanent partition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t0, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t20, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t40, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t60, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t80, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 80, 0.46).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_tr_t100, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t0, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t20, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t40, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t60, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t80, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_be_t100, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t0, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t20, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t40, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t60, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t80, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__self_determination_reading_su_t100, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 100, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_sovereignty_legitimacy kernel, decomposed per the epsilon-invariance principle because the natural-language label conflates structurally distinct claims: divine covenant and continuous presence (covenant_continuity_reading), modern demographic self-determination (this reading), and existential survival necessity (existential_matrix_reading). Each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
