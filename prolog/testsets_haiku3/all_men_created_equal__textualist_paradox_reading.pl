% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: All Men Created Equal: Textualist Paradox Reading
 *   domain: constitutional/political philosophy
 *
 * SUMMARY:
 *   The Declaration of Independence asserts universal equality—'all men are
 *   created equal'—in language with no scope restriction. Yet the founding
 *   generation restricted the claim's application to propertied white men,
 *   excluding women, enslaved people, and Native Americans. The textualist
 *   paradox reading identifies this as a performative contradiction: the text
 *   says universal, the practice says restricted. This reading does not
 *   author a universalist alternative (that is the universalist_reading
 *   constraint); it exposes structural incoherence in the originalist
 *   reading's authority claim. The paradox shows that originalist fidelity to
 *   the founders' intent cannot coherently honor both the text (universal)
 *   and the practice (restricted) simultaneously—a reading must choose. The
 *   textualist reading extracts legitimacy by weaponizing this incoherence
 *   against originalism. It benefits textualist and delegitimization-focused
 *   interpreters; it victimizes the originalist authority structure.
 *
 * KEY AGENTS:
 *   - originalist_interpretive_authority: Defends founder-intent readings; claims coherence between universal language and restricted application via unstated scope qualifiers
 *   - textualist_interpretive_community: Exposes the performative contradiction; argues that universal language cannot coexist with restricted application without explicit scope limitation
 *   - excluded_categories: Enslaved people, women, Native Americans at the founding; contemporary classes whose inclusion the constraint's instability creates pressure toward
 *   - universalist_reading (sibling constraint): Offers a positive interpretation that resolves the paradox by expanding scope iteratively
 *   - originalist_reading (sibling constraint): Defends the coherence of universal language + restricted application via implicit founding-era qualifiers
 *   - constitutional_courts: Apply the constraint and must adjudicate which reading governs interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.68).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.41).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "All Men Created Equal: Textualist Paradox Reading").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional/political philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '24ac81d5-7695-4fe3-8929-9838fce3d043').
narrative_ontology:cs_kernel_codification('24ac81d5-7695-4fe3-8929-9838fce3d043', fixed_text).
narrative_ontology:cs_authority_grounding('24ac81d5-7695-4fe3-8929-9838fce3d043', lineage).
narrative_ontology:cs_interpretation_layer_present('24ac81d5-7695-4fe3-8929-9838fce3d043').
narrative_ontology:cs_reading_relation('24ac81d5-7695-4fe3-8929-9838fce3d043', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('24ac81d5-7695-4fe3-8929-9838fce3d043', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('24ac81d5-7695-4fe3-8929-9838fce3d043', foundational, universal_language_logically_constrains_scope).
narrative_ontology:cs_axiom_status(universal_language_logically_constrains_scope, holdable).
narrative_ontology:cs_axiom_grounding('24ac81d5-7695-4fe3-8929-9838fce3d043', universal_language_logically_constrains_scope, empirically_contingent).
narrative_ontology:cs_axiom('24ac81d5-7695-4fe3-8929-9838fce3d043', foundational, performative_contradiction_delegitimizes_originalism).
narrative_ontology:cs_axiom_status(performative_contradiction_delegitimizes_originalism, holdable).
narrative_ontology:cs_axiom_grounding('24ac81d5-7695-4fe3-8929-9838fce3d043', performative_contradiction_delegitimizes_originalism, instrumental).
narrative_ontology:cs_reference_frame('24ac81d5-7695-4fe3-8929-9838fce3d043', originalist_textual_fidelity_framework).
narrative_ontology:cs_drift_state('24ac81d5-7695-4fe3-8929-9838fce3d043', contemporary_legal_academia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('24ac81d5-7695-4fe3-8929-9838fce3d043', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, excluded_categories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, excluded_categories).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, progressive_legal_academia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defends the coherence of the founding text by arguing that universal language carried implicit scope restrictions understood by the founding era. Sets the interpretive framework by which courts read the Declaration and Constitution. Faces the textualist challenge that this reading requires explicit scope restrictions that do not appear in the text—a requirement that undermines the claimed method (reading founders' intent vs. amending the text). Constrained exit: cannot abandon originalism without institutional collapse; cannot coherently add explicit scope restrictions without contradicting the claimed fidelity-to-text method.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority, payer).

% Gains analytical authority and institutional influence by exposing the performative contradiction. The reading supplies ammunition against originalist authority claims and justifies textualist hermeneutics as more rigorous (the text says what it says; scope restrictions must be explicit, not implicit). Can exit by abandoning the textualist critique, but doing so forfeits the authority gain the paradox provides. Mobile exit: can adapt the textualist method to other domains; the paradox exposure is not the only textualist argument, only the most powerful.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community, beneficiary,
    institutional, generational, mobile, national).

% Enslaved people, women, Native Americans at the founding; contemporary classes (immigrants, non-citizens) whose inclusion the constraint's exposure creates pressure toward. The textualist reading does not directly include them (does not expand scope itself—that is the universalist_reading's function), but it weakens the originalist argument that scope was legitimately restricted at the founding. Trapped exit: their status depends on how the contradiction is resolved; they cannot exit the constraint's domain.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_categories, beneficiary,
    powerless, generational, trapped, national).

% The universalist reading (sibling constraint) benefits structurally from the textualist paradox exposition: the paradox makes originalism untenable and creates space for universalist scope expansion. The textualist reading is not the universalist reading, but it supports universalism by delegitimizing the main alternative to it (originalism). Listed as non-agent: a reading, not an actor collecting or paying.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_interpretive_reading, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, universalist_interpretive_reading, observer).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, universalist_interpretive_reading).

% Apply these readings when adjudicating equality cases. Must choose between originalist coherence (add implicit scope restrictions) and textualist literalism (acknowledge universal language and expand scope). The textualist paradox reading constrains their options: they cannot maintain both fidelity-to-founders and literal text-reading without explicit amendment. Constrained exit: cannot avoid the contradiction; only can manage which horn they grasp.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% The textualist paradox reading is deployed by progressive legal scholars to delegitimize originalism in courts and law schools. The reading provides analytical credibility for progressive constitutional positions by showing originalism is incoherent. Can exit by adopting originalism or abandoning textualism; exit is institutionally costly (credibility loss) but structurally available.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, progressive_legal_academia, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The textualist paradox reading coordinates no positive constitutional arrangement. Instead, it exposes structural incoherence in the originalist reading's authority claim—the claim that universal founding language can coherently govern a restricted application without explicit scope restriction. The reading's function is diagnostic, not coordinative: it shows that originalist doctrine requires unstated qualifications to survive.
% TRANSFER_FUNCTION: The reading transfers legitimacy from originalist authority to textualist and progressive interpretive communities. It moves institutional authority and rhetorical power by showing that originalism's foundational coherence is compromised—a transfer from one interpretive method to another. It also creates pressure to expand equality scope (transfers toward excluded_categories) by delegitimizing the originalist defense of founding-era scope restrictions.
% ABSENT_VOICES: Conservative originalists who would defend the coherence of the founding text+practice combination are formally present as the target of the reading, but their own ontology of founding-era scope restrictions is treated as indefensible—their voice is structurally subordinated. Originalists would argue that the reading anachronistically applies post-founding standards of consistency to founding-era rhetoric. The reading excludes that defense by treating universal language as constraining regardless of era.
% DISAPPEARANCE_RATIONALE: If the textualist paradox reading disappeared (the contradiction-exposure went unnoticed), originalism would retain uncontested authority and could continue to justify founding-era scope restrictions as implicit qualifiers. Constitutional interpretation would rearrange: courts would apply originalism without the delegitimizing paradox; progressive and universalist readings would lose their most powerful argument against originalist scope restriction. The absence of the reading would not return the world to the founding era, but it would strengthen originalism's institutional position and weaken universalist constitutional pressure.
% FOUNDING_PROBLEM: How can a founding document claim universal equality while restricting that equality to a subset? The founding generation resolved this by using universal rhetoric while practicing restriction, leaving the tension unexamined. The textualist paradox reading makes the tension visible by insisting that universal language cannot coherently govern a restricted application without explicit scope limitation.
% FOUNDING_PROBLEM_CORROBORATION: The paradox is attested by constitutional scholars, historians, and legal analysts outside originalism (progressive and textualist academic camps). Historical record confirms the founding-era claim of universality alongside explicit founding-era restriction (slavery, women's exclusion). The originalist seat denies the paradox is live, arguing implicit scope was understood. Outside corroboration comes from historians documenting the founding-era awareness of the tension and deliberate compartmentalization; from legal scholars analyzing founding-era texts showing the universal language was chosen despite known restriction; from the historical fact that the restriction had to be later amended to change (Reconstruction amendments, women's suffrage), indicating the original text did not naturally authorize scope restriction.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the textualist reading's primary effect is delegitimizing originalist authority claims, not offering positive coordination or expanding equality scope itself. The reading provides an analytical service (exposing contradiction) that benefits those seeking to overturn originalism while harming the originalist authority structure. Suppression is modest (0.41) because the paradox cannot be suppressed once exposed—it is a visible logical fact, not a coercive fact. Once identified, the contradiction is accessibility-inescapable (0.72). Theater is moderate-to-high (0.52) because much of the reading's function is performative: it destabilizes originalism through contradiction-exposure rather than offering its own positive authority. The measurement series shows extractiveness and theater rising over the interval (0→30) as the textualist reading gains institutional visibility and influence, particularly in legal academia and progressive constitutional law. Suppression_requirement also rises modestly as originalism must work harder to maintain coherence against the exposed contradiction.
 *
 * PERSPECTIVAL GAP:
 *   An originalist judge reading this constraint sees it as a sophistic attack: the founders' use of universal language was common rhetoric of the era, and the unstated scope restrictions were understood implicitly. The originalist seat computes the constraint as snare—pure extraction hiding as linguistic analysis. A textualist or progressive constitutionalist reading the same constraint sees it as exposure of genuine incoherence: the text says universal and the founders' practice denies it, an actual contradiction requiring resolution. That seat computes the constraint as tangled_rope—it coordinates with non-originalist authority claims while extracting from originalism. The originalist seat experiences the reading as suppressible (can be dismissed as anachronistic) while textualist seats experience it as inescapable (the paradox is real). The engine computes these divergent perceptions from the structural data: the originalist seat, defending a challenged authority, experiences higher suppression_requirement; the textualist seat, deploying the challenge, experiences lower suppression_requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   The textualist_interpretive_community benefits from the reading's delegitimizing effect on originalism (low d, subsidy-like: the reading supplies ammunition against a rival interpretation). The originalist_interpretive_authority bears the cost of the contradiction-exposure (high d: the reading extracts legitimacy by exposing incoherence in their authority claim). Excluded_categories sit in a special position: they bear historical costs from the founding restriction, but the textualist reading does not directly remediate that (does not expand scope itself—that is the universalist_reading's function). Rather, the textualist reading creates structural pressure on originalism to either abandon fidelity to the founders' restricted application or explicitly author the scope restrictions in the text (neither option preserves the originalist authority claim intact). This makes excluded_categories beneficiaries of the reading's pressure (d asymmetrically favorable to them), even though the reading does not directly serve their interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was: how can a new nation claim to be founded on universal human equality while explicitly restricting that equality to a subset of people? The founding-era resolution was suppression: the universal language was authored for rhetorical power, the restriction was practiced silently. The textualist paradox reading identifies the mandate (founding authority grounded in universal equality) as decoupled from the application (equality restricted to property-holding white men). The reading argues that originalism requires choosing: either the text governs (universal) or the founders' intent-to-restrict governs (bounded), but not both coherently. This makes originalism's mandate obsolete for originalists: they cannot claim to honor both the founding text and the founding practice without explicitly authoring the scope restrictions—at which point they are not reading the text, but rewriting it. The reading thus exposes a mandatrophy of originalism itself: the authority claim (fidelity to founders + universal equality) cannot be sustained without explicit repairs that violate the claimed method (fidelity without amendment). The textualist reading does not resolve this; it only makes it visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualist_authority_grounding,
    'Does the textualist reading''s authority derive from linguistic analysis of the founding text, or from a prior commitment to delegitimizing originalist claims? Is the reading describing the contradiction, or using it to override authority?',
    'Trace the genealogy of the textualist critique: did it emerge as a descriptive linguistic observation, or as a reactive intervention to counter originalist authority claims? Examine the reading''s own founding moment and whether it predates or postdates the originalist authority claim it targets.',
    'If the reading is reactive rather than foundational, its authority is derivative (critiquing another reading) rather than self-grounding. This affects whether it can sustain its own legitimacy claim or collapses into pure negation. If foundational, the reading stands on its own linguistic analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_authority_grounding, conceptual, 'Whether textualist authority is foundational linguistic analysis or reactive delegitimization.').

omega_variable(
    paradox_as_extraction_vector,
    'Is the performative contradiction inherent to the founding text, or does the textualist reading CONSTRUCT the paradox by holding the text to standards the founding era did not apply to it (anachronistic fidelity)?',
    'Compare the textualist reading''s standard of consistency (what counts as a genuine contradiction) to the standards applied by the founding era itself. If the founding era tolerated unstated scope restrictions on universal language, the paradox is anachronistic; if the contradiction was recognized and suppressed, it is inherent.',
    'If anachronistic, the textualist reading extracts authority by imposing post-founding interpretive standards retroactively, making it a snare hiding as linguistic analysis. If inherent, the reading exposes genuine structural instability in the kernel and operates as coordination with the universalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paradox_as_extraction_vector, empirical, 'Whether the contradiction is structural or imposed by the reading''s interpretive method.').

omega_variable(
    suppression_mechanism_origin,
    'Is the suppression of the paradox (the founding era''s compartmentalization of universal language and restricted application) structural to the period, or performative—a deliberate rhetorical move to hide known contradiction?',
    'Historical evidence: founding-era writings on the universality claim vs. slavery/women''s exclusion. Did founders acknowledge the tension and argue it away, or genuinely not see it? Letters, debates, private records.',
    'Structural suppression means the founders could not see the paradox (linguistic/cognitive capacity limit). Performative suppression means they saw and deliberately hid it. The textualist reading''s extractiveness rating depends on this: if structural, the reading is analyzing latent contradiction; if performative, the reading is exposing deliberate deception, which has higher moral authority but also higher threat to originalist legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether founding-era suppression was structural or performative.').

omega_variable(
    kernel_reading_vs_kernel_critique,
    'Is the textualist reading a READING of the kernel (offering a coherent interpretation that could govern practice), or a CRITIQUE of the kernel (showing that no coherent interpretation is possible)? Can this reading actually rule cases, or does it only undermine other readings?',
    'Operational test: can a judge apply the textualist paradox reading to adjudicate a specific case about equality scope? Or does the reading only say ''the kernel is incoherent, consult another reading''? Examine actual case law where the reading has been invoked.',
    'If a true reading, it offers a coherent authority frame. If only a critique, it delegitimizes other readings without providing its own positive authority—making it extractive (undermining legitimacy of originalism) but unable to sustain coordination (no judicial doctrine). This would make it a snare, not a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_kernel_critique, empirical, 'Whether the textualist paradox reading functions as a positive interpretation or only as a negative critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(all__tr_t10, all_men_created_equal__textualist_paradox_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(all__tr_t20, all_men_created_equal__textualist_paradox_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__textualist_paradox_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(all__be_t10, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(all__be_t20, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(all__su_t10, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(all__su_t20, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 30, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% The all_men_created_equal kernel decomposes into three constraint stories with different ε values, different beneficiary/victim structures, and different types. The originalist_reading defends the coherence of universal language + implicit scope restriction (Mountain or Rope, negligible extraction). The universalist_reading offers a positive interpretation based on universal scope expansion (Rope, coordination with excluded categories). This textualist_paradox_reading exposes the performative contradiction between universal language and restricted application, delegitimizing originalism without offering positive interpretation (Tangled Rope, moderate extraction, victim is originalist authority). Each reading generates distinct structural data and classification; the three stories are linked via network.affects_constraints. The textualist reading influences both siblings by exposing the contradiction both must address—originalism must defend against the paradox, universalism gains support from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
