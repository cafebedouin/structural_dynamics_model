% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex-Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This is the INTERSEX-ACCOMMODATION READING of the contested kernel
 *   'woman_category.' The reading instantiates a biological-spectrum
 *   understanding: the 'woman' category includes typical female reproductive
 *   anatomy and secondary sex characteristics, plus intersex variations that
 *   are closer to female-typical than male-typical on any axis (reproductive,
 *   hormonal, chromosomal, anatomical). This reading does NOT depend on
 *   gender identity—it is grounded in biological acknowledgment. The core
 *   innovation is that it makes 'biological spectrum' the ground of the
 *   category, not an exception or special case. The core structural problem
 *   is that this reading benefits intersex individuals by including them
 *   categorically, while simultaneously exposing them to boundary-drawing by
 *   medical and institutional authorities who must determine which intersex
 *   conditions count as 'female-typical.' The claim is TANGLED ROPE: genuine
 *   coordination function (acknowledging biological variation, reducing
 *   medicalization pressure) paired with asymmetric institutional authority
 *   (medical boards and sports bodies gain power to classify intersex
 *   individuals and may enforce performance restrictions in sports). The
 *   metrics reflect the contested status and domain-variable extractiveness:
 *   low extractiveness in most legal/medical domains (inclusion is
 *   beneficial, harm is primarily from contested classification), but high
 *   extractiveness in elite sports (where performance-advantage boundaries
 *   mean some intersex athletes face hormone suppression mandates or
 *   exclusion despite categorical inclusion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.62).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.71).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex-Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '25c9620f-7785-4e4b-b0f0-9e73b97afb5a').
narrative_ontology:cs_kernel_codification('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', distributed).
narrative_ontology:cs_authority_grounding('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', distributed).
narrative_ontology:cs_reading_relation('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', foundational, biological_sex_spectrum_exists).
narrative_ontology:cs_axiom_status(biological_sex_spectrum_exists, holdable).
narrative_ontology:cs_axiom_grounding('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', biological_sex_spectrum_exists, empirically_contingent).
narrative_ontology:cs_axiom('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', secondary, spectrum_inclusion_reduces_medicalization).
narrative_ontology:cs_axiom_status(spectrum_inclusion_reduces_medicalization, holdable).
narrative_ontology:cs_axiom_grounding('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', spectrum_inclusion_reduces_medicalization, instrumental).
narrative_ontology:cs_reference_frame('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', pre_public_intersex_visibility).
narrative_ontology:cs_drift_state('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', contemporary_global_intersex_advocacy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25c9620f-7785-4e4b-b0f0-9e73b97afb5a', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals_with_female_typical_biology).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, advocacy_organizations_intersex_rights).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_athletes_elite_sports).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_individuals_gender_category_boundary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_individuals_with_female_typical_biology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals born with ovaries, uterus, or partial female reproductive anatomy but with androgen insensitivity, partial virilization, or other conditions that do not produce typical male anatomy. Under this reading they are included in the 'woman' category as a matter of biological acknowledgment, not a matter of choice or identity claim. They benefit from legal, social, and institutional recognition without the requirement to make a gender-identity argument. Their embodied situation is their evidence. But this inclusion comes with the cost of living in a category definition contested by two other readings, and of bearing the epistemic labor of justifying the boundary.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals_with_female_typical_biology, beneficiary,
    powerless, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, intersex_individuals_with_female_typical_biology, payer).

% Athletes with intersex conditions (e.g., androgen-insensitive XY, 46,XX with elevated androgen production) who compete in women's categories and face testing, hormone suppression mandates, or exclusion based on sex-differentiation protocols. Under this reading they are categorically included as 'women,' but sports governing bodies impose performance-based restrictions (testosterone caps, hormone therapy requirements, surgical intervention mandates) that do not apply equally to sex-typical athletes. The reading's categorization acknowledges them as women while enforcement mechanisms simultaneously restrict their participation or require bodily modification.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_athletes_elite_sports, payer,
    organized, biographical, constrained, global).

% Individuals whose intersex condition is ambiguous even under the biological-spectrum reading—those whose reproductive anatomy, hormone profile, or gonad structure does not clearly fit either typical female or typical male category. Under this reading they are included in 'woman' only if their condition is read as closer to female-typical than male-typical; those whose condition is centered rather than distributed toward either pole bear the cost of boundary ambiguity. They are asked to accept categorization that acknowledges them while still requiring institutional adjudication of where they belong.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals_gender_category_boundary, payer,
    powerless, biographical, identity_locked, universal).

% Advocates, institutions, and policymakers (courts, sports bodies, medical regulatory boards) who hold that the 'woman' category should be defined by chromosomal/anatomical/reproductive biology in its typical form. They are excluded from the framing that would include intersex variations within the category. If present in the conversation, they would argue that acknowledging biological spectrum requires abandoning the category definition altogether, not expanding it.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_reading_advocates, excluded,
    institutional, generational, trapped, universal).

% Advocates and institutions who hold that gender identity is the proper criterion for category membership. They are excluded from a reading that makes biological spectrum (rather than identity) the basis. If present, they would argue that a biological reading—even spectrum-inclusive—does not address the core question of gender-based recognition and may reinscribe medicalization of identity.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_reading_advocates, excluded,
    institutional, generational, trapped, universal).

% International sports federations and national Olympic committees that implement sex-category enforcement in elite athletics. Under this reading they are tasked with including intersex athletes within 'woman' while simultaneously enforcing performance boundaries to maintain competitive fairness. This reading exposes the contradiction: acknowledging intersex individuals as women while restricting their participation or requiring medical intervention creates a class of women whose membership is conditional on bodily modification.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Medical boards, intersex medical associations, and clinicians who establish diagnostic criteria and treatment protocols for intersex conditions. Under this reading they must navigate the biological spectrum claim: they author and maintain the classifications that determine whether an individual's condition is 'female-typical variation' or 'ambiguous.' Their technical definitions become the boundary-drawing mechanism that determines category membership and enforcement scope.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_institutions, agenda_setter,
    institutional, generational, constrained, universal).

% Intersex-rights organizations and medical ethicists advocating for spectrum-inclusive definition of sex categories and against non-consensual medical intervention on intersex children. They benefit from a reading that acknowledges biological spectrum because it validates their core claim: that intersex variation is normal and does not require erasure or normalization. But they also face the cost of living with a reading whose enforcement (in sports, law, medicine) is contested by two other readings.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, advocacy_organizations_intersex_rights, beneficiary,
    organized, generational, mobile, global).

% Legislatures, courts, and regulatory bodies that must translate the category definition into legal rules (tax, benefit eligibility, anti-discrimination protection, civil status). They adjudicate cases (Caster Semenya's 2019 Court of Arbitration for Sport case; German Bundestag's 2018 third-gender option; recent trans and intersex recognition legislation in Argentina, Iceland, and others) and must decide which reading of the kernel they instantiate in law. This reading's presence in that decision space creates pressure on enforcement of the other readings.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, policymakers_legal_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, sports_governing_bodies).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Acknowledges the biological reality that reproductive anatomy, hormone production, gonadal structure, and chromosomal organization do not always cluster in the way binary sex categories assume. The coordination problem is: How should legal, medical, and social institutions classify and make decisions about individuals whose biology does not fit into binary categories, without requiring erasure, medical normalization, or categorical exclusion? This reading proposes that including intersex variation within 'woman' solves the erasure problem—it acknowledges intersex individuals as women without requiring identity-based claims—while exposing the boundary-drawing work that falls to institutions.
% TRANSFER_FUNCTION: The arrangement transfers medical authority and institutional discretion to medical boards and sports governing bodies: they gain the power to draw the spectrum boundary (to decide which intersex variations count as 'female-typical' and which do not). It transfers the burden of justification from intersex individuals (who no longer need to claim gender identity to be recognized as women) to the institutions maintaining the boundary. It also transfers the cost of living with contested categorization to intersex individuals themselves—their category membership is now conditional on medical classification and institutional acceptance.
% ABSENT_VOICES: Individuals with intersex conditions who do not identify with either the 'woman' category or a gender-identity framework and who instead call for decategorization or a third category are structurally excluded from this reading (the reading assumes the person will accept 'woman' as their category). Sex-biology-reading advocates are excluded from the framing of the question itself (they are not asked, 'Should the spectrum be acknowledged?'—that is already decided; they are only asked how broad it should be). Gender-identity advocates are excluded from a category system grounded in biology rather than identity.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—if institutions reverted to binary sex classification—legal and medical systems would have to reclassify intersex individuals who are now recognized under this reading into either 'woman' (if they are closer to female-typical) or 'man' (if they are closer to male-typical or ambiguous), or exclude them from the category altogether. Sports bodies would have to re-establish testosterone thresholds or other performance-based criteria to manage the competitive inclusion of boundary cases. Medical institutions would lose the conceptual framework that acknowledges intersex variation as normal. The world would reorganize around binary classification again, though the pressure from individuals with intersex conditions would likely resurface demands for a spectrum reading.
% FOUNDING_PROBLEM: Intersex individuals have always existed but have been systematically misclassified, medicalized, subjected to non-consensual surgical normalization, and denied legal recognition under either category. The founding problem is twofold: (1) medical erasure—the treatment of intersex conditions as disorders requiring surgical correction in infants and children; (2) categorical erasure—the requirement that intersex individuals either fit into one of two binary categories or be excluded from legal and social recognition. The spectrum reading was built to solve both problems simultaneously by making biological spectrum the ground of the category itself, not an exception to be managed.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists and intersex rights organizations (Intersex Society of North America, Organisation Intersex International, United Nations Office of the High Commissioner for Human Rights) attest that non-consensual medical normalization of intersex children is a human rights violation and that spectrum-inclusive categories reduce medicalization pressure. Caster Semenya's case and subsequent CAS ruling (2019) confirm that the categorization problem is live and globally unresolved. However, sex-biology advocates argue that the founding problem is over-stated (intersex conditions are rare) and that the real problem is non-consensual surgery, which can be solved without changing the category definition. Gender-identity advocates argue that the founding problem is not about biology at all but about institutional refusal to honor self-identification. The founding problem is not resolved—it is relocated by the choice of reading.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at the interval end (2026), down from 0.88 in 1970, reflecting the visibility shift from absolute medicalization and erasure (high extraction, high suppression) toward partial institutional recognition (moderate extraction, spectrum-boundary complexity). The temporal trend is driven by: (1) visibility of intersex conditions in medical and advocacy literature after ~1990; (2) high-profile cases (Caster Semenya, 2009 onwards) forcing sports governing bodies to adjudicate spectrum boundaries; (3) legal recognition shifts in some jurisdictions after ~2015 (Iceland, Argentina, Germany). The baseline extractiveness remains moderate (0.62) because even where legal recognition exists, the boundary-drawing work is imposed on intersex individuals and institutions jointly. Suppression requirement declines (0.92 → 0.71) because active medical normalization of intersex children faces increasing resistance from advocacy and ethics frameworks; but suppression does not vanish because sports bodies and some medical institutions still enforce testosterone thresholds and performance restrictions. Theater ratio is stable and modest (0.15–0.28) because the coordination function is real (the reading does reduce medicalization of infants, does provide legal recognition) but the enforcement work (boundary classification in sports and medicine) creates visible administrative overhead rather than pure theatrical performance. Accessibility collapse is moderate (0.58): intersex individuals have genuine exit options (legal challenge, advocacy organization support, media visibility of Semenya case) that did not exist in 1970, so alternatives to absolute binary classification are increasingly accessible. Resistance is high (0.74) because intersex individuals, advocacy organizations, and medical ethicists actively push back on both binary enforcement and on the reading's own boundary-drawing requirements.
 *
 * PERSPECTIVAL GAP:
 *   The spectrum reading generates structural perspectival divergence: (1) From the seat of intersex advocacy organizations and intersex individuals benefiting from categorical inclusion, this reading is coordination—it solves the erasure problem and reduces medicalization pressure. (2) From the seat of sports governing bodies, this reading is enforced specification: they must implement the spectrum boundary and often impose performance restrictions to maintain competitive categories. (3) From the seat of sex-biology advocates (excluded), this reading solves no problem; it dissolves the category. (4) From the seat of gender-identity advocates (excluded), this reading misses the point by grounding recognition in biology rather than identity. The engine will compute these seats differently: beneficiary seats (intersex individuals with female-typical biology, advocacy organizations) will experience d near the beneficiary end; payer seats in elite sports will experience d near the target end (constrained exit, medically imposed restrictions); advocacy/identity seats will show high directionality variance because the reading's institutional deployment (sports restrictions) diverges from the advocacy framing (protection from non-consensual medicalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d ≈ 0.1–0.25): Intersex individuals with female-typical biology and advocacy organizations benefit from spectrum inclusion without requiring identity claims. They are identity-locked (the category inclusion is about their embodiment, not their choice) but have arbitrage options (legal challenge, activism, cross-border relocation). Power is low to organized (individual intersex people are powerless; advocacy organizations are organized). Effective extraction is damped by beneficiary position and arbitrage exit. Payer directionality (d ≈ 0.65–0.85): Intersex athletes in elite sports face hormone suppression mandates, testing requirements, or exclusion despite categorical inclusion. Their exit is constrained (elite athletics is their professional identity and means of livelihood); their power is organized (athletes' unions, advocacy support) but institutional power (sports bodies) is greater. Effective extraction is amplified by target position and identity-locked time horizon (athletic career is short, identity-fused). Medical institutions and sports bodies (agenda-setters) sit near d ≈ 0.5 (they benefit from authority to classify, but carry the administrative and political cost of boundary-drawing; their exit is constrained by institutional mandate). Excluded seats (sex-biology advocates, gender-identity advocates) do not map to beneficiary/victim structure—they are excluded from the framing itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy misclassification in the following way: (1) The founding problem is not dead—erasure of intersex individuals is still live in many jurisdictions and in medical practice. (2) The founding problem is also contested—sex-biology and gender-identity advocates argue the founding problem is either over-stated or differently framed. (3) The arrangement is not a zombie (persisting purely through inertia)—legal recognition of intersex categories, institutional amendment of sports rules (CAS Semenya ruling, subsequent testosterone protocols), and medical ethics reorientation toward non-normalization represent active institutional response to the problem the reading identifies. However, the reading does instantiate a tangled-rope structure that could become extractive if the institutional response hardens around boundary-enforcement (particularly in sports) and loses sight of the anti-normalization rationale. The theater ratio remaining stable at ~0.28 (not rising toward 0.5+) suggests the coordination function is still visible relative to administrative overhead; if theater ratio were to rise above 0.4, the reading would signal a drift toward piton status (institutional classification machinery persisting for its own sake, losing the medical-ethics rationale that gave it legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectrum_boundary_ambiguity,
    'Where does the spectrum boundary between ''female-typical'' and ''ambiguous'' actually lie, and who has the authority to draw it?',
    'Comparative analysis of institutional boundary-drawing across sports bodies, medical boards, and legal systems; documentation of which intersex conditions are classified as ''female-typical'' vs. ''ambiguous'' in each jurisdiction and domain.',
    'If the boundary is stable and consensual (intersex individuals and institutions agree on the classification line), the reading functions as coordination. If the boundary is contested or imposed (institutions classify in ways that intersex individuals dispute), the reading instantiates asymmetric authority and higher extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_boundary_ambiguity, empirical, 'The institutional boundary between female-typical and ambiguous intersex variation is not pregiven—it is authored by medical boards and sports governing bodies. Its stability and legitimacy are unresolved.').

omega_variable(
    sports_performance_advantage_vs_inclusion,
    'Is the requirement for hormone suppression in intersex athletes a legitimate competitive-fairness measure, or a disguised exclusionary mechanism that contradicts the spectrum reading''s inclusion premise?',
    'Systematic evidence review on the correlation between intersex conditions and athletic performance; cross-jurisdictional analysis of whether performance restrictions are applied uniformly across all women athletes with the same hormone profiles, or selectively to intersex athletes; longitudinal outcome data from athletes subjected to hormone suppression vs. unsuppressed athletes.',
    'If suppression requirements are scientifically justified and applied uniformly, the constraint remains tangled-rope (mixed coordination and restriction). If suppression is selective and not evidence-based, it reclassifies to snare (pure exclusionary extraction riding on inclusion rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_performance_advantage_vs_inclusion, empirical, 'Whether sports-domain restrictions on intersex athletes are justified restrictions of a coordinated category or exclusionary mechanisms disguised as fairness enforcement.').

omega_variable(
    medical_normalization_pressure_persistence,
    'To what extent does the spectrum reading actually reduce non-consensual medical normalization of intersex children, given that medical institutions still classify intersex variation and impose treatment protocols?',
    'Longitudinal documentation of non-consensual surgical and hormonal normalization rates in jurisdictions with spectrum-inclusive category recognition vs. binary-only recognition; interviews with parents, physicians, and intersex-rights advocates on whether institutional acknowledgment of spectrum reduces pressure to ''correct'' intersex anatomy.',
    'If spectrum recognition demonstrably reduces non-consensual normalization, the founding-problem coordination function is validated. If normalization persists at comparable rates, the reading has solved the erasure problem but not the medicalization problem—the founding problem is only partially addressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medical_normalization_pressure_persistence, empirical, 'Whether acknowledging biological spectrum as the ground of category definition produces real change in medical practice, or whether medicalization persists within a reconstituted categorization framework.').

omega_variable(
    gender_identity_reading_foreclosure,
    'Does this reading (biological spectrum) foreclose the gender-identity reading, or do they coexist as two different ways of grounding category membership?',
    'Logical analysis: can an institutional system simultaneously instantiate both readings (some individuals recognized as ''woman'' via gender identity, others via biological spectrum)? Empirical observation in jurisdictions that have adopted both readings (Canada, Spain, Argentina) of whether they are treated as compatible paths to the same category or as competing definitions.',
    'If they coexist, the constraint is correctly classified as coexisting with gender-identity reading. If this reading''s institutionalization actively forecloses identity-based category membership (e.g., sports bodies reject identity-based waivers for intersex athletes), the relation is foreclosure rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_identity_reading_foreclosure, conceptual, 'Whether the intersex-accommodation reading and the gender-identity reading are logically compatible within a single legal/institutional framework, or whether the choice of one forecloses the other.').

omega_variable(
    collective_vs_individual_intersex_identity,
    'To what extent do intersex individuals experience this reading''s spectrum-inclusive category as genuinely accommodating vs. as a form of biological essentialism that medicalizes their difference under the guise of inclusion?',
    'Qualitative research and testimony from intersex individuals in jurisdictions where this reading has been institutionalized; documentation of whether spectrum inclusion is experienced as liberation from erasure or as reinscription of medical authority.',
    'If intersex individuals experience the spectrum reading as accommodating, the beneficiary-seat classification and coordination function assessment are validated. If the reading is experienced as a more subtle form of exclusion (medicalized inclusion vs. authentic recognition), the classification should account for internalized suppression and identity-lock dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_vs_individual_intersex_identity, preference, 'Whether the lived experience of spectrum-inclusive categorization aligns with the reading''s accommodation premise, or whether inclusion without identity-based recognition is itself a form of erasure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1970, woman_category__intersex_accommodation_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(woma_tr_t1990, woman_category__intersex_accommodation_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(woma_tr_t2005, woman_category__intersex_accommodation_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(woma_tr_t2015, woman_category__intersex_accommodation_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(woma_tr_t2020, woman_category__intersex_accommodation_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(woma_tr_t2026, woman_category__intersex_accommodation_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t1970, woman_category__intersex_accommodation_reading, base_extractiveness, 1970, 0.88).
narrative_ontology:measurement(woma_be_t1990, woman_category__intersex_accommodation_reading, base_extractiveness, 1990, 0.81).
narrative_ontology:measurement(woma_be_t2005, woman_category__intersex_accommodation_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(woma_be_t2015, woman_category__intersex_accommodation_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(woma_be_t2020, woman_category__intersex_accommodation_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(woma_be_t2026, woman_category__intersex_accommodation_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1970, woman_category__intersex_accommodation_reading, suppression_requirement, 1970, 0.92).
narrative_ontology:measurement(woma_su_t1990, woman_category__intersex_accommodation_reading, suppression_requirement, 1990, 0.87).
narrative_ontology:measurement(woma_su_t2005, woman_category__intersex_accommodation_reading, suppression_requirement, 2005, 0.79).
narrative_ontology:measurement(woma_su_t2015, woman_category__intersex_accommodation_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(woma_su_t2020, woman_category__intersex_accommodation_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(woma_su_t2026, woman_category__intersex_accommodation_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, sports_testosterone_threshold_enforcement).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, medical_intersex_normalization_protocol).

% DUAL FORMULATION NOTE:
% The 'woman_category' kernel decomposes into three constraint stories, each instantiating a different reading: (1) sex_biology_reading—category grounded in chromosomal/reproductive biology; (2) gender_identity_reading—category grounded in internal identity; (3) intersex_accommodation_reading (this story)—category grounded in biological spectrum. These are not three perspectives on a single constraint. They are three distinct constraints that contest the same institutional category. The ε values diverge substantially across readings: the biology reading has low ε in most jurisdictions (it is the existing default, with minimal extraction), the identity reading has moderate-to-high ε (contested, undermines binary enforcement, creates institutional friction), and the spectrum reading has moderate ε (partly beneficial to intersex individuals, partly extractive through boundary-drawing). The spectrum reading influences both siblings by making biologization visible (it forecloses some pure-identity framings by insisting on bodily acknowledgment) and by creating political pressure for legal recognition that the identity reading can leverage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
