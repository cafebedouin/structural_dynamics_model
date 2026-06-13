% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership via Identity Self-Declaration
 *   domain: social_ontology/bioethics/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   gendered-category-membership kernel: category membership is grounded in
 *   subjective gender identity and self-declaration, not biological sex
 *   markers or sustained social performance. Under this reading, trans women
 *   are women by declaration; sex-segregated spaces become gender-segregated;
 *   cisgender women lose exclusive access; institutional authorities manage
 *   identity claims and enforce the reclassification. The constraint is
 *   claimed as tangled_rope (genuine coordination for trans inclusion +
 *   asymmetric extraction from enforcement and displacement of cisgender
 *   women) because the reading solves a real coordination problem
 *   (institutional recognition of gender) while creating extraction
 *   (gatekeeping costs, space reclassification friction, cis women positioned
 *   as perpetrators of exclusion if they resist). The kernel context section
 *   (below) documents this reading's sibling readings and their structural
 *   relationships.
 *
 * KEY AGENTS:
 *   - trans_women: powerless beneficiaries with identity-locked exit; gain institutional recognition and space access
 *   - cisgender_women: moderate-power payers with constrained exit; lose exclusive space access, experience identity costs from boundary renegotiation
 *   - institutional_authorities: institutional-power agenda-setters; define and enforce the reading's rules; bear administrative overhead
 *   - biological_sex_essentialists: excluded organized advocates; their framework is declared incoherent by the reading
 *   - social_role_theorists: excluded organized advocates; their performance-based reading is sidelined in favor of identity-based gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.48).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.52).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Identity Self-Declaration").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/bioethics/political_philosophy").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '26613378-1a7c-4b54-a0bc-55cd75937b22').
narrative_ontology:cs_kernel_codification('26613378-1a7c-4b54-a0bc-55cd75937b22', distributed).
narrative_ontology:cs_authority_grounding('26613378-1a7c-4b54-a0bc-55cd75937b22', extraction).
narrative_ontology:cs_reading_relation('26613378-1a7c-4b54-a0bc-55cd75937b22', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('26613378-1a7c-4b54-a0bc-55cd75937b22', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('26613378-1a7c-4b54-a0bc-55cd75937b22', foundational, gender_identity_self_declaration_legitimate).
narrative_ontology:cs_axiom_status(gender_identity_self_declaration_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('26613378-1a7c-4b54-a0bc-55cd75937b22', gender_identity_self_declaration_legitimate, deontological).
narrative_ontology:cs_axiom('26613378-1a7c-4b54-a0bc-55cd75937b22', foundational, biological_sex_not_determinative_of_gendered_category).
narrative_ontology:cs_axiom_status(biological_sex_not_determinative_of_gendered_category, holdable).
narrative_ontology:cs_axiom_grounding('26613378-1a7c-4b54-a0bc-55cd75937b22', biological_sex_not_determinative_of_gendered_category, deontological).
narrative_ontology:cs_reference_frame('26613378-1a7c-4b54-a0bc-55cd75937b22', identity_based_category_membership).
narrative_ontology:cs_drift_state('26613378-1a7c-4b54-a0bc-55cd75937b22', contemporary_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26613378-1a7c-4b54-a0bc-55cd75937b22', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, non_binary_people).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_men).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sex_segregated_space_operators).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_identity_is_legitimate_category_basis).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, self_declaration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and institutional access as women through self-identification. Benefit from reclassification of sex-segregated spaces as gender-segregated (bathrooms, shelters, prisons, sports). The identity-lock is structural: denying self-identification is experienced as fundamental erasure. Exit would mean institutional denial of declared gender, which carries psychological and social costs that make exit identity-incompatible rather than merely constrained.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    powerless, civilizational, identity_locked, global).

% Bear costs through renegotiated space-access rules (bathrooms, locker rooms, shelters, prisons, sports categories). Some benefit from expanded solidarity framing; others experience loss of sex-segregated sanctuary spaces and report increased vulnerability or discomfort. Objecting to the classification rule positions them as perpetrators of exclusion in the institutional framework, creating social cost to resistance. Cannot leave womanhood but can object at institutional/social cost.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_women, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, cisgender_women, excluded).

% Gain recognition and institutional access as men through self-identification. Removed from women's spaces by institutional re-sorting. Identity-lock: denying self-identification carries the same erasure cost. Structural benefit depends on the receiving category (men's spaces/sports/military) treating the reclassification as legitimate, which varies by jurisdiction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_men, beneficiary,
    powerless, civilizational, identity_locked, global).

% Bear lower salience costs than cisgender women (men's spaces are typically less bounded or exclusive), but experience category boundary renegotiation. Some resist inclusivity of trans men in male-coded spaces (sports, military, prisons). Objecting to trans male inclusion positions them as exclusionary, creating institutional/social friction. Exit is available (social position as 'man' is not erasable), but objecting carries social cost.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_men, payer,
    moderate, biographical, constrained, global).

% Prison systems, shelter networks, sports governing bodies, military institutions, and public facility operators must reclassify access rules from sex-segregated (biological marker gates) to gender-segregated (self-identification gates). This shifts administrative burden: new intake protocols, medical privacy redefinitions, grievance handling around access disputes. Some gain from reduced legal exposure to discrimination claims; others bear costs of facility conflicts and policy reversal cycles as legal standards shift. Constrained exit: they cannot opt out where legally mandated.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sex_segregated_space_operators, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, sex_segregated_space_operators, agenda_setter).

% Advocates and philosophers who ground category membership in immutable biological markers are excluded from the institutional conversation: the reading declares their ontological basis (biological sex as THE category gate) incoherent or illegitimate. They would argue the reading conflates sex (material fact) with gender (social interpretation), and that boundary collapse harms women's sex-based rights. Structurally trapped: cannot exit the domain (sex-based category membership is their framework), and asserting it positions them as bigots in the institutional frame.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, biological_sex_essentialists, excluded,
    organized, biographical, trapped, global).

% Sociologists and institutional designers who read gender category membership through sustained social performance and recognition (vs. internal identity or biology) are excluded from institutional decision-making. They would argue that institutional recognition requires demonstrated sustained performance, not private belief, and that pure self-identification without social corroboration creates verification problems and gaming incentives. Trapped: cannot exit the field, and advocating social-role criteria marks them as transphobic in the identity-reading frame.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, social_role_advocates, excluded,
    organized, biographical, trapped, global).

% Courts, executive agencies, and legislative bodies in liberal jurisdictions set and enforce the gender-identity reading through non-discrimination law, civil service policy, and institutional guidance. They author the rules, define compliance, and handle disputes. Positioned as neutral enforcers but structurally embedded in the reading's legitimacy: the reading gains force through their authority. Could theoretically adopt a different reading, but institutional inertia and legal precedent constrain exit.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, liberal_institutional_authority, agenda_setter,
    institutional, generational, analytical, global).

% Metaphysicians, epistemologists, and institutional theorists analyzing how the reading constructs and stabilizes the gendered category boundary. They measure the ε via gatekeeping costs, enforcement machinery, and resistance patterns. Neither beneficiaries nor payers; external to the stakeholder structure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, observer_philosophers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, liberal_institutional_authority).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes a shared category ('woman', 'man') whose boundaries had been contested. Allows institutions, legal systems, and social movements to coordinate on who is included in women's advocacy, protection, and solidarity without requiring biological gatekeeping. Enables trans people to access institutional recognition and space without medical/biological proof.
% TRANSFER_FUNCTION: Moves institutional authority to define the category boundary from biological/anatomical criteria to self-declared identity. This transfer creates new gatekeeping costs: institutions must process identity claims, handle disputes about sincerity, and absorb friction when assigned-sex populations resist reclassification of spaces. The extraction lies in the enforcement: trans people gain access, but the cost of enforcement (administrative overhead, resistance management, cis-women displacement from exclusive spaces) is borne by space operators and existing category members.
% ABSENT_VOICES: Biological sex essentialists and social-role theorists are structurally excluded from the consensus-building process. Women whose objections to space reclassification arise from sex-based safety analysis (rather than mere gender preference) find their concerns routed as bigotry rather than engaged as legitimate competing interests. Sex-segregated space users with no voice in policy design (incarcerated people, shelter residents, asylum seekers) experience reclassification as an administrative change imposed without consultation.
% DISAPPEARANCE_RATIONALE: If institutional recognition of gender identity as a category gate vanished overnight, trans women would lose legal sex/gender marker change, institutional bathroom/shelter/sports access would revert to sex-segregation, and the category 'woman' would re-anchor to biological criteria. Multiple stable institutional equilibria exist (and coexist in different jurisdictions), so the world does not rearrange into a single configuration, but institutional arrangements would substantially shift.
% FOUNDING_PROBLEM: Trans and non-binary people were institutionally invisible and unable to access sex-segregated spaces consistent with their gender identity. Medical gatekeeping and documentary evidence requirements for sex-marker change were cumbersome and dysphoria-inducing. Institutional categories ('woman', 'man') were treated as natural kinds grounded in biology, rendering gender identity incoherent in legal and policy frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocates attest the problem is live and urgent. Institutional non-discrimination offices and liberal legislative bodies attest the problem justifies the reading. Biological sex essentialists and gender-critical feminists attest the problem was misframed: the issue was not the category boundary but bureaucratic gatekeeping, which could have been addressed without boundary collapse. Sex-segregated space operators attest the problem's existence but dispute the solution's proportionality.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.48 (moderate) rather than high, because the reading does solve a genuine institutional coordination problem: trans people needed a pathway to legal sex-marker change and institutional recognition that did not require medical gatekeeping or documentary proof. However, extractiveness is not low because the enforcement creates gatekeeping costs (institutions must process and verify identity claims) and displacement costs (cisgender women lose exclusive space access). Suppression is 0.52 (moderate) because the reading's durability depends on actively excluding alternative frameworks: biological sex essentialists and social-role theorists are shut out of the institutional conversation, and cisgender women who object find their concerns routed as bigotry rather than engaged as competing interests. This is not violent suppression, but it is institutional: legal liability for 'discrimination,' employment risk for advocates, social shame for resistance. Theater ratio is 0.41 (moderate) because institutional compliance rhetoric emphasizes 'inclusion' and 'non-discrimination,' while much actual enforcement activity focuses on managing space-access disputes and controlling objections—the coordination function is real, but its salience in institutional communication is amplified relative to the extraction mechanisms. Accessibility collapse is 0.38 (moderate-low) because alternatives to the identity-reading remain intellectually available and are held by organized communities (biological essentialists, gender-critical feminists, social-role theorists); the collapse is institutional and legal, not complete. Resistance is 0.71 (high) because the reading meets sustained objections from cisgender women, biological essentialists, and social-role advocates; the resistance has not been overcome despite institutional enforcement. The time series shows extraction, suppression, and theater rising from t=0 to t=15 (the reading's institutional entrenchment period) and then plateauing from t=15 to t=35 (stable-state enforcement without further growth), suggesting the reading has reached institutional equilibrium rather than exponentially expanding.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (institutional authorities) and the payer seats (cisgender women, space operators) compute the constraint very differently. From the institutional authority position, the reading is genuine coordination: it solves the legitimate problem of trans institutional invisibility and reduces discrimination liability. From the cisgender-women position, the reading is enforced extraction: they lose exclusive space access and face social sanction for objecting. The engine computes this per-seat divergence from directionality (authorities are beneficiaries or neutral, women are payers) and power differentials. The excluded seats (biological essentialists, social-role theorists) would compute the reading as snare if seated: a pure extraction mechanism disguised as coordination, enforced by institutional authority and moral shaming. The framework's structural data enables the engine to measure this multi-seat divergence without adjudicating which perspective is 'correct'—the divergence itself is the metric.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and trans men are structural beneficiaries (d near 0.0): they gain institutional recognition, space access, and legal sex-marker change that was previously gatekept. Their exit is identity-locked: refusing institutional recognition of their declared gender feels like fundamental erasure, making exit identity-incompatible rather than merely constrained. Cisgender women are structural payers (d near 0.9 for many): they lose exclusive space access, face social sanction for objecting, and are positioned as perpetrators of exclusion if they resist. Their exit is constrained rather than trapped: they cannot leave womanhood, but they can object (at social cost), or accept the reclassification (at space-access cost). Space operators are payers (d moderate-to-high): they bear administrative overhead for identity verification and space-access management, and absorb conflict from cisgender-women objections. Institutional authorities are partially captured beneficiaries (d moderate): they gain authority (defining identity, managing disputes) but also bear enforcement costs. Biological essentialists and social-role theorists are excluded targets (d near 1.0): their frameworks are declared illegitimate, and asserting them carries employment and social risk. The directionality derives from beneficiary/victim declarations and exit-option structures; no overrides are needed because the structural relationships are stable across the interval.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure coordination by declaring cisgender women and space operators as victims: the reading solves a genuine institutional problem (trans recognition) but does so in a way that imposes costs on specific groups rather than symmetric coordination. The claim of tangled_rope is structurally sound: it requires both coordination function (trans access via identity recognition) AND asymmetric extraction (enforcement overhead, displacement, excluded frameworks), which matches the tangled_rope canonical definition. The constraint is not a snare because the coordination function is real and non-trivial (institutional recognition of gender identity was a live problem), and the payers are not purely coerced—some cisgender women benefit from expanded solidarity framing, and space operators who prioritize non-discrimination benefit from liability reduction. The constraint is not a rope because the asymmetry is structural: the reading's gatekeeping rules create extraction that cannot be reduced without compromising the coordination function itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of alternative frameworks (biological sex, social role) structural (institutional barriers, legal exclusion, resource withdrawal) or internalized (advocates of alternative readings have incorporated the reading''s moral framework such that resistance feels self-delegitimizing)?',
    'Longitudinal study of advocates who shift from biological/social-role readings to identity readings: do they report external pressure (law, employment risk, social shaming) or internal value realignment (the reading convinced them the prior framework was wrong)? Post-exit trajectories of those who maintain alternative frameworks despite suppression.',
    'If suppression is primarily structural, the constraint''s persistence depends on continued enforcement machinery; if internalized, the reading is durable even if enforcement relaxes. This affects whether the type is snare (structural extraction) or partially rope (adopted coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression of alternative readings is structural (external barriers) or internalized (advocates have adopted the reading''s moral logic)').

omega_variable(
    identity_authenticity_verification,
    'How do institutions operationalize the boundary between sincere gender identity and instrumental self-identification for resource gain (bathroom access, sports advantage, prison placement safety)? Is the boundary coherent or vague?',
    'Case law analysis of contested identity claims in institutional settings; empirical study of gaming incentives and actual gaming prevalence across different institutional domains.',
    'If the boundary is vague and verification is costly, extraction increases (institutions bear gatekeeping costs; legitimate trans people bear verification burden; gaming incentives create friction). If the boundary is operationally coherent, extraction is lower. This affects whether the reading is tangled_rope (moderate extraction) or snare (high extraction from gatekeeping costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_authenticity_verification, empirical, 'Whether self-identification can be operationalized without imposing verification costs that function as hidden extraction').

omega_variable(
    kernel_reading_contest,
    'Is the gender-identity reading one legitimate perspective on how category membership should work, or is it THE correct answer such that alternative readings (biological sex, social role) are simply false?',
    'Philosophical analysis: what would it take to show the reading is wrong? If no empirical or logical evidence could refute it, it is a framework choice, not a discovery. If specific countervailing evidence would vindicate an alternative reading, the reading is empirically contestable.',
    'If the reading is a framework choice (conceptual), the constraint is less about discovering the true nature of gender and more about coordinating institutional recognition under one framing—which is a different extraction story than natural-law verification. If empirically contestable, the constraint''s durability depends on evidence and enforceability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the identity-reading is one legitimate framework or a factual discovery that makes alternatives false').

omega_variable(
    cis_women_cost_asymmetry,
    'Do cisgender women experience the reclassification as loss of exclusive access to sex-segregated spaces (material cost) or as symbolic delegitimization of sex-based solidarity (identity cost)? Are these experienced as equivalent or asymmetric?',
    'Qualitative research with cisgender women in affected spaces (bathrooms, shelters, prisons, sports); measurement of reported safety changes vs. reported identity/solidarity costs; comparison to cost reports from trans women.',
    'If cisgender women experience primarily material safety costs, those are tractable (facility design, management protocols). If primarily identity costs (the reading symbolically negates sex-based womanhood), the cost is structural to the reading itself and cannot be fixed via facility improvement. Type divergence: if material costs dominate, institutional redesign might reduce extraction; if identity costs dominate, the constraint is inherently asymmetric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cis_women_cost_asymmetry, empirical, 'Whether cis women''s costs are material (safety) or identity-level (sex-based solidarity negated)').

omega_variable(
    institutional_agenda_setter_capture,
    'Are institutional authorities (courts, executive agencies, legislative bodies) genuine neutral enforcers of the reading, or do they have vested interests in the reading''s durability (expanding their authority to define identity, reducing liability from discrimination claims, signaling institutional progressivism)?',
    'Institutional analysis of how authority structures changed in response to the reading; cost-benefit analysis of identity-verification vs. sex-segregation for institutional administrators; study of whether institutions would revert if legal mandate changed.',
    'If authorities are vested beneficiaries (not just enforcers), the constraint''s extraction includes the institutional authority''s rent-collection—the ''active enforcement'' is self-interested. This would make the type more snare-like (institutional capture hiding behind coordination frame) vs. genuine tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_agenda_setter_capture, empirical, 'Whether institutional authorities are neutral enforcers or vested beneficiaries of the reading''s persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gend_tr_t0, observed).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(gend_tr_t5, observed).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(gend_tr_t10, observed).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__gender_identity_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(gend_tr_t15, observed).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(gend_tr_t20, observed).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__gender_identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(gend_tr_t25, observed).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__gender_identity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(gend_tr_t30, observed).
narrative_ontology:measurement(gend_tr_t35, gendered_category_membership__gender_identity_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(gend_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(gend_be_t0, observed).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(gend_be_t5, observed).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(gend_be_t10, observed).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__gender_identity_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(gend_be_t15, observed).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(gend_be_t20, observed).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__gender_identity_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(gend_be_t25, observed).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__gender_identity_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(gend_be_t30, observed).
narrative_ontology:measurement(gend_be_t35, gendered_category_membership__gender_identity_reading, base_extractiveness, 35, 0.48).
narrative_ontology:measurement_basis(gend_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(gend_su_t0, observed).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(gend_su_t5, observed).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(gend_su_t10, observed).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__gender_identity_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(gend_su_t15, observed).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(gend_su_t20, observed).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__gender_identity_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(gend_su_t25, observed).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__gender_identity_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(gend_su_t30, observed).
narrative_ontology:measurement(gend_su_t35, gendered_category_membership__gender_identity_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(gend_su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(gend_grid_01, gendered_category_membership__gender_identity_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(gend_grid_02, gendered_category_membership__gender_identity_reading, accessibility_collapse(class), 35, 0.48).
narrative_ontology:measurement(gend_grid_03, gendered_category_membership__gender_identity_reading, accessibility_collapse(individual), 0, 0.18).
narrative_ontology:measurement(gend_grid_04, gendered_category_membership__gender_identity_reading, accessibility_collapse(individual), 35, 0.35).
narrative_ontology:measurement(gend_grid_05, gendered_category_membership__gender_identity_reading, accessibility_collapse(organizational), 0, 0.28).
narrative_ontology:measurement(gend_grid_06, gendered_category_membership__gender_identity_reading, accessibility_collapse(organizational), 35, 0.42).
narrative_ontology:measurement(gend_grid_07, gendered_category_membership__gender_identity_reading, accessibility_collapse(structural), 0, 0.22).
narrative_ontology:measurement(gend_grid_08, gendered_category_membership__gender_identity_reading, accessibility_collapse(structural), 35, 0.38).
narrative_ontology:measurement(gend_grid_09, gendered_category_membership__gender_identity_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(gend_grid_10, gendered_category_membership__gender_identity_reading, resistance(class), 35, 0.75).
narrative_ontology:measurement(gend_grid_11, gendered_category_membership__gender_identity_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(gend_grid_12, gendered_category_membership__gender_identity_reading, resistance(individual), 35, 0.7).
narrative_ontology:measurement(gend_grid_13, gendered_category_membership__gender_identity_reading, resistance(organizational), 0, 0.65).
narrative_ontology:measurement(gend_grid_14, gendered_category_membership__gender_identity_reading, resistance(organizational), 35, 0.72).
narrative_ontology:measurement(gend_grid_15, gendered_category_membership__gender_identity_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(gend_grid_16, gendered_category_membership__gender_identity_reading, resistance(structural), 35, 0.68).
narrative_ontology:measurement(gend_grid_17, gendered_category_membership__gender_identity_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(gend_grid_18, gendered_category_membership__gender_identity_reading, stakes_inflation(class), 35, 0.62).
narrative_ontology:measurement(gend_grid_19, gendered_category_membership__gender_identity_reading, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(gend_grid_20, gendered_category_membership__gender_identity_reading, stakes_inflation(individual), 35, 0.48).
narrative_ontology:measurement(gend_grid_21, gendered_category_membership__gender_identity_reading, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(gend_grid_22, gendered_category_membership__gender_identity_reading, stakes_inflation(organizational), 35, 0.52).
narrative_ontology:measurement(gend_grid_23, gendered_category_membership__gender_identity_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(gend_grid_24, gendered_category_membership__gender_identity_reading, stakes_inflation(structural), 35, 0.55).
narrative_ontology:measurement(gend_grid_25, gendered_category_membership__gender_identity_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(gend_grid_26, gendered_category_membership__gender_identity_reading, suppression(class), 35, 0.58).
narrative_ontology:measurement(gend_grid_27, gendered_category_membership__gender_identity_reading, suppression(individual), 0, 0.22).
narrative_ontology:measurement(gend_grid_28, gendered_category_membership__gender_identity_reading, suppression(individual), 35, 0.42).
narrative_ontology:measurement(gend_grid_29, gendered_category_membership__gender_identity_reading, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(gend_grid_30, gendered_category_membership__gender_identity_reading, suppression(organizational), 35, 0.55).
narrative_ontology:measurement(gend_grid_31, gendered_category_membership__gender_identity_reading, suppression(structural), 0, 0.28).
narrative_ontology:measurement(gend_grid_32, gendered_category_membership__gender_identity_reading, suppression(structural), 35, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.1).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the gendered-category-membership constraint family. All three readings share a kernel (what grounds category membership) but differ in how they instantiate it, with different ε profiles, victim/beneficiary structures, and institutional durability. The identity-reading affects the biological and social-role readings by redefining the institutional terrain: institutional authority now grounds membership in identity rather than biology or performance, which creates pressure on the alternative readings' institutional viability. The biological reading and social-role reading remain live in other institutional contexts (courts, scientific communities, other jurisdictions) but are excluded from the primary institutional frame under which this reading operates. Network edges propagate this exclusion: institutional capture of the identity-reading raises the cost of maintaining the alternative readings in adjacent domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
