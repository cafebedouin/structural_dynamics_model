% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Woman Category Definition: Gender Identity Reading
 *   domain: political_philosophy/law/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the gender_identity_reading of the contested
 *   kernel 'woman_category': a legal and policy regime that defines womanhood
 *   on the basis of internal gender identity rather than biological sex. The
 *   reading benefits transgender women and nonbinary individuals seeking
 *   legal recognition, while imposing costs on cisgender women (loss of
 *   exclusive boundary), sex-segregated space users (ambiguous access), and
 *   athletes in sex-segregated sports (fairness collision). The constraint is
 *   actively enforced through legal mandates, institutional policy,
 *   professional standards (medical, human-rights), and social pressure
 *   against dissenting views. The claimed type is tangled rope: it has
 *   genuine coordination function (unified, legally-enforceable category
 *   definition) AND asymmetric extraction (costs borne by some, gains to
 *   others, active enforcement suppressing alternatives). The metrics reflect
 *   high extractiveness and suppression because the constraint's persistence
 *   depends on institutional power and professional consensus that silences
 *   sex-based objections, not on participant voluntary coordination.
 *
 * KEY AGENTS:
 *   - transgender_women: powerless agents seeking legal recognition; identity_locked exit (recognition is identity-constituting)
 *   - cisgender_women: organized powerful agents losing exclusive boundary and sex-based legal analysis framework
 *   - sex_segregated_space_users: moderate-power constrained agents experiencing boundary ambiguity
 *   - athletes in regulated sports: moderate-power constrained agents facing fairness collision
 *   - sports governing bodies and legal authorities: institutional agenda-setters enforcing the reading
 *   - sex_based rights advocates: excluded, constrained organized agents whose core claim is structurally foreclosed
 *   - analytical observer: reads the access-rights vs. exclusion-rights collision as the constraint's structural signature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.72).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category Definition: Gender Identity Reading").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '39945fe2-3b50-4536-884a-eed699046295').
narrative_ontology:cs_kernel_codification('39945fe2-3b50-4536-884a-eed699046295', distributed).
narrative_ontology:cs_authority_grounding('39945fe2-3b50-4536-884a-eed699046295', extraction).
narrative_ontology:cs_interpretation_layer_present('39945fe2-3b50-4536-884a-eed699046295').
narrative_ontology:cs_reading_relation('39945fe2-3b50-4536-884a-eed699046295', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('39945fe2-3b50-4536-884a-eed699046295', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('39945fe2-3b50-4536-884a-eed699046295', foundational, gender_identity_legally_determinative).
narrative_ontology:cs_axiom_status(gender_identity_legally_determinative, holdable).
narrative_ontology:cs_axiom_grounding('39945fe2-3b50-4536-884a-eed699046295', gender_identity_legally_determinative, deontological).
narrative_ontology:cs_axiom('39945fe2-3b50-4536-884a-eed699046295', foundational, sex_based_analysis_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_analysis_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('39945fe2-3b50-4536-884a-eed699046295', sex_based_analysis_discriminatory, deontological).
narrative_ontology:cs_reference_frame('39945fe2-3b50-4536-884a-eed699046295', identity_based_legal_recognition).
narrative_ontology:cs_drift_state('39945fe2-3b50-4536-884a-eed699046295', contemporary_institutional_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39945fe2-3b50-4536-884a-eed699046295', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, nonbinary_individuals_identifying_feminine).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_segregated_space_users).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, athletes_in_regulated_sports).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, sex_segregated_space_users).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_institutional_advocates).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_only_organizations).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_is_legally_relevant).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, self_identification_suffices_for_legal_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition of womanhood based on internal gender identity without requiring medical transition, biological proof, or administrative delay. Can access women-designated services, shelters, bathrooms, and legal documents (birth certificate, ID) reflecting their identified gender. The constraint is a direct enabler of their civil recognition. Exit would mean reversion to legal non-recognition or documentation as male, which contradicts their identity.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    powerless, biographical, identity_locked, national).

% Gain access to 'woman' category legally and socially under the identity criterion, even without identifying fully as woman—those identifying as feminine-nonbinary or genderqueer benefit from the expanded, identity-inclusive definition. Can choose whether to use the category for different purposes (ID documents, sports, spaces). Exit from this recognition contradicts their actual identity.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, nonbinary_individuals_identifying_feminine, beneficiary,
    powerless, biographical, identity_locked, national).

% Bear diffuse costs: sex-segregated spaces (bathrooms, shelters, changing rooms, prisons) become identity-inclusive rather than biology-based. Abortion access arguments invoking 'women's bodies' must specify 'pregnant people' or exclude transgender men, altering reproductive-autonomy vocabulary. Women's sports eligibility shifts from sex-based to identity-based, creating fairness ambiguity. Cannot exit 'woman' category to preserve its exclusivity without erasing their own gender.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women, payer,
    organized, generational, constrained, national).

% Users of sex-segregated spaces (bathrooms, shelters, changing facilities, prisons) face boundary ambiguity about who may enter. Some experience loss of privacy protection (women-only boundary now includes all who identify as women); others experience inclusivity gain. Functionally trapped (cannot exit bathrooms, must use shelters, legally mandated prison segregation), so exit options are minimal.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_segregated_space_users, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, sex_segregated_space_users, beneficiary).

% Face identity-inclusive eligibility criteria for women's sports categories. Transgender women admitted without medical transition may retain strength/speed advantages from male puberty, creating fairness collision. Competitive opportunity and safety concerns collide with inclusion mandates. Cannot exit sport entirely if it is their career path.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    moderate, biographical, constrained, global).

% Set eligibility rules for competition categories under legal non-discrimination mandates. Must accommodate identity-based membership while managing physiological variation and fairness claims. Options: (a) accept identity-only, manage variation openly, (b) demand medical transition proof, (c) create new categories. Each option produces different victim sets and enforcement costs. Navigate between legal mandates and athlete/stakeholder demands.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Implement legal recognition of gender identity for documents (birth certificate, driver license, passport) under anti-discrimination legal frameworks. Balance anti-discrimination mandates against competing claims about privacy, safety, fraud prevention, and sex-based protections. Set enforcement machinery: what evidence suffices for identity change, whether prior identity is disclosed, whether sex-segregated access tracks identity or biology.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legal_authorities_and_states, agenda_setter,
    institutional, generational, constrained, national).

% Organizations built on sex-exclusivity (women's colleges, feminist collectives, lesbian communities) face boundary redefinition. The constraint mandates they treat identity as sufficient for membership. Some welcome expansion; others experience institutional capture or loss of organizing principle. Theoretically can exit by closure or mission change, but institutional identity is tied to women-only status.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_only_organizations, agenda_setter,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, women_only_organizations, payer).

% Argue that 'woman' should remain sex-based because sex-based protections (abortion access, domestic violence law, workplace discrimination law) are necessary and irreplaceable by gender-identity-based protections. Systematically excluded from institutional decision-making, treated as discriminatory, facing social and professional penalties for advancing sex-based analysis. Their core claim—that sex and gender are analytically distinct and sex-based harms require sex-based solutions—is structurally foreclosed by this reading.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, excluded,
    organized, generational, constrained, national).

% Institutional advocates (human-rights organizations, medical authorities, legal scholars) who promote legal recognition of gender identity. Benefit from institutional adoption of the reading; deploy social, legal, professional pressure to enforce compliance. Can shift organizing focus if the constraint weakens. Institutional power and legitimacy depend on the reading's dominance.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_institutional_advocates, beneficiary,
    organized, generational, mobile, national).

% Examines the structural properties: beneficiary/victim asymmetry, enforcement machinery, the boundary it draws, and the access-rights vs. exclusion-rights collision it creates. Does not benefit or pay; observes how institutional and individual seats experience divergent constraints from the same rule.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, gender_identity_institutional_advocates).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, legally-enforceable definition of womanhood for civil purposes (law, policy, institutional practice) rather than allowing each domain to use competing definitions (biology, social role, identity). Enables transgender people to access legal recognition. Ends fragmentation where someone is legally a man for some purposes and a woman for others.
% TRANSFER_FUNCTION: Transfers from cisgender women and sex-segregated space users the exclusive claim to 'woman' category membership and the boundary protections it provided (privacy, safety, exclusivity). Transfers from sex-based-rights advocates institutional legitimacy of sex-based legal analysis. Transfers to transgender women legal recognition and access rights previously unavailable. Accrues institutional power and legitimacy to gender-identity institutional advocates.
% ABSENT_VOICES: Sex-based rights advocates are systematically excluded from institutional decision-making, treated as discriminatory, and face professional and social penalties. The intersex accommodation reading is sidelined—biological sex variation complicates the binary identity framework.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, transgender women would lose legal recognition, sex-segregated spaces would revert to biology-based access, sports categories would return to sex-only eligibility, and identity-based civil recognition would require lengthy medical transition. The institutional machinery enforcing identity-as-sufficient would be dismantled; the political landscape would reorganize around competing definitions.
% FOUNDING_PROBLEM: Historically, transgender people were denied legal recognition of their gender identity and forced into legal categories (male/female) that contradicted their identity, creating severe civil, social, and psychological harms. The state treated gender identity as irrelevant to legal personhood despite its centrality to dignity.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocates and many human-rights organizations attest the problem is live and urgent. Medical authorities (AMA, APA) support recognition of gender identity. Sex-based rights advocates contest the diagnosis: the founding problem is real but incorrectly solved by erasing sex-based categories; they propose recognizing both sex and gender as legally relevant. Legislative and judicial testimony reveals active disagreement on whether legal recognition requires de-prioritizing sex-based analysis.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.68 over the interval because institutional adoption of the reading increases: initially contested and partial (T=0), the reading becomes legal default in some jurisdictions and professional norm in medicine/human-rights by T=25. Suppression is high throughout (0.48→0.72) because the constraint's stability depends on institutional enforcement and suppression of sex-based objections—sex-based advocates face professional penalties, legal exclusion, and social pressure that escalate through the interval. Theater ratio is moderate and rising (0.22→0.41) because institutional rhetoric emphasizes inclusion and non-discrimination (the performative frame), while the underlying enforcement against sex-based analysis has intensified. Accessibility_collapse (0.62) is moderate: alternatives (sex-based definition, intersex-accommodation reading) remain theoretically available but institutionally delegitimized; someone wanting to use sex-based analysis faces severe professional and social costs. Resistance (0.78) is high: sex-based advocates maintain active objection despite suppression; the constraint does not achieve normalization; compliance is enforced, not internalized. The measurements share one time grid (every metric authored at every time point) so temporal analysis is coherent.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (transgender women) and the payer seats (cisgender women, athletes, space users) compute radically different types. From the transgender_women_beneficiary seat with identity_locked exit: this is rope or scaffold (coordination solving their recognition problem, enabling exit from non-recognition). From the cisgender_women_payer seat with organized power but constrained exit: this is snare or tangled_rope (asymmetric extraction, enforced, no exit to maintain sex-based boundaries). The agenda-setter seat (legal authorities) computes as rope: they manage coordination between competing claims. The engine computes the per-seat type from power + exit + directionality; the authored divergence in interpretation (coordination vs. extraction, inclusion vs. boundary loss) should surface as seat divergence in computed classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender_women: beneficiary, powerless, identity_locked. Derivation: beneficiary role + identity_locked exit → d near 0.0 (full beneficiary, receives recognition without running the system). Cisgender_women: payer, organized power, constrained exit. Derivation: payer role + organized power + constrained exit (cannot exit 'woman' category) → d near 0.8 (targets of extraction who bear costs and cannot walk away). Sex_based_rights_advocates: excluded, organized, constrained. Derivation: excluded role + constrained exit (cannot exit the discourse without social/professional penalty) → d near 1.0 (full targets of the suppression machinery). The directionality profile explains why the same constraint produces different beneficiary/target classifications per seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling as pure rope by declaring both beneficiaries (transgender women) and victims (cisgender women, athletes, space users). The victims are not incidental—they are structural: the constraint's coordination function (unified definition) is inseparable from asymmetric access rights (transgender women gain, cisgender women lose exclusive boundary). Tangled rope captures this: genuine coordination + compulsory participation + asymmetric costs. Without the victim declarations, the constraint would mislabel as rope (coordination without extraction). Without the beneficiary declarations, it would mislabel as pure snare (extraction without coordination). The theater ratio (0.41 at T=25) indicates rising performative frame (inclusion rhetoric) masking enforcement machinery (suppression of sex-based objections), consistent with a constraint that extracts while claiming coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_verification_boundary,
    'What evidence suffices to verify gender identity for legal purposes, and does the sufficiency criterion constitute a hidden biology gate?',
    'Document the operational standard used by legal authorities (self-declaration only vs. medical letter vs. psychological evaluation) and track whether practice converges toward medical gatekeeping despite formal self-declaration policy.',
    'If practice requires medical evaluation while policy claims self-declaration, the constraint contains hidden asymmetry: transgender people with medical access and diagnosis are beneficiaries; transgender people without access are excluded, making the constraint effectively snare-ish for them. The ε would shift depending on who can access verification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_verification_boundary, empirical, 'Whether identity verification requires medical mediation despite policy language').

omega_variable(
    sex_based_protections_fungibility,
    'Are sex-based legal protections (abortion access, workplace discrimination law, domestic violence law) functionally replaceable by gender-identity-based protections, or does sex-based analysis capture harms gender-based analysis misses?',
    'Compare legal outcomes (case decisions, policy effectiveness) across jurisdictions that use sex-based vs. gender-identity-based frameworks; track whether sex-specific harms (pregnancy-based discrimination, menstruation-related stigma, reproductive coercion) are adequately captured under identity-based doctrines.',
    'If sex-based protections are not fungible, the constraint''s mandate to replace sex-based with identity-based analysis creates genuine victim harm to those needing sex-specific protections. Extractiveness would be higher for those victims because the constraint forecloses analysis that protects them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_protections_fungibility, empirical, 'Whether sex-based legal protections are replaceable by identity-based protections').

omega_variable(
    suppression_mechanism_scope,
    'Is the high suppression (0.72) structural (institutional policy excludes sex-based voices) or internalized (advocates internalize the constraint''s delegitimizing framing and self-silence)?',
    'Post-institutional-change trajectory: if suppression persists after legal mandate is removed (private-context persistence of self-silencing), reclassify as partially internalized; if suppression drops when institutional enforcement is removed, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the 0.72 structural measure suggests—advocates carry the suppression with them into spaces without enforcement. Suppression_ambiguity_omega implies the constraint operates differently across contexts (institutional vs. private).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_scope, empirical, 'Whether suppression is structural (institutional) or internalized (belief-level)').

omega_variable(
    boundary_vs_inclusion_tension,
    'Is the access-rights vs. exclusion-rights collision (transgender women''s access rights conflict with cisgender women''s exclusivity claims) conceptually resolvable, or is it a structural zero-sum imposed by category design?',
    'Propose and evaluate alternative institutional designs (e.g., sex-segregated + identity-based layered categories, context-specific definitions) and assess whether any design dissolves the collision without creating new victims.',
    'If resolvable, the high extractiveness reflects institutional design choice, not inherent constraint structure—different regime choice could lower costs. If zero-sum structural, extractiveness reflects the kernel''s irreducible tension and cannot be reduced without reframing ''woman'' entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_vs_inclusion_tension, conceptual, 'Whether the access-rights collision is structurally inherent or institutionally designed').

omega_variable(
    kernel_reading_boundary,
    'Is the gender_identity_reading internally coherent, or does it smuggle in residual sex-based distinctions (e.g., defining womanhood by identity but allowing testosterone limits in sports, which are sex-linked)?',
    'Audit institutional policies across domains (identity documents, bathrooms, sports, prisons, shelters) for consistency in identity-sufficiency criterion vs. hidden biological gates; document contradictions.',
    'Incoherence would indicate the reading is not a clean identity framework but rather a hybrid framework overlaying identity on biology, generating confusion and different effective constraints per domain. Would suggest the three sibling readings (identity-only, biology-only, intersex-spectrum) are each more internally consistent than the actual institutional implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the identity-reading is internally coherent or contains hidden biological gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(woma_tr_t4, observed).
narrative_ontology:measurement(woma_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(woma_tr_t8, observed).
narrative_ontology:measurement(woma_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(woma_tr_t12, observed).
narrative_ontology:measurement(woma_tr_t17, woman_category__gender_identity_reading, theater_ratio, 17, 0.4).
narrative_ontology:measurement_basis(woma_tr_t17, observed).
narrative_ontology:measurement(woma_tr_t25, woman_category__gender_identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(woma_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(woma_be_t4, observed).
narrative_ontology:measurement(woma_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(woma_be_t8, observed).
narrative_ontology:measurement(woma_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(woma_be_t12, observed).
narrative_ontology:measurement(woma_be_t17, woman_category__gender_identity_reading, base_extractiveness, 17, 0.67).
narrative_ontology:measurement_basis(woma_be_t17, observed).
narrative_ontology:measurement(woma_be_t25, woman_category__gender_identity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(woma_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(woma_su_t4, observed).
narrative_ontology:measurement(woma_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(woma_su_t8, observed).
narrative_ontology:measurement(woma_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(woma_su_t12, observed).
narrative_ontology:measurement(woma_su_t17, woman_category__gender_identity_reading, suppression_requirement, 17, 0.7).
narrative_ontology:measurement_basis(woma_su_t17, observed).
narrative_ontology:measurement(woma_su_t25, woman_category__gender_identity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(woma_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_segregated_bathrooms_access_policy).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, female_athlete_eligibility_rule).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, abortion_access_pregnant_people_framing).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_based_discrimination_law_scope).

% DUAL FORMULATION NOTE:
% The woman_category kernel contains three structurally distinct readings with different ε values and beneficiary/victim sets. The gender_identity_reading (this story) benefits transgender people but imposes costs on cisgender women and sex-segregated space users; ε~0.68 at T=25. The sex_biology_reading benefits those needing sex-specific legal analysis and imposes costs on transgender people; ε would be ~0.45 (lower suppression, weaker enforcement infrastructure). The intersex_accommodation_reading benefits intersex people but requires institutional complexity that neither identity-only nor biology-only readings demand; ε would be ~0.55 (moderate extraction from both sides, enforcement costs for new category infrastructure). Do not merge these stories—each reading is a separate constraint with independent classification. Link via network.affects_constraints to indicate the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__gender_identity_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
