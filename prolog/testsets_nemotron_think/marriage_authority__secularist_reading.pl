% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Uniform Civil Code as Elimination of Personal Law Pluralism
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   The secularist reading of marriage authority holds that the democratic
 *   legislature has exclusive competence to enact a Uniform Civil Code, and
 *   that the persistence of religious personal laws is a colonial-era anomaly
 *   that undermines gender equality and national integration. This reading
 *   drives the current political project to legislate a UCC, which would
 *   eliminate the field of personal law pluralism. The constraint is a
 *   tangled rope: it performs a genuine coordination function (uniform family
 *   law) while extracting communal legal autonomy from minority communities,
 *   and it requires active enforcement (legislative action, judicial review,
 *   police enforcement) to override entrenched pluralism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.75).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.8).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Uniform Civil Code as Elimination of Personal Law Pluralism").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '89115040-66a1-4a9e-b63b-2a6630b519f4').
narrative_ontology:cs_kernel_codification('89115040-66a1-4a9e-b63b-2a6630b519f4', formalized).
narrative_ontology:cs_authority_grounding('89115040-66a1-4a9e-b63b-2a6630b519f4', lineage).
narrative_ontology:cs_reading_relation('89115040-66a1-4a9e-b63b-2a6630b519f4', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('89115040-66a1-4a9e-b63b-2a6630b519f4', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('89115040-66a1-4a9e-b63b-2a6630b519f4', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('89115040-66a1-4a9e-b63b-2a6630b519f4', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('89115040-66a1-4a9e-b63b-2a6630b519f4', foundational, legislative_monopoly_on_family_law).
narrative_ontology:cs_axiom_status(legislative_monopoly_on_family_law, holdable).
narrative_ontology:cs_axiom_grounding('89115040-66a1-4a9e-b63b-2a6630b519f4', legislative_monopoly_on_family_law, conventional).
narrative_ontology:cs_axiom('89115040-66a1-4a9e-b63b-2a6630b519f4', foundational, personal_law_pluralism_as_transitional_anomaly).
narrative_ontology:cs_axiom_status(personal_law_pluralism_as_transitional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('89115040-66a1-4a9e-b63b-2a6630b519f4', personal_law_pluralism_as_transitional_anomaly, empirically_contingent).
narrative_ontology:cs_reference_frame('89115040-66a1-4a9e-b63b-2a6630b519f4', constitutional_directive_article_44).
narrative_ontology:cs_drift_state('89115040-66a1-4a9e-b63b-2a6630b519f4', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89115040-66a1-4a9e-b63b-2a6630b519f4', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, women_in_minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, majoritarian_religious_groups).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, legislative_supremacy_in_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, legal_uniformity_as_modernity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of majoritarian political parties, state legislatures, and sections of the feminist movement that pushes for a Uniform Civil Code. They control the legislative agenda and benefit from a unified legal framework that consolidates state authority over family law and aligns with a homogenized national identity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary).

% Religious minority communities (primarily Muslim, but also Christian, Parsi, and tribal groups) whose personal laws govern marriage, divorce, inheritance, and adoption. They bear the cost of losing communal legal autonomy, seeing the UCC as majoritarian imposition. Exit is constrained because they are subject to the sovereign's territorial jurisdiction and cannot opt out of a national code.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, constrained, national).

% Women within minority communities who may gain expanded rights (e.g., divorce, maintenance, inheritance) under a UCC but lose the protective framework of community adjudication. The secularist reading claims to act in their interest, yet they are not the primary architects of the constraint and their agency is mediated by both community and state.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, women_in_minority_communities, beneficiary,
    moderate, biographical, constrained, national).

% The Supreme Court has repeatedly urged the legislature to enact a UCC (Article 44) and has incrementally imposed constitutional floors on personal laws through judicial review. It sits as an observer-analyst that can accelerate or delay the constraint's realization through its jurisprudence.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Hindu nationalist organizations and traditionalist groups that support a UCC because they expect it to reflect majoritarian norms (e.g., Hindu law as the default). They benefit from the symbolic and substantive consolidation of a uniform code that mirrors their practices, though they are not the primary agenda-setters.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, majoritarian_religious_groups, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable family law framework for all citizens, replacing a fragmented plural system; enables portable marriage/divorce rights, simplifies inter-community disputes, and gives the state a unified regulatory surface for gender-justice reforms.
% TRANSFER_FUNCTION: Moves legislative authority over family law from community-based bodies (personal law boards, religious tribunals) to the democratic legislature, and transfers the power to define marital rights from communal norms to a uniform statutory code. The extraction is the loss of communal legal autonomy by minority communities; the gain is centralized state authority and a putatively gender-just code.
% ABSENT_VOICES: Minority community members who support pluralism as a shield against majoritarianism; tribal groups with customary laws not captured by existing personal law systems; queer and non-binary persons whose relationship recognition is not addressed by either personal laws or current UCC proposals. These voices are excluded from the dominant secularist-communal binary.
% DISAPPEARANCE_RATIONALE: If the UCC project vanished overnight, personal law pluralism would persist and likely deepen; the legislative push for uniformity would collapse, and the constitutional directive (Article 44) would remain a dead letter. The political coalition driving the UCC would lose its central mobilizing issue, and minority communities would retain legal autonomy.
% FOUNDING_PROBLEM: The colonial state's 1772 Warren Hastings plan froze personal laws as they stood in 1772, creating a fragmented legal landscape that the postcolonial Constitution sought to overcome via Article 44 (UCC). The founding problem was to replace colonial-era religious legal fragments with a single modern code that ensures gender equality and national integration.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional framers (Constituent Assembly debates) attest that Article 44 was a directive for future legislative action, not an immediate command. Feminist scholars (e.g., Flavia Agnes, Lotika Sarkar) corroborate that gender justice was a stated goal but argue the UCC has become a majoritarian weapon. Minority community leaders (All India Muslim Personal Law Board) attest the problem is manufactured — pluralism was a negotiated settlement, not an anomaly.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the UCC project transfers lawmaking authority from communities to the state, stripping minority communities of a core aspect of self-governance. Suppression is high (0.8) because the constraint's realization depends on overriding constitutional protections for religious freedom (Article 25-26) and political resistance from organized minority institutions. Theater ratio is moderate (0.4): the gender-justice rhetoric is real but a growing share of enforcement energy serves majoritarian consolidation. Accessibility collapse (0.6) reflects that personal law alternatives still exist but are legally and politically besieged. Resistance (0.7) captures sustained mobilization by minority boards, regional parties, and civil society.
 *
 * PERSPECTIVAL GAP:
 *   From the secularist seat, the constraint is a rope (coordination for gender justice and national unity). From the minority community seat, it is a snare (extraction of autonomy under cover of reform). The engine will compute this divergence from the declared beneficiaries/victims and exit options. The claimed_type (tangled_rope) acknowledges both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_modernist_coalition is the structural beneficiary (d near 0.0): it controls the legislative agenda, gains centralized authority, and advances a majoritarian nation-building project. Minority_religious_communities are the structural targets (d near 1.0): they bear the loss of legal autonomy with constrained exit. Women_in_minority_communities sit near symmetric (d ~0.5): they may gain statutory rights but lose community adjudication forums. The Supreme Court is an analytical observer (d=0.5). Majoritarian_religious_groups are incidental beneficiaries (d low) but not agenda-setters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial legal fragmentation) is contested: the secularist reading treats it as live, while minority communities and many historians treat it as dead (pluralism was a deliberate postcolonial choice). The constraint persists because the mandatrophy (legislative monopoly over family law) has not been resolved — the legislature has not enacted a UCC in 74 years, but the demand remains a live political project. The classification prevents mislabeling this as pure coordination (rope) by naming the victims and the active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the marriage_authority kernel, or does it collapse into the communal_autonomy_reading''s counter-claim?',
    'Trace the legislative history of UCC demands and the constitutional debates on Article 44 to see if the secularist reading has a continuous distinct lineage or is a post-1980s majoritarian construction.',
    'If the secularist reading is a recent construction, its claim to be the ''true'' constitutional mandate weakens, and the constraint may reclassify as a snare (majoritarian extraction) rather than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the secularist reading is a genuine constitutional commitment or a majoritarian repurposing.').

omega_variable(
    zero_sum_challenge,
    'Does the secularist reading''s core premise (pluralism as transitional anomaly) logically foreclose the communal_autonomy_reading, or do they coexist as competing legitimate frameworks?',
    'Analyze whether a single constitutional framework can simultaneously treat pluralism as both a permanent right (communal) and a transitional anomaly (secularist). The Supreme Court''s jurisprudence on Article 25 vs Article 44 is the test case.',
    'If forecloses, the constraint family has a structural fault line that prevents stable coexistence; if coexists_with, the kernel remains a site of permanent contestation without logical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zero_sum_challenge, conceptual, 'Logical relationship between secularist and communal autonomy readings.').

omega_variable(
    gender_justice_vs_majoritarianism,
    'Is the measured extraction (loss of minority autonomy) a necessary cost of gender justice, or is gender justice a cover for majoritarian extraction?',
    'Compare the gender-equality provisions in actual UCC drafts (e.g., 2018 Law Commission consultation paper) with the provisions in reformed personal laws (e.g., 2019 Triple Talaq Act, 2005 Hindu Succession Amendment). If UCC drafts do not improve on reformed personal laws, the gender-justification is suspect.',
    'If cover, the constraint''s claimed coordination function is largely theater, pushing theater_ratio higher and potentially reclassifying toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_justice_vs_majoritarianism, empirical, 'Whether gender justice is the genuine coordination function or a legitimating cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t15, marriage_authority__secularist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t45, marriage_authority__secularist_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t60, marriage_authority__secularist_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(marriage_authority__secularist_reading_tr_t74, marriage_authority__secularist_reading, theater_ratio, 74, 0.4).

% Extraction over time
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t15, marriage_authority__secularist_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t45, marriage_authority__secularist_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t60, marriage_authority__secularist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(marriage_authority__secularist_reading_be_t74, marriage_authority__secularist_reading, base_extractiveness, 74, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t15, marriage_authority__secularist_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t45, marriage_authority__secularist_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t60, marriage_authority__secularist_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(marriage_authority__secularist_reading_su_t74, marriage_authority__secularist_reading, suppression_requirement, 74, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__secularist_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the secularist_reading of the marriage_authority kernel. It forecloses the communal_autonomy_reading and federalist_millet_reading by asserting pluralism is transitional. It influences the gender_rights_reading and judicial_harmonization_reading by making their reformist projects appear insufficient. The kernel family is linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__secularist_reading, organized, 0.2).
constraint_indexing:directionality_override(marriage_authority__secularist_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
