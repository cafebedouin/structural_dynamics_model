% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Enforced Against Women's Constitutional Equality
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   Under India's constitutional regime and similar legal-pluralist systems,
 *   marriage and family law remain governed by personal (religious) law
 *   codes—Islamic, Hindu, Christian, etc.—rather than a unified civil code.
 *   The gender-rights reading contests specific patriarchal practices within
 *   personal law (triple talaq, unequal maintenance obligations, restricted
 *   property rights) by invoking constitutional equality guarantees that
 *   apply uniformly across all citizens regardless of religion. Women subject
 *   to patriarchal personal law norms are the identified victims; their path
 *   to legal equality runs through constitutional litigation led by women's
 *   rights advocates who argue that specific practices violate equality
 *   principles. The constraint is the institutional arrangement that
 *   perpetuates patriarchal personal law practices by treating them as
 *   expressions of community autonomy, shielded from equal-protection review.
 *   The gender-rights reading explicitly reframes this shield as a mechanism
 *   of gender-based discrimination.
 *
 * KEY AGENTS:
 *   - Women within patriarchal personal law systems (powerless, identity_locked): subjected to unequal divorce, maintenance, property rights; exit costs are identity dissolution and social ostracization
 *   - Women's rights advocates within community (moderate, mobile): litigate constitutional equality challenges, shift legitimacy frames, build jurisprudential precedent for gender-equal reform
 *   - Patriarchal community authority structures (organized, constrained): administer personal law norms through scholars and councils; defend practices as rooted in tradition and self-determination
 *   - Apex court judges (institutional, analytical): under gender-rights reading, interpret constitutional equality to impose gender-equal floor on personal law practices
 *   - Parliament (institutional, analytical/excluded): possesses formal authority to enact Uniform Civil Code but is sidestepped by judicial constitutional review
 *   - Communal autonomy advocates (organized, analytical/excluded): defend personal law pluralism on self-determination grounds; excluded because gender-rights reading prioritizes equality over autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.82).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Enforced Against Women's Constitutional Equality").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '2558d7a4-0e86-40a1-b5f3-f06e05e93243').
narrative_ontology:cs_kernel_codification('2558d7a4-0e86-40a1-b5f3-f06e05e93243', fixed_text).
narrative_ontology:cs_authority_grounding('2558d7a4-0e86-40a1-b5f3-f06e05e93243', extraction).
narrative_ontology:cs_interpretation_layer_present('2558d7a4-0e86-40a1-b5f3-f06e05e93243').
narrative_ontology:cs_reading_relation('2558d7a4-0e86-40a1-b5f3-f06e05e93243', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2558d7a4-0e86-40a1-b5f3-f06e05e93243', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('2558d7a4-0e86-40a1-b5f3-f06e05e93243', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('2558d7a4-0e86-40a1-b5f3-f06e05e93243', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('2558d7a4-0e86-40a1-b5f3-f06e05e93243', foundational, constitutional_gender_equality_overrides_personal_law_patriarchy).
narrative_ontology:cs_axiom_status(constitutional_gender_equality_overrides_personal_law_patriarchy, holdable).
narrative_ontology:cs_axiom_grounding('2558d7a4-0e86-40a1-b5f3-f06e05e93243', constitutional_gender_equality_overrides_personal_law_patriarchy, deontological).
narrative_ontology:cs_axiom('2558d7a4-0e86-40a1-b5f3-f06e05e93243', foundational, personal_law_pluralism_compatible_with_gender_equal_reform).
narrative_ontology:cs_axiom_status(personal_law_pluralism_compatible_with_gender_equal_reform, holdable).
narrative_ontology:cs_axiom_grounding('2558d7a4-0e86-40a1-b5f3-f06e05e93243', personal_law_pluralism_compatible_with_gender_equal_reform, conventional).
narrative_ontology:cs_reference_frame('2558d7a4-0e86-40a1-b5f3-f06e05e93243', legal_pluralism_with_patriarchal_personal_law).
narrative_ontology:cs_drift_state('2558d7a4-0e86-40a1-b5f3-f06e05e93243', contemporary_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2558d7a4-0e86-40a1-b5f3-f06e05e93243', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, patriarchal_community_authority_structures).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates_within_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to family law norms (triple talaq, unequal maintenance, restricted property rights, guardianship rules) that the constitutional equality principle declares impermissible when applied to men. The exit cost is not merely legal—it is identity dissolution: leaving the community means abandoning religious identity, family ties, social belonging, and often economic security. Within the framework, they bear the direct extraction: unilateral divorce rights for men, lower maintenance obligations, restricted inheritance.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law_systems, payer,
    powerless, biographical, identity_locked, national).

% Mobilize litigation strategy within the gender-rights reading: file public interest cases arguing that specific personal law practices violate constitutional equality guarantees, build jurisprudential precedent through appellate victories, shift legitimacy frames within legal discourse. They are not themselves the direct victims of the constraint (many are not subject to the personal law in question), but they benefit from the constraint's contestation because each successful case reallocates authority away from community traditional structures toward constitutional review.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates_within_community, beneficiary,
    moderate, biographical, mobile, national).

% Administer and enforce personal law norms through religious scholars, community councils, and informal dispute resolution mechanisms. They justify the norms as rooted in sacred tradition and community self-determination. The constraint's persistence depends on their ability to enforce these norms within the community and their legal standing to defend them against judicial review. Their exit cost is loss of regulatory authority over family life—the core institutional function that legitimizes their role.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, patriarchal_community_authority_structures, agenda_setter,
    organized, generational, constrained, national).

% Under the gender-rights reading, interpret constitutional equality guarantees to impose a floor that overrides specific personal law practices deemed discriminatory. They operate through case-by-case review, issuing decrees that declare particular practices (e.g., triple talaq) unconstitutional while ostensibly preserving personal law pluralism. Their authority to do so is contested; the reading claims the constitution itself demands gender-equal application of family law principles across all communities.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, apex_court_judges, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, apex_court_judges, observer).

% Possesses formal authority to enact a Uniform Civil Code that would eliminate personal law pluralism entirely. The gender-rights reading does not call for full legislative abolition of personal law (that is the secularist reading), but it does sidestep parliament by advancing constitutional equality through judicial interpretation, leaving the legislative seat excluded from the primary contestation mechanism.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, legislative_parliament, excluded,
    institutional, generational, analytical, national).

% Would defend personal law on grounds of community self-determination and religious freedom, arguing that the constraint solves the coordination problem of allowing plural communities to govern family life according to their own norms without state interference. They are excluded from the gender-rights reading's primary argument because that reading prioritizes individual constitutional equality over communal autonomy; if present, they would argue for a different constraint entirely (communal_autonomy_reading).
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_autonomy_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, patriarchal_community_authority_structures).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Personal law pluralism coordinates the governance of family relationships across religiously diverse communities by allowing each community's traditional norms to apply to its members without requiring a single state-imposed civil code. The constraint solves the problem: how to govern family law in a multi-religious society without forcing assimilation or imposing majoritarian dominance.
% TRANSFER_FUNCTION: Transfers authority over women's legal status (right to initiate divorce, maintenance entitlement, property rights, guardianship decisions) from individual women with constitutional rights to patriarchal community authority structures that administer norms inconsistent with gender-equal constitutional principles. The arrangement moves power from women as constitutional agents to male-headed households and religious authorities.
% ABSENT_VOICES: Secular citizens who regard personal law pluralism itself as an anomaly would object (secularist reading—excluded here because this reading does not contest pluralism itself, only gender equality within it). Federalist-millet advocates would object that the reading treats pluralism as the problem rather than the solution (federalist_millet_reading—excluded). The gender-rights reading silences the community autonomy defense by treating it as a cover story for patriarchal extraction.
% DISAPPEARANCE_RATIONALE: If the constraint and its enforcement disappeared—i.e., if patriarchal personal law practices were invalidated and women gained equal constitutional standing in family law—the immediate rearrangement would be: women in affected communities gain unilateral divorce rights, maintenance becomes gender-neutral, property and inheritance rules become equal, guardianship becomes joint or gender-neutral. The secondary rearrangement: communities lose regulatory authority over family formation and dissolution; the boundary between 'community law' and 'constitutional law' shifts; women's identity relationship to community becomes more individualized (less fused with family/patriarchal structures). The constraint's disappearance is structurally prevented by the identity-lock and suppression mechanisms—women cannot exit without identity dissolution, and judges are themselves divided on whether to enforce gender equality or respect community autonomy.
% FOUNDING_PROBLEM: In a multi-religious society, family law governance must balance: (1) individual equality under a common constitution, and (2) community self-determination in matters of intimate personal life and religious practice. Personal law pluralism was designed to solve this by allowing diverse communities to govern family life according to their own norms, provided those norms were applied equally within each community. The gender-rights reading arose when it became evident that 'equal application within community' concealed systematic gender inequality—that the founding problem was partially illusory because equality-in-application was defined by patriarchal communities, not by constitutional standards.
% FOUNDING_PROBLEM_CORROBORATION: Community autonomy advocates and traditional religious scholars attest the founding problem is still live: pluralism remains necessary to avoid majoritarian imposition on minority communities. Gender-rights advocates and constitutional scholars outside the benefiting communities attest the founding problem was partially solved (pluralism does exist) but revealed a new problem: pluralism itself became a shield for gender-based discrimination that violates constitutional equality. Empirical corroboration: women's rights organizations document systematic harms (incidence of triple talaq divorces, maintenance non-payment, property denial) that patriarchal personal law enables; apex court judgments acknowledge these harms and cite constitutional equality to override specific practices.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint allocates fundamental family law authority to patriarchal structures rather than to women as constitutional agents; women cannot unilaterally divorce, cannot claim equal maintenance, cannot inherit equally. Suppression is substantial (0.71) because the constraint's persistence depends on: (1) women's identity-lock (cannot exit without religious identity dissolution), (2) community enforcement of patriarchal norms through informal dispute resolution and social ostracization, and (3) judicial deference to personal law on autonomy grounds. Theater is moderate (0.41): the norms are defended with genuine-sounding autonomy language, but the core function is patriarchal extraction; as the gender-rights reading gains traction, judicial language increasingly emphasizes gender equality rather than autonomy, suggesting a shift toward unveiling the extraction. The temporal measurements show extractiveness and suppression rising modestly (0.68→0.82 and 0.54→0.71 over the interval) as litigation pressure from women's rights advocates forces patriarchal structures to defend their practices more explicitly and intensely, thereby increasing the active enforcement machinery's visibility. Theater rises from 0.22 to 0.41 as the constraint becomes more contested—more judicial language emphasizes autonomy even as decisions constrain patriarchal practices, suggesting increasing performative legitimacy work.
 *
 * PERSPECTIVAL GAP:
 *   The gender-rights reading produces sharp perspectival gaps: (1) Patriarchal authority structures see the constraint as solving the founding problem (how to govern family life according to community norms without majoritarian state imposition). Women subject to it see the same constraint as the problem itself (patriarchal extraction shielded by autonomy language). (2) Communal autonomy advocates see the reading as a trojan horse for secular state domination (eliminating pluralism through gender-equality reasoning). Gender-rights advocates see the reading as rescuing pluralism from patriarchal capture (pluralism requires equal internal choice, which patriarchal norms deny). (3) Apex court judges see themselves as enforcing constitutional values that override community norms (legitimacy comes from constitutional authority). Both community authorities and federalist advocates see courts as illegitimately extending authority into matters that should remain pluralist (legitimacy comes from community tradition). These gaps cannot be bridged within the gender-rights reading itself—they are the kernel contest, and the reading instantiates one side of it.
 *
 * DIRECTIONALITY LOGIC:
 *   From women subject to patriarchal personal law: the constraint is high-extraction, high-suppression, fully identity-locked target (d ≈ 1.0). Their structural position is: they cannot exit without identity death, they bear costs unilaterally (unequal divorce rights, unequal maintenance), and they have no power to reform the rules internally. From patriarchal community authority structures: the constraint is pure beneficiary (d ≈ 0.0)—they set the rules, they collect the deference, they have arbitrage-grade exit options (they can adapt practices if forced by courts). From women's rights advocates within community: the constraint is weakly beneficial (d ≈ 0.2)—they do not directly benefit from patriarchal norms, but they benefit from the constraint's contestation because each litigation victory reallocates authority away from community structures. From apex court judges: the constraint positions them as power-holders (d ≈ 0.5 or slightly beneficiary-side)—they gain authority as the arbiter of constitutional equality, but they also bear the cost of being drawn into intimate family disputes that the personal law system had previously kept off the judicial agenda. The directionality divergence across seats is structural and unsynthesizable: the same constraint that extracts maximally from powerless women benefits concentrated patriarchal authority structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under the gender-rights reading is: 'How can a multi-religious society govern family life equitably without imposing majoritarian secular law on minority communities?' The founding problem's status is 'contested': community autonomy advocates say it is still live (pluralism is necessary to prevent majoritarian domination), while gender-rights advocates say the founding problem was partially solved (pluralism does exist) but obscured a new problem (pluralism becomes a shield for patriarchal extraction). Under the gender-rights reading, the constraint persists not because the founding problem demands it but because patriarchal authority structures have institutional power to resist reform. Mandatrophy resolution: the constraint has outlived its justification in the gender-rights frame. If the justification was 'equal governance of family law for diverse communities,' the constraint fails because it is not equal—women are governed unequally within their personal law codes. The constraint persists via institutional inertia: patriarchal structures defend it, courts defer on autonomy grounds, and women are identity-locked, preventing exit-driven contestation. The snare classification is structurally sound: the coordination story (pluralism) is cover; persistence depends on coercion (suppression of women's equality claims, enforcement of patriarchal norms through informal mechanisms) and suppression of exits (identity-lock, economic dependency, social ostracization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_choice,
    'To what extent is women''s exit-cost driven by intrinsic identity fusion (religion/community as constitutive self) versus structural isolation (economic dependency, social ostracization, lack of information about alternatives)?',
    'Longitudinal studies tracking women who exit the personal law system: post-exit identity trajectory, residual suppression, reintegration patterns. Also: interviews with exit-seekers about decision-factors weighted by reported locus of control.',
    'If identity-locked (intrinsic), the suppression persists post-exit and cannot be eliminated by mere legal reform—the constraint''s hold is internalized. If structurally-isolated (extrinsic), legal reform + support structures (safe housing, economic transition, community reintegration) could substantially reduce the constraint''s extraction. Classification consequence: high identity-lock pushes toward higher effective suppression; predominantly structural isolation suggests the base suppression metric understates the constraint''s actual hold because the metric measures institutional suppression, not the internalized kind.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_choice, empirical, 'Suppression mechanism: intrinsic identity fusion vs. extrinsic structural isolation').

omega_variable(
    reading_boundaries_gender_vs_autonomy,
    'Is the gender-rights reading''s target limited to gender equality within personal law (rejecting specific patriarchal practices while preserving pluralism), or does gender equality demand the abolition of personal law pluralism itself as inherently non-equal?',
    'Examine case law and advocacy position statements: do gender-rights advocates defend the coherence of ''equal personal law codes'' (religious law reformed for gender parity), or do they argue that personal law differentiation by religion is itself gender-unequal because it locks women into traditional structures?',
    'If the first: the gender-rights reading coexists with communal-autonomy reading (both endorse pluralism, differ on gender equality within it). If the second: the gender-rights reading forecloses communal-autonomy reading because gender equality becomes incompatible with pluralism itself—the readings cannot both hold in a single framework. This distinction determines whether the kernel contest is a managed disagreement (coexists) or a zero-sum logical conflict (forecloses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundaries_gender_vs_autonomy, conceptual, 'Whether gender equality is compatible with personal law pluralism or demands its abolition').

omega_variable(
    judicial_authority_scope,
    'Under the gender-rights reading, does the apex court''s constitutional equality authority extend to invalidating specific practices (triple talaq, unequal maintenance) while preserving community authority over other family matters? Or does gender equality logic imply apex court authority over the entire personal law domain?',
    'Trace appellate court decisions and constitutional reasoning: do courts claim authority only to set a ''gender-equal floor'' leaving communities room to vary above it, or do courts claim comprehensive family law authority where constitutional equality is the governing principle?',
    'If authority is limited to practice-by-practice floors: the constraint remains substantially extractive but partially reformed—judges invalidate the worst harms while the patriarchal structure persists on other dimensions. If authority is comprehensive: the constraint moves toward full judicial overriding of community authority, which approaches the judicial_harmonization_reading''s framing (judicial authority becomes the actual authority-holder, not merely a review constraint). This determines whether the gender-rights reading is a Tangled Rope (coordination + extraction) or a pure Snare (extraction with community-authority erosion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_scope, conceptual, 'Scope of judicial constitutional authority under gender-equality doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__gender_rights_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__gender_rights_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__gender_rights_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__gender_rights_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__gender_rights_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__gender_rights_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(marr_tr_t35, marriage_authority__gender_rights_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(marr_be_t5, marriage_authority__gender_rights_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(marr_be_t10, marriage_authority__gender_rights_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(marr_be_t15, marriage_authority__gender_rights_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(marr_be_t20, marriage_authority__gender_rights_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(marr_be_t25, marriage_authority__gender_rights_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(marr_be_t30, marriage_authority__gender_rights_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(marr_be_t35, marriage_authority__gender_rights_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(marr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(marr_su_t5, marriage_authority__gender_rights_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(marr_su_t10, marriage_authority__gender_rights_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(marr_su_t15, marriage_authority__gender_rights_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(marr_su_t20, marriage_authority__gender_rights_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(marr_su_t25, marriage_authority__gender_rights_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(marr_su_t30, marriage_authority__gender_rights_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(marr_su_t35, marriage_authority__gender_rights_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five structurally distinct constraints, one per reading. Each reading instantiates a different ε (ranging from near-zero for communal_autonomy to high for gender_rights), different beneficiary/victim structures, and different classifications. The readings are linked via network.affects_constraints because each reading's success conditions constrain the viability of sibling readings. Gender_rights_reading (this file) forecloses communal_autonomy_reading if gender equality is deemed logically incompatible with personal law pluralism; coexists_with federalist_millet_reading if both readings endorse pluralism but disagree on its equality content; influences judicial_harmonization_reading because successful gender-equality litigation expands judicial authority; and coexists_with secularist_reading (both call for equality, differ on whether pluralism is compatible with it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, organized, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
